%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsMonad]{@DsMonad@: monadery used in desugaring}

\begin{code}
module DsMonad (
	DsM, mappM,
	initDs, returnDs, thenDs, listDs, fixDs, mapAndUnzipDs, foldlDs,

	newTyVarsDs, 
	duplicateLocalDs, newSysLocalDs, newSysLocalsDs, newUniqueId,
	newFailLocalDs,
	getSrcLocDs, putSrcLocDs,
	getModuleDs,
	newUnique, 
	UniqSupply, newUniqueSupply,
	getDOptsDs,
	dsLookupGlobal, dsLookupGlobalId, dsLookupTyCon, dsLookupDataCon,

	DsMetaEnv, DsMetaVal(..), dsLookupMetaEnv, dsExtendMetaEnv,

	dsWarn, 
	DsWarning,
	DsMatchContext(..)
    ) where

#include "HsVersions.h"

import TcHsSyn		( TypecheckedPat, TypecheckedMatchContext, TypecheckedHsExpr )
import TcRnMonad
import IfaceEnv		( tcIfaceGlobal )
import HscTypes		( TyThing(..), TypeEnv, HscEnv, 
			  IsBootInterface,
			  tyThingId, tyThingTyCon, tyThingDataCon  )
import Bag		( emptyBag, snocBag, Bag )
import DataCon		( DataCon )
import TyCon		( TyCon )
import DataCon		( DataCon )
import Id		( mkSysLocal, setIdUnique, Id )
import Module		( Module, ModuleName, ModuleEnv )
import Var		( TyVar, setTyVarUnique )
import Outputable
import SrcLoc		( noSrcLoc, SrcLoc )
import Type             ( Type )
import UniqSupply	( UniqSupply, uniqsFromSupply )
import Name		( Name, nameOccName )
import NameEnv
import OccName          ( occNameFS )
import CmdLineOpts	( DynFlags )

import DATA_IOREF	( newIORef, readIORef )

infixr 9 `thenDs`
\end{code}

Now the mondo monad magic (yes, @DsM@ is a silly name)---carry around
a @UniqueSupply@ and some annotations, which
presumably include source-file location information:
\begin{code}
type DsM result = TcRnIf DsGblEnv DsLclEnv result

-- Compatibility functions
fixDs    = fixM
thenDs   = thenM
returnDs = returnM
listDs   = sequenceM
foldlDs  = foldlM
mapAndUnzipDs = mapAndUnzipM


type DsWarning = (SrcLoc, SDoc)

data DsGblEnv = DsGblEnv {
	ds_mod	   :: Module,       		-- For SCC profiling
	ds_warns   :: IORef (Bag DsWarning),	-- Warning messages
	ds_if_env  :: IfGblEnv			-- Used for looking up global, 
						-- possibly-imported things
    }

data DsLclEnv = DsLclEnv {
	ds_meta	   :: DsMetaEnv,	-- Template Haskell bindings
	ds_loc	   :: SrcLoc		-- to put in pattern-matching error msgs
     }

-- Inside [| |] brackets, the desugarer looks 
-- up variables in the DsMetaEnv
type DsMetaEnv = NameEnv DsMetaVal

data DsMetaVal
   = Bound Id		-- Bound by a pattern inside the [| |]. 
			-- Will be dynamically alpha renamed.
			-- The Id has type String

   | Splice TypecheckedHsExpr	-- These bindings are introduced by
				-- the PendingSplices on a HsBracketOut

-- initDs returns the UniqSupply out the end (not just the result)

initDs  :: HscEnv
	-> Module -> TypeEnv
	-> ModuleEnv (ModuleName,IsBootInterface)	
	-> DsM a
	-> IO (a, Bag DsWarning)

initDs hsc_env mod type_env is_boot thing_inside
  = do 	{ warn_var <- newIORef emptyBag
	; let { if_env = IfGblEnv { if_rec_types = Just (mod, return type_env),
				    if_is_boot = is_boot }
	      ; gbl_env = DsGblEnv { ds_mod = mod, 
				     ds_if_env = if_env, 
				     ds_warns = warn_var }
	      ; lcl_env = DsLclEnv { ds_meta = emptyNameEnv, 
				     ds_loc = noSrcLoc } }

	; res <- initTcRnIf 'd' hsc_env gbl_env lcl_env thing_inside

	; warns <- readIORef warn_var
	; return (res, warns)
	}
\end{code}

And all this mysterious stuff is so we can occasionally reach out and
grab one or more names.  @newLocalDs@ isn't exported---exported
functions are defined with it.  The difference in name-strings makes
it easier to read debugging output.

\begin{code}
-- Make a new Id with the same print name, but different type, and new unique
newUniqueId :: Name -> Type -> DsM Id
newUniqueId id ty
  = newUnique 	`thenDs` \ uniq ->
    returnDs (mkSysLocal (occNameFS (nameOccName id)) uniq ty)

duplicateLocalDs :: Id -> DsM Id
duplicateLocalDs old_local 
  = newUnique 	`thenDs` \ uniq ->
    returnDs (setIdUnique old_local uniq)

newSysLocalDs, newFailLocalDs :: Type -> DsM Id
newSysLocalDs ty
  = newUnique 	`thenDs` \ uniq ->
    returnDs (mkSysLocal FSLIT("ds") uniq ty)

newSysLocalsDs tys = mappM newSysLocalDs tys

newFailLocalDs ty 
  = newUnique 	`thenDs` \ uniq ->
    returnDs (mkSysLocal FSLIT("fail") uniq ty)
	-- The UserLocal bit just helps make the code a little clearer
\end{code}

\begin{code}
newTyVarsDs :: [TyVar] -> DsM [TyVar]
newTyVarsDs tyvar_tmpls 
  = newUniqueSupply	`thenDs` \ uniqs ->
    returnDs (zipWith setTyVarUnique tyvar_tmpls (uniqsFromSupply uniqs))
\end{code}

We can also reach out and either set/grab location information from
the @SrcLoc@ being carried around.

\begin{code}
getDOptsDs :: DsM DynFlags
getDOptsDs = getDOpts

getModuleDs :: DsM Module
getModuleDs = do { env <- getGblEnv; return (ds_mod env) }

getSrcLocDs :: DsM SrcLoc
getSrcLocDs = do { env <- getLclEnv; return (ds_loc env) }

putSrcLocDs :: SrcLoc -> DsM a -> DsM a
putSrcLocDs new_loc thing_inside = updLclEnv (\ env -> env {ds_loc = new_loc}) thing_inside

dsWarn :: DsWarning -> DsM ()
dsWarn warn = do { env <- getGblEnv; updMutVar (ds_warns env) (`snocBag` warn) }
\end{code}

\begin{code}
dsLookupGlobal :: Name -> DsM TyThing
-- Very like TcEnv.tcLookupGlobal
dsLookupGlobal name 
  = do	{ env <- getGblEnv
	; setEnvs (ds_if_env env, ())
		  (tcIfaceGlobal name) }

dsLookupGlobalId :: Name -> DsM Id
dsLookupGlobalId name 
  = dsLookupGlobal name		`thenDs` \ thing ->
    returnDs (tyThingId thing)

dsLookupTyCon :: Name -> DsM TyCon
dsLookupTyCon name
  = dsLookupGlobal name		`thenDs` \ thing ->
    returnDs (tyThingTyCon thing)

dsLookupDataCon :: Name -> DsM DataCon
dsLookupDataCon name
  = dsLookupGlobal name		`thenDs` \ thing ->
    returnDs (tyThingDataCon thing)
\end{code}

\begin{code}
dsLookupMetaEnv :: Name -> DsM (Maybe DsMetaVal)
dsLookupMetaEnv name = do { env <- getLclEnv; return (lookupNameEnv (ds_meta env) name) }

dsExtendMetaEnv :: DsMetaEnv -> DsM a -> DsM a
dsExtendMetaEnv menv thing_inside
  = updLclEnv (\env -> env { ds_meta = ds_meta env `plusNameEnv` menv }) thing_inside
\end{code}


%************************************************************************
%*									*
\subsection{Type synonym @EquationInfo@ and access functions for its pieces}
%*									*
%************************************************************************

\begin{code}
data DsMatchContext
  = DsMatchContext TypecheckedMatchContext [TypecheckedPat] SrcLoc
  | NoMatchContext
  deriving ()
\end{code}
