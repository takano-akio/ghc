%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: ClosureInfo.lhs,v 1.52 2002/04/29 14:03:43 simonmar Exp $
%
\section[ClosureInfo]{Data structures which describe closures}

Much of the rationale for these things is in the ``details'' part of
the STG paper.

\begin{code}
module ClosureInfo (
	ClosureInfo, LambdaFormInfo, SMRep, 	-- all abstract
	StandardFormInfo,

	EntryConvention(..),

	mkClosureLFInfo, mkConLFInfo, mkSelectorLFInfo,
	mkApLFInfo, mkLFImported, mkLFArgument, mkLFLetNoEscape,
	UpdateFlag,

	closureSize, closureNonHdrSize,
	closureGoodStuffSize, closurePtrsSize,
	slopSize,

	layOutDynClosure, layOutDynConstr, layOutStaticClosure,
	layOutStaticNoFVClosure, layOutStaticConstr,
	mkVirtHeapOffsets, mkStaticClosure,

	nodeMustPointToIt, getEntryConvention, 
	FCode, CgInfoDownwards, CgState, 

	blackHoleOnEntry,

	staticClosureRequired,
	slowFunEntryCodeRequired, funInfoTableRequired,

	closureName, infoTableLabelFromCI, fastLabelFromCI,
	closureLabelFromCI, closureSRT,
	entryLabelFromCI, 
	closureLFInfo, closureSMRep, closureUpdReqd,
	closureSingleEntry, closureReEntrant, closureSemiTag,
	isStandardFormThunk,
	GenStgArg,

	isToplevClosure,
	closureTypeDescr,		-- profiling

	isStaticClosure,
	allocProfilingMsg,
	cafBlackHoleClosureInfo, seCafBlackHoleClosureInfo,
	maybeSelectorInfo,

	staticClosureNeedsLink,
    ) where

#include "HsVersions.h"

import AbsCSyn		
import StgSyn
import CgMonad

import Constants	( mIN_UPD_SIZE, mIN_SIZE_NonUpdHeapObject,
			  mAX_SPEC_FUN_SIZE, mAX_SPEC_THUNK_SIZE, mAX_SPEC_CONSTR_SIZE )
import CgRetConv	( assignRegs )
import CLabel		( CLabel, mkStdEntryLabel, mkFastEntryLabel,
			  mkInfoTableLabel,
			  mkConInfoTableLabel, 
			  mkCAFBlackHoleInfoTableLabel, 
			  mkSECAFBlackHoleInfoTableLabel, 
			  mkStaticInfoTableLabel, mkStaticConEntryLabel,
			  mkConEntryLabel, mkClosureLabel,
			  mkSelectorInfoLabel, mkSelectorEntryLabel,
			  mkApInfoTableLabel, mkApEntryLabel,
			  mkReturnPtLabel
			)
import CmdLineOpts	( opt_SccProfilingOn, opt_OmitBlackHoling,
			  opt_Parallel, opt_DoTickyProfiling,
			  opt_SMP )
import Id		( Id, idType, idArity )
import DataCon		( DataCon, dataConTag, fIRST_TAG, dataConTyCon,
			  isNullaryDataCon, dataConName
			)
import TyCon		( isBoxedTupleTyCon )
import Name		( Name, nameUnique, getOccName )
import OccName		( occNameUserString )
import PprType		( getTyDescription )
import PrimRep		( getPrimRepSize, separateByPtrFollowness, PrimRep )
import SMRep		-- all of it
import Type		( isUnLiftedType, Type )
import BasicTypes	( TopLevelFlag(..), isNotTopLevel, isTopLevel )
import Util		( mapAccumL, listLengthCmp, lengthIs )
import FastString
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-datatypes]{Data types for closure information}
%*									*
%************************************************************************

The ``wrapper'' data type for closure information:

\begin{code}
data ClosureInfo
  = MkClosureInfo {
	closureName   :: Name,			-- The thing bound to this closure
	closureLFInfo :: LambdaFormInfo,	-- Info derivable from the *source*
	closureSMRep  :: SMRep,			-- representation used by storage manager
	closureSRT    :: C_SRT			-- What SRT applies to this closure
    }
\end{code}

%************************************************************************
%*									*
\subsubsection[LambdaFormInfo-datatype]{@LambdaFormInfo@: source-derivable info}
%*									*
%************************************************************************

\begin{code}
data LambdaFormInfo
  = LFReEntrant		-- Reentrant closure; used for PAPs too
	Type		-- Type of closure    (ToDo: remove)
	TopLevelFlag	-- True if top level
	!Int		-- Arity
	!Bool		-- True <=> no fvs

  | LFCon		-- Constructor
	DataCon		-- The constructor
	Bool		-- True <=> zero arity

  | LFTuple		-- Tuples
	DataCon		-- The tuple constructor
	Bool		-- True <=> zero arity

  | LFThunk		-- Thunk (zero arity)
	Type		-- Type of the thunk   (ToDo: remove)
	TopLevelFlag
	!Bool		-- True <=> no free vars
	Bool		-- True <=> updatable (i.e., *not* single-entry)
	StandardFormInfo

  | LFArgument		-- Used for function arguments.  We know nothing about
			-- this closure.  Treat like updatable "LFThunk"...

  | LFImported		-- Used for imported things.  We know nothing about this
			-- closure.  Treat like updatable "LFThunk"...
			-- Imported things which we do know something about use
			-- one of the other LF constructors (eg LFReEntrant for
			-- known functions)

  | LFLetNoEscape	-- See LetNoEscape module for precise description of
			-- these "lets".
	Int		-- arity;

  | LFBlackHole		-- Used for the closures allocated to hold the result
			-- of a CAF.  We want the target of the update frame to
			-- be in the heap, so we make a black hole to hold it.
        CLabel          -- Flavour (info label, eg CAF_BLACKHOLE_info).


data StandardFormInfo	-- Tells whether this thunk has one of a small number
			-- of standard forms

  = NonStandardThunk	-- No, it isn't

  | SelectorThunk
       Int             	-- 0-origin offset of ak within the "goods" of 
			-- constructor (Recall that the a1,...,an may be laid
			-- out in the heap in a non-obvious order.)

{- A SelectorThunk is of form

     case x of
       con a1,..,an -> ak

   and the constructor is from a single-constr type.
-}

  | ApThunk 
	Int		-- arity

{- An ApThunk is of form

	x1 ... xn

   The code for the thunk just pushes x2..xn on the stack and enters x1.
   There are a few of these (for 1 <= n <= MAX_SPEC_AP_SIZE) pre-compiled
   in the RTS to save space.
-}

\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-construction]{Functions which build LFInfos}
%*									*
%************************************************************************

@mkClosureLFInfo@ figures out the appropriate LFInfo for the closure.

\begin{code}
mkClosureLFInfo :: Id		-- The binder
		-> TopLevelFlag	-- True of top level
		-> [Id]		-- Free vars
		-> UpdateFlag 	-- Update flag
		-> [Id] 	-- Args
		-> LambdaFormInfo

mkClosureLFInfo bndr top fvs upd_flag args@(_:_) -- Non-empty args
  = LFReEntrant (idType bndr) top (length args) (null fvs)

mkClosureLFInfo bndr top fvs ReEntrant []
  = LFReEntrant (idType bndr) top 0 (null fvs)

mkClosureLFInfo bndr top fvs upd_flag []
#ifdef DEBUG
  | isUnLiftedType ty = pprPanic "mkClosureLFInfo" (ppr bndr <+> ppr ty)
#endif
  | otherwise
  = LFThunk ty top (null fvs) (isUpdatable upd_flag) NonStandardThunk
  where
    ty = idType bndr
\end{code}

@mkConLFInfo@ is similar, for constructors.

\begin{code}
mkConLFInfo :: DataCon -> LambdaFormInfo

mkConLFInfo con
  = -- the isNullaryDataCon will do this: ASSERT(isDataCon con)
    (if isBoxedTupleTyCon (dataConTyCon con) then LFTuple else LFCon) 
	con (isNullaryDataCon con)

mkSelectorLFInfo rhs_ty offset updatable
  = LFThunk rhs_ty NotTopLevel False updatable (SelectorThunk offset)

mkApLFInfo rhs_ty upd_flag arity
  = LFThunk rhs_ty NotTopLevel (arity == 0)
	    (isUpdatable upd_flag) (ApThunk arity)
\end{code}

Miscellaneous LF-infos.

\begin{code}
mkLFArgument	= LFArgument
mkLFLetNoEscape = LFLetNoEscape

mkLFImported :: Id -> LambdaFormInfo
mkLFImported id
  = case idArity id of
      n | n > 0 -> LFReEntrant (idType id) TopLevel n True  -- n > 0
      other -> LFImported	-- Not sure of exact arity
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-sizes]{Functions about closure {\em sizes}}
%*									*
%************************************************************************

\begin{code}
closureSize :: ClosureInfo -> HeapOffset
closureSize cl_info = fixedHdrSize + closureNonHdrSize cl_info

closureNonHdrSize :: ClosureInfo -> Int
closureNonHdrSize cl_info
  = tot_wds + computeSlopSize tot_wds 
			      (closureSMRep cl_info)
			      (closureUpdReqd cl_info) 
  where
    tot_wds = closureGoodStuffSize cl_info

slopSize :: ClosureInfo -> Int
slopSize cl_info
  = computeSlopSize (closureGoodStuffSize cl_info)
		    (closureSMRep cl_info)
		    (closureUpdReqd cl_info)

closureGoodStuffSize :: ClosureInfo -> Int
closureGoodStuffSize cl_info
  = let (ptrs, nonptrs) = sizes_from_SMRep (closureSMRep cl_info)
    in	ptrs + nonptrs

closurePtrsSize :: ClosureInfo -> Int
closurePtrsSize cl_info
  = let (ptrs, _) = sizes_from_SMRep (closureSMRep cl_info)
    in	ptrs

-- not exported:
sizes_from_SMRep :: SMRep -> (Int,Int)
sizes_from_SMRep (GenericRep _ ptrs nonptrs _)   = (ptrs, nonptrs)
sizes_from_SMRep BlackHoleRep			 = (0, 0)
\end{code}

Computing slop size.  WARNING: this looks dodgy --- it has deep
knowledge of what the storage manager does with the various
representations...

Slop Requirements:
\begin{itemize}
\item
Updateable closures must be @mIN_UPD_SIZE@.
	\begin{itemize}
	\item
	Indirections require 1 word
	\item
	Appels collector indirections 2 words
	\end{itemize}
THEREFORE: @mIN_UPD_SIZE = 2@.

\item
Collectable closures which are allocated in the heap
must be	@mIN_SIZE_NonUpdHeapObject@.

Copying collector forward pointer requires 1 word

THEREFORE: @mIN_SIZE_NonUpdHeapObject = 1@
\end{itemize}

Static closures have an extra ``static link field'' at the end, but we
don't bother taking that into account here.

\begin{code}
computeSlopSize :: Int -> SMRep -> Bool -> Int

computeSlopSize tot_wds (GenericRep _ _ _ _) True		-- Updatable
  = max 0 (mIN_UPD_SIZE - tot_wds)

computeSlopSize tot_wds (GenericRep True _ _ _) False	-- Non updatable
  = 0							-- Static

computeSlopSize tot_wds (GenericRep False _ _ _) False	-- Non updatable
  = max 0 (mIN_SIZE_NonUpdHeapObject - tot_wds)		-- Dynamic

computeSlopSize tot_wds BlackHoleRep _			-- Updatable
  = max 0 (mIN_UPD_SIZE - tot_wds)
\end{code}

%************************************************************************
%*									*
\subsection[layOutDynClosure]{Lay out a dynamic closure}
%*									*
%************************************************************************

\begin{code}
layOutDynClosure, layOutStaticClosure
	:: Name			    -- STG identifier of this closure
	-> (a -> PrimRep)    	    -- how to get a PrimRep for the fields
	-> [a]			    -- the "things" being layed out
	-> LambdaFormInfo	    -- what sort of closure it is
	-> C_SRT
	-> (ClosureInfo,	    -- info about the closure
	    [(a, VirtualHeapOffset)])	-- things w/ offsets pinned on them

layOutDynClosure name kind_fn things lf_info srt_info
  = (MkClosureInfo { closureName = name, closureLFInfo = lf_info,
		     closureSMRep = sm_rep, closureSRT = srt_info },
     things_w_offsets)
  where
    (tot_wds,		 -- #ptr_wds + #nonptr_wds
     ptr_wds,		 -- #ptr_wds
     things_w_offsets) = mkVirtHeapOffsets kind_fn things
    sm_rep = chooseDynSMRep lf_info tot_wds ptr_wds
\end{code}

Wrappers for when used with data constructors:

\begin{code}
layOutDynConstr, layOutStaticConstr
	:: Name 	-- Of the closure
	-> DataCon 	
	-> (a -> PrimRep) -> [a]
	-> (ClosureInfo, [(a,VirtualHeapOffset)])

layOutDynConstr name data_con kind_fn args
  = layOutDynClosure name kind_fn args (mkConLFInfo data_con) NoC_SRT

layOutStaticConstr name data_con kind_fn things
  = layOutStaticClosure name kind_fn things (mkConLFInfo data_con) NoC_SRT
\end{code}

%************************************************************************
%*									*
\subsection[layOutStaticClosure]{Lay out a static closure}
%*									*
%************************************************************************

layOutStaticClosure is only used for laying out static constructors at
the moment.  

Static closures for functions are laid out using
layOutStaticNoFVClosure.

\begin{code}
layOutStaticClosure name kind_fn things lf_info srt_info
  = (MkClosureInfo { closureName = name, closureLFInfo = lf_info,
		     closureSMRep = rep, closureSRT = srt_info },
     things_w_offsets)
  where
    rep = GenericRep is_static ptr_wds (tot_wds - ptr_wds) closure_type

    (tot_wds,		 -- #ptr_wds + #nonptr_wds
     ptr_wds,		 -- #ptr_wds
     things_w_offsets) = mkVirtHeapOffsets kind_fn things

    -- constructors with no pointer fields will definitely be NOCAF things.
    -- this is a compromise until we can generate both kinds of constructor
    -- (a normal static kind and the NOCAF_STATIC kind).
    closure_type = getClosureType is_static tot_wds ptr_wds lf_info
    is_static    = True

layOutStaticNoFVClosure :: Name -> LambdaFormInfo -> C_SRT -> ClosureInfo
layOutStaticNoFVClosure name lf_info srt_info
  = MkClosureInfo { closureName = name, closureLFInfo = lf_info,
		    closureSMRep = rep, closureSRT = srt_info }
  where
    rep = GenericRep is_static 0 0 (getClosureType is_static 0 0 lf_info)
    is_static = True


-- make a static closure, adding on any extra padding needed for CAFs,
-- and adding a static link field if necessary.

mkStaticClosure closure_info ccs fields cafrefs
  | opt_SccProfilingOn =
	     CStaticClosure
		closure_info
	    	(mkCCostCentreStack ccs)
		all_fields
  | otherwise =
	     CStaticClosure
		closure_info
	    	(panic "absent cc")
		all_fields

   where
    all_fields = fields ++ padding_wds ++ static_link_field

    upd_reqd = closureUpdReqd closure_info

    padding_wds
	| not upd_reqd = []
	| otherwise    = replicate n (mkIntCLit 0) -- a bunch of 0s
	where n = max 0 (mIN_UPD_SIZE - length fields)

	-- We always have a static link field for a thunk, it's used to
	-- save the closure's info pointer when we're reverting CAFs
	-- (see comment in Storage.c)
    static_link_field
	| upd_reqd || staticClosureNeedsLink closure_info = [static_link_value]
	| otherwise 	     		                  = []

	-- for a static constructor which has NoCafRefs, we set the
	-- static link field to a non-zero value so the garbage
	-- collector will ignore it.
    static_link_value
	| cafrefs	= mkIntCLit 0
	| otherwise	= mkIntCLit 1
\end{code}

%************************************************************************
%*									*
\subsection[SMreps]{Choosing SM reps}
%*									*
%************************************************************************

\begin{code}
chooseDynSMRep
	:: LambdaFormInfo
	-> Int -> Int		-- Tot wds, ptr wds
	-> SMRep

chooseDynSMRep lf_info tot_wds ptr_wds
  = let
	 is_static    = False
	 nonptr_wds   = tot_wds - ptr_wds
	 closure_type = getClosureType is_static tot_wds ptr_wds lf_info
    in
    GenericRep is_static ptr_wds nonptr_wds closure_type	

-- we *do* get non-updatable top-level thunks sometimes.  eg. f = g
-- gets compiled to a jump to g (if g has non-zero arity), instead of
-- messing around with update frames and PAPs.  We set the closure type
-- to FUN_STATIC in this case.

getClosureType :: Bool -> Int -> Int -> LambdaFormInfo -> ClosureType
getClosureType is_static tot_wds ptr_wds lf_info
  = case lf_info of
	LFCon con zero_arity
		| is_static && ptr_wds == 0	       -> CONSTR_NOCAF
		| specialised_rep mAX_SPEC_CONSTR_SIZE -> CONSTR_p_n
		| otherwise			       -> CONSTR

	LFTuple _ zero_arity
		| is_static && ptr_wds == 0	       -> CONSTR_NOCAF
		| specialised_rep mAX_SPEC_CONSTR_SIZE -> CONSTR_p_n
		| otherwise			       -> CONSTR

  	LFReEntrant _ _ _ _ 
		| specialised_rep mAX_SPEC_FUN_SIZE -> FUN_p_n
		| otherwise			    -> FUN

	LFThunk _ _ _ _ (SelectorThunk _) -> THUNK_SELECTOR

	LFThunk _ _ _ _ _
		| specialised_rep mAX_SPEC_THUNK_SIZE -> THUNK_p_n
		| otherwise			      -> THUNK

	_ -> panic "getClosureType"
  where
    specialised_rep max_size =  not is_static
			     && tot_wds > 0
			     && tot_wds <= max_size
\end{code}

%************************************************************************
%*									*
\subsection[mkVirtHeapOffsets]{Assigning heap offsets in a closure}
%*									*
%************************************************************************

@mkVirtHeapOffsets@ (the heap version) always returns boxed things with
smaller offsets than the unboxed things, and furthermore, the offsets in
the result list

\begin{code}
mkVirtHeapOffsets :: 
	  (a -> PrimRep)	-- To be able to grab kinds;
				--  	w/ a kind, we can find boxedness
	  -> [a]		-- Things to make offsets for
	  -> (Int,		-- *Total* number of words allocated
	      Int,		-- Number of words allocated for *pointers*
	      [(a, VirtualHeapOffset)])
				-- Things with their offsets from start of 
				--  object in order of increasing offset

-- First in list gets lowest offset, which is initial offset + 1.

mkVirtHeapOffsets kind_fun things
  = let (ptrs, non_ptrs)    	      = separateByPtrFollowness kind_fun things
    	(wds_of_ptrs, ptrs_w_offsets) = mapAccumL computeOffset 0 ptrs
	(tot_wds, non_ptrs_w_offsets) = mapAccumL computeOffset wds_of_ptrs non_ptrs
    in
	(tot_wds, wds_of_ptrs, ptrs_w_offsets ++ non_ptrs_w_offsets)
  where
    computeOffset wds_so_far thing
      = (wds_so_far + (getPrimRepSize . kind_fun) thing,
	 (thing, fixedHdrSize + wds_so_far)
	)
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-4-questions]{Four major questions about @ClosureInfo@}
%*									*
%************************************************************************

Be sure to see the stg-details notes about these...

\begin{code}
nodeMustPointToIt :: LambdaFormInfo -> FCode Bool
nodeMustPointToIt lf_info

  = case lf_info of
	LFReEntrant ty top arity no_fvs -> returnFC (
	    not no_fvs ||   -- Certainly if it has fvs we need to point to it
	    isNotTopLevel top
		    -- If it is not top level we will point to it
		    --   We can have a \r closure with no_fvs which
		    --   is not top level as special case cgRhsClosure
		    --   has been dissabled in favour of let floating

		-- For lex_profiling we also access the cost centre for a
		-- non-inherited function i.e. not top level
		-- the  not top  case above ensures this is ok.
	    )

	LFCon	_ zero_arity -> returnFC True
	LFTuple _ zero_arity -> returnFC True

	-- Strictly speaking, the above two don't need Node to point
	-- to it if the arity = 0.  But this is a *really* unlikely
	-- situation.  If we know it's nil (say) and we are entering
	-- it. Eg: let x = [] in x then we will certainly have inlined
	-- x, since nil is a simple atom.  So we gain little by not
	-- having Node point to known zero-arity things.  On the other
	-- hand, we do lose something; Patrick's code for figuring out
	-- when something has been updated but not entered relies on
	-- having Node point to the result of an update.  SLPJ
	-- 27/11/92.

	LFThunk _ _ no_fvs updatable NonStandardThunk
	  -> returnFC (updatable || not no_fvs || opt_SccProfilingOn)

	  -- For the non-updatable (single-entry case):
	  --
	  -- True if has fvs (in which case we need access to them, and we
	  --		    should black-hole it)
	  -- or profiling (in which case we need to recover the cost centre
	  --		 from inside it)

	LFThunk _ _ no_fvs updatable some_standard_form_thunk
	  -> returnFC True
	  -- Node must point to any standard-form thunk.

	LFArgument    -> returnFC True
	LFImported    -> returnFC True
	LFBlackHole _ -> returnFC True
		    -- BH entry may require Node to point

	LFLetNoEscape _ -> returnFC False
\end{code}

The entry conventions depend on the type of closure being entered,
whether or not it has free variables, and whether we're running
sequentially or in parallel.

\begin{tabular}{lllll}
Closure Characteristics & Parallel & Node Req'd & Argument Passing & Enter Via \\
Unknown 			& no & yes & stack	& node \\
Known fun ($\ge$ 1 arg), no fvs 	& no & no  & registers 	& fast entry (enough args) \\
\ & \ & \ & \ 						& slow entry (otherwise) \\
Known fun ($\ge$ 1 arg), fvs	& no & yes & registers 	& fast entry (enough args) \\
0 arg, no fvs @\r,\s@ 		& no & no  & n/a 	& direct entry \\
0 arg, no fvs @\u@ 		& no & yes & n/a 	& node \\
0 arg, fvs @\r,\s@ 		& no & yes & n/a 	& direct entry \\
0 arg, fvs @\u@ 		& no & yes & n/a 	& node \\

Unknown 			& yes & yes & stack	& node \\
Known fun ($\ge$ 1 arg), no fvs 	& yes & no  & registers & fast entry (enough args) \\
\ & \ & \ & \ 						& slow entry (otherwise) \\
Known fun ($\ge$ 1 arg), fvs	& yes & yes & registers & node \\
0 arg, no fvs @\r,\s@ 		& yes & no  & n/a 	& direct entry \\
0 arg, no fvs @\u@ 		& yes & yes & n/a 	& node \\
0 arg, fvs @\r,\s@ 		& yes & yes & n/a 	& node \\
0 arg, fvs @\u@ 		& yes & yes & n/a 	& node\\
\end{tabular}

When black-holing, single-entry closures could also be entered via node
(rather than directly) to catch double-entry.

\begin{code}
data EntryConvention
  = ViaNode				-- The "normal" convention

  | StdEntry CLabel			-- Jump to this code, with args on stack

  | DirectEntry 			-- Jump directly, with args in regs
	CLabel 				--   The code label
	Int 				--   Its arity
	[MagicId]			--   Its register assignments 
					--	(possibly empty)

getEntryConvention :: Name		-- Function being applied
		   -> LambdaFormInfo	-- Its info
		   -> [PrimRep]		-- Available arguments
		   -> FCode EntryConvention

getEntryConvention name lf_info arg_kinds
 =  nodeMustPointToIt lf_info	`thenFC` \ node_points ->
    returnFC (

    -- if we're parallel, then we must always enter via node.  The reason
    -- is that the closure may have been fetched since we allocated it.

    if (node_points && opt_Parallel) then ViaNode else

    -- Commented out by SDM after futher thoughts:
    --   - the only closure type that can be blackholed is a thunk
    --   - we already enter thunks via node (unless the closure is
    --     non-updatable, in which case why is it being re-entered...)

    case lf_info of

	LFReEntrant _ _ arity _ ->
	    if arity == 0 || (listLengthCmp arg_kinds arity == LT) then
		StdEntry (mkStdEntryLabel name)
	    else
		DirectEntry (mkFastEntryLabel name arity) arity arg_regs
	  where
	    (arg_regs, _) = assignRegs live_regs (take arity arg_kinds)
    	    live_regs = if node_points then [node] else []

	LFCon con True{-zero_arity-}
	      -- a real constructor.  Don't bother entering it, just jump
	      -- to the constructor entry code directly.
			  -> --false:ASSERT (null arg_kinds)	
			     -- Should have no args (meaning what?)
			     StdEntry (mkStaticConEntryLabel (dataConName con))

	LFCon con False{-non-zero_arity-}
			  -> --false:ASSERT (null arg_kinds)	
			     -- Should have no args (meaning what?)
			     StdEntry (mkConEntryLabel (dataConName con))

	LFTuple tup zero_arity
			  -> --false:ASSERT (null arg_kinds)	
			     -- Should have no args (meaning what?)
			     StdEntry (mkConEntryLabel (dataConName tup))

	LFThunk _ _ _ updatable std_form_info
	  -> if updatable || opt_DoTickyProfiling  -- to catch double entry
		|| opt_SMP  -- always enter via node on SMP, since the
			    -- thunk might have been blackholed in the 
			    -- meantime.
	     then ViaNode
             else StdEntry (thunkEntryLabel name std_form_info updatable)

	LFArgument    -> ViaNode
	LFImported    -> ViaNode
	LFBlackHole _ -> ViaNode -- Presumably the black hole has by now
				 -- been updated, but we don't know with
				 -- what, so we enter via Node

	LFLetNoEscape 0
	  -> StdEntry (mkReturnPtLabel (nameUnique name))

	LFLetNoEscape arity
	  -> if (not (arg_kinds `lengthIs` arity)) then pprPanic "let-no-escape: " (ppr name <+> ppr arity) else
	     DirectEntry (mkReturnPtLabel (nameUnique name)) arity arg_regs
	 where
	    (arg_regs, _) = assignRegs [] arg_kinds
	    -- node never points to a LetNoEscape, see above --SDM
    	    --live_regs     = if node_points then [node] else []
    )

blackHoleOnEntry :: ClosureInfo -> Bool

-- Static closures are never themselves black-holed.
-- Updatable ones will be overwritten with a CAFList cell, which points to a 
-- black hole;
-- Single-entry ones have no fvs to plug, and we trust they don't form part 
-- of a loop.

blackHoleOnEntry cl_info
  | isStaticRep (closureSMRep cl_info)
  = False	-- Never black-hole a static closure

  | otherwise
  = case closureLFInfo cl_info of
	LFReEntrant _ _ _ _	  -> False
	LFLetNoEscape _		  -> False
	LFThunk _ _ no_fvs updatable _
	  -> if updatable
	     then not opt_OmitBlackHoling
	     else opt_DoTickyProfiling || not no_fvs
                  -- the former to catch double entry,
                  -- and the latter to plug space-leaks.  KSW/SDM 1999-04.

	other -> panic "blackHoleOnEntry"	-- Should never happen

isStandardFormThunk :: LambdaFormInfo -> Bool

isStandardFormThunk (LFThunk _ _ _ _ (SelectorThunk _)) = True
isStandardFormThunk (LFThunk _ _ _ _ (ApThunk _))	= True
isStandardFormThunk other_lf_info 			= False

maybeSelectorInfo (MkClosureInfo { closureLFInfo = LFThunk _ _ _ _ (SelectorThunk offset) }) 
		    = Just offset
maybeSelectorInfo _ = Nothing
\end{code}

-----------------------------------------------------------------------------
SRT-related stuff

\begin{code}
staticClosureNeedsLink :: ClosureInfo -> Bool
-- A static closure needs a link field to aid the GC when traversing
-- the static closure graph.  But it only needs such a field if either
-- 	a) it has an SRT
--	b) it's a constructor with one or more pointer fields
-- In case (b), the constructor's fields themselves play the role
-- of the SRT.
staticClosureNeedsLink (MkClosureInfo { closureName = name, 
					closureSRT = srt, 
					closureLFInfo = lf_info,
					closureSMRep = sm_rep })
  = needsSRT srt || (constr_with_fields && not_nocaf_constr)
  where
    not_nocaf_constr = 
	case sm_rep of 
	   GenericRep _ _ _ CONSTR_NOCAF -> False
	   _other			 -> True

    constr_with_fields =
	case lf_info of
	  LFThunk _ _ _ _ _    -> False
	  LFReEntrant _ _ _ _  -> False
	  LFCon   _ is_nullary -> not is_nullary
	  LFTuple _ is_nullary -> not is_nullary
	  _other	       -> pprPanic "staticClosureNeedsLink" (ppr name)
\end{code}

Avoiding generating entries and info tables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At present, for every function we generate all of the following,
just in case.  But they aren't always all needed, as noted below:

[NB1: all of this applies only to *functions*.  Thunks always
have closure, info table, and entry code.]

[NB2: All are needed if the function is *exported*, just to play safe.]


* Fast-entry code  ALWAYS NEEDED

* Slow-entry code
	Needed iff (a) we have any un-saturated calls to the function
	OR	   (b) the function is passed as an arg
	OR	   (c) we're in the parallel world and the function has free vars
			[Reason: in parallel world, we always enter functions
			with free vars via the closure.]

* The function closure
	Needed iff (a) we have any un-saturated calls to the function
	OR	   (b) the function is passed as an arg
	OR	   (c) if the function has free vars (ie not top level)

  Why case (a) here?  Because if the arg-satis check fails,
  UpdatePAP stuffs a pointer to the function closure in the PAP.
  [Could be changed; UpdatePAP could stuff in a code ptr instead,
   but doesn't seem worth it.]

  [NB: these conditions imply that we might need the closure
  without the slow-entry code.  Here's how.

	f x y = let g w = ...x..y..w...
		in
		...(g t)...

  Here we need a closure for g which contains x and y,
  but since the calls are all saturated we just jump to the
  fast entry point for g, with R1 pointing to the closure for g.]


* Standard info table
	Needed iff (a) we have any un-saturated calls to the function
	OR	   (b) the function is passed as an arg
	OR 	   (c) the function has free vars (ie not top level)

	NB.  In the sequential world, (c) is only required so that the function closure has
	an info table to point to, to keep the storage manager happy.
	If (c) alone is true we could fake up an info table by choosing
	one of a standard family of info tables, whose entry code just
	bombs out.

	[NB In the parallel world (c) is needed regardless because
	we enter functions with free vars via the closure.]

	If (c) is retained, then we'll sometimes generate an info table
	(for storage mgr purposes) without slow-entry code.  Then we need
	to use an error label in the info table to substitute for the absent
	slow entry code.

\begin{code}
staticClosureRequired
	:: Name
	-> StgBinderInfo
	-> LambdaFormInfo
	-> Bool
staticClosureRequired binder bndr_info
		      (LFReEntrant _ top_level _ _)	-- It's a function
  = ASSERT( isTopLevel top_level )
	-- Assumption: it's a top-level, no-free-var binding
	not (satCallsOnly bndr_info)

staticClosureRequired binder other_binder_info other_lf_info = True

slowFunEntryCodeRequired	-- Assumption: it's a function, not a thunk.
	:: Name
	-> StgBinderInfo
	-> EntryConvention
	-> Bool
slowFunEntryCodeRequired binder bndr_info entry_conv
  =    not (satCallsOnly bndr_info)
    || (case entry_conv of { DirectEntry _ _ _ -> False; other -> True })
	    {- The last case deals with the parallel world; a function usually
	       as a DirectEntry convention, but if it doesn't we must generate slow-entry code -}

funInfoTableRequired
	:: Name
	-> StgBinderInfo
	-> LambdaFormInfo
	-> Bool
funInfoTableRequired binder bndr_info (LFReEntrant _ top_level _ _)
  =    isNotTopLevel top_level
    || not (satCallsOnly bndr_info)

funInfoTableRequired other_binder_info binder other_lf_info = True
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-misc-funs]{Misc functions about @ClosureInfo@, etc.}
%*									*
%************************************************************************

\begin{code}

isStaticClosure :: ClosureInfo -> Bool
isStaticClosure cl_info = isStaticRep (closureSMRep cl_info)

closureUpdReqd :: ClosureInfo -> Bool
closureUpdReqd (MkClosureInfo { closureLFInfo = LFThunk _ _ _ upd _ }) = upd
closureUpdReqd (MkClosureInfo { closureLFInfo = LFBlackHole _ })           = True
	-- Black-hole closures are allocated to receive the results of an
	-- alg case with a named default... so they need to be updated.
closureUpdReqd other_closure = False

closureSingleEntry :: ClosureInfo -> Bool
closureSingleEntry (MkClosureInfo { closureLFInfo = LFThunk _ _ _ upd _ }) = not upd
closureSingleEntry other_closure = False

closureReEntrant :: ClosureInfo -> Bool
closureReEntrant (MkClosureInfo { closureLFInfo = LFReEntrant _ _ _ _ }) = True
closureReEntrant other_closure = False
\end{code}

\begin{code}
closureSemiTag :: ClosureInfo -> Maybe Int
closureSemiTag (MkClosureInfo { closureLFInfo = lf_info })
  = case lf_info of
      LFCon data_con _ -> Just (dataConTag data_con - fIRST_TAG)
      LFTuple _ _      -> Just 0
      _	    	       -> Nothing
\end{code}

\begin{code}
isToplevClosure :: ClosureInfo -> Bool

isToplevClosure (MkClosureInfo { closureLFInfo = lf_info })
  = case lf_info of
      LFReEntrant _ TopLevel _ _ -> True
      LFThunk _ TopLevel _ _ _   -> True
      other -> False
\end{code}

Label generation.

\begin{code}
fastLabelFromCI :: ClosureInfo -> CLabel
fastLabelFromCI (MkClosureInfo { closureName = name, closureLFInfo = LFReEntrant _ _ arity _ })
  = mkFastEntryLabel name arity

fastLabelFromCI cl_info
  = pprPanic "fastLabelFromCI" (ppr (closureName cl_info))

infoTableLabelFromCI :: ClosureInfo -> CLabel
infoTableLabelFromCI (MkClosureInfo { closureName = id, closureLFInfo = lf_info, closureSMRep = rep })
  = case lf_info of
	LFCon con _ 	 -> mkConInfoPtr con rep
	LFTuple tup _	 -> mkConInfoPtr tup rep

	LFBlackHole info -> info

	LFThunk _ _ _ upd_flag (SelectorThunk offset) -> 
		mkSelectorInfoLabel upd_flag offset

	LFThunk _ _ _ upd_flag (ApThunk arity) -> 
		mkApInfoTableLabel upd_flag arity

	other -> {-NO: if isStaticRep rep
		 then mkStaticInfoTableLabel id
		 else -} mkInfoTableLabel id

mkConInfoPtr :: DataCon -> SMRep -> CLabel
mkConInfoPtr con rep
  | isStaticRep rep = mkStaticInfoTableLabel  name
  | otherwise	    = mkConInfoTableLabel     name
  where
    name = dataConName con

mkConEntryPtr :: DataCon -> SMRep -> CLabel
mkConEntryPtr con rep
  | isStaticRep rep = mkStaticConEntryLabel (dataConName con)
  | otherwise       = mkConEntryLabel       (dataConName con)

closureLabelFromCI cl_info = mkClosureLabel (closureName cl_info)

entryLabelFromCI :: ClosureInfo -> CLabel
entryLabelFromCI (MkClosureInfo { closureName = id, closureLFInfo = lf_info, closureSMRep = rep })
  = case lf_info of
	LFThunk _ _ _ upd_flag std_form_info -> thunkEntryLabel id std_form_info upd_flag
	LFCon con _			     -> mkConEntryPtr con rep
	LFTuple tup _			     -> mkConEntryPtr tup rep
	other				     -> mkStdEntryLabel id

-- thunkEntryLabel is a local help function, not exported.  It's used from both
-- entryLabelFromCI and getEntryConvention.

thunkEntryLabel thunk_id (ApThunk arity) is_updatable
  = mkApEntryLabel is_updatable arity
thunkEntryLabel thunk_id (SelectorThunk offset) upd_flag
  = mkSelectorEntryLabel upd_flag offset
thunkEntryLabel thunk_id _ is_updatable
  = mkStdEntryLabel thunk_id
\end{code}

\begin{code}
allocProfilingMsg :: ClosureInfo -> FastString

allocProfilingMsg cl_info
  = case closureLFInfo cl_info of
      LFReEntrant _ _ _ _   -> FSLIT("TICK_ALLOC_FUN")
      LFCon _ _		    -> FSLIT("TICK_ALLOC_CON")
      LFTuple _ _	    -> FSLIT("TICK_ALLOC_CON")
      LFThunk _ _ _ True _  -> FSLIT("TICK_ALLOC_UP_THK")  -- updatable
      LFThunk _ _ _ False _ -> FSLIT("TICK_ALLOC_SE_THK")  -- nonupdatable
      LFBlackHole _	    -> FSLIT("TICK_ALLOC_BH")
      LFImported	    -> panic "TICK_ALLOC_IMP"
\end{code}

We need a black-hole closure info to pass to @allocDynClosure@ when we
want to allocate the black hole on entry to a CAF.  These are the only
ways to build an LFBlackHole, maintaining the invariant that it really
is a black hole and not something else.

\begin{code}
cafBlackHoleClosureInfo cl_info
  = MkClosureInfo { closureName   = closureName cl_info,
		    closureLFInfo = LFBlackHole mkCAFBlackHoleInfoTableLabel,
		    closureSMRep  = BlackHoleRep,
		    closureSRT    = NoC_SRT  }

seCafBlackHoleClosureInfo cl_info
  = MkClosureInfo { closureName   = closureName cl_info,
		    closureLFInfo = LFBlackHole mkSECAFBlackHoleInfoTableLabel,
		    closureSMRep  = BlackHoleRep,
		    closureSRT    = NoC_SRT }
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-Profiling-funs]{Misc functions about for profiling info.}
%*									*
%************************************************************************

Profiling requires two pieces of information to be determined for
each closure's info table --- description and type.

The description is stored directly in the @CClosureInfoTable@ when the
info table is built.

The type is determined from the type information stored with the @Id@
in the closure info using @closureTypeDescr@.

\begin{code}
closureTypeDescr :: ClosureInfo -> String
closureTypeDescr cl_info
  = case closureLFInfo cl_info of
	LFThunk ty _ _ _ _   -> getTyDescription ty
	LFReEntrant ty _ _ _ -> getTyDescription ty
	LFCon data_con _     -> occNameUserString (getOccName (dataConTyCon data_con))
	other		     -> showSDoc (ppr (closureName cl_info))
\end{code}
