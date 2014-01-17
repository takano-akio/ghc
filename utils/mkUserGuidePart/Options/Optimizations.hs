module Options.Optimizations where

import Types

optimizationsOptions :: [Flag]
optimizationsOptions =
  [ flag { flagName = "-fcall-arity"
         , flagDescription =
           "Enable call-arity optimisation. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-call-arity"
         }
  , flag { flagName = "-fcase-merge"
         , flagDescription = "Enable case-merging. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-case-merge"
         }
  , flag { flagName = "-fcase-folding"
         , flagDescription = "Enable constant folding in case expressions. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-case-folding"
         }
  , flag { flagName = "-fcmm-elim-common-blocks"
         , flagDescription =
           "Enable Cmm common block elimination. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-cmm-elim-common-blocks"
         }
  , flag { flagName = "-fcmm-sink"
         , flagDescription = "Enable Cmm sinking. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-cmm-sink"
         }
  , flag { flagName = "-fcpr-anal"
         , flagDescription =
           "Turn on CPR analysis in the demand analyser. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-cpr-anal"
         }
  , flag { flagName = "-fcpr-depth=⟨n⟩"
         , flagDescription =
           "*default: 3.* Analyze the result of functions for constructed " ++
            "product results to that depth. Setting this to zero disables " ++
            "CPR, setting this to one disables nested CPR."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fcse"
         , flagDescription =
           "Enable common sub-expression elimination. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-cse"
         }
  , flag { flagName = "-fdicts-cheap"
         , flagDescription =
           "Make dictionary-valued expressions seem cheap to the optimiser."
         , flagType = DynamicFlag
         , flagReverse = "-fno-dicts-cheap"
         }
  , flag { flagName = "-fdicts-strict"
         , flagDescription = "Make dictionaries strict"
         , flagType = DynamicFlag
         , flagReverse = "-fno-dicts-strict"
         }
  , flag { flagName = "-fdmd-tx-dict-sel"
         , flagDescription =
           "Use a special demand transformer for dictionary selectors. "++
           "Always enabled by default."
         , flagType = DynamicFlag
         , flagReverse = "-fno-dmd-tx-dict-sel"
         }
  , flag { flagName = "-fdo-eta-reduction"
         , flagDescription = "Enable eta-reduction. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-do-eta-reduction"
         }
  , flag { flagName = "-fdo-lambda-eta-expansion"
         , flagDescription =
           "Enable lambda eta-expansion. Always enabled by default."
         , flagType = DynamicFlag
         , flagReverse = "-fno-do-lambda-eta-expansion"
         }
  , flag { flagName = "-feager-blackholing"
         , flagDescription =
           "Turn on :ref:`eager blackholing <parallel-compile-options>`"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fenable-rewrite-rules"
         , flagDescription =
           "Switch on all rewrite rules (including rules generated by "++
           "automatic specialisation of overloaded functions). Implied by "++
           ":ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-enable-rewrite-rules"
         }
  , flag { flagName = "-fexcess-precision"
         , flagDescription = "Enable excess intermediate precision"
         , flagType = DynamicFlag
         , flagReverse = "-fno-excess-precision"
         }
  , flag { flagName = "-fexpose-all-unfoldings"
         , flagDescription =
           "Expose all unfoldings, even for very large or recursive functions."
         , flagType = DynamicFlag
         , flagReverse = "-fno-expose-all-unfoldings"
         }
  , flag { flagName = "-ffloat-in"
         , flagDescription =
           "Turn on the float-in transformation. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-float-in"
         }
  , flag { flagName = "-ffull-laziness"
         , flagDescription =
           "Turn on full laziness (floating bindings outwards). "++
           "Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-full-laziness"
         }
  , flag { flagName = "-ffun-to-thunk"
         , flagDescription =
           "Allow worker-wrapper to convert a function closure into a thunk "++
           "if the function does not use any of its arguments. Off by default."
         , flagType = DynamicFlag
         , flagReverse = "-fno-fun-to-thunk"
         }
  , flag { flagName = "-fignore-asserts"
         , flagDescription =
           "Ignore assertions in the source. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-ignore-asserts"
         }
  , flag { flagName = "-fignore-interface-pragmas"
         , flagDescription =
           "Ignore pragmas in interface files. Implied by :ghc-flag:`-O0` only."
         , flagType = DynamicFlag
         , flagReverse = "-fno-ignore-interface-pragmas"
         }
  , flag { flagName = "-flate-dmd-anal"
         , flagDescription =
           "Run demand analysis again, at the end of the "++
           "simplification pipeline"
         , flagType = DynamicFlag
         , flagReverse = "-fno-late-dmd-anal"
         }
  , flag { flagName = "-fliberate-case"
         , flagDescription =
           "Turn on the liberate-case transformation. Implied by :ghc-flag:`-O2`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-liberate-case"
         }
  , flag { flagName = "-fliberate-case-threshold=⟨n⟩"
         , flagDescription =
           "*default: 2000.* Set the size threshold for the liberate-case "++
           "transformation to ⟨n⟩"
         , flagType = DynamicFlag
         , flagReverse = "-fno-liberate-case-threshold"
         }
  , flag { flagName = "-floopification"
         , flagDescription =
           "Turn saturated self-recursive tail-calls into local jumps in the "++
           "generated assembly. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-loopification"
         }
  , flag { flagName = "-fmax-inline-alloc-size=⟨n⟩"
         , flagDescription =
           "*default: 128.* Set the maximum size of inline array allocations "++
           "to ⟨n⟩ bytes (default: 128). GHC will allocate non-pinned arrays "++
           "of statically known size in the current nursery block if they're "++
           "no bigger than ⟨n⟩ bytes, ignoring GC overheap. This value should "++
           "be quite a bit smaller than the block size (typically: 4096)."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fmax-inline-memcpy-insns=⟨n⟩"
         , flagDescription =
           "*default: 32.* Inline ``memcpy`` calls if they would generate no "++
           "more than ⟨n⟩ pseudo instructions."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fmax-inline-memset-insns=⟨n⟩"
         , flagDescription =
           "*default: 32.* Inline ``memset`` calls if they would generate no "++
           "more than ⟨n⟩ pseudo instructions"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fmax-relevant-binds=⟨n⟩"
         , flagDescription =
           "*default: 6.* Set the maximum number of bindings to display in "++
           "type error messages."
         , flagType = DynamicFlag
         , flagReverse = "-fno-max-relevant-bindings"
         }
  , flag { flagName = "-fmax-valid-substitutions=⟨n⟩"
         , flagDescription =
           "*default: 6.* Set the maximum number of valid substitutions for"++
           "typed holes to display in type error messages."
         , flagType = DynamicFlag
         , flagReverse = "-fno-max-valid-substitutions"
         }
  , flag { flagName = "-fmax-uncovered-patterns=⟨n⟩"
         , flagDescription =
           "*default: 4.* Set the maximum number of patterns to display in "++
           "warnings about non-exhaustive ones."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fmax-simplifier-iterations=⟨n⟩"
         , flagDescription =
           "*default: 4.* Set the max iterations for the simplifier."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fmax-worker-args=⟨n⟩"
         , flagDescription =
           "*default: 10.* If a worker has that many arguments, none will "++
           "be unpacked anymore."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fno-opt-coercion"
         , flagDescription = "Turn off the coercion optimiser"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fno-pre-inlining"
         , flagDescription = "Turn off pre-inlining"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fno-state-hack"
         , flagDescription =
           "Turn off the \"state hack\" whereby any lambda with a real-world "++
           "state token as argument is considered to be single-entry. Hence "++
           "OK to inline things inside it."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fomit-interface-pragmas"
         , flagDescription =
           "Don't generate interface pragmas. Implied by :ghc-flag:`-O0` only."
         , flagType = DynamicFlag
         , flagReverse = "-fno-omit-interface-pragmas"
         }
  , flag { flagName = "-fomit-yields"
         , flagDescription =
           "Omit heap checks when no allocation is being performed."
         , flagType = DynamicFlag
         , flagReverse = "-fno-omit-yields"
         }
  , flag { flagName = "-foptimal-applicative-do"
         , flagDescription =
           "Use a slower but better algorithm for ApplicativeDo"
         , flagType = DynamicFlag
         , flagReverse = "-fno-optimal-applicative-do"
         }
  , flag { flagName = "-fpedantic-bottoms"
         , flagDescription =
           "Make GHC be more precise about its treatment of bottom (but see "++
           "also :ghc-flag:`-fno-state-hack`). In particular, GHC will not "++
           "eta-expand through a case expression."
         , flagType = DynamicFlag
         , flagReverse = "-fno-pedantic-bottoms"
         }
  , flag { flagName = "-fregs-graph"
         , flagDescription =
           "Use the graph colouring register allocator for register "++
           "allocation in the native code generator. Implied by :ghc-flag:`-O2`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-regs-graph"
         }
  , flag { flagName = "-fregs-iterative"
         , flagDescription =
           "Use the iterative coalescing graph colouring register allocator "++
           "in the native code generator."
         , flagType = DynamicFlag
         , flagReverse = "-fno-regs-iterative"
         }
  , flag { flagName = "-fsimplifier-phases=⟨n⟩"
         , flagDescription =
           "*default: 2.* Set the number of phases for the simplifier. "++
           "Ignored with :ghc-flag:`-O0`."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fsimpl-tick-factor=⟨n⟩"
         , flagDescription =
           "*default: 100.* Set the percentage factor for simplifier ticks."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fspec-constr"
         , flagDescription =
           "Turn on the SpecConstr transformation. Implied by :ghc-flag:`-O2`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-spec-constr"
         }
  , flag { flagName = "-fspec-constr-count=⟨n⟩"
         , flagDescription =
           "default: 3.* Set to ⟨n⟩ the maximum number of specialisations that"++
           " will be created for any one function by the SpecConstr "++
           "transformation."
         , flagType = DynamicFlag
         , flagReverse = "-fno-spec-constr-count"
         }
  , flag { flagName = "-fspec-constr-threshold=⟨n⟩"
         , flagDescription =
           "*default: 2000.* Set the size threshold for the SpecConstr "++
           "transformation to ⟨n⟩."
         , flagType = DynamicFlag
         , flagReverse = "-fno-spec-constr-threshold"
         }
  , flag { flagName = "-fspecialise"
         , flagDescription =
           "Turn on specialisation of overloaded functions. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-specialise"
         }
  , flag { flagName = "-fcross-module-specialise"
         , flagDescription =
           "Turn on specialisation of overloaded functions imported from "++
           "other modules."
         , flagType = DynamicFlag
         , flagReverse = "-fno-cross-module-specialise"
         }
  , flag { flagName = "-fstatic-argument-transformation"
         , flagDescription = "Turn on the static argument transformation."
         , flagType = DynamicFlag
         , flagReverse = "-fno-static-argument-transformation"
         }
  , flag { flagName = "-fstrictness"
         , flagDescription = "Turn on strictness analysis." ++
           " Implied by :ghc-flag:`-O`. Implies :ghc-flag:`-fworker-wrapper`"
         , flagType = DynamicFlag
         , flagReverse = "-fno-strictness"
         }
  , flag { flagName = "-fstrictness-before=⟨n⟩"
         , flagDescription =
           "Run an additional strictness analysis before simplifier phase ⟨n⟩"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-funbox-small-strict-fields"
         , flagDescription =
           "Flatten strict constructor fields with a pointer-sized "++
           "representation. Implied by :ghc-flag:`-O`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-unbox-small-strict-fields"
         }
  , flag { flagName = "-funbox-strict-fields"
         , flagDescription = "Flatten strict constructor fields"
         , flagType = DynamicFlag
         , flagReverse = "-fno-unbox-strict-fields"
         }
  , flag { flagName = "-funfolding-creation-threshold=⟨n⟩"
         , flagDescription = "*default: 750.* Tweak unfolding settings."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-funfolding-dict-discount=⟨n⟩"
         , flagDescription = "*default: 30.* Tweak unfolding settings."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-funfolding-fun-discount=⟨n⟩"
         , flagDescription = "*default: 60.* Tweak unfolding settings."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-funfolding-keeness-factor=⟨n⟩"
         , flagDescription = "*default: 1.5.* Tweak unfolding settings."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-funfolding-use-threshold=⟨n⟩"
         , flagDescription = "*default: 60.* Tweak unfolding settings."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fvectorisation-avoidance"
         , flagDescription =
           "Enable vectorisation avoidance. Always enabled by default."
         , flagType = DynamicFlag
         , flagReverse = "-fno-vectorisation-avoidance"
         }
  , flag { flagName = "-fvectorise"
         , flagDescription = "Enable vectorisation of nested data parallelism"
         , flagType = DynamicFlag
         , flagReverse = "-fno-vectorise"
         }
  , flag { flagName = "-fworker-wrapper"
         , flagDescription =
           "Enable the worker-wrapper transformation after a strictness" ++
           " analysis pass. Implied by :ghc-flag:`-O`, and by :ghc-flag:`-fstrictness`." ++
           " Disabled by :ghc-flag:`-fno-strictness`. Enabling :ghc-flag:`-fworker-wrapper`" ++
           " while strictness analysis is disabled (by :ghc-flag:`-fno-strictness`)" ++
           " has no effect."
         , flagType = DynamicFlag
         , flagReverse = "-fno-worker-wrapper"
         }
  ]
