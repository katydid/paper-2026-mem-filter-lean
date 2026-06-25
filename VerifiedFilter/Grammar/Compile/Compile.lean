import VerifiedFilter.Std.Vector

import VerifiedFilter.Regex.Regex
import VerifiedFilter.Grammar.Grammar
import VerifiedFilter.Regex.Memoize.Memoize

open Regex.Memoize

-- It is possible to calculate a finite list all derivatives up to similarity.
def Regex.derivatives (r: Regex σ): List (Regex σ) := sorry

def Regex.compile [DecidableEq σ] [Hashable σ] [Monad m] [MemoizeKatydid m σ]
  (r: Regex σ): m Unit := do
  _ ← List.mapM (as := Regex.derivatives r) (fun r => do
    _ ← MemoizeKatydid.enterM r
    _ ← Vector.mapM (xs := (Vector.boolCombos (symcount r))) (fun bs => do
      _ ← MemoizeKatydid.leaveM ⟨r, bs⟩))

def Grammar.Compile [DecidableEq φ] [Hashable φ] [Monad m] [MemoizeKatydid m (φ × Ref n)]
  (G: Grammar n φ): m Unit := do
  _ ← Regex.compile G.start
  _ ← Vector.mapM Regex.compile G.prods
