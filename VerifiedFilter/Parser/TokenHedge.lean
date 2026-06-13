-- TokenHedge is used for testing to easily create a `Hedge Token` values,
-- that can be walked or validated with the Parser.

import VerifiedFilter.Std.Hedge
import VerifiedFilter.Parser.Token

abbrev TokenNode := Hedge.Node Token

abbrev TokenHedge := Hedge Token

def TokenNode.node (t: Token) (children: TokenHedge): TokenNode :=
  Hedge.Node.node t children

namespace TokenHedge

def strnode (s: String) (children: TokenHedge): TokenNode :=
  Hedge.Node.node (Token.string s) children

def str (s: String): TokenNode :=
  Hedge.Node.node (Token.string s) []

-- Examples

open Hedge.Node
open Token

-- The following json: `{"a": ["b", "c"]}`
-- is parsed as: `{"object": {"a": {"array": {0: "b", 1: "c"}}}}`.

example : Hedge Token :=
  [node (tag "object") [
    node (string "a") [
      node (tag "array") [
        node (int64 0) [node (string "b") []],
        node (int64 1) [node (string "c") []]]]]]
