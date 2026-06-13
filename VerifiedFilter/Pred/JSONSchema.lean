import VerifiedFilter.Parser.Token

namespace Pred.JSONSchema

inductive Pred: Type where
  | any
  | eq (t: Token)
  | string
  | strEq (s: String)
  | tagEq (s: String)
  | or (p1 p2: Pred)
  | not (p1: Pred)
  | datetime
  | email
  | isObject
  deriving DecidableEq, Ord, Repr, Hashable

def Pred.evalb (p: Pred) (x: Token): Bool :=
  match p with
  | Pred.any => true
  | Pred.eq y => x = y
  | Pred.string => match x with
    | Token.string _ => true
    | _ => false
  | Pred.datetime => false -- TODO: parse datetime
  | Pred.email => true -- TODO: parse email
  | Pred.isObject => match x with
    | Token.tag "object" => true
    | _ => false
  | Pred.strEq s => match x with
    | Token.string s' => s == s'
    | _ => false
  | Pred.tagEq s => match x with
    | Token.tag s' => s == s'
    | _ => false
  | Pred.or p1 p2 =>
    p1.evalb x || p2.evalb x
  | Pred.not p1 =>
    !p1.evalb x
