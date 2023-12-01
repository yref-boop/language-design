
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyChar
  | TyTuple of ty list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmChar of char
  | TmFirst of term
  | TmSub of term
  | TmTuple of term list
;;

type binding =
    TyBind of ty
    | TyTmBind of (ty * term)
;;
type context =
    ( string * binding ) list
;;
type command =
    Eval of term
    | Bind of string * term
;;

val emptyctx : context;;
val addbinding : context -> string -> ty -> term -> context;;
val addtbinding : context -> string -> ty -> context;;
val gettbinding : context -> string -> ty;;
val getvbinding : context -> string -> term;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : context -> term -> term;;
val execute : context -> command -> context;;

