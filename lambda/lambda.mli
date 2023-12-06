
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyChar
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyList of ty
  | TyCustom of string
  | TyVariant of (string * ty) list
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
  | TmProj of term * string
  | TmRecord of (string * term) list
  | TmLabel of string * term * string
  | TmCase of term * ((string * string * term) list)
  | TmEmptyList of ty
  | TmList of ty * term * term
  | TmIsEmpty of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
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
    | EvalTy of ty
    | BindTm of string * term
    | BindTy of string * ty
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

