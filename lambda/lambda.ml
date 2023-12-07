open Format;;
open Option;;

(* TYPE DEFINITIONS *)

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
  (* Lists *)
  | TmEmptyList of ty
  | TmList of ty * term * term
  | TmIsEmpty of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
;;

type command =
    Eval of term
  | EvalTy of ty
  | BindTm of string * term
  | BindTy of string * ty
;;

type binding =
    TyBind of ty
  | TyTmBind of (ty * term)
;;

type context =
  (string * binding) list
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addtbinding ctx s ty =
  (s, TyBind ty) :: ctx
;;

let addbinding ctx s ty tm =
  (s, TyTmBind (ty, tm)) :: ctx
;;

let gettbinding ctx s =
  match List.assoc s ctx with
      TyBind ty -> ty
    | TyTmBind (ty, _) -> ty
;;

let getvbinding ctx s =
  match List.assoc s ctx with
      TyTmBind (_, tm) -> tm
    | _ -> raise Not_found
;;

(* TYPE MANAGEMENT (TYPING) *)

exception Type_error of string
;;

let rec subtype tm1 tm2 = match (tm1, tm2) with
  (TyArr(s1, s2), TyArr(t1, t2)) ->
    ((subtype s1 t1) && (subtype s2 t2))
  | (TyRecord(r1), TyRecord(r2)) ->
    let rec contains l1 l2 = match l1 with
      [] -> true
      | ((x,ty)::t) ->
          (&&) (try subtype ty (List.assoc x l2) with _ -> false) (contains t l2)
    in contains r1 r2
  | (tm1, tm2) -> tm1=tm2
;;

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      string_of_ty ty1 ^ " -> " ^ string_of_ty ty2
  | TyString ->
      "String"
  | TyChar ->
      "Char"
  | TyTuple fields ->
    let types = String.concat ", " (List.map (fun t -> string_of_ty t) fields) in
    "{" ^ types ^ "}"
  | TyRecord fields ->
    let rec aux list = match list with
      (i, h) :: [] -> i ^ " : " ^ string_of_ty h
      | (i, h) :: t -> (i ^ " : " ^ string_of_ty h ^ ", ") ^ aux t
      | [] -> ""
    in "{" ^ aux fields ^ "}"
  | TyList ty -> "List [" ^ string_of_ty ty ^ "]"
  | TyCustom str -> str 
  | TyVariant fields ->
    let rec aux list = match list with
      (i, h) :: [] -> i ^ " : " ^ string_of_ty h
      | (i, h) :: t -> (i ^ " : " ^ string_of_ty h ^ ", ") ^ aux t
      | [] -> ""
    in "<" ^ aux fields ^ ">"
;;

let rec to_basic_type ctx strty = match strty with
    TyBool -> TyBool
  | TyNat -> TyNat
  | TyString -> TyString
  | TyChar -> TyChar
  | TyArr (ty1, ty2) -> TyArr (to_basic_type ctx ty1, to_basic_type ctx ty2)
  | TyTuple (tyList) -> let f ty = to_basic_type ctx ty in TyTuple (List.map f tyList)
  | TyRecord (tyPairList) -> let f (var, ty) = (var, to_basic_type ctx ty) in TyRecord (List.map f tyPairList)
  | TyList (ty) -> TyList (to_basic_type ctx ty)
  | TyCustom (var) -> gettbinding ctx var
  | TyVariant (tyPairList) -> let f (str, ty) = (str, to_basic_type ctx ty) in TyVariant (List.map f tyPairList)
;;

let rec print_type = function
  TyArr (t1, t2) ->
    open_box 1;
    print_atomic_type t1;
    close_box();
    print_string " ->";
    print_space ();
    open_box 1;
    print_type t2;
    close_box ();
    ()
  | TyList ty ->
    print_string "List ";
    open_box 1;
    print_char '[';
    print_type ty;
    print_char ']';
    close_box ();
    ()
  | ty -> print_atomic_type ty;

and print_atomic_type = function
  TyBool -> print_string "Bool"; ()
  | TyNat -> print_string "Nat"; ()
  | TyString -> print_string "String"; ()
  | TyChar -> print_string "Char"; ()
  | TyCustom ty -> print_string ty; ()
  (* TODO: debug polimorfic case *)
  | TyTuple [] -> print_string "{}"
  | TyTuple l ->
    let rec aux = function
      [] -> ()
      | (ty):: [] ->
        print_type ty;
        ()
      | (ty) :: t ->
        print_type ty;
        print_char ',';
        print_space ();
        aux t;
    in
      print_char '{';
      open_box 1;
      aux l;
      close_box ();
      print_char '}';
      ()
  | TyRecord [] -> print_string "{}"
  (*TyRecord l when isnat (fst (List.hd l)) -> TODO: entender este caso*)
  | TyRecord l ->
    let rec aux = function
      [] -> ()
      | (fn, ty):: [] ->
        print_string fn;
        print_char '=';
        print_type ty;
        ()
      | (fn, ty) :: t ->
        print_string fn;
        print_char '=';
        print_type ty;
        print_char ',';
        print_space ();
        aux t;
    in
      print_char '{';
      open_box 1;
      aux l;
      close_box ();
      print_char '}';
        ()
  | TyVariant l ->
      let rec aux = function
        [] -> ()
        | (st, ty) :: [] ->
            print_string st;
            print_string " : ";
            print_type ty;
            ()
        | (st, ty) :: t ->
            print_string st;
            print_string " : ";
            print_type ty;
            print_char ',';
            print_space ();
            aux t;
      in
        print_char '<';
        open_box 1;
        aux l;
        close_box ();
        print_char '>';


  (* TODO: polimoric types fall here on loop *)
  | ty ->
    print_char '(';
    open_box 1;
    print_type ty;
    close_box ();
    print_char ')';
    ()
;;

let rec typeof ctx tm = match tm with

    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let tyBase = to_basic_type ctx tyT1 in
      let ctx' = addtbinding ctx x tyBase in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyBase, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if subtype tyT11 tyT2 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
      typeof ctx' t2

    (* T-Fix *)
  | TmFix t1 ->
    let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyArr (tyT11, tyT12) ->
          if subtype tyT11 tyT12 then tyT12
          else raise (Type_error "result body not compatible with domain")
      | _ -> raise (Type_error "arrow type expected"))

    (* new string rules *)
  | TmString t1 ->
    TyString

  | TmConcat (t1, t2) ->
    if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
    else raise (Type_error "argument of concat is not a string")

  | TmChar t ->
    TyChar

  | TmFirst t ->
    if typeof ctx t = TyString then TyChar
    else raise (Type_error "argument of concat is not a string")

  | TmSub t ->
    if typeof ctx t = TyString then TyString
    else raise (Type_error "argument of sub is not a string")

  | TmTuple fields ->
      TyTuple (List.map (fun t -> typeof ctx t) fields)

  | TmRecord fields ->
      let f (li, ti) = (li, typeof ctx ti) in TyRecord (List.map f fields)

  | TmProj (field, s) ->
    (match typeof ctx field with
      TyTuple fieldtys ->
        (try let element = List.nth fieldtys (int_of_string s - 1) in element with
          _ -> raise (Type_error ("Index " ^ s ^ " not found (type)")))
      | TyRecord fieldtys ->
        (try let ty = List.assoc s fieldtys in ty with
          _ -> raise (Type_error ("Label " ^ s ^ " not found (type)")))
      | _ -> raise (Type_error ("Unexpected type")))


    (*let t' = eval1 ctx t in
    let var' = gettbinding ctx var in
    (*print_endline("Testing...  The label is " ^ s ^ " and the type is " ^ string_of_ty var');*)
    (* Assuming var' is a list of pairs (string * type list) *)
    let f ty l = match ty with
    | TyVariant tyList when List.exists ((=) l) (List.map fst tyList) ->
      (*print_endline("Testing...  The label is " ^ l ^ " and the list is " ^ string_of_ty ty);*)
      TmLabel (s, t', var)
    | _ ->
        raise (Type_error "Variable is not of type variant or label doesn't match any label.")
    in f var' s*)

  | TmLabel (s, t, var) -> 
      let newTy = gettbinding ctx var in
      let f ty l = match ty with
          TyVariant tyList ->
            let matchingType = List.assoc_opt s tyList in 
            let checkingType matchTy = match matchTy with
            | Some(labelType) ->
                let typeOfT = typeof ctx t in
                if labelType = typeOfT then ty
                else raise (Type_error "Type mismatch between label type and type of 't'.")
            | None -> raise (Type_error "Label doesn't match any label in variant.")
            in checkingType matchingType
          | _ -> raise (Type_error "Type invalid for invariant.")   
      in f newTy s

  (* LISTAS *)
  | TmEmptyList ty -> TyList (ty)
  | TmList (ty,h,t) ->
        let tyTh = typeof ctx h in
        let tyTt = typeof ctx t in
           if (subtype tyTh ty) && (subtype tyTt (TyList(ty))) then 
              TyList(ty) else raise (Type_error "elements of list have different types")

  | TmIsEmpty (ty,t) ->
      if typeof ctx t = TyList(ty) then TyBool
      else raise (Type_error ("Argument is not a " ^ "List [" ^ (string_of_ty ty) ^ ".]"))

     (* T-Head *)
  | TmHead (ty,t) ->
      if typeof ctx t = TyList(ty) then ty
      else raise (Type_error ("Argument is not a " ^ "List [" ^ (string_of_ty ty) ^ ".]"))

  | TmTail (ty,t) ->
      if typeof ctx t = TyList(ty) then TyList(ty)
      else raise (Type_error ("Argument is not a " ^ "List [" ^ (string_of_ty ty) ^ ".]"))
;;

(* TERMS MANAGEMENT (EVALUATION) *)

      (*TODO: chica date cuenta*)
(*cascada de funciones guiadas por la gramatica:
    estudiar el parser y por cada no terminal ir
    construyendo 

    se van llamando unas a otras a medida que se
    profundiza en la expresion*)

let rec print_term =
  let rec print_sequential = function
    TmApp(TmAbs (_, _, t2), t1) ->
      open_box 1;
        print_term t1;
      close_box ();
      print_char ';';
      print_space ();
      print_sequential t2;
      ()
    | tm ->
      open_box 1;
        print_tm tm;
      close_box();
      ()

  and print_tm = function
    TmIf (t1, t2, t3) ->
        print_string "if";
      print_space ();
      open_box 1;
        print_tm t1;
      close_box ();
      print_space ();
      print_string "then";
      print_space ();
      open_box 1;
        print_tm t2;
      close_box ();
      print_space ();
      print_string "else";
      print_space ();
      open_box 1;
        print_tm t3;
      close_box ();
      ()
    | TmAbs (s, ty, t) ->
      open_box 1;
        print_string "lambda";
        print_space ();
        print_string s;
        print_char ':';
        print_type ty;
        print_char '.';
        print_space ();
        print_term t;
      close_box ();
      ()
    | TmLetIn (s, t1, t2) ->
      print_string "let";
      print_space ();
      print_string s;
      print_space ();
      print_char '=';
      print_space ();
      open_box 1;
        print_tm t1;
      close_box ();
      print_space ();
      print_string "in";
      print_space ();
      open_box 1;
        print_tm t2;
      close_box ();
      ()
    | TmFix t ->
      open_box 1;
        print_string "fix";
        print_space ();
        print_tm t;
      close_box ();
      ()
    | tm -> print_app tm; ()

    (*applications*)
  and print_app = function
    TmApp (t1, t2) ->
      open_box 1;
        print_projection t1;
        print_space ();
        print_projection t2;
      close_box ();
      ()
    | TmPred t ->
      print_string "pred";
      print_space ();
      open_box 1;
        print_projection t;
      close_box ();
      ()
    | TmIsZero t ->
      print_string "iszero";
      print_space ();
      open_box 1;
        print_projection t;
      close_box ();
      ()
    | TmConcat (t1, t2) ->
      open_box 1;
        print_concat t1;
      close_box ();
      print_space ();
      print_char '^';
      print_space ();
      open_box 1;
        print_projection t2;
      close_box ();
      ()
    | TmList (_, _, _) as tm ->
        let print_cons  =
          let rec aux = function
            | TmList (_, t1, TmEmptyList _) ->
                print_projection t1;
                ()
            | TmList (_, t1, t2) ->
                print_projection t1;
                print_char ',';
                print_space ();
                aux t2
            | t ->
              print_projection t
          in function
               TmEmptyList ty ->
                print_string "[]";
                ()
            | TmList (ty, t1, TmEmptyList _) ->
                print_char '[';
                open_box 1;
                  print_projection t1;
                close_box ();
                ()
            | TmList (ty, _, _) as t ->
                print_char '[';
                open_box 1;
                  aux t;
                close_box ();
                print_char ']';
                ()
            | t ->
                print_projection t;
                ()
        in
          print_cons tm;
          ()
    | TmIsEmpty (ty, t) ->
      print_string "isEmpty";
      print_cut ();
      print_char '[';
      print_type ty;
      print_char ']';
      print_space ();
      open_box 1;
        print_projection t;
      close_box ();
      ()
    | TmHead (ty, t) ->
      print_string "head";
      print_cut ();
      print_char '[';
      print_type ty;
      print_char ']';
      print_space ();
      open_box 1;
        print_projection t;
      close_box ();
      ()
    | TmTail (ty, t) ->
      print_string "tail";
      print_cut ();
      print_char '[';
      print_type ty;
      print_char ']';
      print_space ();
      open_box 1;
        print_projection t;
      close_box ();
      ()
    | TmLabel (st1, tm, st2) ->
      print_char '<';
      print_string st1;
      print_string " = ";
      open_box 1;
        print_term tm;
      close_box ();
      print_char '>';
      ()
    | tm -> print_projection tm; ()

  and print_concat = function
    TmConcat (t1, t2) ->
      open_box 1;
        print_concat t1;
      close_box ();
      print_space ();
      print_char '^';
      print_space ();
      open_box 1;
        print_projection t2;
      close_box ();
      ()
  | tm ->
      open_box 1;
        print_projection tm;
      close_box ();
      ()

    (*projections*)
  (* TODO: debug polimorfic case *)
  and print_projection = function
    TmProj (t1, fn) ->
      open_box 1;
        print_projection t1;
      close_box ();
      print_char '.';
      print_string fn;
    | TmRecord [] as tm -> print_atomic tm
    (* tmRecord if isnar  TODO: understand this case *)
    | TmRecord list ->
      let rec aux = function
        [] ->
          ()
        | (fn,tm)::[] ->
          print_string fn;
          print_char '=';
          print_projection tm;
          ()
        | (fn,tm)::t ->
          print_string fn;
          print_char '=';
          print_projection tm;
          print_char ',';
          print_space ();
          aux t;
      in
        print_char '{';
        open_box 1;
          aux list;
        close_box ();
        print_char '}';
        ()
    | TmTuple [] as tm -> print_atomic tm
    (* tmRecord if isnar  TODO: understand this case *)
    | TmTuple list ->
      let rec aux = function
        [] ->
          ()
        | tm::[] ->
          print_projection tm;
          ()
        | tm::t ->
          print_projection tm;
          print_char ',';
          print_space ();
          aux t;
      in
        print_char '{';
        open_box 1;
          aux list;
        close_box ();
        print_char '}';
        ()
    | tm -> print_atomic tm; ();

    (*atomic values*)
  and print_atomic = function
    TmTrue -> print_string "true"; ()
    | TmChar c -> print_char c; ()
    | TmFalse -> print_string "false"; ()
    | TmZero -> print_char '0'; ()
    | TmVar s -> print_string s; ()
    | TmSucc t ->
      let rec f n t' = match t' with
          TmZero -> print_int n; ()
        | TmSucc s -> f (n+1) s
        | _ ->
          print_string "succ";
          print_space ();
          open_box 1;
            print_projection t;
          close_box ();
          ()
      in f 1 t
    | TmString s -> print_string s; ()
    | TmEmptyList t -> print_string "[]"; ()
    | TmRecord [] -> print_string "{}"; ()
    (* TODO: polimorfic types fall here on loop*)
    | tm ->
      print_char '(';
      open_box 1;
        print_sequential tm;
      close_box ();
      print_char ')';
      ()
  in fun tm -> print_sequential tm
;;

(*
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
  (* Lists *)
  | TmEmptyList of ty
  | TmList of ty * term * term
  | TmIsEmpty of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
 *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      "var" ^ s
  | TmAbs (s, tyS, t) ->
      "\n(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "app(" ^ "t1" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
  | TmString s ->
    "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
    "concat " ^ "(" ^ string_of_term t1 ^ ")" ^ " " ^ "(" ^ string_of_term t2 ^ ")"
  | TmChar c ->
    "\'" ^ String.make 1 c ^ "\'"
  | TmFirst t ->
    "first " ^ "(" ^ "\"" ^ string_of_term t ^ "\"" ^ ")"
  | TmSub t ->
    "sub " ^ "(" ^ "\"" ^ string_of_term t ^ "\"" ^ ")"
  | TmTuple fields ->
    let terms = String.concat ", " (List.map (fun t -> string_of_term t) fields) in
    "{" ^ terms ^ "}"
  | TmRecord fields ->
    let rec aux list = match list with
      [] -> ""
      | [(i, h)] -> i ^ " : " ^ string_of_term h
      | (i, h)::t -> i ^ " : " ^ string_of_term h ^ ", " ^ aux t
    in "{" ^ aux fields ^ "}"
  | TmProj (t, s) ->   
    "p(" ^ s ^ ")" ^ "of" ^ string_of_term t
  | TmLabel (s, t, _) ->
    "<" ^ s ^ " : " ^ string_of_term t ^ ">"
    (* LISTS *)
  | TmEmptyList ty -> "[]"
  | TmList (ty,h,t) -> 
    let rec string_of_list lst = match lst with
      | TmEmptyList ty -> ""
      | TmList(ty,h,(TmEmptyList t)) -> string_of_term h
      | TmList(ty,h,t) -> string_of_term h ^ ", " ^ string_of_list t
      | t -> string_of_term t
    in "[" ^ string_of_term h ^ ", " ^ string_of_list t ^ "]"
  | TmIsEmpty (ty,t) -> "IsEmpty? List [" ^ string_of_ty ty ^ "] : [" ^ string_of_term t ^ "]" 
  | TmHead (ty,t) -> "Head :"  ^ string_of_term t
  | TmTail (ty,t) -> 
    let rec string_of_list lst = match lst with
      | TmEmptyList ty -> ""
      | TmList(ty,h,(TmEmptyList t)) -> string_of_term h
      | TmList(ty,h,t) -> string_of_term h ^ ", " ^ string_of_list t
      | t -> string_of_term t
    in "Tail: [" ^ string_of_list t ^ "]"
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmString _ ->
      []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmChar _ ->
      []
  | TmFirst t ->
      free_vars t
  | TmSub t ->
      free_vars t
  | TmTuple fields -> 
      List.fold_left (fun fv ti -> lunion (free_vars ti) fv) [] fields    
  | TmRecord t ->
    let rec aux list = match list with
      | (i, h)::[] -> free_vars h
      | (i, h)::t -> lunion (free_vars h) (aux t)
      | [] -> []
    in aux t
  | TmProj (t, s) ->
    (match t with
      TmTuple fields -> 
        (try let element = List.nth fields (int_of_string s - 1) in free_vars element with 
          _ -> 
            raise (Type_error ("Index " ^ s ^ " not found (term)")))
      | TmRecord fields ->
        (try let element = List.assoc s fields in free_vars element with
          _ ->
            raise (Type_error ("Label " ^ s ^ " not found (term)")))
      | _ -> 
        raise(Type_error("Unexpected type of term")))

  | TmLabel (_, t, _) -> free_vars t
    
  (* LISTS *) 
  | TmEmptyList ty -> []
  | TmList (ty,t1,t2) -> lunion (free_vars t1) (free_vars t2)
  | TmIsEmpty (ty,t) -> free_vars t
  | TmHead (ty,t) -> free_vars t
  | TmTail (ty,t) -> free_vars t    
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2) 
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmString st ->
      TmString st
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmChar c ->
      TmChar c
  | TmFirst t ->
      TmFirst (subst x s t)
  | TmSub t ->
      TmSub (subst x s t)
  | TmTuple fields ->
      TmTuple (List.map (fun t1 -> subst x s t1) fields)    
  | TmRecord fields -> 
    let f (li, ti) = (li, subst x s ti) in
      TmRecord (List.map f fields)    
  | TmProj (tuple, str) ->
    (match typeof emptyctx tuple with
      TyTuple fields ->  subst x s tuple
      | _ ->  raise(Type_error("Tuple type expected (3)")))
  | TmLabel (str, t, var) ->
    TmLabel (str, subst x s t, var)
    (* LISTS *)  
  | TmEmptyList ty -> tm
  | TmList (ty,t1,t2) -> TmList (ty, (subst x s t1), (subst x s t2))
  | TmIsEmpty (ty,t) -> TmIsEmpty (ty, (subst x s t))
  | TmHead (ty,t) -> TmHead (ty, (subst x s t))
  | TmTail (ty,t) -> TmTail (ty, (subst x s t))
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmChar _ -> true
  | TmTuple fields -> List.for_all (fun ti -> isval ti) fields
  | TmRecord list -> List.for_all (fun t -> isval t) (List.map snd list)
  | TmEmptyList _ -> true
  | TmList(_,h,t) -> (isval h) && (isval t)
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)

      (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2

      (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'

      (* new string rules *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)

  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 ctx t2 in
      TmConcat (TmString s1, t2')

  | TmConcat (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmConcat (t1', t2)

  | TmFirst (TmString s) ->
      TmChar s.[0]

  | TmFirst s ->
      let s' = eval1 ctx s in
      TmFirst s'

  | TmSub (TmString s) ->
      let str =
          if s = "" then "" else
          String.sub s 1 ((String.length s) - 1)
      in TmString str

  | TmSub s ->
      let s' = eval1 ctx s in
      TmSub s'

      (* var rule *)
  | TmVar s ->
      getvbinding ctx s

   (* E-Tuple*)
  | TmTuple fields -> 
    let rec evalafield = function
      [] -> raise NoRuleApplies
      | vi::rest when isval vi -> 
        let rest' = evalafield rest in
          vi::rest'
      | ti::rest -> 
        let ti' = eval1 ctx ti in 
        ti'::rest
    in
    let fields' = evalafield fields in 
    TmTuple fields'    

  | TmRecord record ->
    let rec evalfield = function
      [] -> raise NoRuleApplies
      | (lb, vi)::rest when isval vi ->
        let rest' = evalfield rest in (lb, vi)::rest'
      | (lb, ti)::rest ->
        let ti' = eval1 ctx ti in (lb, ti')::rest
    in let record' = evalfield record in 
    TmRecord record'

  | TmProj (TmTuple fields as v1, lb) when isval v1 -> 
    (try List.nth fields (int_of_string lb - 1) with
    _ -> raise NoRuleApplies)

  | TmProj (TmRecord list as v, s) when isval v ->
    List.assoc s list

  | TmProj (t1, lb) -> 
    let t1' = eval1 ctx t1 in 
    TmProj (t1', lb)

  | TmLabel (s, t, var) ->  
    let t' = eval1 ctx t in
    let var' = gettbinding ctx var in
    (*print_endline("Testing...  The label is " ^ s ^ " and the type is " ^ string_of_ty var');*)
    (* Assuming var' is a list of pairs (string * type list) *)
    let f ty l = match ty with
    | TyVariant tyList when List.exists ((=) l) (List.map fst tyList) ->
      (*print_endline("Testing...  The label is " ^ l ^ " and the list is " ^ string_of_ty ty);*)
      TmLabel (s, t', var)
    | _ ->
        raise (Type_error "Variable is not of type variant or label doesn't match any label.")
    in f var' s

    (*E-Cons2*)
  | TmList(ty,h,t) when isval h -> TmList(ty,h,(eval1 ctx t))
  
    (*E-Cons1*)
  |TmList(ty,h,t) -> TmList(ty,(eval1 ctx h),t)
  
    (*E-IsNilNil*)
  |TmIsEmpty(ty,TmEmptyList(_)) -> TmTrue
  
    (*E-IsNilCons*)
  |TmIsEmpty(ty,TmList(_,_,_)) -> TmFalse
  
    (*E-IsNil*)
  |TmIsEmpty(ty,t) -> TmIsEmpty(ty,eval1 ctx t)
  
    (*E-HeadCons*)
  |TmHead(ty,TmList(_,h,_))-> h
  
    (*E-Head*)
  |TmHead(ty,t) -> TmHead(ty,eval1 ctx t)
  
    (*E-TailCons*)
  |TmTail(ty,TmList(_,_,t)) -> t
  
    (*E-Tail*)
  |TmTail(ty,t) -> TmTail(ty,eval1 ctx t)  

  | _ ->
      raise NoRuleApplies
;;

let apply_ctx ctx tm = 
  List.fold_left (fun t x -> subst x (getvbinding ctx x) t) tm (free_vars tm)
;;

let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;

let printer s tyTm tm =

    (* terms *)
  if (is_some tm) then (
    open_box 1;
      print_string s;
      print_space ();
      print_char ':';
      print_space ();
      print_type tyTm;
      print_space ();
      print_char '=';
      print_space();
      open_box 1;
        print_term (get tm);
    close_box ();
    print_newline();
    print_flush ();
  )

    (* types *)
  else (
    open_box 1;
      print_string s;
      print_space ();
      print_char ':';
      print_space ();
      print_string "type";
      print_space ();
      print_char '=';
      print_space();
      print_type tyTm;
    close_box ();
    print_newline();
    print_flush ();

  )
;;

let execute ctx = function
    Eval tm ->
      let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
      (*:print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');*)
      printer "-" tyTm (some tm');
      ctx

  | EvalTy ty ->
      let tyTm = to_basic_type ctx ty in
      (*print_endline ("- : type = " ^ string_of_ty tyTm);*)
      printer "-" tyTm None;
      ctx

  | BindTm (s, tm) ->
      let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
      (*print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');*)
      printer s tyTm (some tm');

      addbinding ctx s tyTm tm'
  | BindTy (s, ty) ->
      let tyTm = to_basic_type ctx ty in
      (* print_endline (s ^ " : type = " ^ string_of_ty tyTm); *)
      printer s tyTm None;
      addtbinding ctx s tyTm
;;

