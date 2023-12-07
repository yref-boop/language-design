
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
  | TmCase of term * ((string * string * term) list)
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
  | TmCase (tmlabel, tmcases) -> 
    let rec traverse list out = match list with
    | [] -> out
    | [(_, _, term) :: []] -> traverse [] (out @ free_vars term)
    | [(_, _, term) :: tail] -> traverse [tail] (out @ free_vars term)
    | _ -> raise (Type_error "This should happend, right? (2)")
    in (free_vars tmlabel) @ (traverse [tmcases] [])
    
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
  | TmProj (tm, str) ->
    (match tm with
      | TmTuple fields ->  subst x s tm
      | TmRecord fields ->  subst x s tm
      | _ ->  raise(Type_error("Tuple type expected (3)")))
  | TmLabel (str, t, var) ->
    TmLabel (str, subst x s t, var)
  | TmCase (label, l) -> 
    TmCase (subst x s label, l)
    (* LISTS *)  
  | TmEmptyList ty -> tm
  | TmList (ty,t1,t2) -> TmList (ty, (subst x s t1), (subst x s t2))
  | TmIsEmpty (ty,t) -> TmIsEmpty (ty, (subst x s t))
  | TmHead (ty,t) -> TmHead (ty, (subst x s t))
  | TmTail (ty,t) -> TmTail (ty, (subst x s t))
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
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
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

  | TmCase (tm_label, case_list) -> 
    let rec isVariant tm = match tm with
      | TmVar (s) -> (try let value = getvbinding ctx s in isVariant value
                      with _ -> raise (Type_error ("no binding type for variable " ^ s)))
      | TmLabel (s, label_term, label_ty) -> 
        let rec string_exists_in_list target list =
          match list with
            | [] -> false
            | (str, _, _) :: tail ->  if str = target then true else string_exists_in_list target tail

        in  if (string_exists_in_list s case_list) == true   
            then  let rec auxFindLabel labels = match labels with (* Find the variable of the correct case *)
                    | [] -> None
                    | (label, varname, term) :: tail ->
                      if label = s then Some (label, varname, term)
                      else auxFindLabel tail
                  in let findLabel c_list = match auxFindLabel c_list with
                        | Some (r_label, r_varname, r_term) ->
                              let correct_term = subst r_varname label_term r_term in 
                                typeof ctx correct_term
                        | None -> raise (Type_error "No matching label found in cases.")
                      in findLabel case_list
                
            else raise (Type_error "Cases don't align with the possible labels of variable.")

      | _ -> raise (Type_error "Variable for case must be an invariant.")
    in isVariant tm_label   

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
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
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

  | TmCase (label, cases) -> 
    let label' = match label with
      | TmLabel (s, t, _) -> s
      | _ -> raise (Type_error "I wanna go home")
    in let rec traverse list = match list with
    | [] -> raise (Type_error "Run through every label, no matches detected.")
    | [(l, _, term)] -> if l == label' 
                          then term
                          else traverse []
    | (l, _, term) :: tail -> if l == label' 
                                then term
                                else traverse tail
    in string_of_term label ^ " => " ^ string_of_term (traverse cases)

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
    (* Assuming var' is a list of pairs (string * type list) *)
    let f ty l = match ty with
    | TyVariant tyList when List.exists ((=) l) (List.map fst tyList) ->
      TmLabel (s, t', var)
    | _ ->
        raise (Type_error "Variable is not of type variant or label doesn't match any label.")
    in f var' s

  | TmCase (label, cases) ->
    let label' = eval1 ctx label in
    let extracted_label = match label' with TmLabel (l, _, _) -> l | _ -> raise (Type_error "No label found.") in
    (*let extracted_value = match label' with TmLabel (_, tm, _) -> eval1 ctx tm | _ -> raise (Type_error "No term found.") in*)
    let rec traverse list = match list with
        | [] -> raise (Type_error "Run through every label, no matches detected.")
        | [(l, t, term)] -> if l == extracted_label 
                                    then term
                                    else traverse []
        | (l, t, term) :: tail -> if l == extracted_label 
                                    (*then eval1 ctx (subst t extracted_value term)*)
                                    then term
                                    else traverse tail
    in traverse cases

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

let execute ctx = function
    Eval tm ->
      let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
      print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      ctx
  | EvalTy ty ->
      let tyTm = to_basic_type ctx ty in
      print_endline ("- : type = " ^ string_of_ty tyTm);
      ctx
  | BindTm (s, tm) ->
      let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
      print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      addbinding ctx s tyTm tm'
  | BindTy (s, ty) ->
      let tyTm = to_basic_type ctx ty in
      print_endline (s ^ " : type = " ^ string_of_ty tyTm);
      addtbinding ctx s tyTm        
;;

