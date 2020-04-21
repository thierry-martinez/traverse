let mklid (ident : Longident.t) : Ast_helper.lid =
  Metapp.mkloc ident

let type_name_of_ident (ident : Longident.t) : string =
  let buffer = Buffer.create 16 in
  let rec aux (ident : Longident.t) =
    match ident with
    | Lident s ->
        Buffer.add_string buffer s;
    | Ldot (m, s) ->
        aux m;
        Buffer.add_char buffer '_';
        Buffer.add_string buffer s
    | Lapply _ -> invalid_arg "type_name_of_ident" in
  aux ident;
  Buffer.contents buffer

let visit_method_name (name : string) =
  "visit_" ^ name

let f_var (name : string) =
  "f" ^ name

module VarName = struct
  type t = {
      f : string;
      x : string;
    }

  let make x : t = { f = f_var x; x }

  let f name = name.f

  let x name = name.x

  let to_fexp name =
    Metapp.Exp.var name.f

  let to_fpat name =
    Metapp.Pat.var name.f

  let to_exp name =
    Metapp.Exp.var name.x

  let to_pat name =
    Metapp.Pat.var name.x
end

module NamedArg = struct
  type t = {
      var : VarName.t;
      ty : Parsetree.core_type;
    }

  let var (arg : t) : VarName.t =
    arg.var

  let make (x : string) (ty : Parsetree.core_type) =
    { var = VarName.make x; ty }

  let of_list (args : Parsetree.core_type list) : t list =
    List.mapi (fun i ty ->
      make (Printf.sprintf "x%d" i) ty) args

  let to_fexp arg =
    VarName.to_fexp arg.var

  let to_fpat arg =
    VarName.to_fpat arg.var

  let to_exp arg =
    VarName.to_exp arg.var

  let to_pat arg =
    VarName.to_pat arg.var
end

let self = "self"

let visit_var (name : string) =
  "visit_'" ^ name

let traverse_var (name : string) =
  "traverse_'" ^ name

let visit_self name =
  Metapp.Exp.send (Metapp.Exp.var self) (Metapp.mkloc name)

let fun_ =
  Ast_helper.Exp.fun_ Nolabel None

let applicative_apply f args =
  match args with
  | [] -> [%expr Applicative.pure [%e f]]
  | hd :: tl ->
      let apply accu e =
        [%expr Applicative.apply [%e accu] (fun () -> [%e e])] in
      List.fold_left apply [%expr Applicative.map [%e f] [%e hd]] tl

let length_of_list list =
  List.fold_left (fun accu _ -> [%expr Succ [%e accu]]) [%expr Zero] list

let sequence_of_vars vars =
  List.fold_left
    (fun accu x -> [%expr [%e NamedArg.to_exp x] :: [%e accu]])
    [%expr []] vars

let pattern_of_vars module_ vars =
  List.fold_left
    (fun accu x ->
      Ast_helper.Pat.construct
        (mklid (Ldot (Ldot (module_, "ArrowSequence"), "::")))
        (Some (Ast_helper.Pat.tuple [NamedArg.to_fpat x; accu])))
    (Ast_helper.Pat.construct
       (mklid (Ldot (Ldot (module_, "ArrowSequence"), "[]")))
       None) vars

let destruct (vars, pat, exp) =
  [%expr Arity.destruct [%e length_of_list vars]
     (fun [%p pat] -> [%e sequence_of_vars vars])
     (fun [%p pattern_of_vars (Lident "Arity") vars] ->
       [%e exp])]

let arity_module_name (type_name : string) : string =
  "Arity_" ^ type_name

let traverse_module_name (type_name : string) : string =
  "Traverse_" ^ type_name

let traverse_function_name (type_name : string) : string =
  match type_name with
  | "t" -> "traverse"
  | _ -> "traverse_" ^ type_name

type builtin_type =
  | Array
  | Bool
  | Bytes
  | Char
  | Float
  | Int
  | Int32
  | Int64
  | Lazy
  | List
  | Nativeint
  | Option
  | Ref
  | Result
  | Seq
  | String
  | Unit

let builtin_type_of_ident (ident : Longident.t) : builtin_type option =
  let ident : Longident.t =
    match ident with
    | Ldot (Lident "Stdlib", name) -> Lident name
    | Ldot (Ldot (Lident "Stdlib", m), name) -> Ldot (Lident m, name)
    | _ -> ident in
  match ident with
  | Lident "array" | Ldot (Lident "Array", "t") -> Some Array
  | Lident "bool" -> Some Bool
  | Lident "bytes" | Ldot (Lident "Bytes", "t") -> Some Bytes
  | Lident "char" -> Some Char
  | Lident "float" | Ldot (Lident "Float", "t") -> Some Float
  | Lident "int" | Ldot (Lident "Int", "t") -> Some Int
  | Lident "int32" | Ldot (Lident "Int32", "t") -> Some Int32
  | Lident "int64" | Ldot (Lident "Int64", "t") -> Some Int64
  | Ldot (Lident "Lazy", "t") -> Some Lazy
  | Lident "list" | Ldot (Lident "List", "t") -> Some List
  | Lident "nativeint" | Ldot (Lident "Nativeint", "t") -> Some Nativeint
  | Lident "option" | Ldot (Lident "Option", "t") -> Some Option
  | Lident "ref" -> Some Ref
  | Lident "result" | Ldot (Lident "Result", "t") -> Some Result
  | Ldot (Lident "Seq", "t") -> Some Seq
  | Lident "string" | Ldot (Lident "String", "t") -> Some String
  | Lident "unit" -> Some Unit
  | _ -> None

let traverse_of_builtin_type (builtin : builtin_type) : Longident.t =
  match builtin with
  | Array -> Ldot (Ldot (Lident "Traverse", "Primitives"), "Array")
  | Lazy -> Ldot (Ldot (Lident "Traverse", "Primitives"), "Lazy")
  | List -> Ldot (Ldot (Lident "Traverse", "Primitives"), "List")
  | Option -> Ldot (Ldot (Lident "Traverse", "Primitives"), "Option")
  | Ref -> Ldot (Ldot (Lident "Traverse", "Primitives"), "Ref")
  | Result -> Ldot (Ldot (Lident "Traverse", "Primitives"), "Result")
  | Seq -> Ldot (Ldot (Lident "Traverse", "Primitives"), "Seq")
  | Bool
  | Bytes
  | Char
  | Float
  | Int
  | Int32
  | Int64
  | Nativeint
  | String
  | Unit -> Ldot (Ldot (Lident "Traverse", "Primitives"), "Atomic")

let visit_of_builtin_type (builtin : builtin_type) : string =
  match builtin with
  | Array -> "visit_array"
  | Bool -> "visit_bool"
  | Bytes -> "visit_bytes"
  | Char -> "visit_char"
  | Float -> "visit_float"
  | Int -> "visit_int"
  | Int32 -> "visit_int32"
  | Int64 -> "visit_int64"
  | Lazy -> "visit_lazy_t"
  | List -> "visit_list"
  | Nativeint -> "visit_nativeint"
  | Option -> "visit_option"
  | Ref -> "visit_ref"
  | Result -> "visit_result"
  | Seq -> "visit_seq"
  | String -> "visit_string"
  | Unit -> "visit_unit"

let traverse_module (ident : Longident.t) : Longident.t =
  match ident with
  | Lident name -> Lident (traverse_module_name name)
  | Ldot (m, name) -> Ldot (m, traverse_module_name name)
  | Lapply _ -> assert false

let f_apply f args =
  match args with
  | [] -> f
  | _ -> Metapp.apply f args

module StringSet = Set.Make (String)

let compare_pair compare_fst compare_snd (a, b) (c, d) : int =
  let o = compare_fst a c in
  if o = 0 then
    compare_snd b d
  else
    o

let rec compare_longident (a : Longident.t) (b : Longident.t) : int =
  match a, b with
  | Lident a, Lident b -> String.compare a b
  | Ldot (am, ax), Ldot (bm, bx) ->
      compare_pair compare_longident String.compare (am, ax) (bm, bx)
  | Lapply (af, am), Lapply (bf, bm) ->
      compare_pair compare_longident compare_longident (af, am) (bf, bm)
  | Lident _, (Ldot _ | Lapply _)
  | Ldot _, Lapply _ -> -1
  | Lapply _, Ldot _ -> 1
  | (Ldot _ | Lapply _), Lident _ -> 1

let rec compare_list compare_item a b =
  match a, b with
  | [], [] -> 0
  | a_hd :: a_tl, b_hd :: b_tl ->
      compare_pair compare_item (compare_list compare_item) (a_hd, a_tl)
        (b_hd, b_tl)
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1

let rec compare_coretype (a : Parsetree.core_type) (b : Parsetree.core_type)
    : int =
  match a.ptyp_desc, b.ptyp_desc with
  | Ptyp_var x, Ptyp_var y ->
      String.compare x y
  | Ptyp_constr (a_f, a_args), Ptyp_constr (b_f, b_args) ->
      compare_constr (a_f.txt, a_args) (b_f.txt, b_args)
  | Ptyp_tuple a, Ptyp_tuple b ->
      compare_list compare_coretype a b
  | Ptyp_var _, (Ptyp_constr _ | Ptyp_tuple _)
  | Ptyp_constr _, Ptyp_tuple _ -> -1
  | Ptyp_tuple _, Ptyp_constr _
  | (Ptyp_constr _ | Ptyp_tuple _), Ptyp_var _ -> 1
  | _ -> assert false

and compare_constr (a_f, a_args) (b_f, b_args) =
  compare_pair compare_longident (compare_list compare_coretype)
    (a_f, a_args) (b_f, b_args)

module Constr = struct
  type t = Longident.t * Parsetree.core_type list

  let compare = compare_constr
end

module ConstrSet = Set.Make (Constr)

let rec constr_set_of_core_type (ty : Parsetree.core_type) : ConstrSet.t =
  match ty.ptyp_desc with
  | Ptyp_var _ -> ConstrSet.empty
  | Ptyp_constr (f, args) ->
      ConstrSet.add (f.txt, args) (constr_set_of_core_types args)
  | Ptyp_tuple items ->
      constr_set_of_core_types items
  | _ -> assert false

and constr_set_of_core_types (list : Parsetree.core_type list) : ConstrSet.t =
  List.fold_left ConstrSet.union ConstrSet.empty
    (List.map constr_set_of_core_type list)

module Traverse = struct
  type t = {
      apply : NamedArg.t -> Parsetree.expression -> Parsetree.expression;
      rec_group : StringSet.t;
      var : string -> Parsetree.expression;
      constr : StringSet.t -> Longident.t -> Parsetree.expression list ->
        Parsetree.expression;
    }

  let visit apply rec_group = {
    apply; rec_group;
    var = (fun x -> Metapp.Exp.var (visit_var x));
    constr =
      (fun _rec_group name args ->
        let method_name =
          match builtin_type_of_ident name with
          | None -> visit_method_name (type_name_of_ident name)
          | Some builtin_type -> visit_of_builtin_type builtin_type in
        f_apply (visit_self method_name) args);
  }

  let traverse apply rec_group = {
    apply; rec_group;
    var = (fun x -> Metapp.Exp.var (traverse_var x));
    constr = (fun rec_group name args ->
      let traverse =
        match name with
        | Lident name when StringSet.mem name rec_group ->
            Ast_helper.Exp.ident
              (mklid (Ldot (Lident (traverse_module_name name), "traverse")))
        | _ ->
            let module_name : Longident.t =
              match builtin_type_of_ident name with
              | None -> Ldot (traverse_module name, "Make")
              | Some builtin_type -> traverse_of_builtin_type builtin_type in
            [%expr
               let module Traverse =
                 [%m Ast_helper.Mod.ident
                   (Metapp.mkloc module_name)] (Applicative)
                   (Arity) in
               Traverse.traverse] in
      f_apply traverse args);
  }
end

let full_apply (arg : NamedArg.t) (traverse : Parsetree.expression) =
  [%expr [%e NamedArg.to_fexp arg] [%e traverse]]

let partial_apply (arg : NamedArg.t) (traverse : Parsetree.expression) =
  [%expr [%e NamedArg.to_fexp arg] ([%e traverse] [%e NamedArg.to_exp arg])]

let rec visit_expr_of_type
    (traverse : Traverse.t) (ty : Parsetree.core_type)
    : Parsetree.expression =
  Ast_helper.with_default_loc ty.ptyp_loc @@ fun () ->
  match ty.ptyp_desc with
  | Ptyp_var x ->
      traverse.var x
  | Ptyp_constr (ident, args) ->
      traverse.constr traverse.rec_group ident.txt
        (List.map (visit_expr_of_type traverse) args)
  | Ptyp_tuple args ->
      destruct (visit_tuple traverse
        (fun args -> Ast_helper.Exp.tuple args) args)
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "traverse: cannot derive type %a" Pprintast.core_type ty

and visit_tuple
    (traverse : Traverse.t)
    (construct : Parsetree.expression list -> Parsetree.expression)
    (args : Parsetree.core_type list)
    : NamedArg.t list * Parsetree.pattern * Parsetree.expression =
  let var_args = NamedArg.of_list args in
  let var_pat = List.map NamedArg.to_pat var_args in
  let var_exp = List.map NamedArg.to_exp var_args in
  let construct = List.fold_right fun_ var_pat (construct var_exp) in
  let apply (arg : NamedArg.t) =
    traverse.apply arg (visit_expr_of_type traverse arg.ty) in
  let exp = applicative_apply construct (List.map apply var_args) in
  (var_args, Ast_helper.Pat.tuple var_pat, exp)

let visit_record
    (traverse : Traverse.t)
    (construct :
      (Ast_helper.lid * Parsetree.expression) list -> Parsetree.expression)
    (labels : Parsetree.label_declaration list)
    : NamedArg.t list * Parsetree.pattern * Parsetree.expression =
  let named_arg_of_label_declaration (label : Parsetree.label_declaration) =
    NamedArg.make label.pld_name.txt label.pld_type in
  let label_names = List.map named_arg_of_label_declaration labels in
  let label_values =
    List.map (fun (name : NamedArg.t) ->
        (Longident.Lident name.var.x, Metapp.Value.var name.var.x))
      label_names in
  let record_value = Metapp.Value.record label_values in
  let var_pat = List.map NamedArg.to_pat label_names in
  let fields =
    List.map
      (fun (name : NamedArg.t) ->
        (mklid (Lident name.var.x), NamedArg.to_exp name))
      label_names in
  let construct =
    List.fold_right fun_ var_pat (construct fields) in
  let apply (arg : NamedArg.t) =
    traverse.apply arg (visit_expr_of_type traverse arg.ty) in
  let exp =
    applicative_apply construct (List.map apply label_names) in
  (label_names, record_value.pat, exp)

let visit_constructor
    (traverse : Traverse.t)
    (constructor : Parsetree.constructor_declaration)
    : NamedArg.t list * Parsetree.pattern * Parsetree.expression =
  let lid = mklid (Lident constructor.pcd_name.txt) in
  let pat_construct = Ast_helper.Pat.construct lid in
  let exp_construct = Ast_helper.Exp.construct lid in
  match constructor.pcd_args with
  | Pcstr_tuple [] ->
      ([], pat_construct None,
        [%expr Applicative.pure [%e exp_construct None]])
  | Pcstr_tuple [ty] ->
      let arg = NamedArg.make "x" ty in
      ([arg], pat_construct (Some [%pat? x]),
        [%expr Applicative.map (fun x -> [%e exp_construct (Some [%expr x])])
           [%e traverse.apply arg (visit_expr_of_type traverse ty)]])
  | Pcstr_tuple args ->
      let (vars, pat, exp) =
        visit_tuple traverse
          (fun args -> exp_construct (Some (Ast_helper.Exp.tuple args)))
          args in
      (vars, pat_construct (Some pat), exp)
  | Pcstr_record labels ->
      let (vars, pat, exp) =
        visit_record traverse
          (fun fields ->
            exp_construct (Some (Ast_helper.Exp.record fields None)))
          labels in
      (vars, pat_construct (Some pat), exp)

let abstract_params var_name (params : string list) exp =
  List.fold_right
    (fun param accu ->
      [%expr fun [%p Metapp.Pat.var (var_name param)] -> [%e accu]]) params exp

let arity_t a b =
  [%type: ([%t a], [%t b]) Arity.t]

let app_t t =
  [%type: [%t t] Applicative.t]

let arity_app t =
  arity_t t (app_t t)

let type_constr (type_name : string) (params : string list)
    : Parsetree.core_type =
  Ast_helper.Typ.constr (mklid (Lident type_name))
    (List.map Ast_helper.Typ.var params)

let add_param_type param accu =
  Ast_helper.Typ.arrow Nolabel (arity_app (Ast_helper.Typ.var param)) accu

let add_param_types params ty =
  List.fold_right add_param_type params ty

let annot_method_type params ty exp =
  let visit_type = add_param_types params ty in
  Ast_helper.Exp.poly (abstract_params visit_var params exp)
    (Some (Metapp.Typ.poly (List.map Metapp.mkloc params) visit_type))

let visit_case_of_constructor (rec_group : StringSet.t) (type_name : string)
    (params : string list) (constructor : Parsetree.constructor_declaration)
    : Parsetree.class_field * Parsetree.case =
  Ast_helper.with_default_loc constructor.pcd_loc @@ fun () ->
  let (vars, pattern, exp) =
    visit_constructor (Traverse.visit full_apply rec_group) constructor in
  let exp =
    List.fold_right (fun (var : NamedArg.t) e -> [%expr
        Arity.destruct (Succ Zero) (fun x -> [x])
          (let open Arity.ArrowSequence in
            function ([[%p NamedArg.to_fpat var]]) -> [%e e])])
      vars exp in
  let constructor_type =
    List.fold_right
      (fun (var : NamedArg.t) ty ->
        arity_t var.ty ty)
      vars
      (app_t (type_constr type_name params)) in
  let exp = annot_method_type params constructor_type exp in
  let visit_constructor_method =
    Ast_helper.Cf.method_
      (Metapp.mkloc (visit_method_name constructor.pcd_name.txt))
      Public (Ast_helper.Cf.concrete Fresh exp) in
  let exp =
    [%expr Arity.Pred.destruct [%e length_of_list vars]
      (function
        | [%p pattern] -> [%e sequence_of_vars vars]
        | _ -> raise Traverse.StructuralMismatch)
      (fun [%p pattern_of_vars (Ldot (Lident "Arity", "Pred")) vars] ->
        [%e List.fold_left
          (fun accu (var : NamedArg.t) ->
            Metapp.apply (NamedArg.to_fexp var)
              [Metapp.apply accu [NamedArg.to_exp var]])
          (Metapp.apply
             (visit_self (visit_method_name constructor.pcd_name.txt))
             (List.map Metapp.Exp.var (List.map visit_var params)))
          vars])] in
  (visit_constructor_method, Ast_helper.Exp.case pattern exp)

let extract_param ((ty : Parsetree.core_type), _variance) : string =
  match ty.ptyp_desc with
  | Ptyp_var var -> var
  | _ -> invalid_arg "extract_param"

let class_fields_of_type_declaration (rec_group : StringSet.t)
    (type_declaration : Parsetree.type_declaration)
    : Parsetree.class_field list =
  Ast_helper.with_default_loc type_declaration.ptype_loc @@ fun () ->
  let type_name = type_declaration.ptype_name.txt in
  let params =
    List.map extract_param type_declaration.ptype_params in
  let other_fields, visit_type_expr =
    match type_declaration.ptype_kind with
    | Ptype_abstract ->
        begin match type_declaration.ptype_manifest with
        | None ->
            Location.raise_errorf ~loc:!Ast_helper.default_loc
              "traverse: cannot derive abstract types"
        | Some ty ->
            [], visit_expr_of_type (Traverse.visit full_apply rec_group) ty
        end
    | Ptype_open ->
        Location.raise_errorf ~loc:!Ast_helper.default_loc
          "traverse: cannot derive open types"
    | Ptype_variant constructors ->
        let fields, cases =
          List.split (List.map
            (visit_case_of_constructor rec_group type_name params)
              constructors) in
        fields, Ast_helper.Exp.function_ cases
    | Ptype_record labels ->
        let exp =
          destruct
            (visit_record (Traverse.visit full_apply rec_group)
               (fun fields -> Ast_helper.Exp.record fields None)
               labels) in
        [], exp in
  let visit_type_expr =
    annot_method_type params (arity_app (type_constr type_name params))
      visit_type_expr in
  let visit_type_method =
    Ast_helper.Cf.method_
      (Metapp.mkloc (visit_method_name type_name))
      Public (Ast_helper.Cf.concrete Fresh visit_type_expr) in
  visit_type_method :: other_fields

let traverse_case_of_constructor (rec_group : StringSet.t) (type_name : string)
    (params : string list) (constructor : Parsetree.constructor_declaration)
    : Parsetree.case =
  Ast_helper.with_default_loc constructor.pcd_loc @@ fun () ->
  let (vars, pattern, exp) =
    visit_constructor (Traverse.traverse partial_apply rec_group) constructor in
  let exp =
    [%expr Arity.Pred.destruct [%e length_of_list vars]
      (function
        | [%p pattern] -> [%e sequence_of_vars vars]
        | _ -> raise Traverse.StructuralMismatch)
      (fun [%p pattern_of_vars (Ldot (Lident "Arity", "Pred")) vars] ->
        [%e exp])] in
  Ast_helper.Exp.case pattern exp

let module_binding (item : Parsetree.structure_item)
    : Parsetree.module_binding =
  match item.pstr_desc with
    | Pstr_module binding -> binding
    | _ -> assert false

let set_module_name name (item : Parsetree.structure_item)
    : Parsetree.structure_item =
  match item.pstr_desc with
    | Pstr_module binding ->
        let pmb_name = { binding.pmb_name with txt = Some name } in
        let binding = { binding with pmb_name } in
        { item with pstr_desc = Pstr_module binding }
    | _ -> assert false

let (-->) a b =
  [%type: [%t a] -> [%t b]]

let traverse_module_of_type_declaration (rec_group : StringSet.t)
    (type_declaration : Parsetree.type_declaration)
    : Parsetree.module_binding =
  Ast_helper.with_default_loc type_declaration.ptype_loc @@ fun () ->
  let type_name = type_declaration.ptype_name.txt in
  let type_arity = List.length type_declaration.ptype_params in
  let ai = List.init type_arity (Printf.sprintf "a%d") in
  let fi = List.init type_arity (Printf.sprintf "f%d") in
  let result = "result" in
  let result_var = Ast_helper.Typ.var result in
  let params =
    List.map extract_param type_declaration.ptype_params in
  let traverse_expr =
    match type_declaration.ptype_kind with
    | Ptype_abstract ->
        begin match type_declaration.ptype_manifest with
        | None ->
            Location.raise_errorf ~loc:!Ast_helper.default_loc
              "traverse: cannot derive abstract types"
        | Some ty ->
            visit_expr_of_type (Traverse.traverse full_apply rec_group) ty
        end
    | Ptype_open ->
        Location.raise_errorf ~loc:!Ast_helper.default_loc
          "traverse: cannot derive open types"
    | Ptype_variant constructors ->
        let cases =
          List.map
            (traverse_case_of_constructor rec_group type_name params)
            constructors in
        Ast_helper.Exp.function_ cases
    | Ptype_record labels ->
        destruct
          (visit_record (Traverse.traverse full_apply rec_group)
            (fun fields -> Ast_helper.Exp.record fields None)
               labels) in
  let vfi = List.map Ast_helper.Typ.var fi in
  let traverse_type =
    Ast_helper.Typ.constr
      (mklid (Ldot (Lident (arity_module_name type_name), "t")))
      (List.map (fun s -> app_t (Ast_helper.Typ.var s)) ai @
      [app_t (Ast_helper.Typ.constr (mklid (Lident type_name))
        (List.map Ast_helper.Typ.var ai))] @ vfi @ [result_var] @
      [[%type: [`Not_empty]]]) -->
    (List.fold_right (-->) vfi result_var) in
  let traverse_expr = abstract_params traverse_var params traverse_expr in
  set_module_name (traverse_module_name type_name)
    [%stri module Traverse : sig
        val traverse : [%t traverse_type]
      end = struct
        let rec traverse :
          [%t Ast_helper.Typ.poly (List.map Metapp.mkloc (result :: ai @ fi))
            traverse_type]
        = fun arity ->
 [%e traverse_expr]
      end] |>
  module_binding

let make_group_module_name
    (type_declarations : Parsetree.type_declaration list) : string =
  let buffer = Buffer.create 16 in
  Buffer.add_string buffer "Traverse_group";
  type_declarations |> List.iter
    (fun (type_declaration : Parsetree.type_declaration) ->
      Buffer.add_char buffer '_';
      Buffer.add_string buffer type_declaration.ptype_name.txt);
  Buffer.contents buffer

let type_param name =
  name, Asttypes.Invariant

let set_type_params ptype_params (item : Parsetree.structure_item)
    : Parsetree.structure_item =
  match item.pstr_desc with
  | Pstr_type (rec_flag, [type_decl]) ->
      let type_decl = { type_decl with ptype_params } in
      { item with pstr_desc = Pstr_type (rec_flag, [type_decl]) }
  | _ ->
      invalid_arg "type_params"

let arities_of_type_declaration
    (type_declaration : Parsetree.type_declaration) : Parsetree.structure_item =
  Ast_helper.with_default_loc type_declaration.ptype_loc @@ fun () ->
  let type_name = type_declaration.ptype_name.txt in
  let type_arity = List.length type_declaration.ptype_params in
  let init_var_list f =
    List.init type_arity (fun i -> Ast_helper.Typ.var (f i)) in
  let ai = init_var_list (Printf.sprintf "a%d") in
  let fi = init_var_list (Printf.sprintf "f%d") in
  let xi = init_var_list (Printf.sprintf "x%d") in
  let arity_t = "arity_t" in
  let arity_t_lid = mklid (Lident arity_t) in
  let arity_t_var = Ast_helper.Typ.var arity_t in
  let result_var = Ast_helper.Typ.var "result" in
  let t_to_result = [%type:
    [%t Ast_helper.Typ.constr (mklid (Lident type_name)) xi] ->
      [%t result_var]] in
  let xi_to_fi = List.map2 (fun xi fi -> [%type: [%t xi] -> [%t fi]]) xi fi in
  let zero_type =
    Ast_helper.Typ.constr arity_t_lid (
      ai @ [arity_t_var] @ ai @ [arity_t_var] @ [[%type: [`Empty]]]) in
  let succ_input =
    Ast_helper.Typ.constr arity_t_lid (
      ai @ [arity_t_var] @ fi @ [result_var] @ [[%type: _]]) in
  let succ_output =
    Ast_helper.Typ.constr arity_t_lid (
      ai @ [arity_t_var] @ xi_to_fi @ [t_to_result]
        @ [[%type: [`Not_empty]]]) in
  let args =
    (ai @ [arity_t_var] @ fi @ [result_var; Ast_helper.Typ.var "is_empty"]) in
  let params = List.map type_param args in
  set_module_name (arity_module_name type_name)
    [%stri module Arity = struct
      [%%i set_type_params params [%stri
        type arity_t =
          | O : [%t zero_type]
          | S : [%t succ_input] -> [%t succ_output]]]
      [%%i set_type_params params [%stri
        type t = [%t Ast_helper.Typ.constr arity_t_lid args]]]
    end]

let make_str ~loc ((rec_flag : Asttypes.rec_flag), type_declarations)
    : Parsetree.structure =
  Ast_helper.with_default_loc loc @@ fun () ->
  let rec_group =
    StringSet.of_list (type_declarations |> List.map
      (fun (decl : Parsetree.type_declaration) -> decl.ptype_name.txt)) in
  let fields =
    List.concat_map (class_fields_of_type_declaration rec_group)
      type_declarations in
  let fields =
    Ast_helper.Cf.inherit_ Fresh
      (Ast_helper.Cl.constr
        (mklid (Ldot (Lident "Primitives", "traverse")))
        [Ast_helper.Typ.any ()]) None :: fields in
  let class_name = "traverse" in
  let class_declaration =
    Ast_helper.Ci.mk ~virt:Virtual ~params:[([%type: 'self], Invariant)]
      (Metapp.mkloc class_name)
      (Ast_helper.Cl.structure
        (Ast_helper.Cstr.mk [%pat? (self : 'self)] fields)) in
  let traverse_arities =
    List.map arities_of_type_declaration type_declarations in
  let bindings =
    List.map (traverse_module_of_type_declaration rec_group)
      type_declarations in
  let traverse_modules =
    match rec_flag with
    | Recursive ->
        [Ast_helper.Str.rec_module bindings]
    | Nonrecursive ->
        List.map Ast_helper.Str.module_ bindings in
  let group_module_name = make_group_module_name type_declarations in
  let group_module_item = [%stri
    module Group = struct
      [%%i Metapp.Stri.of_list traverse_arities]
      module Make (Applicative : Traverse.Applicative.S) = struct
        [%%i Metapp.Stri.of_list traverse_modules]
        module Class (Arity : Traverse.Arity.NonNullS) = struct
          module Primitives = Traverse.Primitives.Classes (Applicative) (Arity)
          [%%i Ast_helper.Str.class_ [class_declaration]]
        end
      end
    end] in
  let group_module =
    Metapp.Mb.mk (Metapp.mkloc (Some group_module_name))
      (module_binding group_module_item).pmb_expr in
  let single_type_modules =
    type_declarations |> List.map (fun (decl : Parsetree.type_declaration) ->
      let arity_module_name = arity_module_name decl.ptype_name.txt in
      let module_name = traverse_module_name decl.ptype_name.txt in
      let module_item = [%stri
        module Traverse_t = struct
          module Group =
            [%m Ast_helper.Mod.ident (mklid (Lident group_module_name))]
              (Applicative)

          module Arity =
            [%m Ast_helper.Mod.ident
              (mklid (Ldot (Lident "Group", arity_module_name)))]

          module Make (Applicative : Traverse.Applicative.S) = struct
            module Group = Group.Make (Applicative)

            include [%m Ast_helper.Mod.ident
              (mklid (Ldot (Lident "Group", module_name)))]

            module Class = Group.Class
          end
        end] in
     Metapp.Mb.mk (Metapp.mkloc (Some module_name))
      (module_binding module_item).pmb_expr) in
(*
  let values =
    type_declaration |> List.map
      (fun (decl : Parsetree.type_declaration) ->
        let function_name = traverse_function_name decl.ptype_name.txt in
        let f =
          [%expr:
             let Applicative.A app = app () in
             let module M = (val app) in
             let module Traverse = [%m ].Make (M.Applicative) (Arity) in
             Traverse.traverse]
        )
*)
  List.map Ast_helper.Str.module_ (group_module :: single_type_modules) (*@
  values*)

let make_sig ~loc _type_declarations : Parsetree.signature =
  Location.raise_errorf ~loc "traverse: cannot make signature"

let deriver_name = "traverse"

type Ppx_derivers.deriver += Traverse

let () =
  Ppx_derivers.register deriver_name Traverse

let get_derivers (attributes : Parsetree.attributes)
    : Parsetree.expression list =
  match Metapp.Attr.find "deriving" attributes with
  | None -> []
  | Some derivers ->
      let derivers = Metapp.Exp.of_payload (Metapp.Attr.payload derivers) in
      match derivers.pexp_desc with
      | Pexp_tuple derivers -> derivers
      | _ -> [derivers]

let has_deriver (attributes : Parsetree.attributes) : bool =
  get_derivers attributes |> List.exists (fun (e : Parsetree.expression) ->
    match e.pexp_desc with
    | Pexp_ident { txt = Lident name } ->
        String.equal name deriver_name
    | _ -> false)

let declarations_has_deriver (declarations : Parsetree.type_declaration list)
    : bool =
  declarations |> List.exists (fun (decl : Parsetree.type_declaration) ->
    has_deriver decl.ptype_attributes)

let signature (mapper : Ast_mapper.mapper) (s : Parsetree.signature)
    : Parsetree.signature =
  let s = Ast_mapper.default_mapper.signature mapper s in
  s |> List.concat_map (fun (item : Parsetree.signature_item) ->
    match item.psig_desc with
    | Psig_type (rec_flag, type_declarations)
      when declarations_has_deriver type_declarations ->
        item :: make_sig ~loc:item.psig_loc (rec_flag, type_declarations)
    | _ -> [item])

let structure (mapper : Ast_mapper.mapper) (s : Parsetree.structure)
    : Parsetree.structure =
  let s = Ast_mapper.default_mapper.structure mapper s in
  s |> List.concat_map (fun (item : Parsetree.structure_item) ->
    match item.pstr_desc with
    | Pstr_type (rec_flag, type_declarations)
      when declarations_has_deriver type_declarations ->
        item :: make_str ~loc:item.pstr_loc (rec_flag, type_declarations)
    | _ -> [item])

let type_variables_of_core_type (ty : Parsetree.core_type) : string list =
  let accu_list = ref [] in
  let accu_set = ref StringSet.empty in
  let add_var_name var_name =
    let set = !accu_set in
    if not (StringSet.mem var_name set) then
      begin
        accu_set := StringSet.add var_name set;
        Metapp.mutate (List.cons var_name) accu_list;
      end in
  let typ (iterator : Ast_iterator.iterator) (ty : Parsetree.core_type) =
    match ty.ptyp_desc with
    | Ptyp_var var_name -> add_var_name var_name
    | _ -> Ast_iterator.default_iterator.typ iterator ty in
  let iterator = { Ast_iterator.default_iterator with typ } in
  iterator.typ iterator ty;
  List.rev !accu_list

let module_expr (mapper : Ast_mapper.mapper) (m : Parsetree.module_expr)
    : Parsetree.module_expr =
  Ast_helper.with_default_loc m.pmod_loc @@ fun () ->
  match m.pmod_desc with
  | Pmod_extension ({ txt = "traverse" }, payload) ->
      let ty = Metapp.Typ.of_payload payload in
      let expr =
        visit_expr_of_type (Traverse.traverse full_apply StringSet.empty) ty in
      let params = type_variables_of_core_type ty in
      let expr = abstract_params traverse_var params expr in
      let item = [%stri
        module M (Applicative : Traverse.Applicative.S)
          (Arity : Traverse.Arity.NonNullS) = struct
          let traverse = [%e expr]
        end] in
      (module_binding item).pmb_expr
  | _ -> Ast_mapper.default_mapper.module_expr mapper m

let mapper : Ast_mapper.mapper =
  { Ast_mapper.default_mapper with signature; structure; module_expr }

let rewriter _config _cookies : Ast_mapper.mapper =
  mapper

let () =
  Migrate_parsetree.Driver.register ~name:"traverse_ppx"
    (module Migrate_parsetree.OCaml_current) rewriter
