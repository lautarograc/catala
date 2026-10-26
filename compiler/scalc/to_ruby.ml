(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(* Translation of the statement calculus to Ruby. The design follows closely the
   Python backend ([to_python.ml]), with the following specificities:

   - The code generated from a Catala module (or file) is wrapped in a Ruby
     [module]. Scopes and functions become singleton methods ([def self.f]),
     top-level values become module instance variables ([@x]) with a reader
     method, so that other modules can access them as [::Mod.x].

   - Integers, decimals, booleans and lists are represented by native Ruby
     [Integer], [Rational], [true]/[false] and [Array]. Other values use the
     classes of the [CatalaRuntime] module.

   - Inner functions are Ruby lambdas. Since Ruby closures share the local
     variables of their enclosing scope, all the locals of a lambda are declared
     as block-local variables ([->(x; local1, local2) do ... end]). *)

open Catala_utils
open Shared_ast
open Ast
module Runtime = Catala_runtime

let rt = "CatalaRuntime"

type ctx = {
  sctx : Ast.ctx;
  toplevel_vars : VarName.Set.t;
      (** Variables bound by top-level [SVar] items, printed as [@var] *)
  modnames : ModuleName.t VarName.Map.t;
      (** Reverse mapping of [sctx.modules] *)
  read_vars : VarName.Set.t;
      (** Variables that are read somewhere: the others are prefixed with [_] to
          avoid Ruby's "assigned but unused variable" warnings *)
}

(* Ruby string literals: we use single-quoted strings, where only the backslash
   and the single quote need escaping (no interpolation can happen). Strings
   containing control characters use double-quoted literals with escapes, so
   that the generated code never has string literals spanning multiple lines. *)
let ruby_string (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  if String.exists (fun c -> Char.code c < 0x20 || c = '\x7f') s then (
    Buffer.add_char b '"';
    String.iter
      (function
        | '\\' -> Buffer.add_string b "\\\\"
        | '"' -> Buffer.add_string b "\\\""
        | '#' -> Buffer.add_string b "\\#"
        | '\n' -> Buffer.add_string b "\\n"
        | '\t' -> Buffer.add_string b "\\t"
        | c when Char.code c < 0x20 || c = '\x7f' ->
          Buffer.add_string b (Printf.sprintf "\\x%02x" (Char.code c))
        | c -> Buffer.add_char b c)
      s;
    Buffer.add_char b '"')
  else (
    Buffer.add_char b '\'';
    String.iter
      (function
        | '\\' -> Buffer.add_string b "\\\\"
        | '\'' -> Buffer.add_string b "\\'"
        | c -> Buffer.add_char b c)
      s;
    Buffer.add_char b '\'');
  Buffer.contents b

(* No break hints: Ruby is sensitive to newlines, the code is printed with an
   infinite margin and expressions always fit on one line *)
let pp_comma ppf () = Format.pp_print_string ppf ", "

let ruby_keywords =
  [
    "BEGIN";
    "END";
    "alias";
    "and";
    "begin";
    "break";
    "case";
    "class";
    "def";
    "defined";
    "do";
    "else";
    "elsif";
    "end";
    "ensure";
    "false";
    "for";
    "if";
    "in";
    "module";
    "next";
    "nil";
    "not";
    "or";
    "redo";
    "rescue";
    "retry";
    "return";
    "self";
    "super";
    "then";
    "true";
    "undef";
    "unless";
    "until";
    "when";
    "while";
    "yield";
    "__FILE__";
    "__LINE__";
    "__ENCODING__";
    "__method__";
  ]

(* Names that must not be redefined by generated methods (struct fields and
   singleton methods of the generated modules) or that the generated code
   relies upon. Names of methods of the runtime classes that generated classes
   don't inherit from (e.g. [Option#value], [Enum#code]) need not be reserved. *)
let ruby_reserved_methods =
  [
    "call";
    "class";
    "clone";
    "display";
    "dup";
    "eql";
    "equal";
    "extend";
    "freeze";
    "frozen";
    "hash";
    "initialize";
    "inspect";
    "instance_variable_get";
    "instance_variable_set";
    "is_a";
    "itself";
    "kind_of";
    "lambda";
    "method";
    "methods";
    "new";
    "object_id";
    "proc";
    "public_send";
    "raise";
    "require";
    "require_relative";
    "respond_to";
    "send";
    "singleton_class";
    "tap";
    "to_json";
    "to_s";
    "CatalaRuntime";
  ]

(* Built-in Ruby constants which a generated module must not reopen *)
let ruby_core_constants =
  [
    "ARGF";
    "ARGV";
    "Array";
    "BasicObject";
    "Binding";
    "Class";
    "Comparable";
    "Complex";
    "Data";
    "Date";
    "Dir";
    "ENV";
    "Encoding";
    "Enumerable";
    "Enumerator";
    "Errno";
    "Exception";
    "FalseClass";
    "Fiber";
    "File";
    "Float";
    "GC";
    "Hash";
    "IO";
    "Integer";
    "JSON";
    "Kernel";
    "Marshal";
    "Math";
    "Method";
    "Module";
    "Mutex";
    "NilClass";
    "Numeric";
    "Object";
    "ObjectSpace";
    "OptionParser";
    "Proc";
    "Process";
    "Queue";
    "Ractor";
    "Random";
    "Range";
    "Rational";
    "Regexp";
    "RubyVM";
    "Set";
    "Signal";
    "String";
    "Struct";
    "Symbol";
    "Thread";
    "Time";
    "TrueClass";
    "Warning";
    rt;
  ]

let module_constant (name : string) : string =
  let id = String.to_id name in
  let id =
    match id.[0] with
    | 'A' .. 'Z' -> id
    | 'a' .. 'z' -> String.capitalize_ascii id
    | _ -> "M_" ^ id
    | exception Invalid_argument _ -> "M_"
  in
  if List.mem id ruby_core_constants then id ^ "_" else id

let format_module_name ppf (m : ModuleName.t) =
  Format.fprintf ppf "::%s"
    (module_constant (ModuleName.to_string (ModuleName.normalise m)))

let format_lit (fmt : Format.formatter) (l : lit Mark.pos) : unit =
  match Mark.remove l with
  | LBool true -> Format.pp_print_string fmt "true"
  | LBool false -> Format.pp_print_string fmt "false"
  | LInt i ->
    let s = Runtime.integer_to_string i in
    if s.[0] = '-' then Format.fprintf fmt "(%s)" s
    else Format.pp_print_string fmt s
  | LUnit -> Format.fprintf fmt "%s::UNIT" rt
  | LRat q ->
    Format.fprintf fmt "Rational(%s, %s)"
      (Z.to_string (Q.num q))
      (Z.to_string (Q.den q))
  | LMoney e ->
    Format.fprintf fmt "%s::Money.new(%s)" rt
      (Runtime.integer_to_string (Runtime.money_to_cents e))
  | LDate d ->
    Format.fprintf fmt "%s::Date.new(%d, %d, %d)" rt
      (Runtime.integer_to_int (Runtime.year_of_date d))
      (Runtime.integer_to_int (Runtime.month_number_of_date d))
      (Runtime.integer_to_int (Runtime.day_of_month_of_date d))
  | LDuration d ->
    let years, months, days = Runtime.duration_to_years_months_days d in
    Format.fprintf fmt "%s::Duration.new(%d, %d, %d)" rt years months days

(* Native Ruby binary operators *)
let binop_symbol (op : operator) : string option =
  match op with
  | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dur_dur | Concat -> Some "+"
  | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dur_dur ->
    Some "-"
  | Mult_int_int | Mult_rat_rat | Mult_mon_int | Mult_mon_rat | Mult_dur_int ->
    Some "*"
  | And -> Some "&&"
  | Or -> Some "||"
  | Eq -> Some "=="
  | Xor -> Some "!="
  | Lt -> Some "<"
  | Lte -> Some "<="
  | Gt -> Some ">"
  | Gte -> Some ">="
  | _ -> None

(* Operators implemented as functions of the runtime, with the position (when
   needed) passed as last argument *)
let runtime_function (op : operator) : string option =
  match op with
  | ToInt_rat -> Some "integer_of_decimal"
  | ToInt_mon -> Some "integer_of_money"
  | ToRat_int -> Some "decimal_of_integer"
  | ToRat_mon -> Some "decimal_of_money"
  | ToMoney_rat -> Some "money_of_decimal"
  | ToMoney_int -> Some "money_of_integer"
  | Round_mon -> Some "money_round"
  | Round_rat -> Some "decimal_round"
  | Div_int_int -> Some "div_int_int"
  | Div_rat_rat -> Some "div_rat_rat"
  | Div_mon_mon -> Some "div_mon_mon"
  | Div_mon_int -> Some "div_mon_int"
  | Div_mon_rat -> Some "div_mon_rat"
  | Div_dur_dur -> Some "div_dur_dur"
  | Eq -> Some "eq"
  | Lt -> Some "lt"
  | Lte -> Some "lte"
  | Gt -> Some "gt"
  | Gte -> Some "gte"
  | Map -> Some "map"
  | Map2 -> Some "map2"
  | Reduce -> Some "reduce"
  | Filter -> Some "filter"
  | Find -> Some "find"
  | Fold -> Some "fold"
  | Sort `Asc -> Some "sort_asc"
  | Sort `Desc -> Some "sort_desc"
  | HandleExceptions -> Some "handle_exceptions"
  | _ -> None

let op_needs_pos (type a) (op : a Op.t) ty =
  match op with
  | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int | Div_mon_rat
  | Div_dur_dur | Add_dat_dur _ | Sub_dat_dur _ | Sort _ | Map2
  | ValueFromJson _ ->
    true
  | Op.Eq | Lt | Lte | Gt | Gte -> (
    match ty with
    | TLit (TUnit | TBool | TInt | TMoney | TRat | TDate) -> false
    | _ -> true)
  | _ -> false

let renaming =
  Renaming.program ()
    ~reserved:(ruby_keywords @ ruby_reserved_methods)
    ~skip_constant_binders:false ~constant_binder_name:None
    ~namespaced_fields:true ~namespaced_constrs:true ~prefix_module:false
    ~modnames_conflict:false ~f_var:String.to_snake_case
    ~f_struct:String.to_camel_case ~f_enum:String.to_camel_case

(* Types are referred to by their class, qualified by the Ruby module of the
   Catala module that defines them, if any. All the modules are loaded
   transitively, so the defining module is always accessible. *)
let format_qualified
    (type id)
    (module Id : Uid.Qualified with type t = id)
    ppf
    (s : id) =
  (match List.rev (Id.path s) with
  | m :: _ -> Format.fprintf ppf "%a::" format_module_name m
  | [] -> ());
  Format.pp_print_string ppf (Id.base s)

let format_struct = format_qualified (module StructName)
let format_enum = format_qualified (module EnumName)
let format_abstract = format_qualified (module AbstractType)

(* Human-readable types, only used in comments *)
let rec format_typ (fmt : Format.formatter) (typ : typ) : unit =
  match Mark.remove typ with
  | TLit TUnit -> Format.pp_print_string fmt "Unit"
  | TLit TMoney -> Format.pp_print_string fmt "Money"
  | TLit TInt -> Format.pp_print_string fmt "Integer"
  | TLit TRat -> Format.pp_print_string fmt "Decimal"
  | TLit TDate -> Format.pp_print_string fmt "Date"
  | TLit TDuration -> Format.pp_print_string fmt "Duration"
  | TLit TBool -> Format.pp_print_string fmt "Bool"
  | TLit TPos -> Format.pp_print_string fmt "SourcePosition"
  | TTuple ts ->
    Format.fprintf fmt "Tuple(%a)"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
         format_typ)
      ts
  | TStruct s -> format_struct fmt s
  | TOption t -> Format.fprintf fmt "Option(%a)" format_typ t
  | TDefault t -> format_typ fmt t
  | TEnum e -> format_enum fmt e
  | TAbstract t -> format_abstract fmt t
  | TArrow (t1, t2) ->
    Format.fprintf fmt "(%a) -> %a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
         format_typ)
      t1 format_typ t2
  | TArray t1 -> Format.fprintf fmt "Array(%a)" format_typ t1
  | TVar _ -> Format.pp_print_string fmt "Any"
  | TForAll tb ->
    let _v, typ = Bindlib.unmbind tb in
    format_typ fmt typ
  | TClosureEnv -> Format.pp_print_string fmt "ClosureEnv"
  | TError -> assert false

(* Type descriptors interpreted by the runtime (used for JSON decoding) *)
let rec format_typ_descr (fmt : Format.formatter) (typ : typ) : unit =
  match Mark.remove typ with
  | TLit TUnit -> Format.pp_print_string fmt ":unit"
  | TLit TMoney -> Format.pp_print_string fmt ":money"
  | TLit TInt -> Format.pp_print_string fmt ":integer"
  | TLit TRat -> Format.pp_print_string fmt ":decimal"
  | TLit TDate -> Format.pp_print_string fmt ":date"
  | TLit TDuration -> Format.pp_print_string fmt ":duration"
  | TLit TBool -> Format.pp_print_string fmt ":bool"
  | TLit TPos -> Format.pp_print_string fmt ":position"
  | TTuple ts ->
    Format.fprintf fmt "[:tuple, %a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
         format_typ_descr)
      ts
  | TStruct s -> format_struct fmt s
  | TOption t -> Format.fprintf fmt "[:option, %a]" format_typ_descr t
  | TDefault t -> format_typ_descr fmt t
  | TEnum e -> format_enum fmt e
  | TAbstract t -> format_abstract fmt t
  | TArrow _ -> Format.pp_print_string fmt ":function"
  | TArray t1 -> Format.fprintf fmt "[:array, %a]" format_typ_descr t1
  | TVar _ | TClosureEnv -> Format.pp_print_string fmt ":any"
  | TForAll tb ->
    let _v, typ = Bindlib.unmbind tb in
    format_typ_descr fmt typ
  | TError -> assert false

(* Emit a source position as a [SourcePosition.new(...)] literal. The filename
   is a single-quoted Ruby literal, so that backslashes in Windows paths are
   preserved. *)
let format_pos (fmt : Format.formatter) (pos : Pos.t) : unit =
  Format.fprintf fmt "%s::SourcePosition.new(%s, %d, %d, %d, %d, [%a])" rt
    (ruby_string (Pos.get_file pos))
    (Pos.get_start_line pos) (Pos.get_start_column pos) (Pos.get_end_line pos)
    (Pos.get_end_column pos)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
       (fun fmt s -> Format.pp_print_string fmt (ruby_string s)))
    (Pos.get_law_info pos)

(* Prints a local variable at an assignment *)
let format_local ctx (fmt : Format.formatter) (v : VarName.t) : unit =
  if not (VarName.Set.mem v ctx.read_vars) then Format.pp_print_char fmt '_';
  VarName.format fmt v

let format_var ctx (fmt : Format.formatter) (v : VarName.t) : unit =
  if VarName.Set.mem v ctx.toplevel_vars then Format.pp_print_char fmt '@';
  VarName.format fmt v

let external_module ctx (modname : VarName.t) : ModuleName.t =
  VarName.Map.find modname ctx.modnames

(* Is the external reference [m.name] a function (as opposed to a value) ? *)
let external_is_function ctx (modname : VarName.t) (name : string) : bool =
  let in_module path =
    match Uid.Path.last_member path with
    | Some m -> (
      match ModuleName.Map.find_opt m ctx.sctx.modules with
      | Some v -> VarName.equal v modname
      | None -> false)
    | None -> false
  in
  let is_scope =
    ScopeName.Map.exists
      (fun s _ -> ScopeName.base s = name && in_module (ScopeName.path s))
      ctx.sctx.decl_ctx.ctx_scopes
  in
  is_scope
  || TopdefName.Map.exists
       (fun t (typ, _) ->
         TopdefName.base t = name
         && in_module (TopdefName.path t)
         &&
         match Mark.remove (Type.unquantify typ) with
         | TArrow _ -> true
         | _ -> false)
       ctx.sctx.decl_ctx.ctx_topdefs

let rounding_symbol : date_rounding -> string = function
  | RoundUp -> ":round_up"
  | RoundDown -> ":round_down"
  | AbortOnRound -> ":abort_on_round"

let rec format_expression ctx (fmt : Format.formatter) (e : expr) : unit =
  let format_args =
    Format.pp_print_list ~pp_sep:pp_comma (format_expression ctx)
  in
  match Mark.remove e with
  | EVar v -> format_var ctx fmt v
  | EFunc f -> Format.fprintf fmt "method(:%a)" FuncName.format f
  | EStruct { fields = es; name = s } ->
    Format.fprintf fmt "%a.new(%a)" format_struct s
      (Format.pp_print_list ~pp_sep:pp_comma (fun fmt (struct_field, e) ->
           Format.fprintf fmt "%a: %a" StructField.format struct_field
             (format_expression ctx) e))
      (StructField.Map.bindings es)
  | EStructFieldAccess { e1; field; _ } ->
    Format.fprintf fmt "%a.%a" (format_expression ctx) e1 StructField.format
      field
  | EInj { cons; name = e_name; _ }
    when EnumName.equal e_name ConstantNames.option_enum
         && EnumConstructor.equal cons ConstantNames.none_constr ->
    Format.fprintf fmt "%s::Option::NONE" rt
  | EInj { e1 = e; cons; name = e_name; _ }
    when EnumName.equal e_name ConstantNames.option_enum
         && EnumConstructor.equal cons ConstantNames.some_constr ->
    Format.fprintf fmt "%s::Option.some(%a)" rt (format_expression ctx) e
  | EInj { e1 = e; cons; name = enum_name; _ } ->
    Format.fprintf fmt "%a.new(:%a, %a)" format_enum enum_name
      EnumConstructor.format cons (format_expression ctx) e
  | EArray { elts; _ } -> Format.fprintf fmt "[%a]" format_args elts
  | ELit l -> format_lit fmt (Mark.copy e l)
  | EPosLit -> format_pos fmt (Mark.get e)
  | ETuple es -> Format.fprintf fmt "%s::Tuple.new(%a)" rt format_args es
  | ETupleAccess { e1; index; _ } ->
    Format.fprintf fmt "%a[%d]" (format_expression ctx) e1 index
  | EExternal { modname; name } ->
    let m = external_module ctx (Mark.remove modname) in
    let name = Mark.remove name in
    if external_is_function ctx (Mark.remove modname) name then
      Format.fprintf fmt "%a.method(:%s)" format_module_name m name
    else Format.fprintf fmt "%a.%s" format_module_name m name
  | EApp { f = EFunc f, _; args; _ } ->
    Format.fprintf fmt "%a(%a)" FuncName.format f format_args args
  | EApp { f = EExternal { modname; name }, _; args; _ } ->
    Format.fprintf fmt "%a.%s(%a)" format_module_name
      (external_module ctx (Mark.remove modname))
      (Mark.remove name) format_args args
  | EApp { f = EAppOp { op = Tag _, _; args = [_f]; _ }, _; args = [_arg]; _ }
    when Global.options.trace <> None ->
    Message.error "Traces in Ruby are not yet implemented"
  | EApp { f; args; _ } ->
    Format.fprintf fmt "%a.call(%a)" (format_expression ctx) f format_args args
  | EAppOp { op = Tag _, _; args = [arg1]; _ } -> format_expression ctx fmt arg1
  | EAppOp { op = (ToClosureEnv | FromClosureEnv), _; args = [arg1]; _ } ->
    (* Closure environments are only a typing artifact in a dynamic language *)
    format_expression ctx fmt arg1
  | EAppOp { op = Length, _; args = [l]; _ } ->
    Format.fprintf fmt "%a.length" (format_expression ctx) l
  | EAppOp { op = Not, _; args = [arg1]; _ } ->
    Format.fprintf fmt "(!%a)" (format_expression ctx) arg1
  | EAppOp
      {
        op = (Minus_int | Minus_rat | Minus_mon | Minus_dur), _;
        args = [arg1];
        _;
      } ->
    Format.fprintf fmt "(-%a)" (format_expression ctx) arg1
  | EAppOp { op = ArrayAccess n, _; args = [a]; _ } ->
    Format.fprintf fmt "%a[%d]" (format_expression ctx) a n
  | EAppOp { op = ConstructorCheck (enum, case), _; args = [a]; _ } ->
    if EnumName.equal enum ConstantNames.option_enum then
      if EnumConstructor.equal case ConstantNames.none_constr then
        Format.fprintf fmt "%a.none?" (format_expression ctx) a
      else Format.fprintf fmt "%a.some?" (format_expression ctx) a
    else
      Format.fprintf fmt "(%a.code == :%a)" (format_expression ctx) a
        EnumConstructor.format case
  | EAppOp { op = DebugPrint str, _; args = [a]; _ } ->
    Format.fprintf fmt "%s.debug_print(%s, %a)" rt (ruby_string str)
      (format_expression ctx) a
  | EAppOp
      { op = ValueFromJson (ty, str), _; args = [pos_expr; (ELit LUnit, _)]; _ }
    ->
    Format.fprintf fmt "%s.from_json(%a, %s, %a)" rt format_typ_descr ty
      (ruby_string str) (format_expression ctx) pos_expr
  | EAppOp
      {
        op = ((Add_dat_dur rounding | Sub_dat_dur rounding), _) as op;
        args = [pos; d; dur];
        _;
      } ->
    Format.fprintf fmt "%s.%s(%a, %a, %s, %a)" rt
      (match op with
      | Add_dat_dur _, _ -> "add_date_duration"
      | _ -> "sub_date_duration")
      (format_expression ctx) d (format_expression ctx) dur
      (rounding_symbol rounding) (format_expression ctx) pos
  | EAppOp
      {
        op =
          ( ( Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int
            | Div_mon_rat | Div_dur_dur | Eq | Lt | Lte | Gt | Gte ),
            _ ) as op;
        args = [pos; arg1; arg2];
        _;
      }
  | EAppOp { op = (Sort _, _) as op; args = [pos; arg1; arg2]; _ } ->
    (* the position comes first in the scalc arguments, but last in the runtime
       functions *)
    Format.fprintf fmt "%s.%s(%a, %a, %a)" rt
      (Option.get (runtime_function (Mark.remove op)))
      (format_expression ctx) arg1 (format_expression ctx) arg2
      (format_expression ctx) pos
  | EAppOp { op = Map2, _; args = [pos; f; l1; l2]; _ } ->
    Format.fprintf fmt "%s.map2(%a, %a, %a, %a)" rt (format_expression ctx) f
      (format_expression ctx) l1 (format_expression ctx) l2
      (format_expression ctx) pos
  | EAppOp { op = ((And | Or), _) as op; args = [arg1; arg2]; _ } ->
    (* right-associative *)
    let rec aux = function
      | EAppOp { op = op1; args = [arg2; arg3]; _ }, _
        when Mark.equal Operator.equal op op1 ->
        arg2 :: aux arg3
      | a -> [a]
    in
    format_binop ctx fmt op (arg1 :: aux arg2)
  | EAppOp
      {
        op =
          ( ( Add_int_int | Add_rat_rat | Add_mon_mon | Add_dur_dur
            | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dur_dur
            | Mult_int_int | Mult_rat_rat ),
            _ ) as op;
        args = [arg1; arg2];
        _;
      } ->
    (* left-associative *)
    let rec aux = function
      | EAppOp { op = op1; args = [arg2; arg3]; _ }, _
        when Mark.equal Operator.equal op op1 ->
        aux arg2 @ [arg3]
      | a -> [a]
    in
    format_binop ctx fmt op (aux arg1 @ [arg2])
  | EAppOp
      {
        op = ((Lt | Lte | Gt | Gte), _) as op;
        args = [arg1; arg2];
        tys = (TLit TBool, _) :: _;
      } ->
    (* Booleans are not ordered in Ruby *)
    Format.fprintf fmt "%s.%s(%a, %a)" rt
      (Option.get (runtime_function (Mark.remove op)))
      (format_expression ctx) arg1 (format_expression ctx) arg2
  | EAppOp { op; args = [_; _] as args; _ }
    when binop_symbol (Mark.remove op) <> None ->
    format_binop ctx fmt op args
  | EAppOp { op; args; _ } -> (
    match runtime_function (Mark.remove op) with
    | Some f -> Format.fprintf fmt "%s.%s(%a)" rt f format_args args
    | None ->
      Message.error ~internal:true ~pos:(Mark.get e)
        "Unsupported operator %a in the Ruby backend"
        (Print.operator ~debug:false)
        (Mark.remove op))

and format_binop ctx fmt op args =
  let sym = Option.get (binop_symbol (Mark.remove op)) in
  Format.fprintf fmt "(%a)"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf " %s " sym)
       (format_expression ctx))
    args

let format_pos_as_expr ctx ppf p = format_expression ctx ppf (EPosLit, p)

let format_trace_kind ctx ppf (tag, _pos) =
  let open Format in
  let format_pos = format_pos_as_expr ctx in
  let tr = rt ^ "::Trace" in
  match tag with
  | ScopeCall scopename ->
    let _, decl_pos = ScopeName.get_info scopename in
    fprintf ppf "%s::ScopeCall.new(%s, %a)" tr
      (ruby_string (ScopeName.original_base scopename))
      format_pos decl_pos
  | ScopeVarDef { var = scope_var; io } ->
    let _, decl_pos = ScopeVar.get_info scope_var in
    fprintf ppf "%s::ScopeVarDef.new(%s, %a, %s, %b)" tr
      (ruby_string (ScopeVar.original_string scope_var))
      format_pos decl_pos
      (match io.io_input with
      | NoInput -> ":no_input"
      | OnlyInput -> ":only_input"
      | Reentrant -> ":reentrant")
      io.io_output
  | LocalVarDef { name; decl_pos } ->
    fprintf ppf "%s::LocalVarDef.new(%s, %a)" tr (ruby_string name) format_pos
      decl_pos
  | FunCall topdef ->
    let _, decl_pos = TopdefName.get_info topdef in
    fprintf ppf "%s::FunCall.new(%s, %a)" tr
      (ruby_string (TopdefName.original_base topdef))
      format_pos decl_pos
  | LocalTupDef { names } ->
    fprintf ppf "%s::LocalTupDef.new([%a])" tr
      (pp_print_list ~pp_sep:pp_comma (fun fmt (name, decl_pos) ->
           fprintf fmt "%s::VarDef.new(%s, %a)" tr (ruby_string name) format_pos
             decl_pos))
      names
  | BranchingCondition -> fprintf ppf "%s::BranchingCondition.new" tr
  | Assertion -> fprintf ppf "%s::Assertion.new" tr
  | Branching None -> fprintf ppf "%s::IfBranching.new" tr
  | Branching (Some cstr) ->
    fprintf ppf "%s::MatchBranching.new(%s)" tr (ruby_string cstr)
  | Exception { label = None; cons_pos } ->
    fprintf ppf "%s::ExceptionTrace.new(nil, nil, %a)" tr format_pos cons_pos
  | Exception { label = Some (lbl, pos); cons_pos } ->
    fprintf ppf "%s::ExceptionTrace.new(%s, %a, %a)" tr (ruby_string lbl)
      format_pos pos format_pos cons_pos

let format_begin_trace ctx ppf ((_, pos) as tag) =
  Format.fprintf ppf "%s.begin_trace(%a, %a)" rt (format_trace_kind ctx) tag
    (format_pos_as_expr ctx) pos

let error_class (error : Runtime.error) : string =
  match error with DateError _ -> "DateError" | e -> Runtime.error_to_string e

(* In the switch cases, the payload variables are substituted by accesses to
   the fields of the matched value *)
let subst_payload switch_var field_name payload_var payload_typ pos block =
  Utils.subst_block payload_var
    (* Not a real catala struct, but will print as <var>.<field_name> *)
    ( EStructFieldAccess
        {
          e1 = EVar switch_var, pos;
          field = StructField.fresh (field_name, pos);
          name = StructName.fresh [] ("Dummy", pos);
        },
      pos )
    payload_typ pos block

(* Returns the blocks of the switch, with the payloads substituted, along with
   the corresponding constructors *)
let switch_blocks ctx s =
  match Mark.remove s with
  | SSwitch
      {
        switch_var;
        enum_name = e_name;
        switch_cases =
          [
            { case_block = case_none; _ };
            {
              case_block = case_some;
              payload_var_name = case_some_var;
              payload_var_typ;
            };
          ];
        _;
      }
    when EnumName.equal e_name ConstantNames.option_enum ->
    `Option
      ( subst_payload switch_var "value" case_some_var payload_var_typ
          (Mark.get s) case_some,
        case_none )
  | SSwitch { switch_var; enum_name = e_name; switch_cases = cases; _ } ->
    let cons_map = EnumName.Map.find e_name ctx.sctx.decl_ctx.ctx_enums in
    `Enum
      (List.map2
         (fun case (cons, _) ->
           ( cons,
             subst_payload switch_var "payload" case.payload_var_name
               case.payload_var_typ (Mark.get s) case.case_block ))
         cases
         (EnumConstructor.Map.bindings cons_map))
  | _ -> invalid_arg "switch_blocks"

(* The local variables assigned within a block, not counting nested function
   bodies *)
let rec block_locals ctx (acc : VarName.Set.t) (b : block) : VarName.Set.t =
  List.fold_left
    (fun acc ((stmt, _) as s) ->
      match stmt with
      | SInnerFuncDef { name; _ }
      | SLocalDecl { name; _ }
      | SLocalInit { name; _ }
      | SLocalDef { name; _ } ->
        VarName.Set.add (Mark.remove name) acc
      | SIfThenElse { then_block; else_block; _ } ->
        block_locals ctx (block_locals ctx acc then_block) else_block
      | SSwitch _ -> (
        match switch_blocks ctx s with
        | `Option (b1, b2) -> block_locals ctx (block_locals ctx acc b1) b2
        | `Enum cases ->
          List.fold_left (fun acc (_, b) -> block_locals ctx acc b) acc cases)
      | _ -> acc)
    acc b

(* The variables read in a block, including in nested function bodies *)
let rec block_reads (acc : VarName.Set.t) (b : block) : VarName.Set.t =
  let expr acc e = VarName.Set.union acc (Utils.get_vars e) in
  List.fold_left
    (fun acc (s, _) ->
      match s with
      | SInnerFuncDef { func; _ } -> block_reads acc func.func_body
      | SLocalDecl _ | SBeginTrace _ -> acc
      | SLocalInit { expr = e; _ } | SLocalDef { expr = e; _ } | SReturn e ->
        expr acc e
      | SFatalError { pos_expr; _ } -> expr acc pos_expr
      | SIfThenElse { if_expr; then_block; else_block } ->
        block_reads (block_reads (expr acc if_expr) then_block) else_block
      | SSwitch { switch_var; switch_cases; _ } ->
        List.fold_left
          (fun acc case -> block_reads acc case.case_block)
          (VarName.Set.add switch_var acc)
          switch_cases
      | SEndTrace { ret_var } ->
        Option.fold ~none:acc ~some:(fun v -> VarName.Set.add v acc) ret_var
      | SSpecialOp _ -> .)
    acc b

let code_item_reads acc = function
  | SVar { expr; _ } -> VarName.Set.union acc (Utils.get_vars expr)
  | SFunc { func; _ } | SScope { scope_body_func = func; _ } ->
    block_reads acc func.func_body

let rec format_statement ctx (fmt : Format.formatter) (s : stmt Mark.pos) : unit
    =
  match Mark.remove s with
  | SInnerFuncDef { name; func = { func_params; func_body; _ } } ->
    (* Distinct variables may be printed with the same name, we need to compare
       the printed names *)
    let to_string v = Format.asprintf "%a" (format_local ctx) v in
    let params =
      List.fold_left
        (fun acc (v, _) ->
          String.Set.add
            (Format.asprintf "%a" VarName.format (Mark.remove v))
            acc)
        String.Set.empty func_params
    in
    let locals =
      VarName.Set.fold
        (fun v acc -> String.Set.add (to_string v) acc)
        (block_locals ctx VarName.Set.empty func_body)
        String.Set.empty
    in
    let locals = String.Set.diff locals params in
    Format.fprintf fmt "@[<v 2>%a = ->(%a%a) do%a@]@,end" (format_local ctx)
      (Mark.remove name)
      (Format.pp_print_list ~pp_sep:pp_comma (fun fmt (var, _typ) ->
           VarName.format fmt (Mark.remove var)))
      func_params
      (fun fmt locals ->
        if not (String.Set.is_empty locals) then
          Format.fprintf fmt "; %a"
            (Format.pp_print_list ~pp_sep:pp_comma Format.pp_print_string)
            (String.Set.elements locals))
      locals (format_body ctx) func_body
  | SLocalDecl _ -> assert false (* Ruby doesn't need variable declarations *)
  | SLocalDef { expr = e; typ = TLit TUnit, _; _ }
  | SLocalInit { expr = e; typ = TLit TUnit, _; _ } ->
    format_expression ctx fmt e
  | SLocalDef { name = v; expr = e; _ } | SLocalInit { name = v; expr = e; _ }
    ->
    Format.fprintf fmt "%a = %a" (format_local ctx) (Mark.remove v)
      (format_expression ctx) e
  | SFatalError { pos_expr; error } ->
    let message =
      match error with
      | DateError m -> Some m
      | _ ->
        Pos.get_attr (Mark.get s) (function
          | ErrorMessage m -> Some m
          | _ -> None)
    in
    Format.fprintf fmt "raise %s::%s.new(%a%s)" rt (error_class error)
      (format_expression ctx) pos_expr
      (match message with None -> "" | Some m -> ", " ^ ruby_string m)
  | SIfThenElse { if_expr; then_block; else_block } ->
    let rec pr_else = function
      | [(SIfThenElse { if_expr; then_block; else_block }, _)] ->
        Format.fprintf fmt "@,@[<v 2>elsif %a%a@]" (format_expression ctx)
          if_expr (format_body ctx) then_block;
        pr_else else_block
      | [(SLocalDef { expr = ELit LUnit, _; _ }, _)]
      | [(SReturn (ELit LUnit, _), _)] ->
        ()
      | else_block ->
        Format.fprintf fmt "@,@[<v 2>else%a@]" (format_body ctx) else_block
    in
    Format.fprintf fmt "@[<v 2>if %a%a@]" (format_expression ctx) if_expr
      (format_body ctx) then_block;
    pr_else else_block;
    Format.fprintf fmt "@,end"
  | SSwitch { switch_var; _ } -> (
    match switch_blocks ctx s with
    | `Option (case_some, case_none) ->
      Format.fprintf fmt "@[<v 2>if %a.some?%a@]@,@[<v 2>else%a@]@,end"
        (format_var ctx) switch_var (format_body ctx) case_some
        (format_body ctx) case_none
    | `Enum cases ->
      Format.fprintf fmt "@[<v 0>case %a.code" (format_var ctx) switch_var;
      List.iter
        (fun (cons_name, block) ->
          Format.fprintf fmt "@,@[<v 2>when :%a%a@]" EnumConstructor.format
            cons_name (format_body ctx) block)
        cases;
      Format.fprintf fmt "@,end@]")
  | SReturn e1 -> Format.fprintf fmt "return %a" (format_expression ctx) e1
  | SBeginTrace t -> format_begin_trace ctx fmt t
  | SEndTrace { ret_var = None } -> Format.fprintf fmt "%s.end_trace(nil)" rt
  | SEndTrace { ret_var = Some v } ->
    Format.fprintf fmt "%s.end_trace(%a)" rt (format_var ctx) v
  | SSpecialOp _ -> .

and format_block ctx (fmt : Format.formatter) (b : block) : unit =
  Format.pp_open_vbox fmt 0;
  Format.pp_print_list (format_statement ctx) fmt
    (List.filter
       (fun s -> match Mark.remove s with SLocalDecl _ -> false | _ -> true)
       b);
  Format.pp_close_box fmt ()

(* Prints a block on new lines, or nothing if it is empty. To be used within an
   indented vbox *)
and format_body ctx (fmt : Format.formatter) (b : block) : unit =
  if List.exists (function SLocalDecl _, _ -> false | _ -> true) b then
    Format.fprintf fmt "@,%a" (format_block ctx) b

let format_ctx (type_ordering : TypeIdent.t list) (fmt : Format.formatter) ctx :
    unit =
  let format_struct_decl fmt (struct_name, struct_fields) =
    let fields = StructField.Map.bindings struct_fields in
    Format.fprintf fmt "@[<v 2>class %a < %s::Struct" StructName.format
      struct_name rt;
    Format.fprintf fmt "@,struct_name %s"
      (ruby_string
         (Format.asprintf "%a" StructName.format_original struct_name));
    List.iter
      (fun (struct_field, struct_field_type) ->
        Format.fprintf fmt "@,field :%a, %s, %a # %a" StructField.format
          struct_field
          (ruby_string
             (Format.asprintf "%a" StructField.format_original struct_field))
          format_typ_descr struct_field_type format_typ struct_field_type)
      fields;
    Format.fprintf fmt "@]@,end"
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    if EnumConstructor.Map.is_empty enum_cons then
      failwith "no constructors in the enum";
    Format.fprintf fmt "@[<v 2>class %a < %s::Enum" EnumName.format enum_name rt;
    Format.fprintf fmt "@,enum_name %s"
      (ruby_string (Format.asprintf "%a" EnumName.format_original enum_name));
    EnumConstructor.Map.iter
      (fun enum_cons enum_cons_type ->
        Format.fprintf fmt "@,constructor :%a, %s, %a # %a"
          EnumConstructor.format enum_cons
          (ruby_string
             (Format.asprintf "%a" EnumConstructor.format_original enum_cons))
          format_typ_descr enum_cons_type format_typ enum_cons_type)
      enum_cons;
    Format.fprintf fmt "@]@,end"
  in
  let is_in_type_ordering s =
    List.exists
      (fun struct_or_enum ->
        match struct_or_enum with
        | TypeIdent.Enum _ | TypeIdent.Abstract _ -> false
        | TypeIdent.Struct s' -> s = s')
      type_ordering
  in
  let scope_structs =
    List.map
      (fun (s, _) -> TypeIdent.Struct s)
      (StructName.Map.bindings
         (StructName.Map.filter
            (fun s _ -> not (is_in_type_ordering s))
            ctx.decl_ctx.ctx_structs))
  in
  List.iter
    (fun struct_or_enum ->
      match struct_or_enum with
      | TypeIdent.Struct s ->
        if StructName.path s = [] then
          Format.fprintf fmt "%a@,@," format_struct_decl
            (s, StructName.Map.find s ctx.decl_ctx.ctx_structs)
      | TypeIdent.Enum e ->
        if
          EnumName.path e = []
          && not (EnumName.equal e ConstantNames.option_enum)
        then
          Format.fprintf fmt "%a@,@," format_enum_decl
            (e, EnumName.Map.find e ctx.decl_ctx.ctx_enums)
      | TypeIdent.Abstract t ->
        if AbstractType.path t = [] then
          Format.fprintf fmt
            "@[<v 2>class %a < %s::Value@,\
             # Define your external type here@]@,\
             end@,\
             @,"
            AbstractType.format t rt)
    (type_ordering @ scope_structs)

let format_func ctx fmt (name : FuncName.t) (func : func) =
  Format.fprintf fmt "@[<h># (%a) -> %a@]@,"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
       (fun fmt (_, typ) -> format_typ fmt typ))
    func.func_params format_typ func.func_return_typ;
  Format.fprintf fmt "@[<v 2>def self.%a(%a)%a@]@,end@," FuncName.format name
    (Format.pp_print_list ~pp_sep:pp_comma (fun fmt (var, _) ->
         VarName.format fmt (Mark.remove var)))
    func.func_params (format_body ctx) func.func_body

let format_code_item ctx fmt = function
  | SVar { var; expr; typ; visibility = _ } -> (
    Format.fprintf fmt "@@%a = %a@," VarName.format var (format_expression ctx)
      expr;
    match Mark.remove (Type.unquantify typ) with
    | TArrow _ ->
      Format.fprintf fmt "@[<v 2>def self.%a(*args)@,@@%a.call(*args)@]@,end@,"
        VarName.format var VarName.format var
    | _ ->
      Format.fprintf fmt "@[<v 2>def self.%a@,@@%a@]@,end@," VarName.format var
        VarName.format var)
  | SScope { scope_body_var_defs = Some _; _ } ->
    Message.error ~internal:true
      "Found unexpected split scope variable definitions."
  | SFunc { var; func; visibility = _ } -> format_func ctx fmt var func
  | SScope { scope_body_name; scope_body_var = var; scope_body_func = func; _ }
    ->
    Format.fprintf fmt "@[<h># Scope %s@]@,"
      (ScopeName.original_base scope_body_name);
    format_func ctx fmt var func

let format_tests ctx ~module_const ppf (p : Ast.program) =
  let closures, tests = p.tests in
  if tests = [] then
    Message.warning
      "%a@{<magenta>#[test]@}@ attribute@ above@ their@ declaration."
      Format.pp_print_text
      "No test scope were found: the generated executable won't test any \
       computation. To mark scopes as tests, ensure they don't require inputs, \
       and add the "
  else
    let () =
      Message.debug "@[<hov 2>Generating entry points for scopes:@ %a@]@."
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           (fun ppf (s, _, _) -> ScopeName.format ppf s))
        tests
    in
    Format.fprintf ppf "@,# Automatic Catala tests@,";
    Format.fprintf ppf "@[<v 2>if __FILE__ == $PROGRAM_NAME";
    Format.fprintf ppf "@,@[<v 2>module %s" module_const;
    (* Closures lifted from the tests, when closure conversion is enabled *)
    List.iter
      (fun item -> Format.fprintf ppf "@,%a" (format_code_item ctx) item)
      closures;
    Format.fprintf ppf
      "@,@[<v 2>%s.run_tests(ARGV, lang: %s, description: %s) do |t|" rt
      (match p.lang with `Fr -> ":fr" | _ -> ":en")
      (ruby_string
         ("Catala test program"
         ^
         match p.module_name with
         | Some (m, _) -> Format.asprintf " for %a" ModuleName.format_original m
         | None -> ""));
    List.iter
      (fun (name, var, block) ->
        Format.fprintf ppf "@,@[<v 2>t.scope(%s"
          (ruby_string (ScopeName.original_base name));
        (if Global.options.trace <> None then
           let scope_pos = Mark.get (ScopeName.get_info name) in
           Format.fprintf ppf ", trace: [%a, %a]" (format_trace_kind ctx)
             (ScopeCall name, scope_pos)
             (format_pos_as_expr ctx) scope_pos);
        Format.fprintf ppf ") do%a@,%a@]@,end" (format_body ctx) block
          (format_var ctx) var)
      tests;
    Format.fprintf ppf "@]@,end@]@,end@]@,end@,"

(* Format indents empty lines within boxes, and the separators between items
   may add up: we strip the trailing spaces, consecutive blank lines and blank
   lines before an [end] *)
let cleanup_layout (code : string) : string =
  let lines = String.split_on_char '\n' code in
  let is_blank l = String.trim l = "" in
  let is_end l = String.trim l = "end" in
  let rec aux = function
    | l1 :: (l2 :: _ as r) when is_blank l1 && (is_blank l2 || is_end l2) ->
      aux r
    | l :: r ->
      let l = if is_blank l then "" else l in
      let len = ref (String.length l) in
      while !len > 0 && l.[!len - 1] = ' ' do
        decr len
      done;
      String.sub l 0 !len :: aux r
    | [] -> []
  in
  String.concat "\n" (aux lines)

let format_program
    ~(default_module_name : string)
    (output_file : File.t option)
    (out_fmt : Format.formatter)
    (p : Ast.program)
    (type_ordering : TypeIdent.t list) : unit =
  let buf = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buf in
  (* Disable the formatting line length limit: Ruby is sensitive to newlines,
     so we must make sure that Format never breaks lines on its own *)
  Format.pp_set_geometry fmt ~max_indent:999_990 ~margin:1_000_000;
  Format.pp_open_vbox fmt 0;
  let module_const =
    module_constant
      (match p.module_name with
      | Some (m, _) -> ModuleName.to_string (ModuleName.normalise m)
      | None -> default_module_name)
  in
  let ctx =
    {
      sctx = p.ctx;
      toplevel_vars =
        List.fold_left
          (fun acc -> function
            | SVar { var; _ } -> VarName.Set.add var acc | _ -> acc)
          VarName.Set.empty
          (p.code_items @ fst p.tests);
      modnames =
        ModuleName.Map.fold
          (fun m v acc -> VarName.Map.add v m acc)
          p.ctx.modules VarName.Map.empty;
      read_vars =
        (let closures, tests = p.tests in
         let acc =
           List.fold_left code_item_reads VarName.Set.empty
             (p.code_items @ closures)
         in
         List.fold_left
           (fun acc (_, var, block) ->
             block_reads (VarName.Set.add var acc) block)
           acc tests);
    }
  in
  let header =
    (if Global.options.gen_external then
       [
         "# This is a template file following the expected interface and \
          declarations to";
         "# implement the corresponding Catala module.";
         "#";
         "# You should replace all `raise CatalaRuntime::Impossible` \
          place-holders with";
         "# your implementation and rename it to remove the \".template\" \
          suffix.";
       ]
     else
       ["# This file has been generated by the Catala compiler, do not edit!"])
    @ [""; "require 'catala_runtime'"]
  in
  Format.pp_print_list Format.pp_print_string fmt header;
  List.iter
    (fun (m, _) ->
      Format.fprintf fmt "@,require_relative %s"
        (ruby_string (ModuleName.to_string (ModuleName.normalise m))))
    (Program.modules_to_list ~trim_stdlib:true p.ctx.decl_ctx.ctx_modules);
  Format.fprintf fmt "@,@,@[<v 2>module %s@," module_const;
  format_ctx type_ordering fmt p.ctx;
  Format.pp_print_list ~pp_sep:Format.pp_print_cut (format_code_item ctx) fmt
    p.code_items;
  Format.fprintf fmt "@]@,end@,";
  if snd p.tests <> [] then format_tests ctx ~module_const fmt p;
  Format.pp_close_box fmt ();
  Format.pp_print_flush fmt ();
  Format.pp_print_string out_fmt (cleanup_layout (Buffer.contents buf));
  Format.pp_print_flush out_fmt ();
  if Global.options.gen_external then
    output_file
    |> Option.iter
         (Message.result "Generated template external implementations:@ %a"
            File.format)
