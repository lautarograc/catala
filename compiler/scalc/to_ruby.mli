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

(** Formats a statement calculus program into a valid Ruby program *)

open Catala_utils
open Shared_ast

val op_needs_pos : 'a operator -> naked_typ -> bool

(* Exposed for testing: emits a source position literal with the filename
   escaped (Windows backslash paths must survive as a Ruby string literal). *)
val format_pos : Format.formatter -> Pos.t -> unit
val renaming : Renaming.t

val module_constant : string -> string
(** [module_constant name] is the name of the Ruby constant used for the module
    wrapping the code generated from the Catala module (or file) [name]. *)

val format_program :
  default_module_name:string ->
  File.t option ->
  Format.formatter ->
  Ast.program ->
  TypeIdent.t list ->
  unit
(** Usage [format_program ~default_module_name file fmt p type_ordering]. The
    generated code is wrapped in a Ruby module named after the Catala module, or
    after [default_module_name] if the program is not a module. *)
