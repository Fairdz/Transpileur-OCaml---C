(* translate.ml *)

open Ast
open W

(* Conversion d'un type inféré en chaîne correspondant au type C *)
let rec string_of_c_type t =
  match t with
  | TInt -> "int"
  | TBool -> "bool" 
  | TUnit -> "void"
  | TFun (_, _) -> failwith "Les fonctions de haut niveau sont traduites séparément."
  | TVar n -> failwith ("Type non déterminé : 'a" ^ string_of_int n)

(* Traduction d'un opérateur binaire OCaml en opérateur C *)
let string_of_binop op =
  match op with
  | Plus -> "+"
  | Moins -> "-"
  | Fois -> "*"
  | Divise -> "/"
  | Modulo -> "%"
  | Et -> "&&"
  | Ou -> "||"
  | Egal -> "=="
  | Infegal -> "<="
  | Supegal -> ">="
  | Inf -> "<"
  | Sup -> ">"

(* Traduction d'un opérateur unaire *)
let string_of_unop op =
  match op with
  | Non -> "!"

(* Indente une chaîne selon le niveau d'indentation *)
let indent level s =
  let prefix = String.make (2 * level) ' ' in
  prefix ^ s

(* Fonction utilitaire pour retirer une paire de parenthèses en trop, si présentes (notamment lors de la gestion des conditions) *)
let strip_outer_parens s =
  let s = String.trim s in
  if String.length s >= 2 && s.[0] = '(' && s.[String.length s - 1] = ')' then
    String.sub s 1 (String.length s - 2)
  else s

(* Traduction d'une expression OCaml en code C.
   Le paramètre [is_expr] indique si l'expression doit être traduite en tant qu'expression (par exemple pour un opérateur ternaire) ou en tant qu'instruction (/!\ non maintenu).
*)
let rec translate_expr e is_expr level =
  match e with
  | Entier n -> string_of_int n
  | Booleen b -> if b then "true" else "false"
  | Variable x -> x
  | Binop (op, e1, e2) ->
      "(" ^ translate_expr e1 true level ^ " " ^ string_of_binop op ^ " " ^ translate_expr e2 true level ^ ")"
  | Unop (op, e1) ->
      string_of_unop op ^ translate_expr e1 true level
  | Si (cond, e_then, None) ->
      if is_expr then
        "(( " ^ translate_expr cond true level ^ " ) ? " ^ translate_expr e_then true level ^ " : 0)"
      else
        indent level ("if (" ^ strip_outer_parens (translate_expr cond true level) ^ ") {\n" ^
                      indent (level+1) ("return " ^ translate_expr e_then false (level+1) ^ ";") ^ "\n" ^
                      indent level "}")
  | Si (cond, e_then, Some e_else) ->
      if is_expr then
        "(( " ^ translate_expr cond true level ^ " ) ? " ^ translate_expr e_then true level ^ " : " ^ translate_expr e_else true level ^ ")"
      else
        indent level ("if (" ^ strip_outer_parens (translate_expr cond true level) ^ ") {\n" ^
                      indent (level+1) ("return " ^ translate_expr e_then false (level+1) ^ ";") ^ "\n" ^
                      indent level "} else {\n" ^
                      indent (level+1) ("return " ^ translate_expr e_else false (level+1) ^ ";") ^ "\n" ^
                      indent level "}")
  | TantQue (cond, body) ->
      indent level ("while (" ^ translate_expr cond true level ^ ") {\n" ^
                    translate_expr body false (level+1) ^ "\n" ^
                    indent level "}")
  | Pour (x, e1, e2, body) ->
      indent level ("for (int " ^ x ^ " = " ^ translate_expr e1 true level ^ "; " ^
                    x ^ " <= " ^ translate_expr e2 true level ^ "; " ^ x ^ "++) {\n" ^
                    translate_expr body false (level+1) ^ "\n" ^
                    indent level "}")
  | Let (x, e1, e2) ->
      indent level ("{\n" ^
                    indent (level+1) ("int " ^ x ^ " = " ^ translate_expr e1 true (level+1) ^ ";\n") ^
                    translate_expr e2 false (level+1) ^ "\n" ^
                    indent level "}")
  | Sequence exprs ->
      translate_sequence exprs level
  | Fonction (_, _) ->
      failwith "Traduction des fonctions anonymes non supportée dans cette version."
  | Application (Variable "print_int", e) ->
      "printf(\"%d\", " ^ translate_expr e true level ^ ");"
  | Application (e1, e2) ->
      translate_expr e1 true level ^ "(" ^ translate_expr e2 true level ^ ")"

(* Traduction d'une séquence d'expressions en bloc C. Pour chaque instruction, si le code généré se termine par '}' (bloc), on n'ajoute pas de ';'. *)
and translate_sequence exprs level =
  let rec aux = function
    | [] -> ""
    | [e] ->
        let code = translate_expr e true level in
        if String.trim code <> "" && code.[(String.length code) - 1] = '}' then indent level code
        else indent level (code ^ ";")
    | e :: es ->
        let code = translate_expr e true level in
        let code_line =
          if String.trim code <> "" && code.[(String.length code) - 1] = '}' then indent level code
          else indent level (code ^ ";")
        in
        code_line ^ "\n" ^ aux es
  in
  aux exprs

(* Construction de l'environnement global à partir des fonctions. On associe à chaque fonction un schéma avec une variable fraîche. *)
let build_global_env functions =
  List.fold_left (fun env f ->
    (f.nom, Forall ([], W.fresh_tyvar ())) :: env
  ) [] functions

(* Traduction d'une fonction globale OCaml en fonction C. Si la fonction représente le main (nom = "()"), elle sera traitée séparément. *)
let translate_function global_env f =
  if f.nom = "()" then ""
  else
    let env_params = List.map (fun x -> (x, Forall ([], TInt))) f.params in
    let env = global_env @ env_params in
    let s, body_type = W.w env f.corps in
    let inferred_body_type = W.apply_subst s body_type in
    let ret_type = match inferred_body_type with
      | TInt -> "int"
      | TBool -> "bool"
      | TUnit -> "void"
      | _ -> "int"
    in
    let params_str =
      if f.params = [] then "void"
      else String.concat ", " (List.map (fun x -> "int " ^ x) f.params)
    in
    let body_code_raw = translate_expr f.corps false 1 in
    let body_code =
      if ret_type = "void" then body_code_raw
      else
        let trimmed = String.trim body_code_raw in
        if String.length trimmed >= 2 && ((String.sub trimmed 0 2) = "if" || (String.sub trimmed 0 1) = "{")
        then body_code_raw
        else indent 1 ("return " ^ trimmed ^ ";")
    in
    ret_type ^ " " ^ f.nom ^ "(" ^ params_str ^ ") {\n" ^ body_code ^ "\n}\n"

(* Traduction du bloc principal en fonction C main *)
let translate_main e =
  "int main(void) {\n" ^ translate_expr e false 1 ^ "\n  return 0;\n}\n"

(* Traduction d'un programme (liste de fonctions globales et bloc principal) en code C complet *)
let translate_program functions main_expr =
  let header = "#include <stdio.h>\n#include <stdbool.h>\n\n" in
  let global_env = build_global_env functions in
  let funcs =
    List.filter (fun f -> f.nom <> "()") functions 
    |> List.map (translate_function global_env)
  in
  let main_func = translate_main main_expr in
  header ^ (String.concat "\n" funcs) ^ "\n" ^ main_func

(* Recherche la fonction main (déclarée par let () = ...) *)
let rec chercher_main functions =
  match functions with
  | [] -> failwith "Le fichier ne contient pas de main"
  | t :: q when t.nom = "()" -> t.corps
  | _ :: q -> chercher_main q

(* Écriture du code C généré dans le fichier [filename] *)
let write_c_file filename functions =
  let main_expr = chercher_main functions in 
  let oc = open_out filename in
  output_string oc (translate_program functions main_expr);
  close_out oc;
  Printf.printf "Fichier C généré : %s\n" filename
