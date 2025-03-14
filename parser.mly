/* parser.mly */
%{
open Ast

(* Transforme une liste de paramètres en fonction "currisée" *)
let rec curry params body =
  match params with
  | [] -> body
  | p :: ps -> Fonction(p, curry ps body)
%}

/* Pour lever le problème du "dangling else" :
   la production SI ... ALORS ... sans SINON a une précédence inférieure (INFERIEUR_A_SINON)
   afin que le SINON éventuel se lie toujours au SI le plus proche. */
%nonassoc INFERIEUR_A_SINON
%nonassoc ELSE

/* Déclaration des tokens */
%token <int> ENTIER
%token <string> IDENT
%token LET IN IF THEN ELSE WHILE FOR DO TO FUNCTION NOT
%token PLUS MINUS TIMES DIV MOD AND OR SUPEGAL INFEGAL SUP INF SEMI LPAREN RPAREN
%token TRUE FALSE
%token EGAL ARROW
%token EOF

%start programme
%type <Ast.fonction list> programme

%%

programme:
  | fonctions EOF { $1 }

(* Définition d'une liste (possiblement vide) de fonctions globales *)
fonctions:
  | { [] }
  | fonctions fonction { $1 @ [$2] }

(* Définition d'une fonction de haut niveau *)
fonction:
  | LET IDENT params EGAL expr_seq { { nom = $2; params = $3; corps = $5 } }

(* Liste de paramètres d'une fonction *)
params:
  | { [] }
  | IDENT params { $1 :: $2 }

(* Une séquence d'expressions, séparées par ";".
   On construit un nœud Sequence contenant la liste des expressions. *)
expr_seq:
  | expr { $1 }
  | expr_seq SEMI expr {
        match $1 with
        | Sequence l -> Sequence (l @ [$3])
        | e -> Sequence [e; $3]
    }

(* Les expressions.
   On gère ici les liaisons locales (avec support pour des définitions avec paramètres via currification),
   la condition (avec dangling else), les boucles, la définition de fonctions anonymes, ainsi que les opérations. *)
expr:
  | LET IDENT params EGAL expr IN expr { Let($2, curry $3 $5, $7) }
  | IF expr THEN expr %prec INFERIEUR_A_SINON { Si($2, $4, None) }
  | IF expr THEN expr ELSE expr { Si($2, $4, Some $6) }
  | WHILE expr DO expr { TantQue($2, $4) }
  | FOR IDENT EGAL expr TO expr DO expr { Pour($2, $4, $6, $8) }
  | FUNCTION IDENT ARROW expr { Fonction($2, $4) }
  | logical_expr { $1 }

(* Gestion des opérations logiques, avec OU de plus faible priorité *)
logical_expr:
  | logical_expr OR and_expr { Binop(Ou, $1, $3) }
  | and_expr { $1 }

(* Gestion de l'opérateur ET *)
and_expr:
  | and_expr AND equality_expr { Binop(Et, $1, $3) }
  | equality_expr { $1 }

(* Gestion des comparaisons *)
equality_expr:
  | equality_expr EGAL additive_expr { Binop(Egal, $1, $3) }
  | equality_expr SUP additive_expr { Binop(Sup, $1, $3) }
  | equality_expr INF additive_expr { Binop(Inf, $1, $3) }
  | equality_expr SUPEGAL additive_expr { Binop(Supegal, $1, $3) }
  | equality_expr INFEGAL additive_expr { Binop(Infegal, $1, $3) }
  | additive_expr { $1 }

(* Opérations additives (+ et -) *)
additive_expr:
  | additive_expr PLUS multiplicative_expr { Binop(Plus, $1, $3) }
  | additive_expr MINUS multiplicative_expr { Binop(Moins, $1, $3) }
  | multiplicative_expr { $1 }

(* Opérations multiplicatives (fois, /, mod) *)
multiplicative_expr:
  | multiplicative_expr TIMES unary_expr { Binop(Fois, $1, $3) }
  | multiplicative_expr DIV unary_expr { Binop(Divise, $1, $3) }
  | multiplicative_expr MOD unary_expr { Binop(Modulo, $1, $3) }
  | unary_expr { $1 }

(* Opérations unaire (NON) et appels par juxtaposition *)
unary_expr:
  | NOT unary_expr { Unop(Non, $2) }
  | app_expr { $1 }

(* Gestion des appels de fonction par juxtaposition *)
app_expr:
  | app_expr primary_expr { Application($1, $2) }
  | primary_expr { $1 }

(* Expressions de base *)
primary_expr:
  | LPAREN expr RPAREN { $2 }
  | ENTIER { Entier($1) }
  | TRUE { Booleen(true) }
  | FALSE { Booleen(false) }
  | IDENT { Variable($1) }

%%
