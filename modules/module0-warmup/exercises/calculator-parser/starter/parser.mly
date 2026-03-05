(* ================================================================
   Exercise 5: Calculator Parser
   ================================================================

   This Menhir grammar defines the syntax of a simple calculator
   and produces an AST defined in ast.ml.

   TODO: Complete the [expr] and [atom] rules.
   The [program] rule is provided as an example.

   This directly mirrors Lab 2's parser.mly structure -- same
   %left/%prec annotations, same rule organization.
   ================================================================ *)

%{
  open Ast
%}

(* ---- Token declarations ---- *)
%token <int> INT
%token <string> IDENT
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN
%token EOF

(* ---- Precedence and associativity (lowest to highest) ---- *)
%left PLUS MINUS
%left STAR SLASH
%nonassoc UMINUS

(* ---- Start symbol ---- *)
%start <Ast.expr> program

%%

(* ================================================================
   PROVIDED RULE -- do not modify.
   ================================================================ *)

program:
  | e = expr EOF { e }
  ;

(* ================================================================
   TODO: Complete the grammar rules below.

   HINT 1 -- [expr] should handle binary operations:
     e1 + e2    (use PLUS token, produce BinOp(Add, e1, e2))
     e1 - e2    (use MINUS token, produce BinOp(Sub, e1, e2))
     e1 * e2    (use STAR token, produce BinOp(Mul, e1, e2))
     e1 / e2    (use SLASH token, produce BinOp(Div, e1, e2))
   Plus a fallthrough to [atom]:
     a = atom   { a }

   Example rule (given):
     | e1 = expr PLUS e2 = expr { BinOp (Add, e1, e2) }

   HINT 2 -- [atom] should handle:
     - Integer literal:           INT
     - Variable:                  IDENT
     - Parenthesized expression:  LPAREN expr RPAREN
     - Unary minus:  MINUS atom   with %prec UMINUS
   ================================================================ *)

expr:
  (* The PLUS rule is given as an example. Add the rest. *)
  | e1 = expr PLUS e2 = expr  { BinOp (Add, e1, e2) }
  (* EXERCISE: add rules for MINUS, STAR, SLASH *)
  | e1 = expr MINUS e2 = expr { BinOp (Sub, e1, e2) }
  | e1 = expr STAR e2 = expr  { BinOp (Mul, e1, e2) }
  | e1 = expr SLASH e2 = expr { BinOp (Div, e1, e2) }
  | a = atom                   { a }
  ;

atom:
  | n = INT                    { Num n }
  (* EXERCISE: add rules for IDENT, LPAREN/RPAREN, and unary MINUS *)
  | x = IDENT                  { Var x }
  | LPAREN e = expr RPAREN     { e }
  | MINUS a = atom %prec UMINUS { Neg a }
  ;
