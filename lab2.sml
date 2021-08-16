(***********************************************************)
(*       LISP lexer                                        *)
(*                                                         *)
(*       Carter King                                       *)
(*       Dr. Larkins                                       *)
(*       February 23, 2021                                 *)
(*       
         This lab reads in a text file that contains code
         from the LISP programming language. The SML lexer
         reads in the file into a list of characters. The
         list of characters is transformed into a list of 
         LISP tokens removing all of the whitespace, 
         newline, and tab characters..                     *)
(*                                                         *)
(***********************************************************)

exception LexerError;

datatype sign =
    Plus
  | Minus
;

datatype atom =
  T
| NIL
| Int of int
| Ident of string
;

datatype token =
  Lparen
| Rparen
| Dot
| Sign of sign
| Atom of atom
;

(* spaces receives a list of tokens and removes all of the 
   whitespaces, tabs, and newlines. Returns the list of 
   characters starting with a non-space character *)

fun spaces [] = []
  | spaces (x    :: tks) = if Char.isSpace x then spaces tks else (x :: tks)
;

(* lexid receives a list containing an alphabet character 
   and the list of characters. lexid recursively calls itself
   until the sequence of AlphaNumeric characters is over. The
   function returns a tuple containing the string of the 
   combined AlphaNumeric characters and the list of characters *)

fun lexid ls [] = ((implode ls), [])
  | lexid ls (tk :: tks) = if Char.isAlphaNum tk then lexid (ls @ [tk]) tks
                                            else ((implode ls), (tk :: tks))

;



(* lexint receives a list containing a digit character
   and the list of characters. lexint recursively calls itself
   until the sequence of numeric characters is over. The 
   function returns a tuple containing the string of the 
   combined AlphaNumeric characters and the list of characters *)

fun lexint ls [] = (valOf(Int.fromString(implode(ls))), [])
  | lexint ls (tk :: tks) = if Char.isDigit tk then lexint (ls @ [tk]) tks
                            else (valOf(Int.fromString(implode(ls))), (tk :: tks))
;


(* lexer takes a list containing the characters in a LISP
   program. The function handles the logic in turning this
   list into a list of token strings. *)

fun  lexer (#"." :: t)  = Dot         :: lexer(t)
 |   lexer (#"(" :: t)  = Lparen      :: lexer(t)
 |   lexer (#")" :: t)  = Rparen      :: lexer(t)
 |   lexer (#"+" :: t)  = Sign(Plus)  :: lexer(t)
 |   lexer (#"-" :: t)  = Sign(Minus) :: lexer(t)
 |   lexer (#" " :: #"T" :: #" " :: t)  = Atom(T)     :: lexer(t)
 |   lexer (#" " :: #"n" :: #"i" :: #"l" :: t) = Atom(NIL) :: lexer(t)
 |   lexer (x    :: t)  = 
       if      Char.isSpace x then lexer(spaces (t))
       else if Char.isDigit x then
        let val (a, b) = lexint [x] t in Atom(Int(a)) :: lexer(b) end
       else                  
        let val (c, d) = lexid [x] t in Atom(Ident(c))         :: lexer(d) end
 |   lexer [] = []
;

fun print_tokens [] = print("\n")
  | print_tokens (Lparen :: t) = (print("Lparen "); print_tokens(t))
  | print_tokens (Rparen :: t) = (print("Rparen "); print_tokens(t))
  | print_tokens (Dot :: t) = (print("Dot "); print_tokens(t))
  | print_tokens (Sign(Plus) :: t) = (print("Plus "); print_tokens(t))
  | print_tokens (Sign(Minus) :: t) = (print("Minus "); print_tokens(t))
  | print_tokens (Atom(a) :: t) =
  (case a of
   T => (print("Atom(T) "); print_tokens(t))
   | NIL => (print("Atom(NIL) "); print_tokens(t))
   | Int i => (print("Atom(Int(" ^ Int.toString(i) ^ ")) "); print_tokens(t))
   | Ident s => (print("Atom(Ident(" ^ s ^ ")) "); print_tokens(t)))
  ;


fun reader(copt: char option, is, l) =
  case copt of
    NONE    => (TextIO.closeIn is; l)
  | SOME(c) => reader (TextIO.input1 is, is, (l@[c]))
  ;

let
  val args = CommandLine.arguments();
  val ins = TextIO.openIn(hd(args));
in
  print_tokens(lexer(reader(TextIO.input1 ins, ins, [])))
end;

val _ = OS.Process.exit(OS.Process.success)
