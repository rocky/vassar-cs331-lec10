open Printf
open Sexplib.Sexp
module Sexp = Sexplib.Sexp

(* grammar representing current language *)
(* expr :=  <number>
          | true
          | false
          | <identifier>
          | (<op> <expr>)
          | (let (<identifier> <expr>) <expr>)
          | (if expr1 expr2 expr3)
          | (<comp> <expr> <expr>)
   op : = inc | dec
   comp := < | > | =
*)

(* A position range in the source text.
   This is a tuple of start and end line + column
   positions.
*)
type source_position_type = Sexp.Annotated.range

(* defining the 331 language *)
type op = Inc | Dec
type comp = Eq | Le | Gt

(* abstract syntax tree *)
type expr =
  (* EBool(true) *)
  | EBool of bool * source_position_type
  | ENum of int * source_position_type (* ENum(4) *)
  (* USE of variable EId("x") *)
  | EId of string * source_position_type
  | EOp of op * expr * source_position_type (* EOp(Inc,ENum(4)) *)
  (* declaration of variable: variable name, value expr, body expr *)
  (* ELet("x",ENum(4), EOp(Inc,EId("x"))) *)
  | ELet of string * expr * expr * source_position_type
  (* EIf(ENum(2),ENum(3),ENum(4))) *)
  | EIf of expr * expr * expr * source_position_type
  (* EComp(Eq,ENum(3),ENum(5)) *)
  | EComp of comp * expr * expr * source_position_type

(* env is a var name and its value *)
type tenv = (string * int) list

(* types for assembly instructions *)
type reg = Rax | Rsp
type arg = Const of int | Reg of reg | RegOffset of int * reg

type instr =
  | Loc of source_position_type
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMov of arg * arg
  | ICmp of arg * arg
  (* unconditional jump *)
  | IJmp of string
  (* jump if = *)
  | IJe of string
  (* jump if not = *)
  | IJne of string
  (* jump greater or = *)
  | IJge of string
  (* jump less than or =  *)
  | IJle of string
  (* making a label you can jump to *)
  | ILab of string
  | IRet

(* converts register into a string *)
let reg_to_string (r : reg) : string =
  match r with Rax -> "%rax" | Rsp -> "%rsp"

(* converts instruction argument into a string *)
let arg_to_string (a : arg) : string =
  match a with
  | Const n -> sprintf "$%d" n
  | Reg r -> reg_to_string r
  | RegOffset (n, r) -> sprintf "%d(%s)" n (reg_to_string r)

(* converts instruction into a string *)
let instr_to_string (i : instr) : string =
  match i with
  | IAdd (s, d) -> sprintf "add %s, %s" (arg_to_string s) (arg_to_string d)
  | ISub (s, d) -> sprintf "sub %s, %s" (arg_to_string s) (arg_to_string d)
  | IMov (s, d) -> sprintf "mov %s, %s" (arg_to_string s) (arg_to_string d)
  | ICmp (x, y) -> sprintf "cmp %s, %s" (arg_to_string x) (arg_to_string y)
  | IJmp l -> sprintf "jmp %s" l
  | IJe l -> sprintf "je %s" l
  | IJne l -> sprintf "jne %s" l
  | IJge l -> sprintf "jge %s" l
  | IJle l -> sprintf "jle %s" l
  | ILab l -> sprintf "%s: " l
  | IRet -> "ret"
  | Loc p -> sprintf ".loc 1 %d %d" (p.start_pos.line + 1) (p.start_pos.col + 1)
(* HELPER FUNCTIONS *)

(* wrapper for int_of_string *)
(* try to make string into an integer,
   if not possible return None instead of error *)
let int_of_string_opt (s : string) : int option =
  try Some (int_of_string s) with Failure _ -> None

(* lookup variable in env and return associated value *)
let rec find (env : tenv) (x : string) : int option =
  match env with
  | [] -> None
  | (name, value) :: tl -> if name = x then Some value else find tl x

(* converts offsets of stack to instruction argument *)
let stackloc (i : int) : arg = RegOffset (i * -8, Rsp)

(* counter to generate unique label names *)
(* this a reference to an integer *)
let counter : int ref = ref 0

(* makes a new label by adding a unique number to string *)
let new_label (s : string) : string =
  (* access old value of counter *)
  let cur : int = !counter in
  (* convert value into a string *)
  let count_str : string = string_of_int cur in
  (* update value of counter *)
  counter := cur + 1;
  (* and concatenate counter to base string *)
  s ^ count_str

let rec sexp_to_expr_with_position (sexp_annotated : Sexp.Annotated.t) =
  match sexp_annotated with
  | Atom (source_position, type_t) -> (
      match type_t with
      | Atom "true" -> EBool (true, source_position)
      | Atom "false" -> EBool (false, source_position)
      | Atom s -> (
          match int_of_string_opt s with
          | None -> EId (s, source_position)
          | Some i -> ENum (i, source_position))
      | _ -> failwith "Error parsing an Atom sexp")
  | List (source_position, annotated_list, type_t) -> (
      match List.hd annotated_list with
      (* inc and dec must be followed by exactly one expression *)
      | Atom (source_position, Atom "inc") ->
          EOp (Inc, (sexp_to_expr_with_position (List.nth annotated_list 1)), source_position)
      | Atom (source_position, Atom "dec") ->
          EOp (Dec, (sexp_to_expr_with_position (List.nth annotated_list 1)), source_position)
      (* (\* need to match down an extra level to access variable name *\) *)
      (* | List [ Atom "let"; List [ Atom name; thing1 ]; thing2 ] -> *)
      (*     ELet (name, sexp_to_expr thing1, sexp_to_expr thing2) *)
      (* | List [ Atom "if"; thing1; thing2; thing3 ] -> *)
      (*     EIf (sexp_to_expr thing1, sexp_to_expr thing2, sexp_to_expr thing3) *)
      | Atom(source_position, Atom "=") ->
        EComp
          (Eq,
           sexp_to_expr_with_position (List.nth annotated_list 1),
           sexp_to_expr_with_position (List.nth annotated_list 2),
           source_position)
      | Atom(source_position, Atom "<") ->
        EComp
          (Le,
           sexp_to_expr_with_position (List.nth annotated_list 1),
           sexp_to_expr_with_position (List.nth annotated_list 2),
           source_position)
      | Atom(source_position, Atom ">") ->
        EComp
          (Gt,
           sexp_to_expr_with_position (List.nth annotated_list 1),
           sexp_to_expr_with_position (List.nth annotated_list 2),
           source_position)
      (* any other List s-expressions aren't legal in the 331 language *)
      | _ -> failwith "Error parsing a List sexp")

let parse_with_position (s : string) : expr =
  (* "parse()" but saving position information.
     First turn a string into an sexpression (built-in).
     Then turn an position-annotated sexpression into an expression.
   *)
  sexp_to_expr_with_position (Sexp.Annotated.of_string s)

(* turns an expression into a list of instructions *)
(* env is the variable environment that maps variable names to the stack
   offset where they are stored *)
(* si is the next available stack index *)
let rec expr_to_instrs (e : expr) (env : tenv) (si : int) : instr list =
  match e with
  | EBool (b, source_position) ->
      (* if b then [IMov(Const(1),Reg(Rax))]
                  else [IMov(Const(0),Reg(Rax))] *)
      (* even more compact version of the above *)
      [ Loc source_position; IMov (Const (if b then 1 else 0), Reg Rax) ]
  | ENum (i, source_position) ->
      (* move into rax *)
      [ Loc source_position; IMov (Const i, Reg Rax) ]
  | EId (x, source_position) -> (
      (* look up x in env *)
      match find env x with
      | None -> failwith "Unbound variable"
      | Some i ->
          (* move from location in env to rax *)
          [ Loc source_position; IMov (stackloc i, Reg Rax) ])
  | EOp (op, e2, source_position) ->
      (* handle nested expression *)
      let rec_instrs = expr_to_instrs e2 env si in
      (* figure out which op it is *)
      let new_instr =
        match op with
        | Inc -> [ Loc source_position; IAdd (Const 1, Reg Rax) ]
        | Dec -> [ Loc source_position; ISub (Const 1, Reg Rax) ]
        (* smush the instructions *)
      in
      rec_instrs @ new_instr
  | ELet (x, v, b, source_position) ->
      (* figure out what value is -> rax *)
      (* note that if a var is declared inside v
                       then x will clobber it because si is unchanged *)
      let v_instrs : instr list = expr_to_instrs v env si in
      (* move value from rax to next available stack spot *)
      let store : instr list = [ IMov (Reg Rax, stackloc si) ] in
      (* generate instructions for body *)
      (* env must be updated so x can be found inside b.
                       si must be updated so that var inside v doesn't clobber x *)
      let b_instrs : instr list = expr_to_instrs b ((x, si) :: env) (si + 1) in
      (* smush all instructions together *)
      v_instrs @ store @ b_instrs
  | EIf (e1, e2, e3, source_position) ->
      (* generate instrs for recursive expressions *)
      let e1_instrs : instr list = expr_to_instrs e1 env si in
      let e2_instrs : instr list = expr_to_instrs e2 env si in
      let e3_instrs : instr list = expr_to_instrs e3 env si in
      (* will compare the result of e1 in rax with 0 *)
      let compare : instr list = [ ICmp (Const 0, Reg Rax) ] in
      (* make base strings for labels and jumps *)
      let else_s : string = new_label "else" in
      let after_s : string = new_label "after" in
      (* jumps *)
      let jmp : instr list = [ IJmp after_s ] in
      let je : instr list = [ IJe else_s ] in
      (* actual labels *)
      let else_l : instr list = [ ILab else_s ] in
      let after_l : instr list = [ ILab after_s ] in
      (* put it all together - order matters *)
      e1_instrs @ compare @ je @ e2_instrs @ jmp @ else_l @ e3_instrs @ after_l
  | EComp (comp, e1, e2, source_position) ->
      (* generate instrs for e1 and save to stack *)
      let loc : instr list = [ Loc source_position ] in
      let e1_instrs : instr list = expr_to_instrs e1 env si in
      let store_e1 : instr list = [ IMov (Reg Rax, stackloc si) ] in
      (* generate instructions for e2 ---> rax *)
      let e2_instrs : instr list = expr_to_instrs e2 env si in
      (* compare e1 and e2 *)
      let compare : instr list = [ ICmp (Reg Rax, stackloc si) ] in
      let t_instrs : instr list = [ IMov (Const 1, Reg Rax) ] in
      let f_instrs : instr list = [ IMov (Const 0, Reg Rax) ] in
      let after_s : string = new_label "after" in
      let after_l : instr list = [ ILab after_s ] in
      (* the jump is the opposite of the comparison *)
      let jmp : instr list =
        match comp with
        | Eq -> [ IJne after_s ]
        | Le -> [ IJge after_s ]
        | Gt -> [ IJle after_s ]
        (* will put 0 into rax immediately and if comparison is true,
                              won't jump and then will move into 1 rax *)
        (* this is simpler than if because only uses one jump and label *)
      in
      loc @ e1_instrs @ store_e1 @ e2_instrs @ compare @ f_instrs @ jmp
      @ t_instrs @ after_l

(* convert a list of instructions into one properly formatted string *)
let rec instrs_to_string (instrs : instr list) : string =
  match instrs with
  | [] -> ""
  | hd :: tl -> instr_to_string hd ^ "\n  " ^ instrs_to_string tl

(* the above can be done more cleanly with a fold like this: *)
(* List.fold_right (fun i r -> instr_to_string i ^ "\n  " ^ r) is "" *)

(* compiles a source program to an x86 string *)
let compile_with_position (program : string) (source_file_path : string) :
    string =
  (* source program converted to expressions *)
  let ast : expr = parse_with_position program in
  (* generate code for the AST *)
  (* si starts at 1 to not clobber stack pointer *)
  let instrs : instr list = expr_to_instrs ast [] 1 in
  (* make instrs into a giant string *)
  let instrs_str : string = instrs_to_string (instrs @ [ IRet ]) in
  (* add the boilerplate to instructions to make it work *)
  sprintf
    "\n\
    \  .file 1 \"%s.h\"\n\
    \  .text\n\
    \  .globl our_code_starts_here\n\
    \  our_code_starts_here:\n\
    \  %s\n\
    \  \n"
    source_file_path instrs_str

(* top-level-function -- code execution starts here *)
let () =
  (* opens the file passed in on the command line  *)
  let source_file_path = Sys.argv.(1) in
  let input_channel = open_in source_file_path in
  (* reads the file in *)
  let input_program : string = input_line input_channel in
  (* compiles the file to an X86 string *)
  let program : string = compile_with_position input_program source_file_path in
  (* prints the resulting x86 string *)
  printf "%s\n" program
