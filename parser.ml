type token =
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | NULL
  | TRUE
  | FALSE
  | STRING of string
  | INTEGER of int
  | FLOAT of float
  | KEY of string
  | COMMA
  | COLON
  | EOF

let token_to_string = function
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | LEFT_BRACKET -> "LEFT_BRACKET"
  | RIGHT_BRACKET -> "RIGHT_BRACKET"
  | NULL -> "NULL"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | STRING str -> "STRING (" ^ str ^ ")"
  | INTEGER i -> "INTEGER (" ^ string_of_int i ^ ")"
  | FLOAT f -> "FLOAT (" ^ string_of_float f ^ ")"
  | KEY key -> "KEY (" ^ key ^ ")"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | EOF -> "EOF"

type t =
  | JsonString of string
  | JsonObject of (string * t) list

let explode str = List.init (String.length str) (String.get str)

let drop_spaces str =
  let flag = ref false in
  let check_char = function
    | ' ' -> if !flag then true else false
    | '"' -> flag := not !flag; true
    | _   -> true
  in
  (List.filter check_char str)

let uncons l = (List.hd l, List.tl l)

let rec json_to_tokens = function
  | []      -> []
  | x :: xs ->
    let token = match x with
      | '{' -> LEFT_BRACKET
      | '}' -> RIGHT_BRACKET
      | '[' -> LEFT_BRACE
      | ']' -> RIGHT_BRACE
      | ',' -> COMMA
      | ':' -> COLON
      | _ -> NULL
    in
    token :: json_to_tokens xs

  (* | '[' -> LEFT_BRACE
  | ']' -> RIGHT_BRACE
  | ',' -> COMMA
  | ':' -> COLON *)

(* 
  | NULL
  | TRUE
  | FALSE
  | STRING of string
  | INTEGER of int
  | FLOAT of float
  | KEY of string
  | COLON
  | EOF *)

let parse_json str = str
  |> explode
  |> drop_spaces
  |> json_to_tokens

let print_tokens l = List.map token_to_string l
  |> List.map (fun x -> x ^ ", ")
  |> List.fold_left (^) ""
  |> print_string

let example = "{ \"key\": \"value\" }"

let read () =
  print_string "> ";
  flush stdout;
  (* input_line stdin *)
  example
    |> parse_json
    |> print_tokens;
  print_newline();;
  (* read ();; *)

let main =
  read()
