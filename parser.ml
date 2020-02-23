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

let to_string chars = String.concat "" (List.map (String.make 1) chars)

let find_index_right l item =
  let index = ref None in
  for i = List.length l - 1 downto 0 do
    if List.nth l i = item && !index = None
    then index := Some i
  done;
  !index;;

let take l c =
  let output = ref [] in
  print_string (to_string l);
  print_int (List.length l);
  print_newline ();
  for i = 0 to c - 1 do
    output := !output @ [List.nth l i]
  done;
  !output;;

let take_while l p =
  let output = ref [] in
  let flag = ref false in
  for i = 0 to List.length l - 1 do
    if (not (p (List.nth l i))) && not !flag
    then output := !output @ [List.nth l i]
    else flag := true
  done;
  !output;;

let drop l c =
  let output = ref [] in
  for i = 0 to List.length l - 1 do
    if i >= c
    then output := !output @ [List.nth l i]
  done;
  !output;;

let drop_spaces str =
  let flag = ref false in
  let check_char = function
    | ' ' -> if !flag then true else false
    | '"' -> flag := not !flag; true
    | _   -> true
  in
  (List.filter check_char str)

let uncons l = (List.hd l, List.tl l)

let rec json_to_tokens tokens = function
  | []      -> tokens
  | xs ->
    let x = List.hd xs in
    let xs = ref (List.tl xs) in
    let drop_by length = xs := drop !xs length in
    let token = match x with
      | '{' -> LEFT_BRACKET
      | '}' -> RIGHT_BRACKET
      | '[' -> LEFT_BRACE
      | ']' -> RIGHT_BRACE
      | ',' -> COMMA
      | ':' -> COLON
      | '"' ->
        let closestCommaIndex = find_index_right tokens COMMA in
        let closestColonIndex = find_index_right tokens COLON in
        let str = take_while !xs ((=)'"') in
        drop_by (List.length str + 1);
        if closestColonIndex > closestCommaIndex
        then STRING (to_string str)
        else KEY (to_string str)
      | '0'..'9' as n ->
        let xs' = (n :: !xs) in
        let value = take_while xs' ((=)',') in
        let dot = List.find_opt ((=)'.') value in
        let result = match dot with
          | Some _ -> FLOAT (float_of_string (to_string value))
          | None   -> INTEGER (int_of_string (to_string value))
        in
        drop_by (List.length value);
        result
      | _ as c -> (* rewrite this govno code *)
        let xs' = (c :: !xs) in
        let literal4 = match to_string (take xs' 4) with
          | "null" -> Some NULL
          | "true" -> Some TRUE
          | _      -> None
        in
        let literal5 = match to_string (take xs' 5) with
          | "false" -> Some FALSE
          | _       -> None
        in
        match (literal4, literal5) with
          | (Some x, _) -> drop_by 3; x
          | (_, Some x) -> drop_by 4; x
          | (_, _)      -> failwith "something went wrong"
    in
    json_to_tokens (tokens @ [token]) !xs;;

let parse_json str = str
  |> explode
  |> drop_spaces
  |> json_to_tokens []

let print_tokens l = List.map token_to_string l
  |> List.map (fun x -> x ^ ", ")
  |> List.fold_left (^) ""
  |> print_string

let example = "{ \"key1234\": 11, \"key\": null }"

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
