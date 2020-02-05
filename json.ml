exception SyntaxError of string

type t =
  | JsonString of string
  | JsonObject of (string * t) list

let (>>) f g x = g (f x)

let explode str = List.init (String.length str) (String.get str)

let to_string chars = String.concat "" (List.map (String.make 1) chars)

let lst l = l |> List.length |> ((-) 1) |> List.nth l

let drop_spaces str =
  let flag = ref false in
  let check_char = function
    | ' ' -> if !flag then true else false
    | '"' -> flag := not !flag; true
    | _   -> true
  in
  (List.filter check_char str)

let parse = function
  | '{' :: [] -> raise (SyntaxError ("Unexpected end of input"))
  | '{' :: xs when lst xs = '}' -> JsonObject (["", JsonString ""])
  | x :: _    -> raise (SyntaxError ("Unexpected char: " ^ Char.escaped x))
  | []        -> raise (SyntaxError ("Unexpected end of input"))

let parse_json = explode >> drop_spaces >> parse

let rec json_to_string json =
  let make_pair (key, value) = "\t" ^ key ^ ", " ^ value ^ ";\n" in
  let json_to_beautify_string = fun prev (key, value) -> prev ^ make_pair (key, json_to_string value) in
  match json with
  | JsonObject xs -> "JsonObject [\n" ^ List.fold_left json_to_beautify_string "" xs ^ "\n]"
  | JsonString x -> "JsonString \"" ^ x ^ "\""

let example = JsonObject [
  "key", JsonString "somevalue";
  "key", JsonString "somevalue"
]

let read () =
  print_string "> ";
  print_newline();
  example
    |> json_to_string
    |> print_string;;

let main =
  read()
