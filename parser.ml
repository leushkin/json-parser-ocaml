exception SyntaxError of string

type t =
  | JsonString of string
  | JsonObject of (string * t) list

let (>>) f g x = g (f x)

let explode str = List.init (String.length str) (String.get str)

let to_string chars = String.concat "" (List.map (String.make 1) chars)

let lst l = l |> List.length |> ((-) 1) |> List.nth l

let drop l i =
  let output = ref [] in
  for index = 0 to List.length l - 1 do
    if (not (index = i)) then output := !output @ [List.nth l index]
  done;
  !output

let drop_last l = drop l (List.length l - 1)

let drop_spaces str =
  let flag = ref false in
  let check_char = function
    | ' ' -> if !flag then true else false
    | '"' -> flag := not !flag; true
    | _   -> true
  in
  (List.filter check_char str)

let parse_exn = function
  | '{' :: [] -> raise (SyntaxError ("Unexpected end of input"))
  | '{' :: xs when lst xs = '}' -> ""
  | x :: _    -> raise (SyntaxError ("Unexpected char: " ^ Char.escaped x))
  | []        -> raise (SyntaxError ("Unexpected end of input"))

let parse_json = explode >> drop_spaces >> parse_exn

let rec json_to_string json ~l =
  let make_pair (key, value) = key ^ ", " ^ value ^ ";" in
  let json_to_beautify_string l prev (key, value) =
    let tabs = List.init l (fun _ -> '\t') |> to_string in
    tabs ^ prev ^ make_pair (key, json_to_string value ~l) ^ "\n"
  in
  match json with
  | JsonObject xs -> "JsonObject [\n" ^ List.fold_left (json_to_beautify_string l) "" xs ^ "\n]"
  | JsonString x -> "JsonString \"" ^ x ^ "\""

let example = JsonObject [
  "key", JsonString "somevalue";
  "key", JsonString "somevalue"
]

let read () =
  print_string "> ";
  print_newline();
  example
    |> json_to_string ~l:1
    |> print_string;;

let main =
  read()
