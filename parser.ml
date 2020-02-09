exception SyntaxError of string

type stream = { mutable chr : char list; chan : in_channel }

type t =
  | JsonString of string
  | JsonObject of (string * t) list

let (>>) f g x = g (f x)

let explode str = List.init (String.length str) (String.get str)

let to_string chars = String.concat "" (List.map (String.make 1) chars)

let lst l =
  let index = (List.length l) - 1 in
  List.nth l index;;

let find_index list item =
  let result = ref 0 in
  for i = List.length list downto 0 do
    if (List.nth list i = item)
    then result := i
  done;
  !result

let split l ~c =
  let output = ref [] in
  let current = ref [] in
  let helper = function
    | x when x = c -> output := !output @ [!current]
    | _ as x       -> current := !current @ [x]
  in
  List.iter helper l;
  !output;;

let split_to_tuple2 l ~c =
  let left = ref [] in
  let right = ref [] in
  let index = find_index l c in
  for i = 0 to (List.length l) - 1 do
    if i < index
    then left := !left @ [List.nth l i]
    else right := !right @ [List.nth l i]
  done;
  (!left, !right)

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

let rec parse_exn = function
  | '{' :: [] -> raise (SyntaxError ("Unexpected end of input"))
  | '{' :: xs when lst xs = '}' ->
    let pairs = (drop_last xs |> split ~c:',') in
    let key_values = pairs
      |> List.map (split_to_tuple2 ~c:':')
      |> List.map (fun (key, value) -> (to_string key, parse_exn value))
    in
    JsonObject key_values
  | '"' :: xs when lst xs = '"' -> JsonString (xs |> drop_last |> to_string)
  | x :: _ -> raise (SyntaxError ("Unexpected char: " ^ Char.escaped x))
  | []     -> raise (SyntaxError ("Unexpected end of input"))

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

let example = "{ \"key\": \"value\" }"

let read () =
  print_string "> ";
  flush stdout;
  (* input_line stdin *)
  example
    |> parse_json
    |> json_to_string ~l:1
    |> print_string;
  print_newline();;
  (* read ();; *)

let main =
  read()
