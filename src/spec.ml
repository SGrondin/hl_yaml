module StringMap = struct
  module StringMap = Map.Make (String)
  include StringMap

  let fold2 m1 m2 ~init ~f =
    let acc = ref init in
    let _map =
      StringMap.merge
        (fun key x y ->
          let data =
            match x, y with
            | Some x, None -> `Left x
            | None, Some y -> `Right y
            | Some x, Some y -> `Both (x, y)
            | None, None -> assert false
          in
          acc := f ~key ~data !acc;
          None)
        m1 m2
    in
    !acc

  let to_yojson f map : Yojson.Safe.t =
    `Assoc (StringMap.fold (fun key data acc -> (key, f data) :: acc) map [])
end

module OneOrList = struct
  type 'a t = 'a list [@@deriving to_yojson]

  let of_yojson (type el) (el_of_yojson : Yojson.Safe.t -> (el, string) result) = function
  | `Null -> Ok []
  | `List _ as json -> [%of_yojson: el list] json
  | json -> [%of_yojson: el] json |> Result.map (fun x -> [ x ])
end

module OneOrArray = struct
  type 'a t = 'a array [@@deriving to_yojson]

  let of_yojson (type el) (el_of_yojson : Yojson.Safe.t -> (el, string) result) = function
  | `Null -> Ok [||]
  | `List _ as json -> [%of_yojson: el array] json
  | json -> [%of_yojson: el] json |> Result.map (fun x -> [| x |])
end

type object_entry = {
  key: string;
  required: bool;
  spec: t;
}

and schema_keys = object_entry StringMap.t

and enum = {
  expected: (Yojson.Basic.t[@to_yojson (fun (x : Yojson.Basic.t) -> (x :> Yojson.Safe.t))]) list;
  json_types: t list;
  type_description: string;
}

and t =
  | JAny
  | JAtom (* Atoms are supposed to be strings, but other simple values are ok *)
  | JNull
  | JBool
  | JInt
  | JFloat
  | JNumeric (* Accepts Int and Float *)
  | JString
  | JEnum of enum
  | JArray of t
  | JOneOrArray of t
  | JObject of t
  | JSchema of {
      name: string;
      reject_extras: bool;
      keys: schema_keys;
    }
[@@deriving to_yojson]

let rec infer : Yojson.Safe.t -> t = function
| `Null -> JNull
| `Bool _ -> JBool
| `Int _ -> JInt
| `Float _ -> JFloat
| `String _ -> JString
| `List [] -> JArray JAny
| `List (nested :: _) -> JArray (infer nested)
| `Assoc [] -> JObject JAny
| `Assoc ((_, nested) :: _) -> JObject (infer nested)
| (`Intlit _ | `Tuple _ | `Variant _) as json -> infer (Yojson.Safe.to_basic json :> Yojson.Safe.t)

let infer_non_rec : Yojson.Safe.t -> t = function
| `Null -> JNull
| `Bool _ -> JBool
| `Int _ -> JInt
| `Float _ -> JFloat
| `String _ -> JString
| `List [] -> JArray JAny
| `List _ -> JArray JAny
| `Assoc [] -> JObject JAny
| `Assoc _ -> JObject JAny
| (`Intlit _ | `Tuple _ | `Variant _) as json -> infer (Yojson.Safe.to_basic json :> Yojson.Safe.t)

let format ppf json =
  let rec loop nested ppf json =
    match json, nested with
    | JAny, false -> Format.fprintf ppf "Any"
    | JAny, true -> ()
    | JNull, false -> Format.fprintf ppf "Null"
    | JNull, true -> Format.fprintf ppf " of Nulls"
    | JBool, false -> Format.fprintf ppf "Boolean"
    | JBool, true -> Format.fprintf ppf " of Booleans"
    | JInt, false -> Format.fprintf ppf "Integer"
    | JInt, true -> Format.fprintf ppf " of Integers"
    | (JFloat | JNumeric), false -> Format.fprintf ppf "Double"
    | (JFloat | JNumeric), true -> Format.fprintf ppf " of Doubles"
    | (JString | JAtom), false -> Format.fprintf ppf "String"
    | (JString | JAtom), true -> Format.fprintf ppf " of Strings"
    | JEnum { type_description; _ }, false -> Format.fprintf ppf "%s" type_description
    | JEnum { type_description; _ }, true -> Format.fprintf ppf " of %s" type_description
    | JArray ty, false -> Format.fprintf ppf "Array%a" (loop true) ty
    | JArray ty, true -> Format.fprintf ppf " of Arrays%a" (loop true) ty
    | JOneOrArray ty, false -> Format.fprintf ppf "%a or Array%a" (loop false) ty (loop true) ty
    | JOneOrArray ty, true -> Format.fprintf ppf " of %a or Arrays%a" (loop false) ty (loop true) ty
    | JObject ty, false -> Format.fprintf ppf "Object%a" (loop true) ty
    | JObject ty, true -> Format.fprintf ppf " of Objects%a" (loop true) ty
    | JSchema { name; _ }, false -> Format.fprintf ppf "%s Object" name
    | JSchema { name; _ }, true -> Format.fprintf ppf " of %s Objects" name
  in
  loop false ppf json

let to_string json = Format.asprintf "%a" format json

let pp_list ppf ll ~f ~sep =
  List.iteri
    (fun i x ->
      if i > 0 then Format.fprintf ppf "%s" sep;
      f ppf x)
    ll

let make_enum expected =
  let json_types =
    expected
    |> List.map (fun (x : Yojson.Basic.t) -> infer (x :> Yojson.Safe.t))
    |> List.sort_uniq compare
  in
  let type_description = Format.asprintf "(%a)" (pp_list ~f:format ~sep:" | ") json_types in
  JEnum { expected; json_types; type_description }

let make_schema ~name ~reject_extras ll =
  let keys = List.fold_left (fun acc data -> StringMap.add data.key data acc) StringMap.empty ll in
  JSchema { name; reject_extras; keys }

type step =
  | Dot of string
  | Index of int

let is_brackets s =
  String.exists
    (function
      | 'a' .. 'z'
       |'A' .. 'Z'
       |'0' .. '9'
       |'_' ->
        false
      | _ -> true)
    s

let step_to_yojson : step -> Yojson.Safe.t = function
| Dot s when is_brackets s -> `Assoc [ "type", `String "key"; "key", `String s ]
| Dot s -> `Assoc [ "type", `String "dot"; "dot", `String s ]
| Index i -> `Assoc [ "type", `String "index"; "index", `Int i ]

(* path is always a reversed list *)
type path = step list

let path_to_yojson path : Yojson.Safe.t =
  `List (List.fold_left (fun acc step -> [%to_yojson: step] step :: acc) [] path)

let path_to_string path =
  let pp_step ppf = function
    | Dot s when is_brackets s -> Format.fprintf ppf "[%S]" s
    | Dot s -> Format.fprintf ppf ".%s" s
    | Index i -> Format.fprintf ppf "[%d]" i
  in
  let render ppf path =
    match List.rev path with
    | Dot s :: ll ->
      Format.fprintf ppf "%s" s;
      List.iter (pp_step ppf) ll
    | ll -> List.iter (pp_step ppf) ll
  in
  Format.asprintf "%a" render path

type error =
  | Extraneous of {
      path: path;
      name: string option;
      extra: Yojson.Safe.t;
    }
  | Missing of {
      path: path;
      name: string option;
      missing: t;
    }
  | Type of {
      path: path;
      name: string option;
      expected: t;
      found: Yojson.Safe.t;
    }
  | Incorrect_value of {
      path: path;
      name: string option;
      expected: Yojson.Basic.t list;
      found: Yojson.Safe.t;
    }
  | Deserialization of {
      message: string;
      attempted: [ `JSON of Yojson.Safe.t | `String of string ];
    }
  | Processing of { message: string }

let error_to_yojson : error -> Yojson.Safe.t = function
| Extraneous { path; name; extra } ->
  `Assoc
    [
      "type", `String "extraneous";
      "name", [%to_yojson: string option] name;
      "path", `String (path_to_string path);
      "path_parts", [%to_yojson: path] path;
      "extra", extra;
    ]
| Missing { path; name; missing } ->
  `Assoc
    [
      "type", `String "missing";
      "name", [%to_yojson: string option] name;
      "path", `String (path_to_string path);
      "path_parts", [%to_yojson: path] path;
      "missing", [%to_yojson: t] missing;
    ]
| Type { path; name; expected; found } ->
  `Assoc
    [
      "type", `String "type";
      "name", [%to_yojson: string option] name;
      "path", `String (path_to_string path);
      "path_parts", [%to_yojson: path] path;
      "expected", [%to_yojson: t] expected;
      "found", found;
    ]
| Incorrect_value { path; name; expected; found } ->
  `Assoc
    [
      "type", `String "missing";
      "name", [%to_yojson: string option] name;
      "path", `String (path_to_string path);
      "path_parts", [%to_yojson: path] path;
      "expected", `List (expected :> Yojson.Safe.t list);
      "found", (found :> Yojson.Safe.t);
    ]
| Deserialization { message; attempted } ->
  `Assoc
    [
      "type", `String "deserialization";
      "message", `String message;
      ( "attempted",
        match attempted with
        | `JSON json -> json
        | `String s -> `String s );
    ]
| Processing { message } -> `Assoc [ "type", `String "processing"; "message", `String message ]

let error_to_string ?emphasis error =
  let emphasis = Option.value emphasis ~default:Fun.id in
  let emphasis ppf s = Format.fprintf ppf "%s" (emphasis s) in
  let pp_name ppf = function
    | None -> ()
    | Some s -> Format.fprintf ppf " (in %a)" emphasis s
  in
  let pp_path ppf path = Format.fprintf ppf "%a" emphasis (path_to_string path) in
  let key_or_option ppf = function
    | []
     |[ _ ] ->
      Format.fprintf ppf "option"
    | _ -> Format.fprintf ppf "key"
  in
  let pp_json ppf j = Format.fprintf ppf "%s" (Yojson.Safe.to_string j) in
  match error with
  | Extraneous { path; name; extra } ->
    Format.asprintf "Extraneous %a: %a (%a)%a" key_or_option path pp_path path format
      (infer_non_rec extra) pp_name name
  | Missing { path; name; missing } ->
    Format.asprintf "Missing %a: %a (%a)%a" key_or_option path pp_path path format missing pp_name name
  | Type { path; name; expected; found } ->
    Format.asprintf "Type mismatch at %a, expected %a, but found: %a%a" pp_path path format expected
      format (infer found) pp_name name
  | Incorrect_value { path; name; expected; found } ->
    Format.asprintf "Incorrect value at %a, found %a, but expected one of: %a%a" pp_path path pp_json
      found (pp_list ~f:pp_json ~sep:" | ")
      (expected :> Yojson.Safe.t list)
      pp_name name
  | Deserialization { message; attempted = _ } -> Format.asprintf "Unexpected format: %a" emphasis message
  | Processing { message } -> Format.asprintf "%a" emphasis message

let get_name ll =
  List.find_map
    (function
      | "name", `String s -> Some s
      | "name", `Null -> None
      | "name", json -> Some (Yojson.Safe.to_string json)
      | _ -> None)
    ll

let rec validate ~path (json : Yojson.Safe.t) spec ~nullable =
  let basic = lazy (Yojson.Safe.to_basic json) in
  match spec, json with
  | JNull, `Null -> []
  | _, `Null when nullable -> []
  | JAny, _ -> []
  | JAtom, (`String _ | `Int _ | `Float _ | `Bool _) -> []
  | JBool, `Bool _ -> []
  | JInt, `Int _ -> []
  | JFloat, `Float _ -> []
  | JNumeric, (`Int _ | `Float _) -> []
  | JString, `String _ -> []
  | JEnum { expected; _ }, _ when List.mem (Lazy.force basic) expected -> []
  | JEnum { expected; json_types; _ }, _
  (* Already forced from the previous clause so let's use it in case it saves a call to to_basic in infer *)
    when List.mem (infer (Lazy.force basic :> Yojson.Safe.t)) json_types ->
    [ Incorrect_value { path; name = None; expected; found = json } ]
  | (JArray JAny | JOneOrArray JAny), `List _ -> []
  | (JArray nested_type | JOneOrArray nested_type), `List ll ->
    List.mapi
      (fun i nested_json -> validate ~path:(Index i :: path) nested_json nested_type ~nullable:false)
      ll
    |> List.concat
  | JOneOrArray nested_type, one -> validate ~path one nested_type ~nullable:false
  | JObject JAny, `Assoc _ -> []
  | JObject nested, `Assoc ll ->
    List.concat_map (fun (key, json) -> validate ~path:(Dot key :: path) json nested ~nullable:false) ll
  | JSchema { keys; reject_extras; _ }, `Assoc ll ->
    let passed = List.fold_left (fun acc (key, json) -> StringMap.add key json acc) StringMap.empty ll in
    StringMap.fold2 passed keys ~init:[] ~f:(fun ~key ~data acc ->
      match data with
      | `Left extra when reject_extras ->
        Extraneous { path = Dot key :: path; name = get_name ll; extra } :: acc
      | `Right { spec; required = true; _ } ->
        Missing { path = Dot key :: path; name = get_name ll; missing = spec } :: acc
      | `Left _
       |`Right _ ->
        acc
      | `Both (json, { spec; required; _ }) ->
        validate ~path:(Dot key :: path) json spec ~nullable:(not required) @ acc )
  | _, (`Intlit _ | `Tuple _ | `Variant _) ->
    validate ~path (Yojson.Safe.to_basic json :> Yojson.Safe.t) spec ~nullable
  | _, `Assoc ll -> [ Type { path; name = get_name ll; expected = spec; found = json } ]
  | JAtom, _
   |JNull, _
   |JBool, _
   |JInt, _
   |JFloat, _
   |JNumeric, _
   |JString, _
   |JEnum _, _
   |JArray _, _
   |JObject _, _
   |JSchema _, _ ->
    [ Type { path; name = None; expected = spec; found = json } ]

let validate ?(path = [ Dot "$" ]) ?(nullable = false) spec (json : Yojson.Safe.t) =
  validate ~path (json : Yojson.Safe.t) spec ~nullable
