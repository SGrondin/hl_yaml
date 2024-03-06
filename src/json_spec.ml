module StringMap = struct
  module StringMap = Map.Make (struct
    type t = string

    let compare = String.compare
  end)

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
end

type object_entry = {
  key: string;
  required: bool;
  json_spec: t;
}

and schema_keys = object_entry StringMap.t

and enum = {
  expected: Yojson.Basic.t list;
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

let rec of_yojson : Yojson.Safe.t -> t = function
| `Null -> JNull
| `Bool _ -> JBool
| `Int _ -> JInt
| `Float _ -> JFloat
| `String _ -> JString
| `List [] -> JArray JAny
| `List (nested :: _) -> JArray (of_yojson nested)
| `Assoc [] -> JObject JAny
| `Assoc ((_, nested) :: _) -> JObject (of_yojson nested)
| (`Intlit _ | `Tuple _ | `Variant _) as json -> of_yojson (Yojson.Safe.to_basic json :> Yojson.Safe.t)

let format fmt json =
  let rec loop nested fmt json =
    match json, nested with
    | JAny, false -> Format.fprintf fmt "Any"
    | JAny, true -> ()
    | JNull, false -> Format.fprintf fmt "Null"
    | JNull, true -> Format.fprintf fmt " of Nulls"
    | JBool, false -> Format.fprintf fmt "Boolean"
    | JBool, true -> Format.fprintf fmt " of Booleans"
    | JInt, false -> Format.fprintf fmt "Integer"
    | JInt, true -> Format.fprintf fmt " of Integers"
    | (JFloat | JNumeric), false -> Format.fprintf fmt "Double"
    | (JFloat | JNumeric), true -> Format.fprintf fmt " of Doubles"
    | (JString | JAtom), false -> Format.fprintf fmt "String"
    | (JString | JAtom), true -> Format.fprintf fmt " of Strings"
    | JEnum { type_description; _ }, false -> Format.fprintf fmt "%s" type_description
    | JEnum { type_description; _ }, true -> Format.fprintf fmt " of %s" type_description
    | JArray ty, false -> Format.fprintf fmt "Array%a" (loop true) ty
    | JArray ty, true -> Format.fprintf fmt " of Arrays%a" (loop true) ty
    | JOneOrArray ty, false -> Format.fprintf fmt "%a or Array%a" (loop false) ty (loop true) ty
    | JOneOrArray ty, true -> Format.fprintf fmt " of %a or Arrays%a" (loop false) ty (loop true) ty
    | JObject ty, false -> Format.fprintf fmt "Object%a" (loop true) ty
    | JObject ty, true -> Format.fprintf fmt " of Objects%a" (loop true) ty
    | JSchema { name; _ }, false -> Format.fprintf fmt "%s Object" name
    | JSchema { name; _ }, true -> Format.fprintf fmt " of %s Objects" name
  in
  loop false fmt json

let to_string json = Format.asprintf "%a" format json

let fmt_list fmt ll ~f ~sep =
  List.iteri
    (fun i x ->
      if i > 0 then Format.fprintf fmt "%s" sep;
      f fmt x)
    ll

let make_enum expected =
  let json_types =
    expected
    |> List.map (fun (x : Yojson.Basic.t) -> of_yojson (x :> Yojson.Safe.t))
    |> List.sort_uniq compare
  in
  let type_description = Format.asprintf "(%a)" (fmt_list ~f:format ~sep:" | ") json_types in
  JEnum { expected; json_types; type_description }

let make_schema ~name ~reject_extras ll =
  let keys = List.fold_left (fun acc data -> StringMap.add data.key data acc) StringMap.empty ll in
  JSchema { name; reject_extras; keys }

type step =
  | Dot of string
  | Index of int

type error =
  | Extraneous of {
      path: step list;
      name: string option;
    }
  | Missing of {
      path: step list;
      name: string option;
      missing: t;
    }
  | Type of {
      path: step list;
      name: string option;
      expected: t;
      found: Yojson.Safe.t;
    }
  | Incorrect_value of {
      path: step list;
      name: string option;
      expected: Yojson.Basic.t list;
      found: Yojson.Basic.t;
    }
  | Deserialization of {
      message: string;
      attempted: (Yojson.Safe.t, string) Either.t;
    }

let render_error ?emphasis error =
  let emphasis fmt s =
    let f = Option.value emphasis ~default:Fun.id in
    Format.fprintf fmt "%s" (f s)
  in
  let fmt_name fmt = function
    | None -> ()
    | Some s -> Format.fprintf fmt " (in %a)" emphasis s
  in
  let fmt_path fmt path =
    let fmt_step fmt = function
      | Dot s
        when String.exists
               (function
                 | 'a' .. 'z'
                  |'A' .. 'Z'
                  |'0' .. '9'
                  |'_' ->
                   false
                 | _ -> true)
               s ->
        Format.fprintf fmt "[%S]" s
      | Dot s -> Format.fprintf fmt ".%s" s
      | Index i -> Format.fprintf fmt "[%d]" i
    in
    let render fmt path =
      match List.rev path with
      | Dot s :: ll ->
        Format.fprintf fmt "%s" s;
        List.iter (fmt_step fmt) ll
      | ll -> List.iter (fmt_step fmt) ll
    in
    Format.fprintf fmt "%a" emphasis (Format.asprintf "%a" render path)
  in
  let key_or_option fmt = function
    | []
     |[ _ ] ->
      Format.fprintf fmt "option"
    | _ -> Format.fprintf fmt "key"
  in
  match error with
  | Extraneous { path; name } ->
    Format.asprintf "Extraneous %a: %a%a" key_or_option path fmt_path path fmt_name name
  | Missing { path; name; missing } ->
    Format.asprintf "Missing %a: %a (%a)%a" key_or_option path fmt_path path format missing fmt_name name
  | Type { path; name; expected; found } ->
    Format.asprintf "%a expected a %a, but found: %a%a" fmt_path path format expected format
      (of_yojson found) fmt_name name
  | Incorrect_value { path; name; expected; found } ->
    let render_json fmt j = Format.fprintf fmt "%s" (Yojson.Basic.to_string j) in
    Format.asprintf "%a incorrect value %a, expected one of: %a%a" fmt_path path render_json found
      (fmt_list ~f:render_json ~sep:" | ")
      expected fmt_name name
  | Deserialization { message; attempted = _ } ->
    Format.asprintf "Invalid configuration at %a" emphasis message

let get_name ll =
  List.find_map
    (function
      | "name", `String s -> Some s
      | "name", `Null -> None
      | "name", json -> Some (Yojson.Safe.to_string json)
      | _ -> None)
    ll

let rec validate ~path (json : Yojson.Safe.t) json_spec ~nullable =
  match json_spec, json with
  | JNull, `Null -> []
  | _, `Null when nullable -> []
  | JAny, _ -> []
  | JAtom, (`String _ | `Int _ | `Float _ | `Bool _) -> []
  | JBool, `Bool _ -> []
  | JInt, `Int _ -> []
  | JFloat, `Float _ -> []
  | JNumeric, (`Int _ | `Float _) -> []
  | JString, `String _ -> []
  | JEnum { expected; _ }, found when List.mem (Yojson.Safe.to_basic found) expected -> []
  | JEnum { expected; json_types; _ }, found when List.mem (of_yojson found) json_types ->
    [ Incorrect_value { path; name = None; expected; found = Yojson.Safe.to_basic found } ]
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
      | `Left _ when reject_extras -> Extraneous { path = Dot key :: path; name = get_name ll } :: acc
      | `Right { json_spec; required = true; _ } ->
        Missing { path = Dot key :: path; name = get_name ll; missing = json_spec } :: acc
      | `Left _
       |`Right _ ->
        acc
      | `Both (json, { json_spec; required; _ }) ->
        validate ~path:(Dot key :: path) json json_spec ~nullable:(not required) @ acc )
    |> List.rev
  | _, (`Intlit _ | `Tuple _ | `Variant _) ->
    validate ~path (Yojson.Safe.to_basic json :> Yojson.Safe.t) json_spec ~nullable
  | _, `Assoc ll -> [ Type { path; name = get_name ll; expected = json_spec; found = json } ]
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
    [ Type { path; name = None; expected = json_spec; found = json } ]

let validate ?(path = [ Dot "$" ]) ?(nullable = false) json_spec (json : Yojson.Safe.t) =
  validate ~path (json : Yojson.Safe.t) json_spec ~nullable
