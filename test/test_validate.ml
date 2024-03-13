open! Core
module Y = Hl_yaml.Unix

type role =
  | Manager
  | Developer
  | Devops
[@@deriving sexp_of]

let role_of_yojson = function
| `String "manager" -> Ok Manager
| `String "developer" -> Ok Developer
| `String "devops" -> Ok Devops
| json -> Error ("Invalid JSON for 'role': " ^ Yojson.Safe.to_string json)

type person = {
  first_name: string option; [@key "firstName"] [@default None]
  last_name: string; [@key "lastName"]
  age: int option; [@default None]
  roles: role list; [@of_yojson Y.Spec.OneOrMany.of_yojson role_of_yojson]
}
[@@deriving sexp_of, of_yojson]

type t = person list [@@deriving sexp_of, of_yojson]

let spec =
  let open Y.Spec in
  let role = make_enum [ `String "manager"; `String "developer"; `String "devops" ] in
  let person =
    make_schema ~name:"person" ~reject_extras:true
      [
        { key = "firstName"; required = false; spec = JString };
        { key = "lastName"; required = true; spec = JString };
        { key = "age"; required = false; spec = JInt };
        { key = "roles"; required = true; spec = JOneOrArray role };
      ]
  in
  JArray person

let%expect_test "README Full Validation example" =
  let test ?validate filename =
    let options = Y.make_options ~config_path_filter_map:(Filename.concat "../../../test/files") () in
    match Y.parse ?validate ~of_yojson ~options ("!CONFIG " ^ filename) with
    | Ok people -> print_endline (sprintf !"%{sexp#hum: t}" people)
    | Error errors ->
      let message = List.map ~f:Y.Spec.error_to_string errors |> String.concat ~sep:"\n" in
      print_endline message
  in

  test "people.yml";
  [%expect {| Unexpected format: Invalid JSON for 'role': "Developer" |}];

  test ~validate:spec "people.yml";
  [%expect
    {|
    Incorrect value at $[0].roles, found "Developer", but expected one of: "manager" | "developer" | "devops"
    Missing key: $[1].lastName (String)
    Extraneous key: $[1].dateOfBirth (String) |}];

  test "people_fixed.yml";
  [%expect
    {|
      (((first_name (Bobby)) (last_name Tables) (age (52)) (roles (Developer)))
       ((first_name (John)) (last_name Doe) (age ()) (roles (Manager Devops)))) |}]
