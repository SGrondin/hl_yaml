open! Core
open Lwt.Syntax
open Utils.Lwt_expect_tests
module Y = Hl_yaml.Make_Lwt (Lwt) (Lwt_io)

let%expect_test "Config YAML processing - Lwt" =
  let test ?options raw =
    let* parsed = Y.YAML.of_string ?options raw in
    let* () =
      match parsed with
      | Ok x -> x |> Y.YAML.to_string |> Utils.ok_or_exn |> Lwt_io.printl
      | Error ll ->
        Lwt_io.printlf !"Error: %{Yojson.Safe.pretty_to_string}" ([%to_yojson: Y.Spec.error list] ll)
    in
    Lwt_io.flush_all ()
  in

  let get_env_var = function
    | "env1" -> Some "got it"
    | _ -> None
  in

  let* () =
    test
      ~options:(Y.make_options ~get_env_var ~config_path_filter_map:Utils.map_path_lwt ())
      "!CONFIG advanced.yml"
  in
  [%expect
    {|
    exists: got it
    missing:
    abc: def
    some array:
    - name: Alice
    - name: Bob
    - name: Charlie
    - name: Diane
    - name: Eric |}];

  let* () =
    let* x =
      Y.parse
        ~options:(Y.make_options ~get_env_var ~config_path_filter_map:Utils.map_path_lwt ())
        ~of_yojson:Utils.foo_of_yojson "!CONFIG advanced.yml"
    in
    let* () = Utils.render x |> Lwt_io.printl in
    Lwt_io.flush_all ()
  in
  [%expect
    {|
    ((exists ("got it")) (missing ()) (abc DEF)
     (names
      (((name Alice)) ((name Bob)) ((name Charlie)) ((name Diane)) ((name Eric))))) |}];

  Lwt.return_unit
