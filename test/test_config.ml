open! Core
open Lwt.Syntax
open Utils.Lwt_expect_tests
module Y = Hl_yaml.Make_Lwt (Lwt) (Lwt_io)

let%expect_test "Config YAML processing" =
  let test ?options raw =
    let* parsed = Y.YAML.of_string ?options raw in
    let* () =
      match parsed with
      | Ok x ->
        x
        |> Y.YAML.to_string ~layout_style:`Block ~scalar_style:`Plain
        |> Utils.ok_or_exn
        |> Lwt_io.printl
      | Error ll ->
        Lwt_io.printlf !"Error: %{Yojson.Safe.pretty_to_string}" ([%to_yojson: Y.Spec.error list] ll)
    in
    Lwt_io.flush_all ()
  in

  let* () = test ~options:(Y.make_options ~enable_imports:false ()) "!CONFIG simple.yml" in
  [%expect {| Error: [ { "type": "processing", "message": "YAML !CONFIG imports are disabled" } ] |}];

  let* () = test "!CONFIG ../../../test/files/simple.yml" in
  [%expect {| abc: def |}];

  let* () =
    test
      ~options:
        (Y.make_options
           ~config_path_filter_map:(fun s -> Lwt.return (Filename.concat "../../../test" s))
           () )
      "!CONFIG files/simple.yml"
  in
  [%expect {| abc: def |}];

  let* () =
    test
      ~options:(Y.make_options ~file_path_filter_map:Utils.map_path_lwt ())
      "data: 1\ndata: !FILE data.json"
  in
  [%expect {| data: "{\n  \"hello\": {\n    \"world\": [ 1, 2, null, \"foobar\" ]\n  }\n}" |}];

  let* () =
    test
      ~options:(Y.make_options ~config_path_filter_map:Utils.map_path_lwt ())
      "data: 1\ndata: !CONFIG data.json"
  in
  [%expect {|
    data:
      hello:
        world:
        - 1
        - 2
        -
        - foobar |}];

  let* () =
    let get_env_var = function
      | "env1" -> Some "got it"
      | _ -> None
    in
    let* raw =
      Lwt_io.with_file ~flags:[ O_RDONLY; O_NONBLOCK ] ~mode:Input "../../../test/files/advanced.yml"
        Lwt_io.read
    in
    test ~options:(Y.make_options ~get_env_var ~config_path_filter_map:Utils.map_path_lwt ()) raw
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
    let validate_config_path = function
      | "imported.yml" as s -> raise (Y.HL_YAML_error ("Illegal file! " ^ s))
      | path ->
        let* () = Lwt_io.printlf "Validated: %S" path in
        Utils.map_path_lwt path
    in
    test ~options:(Y.make_options ~config_path_filter_map:validate_config_path ()) "!CONFIG advanced.yml"
  in
  [%expect
    {|
    Validated: "advanced.yml"
    Validated: "simple.yml"
    Error: [ { "type": "processing", "message": "Illegal file! imported.yml" } ] |}];

  Lwt.return_unit
