open Lwt.Syntax

open Hl_yaml.Make_Lwt (Lwt) (Lwt_io)

module Expect_test_config : Expect_test_config_types.S with module IO = Lwt = struct
  module IO = Lwt

  let sanitize s = s

  let run f = Lwt_main.run (f ())

  let upon_unreleasable_issue = `CR
end

let ok_or_failwith = function
| Ok x -> x
| Error s -> failwith s

let%expect_test "Config YAML processing" =
  let test ?options raw =
    let* parsed = YAML.of_string ?options raw in
    let* () =
      match parsed with
      | Ok x ->
        x |> YAML.to_string ~layout_style:`Block ~scalar_style:`Plain |> ok_or_failwith |> Lwt_io.printl
      | Error s -> Lwt_io.printlf "!ERROR! %s" s
    in
    Lwt_io.flush_all ()
  in

  let* () = test ~options:(make_options ~enable_imports:false ()) "!CONFIG simple.yml" in
  [%expect {| !ERROR! YAML !CONFIG imports are disabled |}];

  let* () = test "!CONFIG ../../../test/simple.yml" in
  [%expect {| abc: def |}];

  let* () =
    test ~options:(make_options ~config_path_relative_to:"../../../test" ()) "!CONFIG simple.yml"
  in
  [%expect {| abc: def |}];

  let* () =
    test
      ~options:(make_options ~file_path_relative_to:"../../../test" ())
      "data: 1\ndata: !FILE data.json"
  in
  [%expect {| data: "{\n  \"hello\": {\n    \"world\": [ 1, 2, null, \"foobar\" ]\n  }\n}" |}];

  let* () =
    test
      ~options:(make_options ~config_path_relative_to:"../../../test" ())
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
    let* raw =
      Lwt_io.with_file ~flags:[ O_RDONLY; O_NONBLOCK ] ~mode:Input "../../../test/advanced.yml"
        Lwt_io.read
    in
    test ~options:(make_options ~config_path_relative_to:"../../../test" ()) raw
  in
  [%expect
    {|
    hello: world
    abc: def
    some array:
    - name: Alice
    - name: Bob
    - name: Charlie
    - name: Diane
    - name: Eric |}];

  let* () =
    let validate_config_path path =
      let* () = Lwt_io.printlf "Validated: %S" path in
      Lwt.return_true
    in
    test
      ~options:(make_options ~config_path_relative_to:"../../../test" ~validate_config_path ())
      "!CONFIG advanced.yml"
  in
  [%expect
    {|
    Validated: "advanced.yml"
    Validated: "simple.yml"
    Validated: "imported.yml"
    hello: world
    abc: def
    some array:
    - name: Alice
    - name: Bob
    - name: Charlie
    - name: Diane
    - name: Eric |}];

  Lwt.return_unit
