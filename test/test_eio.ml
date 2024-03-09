open! Core
module Y = Hl_yaml.Make_Eio (Eio)

let%expect_test "Config YAML processing - Eio" =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.fs env in
  let test ?options raw =
    let parsed = Y.YAML.of_string ~cwd ?options raw in
    let () =
      match parsed with
      | Ok x ->
        x
        |> Y.YAML.to_string ~layout_style:`Block ~scalar_style:`Plain
        |> Utils.ok_or_failwith
        |> Stdlib.print_endline
      | Error s -> Stdlib.Printf.printf "!ERROR! %s" s
    in
    Stdlib.flush_all ()
  in

  test ~options:(Y.make_options ~config_path_relative_to:"../../../test/files" ()) "!CONFIG advanced.yml";
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

  Y.parse ~cwd
    ~options:(Y.make_options ~config_path_relative_to:"../../../test/files" ())
    ~of_yojson:Utils.foo_of_yojson "!CONFIG advanced.yml"
  |> Utils.render
  |> print_endline;
  [%expect
    {|
    ((hello world) (abc DEF)
     (names
      (((name Alice)) ((name Bob)) ((name Charlie)) ((name Diane)) ((name Eric))))) |}]
