module M = Hl_yaml.Make_Eio (Eio)
open M

let ok_or_failwith = function
| Ok x -> x
| Error s -> failwith s

let%expect_test "Config YAML processing - Eio" =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.fs env in
  let test ?options raw =
    let parsed = YAML.of_string ~cwd ?options raw in
    let () =
      match parsed with
      | Ok x ->
        x |> YAML.to_string ~layout_style:`Block ~scalar_style:`Plain |> ok_or_failwith |> print_endline
      | Error s -> Printf.printf "!ERROR! %s" s
    in
    Stdlib.flush_all ()
  in
  test ~options:(make_options ~config_path_relative_to:"../../../test" ()) "!CONFIG advanced.yml";
  [%expect
    {|
    hello: world
    abc: def
    some array:
    - name: Alice
    - name: Bob
    - name: Charlie
    - name: Diane
    - name: Eric |}]
