open! Core
open Lwt.Syntax
open Utils.Lwt_expect_tests
module Y = Hl_yaml.Make_Lwt (Lwt) (Lwt_io)

let%expect_test "Config YAML processing - Lwt" =
  let test ?options raw =
    let* parsed = Y.YAML.of_string ?options raw in
    let* () =
      match parsed with
      | Ok x -> x |> Y.YAML.to_string |> Utils.ok_or_failwith |> Lwt_io.printl
      | Error _ as err -> Lwt_io.printlf !"%{sexp: (_, string) Result.t}" err
    in
    Lwt_io.flush_all ()
  in

  let* () =
    test ~options:(Y.make_options ~config_path_filter_map:Utils.map_path_lwt ()) "!CONFIG advanced.yml"
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
    let* x =
      Y.parse
        ~options:(Y.make_options ~config_path_filter_map:Utils.map_path_lwt ())
        ~of_yojson:Utils.foo_of_yojson "!CONFIG advanced.yml"
    in
    let* () = Utils.render x |> Lwt_io.printl in
    Lwt_io.flush_all ()
  in
  [%expect
    {|
    ((hello world) (abc DEF)
     (names
      (((name Alice)) ((name Bob)) ((name Charlie)) ((name Diane)) ((name Eric))))) |}];

  Lwt.return_unit
