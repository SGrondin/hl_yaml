open! Core
module Y = Hl_yaml.Unix

let ok_or_failwith = function
| Ok x -> x
| Error s -> failwith s

let%expect_test "Config YAML processing - Unix" =
  let test ?options raw =
    let parsed = Y.YAML.of_string ?options raw in
    let () =
      match parsed with
      | Ok x -> x |> Y.YAML.to_string |> ok_or_failwith |> print_endline
      | Error _ as err -> Stdlib.Printf.printf !"%{sexp: (_, string) Result.t}" err
    in
    Stdlib.flush_all ()
  in
  test ~options:(Y.make_options ~config_path_filter_map:Utils.map_path ()) "!CONFIG advanced.yml";
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

  Y.parse
    ~options:(Y.make_options ~config_path_filter_map:Utils.map_path ())
    ~of_yojson:Utils.foo_of_yojson "!CONFIG advanced.yml"
  |> Utils.render
  |> print_endline;
  [%expect
    {|
    ((hello world) (abc DEF)
     (names
      (((name Alice)) ((name Bob)) ((name Charlie)) ((name Diane)) ((name Eric))))) |}]
