open! Core
open Hl_yaml.Unix.Spec

let%expect_test "Validations" =
  let test json spec =
    validate spec json |> function
    | [] -> print_endline "Passed"
    | ll -> List.map ll ~f:error_to_string |> String.concat_lines |> print_endline
  in

  test (`List []) (JArray JAny);
  [%expect {| Passed |}];

  test (`List [ `Int 5 ]) (JArray JAny);
  [%expect {| Passed |}];

  test (`List [ `String ""; `Int 5 ]) (JArray JAny);
  [%expect {| Passed |}];

  test (`List [ `String ""; `Int 5 ]) (JArray JString);
  [%expect {| Type mismatch at $[1], expected String, but found: Integer |}];

  test (`List [ `String ""; `Int 5 ]) (JArray JAtom);
  [%expect {| Passed |}];

  test (`List [ `Int 5 ]) (JArray JInt);
  [%expect {| Passed |}];

  test (`List [ `Int 5 ]) (JArray JFloat);
  [%expect {| Type mismatch at $[0], expected Double, but found: Integer |}];

  test (`List []) (JArray JNumeric);
  [%expect {| Passed |}];

  test (`List [ `String "abc"; `String "def"; `Int 123 ]) (JArray (make_enum [ `String "abc"; `Int 111 ]));
  [%expect
    {|
    Incorrect value at $[1], found "def", but expected one of: "abc" | 111
    Incorrect value at $[2], found 123, but expected one of: "abc" | 111 |}];

  test (`List [ `Int 111; `String "abc"; `Int 111 ]) (JOneOrArray (make_enum [ `String "abc"; `Int 111 ]));
  [%expect {| Passed |}];

  test (`Int 111) (JOneOrArray (make_enum [ `String "abc"; `Int 111 ]));
  [%expect {| Passed |}];

  test (`String "abc") (JOneOrArray (make_enum [ `String "abc"; `Int 111 ]));
  [%expect {| Passed |}];

  test
    (`Assoc [ "extra", `List [] ])
    (make_schema ~name:"foo" ~reject_extras:true [ { key = "abc"; required = true; spec = JAtom } ]);
  [%expect {|
    Missing key: $.abc (String)
    Extraneous key: $.extra (Array) |}];

  test
    (`List [ `Int 5; `String "abc" ])
    (make_schema ~name:"foo" ~reject_extras:true [ { key = "abc"; required = true; spec = JAtom } ]);
  [%expect {| Type mismatch at $, expected foo Object, but found: Array of Integers |}];

  test (`Assoc []) (JObject (JArray JString));
  [%expect {| Passed |}];

  test
    (`Assoc [ "a", `List []; "b", `String "x"; "c d", `List [ `Null; `Int 5; `String "y" ] ])
    (JObject (JArray JString));
  [%expect
    {|
    Type mismatch at $.b, expected Array of Strings, but found: String
    Type mismatch at $["c d"][0], expected String, but found: Null
    Type mismatch at $["c d"][1], expected String, but found: Integer |}];

  test
    (`Assoc
      [
        "a", `List [];
        "b", `String "x";
        "c d", `List [ `Null; `Int 5; `String "y" ];
        "f", `Null;
        "g", `Null;
      ] )
    (make_schema ~name:"foo" ~reject_extras:false
       [
         { key = "a"; required = true; spec = JArray JAtom };
         { key = "b"; required = true; spec = JOneOrArray JAtom };
         { key = "c d"; required = true; spec = JOneOrArray JAtom };
         { key = "e"; required = false; spec = JBool };
         { key = "f"; required = false; spec = JBool };
         { key = "g"; required = true; spec = JBool };
       ] );
  [%expect
    {|
    Type mismatch at $["c d"][0], expected String, but found: Null
    Type mismatch at $.g, expected Boolean, but found: Null |}];

  validate
    (make_schema ~name:"foo" ~reject_extras:false
       [
         { key = "a"; required = true; spec = JArray JAtom };
         { key = "b"; required = true; spec = JOneOrArray JAtom };
         { key = "c d"; required = true; spec = JOneOrArray JAtom };
         { key = "e"; required = false; spec = JBool };
         { key = "f"; required = false; spec = JBool };
         { key = "g"; required = true; spec = JBool };
       ] )
    (`Assoc
      [
        "a", `List [];
        "b", `String "x";
        "c d", `List [ `Null; `Int 5; `String "y" ];
        "f", `Null;
        "g", `Null;
      ] )
  |> List.map ~f:error_to_yojson
  |> (fun ll -> `List ll)
  |> Yojson.Safe.pretty_to_string
  |> print_endline;
  [%expect {|
    [
      {
        "type": "type",
        "name": null,
        "path": "$[\"c d\"][0]",
        "path_parts": [
          { "type": "key", "key": "$" },
          { "type": "key", "key": "c d" },
          { "type": "index", "index": 0 }
        ],
        "expected": [ "JAtom" ],
        "found": null
      },
      {
        "type": "type",
        "name": null,
        "path": "$.g",
        "path_parts": [
          { "type": "key", "key": "$" }, { "type": "dot", "dot": "g" }
        ],
        "expected": [ "JBool" ],
        "found": null
      }
    ] |}]
