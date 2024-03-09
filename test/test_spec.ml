open! Core
open Hl_yaml.Unix.Spec

let%expect_test "Validations" =
  let test json spec =
    validate spec json |> function
    | [] -> print_endline "Passed"
    | ll -> List.map ll ~f:render_error |> String.concat_lines |> print_endline
  in

  test (`List []) (JArray JAny);
  [%expect {| Passed |}];

  test (`List [ `Int 5 ]) (JArray JAny);
  [%expect {| Passed |}];

  test (`List [ `String ""; `Int 5 ]) (JArray JAny);
  [%expect {| Passed |}];

  test (`List [ `String ""; `Int 5 ]) (JArray JString);
  [%expect {| $[1] expected a String, but found: Integer |}];

  test (`List [ `String ""; `Int 5 ]) (JArray JAtom);
  [%expect {| Passed |}];

  test (`List [ `Int 5 ]) (JArray JInt);
  [%expect {| Passed |}];

  test (`List [ `Int 5 ]) (JArray JFloat);
  [%expect {| $[0] expected a Double, but found: Integer |}];

  test (`List []) (JArray JNumeric);
  [%expect {| Passed |}];

  test (`List [ `String "abc"; `String "def"; `Int 123 ]) (JArray (make_enum [ `String "abc"; `Int 111 ]));
  [%expect
    {|
    $[1] incorrect value "def", expected one of: "abc" | 111
    $[2] incorrect value 123, expected one of: "abc" | 111 |}];

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
    Extraneous key: $.extra
    Missing key: $.abc (String) |}];

  test
    (`List [ `Int 5; `String "abc" ])
    (make_schema ~name:"foo" ~reject_extras:true [ { key = "abc"; required = true; spec = JAtom } ]);
  [%expect {| $ expected a foo Object, but found: Array of Integers |}];

  test (`Assoc []) (JObject (JArray JString));
  [%expect {| Passed |}];

  test
    (`Assoc [ "a", `List []; "b", `String "x"; "c d", `List [ `Null; `Int 5; `String "y" ] ])
    (JObject (JArray JString));
  [%expect
    {|
    $.b expected a Array of Strings, but found: String
    $["c d"][0] expected a String, but found: Null
    $["c d"][1] expected a String, but found: Integer |}];

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
    $.g expected a Boolean, but found: Null
    $["c d"][0] expected a String, but found: Null |}]
