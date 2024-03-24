open! Core
open Lwt.Syntax
open Utils.Lwt_expect_tests
module Y = Hl_yaml.Make_Lwt (Lwt) (Lwt_io)

let config1 =
  {|
&abc1: !ENV_SQ world
&abc2: !ENV_DQ world

hello:
  - *abc1
  - *abc2
  - &foo !ENV foo
  - *foo

password1: &password1 supersecret1
&password2 password2: supersecret2
&password3: supersecret3

&BASE_OPTIONS:
  host: 123
  port: !ENV PORT

&connection1 connection1:
  <<: *BASE_OPTIONS
  user: admin
  password: *password1

connection2:
  <<: &options
    <<: *connection1
    user: user
    password: *password2

connection3:
  <<: *options
  password: *password3

connection4:
  <<: *options
  !IF_DEF &save1 not-present:
    user: hello
  !IF_NOT_DEF not-present:
    user: goodbye
  !IF_NOT_DEF &save2 foo: &save3
    password: abc123
  !IF_DEF foo:
    password: def456
  anchors:
    save1: *save1
    save2: *save2
    save3: *save3

&admin_connections admin_connections:
  - connection1

&user_connections user_connections:
  - connection2
  - connection3

all_connections:
  - <<: *admin_connections
  - <<: *user_connections
|}

let config2 = {|
&ref: abc
y: *ref
&ref: def
z: *ref
x-x: y # y == true
|}

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

  let* () = test "&s: world\nhello: *s" in
  [%expect {| hello: world |}];

  let* () =
    let make_scalar c =
      `Scalar
        Yaml.
          {
            anchor = None;
            tag = None;
            value = Format.sprintf "%c" c;
            plain_implicit = true;
            quoted_implicit = true;
            style = `Any;
          }
    in
    let process_scalar_tag ~tag str =
      match tag with
      | "!PLUS_FIVE" -> Some (Lwt.return (`Scalar (int_of_string str + 5 |> Int.to_string)))
      | "!SPREAD_IT" ->
        let m_members =
          String.to_list str
          |> List.map ~f:(fun c ->
               let x = make_scalar c in
               x, x )
        in
        Some
          (Lwt.return (`YAML (`O Yaml.{ m_anchor = None; m_tag = None; m_implicit = true; m_members })))
      | _ -> None
    in
    test ~options:(Y.make_options ~process_scalar_tag ()) "foo: !PLUS_FIVE 2\nbar: !SPREAD_IT abcdef"
  in
  [%expect {|
    foo: 7
    bar:
      a: a
      b: b
      c: c
      d: d
      e: e
      f: f |}];

  let* () = test "&x: 5\n&y: 6\nfoo: bar" in
  [%expect
    {|
    Error: [
      { "type": "processing", "message": "YAML anchor &y is never used." },
      { "type": "processing", "message": "YAML anchor &x is never used." }
    ] |}];

  let* () =
    test
      {|
&supervisors supervisors:
  - name: Daniel
    role: project manager
  - name: Elizabeth
    role: product owner

employees:
  - name: Alice
    &programmer <<:
      role: programmer
      language: OCaml
  - name: Bob
    <<: *programmer
  - name: Charlie
    <<: *programmer
    language: JavaScript
  - <<: *supervisors
|}
  in
  [%expect
    {|
    supervisors:
    - name: Daniel
      role: project manager
    - name: Elizabeth
      role: product owner
    employees:
    - name: Alice
      role: programmer
      language: OCaml
    - name: Bob
      role: programmer
      language: OCaml
    - name: Charlie
      role: programmer
      language: JavaScript
    - name: Daniel
      role: project manager
    - name: Elizabeth
      role: product owner |}];

  let* () = test ~options:(Y.make_options ~allow_unused_anchors:true ()) "&x: 5\nfoo: bar" in
  [%expect {| foo: bar |}];

  let* () = test ~options:(Y.make_options ()) config2 in
  [%expect {| Error: [ { "type": "processing", "message": "Duplicate YAML anchor name &ref" } ] |}];

  let* () = test "abc: *def" in
  [%expect
    {|
    Error: [
      {
        "type": "processing",
        "message": "YAML *def references the missing anchor &def"
      }
    ] |}];

  let* () = test ~options:(Y.make_options ~allow_redefining_anchors:true ()) config2 in
  [%expect {|
    "y": abc
    z: def
    x-x: true |}];

  let* () =
    let get_env_var _ = None in
    test ~options:(Y.make_options ~get_env_var ()) "foo: !ENV_OPT foo\nbar: null"
  in
  [%expect {|
    foo:
    bar: |}];

  let get_env_var = function
    | "foo" -> Some "bar "
    | "PORT" -> Some "456"
    | "world" -> Some "WORLD-\\-\"-"
    | _ -> None
  in

  let* () = test config1 ~options:(Y.make_options ~get_env_var ()) in
  [%expect
    {|
    hello:
    - '''WORLD-\\-"-'''
    - '"WORLD-\\-\"-"'
    - 'bar '
    - 'bar '
    password1: supersecret1
    password2: supersecret2
    connection1:
      host: 123
      port: 456
      user: admin
      password: supersecret1
    connection2:
      host: 123
      port: 456
      user: user
      password: supersecret2
    connection3:
      host: 123
      port: 456
      user: user
      password: supersecret3
    connection4:
      host: 123
      port: 456
      user: goodbye
      password: def456
      anchors:
        save1:
          user: hello
        save2:
          password: abc123
        save3:
          password: abc123
    admin_connections:
    - connection1
    user_connections:
    - connection2
    - connection3
    all_connections:
    - connection1
    - connection2
    - connection3 |}];

  let* () =
    test config1
      ~options:(Y.make_options ~enable_includes:false ~enable_conditional_includes:false ~get_env_var ())
  in
  [%expect
    {|
    hello:
    - '''WORLD-\\-"-'''
    - '"WORLD-\\-\"-"'
    - 'bar '
    - 'bar '
    password1: supersecret1
    password2: supersecret2
    connection1:
      <<:
        host: 123
        port: 456
      user: admin
      password: supersecret1
    connection2:
      <<:
        <<:
          <<:
            host: 123
            port: 456
          user: admin
          password: supersecret1
        user: user
        password: supersecret2
    connection3:
      <<:
        <<:
          <<:
            host: 123
            port: 456
          user: admin
          password: supersecret1
        user: user
        password: supersecret2
      password: supersecret3
    connection4:
      <<:
        <<:
          <<:
            host: 123
            port: 456
          user: admin
          password: supersecret1
        user: user
        password: supersecret2
      not-present:
        user: goodbye
      foo:
        password: def456
      anchors:
        save1:
          user: hello
        save2:
          password: abc123
        save3:
          password: abc123
    admin_connections:
    - connection1
    user_connections:
    - connection2
    - connection3
    all_connections:
    - <<:
      - connection1
    - <<:
      - connection2
      - connection3 |}];

  let* () =
    test config1
      ~options:(Y.make_options ~enable_includes:true ~enable_conditional_includes:false ~get_env_var ())
  in
  [%expect
    {|
    hello:
    - '''WORLD-\\-"-'''
    - '"WORLD-\\-\"-"'
    - 'bar '
    - 'bar '
    password1: supersecret1
    password2: supersecret2
    connection1:
      host: 123
      port: 456
      user: admin
      password: supersecret1
    connection2:
      host: 123
      port: 456
      user: user
      password: supersecret2
    connection3:
      host: 123
      port: 456
      user: user
      password: supersecret3
    connection4:
      host: 123
      port: 456
      user: user
      password: supersecret2
      not-present:
        user: goodbye
      foo:
        password: def456
      anchors:
        save1:
          user: hello
        save2:
          password: abc123
        save3:
          password: abc123
    admin_connections:
    - connection1
    user_connections:
    - connection2
    - connection3
    all_connections:
    - connection1
    - connection2
    - connection3 |}];

  let* () =
    test config1
      ~options:(Y.make_options ~enable_includes:false ~enable_conditional_includes:true ~get_env_var ())
  in
  [%expect
    {|
    hello:
    - '''WORLD-\\-"-'''
    - '"WORLD-\\-\"-"'
    - 'bar '
    - 'bar '
    password1: supersecret1
    password2: supersecret2
    connection1:
      <<:
        host: 123
        port: 456
      user: admin
      password: supersecret1
    connection2:
      <<:
        <<:
          <<:
            host: 123
            port: 456
          user: admin
          password: supersecret1
        user: user
        password: supersecret2
    connection3:
      <<:
        <<:
          <<:
            host: 123
            port: 456
          user: admin
          password: supersecret1
        user: user
        password: supersecret2
      password: supersecret3
    connection4:
      <<:
        <<:
          <<:
            host: 123
            port: 456
          user: admin
          password: supersecret1
        user: user
        password: supersecret2
      user: goodbye
      password: def456
      anchors:
        save1:
          user: hello
        save2:
          password: abc123
        save3:
          password: abc123
    admin_connections:
    - connection1
    user_connections:
    - connection2
    - connection3
    all_connections:
    - <<:
      - connection1
    - <<:
      - connection2
      - connection3 |}];

  Lwt.return_unit
