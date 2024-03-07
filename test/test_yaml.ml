open Lwt.Syntax
open Utils.Lwt_expect_tests
module Y = Hl_yaml.Make_Lwt (Lwt) (Lwt_io)

let config1 =
  {|
&abc: !ENV_STR world

hello:
  - *abc
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
  !IF_NOT_DEF &save2 foo: &save3
    password: abc123
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

let%expect_test "Config YAML processing" =
  let test ?options raw =
    let* parsed = Y.YAML.of_string ?options raw in
    let* () =
      match parsed with
      | Ok x ->
        x
        |> Y.YAML.to_string ~layout_style:`Block ~scalar_style:`Plain
        |> Utils.ok_or_failwith
        |> Lwt_io.printl
      | Error s -> Lwt_io.printlf "!ERROR! %s" s
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
          String.to_seq str
          |> Seq.map (fun c ->
               let x = make_scalar c in
               x, x )
          |> List.of_seq
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

  let* () = test "&x: 5\nfoo: bar" in
  [%expect {| !ERROR! YAML anchor &x is never used |}];

  let* () = test ~options:(Y.make_options ~allow_unused_anchors:true ()) "&x: 5\nfoo: bar" in
  [%expect {| foo: bar |}];

  let* () =
    test config1
      ~options:
        (Y.make_options
           ~get_env_var:(function
             | "foo" -> Some "bar "
             | "PORT" -> Some "456"
             | "world" -> Some "WORLD"
             | _ -> None)
           () )
  in
  [%expect
    {|
    hello:
    - '''WORLD'''
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
      ~options:
        (Y.make_options ~enable_includes:false
           ~get_env_var:(function
             | "foo" -> Some "bar "
             | "PORT" -> Some "456"
             | "world" -> Some "WORLD"
             | _ -> None)
           () )
  in
  [%expect
    {|
    hello:
    - '''WORLD'''
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
        user: hello
      foo:
        password: abc123
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
