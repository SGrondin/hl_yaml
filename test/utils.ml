open! Core

module Lwt_expect_tests = struct
  module Expect_test_config : Expect_test_config_types.S with module IO = Lwt = struct
    module IO = Lwt

    let sanitize s = s

    let run f = Lwt_main.run (f ())

    let upon_unreleasable_issue = `CR
  end
end

let ok_or_failwith = function
| Ok x -> x
| Error s -> failwith s

type enum =
  | ABC
  | DEF
[@@deriving sexp_of]

let enum_of_yojson = function
| `String "abc" -> Ok ABC
| `String "def" -> Ok DEF
| json -> failwith ("Unexpected value for enum: " ^ Yojson.Safe.to_string json)

type name = { name: string } [@@deriving sexp_of, of_yojson]

type foo = {
  hello: string;
  abc: enum;
  names: name list; [@key "some array"]
}
[@@deriving sexp_of, of_yojson]

let render = function
| Error errors -> Yojson.Safe.pretty_to_string ([%to_yojson: Hl_yaml.Spec.error list] errors)
| Ok foo -> Sexp.to_string_hum ([%sexp_of: foo] foo)
