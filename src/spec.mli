type object_entry = {
  key: string;
  required: bool;
  spec: t;
}

and schema_keys

and enum

and t =
  | JAny
  | JAtom  (** Atoms are supposed to be strings, but other simple values are okay (int, float, bool) *)
  | JNull
  | JBool
  | JInt
  | JFloat
  | JNumeric (* Accepts Int and Float *)
  | JString
  | JEnum of enum
  | JArray of t
  | JOneOrArray of t
  | JObject of t
  | JSchema of {
      name: string;
      reject_extras: bool;
      keys: schema_keys;
    }
[@@deriving to_yojson]

val make_enum : Yojson.Basic.t list -> t

val make_schema : name:string -> reject_extras:bool -> object_entry list -> t

val to_string : t -> string

type step =
  | Dot of string
  | Index of int
[@@deriving to_yojson]

type path = step list [@@deriving to_yojson]

type error =
  | Extraneous of {
      path: path;
      name: string option;
    }
  | Missing of {
      path: path;
      name: string option;
      missing: t;
    }
  | Type of {
      path: path;
      name: string option;
      expected: t;
      found: Yojson.Safe.t;
    }
  | Incorrect_value of {
      path: path;
      name: string option;
      expected: Yojson.Basic.t list;
      found: Yojson.Basic.t;
    }
  | Deserialization of {
      message: string;
      attempted: [ `JSON of Yojson.Safe.t | `String of string ];
    }
  | Processing of { message: string }
[@@deriving to_yojson]

val validate : ?path:path -> ?nullable:bool -> t -> Yojson.Safe.t -> error list

val render_error : ?emphasis:(string -> string) -> error -> string
