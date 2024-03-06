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

val make_enum : Yojson.Basic.t list -> t

val make_schema : name:string -> reject_extras:bool -> object_entry list -> t

val to_string : t -> string

type step =
  | Dot of string
  | Index of int

type error =
  | Extraneous of {
      path: step list;
      name: string option;
    }
  | Missing of {
      path: step list;
      name: string option;
      missing: t;
    }
  | Type of {
      path: step list;
      name: string option;
      expected: t;
      found: Yojson.Safe.t;
    }
  | Incorrect_value of {
      path: step list;
      name: string option;
      expected: Yojson.Basic.t list;
      found: Yojson.Basic.t;
    }
  | Deserialization of {
      message: string;
      attempted: (Yojson.Safe.t, string) Either.t;
    }

val validate : ?path:step list -> ?nullable:bool -> t -> Yojson.Safe.t -> error list

val render_error : ?emphasis:(string -> string) -> error -> string
