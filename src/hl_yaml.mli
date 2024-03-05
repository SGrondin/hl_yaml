module JSON : sig
  val to_yaml_string : Yojson.Safe.t -> (string, string) result
end

type options

val default_options : options

val make_options :
  ?get_env_var:(string -> string option) ->
  ?get_file:(string -> string Lwt.t) ->
  ?config_path_relative_to:string ->
  ?file_path_relative_to:string ->
  ?enable_includes:bool ->
  ?enable_imports:bool ->
  ?allow_unused_anchors:bool ->
  ?validate_config_path:(string -> bool Lwt.t) ->
  ?process_scalar_tag:(tag:string -> string -> [ `Scalar of string | `YAML of Yaml.yaml ] Lwt.t option) ->
  unit ->
  options

module YAML : sig
  val of_string : ?options:options -> string -> (Yaml.yaml, string) result Lwt.t

  val to_yojson : Yaml.value -> Yojson.Safe.t

  val to_string :
    ?layout_style:Yaml.layout_style ->
    ?scalar_style:Yaml.scalar_style ->
    Yaml.yaml ->
    (string, string) result
end

module Json_spec : sig
  type object_entry = {
    key: string;
    required: bool;
    json_spec: t;
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
end

val parse :
  ?options:options ->
  ?validate:Json_spec.t ->
  of_yojson:(Yojson.Safe.t -> ('a, string) result) ->
  string ->
  ('a, Json_spec.error list) result Lwt.t
