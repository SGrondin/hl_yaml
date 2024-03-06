module type Intf = sig
  type +'a io

  type options

  val default_options : options

  val make_options :
    ?get_env_var:(string -> string option) ->
    ?get_file:(string -> string io) ->
    ?config_path_relative_to:string ->
    ?file_path_relative_to:string ->
    ?enable_includes:bool ->
    ?enable_imports:bool ->
    ?allow_unused_anchors:bool ->
    ?validate_config_path:(string -> bool io) ->
    ?process_scalar_tag:(tag:string -> string -> [ `Scalar of string | `YAML of Yaml.yaml ] io option) ->
    unit ->
    options

  module JSON : sig
    val to_yaml_string : Yojson.Safe.t -> (string, string) result
  end

  module YAML : sig
    val of_string : ?options:options -> string -> (Yaml.yaml, string) result io

    val to_yojson : Yaml.value -> Yojson.Safe.t

    val to_string :
      ?layout_style:Yaml.layout_style ->
      ?scalar_style:Yaml.scalar_style ->
      Yaml.yaml ->
      (string, string) result
  end

  module Json_spec : sig
    (** @inline *)
    include module type of Json_spec
  end

  val parse :
    ?options:options ->
    ?validate:Json_spec.t ->
    of_yojson:(Yojson.Safe.t -> ('a, string) result) ->
    string ->
    ('a, Json_spec.error list) result io
end

module Make_Lwt : functor (Lwt : S.S_Lwt) (Lwt_io : S.S_Lwt_io with type 'a lwt_t := 'a Lwt.t) ->
  Intf with type 'a io := 'a Lwt.t

module Unix : Intf with type 'a io := 'a
