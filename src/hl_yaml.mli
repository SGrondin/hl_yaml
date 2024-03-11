module type Intf = sig
  type +'a io

  type filepath

  type options = {
    get_env_var: string -> string option;
    get_file: filepath -> string io;
    config_path_filter_map: string -> string io;
    file_path_filter_map: string -> string io;
    allow_unused_anchors: bool;
    allow_redefining_anchors: bool;
    enable_includes: bool;
    enable_conditional_includes: bool;
    enable_imports: bool;
    process_scalar_tag: tag:string -> string -> [ `Scalar of string | `YAML of Yaml.yaml ] io option;
  }

  val default_options : options

  val make_options :
    ?get_env_var:(string -> string option) ->
    ?get_file:(filepath -> string io) ->
    ?config_path_filter_map:(string -> string io) ->
    ?file_path_filter_map:(string -> string io) ->
    ?enable_includes:bool ->
    ?enable_conditional_includes:bool ->
    ?enable_imports:bool ->
    ?allow_unused_anchors:bool ->
    ?allow_redefining_anchors:bool ->
    ?process_scalar_tag:(tag:string -> string -> [ `Scalar of string | `YAML of Yaml.yaml ] io option) ->
    unit ->
    options

  module JSON : sig
    val to_yaml_string : Yojson.Safe.t -> (string, Spec.error list) result
  end

  module YAML : sig
    val of_string : ?options:options -> string -> (Yaml.yaml, Spec.error list) result io

    val to_yojson : Yaml.value -> Yojson.Safe.t

    val to_string :
      ?layout_style:Yaml.layout_style ->
      ?scalar_style:Yaml.scalar_style ->
      Yaml.yaml ->
      (string, Spec.error list) result
  end

  module Spec : sig
    (** @inline *)
    include module type of Spec
  end

  exception HL_YAML_error of string

  val parse :
    ?options:options ->
    ?validate:Spec.t ->
    of_yojson:(Yojson.Safe.t -> ('a, string) result) ->
    string ->
    ('a, Spec.error list) result io
end

module Spec : sig
  (** @inline *)
  include module type of Spec
end

module Unix : Intf with type 'a io := 'a

module Make_Lwt : functor (Lwt : S.S_Lwt) (Lwt_io : S.S_Lwt_io with type 'a lwt_t := 'a Lwt.t) ->
  Intf with type 'a io := 'a Lwt.t with type filepath := string

module type Intf_Eio = sig
  include Intf

  module YAML : sig
    (** @inline *)
    include module type of YAML

    val of_string : cwd:filepath -> ?options:options -> string -> (Yaml.yaml, Spec.error list) result io
  end

  val parse :
    cwd:filepath ->
    ?options:options ->
    ?validate:Spec.t ->
    of_yojson:(Yojson.Safe.t -> ('a, string) result) ->
    string ->
    ('a, Spec.error list) result io
end

module Make_Eio : functor (Eio : S.S_Eio) ->
  Intf_Eio with type 'a io := 'a and type filepath := Eio.Fs.dir_ty Eio.Path.t
