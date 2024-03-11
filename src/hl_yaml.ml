module StringTable = struct
  module StringTable = Hashtbl.Make (String)
  include StringTable

  let add tbl ~key ~data =
    if StringTable.mem tbl key
    then `Duplicate
    else (
      StringTable.add tbl key data;
      `Ok )

  let find_or_add tbl key ~default =
    match StringTable.find_opt tbl key with
    | Some x -> x
    | None ->
      let x = default () in
      StringTable.add tbl key x;
      x
end

module StringSet = Set.Make (String)

let ( >>= ) = Result.bind

let rec safe_yaml_to_string ?(len = 256 * 1024) ?layout_style ?scalar_style yaml =
  (* [Yaml.to_string]: The current implementation uses a non-resizable internal string buffer of 256KB, which can be increased via len *)
  (* This function retries up to a size of 2Mb *)
  ( Yaml.to_json yaml >>= fun json ->
    try Yaml.to_string json ~len ?layout_style ?scalar_style with
    | Failure msg -> Error (`Msg msg)
    | exn -> Error (`Msg (Printexc.to_string exn)) )
  |> function
  | Error _ when len < 4 * 1024 * 1024 ->
    safe_yaml_to_string ~len:(len * 2) ?layout_style ?scalar_style yaml
  | x -> x

let show_yaml_in_error yaml =
  match safe_yaml_to_string yaml with
  | Ok s -> s
  | Error (`Msg msg) -> failwith ("Invalid YAML to flatten: " ^ msg)

let wrap ?attempted f x =
  f x
  |> Result.map_error (fun (`Msg message) ->
       match attempted with
       | None -> [ Spec.Processing { message } ]
       | Some attempted -> [ Spec.Deserialization { message; attempted } ] )

module Make (IO : S.IO) = struct
  type filepath = IO.filepath

  let ( let* ) = IO.bind

  let ( let+ ) x f = IO.map f x

  let ( >|= ) x f = IO.map f x

  exception HL_YAML_error of string

  let failwith s = raise (HL_YAML_error s)

  let failwithf fmt = Format.ksprintf (fun s () -> failwith s) fmt

  type options = {
    get_env_var: string -> string option;
    get_file: IO.filepath -> string IO.t;
    config_path_filter_map: string -> string IO.t;
    file_path_filter_map: string -> string IO.t;
    allow_unused_anchors: bool;
    allow_redefining_anchors: bool;
    enable_includes: bool;
    enable_conditional_includes: bool;
    enable_imports: bool;
    process_scalar_tag: tag:string -> string -> [ `Scalar of string | `YAML of Yaml.yaml ] IO.t option;
  }

  let make_options ?get_env_var ?get_file ?config_path_filter_map ?file_path_filter_map ?enable_includes
    ?enable_conditional_includes ?enable_imports ?allow_unused_anchors ?allow_redefining_anchors
    ?process_scalar_tag () =
    {
      get_env_var = Option.value get_env_var ~default:Sys.getenv_opt;
      get_file = Option.value get_file ~default:IO.read_file;
      config_path_filter_map = Option.value config_path_filter_map ~default:(fun s -> IO.return s);
      file_path_filter_map = Option.value file_path_filter_map ~default:(fun s -> IO.return s);
      enable_includes = Option.value enable_includes ~default:true;
      enable_conditional_includes = Option.value enable_conditional_includes ~default:true;
      enable_imports = Option.value enable_imports ~default:true;
      allow_unused_anchors = Option.value allow_unused_anchors ~default:false;
      allow_redefining_anchors = Option.value allow_redefining_anchors ~default:false;
      process_scalar_tag = Option.value process_scalar_tag ~default:(fun ~tag:_ _ -> None);
    }

  let default_options = make_options ()

  module Spec = Spec

  module JSON = struct
    let to_yaml_string (json : Yojson.Safe.t) =
      let rec loop json : Yaml.value =
        match json with
        | (`String _ as x)
         |(`Bool _ as x)
         |(`Null as x) ->
          x
        | `Int x -> `Float (Int.to_float x)
        | `Float x -> `Float x
        | `List array -> `A (List.map loop array)
        | `Assoc pairs -> `O (List.map (fun (key, value) -> key, loop value) pairs)
        | `Intlit _
         |`Tuple _
         |`Variant _ ->
          (loop [@tailcall]) (Yojson.Safe.to_basic json :> Yojson.Safe.t)
      in
      let attempted = `JSON json in
      json |> loop |> wrap ~attempted Yaml.of_json >>= wrap ~attempted safe_yaml_to_string
  end

  module YAML = struct
    let to_string ?layout_style ?scalar_style yaml =
      wrap (safe_yaml_to_string ?layout_style ?scalar_style) yaml

    open Yaml

    let rec to_yojson (yaml : value) : Yojson.Safe.t =
      match yaml with
      | (`String _ | `Bool _ | `Null) as x -> x
      | `Float number ->
        let as_int = Int.of_float number in
        if Float.equal (Int.to_float as_int) number then `Int as_int else `Float number
      | `A array -> `List (List.map to_yojson array)
      | `O pairs -> `Assoc (List.map (fun (key, value) -> key, to_yojson value) pairs)

    type 'a state = {
      refs: (yaml IO.t * int) StringTable.t;
      env_vars: string StringTable.t;
      files: string IO.t StringTable.t;
      configs: yaml IO.t StringTable.t;
      options: options;
    }

    let resolve_references ~to_filepath state (yaml : yaml) : (yaml, Spec.error list) result IO.t =
      let process_anchor left right =
        let remove_anchor = function
          | `Scalar ({ anchor = Some _; _ } as scalar) -> `Scalar { scalar with anchor = None }
          | `A ({ s_anchor = Some _; _ } as array) -> `A { array with s_anchor = None }
          | `O ({ m_anchor = Some _; _ } as mapping) -> `O { mapping with m_anchor = None }
          | yaml -> yaml
        in
        let left =
          match left with
          | `Scalar { anchor = Some key; _ }
           |`A { s_anchor = Some key; _ }
           |`O { m_anchor = Some key; _ } -> (
            let data = right, 0 in
            match StringTable.add state.refs ~key ~data with
            | `Ok -> remove_anchor left
            | `Duplicate when state.options.allow_redefining_anchors ->
              StringTable.replace state.refs key data;
              remove_anchor left
            | `Duplicate -> failwithf "Duplicate YAML anchor name &%s" key () )
          | _ -> left
        in
        left, right
      in
      let get_env_var ?default name =
        StringTable.find_or_add state.env_vars name ~default:(fun () ->
          match state.options.get_env_var name, default with
          | Some x, _
           |None, Some x ->
            x
          | None, None -> failwith ("Environment variable requested by YAML config not found: " ^ name) )
      in
      let get_file filename =
        StringTable.find_or_add state.files filename ~default:(fun () ->
          let* path = state.options.file_path_filter_map filename in
          to_filepath path |> state.options.get_file )
      in
      let make_key key =
        let buf = Buffer.create (String.length key + 10) in
        Buffer.add_char buf '\'';
        String.iter
          (function
            | '\'' -> Buffer.add_string buf "\\'"
            | '\\' -> Buffer.add_string buf "\\\\"
            | c -> Buffer.add_char buf c)
          key;
        Buffer.add_char buf '\'';
        Buffer.contents buf
      in
      let env_var_exists name =
        match StringTable.mem state.env_vars name, lazy (state.options.get_env_var name) with
        | true, _ -> true
        | false, (lazy (None | Some "")) -> false
        | false, (lazy (Some _)) -> true
      in
      let check_flatten left =
        match left with
        | `Scalar { tag = Some "!IF_DEF"; value; _ } -> env_var_exists value, "!IF_DEF"
        | `Scalar { tag = Some "!IF_NOT_DEF"; value; _ } -> not (env_var_exists value), "!IF_NOT_DEF"
        | `Scalar { value = "<<"; _ } -> true, "<<"
        | yaml ->
          failwithf "YAML processing error in check_flatten. Please report this bug. Unexpected: %s"
            (show_yaml_in_error yaml) ()
      in
      let is_include_allowed = function
        | `Scalar { value = "<<"; _ } -> state.options.enable_includes
        | `Scalar { tag = Some "!IF_DEF"; _ }
         |`Scalar { tag = Some "!IF_NOT_DEF"; _ } ->
          state.options.enable_conditional_includes
        | yaml ->
          failwithf "YAML processing error in is_include_allowed. Please report this bug. Unexpected: %s"
            (show_yaml_in_error yaml) ()
      in
      let rec loop : yaml -> yaml IO.t = function
        | `A ({ s_members; _ } as sequence) as original ->
          IO.map_s
            (function
              | `O { m_members = [ ((`Scalar { tag = Some "!IF_DEF"; _ } as left), right) ]; _ }
               |`O { m_members = [ ((`Scalar { tag = Some "!IF_NOT_DEF"; _ } as left), right) ]; _ }
               |`O { m_members = [ ((`Scalar { value = "<<"; _ } as left), right) ]; _ }
                when is_include_allowed left -> (
                (* !IF_DEF, !IF_NOT_DEF, or "<<" *)
                let* right = process_anchor left (loop right) |> snd in
                match check_flatten left, right with
                | (true, _), `A { s_members; _ } -> IO.map_s loop s_members
                | (false, _), _ -> IO.return []
                | (true, _), yaml -> IO.return [ yaml ] )
              | yaml ->
                (* Normal array element *)
                loop yaml >|= fun x -> [ x ])
            s_members
          >|= (fun lll -> `A { sequence with s_members = List.concat lll })
          |> process_anchor original
          |> snd
        | `O ({ m_members; _ } as mapping) as original ->
          IO.map_s
            (function
              | (`Scalar { tag = Some "!IF_DEF"; _ } as left), right
               |(`Scalar { tag = Some "!IF_NOT_DEF"; _ } as left), right
               |(`Scalar { value = "<<"; _ } as left), right
                when is_include_allowed left -> (
                (* !IF_DEF, !IF_NOT_DEF, or "<<" *)
                let+ right = process_anchor left (loop right) |> snd in
                match check_flatten left, right with
                | (true, _), `O { m_members; _ } -> m_members
                | (false, _), _ -> []
                | (true, flatten_name), yaml ->
                  failwithf
                    "YAML error: right hand side of '%s' should be a list of key-value pairs but found:\n\
                     %s"
                    flatten_name (show_yaml_in_error yaml) () )
              | left, right ->
                (* Normal object key *)
                let left, right = process_anchor left (loop right) in
                let+ v = right in
                [ left, v ])
            m_members
          >|= (fun lll ->
                let m_members, _set =
                  (* Flatten, reverse, dedupe *)
                  List.fold_right
                    (fun ll init ->
                      List.fold_right
                        (fun yaml (acc, set) ->
                          match yaml with
                          | `Scalar { value = ""; _ }, _ -> acc, set
                          | `Scalar { value; _ }, _ when StringSet.mem value set -> acc, set
                          | (`Scalar { value; _ }, _) as pair -> pair :: acc, StringSet.add value set
                          | x -> x :: acc, set)
                        ll init)
                    lll ([], StringSet.empty)
                in
                `O { mapping with m_members })
          |> process_anchor original
          |> snd
        | `Alias key ->
          let ((yaml, _) as data) =
            match StringTable.find_opt state.refs key with
            | Some (found, ref_count) -> found, ref_count + 1
            | None -> failwithf "YAML *%s references the missing anchor &%s" key key ()
          in
          StringTable.replace state.refs key data;
          yaml
        | `Scalar scalar as original ->
          (match scalar with
          | { tag = None; _ } as x -> IO.return (`Scalar { x with anchor = None })
          | { tag = Some tag; value; _ } -> (
            let make_scalar value =
              `Scalar
                {
                  anchor = None;
                  tag = None;
                  value;
                  plain_implicit = true;
                  quoted_implicit = false;
                  style = `Plain;
                }
            in
            match state.options.process_scalar_tag ~tag value, tag with
            | Some p, _ -> (
              p >|= function
              | `Scalar s -> make_scalar s
              | `YAML x -> x )
            | None, "!ENV" -> get_env_var value |> make_scalar |> IO.return
            | None, "!ENV_OPT" -> get_env_var ~default:"" value |> make_scalar |> IO.return
            | None, "!FILE" -> get_file value >|= make_scalar
            | None, "!ENV_FILE" -> get_env_var value |> get_file >|= make_scalar
            | None, "!ENV_STR" -> get_env_var value |> make_key |> make_scalar |> IO.return
            | None, "!CONFIG" when state.options.enable_imports -> get_config value
            | None, "!CONFIG" -> failwith "YAML !CONFIG imports are disabled"
            | None, _ -> failwithf "Undefined YAML tag: %s" tag () ))
          |> process_anchor original
          |> snd
      and get_config filename =
        StringTable.find_or_add state.configs filename ~default:(fun () ->
          let* contents =
            let* path = state.options.config_path_filter_map filename in
            to_filepath path |> state.options.get_file
          in
          match yaml_of_string contents with
          | Ok partial_config -> loop partial_config
          | Error (`Msg msg) -> failwithf "Invalid YAML in !CONFIG %s: %s" filename msg () )
      in
      IO.catch
        (fun () ->
          let+ res = loop yaml in
          if state.options.allow_unused_anchors
          then Ok res
          else
            StringTable.to_seq state.refs
            |> Seq.filter (fun (_, (_, ref_count)) -> ref_count = 0)
            |> List.of_seq
            |> function
            | [] -> Ok res
            | ll ->
              List.map
                (fun (name, _) ->
                  Spec.Processing { message = "YAML anchor &" ^ name ^ " is never used." })
                ll
              |> Result.error)
        (function
          | HL_YAML_error message -> IO.return (Error [ Spec.Processing { message } ])
          | exn -> raise exn)

    let of_string ~to_filepath ?(options = default_options) str =
      match yaml_of_string str with
      | Ok x ->
        let state =
          {
            refs = StringTable.create 16;
            env_vars = StringTable.create 16;
            files = StringTable.create 16;
            configs = StringTable.create 16;
            options;
          }
        in
        resolve_references ~to_filepath state x
      | Error (`Msg message) ->
        IO.return (Error [ Spec.Deserialization { message; attempted = `String str } ])
  end

  let parse ~to_filepath ?options ?validate:(spec = Spec.JAny) ~of_yojson:parser contents =
    let+ json =
      let+ res = YAML.of_string ~to_filepath ?options contents in
      res >>= wrap ~attempted:(`String contents) Yaml.to_json |> Result.map YAML.to_yojson
    in
    json >>= fun json ->
    match Spec.validate spec json with
    | [] ->
      parser json
      |> Result.map_error (fun message -> [ Spec.Deserialization { message; attempted = `JSON json } ])
    | errors -> Error errors
end

module Spec = Spec

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

module Non_Monadic = struct
  type +'a t = 'a

  let return x = x

  let map f x = f x

  let bind x f = f x

  let catch f catch =
    try f () with
    | exn -> catch exn

  let map_s f ll = List.map f ll
end

module Make_Lwt (Lwt : S.S_Lwt) (Lwt_io : S.S_Lwt_io with type 'a lwt_t := 'a Lwt.t) :
  Intf with type 'a io := 'a Lwt.t and type filepath := string = struct
  module M = Make (struct
    include Lwt

    type filepath = string

    let map_s f l =
      let rec inner acc = function
        | [] -> List.rev acc |> Lwt.return
        | hd :: tl -> Lwt.bind (Lwt.apply f hd) (fun r -> (inner [@tailcall]) (r :: acc) tl)
      in
      inner [] l

    let read_file filename =
      Lwt_io.with_file ~flags:Unix.[ O_RDONLY; O_NONBLOCK ] ~mode:Input filename Lwt_io.read
  end)

  include M

  module YAML = struct
    include M.YAML

    let of_string ?options str = M.YAML.of_string ~to_filepath:Fun.id ?options str
  end

  let parse ?options ?validate ~of_yojson contents =
    M.parse ~to_filepath:Fun.id ?options ?validate ~of_yojson contents
end

module type Intf_Eio = sig
  include Intf

  module YAML : sig
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

module Make_Eio (Eio : S.S_Eio) :
  Intf_Eio with type 'a io := 'a and type filepath := Eio.Fs.dir_ty Eio.Path.t = struct
  module M = Make (struct
    include Non_Monadic

    type filepath = Eio.Fs.dir_ty Eio.Path.t

    let read_file filepath = Eio.Path.load filepath
  end)

  include M

  module YAML = struct
    include M.YAML

    let of_string ~cwd ?options str =
      M.YAML.of_string ~to_filepath:Eio.Path.((fun s -> cwd / s)) ?options str
  end

  let parse ~cwd ?options ?validate ~of_yojson contents =
    M.parse ~to_filepath:Eio.Path.((fun s -> cwd / s)) ?options ?validate ~of_yojson contents
end

module Unix : Intf with type 'a io := 'a = struct
  module M = Make (struct
    include Non_Monadic

    type filepath = string

    let read_file filename =
      let chan = In_channel.open_bin filename in
      Fun.protect (fun () -> In_channel.input_all chan) ~finally:(fun () -> In_channel.close chan)
  end)

  include M

  module YAML = struct
    include M.YAML

    let of_string ?options str = M.YAML.of_string ~to_filepath:Fun.id ?options str
  end

  let parse ?options ?validate ~of_yojson contents =
    M.parse ~to_filepath:Fun.id ?options ?validate ~of_yojson contents
end
