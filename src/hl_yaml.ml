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

module StringTable = struct
  module StringTable = Hashtbl.Make (struct
    type t = string

    let equal = String.equal

    let hash = String.hash
  end)

  include StringTable

  let create () = StringTable.create 16

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

  let update_and_return tbl key ~f =
    let x = f (StringTable.find_opt tbl key) in
    StringTable.replace tbl key x;
    x
end

module StringSet = Set.Make (struct
  type t = string

  let compare = String.compare
end)

let ( >>= ) = Result.bind

let rec safe_yaml_to_string ?(len = 256 * 1024) ?layout_style ?scalar_style yaml =
  (* [Yaml.to_string]: The current implementation uses a non-resizable internal string buffer of 256KB, which can be increased via len *)
  (* This function retries up to a size of 2Mb *)
  ( Yaml.to_json yaml >>= fun json ->
    try Yaml.to_string json ~len ?layout_style ?scalar_style with
    | Failure msg -> Error (`Msg msg)
    | exn -> Error (`Msg (Printexc.to_string exn)) )
  |> function
  | Error _ when len < 2 * 1024 * 1024 ->
    safe_yaml_to_string ~len:(len * 2) ?layout_style ?scalar_style yaml
  | x -> x

let show_yaml_in_flatten_error yaml =
  match safe_yaml_to_string yaml with
  | Ok s -> s
  | Error (`Msg msg) -> failwith ("Invalid YAML to flatten: " ^ msg)

let wrap f x = f x |> Result.map_error (fun (`Msg s) -> s)

module Make (IO : S.IO) = struct
  let ( let* ) = IO.bind

  let ( let+ ) f x = IO.map x f

  let ( >|= ) f x = IO.map x f

  type options = {
    get_env_var: string -> string option;
    get_file: string -> string IO.t;
    config_path_relative_to: string -> string;
    file_path_relative_to: string -> string;
    allow_unused_anchors: bool;
    enable_includes: bool;
    enable_imports: bool;
    process_scalar_tag: tag:string -> string -> [ `Scalar of string | `YAML of Yaml.yaml ] IO.t option;
    validate_config_path: string -> bool IO.t;
  }

  let make_options ?get_env_var ?get_file ?config_path_relative_to ?file_path_relative_to ?enable_includes
    ?enable_imports ?allow_unused_anchors ?validate_config_path ?process_scalar_tag () =
    {
      get_env_var = Option.value get_env_var ~default:Sys.getenv_opt;
      get_file = Option.value get_file ~default:IO.read_file;
      config_path_relative_to =
        Option.map Filename.concat config_path_relative_to |> Option.value ~default:Fun.id;
      file_path_relative_to =
        Option.map Filename.concat file_path_relative_to |> Option.value ~default:Fun.id;
      enable_includes = Option.value enable_includes ~default:true;
      enable_imports = Option.value enable_imports ~default:true;
      allow_unused_anchors = Option.value allow_unused_anchors ~default:false;
      process_scalar_tag = Option.value process_scalar_tag ~default:(fun ~tag:_ _ -> None);
      validate_config_path = Option.value validate_config_path ~default:(fun _ -> IO.return_true);
    }

  let default_options = make_options ()

  module Json_spec = Json_spec

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
      json |> loop |> wrap Yaml.of_json >>= wrap safe_yaml_to_string
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

    type state = {
      refs: (yaml IO.t * int) StringTable.t;
      env_vars: string StringTable.t;
      files: string IO.t StringTable.t;
      configs: yaml IO.t StringTable.t;
      options: options;
    }

    let failwithf fmt = Format.ksprintf (fun s () -> failwith s) fmt

    let resolve_references state (yaml : yaml) =
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
            match StringTable.add state.refs ~key ~data:(right, 0) with
            | `Ok -> remove_anchor left
            | `Duplicate -> failwithf "Duplicate YAML anchor name &%s" key () )
          | _ -> left
        in
        left, right
      in
      let get_env_var name =
        StringTable.find_or_add state.env_vars name ~default:(fun () ->
          match state.options.get_env_var name with
          | Some x -> x
          | None -> failwith ("Environment variable requested by YAML config not found: " ^ name) )
      in
      let get_file filename =
        StringTable.find_or_add state.files filename ~default:(fun () ->
          filename |> state.options.file_path_relative_to |> state.options.get_file )
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
          failwithf "YAML processing error in parse_flatten. Please report this bug. Unexpected: %s"
            (show_yaml_in_flatten_error yaml) ()
      in
      let rec loop : yaml -> yaml IO.t = function
        | `A ({ s_members; _ } as sequence) as original ->
          IO.map_s
            (function
              | `O { m_members = [ ((`Scalar { tag = Some "!IF_DEF"; _ } as left), right) ]; _ }
               |`O { m_members = [ ((`Scalar { tag = Some "!IF_NOT_DEF"; _ } as left), right) ]; _ }
               |`O { m_members = [ ((`Scalar { value = "<<"; _ } as left), right) ]; _ }
                when state.options.enable_includes -> (
                (* !IF_DEF, !IF_NOT_DEF, or "<<" *)
                let* right = process_anchor left (loop right) |> snd in
                match check_flatten left, right with
                | (true, _), `A { s_members; _ } -> IO.map_s loop s_members
                | (false, _), _ -> IO.return_nil
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
                when state.options.enable_includes -> (
                (* !IF_DEF, !IF_NOT_DEF, or "<<" *)
                let+ right = process_anchor left (loop right) |> snd in
                match check_flatten left, right with
                | (true, _), `O { m_members; _ } -> m_members
                | (false, _), _ -> []
                | (true, flatten_name), yaml ->
                  failwithf
                    "YAML error: right hand side of '%s' should be a list of key-value pairs but found:\n\
                     %s"
                    flatten_name (show_yaml_in_flatten_error yaml) () )
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
          StringTable.update_and_return state.refs key ~f:(function
            | Some (found, ref_count) -> found, ref_count + 1
            | None -> failwithf "YAML *%s references the missing anchor &%s" key key () )
          |> fst
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
            | None, "!FILE" -> get_file value >|= make_scalar
            | None, "!ENV_FILE" -> get_env_var value |> get_file >|= make_scalar
            | None, "!ENV_STR" -> get_env_var value |> make_key |> make_scalar |> IO.return
            | None, "!CONFIG" when state.options.enable_imports ->
              let* is_allowed = state.options.validate_config_path value in
              if is_allowed
              then get_config value
              else failwithf "YAML !CONFIG was denied for: %s" value ()
            | None, "!CONFIG" -> failwith "YAML !CONFIG imports are disabled"
            | None, _ -> failwithf "Undefined YAML tag: %s" tag () ))
          |> process_anchor original
          |> snd
      and get_config filename =
        StringTable.find_or_add state.configs filename ~default:(fun () ->
          let* contents = filename |> state.options.config_path_relative_to |> state.options.get_file in
          match yaml_of_string contents with
          | Ok partial_config -> loop partial_config
          | Error (`Msg msg) -> failwithf "Invalid YAML in !CONFIG %s: %s" filename msg () )
      in
      IO.catch
        (fun () -> loop yaml >|= Result.ok)
        (function
          | Failure msg -> IO.return (Error msg)
          | exn -> IO.return (Error (Printexc.to_string exn)))
      >|= function
      | Ok _ as res when not state.options.allow_unused_anchors -> (
        StringTable.to_seq state.refs |> Seq.find (fun (_, (_, ref_count)) -> ref_count = 0) |> function
        | None -> res
        | Some (name, _) -> Error ("YAML anchor &" ^ name ^ " is never used") )
      | Ok _ as res -> res
      | Error _ as res -> res

    let of_string ?(options = default_options) str =
      match yaml_of_string str with
      | Ok x ->
        let state =
          {
            refs = StringTable.create ();
            env_vars = StringTable.create ();
            files = StringTable.create ();
            configs = StringTable.create ();
            options;
          }
        in
        resolve_references state x
      | Error (`Msg message) -> IO.return (Error message)
  end

  let parse ?options ?validate:(json_spec = Json_spec.JAny) ~of_yojson:parser contents =
    let+ json =
      let+ res = YAML.of_string ?options contents in
      match res >>= wrap Yaml.to_json with
      | Ok x -> Ok (YAML.to_yojson x)
      | Error message -> Error [ Json_spec.Deserialization { message; attempted = Right contents } ]
    in
    json >>= fun json ->
    match Json_spec.validate json_spec json with
    | [] -> (
      match parser json with
      | Ok _ as x -> x
      | Error message -> Error [ Deserialization { message; attempted = Left json } ] )
    | errors -> Error errors
end

module Make_Lwt (Lwt : S.S_Lwt) (Lwt_io : S.S_Lwt_io with type 'a lwt_t := 'a Lwt.t) :
  Intf with type 'a io := 'a Lwt.t = struct
  include Make (struct
    include Lwt

    let map_s f l =
      let rec inner acc = function
        | [] -> List.rev acc |> Lwt.return
        | hd :: tl -> Lwt.bind (Lwt.apply f hd) (fun r -> (inner [@ocaml.tailcall]) (r :: acc) tl)
      in
      inner [] l

    let read_file filename =
      Lwt_io.with_file ~flags:Unix.[ O_RDONLY; O_NONBLOCK ] ~mode:Input filename Lwt_io.read
  end)
end

module Unix : Intf with type 'a io := 'a = Make (struct
  type +'a t = 'a

  let return x = x

  let map f x = f x

  let bind x f = f x

  let catch f catch =
    try f () with
    | exn -> catch exn

  let return_true = true

  let return_nil = []

  let map_s f ll = List.map f ll

  let read_file filename =
    let chan = In_channel.open_bin filename in
    Fun.protect (fun () -> In_channel.input_all chan) ~finally:(fun () -> In_channel.close chan)
end)
