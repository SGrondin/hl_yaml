module type S_Lwt = sig
  type +'a t

  val return : 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val apply : ('a -> 'b t) -> 'a -> 'b t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  val return_true : bool t

  val return_nil : _ list t
end

module type S_Lwt_io = sig
  type +'a lwt_t

  type input

  type output

  type 'a mode =
    | Input : input mode
    | Output : output mode

  type 'a channel

  type input_channel = input channel

  val with_file :
    ?buffer:(char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    ?flags:Unix.open_flag list ->
    ?perm:int ->
    mode:'a mode ->
    string ->
    ('a channel -> 'b lwt_t) ->
    'b lwt_t

  val read : ?count:int -> input_channel -> string lwt_t
end

module type IO = sig
  type +'a t

  val return : 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  val return_true : bool t

  val return_nil : _ list t

  val map_s : ('a -> 'b t) -> 'a list -> 'b list t

  val read_file : string -> string t
end
