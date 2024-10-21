open! Base
open! Eio.Std
module Y = Hl_yaml.Make_Eio (Eio)

let fail msg =
  Stdlib.prerr_endline msg;
  Stdlib.exit 1

let process target =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let raw = Eio.Path.(load (fs / target)) in
  let dirname = Stdlib.Filename.dirname target in
  let cwd = Eio.Path.(fs / dirname) in
  let parsed = Y.YAML.of_string ~cwd raw |> Y.ok_or_raise in
  Eio.Flow.copy_string (Y.YAML.to_string parsed |> Y.ok_or_raise) (Eio.Stdenv.stdout env)

let () =
  let target =
    let argv = Sys.get_argv () in
    if Array.length argv < 2 then fail "Expected the first argument to be a YAML file" else argv.(1)
  in
  try process target with
  | Failure msg -> fail msg
  | Eio.Io _ as exn -> Stdlib.Format.asprintf "%a" Eio.Exn.pp exn |> fail
  | exn -> Exn.to_string exn |> fail
