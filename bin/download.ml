open Base
open Stdio
open Lwt.Syntax
open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

let rec create_xdg_data_home path =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir path 0o755)
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
          let parent = Stdlib.Filename.dirname path in
          if String.equal parent path then Lwt.fail (Failure "Invalid path")
          else
            create_xdg_data_home parent >>= fun () -> Lwt_unix.mkdir path 0o755
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
      | exn -> Lwt.fail exn)

let rec get_or_create_dir filename =
  Lwt.catch
    (fun () ->
      let dir =
        match Sys.getenv "HOME" with
        | Some home -> home ^ "/.local/share/nori/"
        | None -> failwith "HOME not set, aborting."
      in
      Lwt_unix.mkdir dir 0o755 >>= fun () -> Lwt.return (dir ^ filename))
    (function
      | Unix.Unix_error (Unix.EEXIST, _, dir) -> Lwt.return (dir ^ filename)
      | Unix.Unix_error (Unix.ENOENT, _, dir) ->
          create_xdg_data_home dir >>= fun () -> get_or_create_dir filename
      | exn -> Lwt.fail exn)

let rec download uri filename =
  Lwt.catch
    (fun () ->
      let* res, body = Client.get uri in
      let status = Response.status res in
      let headers = Response.headers res in
      match status with
      | `OK ->
          let stream = Cohttp_lwt.Body.to_stream body in
          get_or_create_dir filename >>= fun dest ->
          Lwt_io.with_file ~mode:Lwt_io.output dest (fun chan ->
              Lwt_stream.iter_s (Lwt_io.write chan) stream)
      | `Temporary_redirect -> (
          match Header.get headers "location" with
          | Some v -> download (Uri.of_string v) filename
          | None ->
              Lwt.return
                (printf "Unknown header: location\n%s"
                   (Header.to_string headers)))
      | _ ->
          Lwt.return
            (printf "Code: %s\n%s"
               (Code.string_of_status status)
               (Header.to_string headers)))
    (fun exn -> Lwt.fail exn)
