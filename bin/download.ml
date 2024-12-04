open Lwt.Syntax
open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Soup

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
        match Base.Sys.getenv "HOME" with
        | Some home -> home ^ "/.local/share/nori/"
        | None -> failwith "HOME not set, aborting."
      in
      Lwt_unix.mkdir dir 0o755 >>= fun () -> Lwt.return (dir ^ filename))
    (function
      | Unix.Unix_error (Unix.EEXIST, _, dir) -> Lwt.return (dir ^ filename)
      | Unix.Unix_error (Unix.ENOENT, _, dir) ->
          create_xdg_data_home dir >>= fun () -> get_or_create_dir filename
      | exn -> Lwt.fail exn)

let draw_progress_bar current total width =
  let percentage =
    if total = 0 then 0.
    else float_of_int current /. float_of_int total *. 100.0
  in
  let completed = int_of_float (percentage *. float_of_int width /. 100.0) in
  let progress =
    String.make completed '=' ^ String.make (width - completed) ' '
  in
  Stdio.printf "\r[%s] %.1f%% (%d/%d bytes)%!" progress percentage current total

let download uri filename =
  draw_progress_bar 0 0 50;
  let rec download_aux uri filename redirects =
    if redirects > 5 then failwith "too many redirects"
    else
      let* res, body = Client.get uri in
      let status = Response.status res in
      let headers = Response.headers res in
      match status with
      | `OK ->
          let content_length =
            match Header.get headers "content-length" with
            | Some len -> int_of_string len
            | None -> failwith "content-length not defined"
          in
          let stream = Cohttp_lwt.Body.to_stream body in
          let* dest = get_or_create_dir filename in
          let* output = Lwt_io.open_file ~mode:Lwt_io.output dest in
          let rec download_chunk current_bytes =
            let* chunk = Lwt_stream.get stream in
            match chunk with
            | Some data ->
                let* () = Lwt_io.write output data in
                let bytes = current_bytes + String.length data in
                if content_length > 0 then
                  draw_progress_bar bytes content_length 50;
                download_chunk bytes
            | None ->
                let* () = Lwt_io.close output in
                if content_length > 0 then
                  draw_progress_bar content_length content_length 50;
                Lwt.return_unit
          in
          download_chunk 0
      | `Temporary_redirect -> (
          match Header.get headers "location" with
          | Some v -> download_aux (Uri.of_string v) filename (redirects + 1)
          | None -> failwith "Unknown header: location")
      | _ ->
          failwith
            ("Code: "
            ^ Code.string_of_status status
            ^ "\n" ^ Header.to_string headers)
  in
  download_aux uri filename 0

let parse_download url =
  Stdio.printf "\nRetrieving download link...%!";
  let uri = Uri.of_string url in
  let body =
    Lwt_main.run
      (let* _, body = Client.get uri in
       Cohttp_lwt.Body.to_string body)
  in
  let endpoint =
    parse body $ "table tr a" |> attribute "href" |> Utils.unwrap_option
  in
  let host =
    match Uri.host uri with
    | Some host ->
        let scheme = Option.value ~default:"https" (Uri.scheme uri) in
        Uri.make ~scheme ~host ()
    | None -> failwith "Invalid URI, no host found."
  in
  (* send pid (Download_finished (Uri.to_string host ^ "/" ^ endpoint)) *)
  Uri.of_string (Uri.to_string host ^ "/" ^ endpoint)
