let unwrap_option = function None -> failwith "unwrapped None" | Some v -> v

let quit () =
  Tty.Escape_seq.show_cursor_seq ();
  Tty.Escape_seq.exit_alt_screen_seq ();
  Riot.shutdown ()

let clean () =
  let rec rmrf path =
    match Sys.is_directory path with
    | true ->
        Sys.readdir path
        |> Array.iter (fun name -> rmrf (Filename.concat path name));
        Unix.rmdir path
    | false -> Sys.remove path
  in
  let dir =
    match Base.Sys.getenv "HOME" with
    | Some home -> home ^ "/.local/share/nori"
    | None -> failwith "HOME not set, aborting."
  in
  rmrf dir
