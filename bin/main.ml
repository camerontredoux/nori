open Base
open Stdio
open Soup
open Lwt.Syntax
open Cohttp_lwt_unix
open Tui
open Download

let libgen_url = "https://libgen.is/fiction/?q="
let unwrap_option = function None -> failwith "unwrapped None" | Some v -> v

let query filename =
  let url = Uri.of_string (libgen_url ^ Uri.pct_encode filename) in
  let* _, body = Client.get url in
  Cohttp_lwt.Body.to_string body

let app = Minttea.app ~init ~update ~view ()

let parse_mirror col =
  let atag = col $$ ".record_mirrors_compact a" |> last in
  attribute "href" (unwrap_option atag)

let parse_col col =
  if Option.is_some (col $? ".record_mirrors_compact") then parse_mirror col
  else
    let col =
      if Option.is_some (col $? "p.catalog_identifier") then col $ "p" else col
    in
    trimmed_texts col |> String.concat ~sep:" " |> fun text ->
    if List.exists (classes col) ~f:(String.equal "catalog_edit") then None
    else Some text

let parse_rows rows = rows $$ "td" |> to_list |> List.filter_map ~f:parse_col

let scrape body =
  parse body $$ ".catalog tbody tr" |> to_list |> List.map ~f:parse_rows

let main filename =
  let body = Lwt_main.run (query filename) in
  let rows = scrape body in
  let config = Minttea.config ~fps:10 () in
  Minttea.start ~config app ~initial_model:(init_model rows)

let () =
  let argv = Sys.get_argv () in
  let args = Array.to_list argv |> List.tl_exn in
  match args with
  | [ "-u"; url; "-d"; filename ] ->
      Lwt_main.run (download (Uri.of_string url) filename)
  | [ filename ] ->
      printf "Searching for \"%s\"...\n%!" filename;
      main filename
  | _ -> eprintf "Usage: nori FILENAME"
