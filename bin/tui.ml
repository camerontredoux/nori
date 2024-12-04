open Minttea
open Leaves
open Styles
open Download
open Screens

let download_ref = Riot.Ref.make ()

type model = { section : section; rows : Table.row list }

let table_screen rows last_cursor =
  Table_screen
    {
      table =
        {
          columns =
            [|
              { title = "Author"; width = 20 };
              { title = "Series"; width = 20 };
              { title = "Title"; width = 20 };
              { title = "Language"; width = 10 };
              { title = "File"; width = 15 };
              { title = "Link"; width = 0 };
            |];
          rows;
          cursor = last_cursor;
          styles = Table.default_styles;
          start_of_frame = 0;
          height_of_frame = 5;
        };
    }

exception Invalid_transition

let transition (model : model) =
  match model.section with
  | Table_screen screen ->
      let cursor = screen.table.cursor in
      let row = List.nth screen.table.rows cursor in
      let author = List.nth row 0 in
      let title = List.nth row 2 in
      let url = List.nth row 5 in
      Tty.Terminal.clear ();
      let download_link = parse_download url in
      let hash =
        match Uri.get_query_param (Uri.of_string url) "md5" with
        | Some hash -> hash
        | None -> failwith "url missing hash"
      in
      ( Download_screen
          {
            choice =
              Text_input.make "" ~placeholder:"Continue? [y/n]"
                ~cursor:in_cursor ();
            progress =
              Progress.make ~width:50
                ~color:
                  (`Gradient (Spices.color "#B14FFF", Spices.color "#00FFA3"))
                ();
            spinner = Spinner.dot;
            finished = false;
            author;
            title;
            url;
            hash;
            download_link;
            last_cursor = cursor;
          },
        Command.Noop )
  | Download_screen screen ->
      if screen.finished then
        (Reading_screen { text = "Downloaded" }, Command.Noop)
      else (table_screen model.rows screen.last_cursor, Command.Noop)
  | _ -> raise Invalid_transition

let init_model rows = { section = table_screen rows 0; rows }
let init _ = Command.(Seq [ Hide_cursor; Enter_alt_screen ])

let update event model =
  try
    if event = Event.KeyDown (Key "x", Ctrl) then raise Exit
    else
      let section, cmd =
        match model.section with
        | Table_screen screen -> (
            match event with
            | Event.KeyDown (Up, No_modifier) ->
                ( Table_screen { table = Table.update screen.table Table.Up },
                  Command.Noop )
            | Event.KeyDown (Down, No_modifier) ->
                ( Table_screen { table = Table.update screen.table Table.Down },
                  Command.Noop )
            | Event.KeyDown (Enter, No_modifier) ->
                (* let _ =
                     Riot.spawn (fun () -> Lwt.async (fun () -> parse_download ""))
                   in *)
                transition model
            | _ -> (model.section, Command.Noop))
        | Download_screen screen -> (
            match event with
            (* | Event.Timer ref
               when (not screen.finished) && screen.confirmed
                    && Riot.Ref.equal ref download_ref ->
                 let progress = Progress.increment screen.progress 0.1 in
                 let finished = Progress.is_finished screen.progress in
                 if finished then transition model
                 else
                   ( Download_screen { screen with progress; finished },
                     Command.Set_timer (download_ref, 0.3) ) *)
            | Event.Timer ref
              when screen.finished && Riot.Ref.equal ref download_ref ->
                transition model
            | Event.KeyDown (Enter, No_modifier) ->
                if String.equal (Text_input.current_text screen.choice) "y" then (
                  let filename =
                    Base.String.substr_replace_all
                      (screen.title ^ " - " ^ screen.author ^ "(" ^ screen.hash
                     ^ ").epub")
                      ~pattern:"/" ~with_:"\\"
                  in
                  Lwt_main.run (download screen.download_link filename);
                  ( Download_screen { screen with finished = true },
                    Command.Set_timer (download_ref, 0.5) ))
                else transition model
            | e ->
                let choice = Text_input.update screen.choice e in
                (Download_screen { screen with choice }, Command.Noop))
        | Reading_screen screen -> (Reading_screen screen, Command.Noop)
        | Landing_screen screen -> (Landing_screen screen, Command.Noop)
      in

      ({ model with section }, cmd)
  with Exit ->
    Utils.quit ();
    (model, Command.Quit)

let view_table (screen : table_screen) =
  let help =
    subtle "up/down: move" ^ dot ^ subtle "enter: choose" ^ dot
    ^ subtle "ctrl+x: quit"
  in
  Format.sprintf {|
%s

%s

|} (Table.view screen.table) help

let view_download screen =
  let help = subtle "enter: submit" ^ dot ^ subtle "ctrl+x: quit" in
  let display =
    if screen.finished then "> Done" else Text_input.view screen.choice
  in
  Format.sprintf {|
Downloading %s by %s
Direct Link: %s

%s

%s
|}
    (keyword "%s" screen.title)
    (bold "%s" screen.author)
    (subtle "%s" (Uri.to_string screen.download_link))
    display help

let view model =
  match model.section with
  | Table_screen screen -> view_table screen
  | Landing_screen screen -> Table.view screen.table
  | Download_screen screen -> view_download screen
  | Reading_screen screen -> screen.text ^ subtle "\n\nctrl+x: quit"
