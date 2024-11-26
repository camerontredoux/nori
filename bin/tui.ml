open Minttea
open Leaves
open Styles

type table_screen = { table : Table.t }

type download_screen = {
  choice : Text_input.t;
  confirmed : bool;
  finished : bool;
  progress : Progress.t;
  author : string;
  title : string;
  url : string;
}

type reading_screen = { text : string }

type section =
  | Table_screen of table_screen
  | Download_screen of download_screen
  | Reading_screen of reading_screen

let download_ref : unit Riot.Ref.t = Riot.Ref.make ()

type model = { section : section; rows : Table.row list }

let table_screen rows =
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
          cursor = 0;
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
      ( Download_screen
          {
            choice =
              Text_input.make "" ~placeholder:"Do you want to continue? [y/N]"
                ~cursor:in_cursor ();
            confirmed = false;
            finished = false;
            progress =
              Progress.make ~width:50
                ~color:
                  (`Gradient (Spices.color "#B14FFF", Spices.color "#00FFA3"))
                ();
            author;
            title;
            url;
          },
        Command.Noop )
  | Download_screen screen ->
      if screen.confirmed then (Reading_screen { text = "Hello!" }, Command.Noop)
      else (table_screen model.rows, Command.Noop)
  | _ -> raise Invalid_transition

let init_model rows = { section = table_screen rows; rows }
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
            | Event.KeyDown (Space, No_modifier) ->
                Tty.Terminal.clear ();
                transition model
            | _ -> (model.section, Command.Noop))
        | Reading_screen screen -> (Reading_screen screen, Command.Noop)
        | Download_screen screen -> (
            match event with
            | Event.Timer ref
              when (not screen.finished) && screen.confirmed
                   && Riot.Ref.equal ref download_ref ->
                let progress = Progress.increment screen.progress 0.1 in
                let finished = Progress.is_finished screen.progress in
                if finished then transition model
                else
                  ( Download_screen { screen with progress; finished },
                    Command.Set_timer (download_ref, 1.) )
            | Event.KeyDown (Enter, No_modifier) ->
                if String.equal (Text_input.current_text screen.choice) "y" then
                  ( Download_screen { screen with confirmed = true },
                    Command.Set_timer (download_ref, 0.1) )
                else transition model
            | e ->
                let choice = Text_input.update screen.choice e in
                (Download_screen { screen with choice }, Command.Noop))
      in
      ({ model with section }, cmd)
  with Exit ->
    Tty.Escape_seq.show_cursor_seq ();
    Riot.shutdown ();
    (model, Command.Quit)

let view_table screen =
  let help =
    subtle "up/down: move" ^ dot ^ subtle "space: choose" ^ dot
    ^ subtle "ctrl+x: quit"
  in
  Format.sprintf {|
%s

%s

|} (Table.view screen.table) help

let view_download screen =
  let help = subtle "enter: submit" ^ dot ^ subtle "ctrl+x: quit" in
  let confirmed = screen.confirmed in
  let display =
    if confirmed then Progress.view screen.progress
    else Text_input.view screen.choice
  in
  Format.sprintf {|
Downloading %s by %s
(%s)

%s

%s

|}
    (keyword "%s" screen.title)
    (subtle "%s" screen.author)
    (subtle "%s" screen.url) display help

let view model =
  match model.section with
  | Table_screen screen -> view_table screen
  | Download_screen screen -> view_download screen
  | Reading_screen screen -> screen.text ^ subtle "\n\nctrl+x: quit"
