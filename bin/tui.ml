open Minttea
open Leaves

let dot = Spices.(default |> fg (color "236") |> build) " • "
let keyword fmt = Spices.(default |> fg (color "211") |> build) fmt
let subtle fmt = Spices.(default |> fg (color "241") |> build) fmt

type table_screen = { table : Table.t }

type download_screen = {
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

exception Invalid_transition

let transition = function
  | Table_screen screen ->
      let cursor = screen.table.cursor in
      let row = List.nth screen.table.rows cursor in
      let author = List.nth row 0 in
      let title = List.nth row 2 in
      let url = List.nth row 5 in
      ( Download_screen
          {
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
        Command.Set_timer (download_ref, 0.1) )
  | Download_screen _screen -> (Reading_screen { text = "Hello!" }, Command.Noop)
  | _ -> raise Invalid_transition

type model = { section : section }

let init_model rows =
  {
    section =
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
        };
  }

let init _ = Command.(Seq [ Hide_cursor; Enter_alt_screen ])

let update event model =
  try
    if event = Event.KeyDown (Key "q", No_modifier) then raise Exit
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
                transition model.section
            | _ -> (model.section, Command.Noop))
        | Reading_screen screen -> (Reading_screen screen, Command.Noop)
        | Download_screen screen -> (
            match event with
            | Event.Timer ref
              when (not screen.finished) && Riot.Ref.equal ref download_ref ->
                let progress = Progress.increment screen.progress 0.1 in
                let finished = Progress.is_finished screen.progress in
                if finished then transition model.section
                else
                  ( Download_screen { screen with progress; finished },
                    Command.Set_timer (download_ref, 1.) )
            | _ -> (model.section, Command.Noop))
      in
      ({ section }, cmd)
  with Exit ->
    Tty.Escape_seq.show_cursor_seq ();
    Riot.shutdown ();
    (model, Command.Quit)

let view model =
  match model.section with
  | Table_screen screen ->
      let help =
        subtle "up/down: move" ^ dot ^ subtle "space: choose" ^ dot
        ^ subtle "q: quit"
      in
      Format.sprintf {|
%s

%s

|} (Table.view screen.table) help
  | Download_screen screen ->
      Format.sprintf {|
Downloading %s by %s
(%s)

%s

|}
        (keyword "%s" screen.title)
        (subtle "%s" screen.author)
        (subtle "%s" screen.url)
        (Progress.view screen.progress)
  | Reading_screen screen -> screen.text ^ subtle "\n\nquit" ^ dot ^ subtle "q"
