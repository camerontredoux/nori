open Minttea
open Leaves

type table_screen = { table : Table.t }

type download_screen = {
  spinner : Sprite.t;
  finished : bool;
  progress : Progress.t;
}

type reading_screen = { text : string }

type section =
  | Table_screen of table_screen
  | Download_screen of download_screen
  | Reading_screen of reading_screen

let download_ref = Riot.Ref.make ()

exception Invalid_transition

let transition = function
  | Table_screen _screen ->
      ( Download_screen
          {
            finished = false;
            spinner = Spinner.globe;
            progress =
              Progress.make ~width:50
                ~color:
                  (`Gradient (Spices.color "#B14FFF", Spices.color "#00FFA3"))
                ();
          },
        Command.Noop )
  | _ -> raise Invalid_transition

type model = { section : section }

let init_model _rows =
  {
    section =
      (* Table_screen
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
         }; *)
      Download_screen
        {
          finished = false;
          spinner = Spinner.globe;
          progress =
            Progress.make ~width:50
              ~color:
                (`Gradient (Spices.color "#B14FFF", Spices.color "#00FFA3"))
              ();
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
            | Event.KeyDown (Space, No_modifier) -> transition model.section
            | _ -> (model.section, Command.Noop))
        | Reading_screen screen -> (Reading_screen screen, Command.Noop)
        | Download_screen screen -> (
            match event with
            | Event.Frame now ->
                let spinner = Sprite.update ~now screen.spinner in
                (Download_screen { screen with spinner }, Command.Noop)
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
      Table.view screen.table ^ "\n\nmove: up/down, select: space, quit: q"
  | Download_screen screen -> Progress.view screen.progress
  | _ -> "unsupported"
