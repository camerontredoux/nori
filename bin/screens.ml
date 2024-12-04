open Leaves

type table_screen = { table : Table.t }

type download_screen = {
  choice : Text_input.t;
  progress : Progress.t;
  spinner : Sprite.t;
  finished : bool;
  author : string;
  title : string;
  url : string;
  hash : string;
  download_link : Uri.t;
  last_cursor : int;
}

type reading_screen = { text : string }
type landing_screen = { table : Table.t }

type section =
  | Table_screen of table_screen
  | Landing_screen of landing_screen
  | Download_screen of download_screen
  | Reading_screen of reading_screen
