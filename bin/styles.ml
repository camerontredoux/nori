open Leaves

let mint = Spices.color "#77e5b7"
let white = Spices.color "#FFFFFF"

let in_cursor =
  Cursor.make ~style:Spices.(default |> bg mint |> fg white |> bold true) ()

let dot = Spices.(default |> fg (color "236") |> build) " • "
let keyword fmt = Spices.(default |> fg (color "211") |> build) fmt
let subtle fmt = Spices.(default |> fg (color "241") |> build) fmt
let bold fmt = Spices.(default |> bold true |> build) fmt
