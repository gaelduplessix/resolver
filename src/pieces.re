/* from http://umtauber.org/ipd/2012/team2/12pieces.png */
let basePieces =
  [
    [
      /* 1 - Orange */
      [1, 0, 0, 0, 0],
      [1, 0, 0, 0, 0],
      [1, 0, 0, 0, 0],
      [1, 1, 0, 0, 0],
      [0, 0, 0, 0, 0]
    ],
    [
      /* 2 - Light blue */
      [2, 0, 0, 0, 0],
      [2, 0, 0, 0, 0],
      [2, 2, 2, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0]
    ],
    [
      /* 3 - Green */
      [3, 3, 3, 0, 0],
      [0, 3, 0, 0, 0],
      [0, 3, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0]
    ],
    [
      /* 4 - Light green */
      [0, 0, 4, 0, 0],
      [0, 4, 4, 0, 0],
      [4, 4, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0]
    ],
    [
      /* 5 - Red */
      [0, 5, 0, 0, 0],
      [5, 5, 5, 0, 0],
      [0, 5, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0]
    ],
    [
      /* 6 - Blue */
      [6, 0, 0, 0, 0],
      [6, 0, 0, 0, 0],
      [6, 0, 0, 0, 0],
      [6, 0, 0, 0, 0],
      [6, 0, 0, 0, 0]
    ],
    [
      /* 7 - Dark blue */
      [0, 7, 0, 0, 0],
      [7, 7, 0, 0, 0],
      [7, 0, 0, 0, 0],
      [7, 0, 0, 0, 0],
      [0, 0, 0, 0, 0]
    ],
    [
      /* 8 - Yellow */
      [8, 0, 8, 0, 0],
      [8, 8, 8, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0]
    ],
    [
      /* 9 - Cyan */
      [9, 9, 0, 0, 0],
      [0, 9, 0, 0, 0],
      [0, 9, 9, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0]
    ],
    [
      /* 10 - Gray */
      [00, 10, 10, 00, 00],
      [10, 10, 00, 00, 00],
      [00, 10, 00, 00, 00],
      [00, 00, 00, 00, 00],
      [00, 00, 00, 00, 00]
    ],
    [
      /* 11 - Pink */
      [11, 11, 00, 00, 00],
      [11, 11, 00, 00, 00],
      [11, 00, 00, 00, 00],
      [00, 00, 00, 00, 00],
      [00, 00, 00, 00, 00]
    ],
    [
      /* 12 - Brown */
      [00, 12, 00, 00, 00],
      [12, 12, 00, 00, 00],
      [00, 12, 00, 00, 00],
      [00, 12, 00, 00, 00],
      [00, 00, 00, 00, 00]
    ]
  ]
  |> List.map((piece) => piece |> List.map(Array.of_list) |> Array.of_list)
  |> Array.of_list;

let string_of_piece = (piece) =>
  piece
  |> Array.map((row) => row |> Array.map(string_of_int) |> Array.to_list |> String.concat(","))
  |> Array.to_list
  |> String.concat("|");

/* Transform functions */
/* identity utility */
let identity = (piece) => piece;

/* matrix transpose */
let transpose = (piece) =>
  piece |> Array.mapi((y, _) => piece[y] |> Array.mapi((x, _) => piece[x][y]));

/* Symmetry on X axis */
let symX = (piece) =>
  piece
  |> Array.mapi(
       (y, _) => piece[y] |> Array.mapi((x, _) => piece[y][Array.length(piece[y]) - 1 - x])
     );

/* Symmetry on Y axis */
let symY = (piece) =>
  piece
  |> Array.mapi((y, _) => piece[y] |> Array.mapi((x, _) => piece[Array.length(piece) - 1 - y][x]));

/* Rotate 90, courtesy of https://stackoverflow.com/a/8664879 */
let rotate90 = (piece) => piece |> transpose |> symX;

let rotate180 = (piece) => piece |> rotate90 |> rotate90;

let rotate270 = (piece) => piece |> rotate180 |> rotate90;

/* combinations of all possible transforms for a piece (generates duplicates) */
let allTransforms =
  [identity, rotate90, rotate180, rotate270]
  |> List.map((t1) => [identity, symX, symY] |> List.map((t2, p) => p |> t1 |> t2))
  |> List.flatten;

let generateTransformsWithDuplicates = (piece) => allTransforms |> List.map((t) => t(piece));

let dedupePieces = (pieces) => {
  let existing = Hashtbl.create(List.length(pieces));
  pieces
  |> List.filter(
       (piece) => {
         let str = string_of_piece(piece);
         let currentExists = Hashtbl.mem(existing, str);
         if (currentExists) {
           false
         } else {
           Hashtbl.add(existing, str, true);
           true
         }
       }
     )
};

/* trim empty rows and columns at top left of a piece */
let rec trimPiece = (piece) => {
  let emptyFirstRow = (piece) =>
    Array.fold_left(
      (acc, curr) =>
        switch curr {
        | 0 => true && acc
        | _ => false
        },
      true,
      piece[0]
    );
  let emptyFirstColumn = (piece) =>
    Array.fold_left(
      (acc, curr) =>
        switch curr[0] {
        | 0 => true && acc
        | _ => false
        },
      true,
      piece
    );
  let trimFirst = (piece) => Array.sub(piece, 1, Array.length(piece) - 1);
  let trimFirstColumn = (piece) => Array.map(trimFirst, piece);
  if (emptyFirstRow(piece)) {
    trimPiece(trimFirst(piece))
  } else if (emptyFirstColumn(piece)) {
    trimPiece(trimFirstColumn(piece))
  } else {
    piece
  }
};

let trimPieces = (pieces) =>
  pieces
  |> List.map
       /* trim piece from all sides, by applying symmetries then reversing them */
       (
         (piece) =>
           piece
           /* top left trim */
           |> trimPiece
           |> symX
           /* right trim */
           |> trimPiece
           |> symX
           /* bottom trim */
           |> symY
           |> trimPiece
           |> symY
       );

let generateTransforms = (piece) =>
  piece |> generateTransformsWithDuplicates |> trimPieces |> dedupePieces;

let pieces = basePieces |> Array.map((piece) => piece |> generateTransforms);

/* piecesSet returns a list of pieces (with their transform)
   given a list of indexes */
let piecesSet = (ids) => ids |> List.map((id) => pieces[id - 1]);