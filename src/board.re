type boardCell =
  /* board cell is either empty */
  | Empty
  /* or contains a piece (index) */
  | Piece(int);

let make = (height) => Array.make_matrix(height, 5, Empty);

/* "reduces" a function over all rows/columns of a board/piece */
let reduce = (f, init, board) => {
  let acc = ref(init);
  for (y in 0 to Array.length(board) - 1) {
    for (x in 0 to Array.length(board[y]) - 1) {
      acc := f(acc^, x, y, board[y][x])
    }
  };
  acc^
};

/* check that there is room for a piece in the board */
let canInsert = (board, piece, (posX, posY)) =>
  if (posX < 0 || posY < 0) {
    false
  } else {
    let boardHeight = Array.length(board);
    let boardWidth = Array.length(board[0]);
    piece
    |> reduce(
         (ok, x, y, currVal) =>
           /* check piece fits in board limit and that value at this pos is 0 */
           if (! ok) {
             false
           } else if (y + posY >= boardHeight) {
             false
           } else if (x + posX >= boardWidth) {
             false
           } else if (currVal != 0 && board[y + posY][x + posX] != Empty) {
             false
           } else {
             true
           },
         true
       )
  };

let insert = (board, piece, (posX, posY)) => {
  let ok = canInsert(board, piece, (posX, posY));
  if (! ok) {
    false
  } else {
    /* if ok, actually insert piece in board */
    piece
    |> reduce(
         (_, x, y, currVal) =>
           /* insert piece */
           if (currVal != 0) {
             board[y + posY][x + posX] = Piece(currVal)
           } else {
             ()
           },
         ()
       );
    true
  }
};

let remove = (board, piece, (posX, posY)) =>
  piece
  |> reduce(
       (_, x, y, currVal) =>
         /* remove piece */
         if (currVal != 0) {
           board[y + posY][x + posX] = Empty
         } else {
           ()
         },
       ()
     );

let print = (board) =>
  board
  |> Array.map(
       (row) =>
         row
         |> Array.map(
              (cell) => {
                let s =
                  switch cell {
                  | Empty => "  "
                  | Piece(id) => string_of_int(id)
                  };
                if (String.length(s) < 2) {
                  "0" ++ s
                } else {
                  s
                }
              }
            )
     );