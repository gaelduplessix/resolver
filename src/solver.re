/* bail-out if board cannot be solved at this point */
let isPossibleBoard = (board, pieces) =>
  /* for each spot in the board */
  board
  |> Board.reduce(
       (possible, x, y, value) =>
         possible
         /* if cell is empty, check that a piece can fit here */
         && (
           value != Board.Empty
           || pieces
           |> List.fold_left(
                (possible, pieceTransforms) =>
                  possible
                  || pieceTransforms
                  /* for each piece transform */
                  |> List.fold_left(
                       (possible, piece) =>
                         possible
                         || piece
                         /* try piece at different positions */
                         |> Board.reduce(
                              (possible, offsetX, offsetY, _) =>
                                possible
                                || Board.canInsert(board, piece, (x - offsetX, y - offsetY)),
                              false
                            ),
                       false
                     ),
                false
              )
         ),
       true
     );

let rec solve = (board, availablePieces) =>
  if (List.length(availablePieces) == 0) {
    print_endline("DONE");
    print_endline(Board.to_string(board));
    true
  } else if (! isPossibleBoard(board, availablePieces)) {
    false
  } else {
    print_endline(Board.to_string(board));
    print_int(List.length(availablePieces));
    print_endline(" --------------------");
    /* for each position on the board */
    board
    |> Board.reduce(
         (found, x, y, currCell) =>
           switch currCell {
           /* cell used: ignore */
           | Board.Piece(_) => found
           | Empty =>
             /* try to insert a piece at this position */
             availablePieces
             |> List.fold_left(
                  (found, pieceTransforms) =>
                    found
                    /* for each transform */
                    || {
                      let newPieces = availablePieces |> List.filter((p) => p != pieceTransforms);
                      pieceTransforms
                      |> List.fold_left(
                           (found, piece) =>
                             found
                             /* try to offset piece */
                             || piece
                             |> Board.reduce(
                                  (found, offsetX, offsetY, _) =>
                                    found
                                    || {
                                      /* print_endline(string_of_int(List.length(newPieces))); */
                                      /* try to insert piece */
                                      let piecePos = (x - offsetX, y - offsetY);
                                      let ok = Board.insert(board, piece, piecePos);
                                      if (! ok) {
                                        false
                                      } else {
                                        /* recurse */
                                        let found = solve(board, newPieces);
                                        Board.remove(board, piece, piecePos);
                                        found
                                      }
                                    },
                                  found
                                ),
                           found
                         )
                    },
                  found
                )
           },
         false
       )
  };