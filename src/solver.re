let itCount = ref(0);

/* this is supposed to improve perf, but it doesn't :( */
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
  } else {
    availablePieces
    /* for each available piece */
    |> List.fold_left(
         (found, pieceTransforms) =>
           if (found) {
             true
           } else {
             let newPieces = availablePieces |> List.filter((p) => p !== pieceTransforms);
             pieceTransforms
             /* for each piece transform */
             |> List.fold_left(
                  (found, piece) =>
                    if (found) {
                      true
                    } else {
                      /* for each position in the board */
                      forEachPosition(
                        found,
                        board,
                        piece,
                        newPieces
                      )
                    },
                  found
                )
           },
         false
       )
  }
and forEachPosition = (found, board, piece, availablePieces) =>
  board
  |> Board.reduce(
       (found, x, y, value) =>
         if (found) {
           true
         } else if
           /* skip position right away if not empty */
           (value != Board.Empty) {
           false
         } else {
           /* try to insert piece at current pos */
           let ok = Board.insert(board, piece, (x, y));
           if (ok) {
             /* check if the board can already be discarded */
             let isPossible = isPossibleBoard(board, availablePieces);
             let found =
               if (! isPossible) {
                 /* Js.log("OUT!"); */
                 false
               } else {
                 /* recurse */
                 itCount := itCount^ + 1;
                 if (itCount^ mod 10000 == 0) {
                   print_endline("pieces left: " ++ string_of_int(List.length(availablePieces)))
                 } else {
                   ()
                 };
                 solve(board, availablePieces)
               };
             Board.remove(board, piece, (x, y));
             found
           } else {
             false
           }
         },
       found
     );