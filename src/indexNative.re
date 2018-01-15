/* let pieces = Pieces.piecesSet([1, 12, 3, 11, 4, 9, 2, 7]); */
/* let pieces = Pieces.piecesSet([1, 12, 3, 11, 4, 9]); */
let pieces = Pieces.piecesSet([1, 12, 3, 11, 4, 9, 2]);

/* let pieces = Pieces.piecesSet([1, 7, 11, 3, 4, 9, 2, 6, 8, 12, 10]); */
let before = Unix.gettimeofday();

Solver.solve(Board.make(List.length(pieces)), pieces);

let after = Unix.gettimeofday();

print_endline("time: " ++ string_of_float((after -. before) *. 1000.0) ++ "ms");