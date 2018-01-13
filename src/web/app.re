let component = ReasonReact.statelessComponent("App");

let make = (_children) => {
  ...component,
  render: (_self) => <div className="App"> (ReasonReact.stringToElement("Hello world")) </div>
};

[%%bs.raw {| const before = performance.now(); |}];

let pieces = Pieces.piecesSet([1, 12, 3, 11, 4]);

Js.log(Solver.solve(Board.make(List.length(pieces)), pieces));

[%%bs.raw {|
  const after = performance.now();
  console.log('time:', after - before, 'ms');
|}];