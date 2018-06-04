open Belt;

let map = (xs, f) =>
  xs
  |. List.mapWithIndex((i, e) => f(e, string_of_int(i)))
  |. List.toArray
  |. ReasonReact.array;

let discClass = disc =>
  "disc disc-"
  ++ (
    switch (disc) {
    | Game.Yellow => "yellow"
    | Game.Red => "red"
    }
  );

let component = ReasonReact.statelessComponent("App");

let make = (~board, _children) => {
  ...component,
  render: _self =>
    <div className="board">
      (
        board
        |. map((col, key) =>
             <div className="col" key>
               (
                 col
                 |. List.reverse
                 |. map((disc, key) =>
                      <div className=(discClass(disc)) key />
                    )
               )
             </div>
           )
      )
    </div>,
};