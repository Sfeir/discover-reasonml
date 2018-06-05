open Belt;

let map = (xs, f) =>
  xs
  |. List.mapWithIndex((i, e) => f(e, string_of_int(i)))
  |. List.toArray
  |. ReasonReact.array;

let discClass = (prefix, disc) =>
  prefix
  ++ (
    switch (disc) {
    | Game.Yellow => "yellow"
    | Game.Red => "red"
    }
  );

let component = ReasonReact.statelessComponent("App");

let make = (~game: Game.t, _children) => {
  ...component,
  render: _self =>
    <div className=(discClass("board turn-", game.turn))>
      (
        game.board
        |. map((col, key) =>
             <div className="col" key>
               (
                 col
                 |. List.reverse
                 |. map((disc, key) =>
                      <div className=(discClass("disc disc-", disc)) key />
                    )
               )
             </div>
           )
      )
    </div>,
};