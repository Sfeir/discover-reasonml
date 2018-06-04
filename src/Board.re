open Belt;

let map = (xs, f) =>
  xs
  |. List.mapWithIndex((i, e) => f(e, i))
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

let component = ReasonReact.statelessComponent("Board");

let make = (~game: Game.t, ~onPlay, _children) => {
  ...component,
  render: _self =>
    <div className=(discClass("board turn-", game.turn))>
      (
        game.board
        |. map((col, i) =>
             <div
               className="col"
               key=(string_of_int(i))
               onClick=(_ => onPlay(i))>
               (
                 col
                 |. List.reverse
                 |. map((disc, j) =>
                      <div
                        className=(discClass("disc disc-", disc))
                        key=(string_of_int(j))
                      />
                    )
               )
             </div>
           )
      )
    </div>,
};