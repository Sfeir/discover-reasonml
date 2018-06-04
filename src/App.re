open Belt;

let map = (xs, f) =>
  xs
  |. List.mapWithIndex((i, e) => f(e, string_of_int(i)))
  |. List.toArray
  |. ReasonReact.array;

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
                      <div className=("disc disc-" ++ disc) key />
                    )
               )
             </div>
           )
      )
    </div>,
};