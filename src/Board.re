open Belt;

[%bs.raw {|require('./Board.css')|}];

let list2el = l => l |. List.toArray |. ReasonReact.array;

let discColor =
  fun
  | Connect4.Red => "red"
  | Connect4.Yellow => "yellow";

let component = ReasonReact.statelessComponent("Board");

let getDiscs = col =>
  List.mapWithIndex(col, (i, disc) =>
    <div
      className=("disc disc-" ++ discColor(disc))
      key=(string_of_int(i))
    />
  );

let getColumns = (board, onPlay) =>
  List.mapWithIndex(board, (i, col) =>
    <div
      className="column" onClick=((_) => onPlay(i)) key=(string_of_int(i))>
      (List.reverse(col) |. getDiscs |. list2el)
    </div>
  );

let make = (~board, ~turn=?, ~onPlay, _children) => {
  ...component,
  render: _self =>
    <div
      className=(
        "board turn-"
        ++ (Option.map(turn, discColor) |. Option.getWithDefault("none"))
      )>
      (getColumns(board, onPlay) |. list2el)
    </div>,
};