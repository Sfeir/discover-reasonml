[%bs.raw {|require('./Board.css')|}];

open Belt;
module RR = ReasonReact;

let discColor =
  fun
  | Connect4.Red => "red"
  | Connect4.Yellow => "yellow";

let listToElement = l => l |. List.toArray |. RR.array;

let getDiscs = col =>
  List.mapWithIndex(col, (i, disc) =>
    <div
      className=("disc disc-" ++ discColor(disc))
      key=(string_of_int(i))
    />
  );

let getColumns = (board, onPlay) =>
  List.mapWithIndex(board, (i, col) =>
    <div className="column" onClick=(_ => onPlay(i)) key=(string_of_int(i))>
      (col |. List.reverse |. getDiscs |. listToElement)
    </div>
  );

let component = RR.statelessComponent("Board");
let make = (~board, ~turn=?, ~onPlay, _children) => {
  ...component,
  render: _self =>
    <div
      className=(
        "board turn-"
        ++ Option.(turn |. map(discColor) |. getWithDefault("none"))
      )>
      (board |. getColumns(onPlay) |. listToElement)
    </div>,
};