open Belt;

type disc =
  | Yellow
  | Red;

type board = list(list(disc));

type t = {
  board,
  turn: option(disc),
};

let numCols = 7;
let numRows = 6;

let initial = () => {board: List.make(numCols, []), turn: Some(Yellow)};

let replaceAtIndex = (xs, index, newVal) =>
  xs |. List.mapWithIndex((i, x) => i === index ? newVal : x);

let hasWinner = _board => false;

let play = (game, colIndex) =>
  switch (game.turn, List.get(game.board, colIndex)) {
  | (Some(player), Some(col)) when List.length(col) < numRows =>
    let newBoard = game.board |. replaceAtIndex(colIndex, [player, ...col]);
    let newTurn =
      if (hasWinner(newBoard)) {
        None;
      } else {
        Some(player === Yellow ? Red : Yellow);
      };

    {board: newBoard, turn: newTurn};

  | _ => game
  };