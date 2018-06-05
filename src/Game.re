open Belt;

type disc =
  | Yellow
  | Red;

type board = list(list(disc));

type t = {
  board,
  turn: disc,
};

let numCols = 7;
let numRows = 6;

let initial = () => {board: List.make(numCols, []), turn: Yellow};

let replaceAtIndex = (xs, index, newVal) =>
  xs |. List.mapWithIndex((i, x) => i === index ? newVal : x);

let play = (game, colIndex) =>
  switch (List.get(game.board, colIndex)) {
  | Some(col) when List.length(col) < numRows =>
    let newBoard =
      game.board |. replaceAtIndex(colIndex, [game.turn, ...col]);
    let newTurn = game.turn === Yellow ? Red : Yellow;

    {board: newBoard, turn: newTurn};

  | _ => game
  };