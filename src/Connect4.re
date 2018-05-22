open Belt;

type piece =
  | Red
  | Yellow;

type board = list(list(piece));

let numColumns = 7;

let numLines = 6;

type game = {
  board,
  turn: piece,
  winner: option(piece),
};

let nextTurn =
  fun
  | Red => Yellow
  | Yellow => Red;

let window4 = xs => {
  let rec aux = (ys, windows) =>
    switch (ys) {
    | [a, b, c, d, ...t] =>
      aux([b, c, d, ...t], [(a, b, c, d), ...windows])
    | _ => windows
    };
  aux(xs, []) |. List.reverse;
};

let has4AdjacentEqual = xs =>
  xs |. window4 |. List.some(((a, b, c, d)) => a == b && b == c && c == d);

let has4ConnectedVertically = columns =>
  columns |. List.some(has4AdjacentEqual);

let init = () => {
  board: List.make(numColumns, []),
  turn: Yellow,
  winner: None,
};

let play = ({board, turn, winner} as game, colIndex) =>
  switch (winner, List.get(board, colIndex)) {
  | (None, Some(column)) when List.length(column) < numLines =>
    let newColumn = [turn, ...column];
    let newBoard =
      board |. List.mapWithIndex((i, col) => i == colIndex ? newColumn : col);
    {
      board: newBoard,
      turn: nextTurn(turn),
      winner: has4ConnectedVertically(newBoard) ? Some(turn) : None,
    };
  | _ => game
  };