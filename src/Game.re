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

let rec windowsOf4 = xs =>
  switch (xs) {
  | [a, b, c, d, ...tail] => [
      (a, b, c, d),
      ...windowsOf4([b, c, d, ...tail]),
    ]
  | _ => []
  };

let zip4 = ((l1, l2, l3, l4)) =>
  List.zipBy(List.zip(l1, l2), List.zip(l3, l4), ((x1, x2), (x3, x4)) =>
    (x1, x2, x3, x4)
  );

let zip4diagonalUp = ((l1, l2, l3, l4)) =>
  switch (l1, l2, l3, l4) {
  | (t1, [_, ...t2], [_, _, ...t3], [_, _, _, ...t4]) =>
    zip4((t1, t2, t3, t4))
  | _ => []
  };

let zip4diagonalDown = ((l1, l2, l3, l4)) =>
  switch (l1, l2, l3, l4) {
  | ([_, _, _, ...t1], [_, _, ...t2], [_, ...t3], t4) =>
    zip4((t1, t2, t3, t4))
  | _ => []
  };

let equal4 = ((a, b, c, d)) => a === b && b === c && c === d;

let hasWinner = board => {
  open List;
  let vertical4 = board |. map(windowsOf4) |. flatten;
  let colsRevBy4 = board |. map(reverse) |. windowsOf4;
  let horizontal4 = colsRevBy4 |. map(zip4) |. flatten;
  let diagonalUp4 = colsRevBy4 |. map(zip4diagonalUp) |. flatten;
  let diagonalDown4 = colsRevBy4 |. map(zip4diagonalDown) |. flatten;

  [|vertical4, horizontal4, diagonalUp4, diagonalDown4|]
  |. concatMany
  |. some(equal4);
};

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