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

let equal4 = ((a, b, c, d)) => a == b && b == c && c == d;

let zip4 = (l1, l2, l3, l4) =>
  List.(
    zipBy(zip(l1, l2), zip(l3, l4), ((e1, e2), (e3, e4)) =>
      (e1, e2, e3, e4)
    )
  );

let window4 = xs => {
  let rec aux = (ys, windows) =>
    switch (ys) {
    | [a, b, c, d, ...t] =>
      aux([b, c, d, ...t], [(a, b, c, d), ...windows])
    | _ => windows
    };
  aux(xs, []) |. List.reverse;
};

let drop = (l, n) => List.drop(l, n) |. Option.getWithDefault([]);

let has4ConnectedVertically = xs => xs |. window4 |. List.some(equal4);

let has4ConnectedSideBySide = ((c1, c2, c3, c4)) =>
  zip4(c1, c2, c3, c4) |. List.some(equal4);

let has4ConnectedDiagonalUp = ((c1, c2, c3, c4)) =>
  (c1, drop(c2, 1), drop(c3, 2), drop(c4, 3)) |. has4ConnectedSideBySide;

let has4ConnectedDiagonalDown = ((c1, c2, c3, c4)) =>
  (drop(c1, 3), drop(c2, 2), drop(c3, 1), c4) |. has4ConnectedSideBySide;

let has4Connected = columns =>
  List.some(columns, has4ConnectedVertically)
  || List.map(columns, List.reverse)
  |. window4
  |. (
    fourConsecutiveColumns =>
      List.some(fourConsecutiveColumns, has4ConnectedSideBySide)
      || List.some(fourConsecutiveColumns, has4ConnectedDiagonalUp)
      || List.some(fourConsecutiveColumns, has4ConnectedDiagonalDown)
  );

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
      winner: has4Connected(newBoard) ? Some(turn) : None,
    };
  | _ => game
  };