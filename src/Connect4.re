open Belt;

type disc =
  | Red
  | Yellow;

type board = list(list(disc));

type game = {
  board,
  turn: disc,
  winner: option(disc),
};

let numColumns = 7;
let numLines = 6;

let nextTurn =
  fun
  | Red => Yellow
  | Yellow => Red;

/* utility functions to deal with quadruples */

let equal4 = ((a, b, c, d)) => a == b && b == c && c == d;

let zip4 = (l1, l2, l3, l4) =>
  List.(
    zipBy(zip(l1, l2), zip(l3, l4), ((e1, e2), (e3, e4)) =>
      (e1, e2, e3, e4)
    )
  );

let rec windowsOf4 =
  fun
  | [a, b, c, d, ...tail] => [
      (a, b, c, d),
      ...windowsOf4([b, c, d, ...tail]),
    ]
  | _ => [];

/* like List.drop but return an empty list if there are not enough elements */
let drop = (l, n) => l |. List.drop(n) |. Option.getWithDefault([]);

let has4ConnectedVertically = xs => xs |. windowsOf4 |. List.some(equal4);

let has4ConnectedSideBySide = ((c1, c2, c3, c4)) =>
  zip4(c1, c2, c3, c4) |. List.some(equal4);

let has4ConnectedDiagonalUp = ((c1, c2, c3, c4)) =>
  (c1, drop(c2, 1), drop(c3, 2), drop(c4, 3)) |. has4ConnectedSideBySide;

let has4ConnectedDiagonalDown = ((c1, c2, c3, c4)) =>
  (drop(c1, 3), drop(c2, 2), drop(c3, 1), c4) |. has4ConnectedSideBySide;

let has4Connected = columns =>
  List.(
    some(columns, has4ConnectedVertically)
    || map(columns, reverse)
    |. windowsOf4
    |. some(fourColumns =>
         has4ConnectedSideBySide(fourColumns)
         || has4ConnectedDiagonalUp(fourColumns)
         || has4ConnectedDiagonalDown(fourColumns)
       )
  );

let init = () => {
  board: List.make(numColumns, []),
  turn: Yellow,
  winner: None,
};

let play = ({board, turn, winner} as game, colIndex) =>
  switch (winner, List.get(board, colIndex)) {
  | (None, Some(column)) when List.length(column) < numLines =>
    let newBoard =
      List.mapWithIndex(board, (i, col) =>
        i == colIndex ? [turn, ...column] : col
      );

    {
      board: newBoard,
      turn: nextTurn(turn),
      winner: has4Connected(newBoard) ? Some(turn) : None,
    };

  | _ => game
  };