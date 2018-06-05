type disc =
  | Yellow
  | Red;

type board = list(list(disc));

type t = {
  board,
  turn: option(disc),
};

let initial: unit => t;

let play: (t, int) => t;