type disc =
  | Red
  | Yellow;

type board = list(list(disc));

type game = {
  board,
  turn: disc,
  winner: option(disc),
};

let init: unit => game;

let play: (game, int) => game;