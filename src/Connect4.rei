type piece =
  | Red
  | Yellow;

type board = list(list(piece));

type game = {
  board,
  turn: piece,
  winner: option(piece),
};

let init: unit => game;

let play: (game, int) => game;