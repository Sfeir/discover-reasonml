open Jest;
open Expect;
open Game;

let emptyGame = {board: [[], [], [], [], [], [], []], turn: Some(Yellow)};

let startedGame = {
  board: [[], [], [], [Yellow], [], [], []],
  turn: Some(Red),
};

let gameState2 = {
  board: [[], [], [], [Red, Yellow], [], [], []],
  turn: Some(Yellow),
};

let gameStateFull = {
  board: [[], [], [], [Red, Yellow, Red, Yellow, Red, Yellow], [], [], []],
  turn: Some(Yellow),
};

test("initial", () =>
  expect(initial()) |> toEqual(emptyGame)
);

test("one turn", () => {
  let afterOneTurn = play(emptyGame, 3);
  expect(afterOneTurn) |> toEqual(startedGame);
});

test("add new disc to head of column", () => {
  let afterTwoTurns = play(startedGame, 3);
  expect(afterTwoTurns) |> toEqual(gameState2);
});

test("out of range", () => {
  let afterIllegal = play(startedGame, 10);
  expect(afterIllegal) |> toBe(startedGame);
});

test("full column", () => {
  let afterFull = play(gameStateFull, 3);
  expect(afterFull) |> toBe(gameStateFull);
});