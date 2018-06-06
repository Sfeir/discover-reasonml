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

describe("hasWinner", () => {
  let vertical = [
    [],
    [],
    [Yellow, Yellow, Yellow, Red],
    [Red, Red, Red, Yellow],
    [],
    [],
    [],
  ];
  let horizontal = [
    [Red],
    [Yellow, Red],
    [Yellow, Yellow],
    [Yellow, Red],
    [],
    [],
    [],
  ];
  let diagonal = [
    [Red],
    [Yellow, Yellow, Red],
    [Yellow, Red, Red, Yellow],
    [Yellow, Red, Yellow, Red, Red],
    [],
    [],
    [],
  ];

  test("vertical", () => {
    let actual = play({board: vertical, turn: Some(Yellow)}, 2);
    expect(actual.turn) |> toEqual(None);
  });

  test("horizontal", () => {
    let actual = play({board: horizontal, turn: Some(Yellow)}, 0);
    expect(actual.turn) |> toEqual(None);
  });

  test("diagonal", () => {
    let actual = play({board: diagonal, turn: Some(Yellow)}, 0);
    expect(actual.turn) |> toEqual(None);
  });
});