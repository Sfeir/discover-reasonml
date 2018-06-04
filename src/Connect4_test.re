open Jest;
open Expect;

describe("Connect4", () =>
  test("init", () => {
    open Connect4;

    let expected = {
      board: [[], [], [], [], [], [], []],
      turn: Yellow,
      winner: None,
    };

    expect(init()) |> toEqual(expected);
  })
);