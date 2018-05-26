[%bs.raw {|require('./App.css')|}];

open Belt;
module RR = ReasonReact;

type state = Connect4.game;

type action =
  | Play(int)
  | Reset;

let initialState = Connect4.init;

let reducer = (action, state) =>
  switch (action) {
  | Play(column) => RR.Update(Connect4.play(state, column))
  | Reset => RR.Update(Connect4.init())
  };

let component = RR.reducerComponent("App");
let make = _children => {
  ...component,
  initialState,
  reducer,
  render: ({state: {Connect4.board, turn, winner}, send}) =>
    <div className="App">
      <Board
        board
        turn=?(Option.isNone(winner) ? Some(turn) : None)
        onPlay=(colIndex => send(Play(colIndex)))
      />
      <br />
      <button onClick=(_ => send(Reset))> (RR.string("Reset")) </button>
    </div>,
};