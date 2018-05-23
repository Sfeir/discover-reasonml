open Belt;

[%bs.raw {|require('./App.css')|}];

type state = Connect4.game;

type action =
  | Play(int)
  | Reset;

let initialState = Connect4.init;

let reducer = (action, state) =>
  switch (action) {
  | Play(column) => ReasonReact.Update(Connect4.play(state, column))
  | Reset => ReasonReact.Update(Connect4.init())
  };

let component = ReasonReact.reducerComponent("App");

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
      <button onClick=((_) => send(Reset))>
        (ReasonReact.string("Reset"))
      </button>
    </div>,
};