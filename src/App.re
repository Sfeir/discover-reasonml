type state = Game.t;

let initialState = Game.initial;

type action =
  | Play(int)
  | Reset;

let reducer = (action, state) =>
  switch (action) {
  | Play(colIndex) => ReasonReact.Update(Game.play(state, colIndex))
  | Reset => ReasonReact.Update(Game.initial())
  };

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState,
  reducer,
  render: ({state, send}) =>
    <div className="game">
      <Board game=state onPlay=(colIndex => send(Play(colIndex))) />
      <button onClick=(_ => send(Reset))>
        (ReasonReact.string("Restart"))
      </button>
    </div>,
};