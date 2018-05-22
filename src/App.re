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

let getPieces = col =>
  col
  |. List.map(piece => {
       let className =
         "piece piece-"
         ++ (
           switch (piece) {
           | Connect4.Red => "red"
           | Connect4.Yellow => "yellow"
           }
         );
       <div className />;
     })
  |. List.toArray
  |. ReasonReact.array;

let getColumns = (board, onClick) =>
  board
  |. List.mapWithIndex((i, col) =>
       <div className="column" onClick=(onClick(i))> (col |. getPieces) </div>
     )
  |. List.toArray
  |. ReasonReact.array;

let make = _children => {
  ...component,
  initialState,
  reducer,
  render: ({state: {Connect4.board}, send}) =>
    <div className="App">
      <div className="board">
        (board |. getColumns((i, _) => send(Play(i))))
      </div>
      <br />
      <button onClick=((_) => send(Reset))>
        (ReasonReact.string("Reset"))
      </button>
    </div>,
};