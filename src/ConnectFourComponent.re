Webpack.import("./connectFour.css");

open ConnectFourTools;

type actionT =
  | Click(int)
  | Restart;

type stateT = {
  gameBoard: gameBoardT,
  currentPlayer: cellT,
  winner: cellT
};

let displayBoard = (gameBoard, send) => {
  let domBoard =
    gameBoard.board
    |> Array.to_list
    |> List.rev
    |> List.map(row => {
         let rowElements =
           row
           |> Array.mapi((x, cell) =>
                <div
                  className=(
                    "board__row__cell board__row__cell--" ++ cellToString(cell)
                  )
                  onClick=(
                    (_) =>
                      if (cell === Empty) {
                        send(Click(x));
                      }
                  )
                />
              );
         ReasonReact.createDomElement(
           "div",
           ~props={"className": "board__row"},
           rowElements
         );
       })
    |> Array.of_list;
  ReasonReact.createDomElement("div", ~props={"className": "board"}, domBoard);
};

let component = ReasonReact.reducerComponent("ConnectFour");

let make = _children => {
  ...component,
  initialState: () => {
    gameBoard: {
      board: Array.make_matrix(6, 7, Empty),
      boardX: nbCellX,
      boardY: nbCellY
    },
    currentPlayer: Red,
    winner: Empty
  },
  reducer: (action, state: stateT) =>
    switch action {
    | Click(x) =>
      switch state.winner {
      | Empty =>
        let y = getMoveY(state.gameBoard, x);
        state.gameBoard.board[y][x] = state.currentPlayer;
        ReasonReact.Update({
          ...state,
          winner:
            isWinning(state.gameBoard, (x, y)) ? state.currentPlayer : Empty,
          currentPlayer: state.currentPlayer === Red ? Yellow : Red
        });
      | _ => ReasonReact.NoUpdate
      }
    | Restart =>
      ReasonReact.Update({
        gameBoard: {
          board: Array.make_matrix(6, 7, Empty),
          boardX: nbCellX,
          boardY: nbCellY
        },
        currentPlayer: Red,
        winner: Empty
      })
    },
  render: ({state, send}) =>
    <div>
      (displayBoard(state.gameBoard, send))
      (
        if (state.winner !== Empty) {
          <div>
            (ReasonReact.string("Winner : " ++ cellToString(state.winner)))
            <br />
            <button onClick=((_) => send(Restart))>
              (ReasonReact.string("Restart"))
            </button>
          </div>;
        } else {
          ReasonReact.string(
            "Current player : " ++ cellToString(state.currentPlayer)
          );
        }
      )
    </div>
};