Webpack.import("./connectFour.css");

open ConnectFourTools;

type actionT =
  | Click(int)
  | Restart;

type stateT = {
  gameBoard: gameBoardT,
  currentPlayer: cellT,
  winner: option(cellT)
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
      board: Array.make_matrix(nbCellY, nbCellX, Empty),
      boardX: nbCellX,
      boardY: nbCellY
    },
    currentPlayer: Red,
    winner: None
  },
  reducer: (action, state: stateT) =>
    switch action {
    | Click(x) =>
      switch state.winner {
      | None =>
        let y = getMoveY(state.gameBoard, x);
        state.gameBoard.board[y][x] = state.currentPlayer;
        ReasonReact.Update({
          ...state,
          winner:
            isWinning(state.gameBoard, (x, y)) ?
              Some(state.currentPlayer) : None,
          currentPlayer: state.currentPlayer === Red ? Yellow : Red
        });
      | Some(_) => ReasonReact.NoUpdate
      }
    | Restart =>
      ReasonReact.Update({
        gameBoard: {
          board: Array.make_matrix(nbCellY, nbCellX, Empty),
          boardX: nbCellX,
          boardY: nbCellY
        },
        currentPlayer: Red,
        winner: None
      })
    },
  render: ({state, send}) =>
    <div>
      (displayBoard(state.gameBoard, send))
      (
        switch state.winner {
        | Some(winner) =>
          <div>
            (ReasonReact.string("Winner : " ++ cellToString(winner)))
            <br />
            <button onClick=((_) => send(Restart))>
              (ReasonReact.string("Restart"))
            </button>
          </div>
        | None =>
          ReasonReact.string(
            "Current player : " ++ cellToString(state.currentPlayer)
          )
        }
      )
    </div>
};