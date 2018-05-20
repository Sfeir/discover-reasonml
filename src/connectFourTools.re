let nbCellX = 7;

let nbCellY = 6;

type cellT =
  | Empty
  | Red
  | Yellow;

type boardT = array(array(cellT));

type gameBoardT = {
  boardX: int,
  boardY: int,
  board: boardT
};

/* Example of an impertative function */
let getMoveY = ({board, boardY}, x) => {
  let index = ref(-1);
  for (i in 0 to boardY - 1) {
    if (index^ === (-1) && board[i][x] === Empty) {
      index := i;
    };
  };
  index^;
};

let cellToString = cell =>
  switch cell {
  | Empty => "empty"
  | Red => "red"
  | Yellow => "yellow"
  };

let sameColor = (board, (x1, y1), (x2, y2)) =>
  board[y1][x1] !== Empty && board[y1][x1] === board[y2][x2];

let isHorizontallyWinning = ({board, boardX}, (x, y)) => {
  let startX = max(0, x - 3);
  let endX = min(boardX - 1, x + 3);
  let rec aux = (count, x) =>
    count === 4 || x > endX ?
      count : aux(sameColor(board, (x - 1, y), (x, y)) ? count + 1 : 1, x + 1);
  aux(1, startX + 1) === 4;
};

let isVerticallyWinning = ({board}, (x, y)) => {
  let rec aux = (count, y) =>
    count === 4 || y < 0 ?
      count : aux(sameColor(board, (x, y + 1), (x, y)) ? count + 1 : 1, y - 1);
  y < 3 ? false : aux(1, y - 1) === 4;
};

let isDiagonallyWinningUp = ({board, boardX, boardY}, (x, y)) => {
  let deltaStartX = x < 3 ? x : 3;
  let deltaStartY = y < 3 ? y : 3;
  let deltaStart = min(deltaStartX, deltaStartY);
  let startX = x - deltaStart;
  let startY = y - deltaStart;
  let deltaEndX = boardX - 1 - x < 3 ? boardX - 1 - x : 3;
  let deltaEndY = boardY - 1 - y < 3 ? boardY - 1 - y : 3;
  let deltaEnd = min(deltaEndX, deltaEndY);
  let endX = x + deltaEnd;
  let endY = y + deltaEnd;
  let rec aux = (count, x, y) =>
    count === 4 || x > endX || y > endY ?
      count :
      aux(
        sameColor(board, (x - 1, y - 1), (x, y)) ? count + 1 : 1,
        x + 1,
        y + 1
      );
  aux(1, startX + 1, startY + 1) === 4;
};

/* Looks a lot like isDiagonallyWinningUp but not factorizing these two for the sake of clarity */
let isDiagonallyWinningDown = ({board, boardX, boardY}, (x, y)) => {
  let deltaStartX = x < 3 ? x : 3;
  let deltaStartY = boardY - 1 - y < 3 ? boardY - 1 - y : 3;
  let startCoord = min(deltaStartX, deltaStartY);
  let startX = x - startCoord;
  let startY = y + startCoord;
  let deltaEndX = boardX - 1 - x < 3 ? boardX - 1 - x : 3;
  let deltaEndY = y < 3 ? y : 3;
  let endCoord = min(deltaEndX, deltaEndY);
  let endX = x + endCoord;
  let endY = y - deltaEndY;
  let rec aux = (count, x, y) =>
    count === 4 || x > endX || y < endY ?
      count :
      aux(
        sameColor(board, (x - 1, y + 1), (x, y)) ? count + 1 : 1,
        x + 1,
        y - 1
      );
  aux(1, startX + 1, startY - 1) === 4;
};

let isDiagonallyWinning = (state, pos) =>
  isDiagonallyWinningDown(state, pos) || isDiagonallyWinningUp(state, pos);

let isWinning = (state, pos) =>
  isHorizontallyWinning(state, pos)
  || isVerticallyWinning(state, pos)
  || isDiagonallyWinning(state, pos);

let printGrid = grid =>
  grid
  |> Array.to_list
  |> List.rev
  |> List.iteri((index, row) => {
       let rowString =
         row
         |> Array.fold_left(
              (string, cell) => string ++ cellToString(cell) ++ " ",
              ""
            );
       print_endline(
         string_of_int(Array.length(row) - index - 2) ++ "   " ++ rowString
       );
     });