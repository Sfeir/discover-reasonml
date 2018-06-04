type disc =
  | Yellow
  | Red;

type board = list(list(disc));

let initial = () => [[], [], [Yellow], [Yellow, Red], [], [], []];