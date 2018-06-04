let component = ReasonReact.statelessComponent("App");

let make = (~greet, _children) => {
  ...component,
  render: _self => <h1> (ReasonReact.string("Hello " ++ greet ++ " !")) </h1>,
};