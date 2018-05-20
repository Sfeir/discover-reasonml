Webpack.import("./App.css");

let component = ReasonReact.statelessComponent("App");

let make = _children => {
  ...component,
  render: _self =>
    <div className="App">
      <h1> (ReasonReact.string("Connect 4")) </h1>
      <ConnectFourComponent />
    </div>
};