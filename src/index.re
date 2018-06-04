let message = "Hello BestOfWeb !";
let element = <h1> (ReasonReact.string(message)) </h1>;

Js.log(message);
ReactDOMRe.renderToElementWithId(element, "root");