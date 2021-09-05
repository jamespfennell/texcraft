import React from "react";
import ReactDOM from "react-dom";
import App from "./App";

var root = document.getElementById('root')!;
ReactDOM.render(<App {...(root.dataset)} />, root);
