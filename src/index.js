import { Elm } from "./Main.elm";

chrome.tabs.query({ active: true }, ([tab]) => {
  Elm.Main.init({
    node: document.querySelector("main"),
    flags: tab.id
  });
});
