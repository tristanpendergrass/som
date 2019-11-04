import { Elm } from "./Main.elm";

chrome.tabs.query({ active: true, currentWindow: true }, ([tab]) => {
  const app = Elm.Main.init({
    node: document.querySelector("main"),
    flags: tab.id
  });
});
