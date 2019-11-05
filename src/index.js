import { Elm } from "./Main.elm";

chrome.tabs.query({ active: true, currentWindow: true }, ([tab]) => {
  const app = Elm.Main.init({
    node: document.querySelector("main"),
    flags: tab.url
  });

  app.ports.sendUrl.subscribe(url => {
    chrome.tabs.update(tab.id, { url });
  });
});
