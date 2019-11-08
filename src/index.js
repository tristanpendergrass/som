import { Elm } from "./Main.elm";

const LOCAL_STORAGE_KEY = "stormcrow_override_manager_data";

chrome.tabs.query({ active: true, currentWindow: true }, ([tab]) => {
  const app = Elm.Main.init({
    node: document.querySelector("main"),
    flags: [tab.url, JSON.parse(localStorage.getItem(LOCAL_STORAGE_KEY))]
  });

  app.ports.sendUrl.subscribe(url => {
    chrome.tabs.update(tab.id, { url });
  });

  app.ports.sendToLocalStorage.subscribe(data => {
    console.log({ data });
    localStorage.setItem(LOCAL_STORAGE_KEY, JSON.stringify(data));
  });
});
