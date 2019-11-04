// window.addEventListener("load", () => {
//   chrome.tabs.getSelected(null, function(tab) {
//     console.log({ tab });
//   });

//   chrome.tabs.onUpdated.addListener(function(...args) {
//     console.log(...args);
//   });
// });

// window.setParam = (key, value) => {
//   chrome.tabs.query({ active: true }, ([tab]) => {
//     if (tab !== undefined) {
//       const url = tab.url + `?${key}=${value}`;
//       console.log("url", url);
//       chrome.tabs.update(tab.id, { url });
//     }
//   });
//   window.close();
// };

chrome.tabs.query({}, tabs => {
  console.log("init", tabs.map(tab => tab.url));
});

chrome.tabs.onActivated.addListener((tabId, windowId) => {
  console.log("onActivated", { tabId, windowId });
});

chrome.tabs.onUpdated.addListener((tabId, _, { url }) => {
  console.log("onUpdated", { tabId, url });
});
