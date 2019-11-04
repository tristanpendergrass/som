// window.addEventListener("load", () => {
//   chrome.tabs.getSelected(null, function(tab) {
//     console.log({ tab });
//   });

//   chrome.tabs.onUpdated.addListener(function(...args) {
//     console.log(...args);
//   });
// });

// window.testSet = () => {
//   chrome.tabs.query({}, ([tab, ...restOfTabs]) => {
//     console.log({ tab, restOfTabs });
//     if (tab !== undefined) {
//       const url = tab.url + `?foo=bar`;
//       console.log("url", url);
//       chrome.tabs.update(tab.id, { url });
//     }
//   });
// };

// chrome.tabs.query({}, tabs => {
//   console.log("init", tabs.map(tab => tab.url));
// });

// chrome.tabs.onActivated.addListener((tabId, windowId) => {
//   console.log("onActivated", { tabId, windowId });
// });

// chrome.tabs.onUpdated.addListener((tabId, _, { url }) => {
//   console.log("onUpdated", { tabId, url });
// });
