parcelRequire=function(e,r,t,n){var i,o="function"==typeof parcelRequire&&parcelRequire,u="function"==typeof require&&require;function f(t,n){if(!r[t]){if(!e[t]){var i="function"==typeof parcelRequire&&parcelRequire;if(!n&&i)return i(t,!0);if(o)return o(t,!0);if(u&&"string"==typeof t)return u(t);var c=new Error("Cannot find module '"+t+"'");throw c.code="MODULE_NOT_FOUND",c}p.resolve=function(r){return e[t][1][r]||r},p.cache={};var l=r[t]=new f.Module(t);e[t][0].call(l.exports,p,l,l.exports,this)}return r[t].exports;function p(e){return f(p.resolve(e))}}f.isParcelRequire=!0,f.Module=function(e){this.id=e,this.bundle=f,this.exports={}},f.modules=e,f.cache=r,f.parent=o,f.register=function(r,t){e[r]=[function(e,r){r.exports=t},{}]};for(var c=0;c<t.length;c++)try{f(t[c])}catch(e){i||(i=e)}if(t.length){var l=f(t[t.length-1]);"object"==typeof exports&&"undefined"!=typeof module?module.exports=l:"function"==typeof define&&define.amd?define(function(){return l}):n&&(this[n]=l)}if(parcelRequire=f,i)throw i;return f}({"HdJB":[function(require,module,exports) {
function t(t,e){return o(t)||r(t,e)||n()}function n(){throw new TypeError("Invalid attempt to destructure non-iterable instance")}function r(t,n){if(Symbol.iterator in Object(t)||"[object Arguments]"===Object.prototype.toString.call(t)){var r=[],o=!0,e=!1,a=void 0;try{for(var i,c=t[Symbol.iterator]();!(o=(i=c.next()).done)&&(r.push(i.value),!n||r.length!==n);o=!0);}catch(l){e=!0,a=l}finally{try{o||null==c.return||c.return()}finally{if(e)throw a}}return r}}function o(t){if(Array.isArray(t))return t}window.addEventListener("load",function(){chrome.tabs.getSelected(null,function(t){console.log({tab:t})}),chrome.tabs.onUpdated.addListener(function(){var t;(t=console).log.apply(t,arguments)})}),window.setParam=function(n,r){chrome.tabs.query({active:!0},function(o){var e=t(o,1)[0];if(void 0!==e){var a=e.url+"?".concat(n,"=").concat(r);console.log("url",a),chrome.tabs.update(e.id,{url:a})}}),window.close()};
},{}]},{},["HdJB"], null)
//# sourceMappingURL=/dist/test.51c437cc.js.map