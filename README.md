# Stormcrow Override Manager

A chrome extension built with Elm to manage your stormcrow overrides. At the press of a button will apply one or more stormcrow overrides to the current url, e.g. "www.dropbox.com/home" -> "www.dropbox.com/home?stormcrow_override=my_experiement:ON". Open it by clicking the icon in the top right of the Chrome window.

## Installation

```
$ git clone git@github.com:tristanpendergrass/som.git
$ cd som
$ npm install
$ npm run build
```

Then go to [chrome://extensions](chrome://extensions) and click "Load unpacked" and select the som directory.

## Development

Make any desired changes in /src and just run the following again:

```
$ npm run build
```

## Test

Tests are in /tests. Written using https://github.com/elm-explorations/test.

```
$ npm test
$ npm test -- --watch
```

## Todos

- [x] Add unit tests for QueryParams.elm (update readme with instructions to run)
- [x] Add Tailwind CSS library
- [ ] Add button that adds a rule to the query string
- [ ] Show which rules are detected in the query string
- [ ] Make it work in Firefox
- [ ] Add a way to easily enter override_token
- [ ] Visual polish
