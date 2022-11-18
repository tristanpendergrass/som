# Stormcrow Override Manager

A chrome extension built with Elm to manage your stormcrow overrides. At the press of a button will apply one or more stormcrow overrides to the current url, e.g. "www.dropbox.com/home" -> "www.dropbox.com/home?stormcrow_override=my_experiement:ON". Open it by clicking the icon in the top right of the Chrome window.

Storm icon credit: (srip)[https://www.flaticon.com/authors/srip].

## Getting Started & Build

1. Install Elm on your computer: https://guide.elm-lang.org/install/elm.html.

2.

```
$ npm install
$ npm run build:css
$ npm run build
```

## Development

There are "watch" versions of the two build commands. You must run each in a different terminal tab.

```
$ npm run build:css:watch
```

and in another tab:

```
$ npm run build:watch
```

### Testing on Chrome

Go to [chrome://extensions](chrome://extensions) and click "Load unpacked" and select the `som` directory. You only have to do this step once.

## Deployment

- Bump the version number in static/manifest.json.
- Bump the version number in this view in Main.elm.
- Run the build commands above to generate an up-to-date /dist folder.
- Compress the /dist folder and upload to Chrome web store or wherever.

## Test

Tests are in /tests. Written using https://github.com/elm-explorations/test.

```
$ npm test
$ npm test -- --watch
```
