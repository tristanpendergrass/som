# Stormcrow Override Manager

A chrome extension built with Elm to manage your stormcrow overrides. At the press of a button will apply one or more stormcrow overrides to the current url, e.g. "www.dropbox.com/home" -> "www.dropbox.com/home?stormcrow_override=my_experiement:ON". Open it by clicking the icon in the top right of the Chrome window.

Storm icon credit: (srip)[https://www.flaticon.com/authors/srip].

## Installation

```
$ git clone git@github.com:tristanpendergrass/som.git
$ cd som
$ npm install
$ npm run build
```

Then go to [chrome://extensions](chrome://extensions) and click "Load unpacked" and select the som directory.

## Development

In one tab run this command to start the Tailwind CSS build watcher:

```
$ npm run build:css:watch
```

and in another tab run this command to build everything else:

```
$ npm run build:watch
```

You do not have to go through the "Load unpacked" step from installation again, just close and reopen the extension to see your changes.

## Test

Tests are in /tests. Written using https://github.com/elm-explorations/test.

```
$ npm test
$ npm test -- --watch
```
