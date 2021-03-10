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
- [x] Check it works in firefox
- [x] Make selected/unselected be a checkbox on left
- [x] Fix layout of rows
- [x] Tooltips for feature and variant
- [x] Add underline for text box for custom variant/feature input
- [x] Make red/green background color of feature name be dependent on variant and only show when selected
- [x] Improve experience of editing name: remove accept button, focus on edit
- [x] Fix unarchive page
- [x] Make add feature look like an override but with (+) button instead of edit and no other stuff
- [x] Make apply overrides button disable styles correct and reposition
- [x] Add github link and version number
- [ ] Make overload list look better when lots of overrides
- [ ] Add a way to easily enter override_token (easily get override token?)
- [ ] Improve styles of select dropdown
- [ ] Let Enter blur the feature/variant input
- [x] Make input for editing feature auto focus when edit button clicked https://stackoverflow.com/questions/31901397/how-to-set-focus-on-an-element-in-elm
