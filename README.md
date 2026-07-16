# Agility

A dashboard for monitoring and visualizing data, built in Haskell.

## Run

`+RTS` is just -rtsopts to allow runtime options.

`-N` is for multithreading, to fetch data from multiple sources concurrently.

```bash
stack build
stack exec Agility -- +RTS -N
```

### Lint and formatting

```bash
stack install stylish-haskell
stylish-haskell -i src/Agility/YOUR_FILE.hs
```

### Navigate

- `arrow keys` to navigate
- `-` / `+` to move backward and forward between table pages
- `enter` to click a link
- `tab` to switch between tables
- `q` to quit

## Tables

Tables are vertical by default. To place tables side by side, wrap them in an object with `tableWeights` and `tables`.

Dynamic sources support both remote JSON and local JSON files:

`static` sources are hardcoded in the configuration file. This is ideal for data that doesn't change frequently or doesn't need to be updated in real time.

```json
{
  "title": "Static data",
  "columnHeaders": ["Label", "Link"],
  "columnWeights": [1, 2],
  "minColumnHeight": 1,
  "maxColumnHeight": 2,
  "colors": {
    "text": "#ffffff",
    "border": "#5e91ac",
    "title": "#88d0d0",
    "paging": "#b9d6df",
    "header": "#81b1c1",
    "selectedText": "#2e3440",
    "selectedBg": "#a3be8c"
  },
  "source": {
    "type": "static",
    "rows": [
      [["Personal"], ["Massentropy", "https://massentropy.com"]],
      [["Code"], ["Github ", "https://github.com/mattiashognas"]]
    ]
  }
}
```

`image` sources fetch an image from a remote URL and render it in the terminal. The `url` value must be a web URL, not a local file path. Set `refreshSeconds` to `0` to fetch once and never refresh.

```json
{
  "title": "Image",
  "columnWeights": [1],
  "minColumnHeight": 1,
  "maxColumnHeight": 50,
  "source": {
    "type": "image",
    "url": "https://mattiashognas.github.io/Ashes/logo.png",
    "refreshSeconds": 600
  }
}
```

`local` sources read from a local JSON file on a periodic basis. This is ideal for data that changes frequently or needs to be updated in real time.

```json
{
  "title": "Local file",
  "columnHeaders": ["Name", "Price"],
  "columnWeights": [1, 2],
  "minColumnHeight": 1,
  "maxColumnHeight": 2,
  "colors": {
    "text": "#ffffff",
    "border": "#6b8f71",
    "title": "#a7c957",
    "paging": "#cfe88c",
    "header": "#79b227",
    "selectedText": "#1f2a1f",
    "selectedBg": "#d4e09b"
  },
  "source": {
    "type": "local",
    "path": "fruits.json",
    "fields": ["name", "price"],
    "refreshSeconds": 10
  }
}
```

`web` sources fetch JSON from a remote endpoint on a periodic basis. This is ideal for data that doesn't change frequently or doesn't need to be updated in real time.

```json
{
  "title": "Live feed",
  "columnHeaders": ["Question", "Answer"],
  "columnWeights": [2, 3],
  "minColumnHeight": 1,
  "maxColumnHeight": 5,
  "colors": {
    "text": "#ffffff",
    "border": "#d08770",
    "title": "#ebcb8b",
    "paging": "#f0d79a",
    "header": "#e5a75e",
    "selectedText": "#2e3440",
    "selectedBg": "#ebcb8b"
  },
  "source": {
    "type": "web",
    "url": "https://raw.githubusercontent.com/elijahmanor/devpun/refs/heads/master/jokes.json",
    "fields": ["question", "answer"],
    "refreshSeconds": 60
  }
}
```
