# Mahjong Waits Trainer

A trainer for Mahjong written in Elm.

Site: https://mahjong-trainer.netlify.app

## Develop

Make sure that you have Elm installed in your system.

```bash
npm install
# generate css/app.css
npm run css-build
npm run live
```
The last command will open your browser, reload the page if you don't see anything the first time. 

## Production build

Generate a build in `public/`:

```bash
npm run build:prod
```

## Translations

If a .ftl file is modified, update the src/I18n.elm file:

```bash
npm run translations
```

## Tests

```bash
npm test
```

## License

MIT
