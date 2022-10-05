#!/bin/sh
# From https://guide.elm-lang.org/optimization/asset_size.html
set -e

js="app.js"
min="public/app.min.js"

elm make --optimize --output=$js "$@"

mkdir -p public
uglifyjs $js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $min

echo "Compiled size: $(wc $js -c) bytes  ($js)"
echo "Minified size: $(wc $min -c) bytes  ($min)"
echo "Gzipped size:  $(gzip $min -c | wc -c) bytes"

npm run css-build:prod

mkdir -p public/img
cp -r img/128px_v2 public/img/
cp img/favicon.ico public/img/

sed "s#../css/app.css#app.min.css#; s/app.js/app.min.js/; s/YOUR_PROJECT_ID/$SWETRIX_PROJECT_ID/; s/disabled: *true/disabled: false/" index.html > public/index.html