#!/bin/bash

JSTMP=./build/index.tmp.js
JS=./build/index.js
JSMIN=./build/index.min.js
DATE=./build/date.js
MODERNIZR=./build/modernizr-custom.js

TEMPLATE=./build/index.template.html

HTMLTMP=./build/index.tmp.html
OUTPUT=./index.html

BROWSERIFY=./node_modules/.bin/browserify
HTMLMINIFIER=./node_modules/.bin/html-minifier


if [ $# -eq 0 ]; then
    JSOUT=$JS
elif [ $1 == "production" ]; then
    JSOUT=$JSMIN
else
    echo "incorrect flag usage - either \"production\" or none"
    exit
fi

pulp build || { exit 1; }
purs bundle output/**/*.js -m Main --main Main > $JSTMP || { echo "Bundle Failed"; exit 1; }
echo "Bundled"
$BROWSERIFY $JSTMP -o $JS -t [ babelify --presets [ @babel/preset-env ] --plugins [ @babel/plugin-proposal-class-properties ] ] || { exit 1; }
rm $JSTMP
if [ $# -eq 1 ] && [ $1 == "production" ]; then
    $BROWSERIFY $JS -g [ envify --NODE_ENV production ] -g uglifyify | ./node_modules/.bin/uglifyjs --compress --mangle > $JSMIN || { exit 1; }
fi
echo "Browserified"
date -R | sed 's/\(.*\)/var buildDate = "\1"/g' > $DATE
ltext "$TEMPLATE $JSOUT $DATE $MODERNIZR" --raw $JSOUT --raw $DATE --raw $MODERNIZR > $HTMLTMP || { exit 1; }
echo "Linked"
$HTMLMINIFIER --collapse-whitespace --collapse-inline-tag-whitespace --remove-comments --remove-optional-tags --remove-redundant-attributes --remove-script-type-attributes --remove-tag-whitespace --use-short-doctype --minify-css true --minify-js false $HTMLTMP > $OUTPUT || { exit 1; }
rm $HTMLTMP
echo "Finished"
