#!/usr/bin/env bash
set -o errexit
set -o nounset

for suit in bamboo man pin; do
    BASE=img/128px_v2/$suit/$suit
    for f in $BASE?.png; do
        prefix="${f%.png}"
        number="${prefix#$BASE}"
        echo $f
        convert "$f" -pointsize 28 -gravity NorthEast \
            -stroke grey90 -strokewidth 5 -annotate +2+23 "$number" \
            -stroke none -fill "#941309" -annotate +2+23 "$number" \
            "${prefix}_annotated.png"
    done
done