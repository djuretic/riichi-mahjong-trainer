#!/usr/bin/env bash
set -o errexit
set -o nounset

annotate_tile () {
    echo $f
    convert "$1" -pointsize 28 -gravity NorthEast \
            -stroke grey90 -strokewidth 5 -annotate +2+23 "$2" \
            -stroke none -fill "#941309" -annotate +2+23 "$2" \
            "${3}_annotated.png"
}


for suit in bamboo man pin; do
    BASE=img/128px_v2/$suit/$suit
    for f in $BASE?.png; do
        prefix="${f%.png}"
        number="${prefix#$BASE}"
        annotate_tile "$f" "$number" "$prefix"
    done
done

for wind in east south west north; do
    f="img/128px_v2/winds/wind-$wind.png"
    prefix="${f%.png}"
    letter=$(echo "${wind:0:1}" | tr '[:lower:]' '[:upper:]' )
    annotate_tile "$f" "$letter" "$prefix"
done


declare -A dragons_map=(["haku"]="W" ["green"]="G" ["chun"]="R")

for dragon in haku green chun; do
    f="img/128px_v2/dragons/dragon-$dragon.png"
    prefix="${f%.png}"
    letter="${dragons_map[$dragon]}"
    annotate_tile "$f" "$letter" "$prefix"
done