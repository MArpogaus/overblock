#!/bin/sh
# One animation out of the frames `demo/record.el' wrote.
#
#     sh demo/assemble.sh SCENARIO [SPEED] [FRAMES-DIR]
#
# Every frame becomes a single-frame GIF, and gifsicle puts them
# together with the hold each one asked for.  SPEED scales those holds:
# a recording reads well watched once, and a README loops, so the
# default is a little quicker than the scenario asked for.
set -u
S=${1:?a scenario: md, pydoc, pycell or rmd}
SPEED=${2:-0.55}
DIR=${3:-${TMPDIR:-/tmp}/overblock-demo/$S}
[ -f "$DIR/holds.txt" ] || { echo "no frames in $DIR"; exit 1; }
set --
i=0
for png in "$DIR"/[0-9]*.png; do
  i=$((i + 1))
  hold=$(awk "BEGIN{h=int($(sed -n "${i}p" "$DIR/holds.txt") * $SPEED)
              print (h < 8 ? 8 : h)}")
  gif=${png%.png}.gif
  ffmpeg -loglevel error -y -i "$png" -vf "scale=900:-1:flags=lanczos" "$gif"
  set -- "$@" -d"$hold" "$gif"
done
gifsicle --loopcount=0 --colors 128 -O3 "$@" -o "$DIR/$S.gif"
gifsicle --info "$DIR/$S.gif" |
  awk -v s="$S" '/delay/{t+=$NF+0} END{printf "%s.gif: %.1fs\n", s, t}'
ls -l "$DIR/$S.gif"
