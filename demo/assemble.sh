#!/bin/sh
# One animation out of the frames `demo/record.el' wrote.
#
#     sh demo/assemble.sh SCENARIO [SPEED] [FRAMES-DIR]
#
# Every frame becomes a single-frame GIF, and gifsicle puts them
# together with the hold each one asked for.  SPEED scales those holds,
# for a reader who wants the animation quicker or slower than the
# scenario asked for.
set -u
S=${1:?a scenario: md, pydoc, pycell or rmd}
SPEED=${2:-1.0}
DIR=${3:-${TMPDIR:-/tmp}/overblock-demo/$S}
[ -f "$DIR/holds.txt" ] || { echo "no frames in $DIR"; exit 1; }
# One palette for the whole animation.  A palette a frame gets to
# itself is a palette the next frame does not have: the colours then
# shift from frame to frame, and text drawn in one grey came out in
# another.
ffmpeg -loglevel error -y -f image2 -pattern_type glob -i "$DIR/[0-9]*.png" \
       -vf palettegen=stats_mode=full "$DIR/palette.png"
set --
i=0
for png in "$DIR"/[0-9]*.png; do
  i=$((i + 1))
  hold=$(awk "BEGIN{h=int($(sed -n "${i}p" "$DIR/holds.txt") * $SPEED)
              print (h < 8 ? 8 : h)}")
  gif=${png%.png}.gif
  # No scaling: the frame was exported at the width the animation is
  # written at, and text scaled by even a little comes out blurred.
  ffmpeg -loglevel error -y -i "$png" -i "$DIR/palette.png" \
         -lavfi "paletteuse=dither=none" "$gif"
  set -- "$@" -d"$hold" "$gif"
done
gifsicle --loopcount=0 -O3 "$@" -o "$DIR/$S.gif"
gifsicle --info "$DIR/$S.gif" |
  awk -v s="$S" '/delay/{t+=$NF+0} END{printf "%s.gif: %.1fs\n", s, t}'
ls -l "$DIR/$S.gif"
