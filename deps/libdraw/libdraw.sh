# libdraw
#
# copyright (c) 2018 Cj-bc
#


# draw <picture> at <x>, <y>
# @param <int x> <int y> <string picture>
Draw::drawAt() {
  pos_x=$1
  pos_y=$2
  file=$3
  declare -i i=1

  tput civis # hide cursor
  tput cup $pos_y $pos_x

  while IFS= read -r line; do
    echo -n "$line"
    tput cup $(( $pos_y + $i)) $pos_x
    i+=1
  done < $file

  tput cnorm # appear cursor
}

# Erase whole area <x1> <y1> to <x2> <y>
# @param <int x> <int y> <int width> <int height>
Draw::erase() {
  local -i pos_x
  local -i pos_y
  local -i width
  local -i height
  pos_x=$1
  pos_y=$2
  width=$3
  height=$4

  tput cup $pos_y $pos_x
  for i in $(seq 0 $height); do
    seq -s ' ' $width | tr -d "[:digit:]" # echo ' ' for $width length
  done
}

# clear whole screen
Draw::clearScreen() {
  tput clear
}
