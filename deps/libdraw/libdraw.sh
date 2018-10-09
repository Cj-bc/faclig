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
    tput cup $(( $pos_y + i)) $pos_x
    i+=1
  done < $file

  tput cnorm # appear cursor
}

# Erase whole area <x1> <y1> to <x2> <y>
# @param <int x> <int y> <int x2> <int y2>
Draw::erase() {
}

# clear whole screen
Draw::clearScreen() {
  tput clear
}
