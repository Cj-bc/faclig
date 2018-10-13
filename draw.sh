#!/usr/bin/env bash
#
# ./draw.sh <int x> <int y> <string part>
# draw <part> at <x> <y>.
# the previous drawed <part> will be erased before rendering new position(if exist).
source "$( cd "${BASH_SOURCE[0]%/*}" && pwd )/deps/bin/libdraw"

declare -r asset_root="src"
declare -Ar asset=([right_eye]="right_eye.txt" [left_eye]="left_eye.txt" [mouth]="mouth.txt" [glasses]="glasses.txt")


x=$1
y=$2
file=$3
fifo_name="faclig_${file}.fifo"

if [ -p "$fifo_name" ];then
  declare -i prev_x
  declare -i prev_y
  read prev_x prev_y < $fifo_name
  Draw::erasePicture $prev_x $prev_y ${asset_root}/${asset[$file]}
fi

Draw::drawAt $x $y ${asset_root}/${asset[$file]}
[ -p "$fifo_name" ] || mkfifo $fifo_name
echo "$x $y" > $fifo_name &
