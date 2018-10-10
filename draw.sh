#!/usr/bin/env bash
#
# ./draw.sh <int x> <int y> <string part>
# draw <part> at <x> <y>.
# the previous drawed <part> will be erased before rendering new position(if exist).
source "$( cd "${BASH_SOURCE[0]%/*}" && pwd )/deps/bin/libdraw"

declare -r asset_root="src"
declare -Ar asset=([right_eye]="eye.txt" [left_eye]="eye.txt" [mouth]="mouth.txt")


x=$1
y=$2
file=$3

Draw::drawAt $x $y ${asset_root}/${asset[$file]}
