#!/bin/bash
if [ $# != 1 ]; then
    echo "Usage: tmp-animate.sh <data-dir>"
    exit 1
fi
echo "Using data-dir: $1"
DATA=$1

for f in `ls -1 ${DATA}/concentrations-*`; do echo -n "$f "; head -n 1 $f; done | awk '
BEGIN {
  printf("set term gif small animate transparent opt delay 10 size 500,400 x000000\n");
  printf("set output\n");
  printf("set pm3d map\n");
  printf("set palette gray\n");
  printf("set xtics rotate by 90 offset 0,-1;\n");
}
{
  printf("set multiplot layout 2,3 columnsfirst scale 0.8,1.0 title \"%s %s\"\n", $3, $4);
  printf("set cbrange[0:100.0];set title \"S\";       splot [0:400][0:600] \"%s\" using 3:4:5  notitle\n", $1);
  printf("set cbrange[0:0.100];set title \"P\";       splot [0:400][0:600] \"%s\" using 3:4:6  notitle\n", $1);
  printf("set cbrange[0:0.003];set title \"E_{ox}\";  splot [0:400][0:600] \"%s\" using 3:4:7  notitle\n", $1);
  printf("set cbrange[0:0.003];set title \"E_{red}\"; splot [0:400][0:600] \"%s\" using 3:4:8  notitle\n", $1);
  printf("set cbrange[0:0.100];set title \"M_{ox}\";  splot [0:400][0:600] \"%s\" using 3:4:9  notitle\n", $1);
  printf("set cbrange[0:0.100];set title \"M_{red}\"; splot [0:400][0:600] \"%s\" using 3:4:10 notitle\n", $1);
  printf("unset multiplot\n");
}
END {
  printf("quit\n");
}
' | gnuplot > ${DATA}-animate.gif

echo "Image written to ${DATA}-animate.gif"
