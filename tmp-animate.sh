#!/bin/bash

for f in `ls -1 tmp-out08/concentrations-*0`; do echo -n "$f "; head -n 1 $f; done | awk '
BEGIN {
  printf("set term gif small animate transparent opt delay 10 size 400,400 x000000\n");
  printf("set output \"tmp-out08-animate.gif\"\n");
  printf("set pm3d map\n");
  printf("set palette gray\n");
}
{
  printf("set multiplot layout 2,3 columnsfirst title \"%s %s\"\n", $3, $4);
  printf("set cbrange[0:100.0];set title \"S\";     splot [0:40][0:60][0:100] \"%s\" using 1:2:3 notitle\n", $1);
  printf("set cbrange[0:100.0];set title \"P\";     splot [0:40][0:60][0:100] \"%s\" using 1:2:4 notitle\n", $1);
  printf("set cbrange[0:0.003];set title \"E_ox\";  splot [0:40][0:60][0:100] \"%s\" using 1:2:5 notitle\n", $1);
  printf("set cbrange[0:0.003];set title \"E_red\"; splot [0:40][0:60][0:100] \"%s\" using 1:2:6 notitle\n", $1);
  printf("set cbrange[0:0.100];set title \"M_ox\";  splot [0:40][0:60][0:100] \"%s\" using 1:2:7 notitle\n", $1);
  printf("set cbrange[0:0.100];set title \"M_red\"; splot [0:40][0:60][0:100] \"%s\" using 1:2:8 notitle\n", $1);
  printf("unset multiplot\n");
}
END {
  printf("quit\n");
}
' > tmp-out08-animate.gpl
gnuplot tmp-out08-animate.gpl
