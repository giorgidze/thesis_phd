set terminal pdf monochrome enhanced dashed font "Times"
set output "pendulumPlot.pdf"
set xlabel "time [s]"

plot  "pendulum.dat" using 1:2 with lines title "x [m]", "pendulum.dat" using 1:3 with lines title "y [m]"
