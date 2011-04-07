set terminal pdf monochrome enhanced dashed font "Times"
set output "circuitPlot1.pdf"
set xlabel "time [s]"

plot "circuit1.dat" using 1:14 with lines title "i_1 [A]", "circuit1.dat" using 1:7  with lines title "i_2 [A]"
