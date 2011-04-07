set terminal pdf monochrome enhanced dashed font "Times"
set output "rectifierCapVol.pdf"
set xlabel "time [s]"

plot "rectifierCapVol.dat" using 1:2 with lines title "C_v [V]"
