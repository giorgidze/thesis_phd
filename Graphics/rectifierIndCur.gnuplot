set terminal pdf monochrome enhanced dashed font "Times"
set output "rectifierIndCur.pdf"
set xlabel "time [s]"

plot "rectifierIndCur.dat" using 1:2 with lines title "L_i [A]"
