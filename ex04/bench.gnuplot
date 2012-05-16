set term pdf
set title "Queue/Deque Benchmark"
set output "bench.pdf"
set datafile separator ","
set xlabel "Input Size"
set ylabel "Runtime (s)"

plot 	"bench.csv" using 2:3 index 0:0 title "ListQueue" with linespoints,\
	"bench.csv" using 2:3 index 1:1 title "SimpleQueue" with linespoints,\
	"bench.csv" using 2:3 index 2:2 title "SimpleQueueMod" with linespoints,\
	"bench.csv" using 2:3 index 3:3 title "SchedQueue" with linespoints,\
	"bench.csv" using 2:3 index 4:4 title "Deque" with linespoints

