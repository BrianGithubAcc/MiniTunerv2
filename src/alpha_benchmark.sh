#!/bin/bash
# Only choose expressions with single variable

cd "$(dirname "$0")" || exit 1

benchmark_time() {
    local files=("$@")
    local runs=3

    for file in "${files[@]}"; do
        expr=$(cat "../FPBenchmarks/$file.txt")

        # Loop over alpha values 0.1 to 1.0 in steps of 0.1
        for alpha in $(seq 0.1 0.1 1.0); do
            sum=0
            output_file="alpha_$(printf "%.1f" $alpha)"

            for i in $(seq 1 $runs); do
                cmd="dune exec ./minitune.exe -- -a $alpha -d -e \"$expr\" -o $output_file >/dev/null 2>/dev/null"

                # Measure the time
                t=$(/usr/bin/time -f "%e" bash -c "$cmd" 2>&1)
                sum=$(awk "BEGIN {print $sum + $t}")
            done

            avg=$(awk "BEGIN {print $sum / $runs}")
            echo "Time of $file with alpha=$alpha (average over $runs runs): $avg s"
        done
    done
}

# Run the benchmark for your example
benchmark_time nmse_example_3_4