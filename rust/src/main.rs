use std::env;
use std::fs;
use std::process::exit;
use std::time::Instant;

extern crate meta;
meta::import_days!();
meta::define_run!(day part);

fn usage(name: &String) {
    println!(
        "Usage: {name} DAY PART
Example: {name} 4 1  # Run day 4 part 1.
to run both parts use 0 for part number.",
        name = name
    )
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        usage(&args[0]);
        exit(1);
    }

    let day: u8 = args[1].parse::<u8>().unwrap_or_else(|_| {
        usage(&args[0]);
        0
    });
    let part: u8 = args[2].parse().unwrap_or_else(|_| {
        usage(&args[0]);
        0
    });

    let begin = Instant::now();
    let output = run(day, part);
    let duration = begin.elapsed();

    println!("{}", output);
    println!(
        "Run time: {}ms",
        (duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9) * 1000.0
    );
}

fn read_input(day: usize) -> String {
    fs::read_to_string(format!("./input/day{}", day))
        .expect(&format!("Input file for day {} not found", day))
}
