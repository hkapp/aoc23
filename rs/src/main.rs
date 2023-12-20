mod day14;
mod grid;

use std::io::{self, BufRead};
use std::fs::File;

fn main() {
    let day: u8 = std::env::args()
                    .nth(1)
                    .unwrap()
                    .parse()
                    .unwrap();
    println!("Day {}", day);

    match day {
        14 => day14::run(),
        _  => panic!("Unrecognized day argument '{}'", day)
    }
}

fn test_data(day: u8) -> impl Iterator<Item=String> {
    let filename = format!("../data/day{}.test.txt", day);
    let file = File::open(filename).unwrap();
    let reader = io::BufReader::new(file);
    reader.lines()
        .map(Result::unwrap)
        .map(|x| x.to_string())
}
