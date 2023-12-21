mod day14;
mod day15;
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
        15 => day15::run(),
        _  => panic!("Unrecognized day argument '{}'", day)
    }
}

fn read_data(filename: &str) -> impl Iterator<Item=String> {
    let filepath = format!("../data/{}", filename);
    let file = File::open(filepath).unwrap();
    let reader = io::BufReader::new(file);
    reader.lines()
        .map(Result::unwrap)
        .map(|x| x.to_string())
}

#[cfg(test)]
fn test_data(day: u8) -> impl Iterator<Item=String> {
    read_data(&format!("day{}.test.txt", day))
}

fn real_data(day: u8) -> impl Iterator<Item=String> {
    read_data(&format!("day{}.data.txt", day))
}
