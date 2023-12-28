mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;
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
        16 => day16::run(),
        17 => day17::run(),
        18 => day18::run(),
        19 => day19::run(),
        20 => day20::run(),
        21 => day21::run(),
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

fn real_data(day: u8) -> impl Iterator<Item=String> {
    read_data(&format!("day{}.data.txt", day))
}

#[cfg(test)]
fn test_data(day: u8) -> impl Iterator<Item=String> {
    read_data(&format!("day{}.test.txt", day))
}

#[cfg(test)]
fn test_data_num(day: u8, num: u8) -> impl Iterator<Item=String> {
    read_data(&format!("day{}.test{}.txt", day, num))
}
