use std::collections::HashSet;
use crate::grid::{Grid, Pos, Direction};

pub fn run () {
    let garden = parse(super::real_data(21));

    let answer1 = part1(&garden, 64);
    println!("{}", answer1);
    assert_eq!(answer1, 3562);

    //reset(&mut system);
    //let answer2 = part2(&mut system);
    //println!("{}", answer2);
    //assert_eq!(answer2, 225514321828633);
}

fn part1(garden: &Garden, n: usize) -> usize {
    let mut poss = HashSet::new();
    poss.insert(garden.start_pos);

    for _ in 0..n {
        expand_once(&mut poss, garden);
    }

    poss.len()
}

struct Garden {
    garden_plots: HashSet<Pos>,
    start_pos:    Pos,
}

fn parse<I: Iterator<Item=String>>(lines: I) -> Garden {
    let grid: Grid<char> = Grid::parse(lines);

    let start_pos = grid.enumerate()
                        .find(|(_, c)| **c == 'S')
                        .unwrap()
                        .0;

    let garden_plots = grid.enumerate()
                            .filter(|(_, c)| **c == 'S' || **c == '.')
                            .map(|(p, _)| p)
                            .collect();

    Garden {
        garden_plots,
        start_pos
    }
}

fn expand_once(poss: &mut HashSet<Pos>, garden: &Garden) {
    *poss = poss.iter()
                .flat_map(|p| garden.neighbor_plots(*p))
                .collect();
}

impl Garden {
    fn neighbor_plots(&self, pos: Pos) -> impl Iterator<Item=Pos> + '_ {
        IntoIterator::into_iter(Direction::all())
                .filter_map(move |d| pos.mov(d))
                .filter(move |next_pos| self.garden_plots.contains(next_pos))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_data() -> Garden {
        parse(super::super::test_data(21))
    }

    #[test]
    fn part1() {
        assert_eq!(super::part1(&test_data(), 6), 16);
    }
}
