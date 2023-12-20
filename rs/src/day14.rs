use crate::grid::{self, Direction, Pos};

type Grid = grid::Grid<Tile>;

#[derive(PartialEq, Eq, Debug)]
enum Tile {
    MovingRock,
    StillRock,
    EmptySpace,
}
use Tile::*;

pub fn run () {
    let g = Grid::parse(super::real_data(14));
    let answer1 = part1(g);
    println!("{}", answer1);
    assert_eq!(answer1, 110565);

    let g = Grid::parse(super::real_data(14));
    let answer2 = part2(g);
    println!("{}", answer2);
    assert_eq!(answer1, 89845);
}

fn part1(mut g: Grid) -> usize {
    tilt(&mut g, Direction::north());

    compute_answer(&g)
}

fn part2(mut g: Grid) -> usize {
    // This number of repetitions is much smaller than what is actually asked,
    // but we can expect the system to stabilize before that
    // In practice this value is good enough
    for _ in 0..1000 {
        tilt_cycle(&mut g);
    }

    compute_answer(&g)
}

fn compute_answer(g: &Grid) -> usize {
    g.enumerate()
        .filter(|(_, tile)| **tile == MovingRock)
        .map(|(p, _)| load(&g, p))
        .sum()
}

// Load of a rock at a given position
fn load(grid: &Grid, pos: Pos) -> usize {
    grid.row_count() - pos.x
}

impl grid::CharTile for Tile {
    fn from_char(c: &char) -> Self {
        match c {
            'O' => MovingRock,
            '#' => StillRock,
            '.' => EmptySpace,
            _   => panic!("Invalid character for Tile: {:?}", c),
        }
    }

    fn to_char(&self) -> char {
        match self {
            MovingRock => 'O',
            StillRock  => '#',
            EmptySpace => '.',
        }
    }
}

fn tilt(grid: &mut Grid, d: Direction) {
    for pos in processing_order(grid, d) {
        move_all_the_way(grid, pos, d);
    }
}

fn processing_order(grid: &Grid, d: Direction) -> Vec<Pos> {
    let mut indexes: Vec<Pos> = grid.all_pos().collect();

    match d {
        // North: process north rows first
        Direction::Up =>
            indexes.sort_unstable_by_key(|pos| pos.row_idx()),

        // South: process south rows first
        Direction::Down =>
            indexes.sort_unstable_by_key(|pos| std::cmp::Reverse(pos.row_idx())),

        // West: process west cols first
        Direction::Left =>
            indexes.sort_unstable_by_key(|pos| pos.col_idx()),

        // East: process east cols first
        Direction::Right =>
            indexes.sort_unstable_by_key(|pos| std::cmp::Reverse(pos.col_idx())),
    };

    return indexes;
}

fn tilt_cycle(grid: &mut Grid) {
    tilt(grid, Direction::north());
    tilt(grid, Direction::west());
    tilt(grid, Direction::south());
    tilt(grid, Direction::east());
}

fn move_all_the_way(grid: &mut Grid, pos: Pos, d: Direction) {
    let tile = grid.get_pos(pos).unwrap();
    if *tile != MovingRock {
        return;
    }

    let new_pos = find_all_the_way_pos(grid, pos, d);
    let _ = grid.swap(pos, new_pos);
}

fn find_all_the_way_pos(grid: &Grid, pos: Pos, d: Direction) -> Pos {
    let mut new_pos = pos;
    let mut next_pos = pos.mov(d);

    let valid_next_pos = |p: Option<Pos>| {
        match p.and_then(|q| grid.get_pos(q)) {
            Some(EmptySpace) => true,
            _ => false,
        }
    };

    while valid_next_pos(next_pos) {
        new_pos = next_pos.unwrap();
        next_pos = new_pos.mov(d);
    }

    // Note: this could be the same as the starting position
    return new_pos;
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_grid() -> Grid {
        Grid::parse(crate::test_data(14))
    }

    #[test]
    fn get_pos() {
        let g = test_grid();
        assert_eq!(g.get_pos(Pos::new(0, 0)), Some(&MovingRock));
        assert_eq!(g.get_pos(Pos::new(0, 1)), Some(&EmptySpace));
        assert_eq!(g.get_pos(Pos::new(1, 0)), Some(&MovingRock));
    }

    fn assert_grid_eq(found: &Grid, expected: &Grid) {
        if expected != found {
            println!("Expected:");
            expected.show();
            println!(" !=");
            println!("Found:");
            found.show();
            panic!();
        }
    }

    #[test]
    fn tilt() {
        let mut g = test_grid();
        super::tilt(&mut g, Direction::north());

        let expected = Grid::parse(
            vec!(
                "OOOO.#.O..",
                "OO..#....#",
                "OO..O##..O",
                "O..#.OO...",
                "........#.",
                "..#....#.#",
                "..O..#.O.O",
                "..O.......",
                "#....###..",
                "#....#...."
            )
            .into_iter()
            .map(String::from));

        assert_grid_eq(&g, &expected);
    }

    #[test]
    fn find_all_the_way_pos() {
        let g = test_grid();
        assert_eq!(super::find_all_the_way_pos(&g, Pos::new(3, 0), Direction::north()),
                    Pos::new(2, 0));
    }

    #[test]
    fn row_col_count() {
        let g = test_grid();
        assert_eq!(g.row_count(), 10);
        assert_eq!(g.col_count(), 10);
    }

    #[test]
    fn part1() {
        let g = test_grid();
        assert_eq!(super::part1(g), 136);
    }

    #[test]
    fn tilt_cycle1() {
        let mut g = test_grid();
        g.show();
        println!();
        super::tilt(&mut g, Direction::north());
        g.show();
        println!();
        super::tilt(&mut g, Direction::west());
        g.show();
        println!();
        super::tilt(&mut g, Direction::south());
        g.show();
        println!();
        super::tilt(&mut g, Direction::east());
        g.show();
        println!();

        let mut g = test_grid();
        super::tilt_cycle(&mut g);

        let expected = Grid::parse(
            vec!(
                ".....#....",
                "....#...O#",
                "...OO##...",
                ".OO#......",
                ".....OOO#.",
                ".O#...O#.#",
                "....O#....",
                "......OOOO",
                "#...O###..",
                "#..OO#...."
            )
            .into_iter()
            .map(String::from));

        assert_grid_eq(&g, &expected);
    }

    #[test]
    fn part2() {
        let g = test_grid();
        assert_eq!(super::part2(g), 64);
    }
}
