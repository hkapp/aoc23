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
    assert_eq!(answer1, 110565)
}

fn part1(mut g: Grid) -> usize {
    tilt(&mut g, Direction::north());

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
    for x in 0..grid.row_count() {
        for y in 0..grid.col_count() {
            let pos = Pos::new(x, y);
            move_all_the_way(grid, pos, d);
        }
    }
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

        if expected != g {
            println!("Expected:");
            expected.show();
            println!(" !=");
            println!("Found:");
            g.show();
            panic!();
        }
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
}
