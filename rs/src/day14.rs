use crate::grid;

type Grid = grid::Grid<Tile>;

enum Tile {
    MovingRock,
    StillRock,
    EmptySpace,
}
use Tile::*;

pub fn run () {
    Grid::parse(super::test_data(14)).show();
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
