use crate::grid::{self, Direction, Pos};
use std::collections::HashSet;

pub fn run () {
    let c = Contraption::parse(super::real_data(16));

    let answer1 = part1(&c);
    println!("{}", answer1);
    assert_eq!(answer1, 7939);

    //let answer2 = part2(&steps);
    //println!("{}", answer2);
    //assert_eq!(answer2, 236057);
}

fn part1(c: &Contraption) -> usize {
    energized(light_flow(c)).len()
}

#[derive(Copy, Clone)]
enum Tile {
    EmptySpace,
    Mirror(Diagonal),
    Splitter(StraightLine),
}
use Tile::*;

#[derive(Copy, Clone)]
enum Diagonal {
    ForwardSlash,
    BackwardSlash,
}
use Diagonal::*;

#[derive(Copy, Clone)]
enum StraightLine {
    Horizontal,
    Vertical
}
use StraightLine::*;

type Contraption = grid::Grid<Tile>;

impl grid::CharTile for Tile {
    fn from_char(c: &char) -> Self {
        match c {
            '.'  => EmptySpace,
            '/'  => Mirror(ForwardSlash),
            '\\' => Mirror(BackwardSlash),
            '|'  => Splitter(Vertical),
            '-'  => Splitter(Horizontal),
            _    => panic!("Unrecognized character {:?}", c),
        }
    }

    fn to_char(&self) -> char {
        match self {
            EmptySpace            => '.',
            Mirror(ForwardSlash)  => '/',
            Mirror(BackwardSlash) => '\\',
            Splitter(Vertical)    => '|',
            Splitter(Horizontal)  => '-',
        }
    }
}

type LightHistory = HashSet<(Pos, Direction)>;

fn light_flow(c: &Contraption) -> LightHistory {
    let start_pos = Pos::new(0, 0);
    let start_dir = Direction::Right;

    let mut history = LightHistory::new();
    let mut frontier = vec![(start_pos, start_dir)];

    while !frontier.is_empty() {
        let curr_key = frontier.pop().unwrap();
        if history.contains(&curr_key) {
            continue;
        }
        else {
            history.insert(curr_key);
        }

        let (curr_pos, curr_dir) = curr_key;
        let curr_tile = c[curr_pos];
        let new_dirs = bend_light(curr_tile, curr_dir);

        for new_dir in new_dirs {
            let mb_new_pos = curr_pos.mov(new_dir);
            if let Some(new_pos) = mb_new_pos {
                if c.contains_pos(new_pos) {
                    frontier.push((new_pos, new_dir));
                }
            }
        }
    }

    return history;
}

// This function is understood as "the light already reached this position, traveling in this direction"
// That is, we must take into account the tile at the current light position
fn bend_light(tile: Tile, curr_dir: Direction) -> Vec<Direction> {
    use Direction::*;

    match tile {
        EmptySpace => vec![curr_dir],

        /*     ^     |
         *     |     v
         *  -> /   <-/
         *
         *   /<-   /->
         *   |     ^
         *   v     |
         */
        Mirror(ForwardSlash) =>
            vec![
                match curr_dir {
                    Right => Up,
                    Down  => Left,
                    Left  => Down,
                    Up    => Right,
                }
            ],

        /*  ^     |
         *  |     v
         *  \<-   \->
         *
         * ->\   <-\
         *   |     ^
         *   v     |
         */
        Mirror(BackwardSlash) =>
            vec![
                match curr_dir {
                    Right => Down,
                    Down  => Right,
                    Left  => Up,
                    Up    => Left,
                }
            ],

        /*  <-<   >->
         *
         *   v    <->
         *  <->    ^
         */
        Splitter(Horizontal) =>
            match curr_dir {
                Left | Right => vec![curr_dir],
                Up   | Down  => vec![Left, Right],
            },

        /*   v    ^
         *   |    |
         *   v    ^
         *
         *   ^    ^
         *  >|    |<
         *   v    v
         *
         */
        Splitter(Vertical) =>
            match curr_dir {
                Up   | Down  => vec![curr_dir],
                Left | Right => vec![Up, Down],
            },
    }
}

fn energized(history: LightHistory) -> HashSet<Pos> {
    history.into_iter()
        .map(|(pos, _dir)| pos)
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_data() -> Contraption {
        Contraption::parse(super::super::test_data(16))
    }

    #[test]
    fn part1() {
        assert_eq!(super::part1(&test_data()), 46);
    }
}
