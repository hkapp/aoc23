use crate::grid::{self, Grid, Direction, Pos};
use std::collections::{BinaryHeap, HashSet};
use std::cmp::{Ordering, Reverse};

pub fn run () {
    let map = CityMap::parse(super::real_data(17));

    let answer1 = part1(&map);
    println!("{}", answer1);
    //assert_eq!(answer1, 7939);

    //let answer2 = part2(&c);
    //println!("{}", answer2);
    //assert_eq!(answer2, 8318);
}

fn part1(map: &CityMap) -> HeatLoss {
    minimize_heat_loss(map)
}

type CityMap = Grid<HeatLoss>;
type HeatLoss = u32;

impl grid::CharTile for HeatLoss {
    fn from_char(c: &char) -> Self {
        c.to_digit(10).unwrap() as HeatLoss
    }

    fn to_char(&self) -> char {
        char::from_digit(*self, 10).unwrap()
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
struct Path {
    heat_loss:   HeatLoss,
    last_dir:    Direction,
    dir_count:   u8,
    curr_pos:    Pos,
}

fn minimize_heat_loss(map: &CityMap) -> HeatLoss {
    let mut min_heap = BinaryHeap::new();
    let mut visited = HashSet::new();

    let start_pos = Pos::new(0, 0);
    let start_state = Path {
        heat_loss: 0,
        last_dir:  Direction::Right,  // note that this allows going down
        dir_count: 0,
        curr_pos:  start_pos,
    };
    min_heap.push(Reverse(start_state));

    let destination = map.bottom_right_corner();
    let is_final = |path: &Path| {
        path.curr_pos == destination
    };

    // If we cross again the same unique state (i.e. without the heat loss),
    // we don't have to extend it again
    let into_dedup = |p: &Path| (p.last_dir, p.dir_count, p.curr_pos);

    loop {
        let curr_path = min_heap.pop().unwrap().0;

        let uniq_det = into_dedup(&curr_path);
        if visited.contains(&uniq_det) {
            continue;
        }
        else {
            visited.insert(uniq_det);
        }

        let dirs = extension_dirs(&curr_path);
        let candidates = dirs.into_iter()
                            .filter_map(|d| extend_path(&curr_path, d, map));

        for new_path in candidates {
            // The first path that reaches the destination position is guaranteed to be minimizing heat loss
            if is_final(&new_path) {
                return new_path.heat_loss;
            }
            else {
                min_heap.push(Reverse(new_path));
            }
        }
    }
}

fn extension_dirs(path: &Path) -> Vec<Direction> {
    let curr_dir = path.last_dir;
    let mut next_dirs = Vec::new();

    next_dirs.push(curr_dir.turn_left());
    next_dirs.push(curr_dir.turn_right());

    if path.dir_count < 3 {
        next_dirs.push(curr_dir);
    }

    return next_dirs;
}

fn extend_path(prev_path: &Path, d: Direction, map: &CityMap) -> Option<Path> {
    prev_path
        .curr_pos
        .move_within(d, map)
        .map(|new_pos| {
            Path {
                heat_loss: prev_path.heat_loss + map[new_pos],
                last_dir:  d,
                dir_count:
                    if d == prev_path.last_dir {
                        prev_path.dir_count + 1
                    }
                    else {
                        1
                    },
                curr_pos: new_pos
            }
        })
}

impl PartialOrd for Path {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // Compare first by heat loss so far,
        // then by "how far we went already"
        // Considering that we don't have a CityMap here,
        // we must approximate the latter by the sum of the x and y
        // coordinates, knowing that the destination is always the bottom right corner.
        // Note that computing the manhattan distance of the
        // destination to the current position would give basically the same value.
        let into_tuple = |path: &Path| {
            (
                path.heat_loss,
                path.curr_pos.x + path.curr_pos.y,
                path.last_dir,
                path.dir_count,
                path.curr_pos // still important to differentiate between path of different curr_pos but same sum of x and y
            )
        };

        into_tuple(self).partial_cmp(&into_tuple(other))
    }
}

impl Ord for Path {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_data() -> CityMap {
        CityMap::parse(super::super::test_data(17))
    }

    #[test]
    fn part1() {
        assert_eq!(super::part1(&test_data()), 102);
    }
}
