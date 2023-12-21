use crate::grid::{self, Grid, Direction, Pos};
use std::collections::{BinaryHeap, HashSet};
use std::cmp::{Ordering, Reverse};
use std::hash::Hash;

pub fn run () {
    let map = CityMap::parse(super::real_data(17));

    let answer1 = part1(&map);
    println!("{}", answer1);
    assert_eq!(answer1, 1128);

    let answer2 = part2(&map);
    println!("{}", answer2);
    assert_eq!(answer2, 1268);
}

fn part1(map: &CityMap) -> HeatLoss {
    let init_inertia = Crucible {
        last_dir:  Direction::Right,  // note that this allows going down
        dir_count: 0,
    };

    minimize_heat_loss(map, init_inertia)
}

fn part2(map: &CityMap) -> HeatLoss {
    let init_inertia = UltraCrucible {
        last_dir:  Direction::Right,  // note that this allows going down
        dir_count: 0,
    };

    minimize_heat_loss(map, init_inertia)
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

#[derive(PartialEq, Eq, Debug, Clone)]
struct Path<T> {
    heat_loss:   HeatLoss,
    inertia:     T,
    curr_pos:    Pos,
}

fn minimize_heat_loss<T>(map: &CityMap, init_inertia: T) -> HeatLoss
    where T: Inertia + Ord + Hash + Clone
{
    let mut min_heap = BinaryHeap::new();
    let mut visited = HashSet::new();

    let start_pos = Pos::new(0, 0);
    let start_state = Path {
        heat_loss: 0,
        inertia:   init_inertia,
        curr_pos:  start_pos,
    };
    min_heap.push(Reverse(start_state));

    let destination = map.bottom_right_corner();
    let is_final = |path: &Path<_>| {
        path.curr_pos == destination
    };

    // If we cross again the same unique state (i.e. without the heat loss),
    // we don't have to extend it again
    let into_dedup = |p: &Path<_>| (p.curr_pos, T::clone(&p.inertia));

    loop {
        let curr_path = min_heap.pop().unwrap().0;

        let uniq_det = into_dedup(&curr_path);
        if visited.contains(&uniq_det) {
            continue;
        }
        else {
            visited.insert(uniq_det);
        }

        let dirs = curr_path.inertia.next_dirs(); // extension_dirs(&curr_path);
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

fn extend_path<T: Inertia>(prev_path: &Path<T>, new_inertia: T, map: &CityMap) -> Option<Path<T>> {
    prev_path
        .curr_pos
        .move_within(new_inertia.as_direction(), map)
        .map(|new_pos| {
            Path {
                heat_loss: prev_path.heat_loss + map[new_pos],
                inertia:   new_inertia,
                curr_pos: new_pos
            }
        })
}

impl<T: PartialOrd + Clone> PartialOrd for Path<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // Compare first by heat loss so far,
        // then by "how far we went already"
        // Considering that we don't have a CityMap here,
        // we must approximate the latter by the sum of the x and y
        // coordinates, knowing that the destination is always the bottom right corner.
        // Note that computing the manhattan distance of the
        // destination to the current position would give basically the same value.
        let into_tuple = |path: &Path<_>| {
            (
                path.heat_loss,
                path.curr_pos.x + path.curr_pos.y,
                T::clone(&path.inertia),
                path.curr_pos // still important to differentiate between path of different curr_pos but same sum of x and y
            )
        };

        into_tuple(self).partial_cmp(&into_tuple(other))
    }
}

impl<T: Ord + Clone> Ord for Path<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

trait Inertia {
    fn next_dirs(&self) -> Vec<Self>
        where Self: Sized;
    fn as_direction(&self) -> Direction;
    fn can_stop(&self) -> bool;
}

#[derive(Hash, Ord, PartialEq, Eq, PartialOrd, Clone)]
struct Crucible {
    last_dir:  Direction,
    dir_count: u8,
}

impl Inertia for Crucible {
    fn next_dirs(&self) -> Vec<Self> {
        let curr_dir = self.last_dir;
        let mut next_dirs = Vec::new();

        next_dirs.push(curr_dir.turn_left());
        next_dirs.push(curr_dir.turn_right());

        if self.dir_count < 3 {
            next_dirs.push(curr_dir);
        }

        // Calculate the dir_count
        next_dirs.into_iter()
            .map(|d| {
                Crucible {
                    last_dir: d,
                    dir_count:
                        if d == curr_dir {
                            self.dir_count + 1
                        }
                        else {
                            1
                        },
                }
            })
            .collect()
    }

    fn as_direction(&self) -> Direction {
        self.last_dir
    }

    fn can_stop(&self) -> bool {
        // In the first part, we can always stop
        true
    }
}

#[derive(Hash, Ord, PartialEq, Eq, PartialOrd, Clone)]
struct UltraCrucible {
    last_dir:  Direction,
    dir_count: u8,
}

impl Inertia for UltraCrucible {
    fn next_dirs(&self) -> Vec<Self> {
        if self.dir_count < 4 {
            let same_dir = UltraCrucible {
                last_dir:  self.last_dir,
                dir_count: self.dir_count + 1
            };
            vec![same_dir]
        }
        else {
            let curr_dir = self.last_dir;
            let mut next_dirs = Vec::new();

            next_dirs.push(curr_dir.turn_left());
            next_dirs.push(curr_dir.turn_right());

            if self.dir_count < 10 {
                next_dirs.push(curr_dir);
            }

            // Calculate the dir_count
            next_dirs.into_iter()
                .map(|d| {
                    UltraCrucible {
                        last_dir: d,
                        dir_count:
                            if d == curr_dir {
                                self.dir_count + 1
                            }
                            else {
                                1
                            },
                    }
                })
                .collect()
        }
    }

    fn as_direction(&self) -> Direction {
        self.last_dir
    }

    fn can_stop(&self) -> bool {
        self.dir_count >= 4
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

    #[test]
    fn part2() {
        assert_eq!(super::part2(&test_data()), 94);
    }
}
