use std::collections::{HashSet, HashMap};
use crate::grid::{Grid, Pos, Direction, Plane};

pub fn run () {
    let garden = parse(super::real_data(21));

    let answer1 = part1(&garden, 64);
    println!("{}", answer1);
    assert_eq!(answer1, 3562);

    let answer2 = part2(garden, 1000);
    println!("Warning! not the correct answer: {}", answer2);
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

fn part2(garden: Garden, n: usize) -> usize {
    let inf_garden = InfGarden::from(garden);
    let mut walker = InfWalker::new(&inf_garden);

    // Note: with the final `count_reachable()` call,
    //       it DOES matter that we use the true 1-starting index
    for i in 1..(n+1) {
        expand_once_inf(&mut walker, &inf_garden, i);
    }

    walker.count_reachable(&inf_garden, n)
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

struct InfGarden {
    basic_garden: HashSet<Pos>,
    width:        usize,
    height:       usize,
    start_pos:    Pos,
}

fn inf_garden(basic_garden: Garden) -> InfGarden {
    // Note: we're guaranteed to have the entire right column
    // and bottom row be garden plots (walkable)
    // Note: we do `+ 1` because this ma value is the last, which is 0-indexed
    let width = 1 + basic_garden.garden_plots
                        .iter()
                        .map(Pos::col_idx)
                        .max()
                        .unwrap();

    let height = 1 + basic_garden.garden_plots
                        .iter()
                        .map(Pos::row_idx)
                        .max()
                        .unwrap();
    println!("width={}, height={}", width, height);

    // Compute the top-left position of the initial garden copy
    let mut origin = Plane::origin();
    while origin.x % height != 0 {
        origin.x += 1;
    }
    while origin.y % width != 0 {
        origin.y += 1;
    }
    println!("origin: {:?}", origin);

    println!("starting point: {:?} -> {:?}", basic_garden.start_pos,
                                            basic_garden.start_pos.vector_add(origin));

    InfGarden {
        basic_garden: basic_garden.garden_plots,
        start_pos:    basic_garden.start_pos.vector_add(origin),
        width,
        height
    }
}

impl InfGarden {
    fn rep_idx(&self, pos: Pos) -> RepIdx {
        (pos.x / self.height, pos.y / self.width)
    }

    fn from(basic_garden: Garden) -> Self {
        inf_garden(basic_garden)
    }

    fn is_walkable(&self, pos: Pos) -> bool {
        let mut re_origined = pos;
        re_origined.x %= self.height;
        re_origined.y %= self.width;

        self.basic_garden.contains(&re_origined)
    }
}

type RepIdx = (usize, usize);
type RepTracker = [usize; 2];

struct InfWalker {
    frontier:       HashSet<Pos>,
    repetitions:    HashMap<RepIdx, RepTracker>,
    stable_indexes: HashSet<RepIdx>
}

fn expand_once_inf(walker: &mut InfWalker, garden: &InfGarden, step_num: usize) {
    let new_frontier = walker.frontier
                            .iter()
                            .flat_map(|p| p.neighbors())
                            .filter(|neigh| garden.is_walkable(*neigh))
                            .filter(|neigh| !walker.pos_is_stable(*neigh, garden))
                            .collect::<HashSet<_>>();

    //show_3_by_3(&new_frontier, garden);

    for (rep_idx, count) in group_by_count(&new_frontier, garden) {
        walker.update(rep_idx, count, step_num);
    }

    //if 10usize.pow(step_num.ilog10()) as usize == step_num {
    if step_num % 100 == 0 {
        println!("Iteration {}:", step_num);
        println!(" frontier size = {}", new_frontier.len());
        println!(" stable count = {}", walker.stable_indexes.len());
    }

    walker.frontier = new_frontier;
}

fn show_3_by_3(frontier: &HashSet<Pos>, garden: &InfGarden) {
    let origin_row = garden.start_pos.row_idx() - (garden.start_pos.row_idx() % garden.height);
    let origin_col = garden.start_pos.col_idx() - (garden.start_pos.col_idx() % garden.width);

    let start_row = origin_row - garden.height;
    let start_col = origin_col - garden.width;
    let end_row = start_row + 3 * garden.height;
    let end_col = start_col + 3 * garden.width;

    for x in start_row..end_row {
        for y in start_col..end_col {
            let pos = Pos::new(x, y);

            let c =
                if !garden.is_walkable(pos) {
                    '#'
                }
                else if frontier.contains(&pos) {
                    'O'
                }
                else {
                    '.'
                };

            print!("{}", c);
        }
        println!();
    }
}

impl InfWalker {
    fn new(garden: &InfGarden) -> Self {
        let mut frontier = HashSet::new();
        frontier.insert(garden.start_pos);

        InfWalker {
            frontier,
            repetitions:    HashMap::new(),
            stable_indexes: HashSet::new(),
        }
    }

    fn update(&mut self, rep_idx: RepIdx, rep_count: usize, step_num: usize) {
        assert!(!self.stable_indexes.contains(&rep_idx));

        let par_idx = step_num % 2;
        let rep_tracker = self.repetitions.entry(rep_idx).or_insert([0, 0]);

        if rep_tracker[par_idx] == rep_count {
            // We reached stability
            // Mark it appropriately
            self.stable_indexes.insert(rep_idx);
        }
        else {
            // Update the tracker
            rep_tracker[par_idx] = rep_count;
        }
        /*match self.repetitions.get_mut(&rep_idx) {
            // This is a newly entered repetition
            // Initialize the repetitions entry
            None => {
                let mut new_entry = [0, 0];
                new_entry[par_idx] = rep_count;
            }
            // Proper stability detection logic
            Some(rep_tracker) => {
                if rep_tracker[par_idx] == rep_count {
                    // We reached stability
                    // Mark it appropriately
                    self.stable_indexes.insert(rep_idx);
                    println!("{:?} stabilized", rep_idx);
                }
                else {
                    // Update the tracker
                    rep_tracker[par_idx] = rep_count;
                }
            }
        }*/
    }

    fn pos_is_stable(&self, pos: Pos, garden: &InfGarden) -> bool {
        let rep_idx = garden.rep_idx(pos);
        self.stable_indexes.contains(&rep_idx)
    }

    fn count_reachable(&self, garden: &InfGarden, step_num: usize) -> usize {
        let par_idx = step_num % 2;
        let stable_count: usize = self.stable_indexes
                                    .iter()
                                    .map(|rep_idx| self.repetitions[rep_idx][par_idx])
                                    .sum();

        // Note: reps that just stabilized have not been filtered in the frontier
        let moving_count = self.frontier
                                .iter()
                                .filter(|p| !self.pos_is_stable(**p, garden))
                                .count();

        stable_count + moving_count
    }
}

fn group_by_count(poss: &HashSet<Pos>, inf_garden: &InfGarden) -> HashMap<RepIdx, usize> {
    let mut counts = HashMap::new();
    for pos in poss.iter() {
        let rep_idx = inf_garden.rep_idx(*pos);
        let this_group = counts.entry(rep_idx).or_insert(0);
        *this_group += 1;
    }
    return counts;
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

    fn part2(n: usize, expected: usize) {
        assert_eq!(super::part2(test_data(), n), expected);
    }

    #[test]
    fn part2_6() {
        part2(6, 16)
    }

    #[test]
    fn part2_10() {
        part2(10, 50)
    }

    #[test]
    fn part2_50() {
        part2(50, 1594)
    }

    #[test]
    fn part2_100() {
        part2(100, 6536)
    }

    #[test]
    fn part2_500() {
        part2(500, 167004)
    }

    #[test]
    fn part2_1000() {
        part2(1000, 668697)
    }

    #[test]
    #[ignore]
    fn part2_5000() {
        part2(5000, 16733044)
    }
}
