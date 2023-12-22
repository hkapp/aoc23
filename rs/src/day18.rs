use crate::grid::{Direction, Pos, Plane, Grid};

pub fn run () {
    let instructions = || super::real_data(18);

    let answer1 = part1(instructions());
    println!("{}", answer1);
    assert_eq!(answer1, 45159);

    let answer2 = part2(instructions());
    println!("{}", answer2);
    //assert_eq!(answer2, 1268);
}

fn part1<I: Iterator<Item=String>>(text_instructions: I) -> usize {
    let instructions = text_instructions.map(|s| parse_line(&s));

    pool_surface(
        &mut dig_pool_border(instructions))
}

fn part2<I: Iterator<Item=String>>(text_instructions: I) -> usize {
    let instructions = text_instructions.map(|s| parse_line_hex(&s));

    pool_surface(
        &mut dig_pool_border(instructions))
}

type Dig = (Direction, usize);

fn parse_line(line: &str) -> Dig {
    let mut cs = line.chars();

    let dir = parse_dir(cs.next().unwrap());

    assert_eq!(cs.next(), Some(' '));

    let count = cs.take_while(|c| c.is_digit(10))
                    .collect::<String>()
                    .parse()
                    .unwrap();

    (dir, count)
}

fn parse_dir(c: char) -> Direction {
    use Direction::*;
    match c {
        'R' => Right,
        'L' => Left,
        'U' => Up,
        'D' => Down,
        _   => panic!("Unrecognized character {:?}", c),
    }
}

struct Digger {
    curr_pos: Pos,
    pool:     Quarry,
}

type Quarry = Plane<Terrain>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Terrain {
    Untouched,
    Tunnel,
    Lhs,
    Rhs
}

impl Digger {
    fn new() -> Self {
        let mut pool = Quarry::new();
        let start_pos = pool.origin();

        // The digger starts in a 1 meter cube hole in the ground
        pool.0.insert(start_pos, Terrain::Tunnel);

        Digger {
            curr_pos: start_pos,
            pool
        }
    }

    fn dig(&mut self, d: Dig) {
        println!("dig {:?}", d);
        let (dir, count) = d;
        for _ in 0..count {
            self.dig_one_meter(dir);
        }
    }

    fn dig_one_meter(&mut self, d: Direction) {
        use Terrain::*;

        let prev_pos = self.curr_pos;
        let new_pos = prev_pos.mov(d).unwrap();
        self.curr_pos = new_pos;

        self.pool.0.insert(new_pos, Terrain::Tunnel);

        let mut mark = |u, v| {
            let p = new_pos.mov(u).unwrap();
            match self.pool.0.get(&p) {
                None => { self.pool.0.insert(p, v); },
                Some(Tunnel) => {}, // don't mark dug tiles
                Some(Untouched) => unreachable!(), // this cannot happen at this stage, would be None
                Some(w) => assert_eq!(w, &v),
            }
        };

        mark(d.turn_left(),  Lhs);
        mark(d.turn_right(), Rhs);
    }
}

type LavaPool = Grid<Terrain>;

fn dig_pool_border<I: Iterator<Item=Dig>>(instructions: I) -> LavaPool {
    let mut worker = Digger::new();

    for w in instructions {
        worker.dig(w);
    }
    println!("Done with the instructions");

    worker.pool
        .into_grid(Terrain::Untouched)
        .unwrap()
}

fn paint_fill(pool: &mut LavaPool, side_yes: Terrain) {
    let mut queue = pool.enumerate()
                        .filter(|(_, t)| **t == side_yes)
                        .map(|(p, _)| p)
                        .collect::<Vec<_>>();

    if queue.len() % 1000000 == 0 {
        println!("queue size = {}", queue.len());
    }

    while !queue.is_empty() {
        let p = queue.pop().unwrap();
        let neighbors = IntoIterator::into_iter(Direction::all())
                            .filter_map(|d| p.move_within(d, pool))
                            .collect::<Vec<_>>();

        for n in neighbors {
            use Terrain::*;
            match pool[n] {
                Untouched => {
                    pool.set_pos(n, side_yes);
                    queue.push(n);
                }

                Tunnel => {},  // do nothing

                some_side => {
                    assert_eq!(some_side, side_yes);
                }
            }
        }
    }
}

fn pool_surface(pool: &mut LavaPool) -> usize {
    println!("Start paint fill 1");
    paint_fill(pool, Terrain::Lhs);
    println!("Start paint fill 2");
    paint_fill(pool, Terrain::Rhs);
    println!("Completed both paint fills");

    let side_in = find_side_in(pool);

    pool.iter()
        .filter(|t| **t == Terrain::Tunnel || **t == side_in)
        .count()
}

// Find the side that is inside
// Result is guaranteed to be either Lhs or Rhs
fn find_side_in(pool: &LavaPool) -> Terrain {
    use Terrain::{Lhs, Rhs};
    let side_out = pool.enumerate()
                    .filter(|(p, _)| pool.on_the_border(*p))
                    .map(|(_, t)| *t)
                    .find(|t| *t == Lhs || *t == Rhs)
                    .unwrap();

    // The side "inside" is the opposite of the side "outside"
    match side_out {
        Lhs => Rhs,
        Rhs => Lhs,
        _   => unreachable!(),
    }
}

fn parse_line_hex(line: &str) -> Dig {
    // R 6 (#70c710)
    let hex_section = line.split(" ").last().unwrap();

    // str range indexing works at the byte level
    // this code only makes sense if this is actually an ascii string
    assert!(hex_section.is_ascii());

    // (#70c710)
    let count_hex = &hex_section[2..7]; // hex_sections.chars().drop(2).take(5);
    let count = usize::from_str_radix(count_hex, 16).unwrap();

    let dir_hex = hex_section.chars().skip(7).next().unwrap();
    let dir = parse_dir_hex(dir_hex);

    (dir, count)
}

fn parse_dir_hex(c: char) -> Direction {
    use Direction::*;
    match c {
        '0' => Right,
        '1' => Down,
        '2' => Left,
        '3' => Up,
        _   => panic!("Unrecognized character {:?}", c),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_data() -> impl Iterator<Item=String> {
        super::super::test_data(18)
    }

    #[test]
    fn part1() {
        assert_eq!(super::part1(test_data()), 62);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(test_data()), 952408144115);
    }
}
