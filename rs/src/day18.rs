use crate::grid::{Direction, Pos, Plane};

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
    curr_pos:    Pos,
    pool_border: Vec<Segment>,
}

// Ranges are inclusive
enum Segment {

    /*  y1        y2
     * x **********
     */
    Horizontal { x: usize, y1: usize, y2: usize },

    /*     y
     *  x1 *
     *     *
     *     *
     *  x2 *
     */
    Vertical   { x1: usize, x2: usize, y: usize },
}

impl Digger {
    fn new() -> Self {
        Digger {
            curr_pos:    Plane::<()>::origin(),
            pool_border: Vec::new(),
        }
    }

    fn dig(&mut self, d: Dig) {
        use Direction::*;
        let (dir, count) = d;

        /*   y-->
         *  x
         *  |
         *  v
         */
        let curr_pos = self.curr_pos;
        let curr_x = curr_pos.x;
        let curr_y = curr_pos.y;
        let (new_pos, new_segment) =
            match dir {
                Right => {
                    let new_y = curr_y + count;
                    let new_pos = Pos::new(curr_x, new_y);
                    let segment = Segment::Horizontal { x: curr_x, y1: curr_y, y2: new_y };
                    (new_pos, segment)
                },

                Left => {
                    let new_y = curr_y - count;
                    let new_pos = Pos::new(curr_x, new_y);
                    let segment = Segment::Horizontal { x: curr_x, y1: new_y, y2: curr_y };
                    (new_pos, segment)
                },

                Down => {
                    let new_x = curr_x + count;
                    let new_pos = Pos::new(new_x, curr_y);
                    let segment = Segment::Vertical { x1: curr_x, x2: new_x, y: curr_y };
                    (new_pos, segment)
                },

                Up => {
                    let new_x = curr_x - count;
                    let new_pos = Pos::new(new_x, curr_y);
                    let segment = Segment::Vertical { x1: new_x, x2: curr_x, y: curr_y };
                    (new_pos, segment)
                },
            };

        self.curr_pos = new_pos;
        self.pool_border.push(new_segment);
    }
}

type LavaPool = Vec<Segment>;

fn dig_pool_border<I: Iterator<Item=Dig>>(instructions: I) -> LavaPool {
    let mut worker = Digger::new();

    for w in instructions {
        worker.dig(w);
    }

    worker.pool_border
}

fn pool_surface(pool: &LavaPool) -> usize {
    let (x_min, x_max) = min_max_x(&pool);

    let mut surface = 0;
    for x in x_min..(x_max + 1) {
        let mut intersection_ys = pool.into_iter()
                                    .flat_map(|s| s.row_intersections(x).into_iter())
                                    .collect::<Vec<_>>();
        intersection_ys.sort_unstable();
        // Note: shared corners will generate duplicates
        intersection_ys.dedup();
        println!("ys = {:?}", &intersection_ys);

        let mut sorted_ys = intersection_ys.into_iter();
        while let Some(left_y) = sorted_ys.next() {
            let right_y = sorted_ys.next().unwrap();
            surface += right_y - left_y + 1;
        }
    }

    return surface;
}

impl Segment {
    // Return the y values that cross row x
    // For any segment, there will be between 0 and 2 such points
    // Note that for horizontal segments with value x, we return the left-most and right-most points only
    fn row_intersections(&self, x_search: usize) -> Vec<usize> {
        match self {
            Segment::Horizontal { x, y1, y2 } => {
                if *x == x_search {
                    /* Patch the special case
                     *
                     *   ***    *
                     *
                     * Without patch, we get an odd number of ys, which is normally not possible
                     */
                    let range_len = y2 - y1 + 1;
                    if range_len % 2 == 0 {
                        println!("{} x (x: {}, y1: {}, y2: {}) = [{}, {}]",
                            x_search, x, y1, y2, y1, y2);
                        vec![*y1, *y2]
                    }
                    else {
                        println!("{} x (x: {}, y1: {}, y2: {}) = [{}, {}, {}]",
                            x_search, x, y1, y2, y1, y2-1, y2);
                        vec![*y1, y2-1, *y2]
                    }
                }
                else {
                    println!("{} x (x: {}, y1: {}, y2: {}) = []",
                        x_search, x, y1, y2);
                    Vec::new()
                }
            }

            Segment::Vertical { x1, x2, y } => {
                if x_search >= *x1 && x_search <= *x2 {
                    println!("{} x (x1: {}, x2: {}, y: {}) = [{}]",
                        x_search, x1, x2, y, y);
                    vec![*y]
                }
                else {
                    println!("{} x (x1: {}, x2: {}, y: {}) = []",
                        x_search, x1, x2, y);
                    Vec::new()
                }
            }
        }
    }

    fn x_range(&self) -> Vec<usize> {
        match self {
            Segment::Horizontal { x, .. } => vec![*x],
            Segment::Vertical { x1, x2, .. } => vec![*x1, *x2],
        }
    }
}

fn min_max_x(pool: &[Segment]) -> (usize, usize) {
    let xs = || pool.iter()
                    .flat_map(|s| s.x_range().into_iter());
    let min = xs().min().unwrap();
    let max = xs().max().unwrap();
    (min, max)
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
