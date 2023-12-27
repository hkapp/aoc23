use crate::grid::{Direction, Pos, Plane};

pub fn run () {
    let instructions = || super::real_data(18);

    let answer1 = part1(instructions());
    println!("{}", answer1);
    assert_eq!(answer1, 45159);

    let answer2 = part2(instructions());
    println!("{}", answer2);
    assert_eq!(answer2, 134549294799713);
}

fn part1<I: Iterator<Item=String>>(text_instructions: I) -> usize {
    let instructions = text_instructions.map(|s| parse_line(&s));

    let pool = dig_pool(instructions);
    pool_surface(&pool)
}

fn part2<I: Iterator<Item=String>>(text_instructions: I) -> usize {
    let instructions = text_instructions.map(|s| parse_line_hex(&s));

    let pool = dig_pool(instructions);
    pool_surface(&pool)
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
    lhs:         Vec<Segment>,
    rhs:         Vec<Segment>,
}

// Ranges are inclusive
#[derive(Copy, Clone)]
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
            lhs:         Vec::new(),
            rhs:         Vec::new(),
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

        let lhs_mark = new_segment.translate(dir.rotate_left());
        self.lhs.push(lhs_mark);

        let rhs_mark = new_segment.translate(dir.rotate_right());
        self.rhs.push(rhs_mark);
    }
}

struct LavaPool {
    border: Vec<Segment>,
    inside: Vec<Segment>
}

fn dig_pool<I: Iterator<Item=Dig>>(instructions: I) -> LavaPool {
    let mut worker = Digger::new();

    for w in instructions {
        worker.dig(w);
    }

    let pool_border = worker.pool_border;
    let side_in = find_side_in(&pool_border, worker.lhs, worker.rhs);

    LavaPool {
        border: pool_border,
        inside: side_in,
    }
}

fn find_side_in(pool_border: &[Segment], lhs: Vec<Segment>, rhs: Vec<Segment>) -> Vec<Segment> {
    /* Here is the method we use to figure out the side inside the loop:
     * We rather find the side outside the loop. The other is then guaranteed to be inside.
     * We find the outside by:
     *  1. generating a point just above the highest point out of all the segments
     *  2. figuring out on which side this point lies
     * This is guaranteed to be the outside.
     * It is also guaranteed to be "caught" by our lhs/rhs marks.
     */
    let highest_point = pool_border.into_iter()
                            .flat_map(|s| IntoIterator::into_iter(s.edges()))
                            .min_by_key(|p| p.x)
                            .unwrap();
    let tested_point = highest_point.mov(Direction::Up).unwrap();

    let lhs_contains = segments_contain(tested_point, &lhs);
    let rhs_contains = segments_contain(tested_point, &rhs);

    match (lhs_contains, rhs_contains) {
        (true, false)  => rhs,
        (false, true)  => lhs,
        (true, true)   => panic!("Both sides contain"),
        (false, false) => panic!("No side contains"),
    }
}

fn segments_contain(point: Pos, segments: &[Segment]) -> bool {
    segments.into_iter()
        .any(|s| s.contains(point))
}

fn pool_surface(pool: &LavaPool) -> usize {
    let (x_min, x_max) = min_max_x(&pool.border);

    let mut surface = 0;
    for x in x_min..(x_max + 1) {
        let mut intersection_ys = pool.border
                                    .iter()
                                    .flat_map(|s| s.row_intersections(x).into_iter())
                                    .collect::<Vec<_>>();
        intersection_ys.sort_unstable();

        let range_len = |a, b| b - a + 1;

        let mut unseen_y = None;
        for (y_start, y_end) in intersection_ys {
            match unseen_y {
                // First range: simple case
                None => {
                    //surface += y_end - y_start + 1;
                    surface += range_len(y_start, y_end);
                }

                Some(unseen_y) => {
                    // Case 1: the last unseen y is beyond the end of the current range
                    // This can only happen if the previous was an overlap
                    if unseen_y > y_end {
                        assert_eq!(unseen_y, y_end + 1);
                    }
                    // Case 2: the previous y overlaps the current range
                    else if unseen_y >= y_start && unseen_y <= y_end {
                        surface += range_len(unseen_y, y_end);
                    }
                    else {
                        // Case 3: we're in between ranges
                        // Count the range *if* it's inside
                        let range_is_inside = segments_contain(Pos::new(x, unseen_y), &pool.inside);
                        if range_is_inside {
                            surface += range_len(unseen_y, y_start - 1);
                        }
                        surface += range_len(y_start, y_end);
                    }
                }
            }
            unseen_y = Some(y_end + 1);
        }
    }

    return surface;
}

impl Segment {
    // Return the y range that crosses row x
    // For any segment, there will be between 0 and 2 such points
    // For vertical segments that cross, the returned range has width 1
    fn row_intersections(&self, x_search: usize) -> Option<(usize, usize)> {
        match self {
            Segment::Horizontal { x, y1, y2 } => {
                if *x == x_search {
                    Some((*y1, *y2))
                }
                else {
                    None
                }
            }

            Segment::Vertical { x1, x2, y } => {
                if x_search >= *x1 && x_search <= *x2 {
                    Some((*y, *y))
                }
                else {
                    None
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

    fn contains(&self, pos: Pos) -> bool {
        match self {
            Segment::Horizontal { x, y1, y2 } => pos.x == *x && (pos.y >= *y1 && pos.y <= *y2),
            Segment::Vertical   { x1, x2, y } => pos.y == *y && (pos.x >= *x1 && pos.x <= *x2),
        }
    }

    fn edges(&self) -> [Pos; 2] {
        match self {
            Segment::Horizontal { x, y1, y2 } => [Pos::new(*x, *y1), Pos::new(*x, *y2)],
            Segment::Vertical   { x1, x2, y } => [Pos::new(*x1, *y), Pos::new(*x2, *y)],
        }
    }

    fn translate(&self, dir: Direction) -> Self {
        let (x_diff, y_diff) = dir.as_vector();
        match self {
            Segment::Horizontal { x, y1, y2 } => {
                Segment::Horizontal {
                    x:  x.checked_add_signed(x_diff).unwrap(),
                    y1: y1.checked_add_signed(y_diff).unwrap(),
                    y2: y2.checked_add_signed(y_diff).unwrap(),
                }
            }
            Segment::Vertical   { x1, x2, y } => {
                Segment::Vertical {
                    x1: x1.checked_add_signed(x_diff).unwrap(),
                    x2: x2.checked_add_signed(x_diff).unwrap(),
                    y:  y.checked_add_signed(y_diff).unwrap(),
                }
            }
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
