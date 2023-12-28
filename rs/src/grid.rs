use std::collections::HashMap;

/*  --y-->
 * |
 * x
 * |
 * v
 *
 * First level of indexing is x
 * Second level of indexing is y
 */
#[derive(PartialEq, Eq)]
pub struct Grid<T> {
    tiles: Vec<Vec<T>>
}

impl<T: CharTile> Grid<T> {
    pub fn parse<I: Iterator<Item=String>>(lines: I) -> Self {
        let parse_row = |s: String| {
            s.chars()
                .map(|c| T::from_char(&c))
                .collect()
        };

        Grid {
            tiles: lines
                    .map(parse_row)
                    .collect()
        }
    }

    #[allow(dead_code)]
    pub fn show(&self) {
        for row in self.rows() {
            for tile in row.iter() {
                print!("{}", tile.to_char());
            }
            println!();
        }
    }
}

impl<T> Grid<T> {
    #[allow(dead_code)]
    pub fn rows(&self) -> impl Iterator<Item=&Vec<T>> {
        self.tiles.iter()
    }

    pub fn height(&self) -> usize {
        self.tiles.len()
    }

    pub fn width(&self) -> usize {
        self.tiles[0].len()
    }

    pub fn row_count(&self) -> usize {
        self.height()
    }

    pub fn col_count(&self) -> usize {
        self.width()
    }

    pub fn get_pos(&self, pos: Pos) -> Option<&T> {
        self.tiles
            .get(pos.x)
            .and_then(|row| row.get(pos.y))
    }

    fn get_pos_mut(&mut self, pos: Pos) -> Option<&mut T> {
        self.tiles
            .get_mut(pos.x)
            .and_then(|row| row.get_mut(pos.y))
    }

    #[allow(dead_code)]
    pub fn set_pos(&mut self, pos: Pos, value: T) {
        let target = self.get_pos_mut(pos).unwrap();
        *target = value;
    }

    pub fn swap(&mut self, pos1: Pos, pos2: Pos) -> Result<(), String> {
        if pos1 == pos2 {
            return Err(String::from("src == dst"));
        }

        let mut get_ptr = |p| {
            self.get_pos_mut(p)
                .map(|r| r as *mut T)
                .ok_or_else(|| String::from("Index out of bounds"))
        };

        let ptr1 = get_ptr(pos1)?;
        let ptr2 = get_ptr(pos2)?;

        unsafe {
            std::mem::swap(&mut *ptr1, &mut *ptr2);
        }
        Ok(())
    }

    pub fn all_pos(&self) -> impl Iterator<Item=Pos> {
        let row_count = self.row_count();
        let col_count = self.col_count();
        (0..row_count)
            .flat_map(move |x| {
                (0..col_count)
                    .map(move |y| Pos::new(x, y))
            })
    }

    pub fn enumerate(&self) -> impl Iterator<Item=(Pos, &T)> {
        self.tiles
            .iter()
            .enumerate()
            .flat_map(|(x, row)| {
                row.iter()
                    .enumerate()
                    .map(move |(y, tile)| (Pos::new(x, y), tile))
            })
    }

    #[allow(dead_code)]
    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.tiles
            .iter()
            .flat_map(|row| row.iter())
    }

    #[allow(unused_comparisons)]
    pub fn contains_pos(&self, p: Pos) -> bool {
        p.x >= 0
        && p.y >= 0
        && p.row_idx() < self.row_count()
        && p.col_idx() < self.col_count()
    }

    pub fn bottom_right_corner(&self) -> Pos {
        Pos::from_row_col(self.row_count() - 1, self.col_count() - 1)
    }

    #[allow(dead_code)]
    pub fn on_the_border(&self, pos: Pos) -> bool {
        (pos.x == 0 || pos.x == self.height() - 1)
        && (pos.y == 0 || pos.y == self.width() - 1)
    }
}

pub trait CharTile {
    fn from_char(c: &char) -> Self;
    fn to_char(&self) -> char;
}

impl CharTile for char {
    fn from_char(c: &char) -> Self {
        *c
    }

    fn to_char(&self) -> char {
        *self
    }
}

impl<T> std::ops::Index<Pos> for Grid<T> {
    type Output = T;

    fn index(&self, index: Pos) -> &Self::Output {
        self.get_pos(index).unwrap()
    }
}

impl<T: Clone> Grid<T> {
    #[allow(dead_code)]
    pub fn init(height: usize, width: usize, repeated_elem: T) -> Self {
        let repeated_row = vec![repeated_elem; width];
        let all_data = vec![repeated_row; height];
        Grid { tiles: all_data }
    }
}

/***** Pos *****/

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, PartialOrd, Ord)]
pub struct Pos {
    pub x: usize,
    pub y: usize,
}

impl Pos {
    pub fn new(x: usize, y: usize) -> Self {
        Pos { x, y }
    }

    pub fn from_row_col(row_idx: usize, col_idx: usize) -> Self {
        Self::new(row_idx, col_idx)
    }

    pub fn mov(&self, d: Direction) -> Option<Self> {
        let (xdiff, ydiff) = d.as_vector();

        self.x
            .checked_add_signed(xdiff)
            .and_then(|x| {
                self.y
                    .checked_add_signed(ydiff)
                    .map(|y| Pos::new(x, y))
            })
    }

    pub fn move_within<T>(&self, d: Direction, g: &Grid<T>) -> Option<Self> {
        self.mov(d)
            .and_then(|new| {
                if g.contains_pos(new) {
                    Some(new)
                }
                else {
                    None
                }
            })
    }

    pub fn row_idx(&self) -> usize {
        self.x
    }

    pub fn col_idx(&self) -> usize {
        self.y
    }
}

/***** Direction *****/

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Direction {
    Left, Right, Up, Down
}

impl Direction {
    pub fn as_vector(&self) -> (isize, isize) {
        use Direction::*;
        match self {
            Left  => (0, -1),
            Right => (0,  1),
            Up    => (-1, 0),
            Down  => ( 1, 0),
        }
    }

    pub fn north() -> Self {
        Direction::Up
    }

    pub fn south() -> Self {
        Direction::Down
    }

    pub fn east() -> Self {
        Direction::Right
    }

    pub fn west() -> Self {
        Direction::Left
    }

    pub fn all() -> [Self; 4] {
        use Direction::*;
        [Left, Right, Up, Down]
    }

    pub fn reverse(&self) -> Self {
        use Direction::*;
        match self {
            Left  => Right,
            Right => Left,
            Up    => Down,
            Down  => Up,
        }
    }

    pub fn turn_left(&self) -> Self {
        /*    ^
         *    |
         * <--+-->
         *    |
         *    v
         */
        use Direction::*;
        match self {
            Up    => Left,
            Left  => Down,
            Down  => Right,
            Right => Up,
        }
    }

    pub fn turn_right(&self) -> Self {
        /*    ^
         *    |
         * <--+-->
         *    |
         *    v
         */
        use Direction::*;
        match self {
            Up    => Right,
            Right => Down,
            Down  => Left,
            Left  => Up,
        }
    }

    pub fn rotate_left(&self) -> Self {
        self.turn_left()
    }

    pub fn rotate_right(&self) -> Self {
        self.turn_right()
    }
}

/***** Plane *****/
// An infinite-looking Grid

pub struct Plane<T> (pub HashMap<Pos, T>);

impl<T> Plane<T> {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Plane(HashMap::new())
    }

    pub fn origin() -> Pos {
        // Note: Pos coordinates are usize, not isize
        // Use the middle of the usize range as origin
        let o = 1usize.rotate_right(1);
        Pos::new(o, o)
    }
}

impl<T: Clone> Plane<T> {
    #[allow(dead_code)]
    pub fn into_grid(self, default_elem: T) -> Option<Grid<T>> {
        let all_pos = || self.0.keys();
        let all_xs = || all_pos().map(|p| p.x);
        let all_ys = || all_pos().map(|p| p.y);

        let top_border = all_xs().min()?;
        let bottom_border = all_xs().max()?;
        let left_border = all_ys().min()?;
        let right_border = all_ys().max()?;

        let width  = right_border - left_border + 1;
        let height = bottom_border - top_border + 1;

        let mut grid = Grid::init(height, width, default_elem);

        for (plane_pos, value) in self.0 {
            let grid_x = plane_pos.x - top_border;
            let grid_y = plane_pos.y - left_border;
            let grid_pos = Pos::new(grid_x, grid_y);
            grid.set_pos(grid_pos, value);
        }

        Some(grid)
    }
}
