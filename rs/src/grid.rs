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
    pub fn rows(&self) -> impl Iterator<Item=&Vec<T>> {
        self.tiles.iter()
    }

    pub fn row_count(&self) -> usize {
        self.tiles.len()
    }

    pub fn col_count(&self) -> usize {
        self.tiles[0].len()
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
}

pub trait CharTile {
    fn from_char(c: &char) -> Self;
    fn to_char(&self) -> char;
}

/***** Direction *****/

#[derive(Copy, Clone)]
pub enum Direction {
    Left, Right, Up, Down
}

impl Direction {
    fn as_vector(&self) -> (isize, isize) {
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
}

/***** Pos *****/

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Pos {
    pub x: usize,
    pub y: usize,
}

impl Pos {
    pub fn new(x: usize, y: usize) -> Self {
        Pos { x, y }
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
}
