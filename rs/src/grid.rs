/*  --y-->
 * |
 * x
 * |
 * v
 *
 * First level of indexing is x
 * Second level of indexing is y
 */
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
    fn rows(&self) -> impl Iterator<Item=&Vec<T>> {
        self.tiles.iter()
    }
}

pub trait CharTile {
    fn from_char(c: &char) -> Self;
    fn to_char(&self) -> char;
}
