use std::{
    convert::{TryInto, TryFrom},
    env,
    fs,
    fmt,
    ops::{Add, Deref, DerefMut, RangeInclusive},
    process,
    str::FromStr,
};

fn parse_delimited<T, I>(raw: &str, delimiter: &str) -> Result<T, String>
where
    T: TryFrom<Vec<I>>,
    I: FromStr {
    raw
        .split(delimiter)
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(|s| s.parse().map_err(|_| format!("Could not parse '{s}' as value")))
        .collect::<Result<Vec<I>, _>>()?
        .try_into()
        .map_err(|_| format!("Could not parse '{raw}' into collection"))
}

fn intersects<T>(lhs: RangeInclusive<T>, rhs: RangeInclusive<T>) -> bool
    where T: Ord {
    lhs.end() >= rhs.start() && lhs.start() <= rhs.end()
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Vec3 {
    x: i32,
    y: i32,
    z: i32,
}

impl Vec3 {
    fn min(self, rhs: Self) -> Self { Self { x: self.x.min(rhs.x), y: self.y.min(rhs.y), z: self.z.min(rhs.z) } }

    fn max(self, rhs: Self) -> Self { Self { x: self.x.max(rhs.x), y: self.y.max(rhs.y), z: self.z.max(rhs.z) } }
}

impl Add<Vec3> for Vec3 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self { x: self.x + rhs.x, y: self.y + rhs.y, z: self.z + rhs.z }
    }
}

impl FromStr for Vec3 {
    type Err = String;

    fn from_str(raw: &str) -> Result<Self, String> {
        let [x, y, z] = parse_delimited(raw, ",")?;
        Ok(Self { x, y, z })
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Id<T> {
    id: usize,
    value: T,
}

impl<T> Deref for Id<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> DerefMut for Id<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Brick {
    start: Vec3,
    end: Vec3,
}

impl Brick {
    fn fall(&self) -> Self {
        let delta = Vec3 { x: 0, y: 0, z: -1 };
        Self { start: self.start + delta, end: self.end + delta }
    }

    fn x_range(&self) -> RangeInclusive<i32> { self.start.x..=self.end.x }

    fn y_range(&self) -> RangeInclusive<i32> { self.start.y..=self.end.y }

    fn z_range(&self) -> RangeInclusive<i32> { self.start.z..=self.end.z }

    fn x_aligned(&self) -> bool { self.start.y == self.end.y && self.start.z == self.end.z }

    fn y_aligned(&self) -> bool { self.start.x == self.end.x && self.start.z == self.end.z }

    fn z_aligned(&self) -> bool { self.start.x == self.end.x && self.start.y == self.end.y }

    fn on_ground(&self) -> bool {
        assert!(self.start.z >= 0 && self.end.z >= 0);
        self.start.z == 1 || self.end.z == 1
    }

    fn collides_with(&self, rhs: &Self) -> bool {
        (self.x_aligned() && rhs.x_aligned() && intersects(self.x_range(), rhs.x_range()))
        || (self.y_aligned() && rhs.y_aligned() && intersects(self.y_range(), rhs.y_range()))
        || (self.z_aligned() && rhs.z_aligned() && intersects(self.z_range(), rhs.z_range()))
    }
}

impl FromStr for Brick {
    type Err = String;

    fn from_str(raw: &str) -> Result<Self, String> {
        let [start, end] = parse_delimited(raw, "~")?;
        Ok(Self { start, end })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Board {
    bricks: Vec<Id<Brick>>,
}

impl Board {
    fn min_bound(&self) -> Vec3 {
        todo!()
    }

    fn max_bound(&self) -> Vec3 {
        todo!()
    }

    fn collides(&self, brick: &Id<Brick>) -> bool {
        self.bricks
            .iter()
            .filter(|b| b.id != brick.id)
            .any(|b| b.on_ground() || b.collides_with(&brick.value))
    }

    fn step(&mut self) {
        todo!()
    }

    fn next(&self) {
        
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl FromStr for Board {
    type Err = String;

    fn from_str(raw: &str) -> Result<Self, String> {
        let bricks: Vec<_> = parse_delimited(raw, "\n")?;
        Ok(Self {
            bricks: bricks
                .into_iter()
                .enumerate()
                .map(|(id, value)| Id { id, value })
                .collect()
        })
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("Usage: {} <path to input>", args[0]);
        process::exit(1);
    }
    let raw_input = fs::read_to_string(&args[1]).unwrap();
    let board: Board = raw_input.parse().unwrap();
    println!("{board:?}");
}
