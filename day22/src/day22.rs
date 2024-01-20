use std::{
    convert::{TryInto, TryFrom},
    env,
    fs,
    ops::{Add, Deref, DerefMut},
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Vec3 {
    x: i32,
    y: i32,
    z: i32,
}

impl Vec3 {
    fn new(x: i32, y: i32, z: i32) -> Self {
        Self { x, y, z }
    }
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
    fn fall(&self) {
        todo!()
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
    fn collides(&self, brick: &Id<Brick>) -> bool {
        todo!()
    }

    fn step() {
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
