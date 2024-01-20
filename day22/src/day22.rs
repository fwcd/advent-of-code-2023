use std::{
    convert::{TryInto, TryFrom},
    env,
    fs,
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
    pub fn new(x: i32, y: i32, z: i32) -> Self {
        Self { x, y, z }
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
struct Brick {
    start: Vec3,
    end: Vec3,
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
    bricks: Vec<Brick>,
}

impl FromStr for Board {
    type Err = String;

    fn from_str(raw: &str) -> Result<Self, String> {
        let bricks = parse_delimited(raw, "\n")?;
        Ok(Self { bricks })
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
