use std::{
    convert::TryInto,
    env,
    fs,
    fmt,
    process,
    str::FromStr,
};

fn parse_delimited<T, const N: usize>(raw: &str, delimiter: &str) -> Result<[T; N], String>
where
    T: FromStr,
    T::Err: fmt::Display {
    raw
        .split(delimiter)
        .map(|s| s.parse().map_err(|e| format!("Could not parse '{s}': {e}")))
        .collect::<Result<Vec<T>, _>>()?
        .try_into()
        .map_err(|e| format!("Could not parse '{raw}': Expected {N} element(s)"))
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

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("Usage: {} <path to input>", args[0]);
        process::exit(1);
    }
    let raw_input = fs::read_to_string(&args[1]).unwrap();
    let bricks: Vec<Brick> = raw_input.lines()
        .map(Brick::from_str)
        .collect::<Result<_, _>>()
        .unwrap();
    println!("{bricks:?}");
}
