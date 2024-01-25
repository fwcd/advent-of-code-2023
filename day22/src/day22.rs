use std::{
    collections::HashSet,
    convert::{TryInto, TryFrom},
    env,
    fs,
    fmt,
    ops::{Add, Sub, Deref, DerefMut, RangeInclusive},
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

    fn signum(self) -> Self { Self { x: self.x.signum(), y: self.y.signum(), z: self.z.signum() } }
}

impl Add<Vec3> for Vec3 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self { x: self.x + rhs.x, y: self.y + rhs.y, z: self.z + rhs.z }
    }
}

impl Sub<Vec3> for Vec3 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Self { x: self.x - rhs.x, y: self.y - rhs.y, z: self.z - rhs.z }
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

impl<T> Id<T> {
    fn map<U>(self, action: impl Fn(T) -> U) -> Id<U> {
        Id { id: self.id, value: action(self.value) }
    }
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

    fn in_ground(&self) -> bool {
        self.start.z < 1 || self.end.z < 1
    }

    fn collides_with(&self, rhs: &Self) -> bool {
        intersects(self.x_range(), rhs.x_range())
        && intersects(self.y_range(), rhs.y_range())
        && intersects(self.z_range(), rhs.z_range())
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
        self.bricks.iter().flat_map(|b| [b.start, b.end]).reduce(Vec3::min).unwrap()
    }

    fn max_bound(&self) -> Vec3 {
        self.bricks.iter().flat_map(|b| [b.start, b.end]).reduce(Vec3::max).unwrap()
    }

    fn colliding_brick(&self, brick: &Id<Brick>) -> Option<&Id<Brick>> {
        self.bricks
            .iter()
            .filter(|b| b.id != brick.id)
            .find(|b| b.collides_with(&**brick))
    }

    fn collides(&self, brick: &Id<Brick>) -> bool {
        brick.in_ground() || self.colliding_brick(brick).is_some()
    }

    fn fall(&self, brick: &Id<Brick>) -> Option<Id<Brick>> {
        let mut last: Option<Id<Brick>> = None;
        loop {
            let next = last.unwrap_or_else(|| brick.clone()).map(|b| b.fall());
            if self.collides(&next) {
                break last;
            }
            last = Some(next);
        }
    }

    fn apply_gravity(&mut self) -> usize {
        let mut fallen = HashSet::new();
        while let Some((i, next)) = self.bricks.iter().enumerate().find_map(|(i, b)| self.fall(b).map(|b| (i, b))) {
            self.bricks[i] = next;
            fallen.insert(i);
        }
        fallen.len()
    }

    fn dependent_brick_count(&self, brick: &Id<Brick>) -> usize {
        let mut next = Board { bricks: self.bricks.iter().filter(|b| b.id != brick.id).cloned().collect() };
        next.apply_gravity()
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let min_bound = Vec3 { z: 0, ..self.min_bound() };
        let max_bound = self.max_bound();
        let width = (1 + max_bound.x - min_bound.x) as usize;
        let height = (1 + max_bound.z - min_bound.z) as usize;
        let mut lines: Vec<Vec<char>> = vec![vec!['.'; width]; height];
        lines[0] = vec!['-'; width];
        for (i, brick) in self.bricks.iter().enumerate() {
            let step = (brick.end - brick.start).signum();
            let mut current = brick.start;
            while current != (brick.end + step) {
                let rel_z = (current.z - min_bound.z) as usize;
                let rel_x = (current.x - min_bound.x) as usize;
                let c = (i + 'A' as usize) as u8 as char;
                lines[rel_z][rel_x] = match lines[rel_z][rel_x] {
                    p if p == '.' || p == c => c,
                    _ => '?',
                };
                current = current + step;
            }
        }
        for line in lines.into_iter().rev() {
            writeln!(f, "{}", line.into_iter().collect::<String>())?;
        }
        Ok(())
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
    let mut board: Board = raw_input.parse().unwrap();

    println!("Applying gravity...");
    board.apply_gravity();

    let mut part1 = 0;
    let mut part2 = 0;

    for (i, brick) in board.bricks.iter().enumerate() {
        let n = board.dependent_brick_count(brick);
        println!("{i} -> {n}");
        part1 += 1;
        part2 += n;
    }

    println!("Part 1: {part1}");
    println!("Part 2: {part2}");
}
