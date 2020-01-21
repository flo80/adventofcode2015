extern crate nom;

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::digit1,
  combinator::{all_consuming, map, map_res, opt},
  multi::many0,
  sequence::{separated_pair, tuple},
  IResult,
};

pub fn part_ab(input: &str, day: usize) -> String {
  format!(
    "AOC 2015 Day {}\n Part 1: {}\n Part 2: {}\n",
    day,
    part_a(&input),
    part_b(&input)
  )
}

type Lights = [bool; 1_000_000];
type Lights2 = [usize; 1_000_000];
type Pos = (usize, usize);

#[derive(PartialEq, Debug)]
enum Instruction {
  LightOn,
  LightOff,
  LightToggle,
}

fn parse_input(input: &str) -> Vec<(Instruction, Pos, Pos)> {
  // turn on 0,0 through 999,999 would turn on (or leave on) every light.
  // toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
  // turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.
  fn parse_line(l: &str) -> IResult<&str, (Instruction, Pos, Pos)> {
    fn number(i: &str) -> IResult<&str, usize> {
      map_res(digit1, |x: &str| x.parse::<usize>())(i)
    }

    fn pos(i: &str) -> IResult<&str, Pos> {
      separated_pair(number, tag(","), number)(i)
    }

    let through = tag(" through ");
    let turn_on = map(tag("turn on "), |_| Instruction::LightOn);
    let turn_off = map(tag("turn off "), |_| Instruction::LightOff);
    let toggle = map(tag("toggle "), |_| Instruction::LightToggle);
    let instr = alt((turn_on, turn_off, toggle));
    let newline = opt(tag("\n"));

    let (input, (i, s, _, e, _)) = tuple((instr, pos, through, pos, newline))(l)?;
    IResult::Ok((input, (i, s, e)))
  }

  match all_consuming(many0(parse_line))(input) {
    Ok((_, res)) => res,
    x => panic!("could not parse input {:?}", x),
  }
}

pub fn part_a(i: &str) -> usize {
  let lines = parse_input(&i);
  let mut lights = [false; 1_000_000];
  for (i, s, e) in lines {
    process(&mut lights, &i, s, e);
  }
  count_on(&lights)
}

fn process(lights: &mut Lights, instr: &Instruction, start: Pos, end: Pos) {
  let (sx, sy) = start;
  let (ex, ey) = end;

  for x in sx..(ex + 1) {
    for y in sy..(ey + 1) {
      let pos = x + y * 1_000;
      let set = match instr {
        Instruction::LightOn => true,
        Instruction::LightOff => false,
        Instruction::LightToggle => !lights[pos],
      };
      lights[pos] = set;
    }
  }
}

fn count_on(lights: &Lights) -> usize {
  lights.iter().filter(|x| **x == true).count()
}

pub fn part_b(i: &str) -> usize {
  let lines = parse_input(&i);
  let mut lights: Lights2 = [0; 1_000_000];
  for (i, s, e) in lines {
    process2(&mut lights, &i, s, e);
  }
  lights.iter().sum()
}

fn process2(lights: &mut Lights2, instr: &Instruction, start: Pos, end: Pos) {
  let (sx, sy) = start;
  let (ex, ey) = end;

  for x in sx..(ex + 1) {
    for y in sy..(ey + 1) {
      let pos = x + y * 1_000;
      let set = match instr {
        Instruction::LightOn => lights[pos] + 1,
        Instruction::LightOff => lights[pos].checked_sub(1).unwrap_or(0),
        Instruction::LightToggle => lights[pos] + 2,
      };
      lights[pos] = set;
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_count() {
    assert_eq!(0, count_on(&[false; 1_000_000]));
    assert_eq!(1_000_000, count_on(&[true; 1_000_000]));
  }

  #[test]
  fn test_all_on() {
    let mut lights: Lights = [false; 1_000_000];
    assert_eq!(count_on(&lights), 0);
    process(&mut lights, &Instruction::LightOn, (0, 0), (999, 999));
    assert_eq!(count_on(&lights), 1_000_000);
  }

  #[test]
  fn test_all_off() {
    let mut lights: Lights = [true; 1_000_000];
    assert_eq!(count_on(&lights), 1_000_000);

    process(&mut lights, &Instruction::LightOff, (0, 0), (999, 999));
    assert_eq!(count_on(&lights), 0);
  }

  #[test]
  fn test_on_off() {
    let mut lights: Lights = [false; 1_000_000];
    process(&mut lights, &Instruction::LightOn, (0, 0), (0, 0));
    assert_eq!(count_on(&lights), 1);

    process(&mut lights, &Instruction::LightOn, (0, 0), (9, 9));
    assert_eq!(count_on(&lights), 100);

    process(&mut lights, &Instruction::LightOff, (0, 0), (8, 8));
    assert_eq!(count_on(&lights), 19);

    process(&mut lights, &Instruction::LightOff, (0, 0), (9, 9));
    assert_eq!(count_on(&lights), 0);
  }

  #[test]
  fn test_toggle() {
    let mut lights: Lights = [false; 1_000_000];
    process(&mut lights, &Instruction::LightToggle, (0, 0), (0, 0));
    assert_eq!(count_on(&lights), 1);
    process(&mut lights, &Instruction::LightToggle, (0, 0), (9, 9));
    assert_eq!(count_on(&lights), 99);
  }

  #[test]
  fn test_parser_single() {
    assert_eq!(
      parse_input("turn on 0,0 through 999,999"),
      vec![(Instruction::LightOn, (0, 0), (999, 999))]
    );
    assert_eq!(
      parse_input("turn off 0,0 through 888,888"),
      vec![(Instruction::LightOff, (0, 0), (888, 888))]
    );
    assert_eq!(
      parse_input("toggle 0,0 through 777,777"),
      vec![(Instruction::LightToggle, (0, 0), (777, 777))]
    );
  }

  #[test]
  fn test_parser_multiple() {
    assert_eq!(
      parse_input(
        "turn on 0,0 through 999,999\nturn off 0,0 through 888,888\ntoggle 0,0 through 777,777"
      ),
      vec![
        (Instruction::LightOn, (0, 0), (999, 999)),
        (Instruction::LightOff, (0, 0), (888, 888)),
        (Instruction::LightToggle, (0, 0), (777, 777))
      ]
    );
  }
}
