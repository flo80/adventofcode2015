extern crate itertools;

use itertools::Itertools;
use std::collections::HashSet;
use std::iter::FromIterator;

type Pos = (i32, i32);
fn add(a: Pos, b: Pos) -> Pos {
  (a.0 + b.0, a.1 + b.1)
}

fn calc_positions(lines: Vec<Pos>) -> HashSet<Pos> {
  let start: Pos = (0, 0);
  let mut all_pos = HashSet::from_iter(lines.iter().scan(start, |a, x| {
    *a = add(*a, *x);
    Some(*a)
  }));
  // add starting position
  all_pos.insert((0, 0));
  all_pos
}

pub fn part_a(i: &str) -> usize {
  let lines = parse_input(i);
  calc_positions(lines).len()
}

pub fn part_b(i: &str) -> usize {
  let lines = parse_input(i);
  let (a, b) = lines.iter().tuples::<(_, _)>().unzip();
  calc_positions(a).union(&calc_positions(b)).count()
}

pub fn part_ab(input: &str, day: usize) -> String {
  format!(
    "AOC 2015 Day {}\n Part 1: {}\n Part 2: {}\n",
    day,
    part_a(&input),
    part_b(&input)
  )
}

fn parse_input(input: &str) -> Vec<Pos> {
  input
    .chars()
    .map(|c| match c {
      '^' => (0, 1),
      'v' => (0, -1),
      '<' => (-1, 0),
      '>' => (1, 0),
      _ => panic!("unexpected character {}", c),
    })
    .collect()
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn parser_test() {
    assert_eq!(parse_input("^"), vec![(0, 1)]);
    assert_eq!(parse_input("^v<>"), vec![(0, 1), (0, -1), (-1, 0), (1, 0)]);
  }

  #[test]
  #[should_panic]
  fn parser_test_illegal() {
    parse_input("c");
  }

  #[test]
  fn day3a_examples() {
    assert_eq!(part_a(">"), 2);
    assert_eq!(part_a("^>v<"), 4);
    assert_eq!(part_a("^v^v^v^v^v"), 2);
  }

  #[test]
  fn day3b_examples() {
    assert_eq!(part_b("^v"), 3);
    assert_eq!(part_b("^>v<"), 3);
    assert_eq!(part_b("^v^v^v^v^v"), 11);
  }
}
