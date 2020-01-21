pub fn part_a(i: &str) -> i32 {
  let lines = parse_input(&i);
  lines
    .iter()
    .map(|(l, w, h)| {
      2 * l * w + 2 * w * h + 2 * h * l + [l * w, w * h, h * l].iter().min().unwrap()
    })
    .sum()
}

pub fn part_b(i: &str) -> i32 {
  let lines = parse_input(&i);
  lines
    .iter()
    .map(|(l, w, h)| l * w * h + 2 * [l + w, l + h, w + h].iter().min().unwrap())
    .sum()
}

pub fn part_ab(input: &str, day: usize) -> String {
  format!(
    "AOC 2015 Day {}\n Part 1: {}\n Part 2: {}\n",
    day,
    part_a(&input),
    part_b(&input)
  )
}

fn parse_input(input: &str) -> Vec<(i32, i32, i32)> {
  input
    .lines()
    .map(|line| {
      let n: Vec<&str> = line.split('x').collect();
      let v: Vec<i32> = n.iter().map(|c| c.parse().unwrap()).collect();
      (v[0], v[1], v[2])
    })
    .collect()
}

#[cfg(test)]
mod test {

  use super::*;

  fn s(x: &str) -> String {
    String::from(x)
  }

  #[test]
  fn day2a_examples() {
    assert_eq!(part_a(&s("2x3x4")), 58);
    assert_eq!(part_a(&s("1x1x10")), 43);
  }

  #[test]
  fn day2b_examples() {
    assert_eq!(part_b(&s("2x3x4")), 34);
    assert_eq!(part_b(&s("1x1x10")), 14);
  }
}
