use md5;

pub fn part_a(secret: &str) -> usize {
  calc(secret, 5)
}

pub fn part_b(secret: &str) -> usize {
  calc(secret, 6)
}

pub fn part_ab(input: &str, day: usize) -> String {
  format!(
    "AOC 2015 Day {}\n Part 1: {}\n Part 2: {}\n",
    day,
    part_a(&input),
    part_b(&input)
  )
}

pub fn calc(secret: &str, digits: usize) -> usize {
  (0..)
    .filter(|nr| is_valid(digits, &hash(secret, nr)))
    .take(1)
    .next()
    .unwrap()
}

fn hash(secret: &str, number: &usize) -> String {
  let digest = md5::compute(secret.to_string() + &number.to_string());
  format!("{:x}", digest)
}

fn is_valid(digits: usize, hash: &str) -> bool {
  hash.chars().take(digits).all(|c| c == '0')
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  // pass but very slow
  #[ignore]
  fn day4a_examples() {
    assert_eq!(609043, part_a("abcdef"));
    assert_eq!(1048970, part_a("pqrstuv"));
  }
}
