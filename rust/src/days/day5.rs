extern crate itertools;

use itertools::Itertools;

pub fn part_ab(input: &str, day: usize) -> String {
  format!(
    "AOC 2015 Day {}\n Part 1: {}\n Part 2: {}\n",
    day,
    part_a(&input),
    part_b(&input)
  )
}

pub fn part_a(input: &str) -> usize {
  input.lines().filter(|x| is_nice(x)).count()
}

fn is_nice(word: &str) -> bool {
  let vowels = word.chars().filter(|c| is_vowel(c)).count() >= 3;
  let pairs = word
    .chars()
    .tuple_windows::<(_, _)>()
    .filter(|(a, b)| a == b)
    .count()
    > 0;
  let contain = !["ab", "cd", "pq", "xy"]
    .iter()
    .map(|p| word.contains(p))
    .any(|x| x == true);

  vowels && pairs && contain
}

fn is_vowel(c: &char) -> bool {
  match c {
    'a' | 'e' | 'i' | 'o' | 'u' => true,
    _ => false,
  }
}

pub fn part_b(input: &str) -> usize {
  input.lines().filter(|x| is_nice2(x)).count()
}

fn is_nice2(word: &str) -> bool {
  let pairs = word
    .chars()
    // get all 2 character pairs
    .tuple_windows::<(_, _)>()
    // check if any pair appears more than once
    .map(|(a, b)| {
      word
        .matches(&([a, b].into_iter().collect::<String>()))
        .count()
        > 1
    })
    .any(|x| x == true);
  let repeat = word
    .chars()
    .tuple_windows::<(_, _, _)>()
    .filter(|(a, _b, c)| a == c)
    .count()
    > 0;
  pairs && repeat
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn day5a_examples() {
    assert_eq!(true, is_nice(&"ugknbfddgicrmopn"));
    assert_eq!(true, is_nice(&"aaa"));
    assert_eq!(false, is_nice(&"jchzalrnumimnmhp"));
    assert_eq!(false, is_nice(&"haegwjzuvuyypxyu"));
    assert_eq!(false, is_nice(&"dvszwmarrgswjxmb"));
  }

  #[test]
  fn day5b_examples() {
    assert_eq!(true, is_nice2(&"qjhvhtzxzqqjkmpb"));
    assert_eq!(true, is_nice2(&"xxyxx"));
    assert_eq!(false, is_nice2(&"uurcxstgmygtbstg"));
    assert_eq!(false, is_nice2(&"ieodomkazucvgmuy"));
  }
}
