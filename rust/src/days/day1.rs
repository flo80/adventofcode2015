pub fn part_a(input: &str) -> i32 {
    let numbers = parse_input(input);
    numbers.iter().sum()
}

pub fn part_b(input: &str) -> usize {
    let numbers = parse_input(input);
    numbers
        .iter()
        .scan(0, |acc, x| {
            *acc = *acc + x;
            Some(*acc)
        })
        .take_while(|x| *x >= 0)
        .count()
        + 1 // plus 1 for beginning
}

pub fn part_ab(input: &str, day: usize) -> String {
    format!(
        "AOC 2015 Day {}\n Part 1: {}\n Part 2: {}\n",
        day,
        part_a(&input),
        part_b(&input)
    )
}

fn parse_input(input: &str) -> Vec<i32> {
    input
        .chars()
        .map(|c| match c {
            '(' => 1,
            ')' => -1,
            _ => panic!("unexpected char {}", c),
        })
        .collect()
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn parsing_check() {
        assert_eq!(parse_input("()"), vec![1, -1]);
    }

    #[test]
    fn day1a_examples() {
        assert_eq!(part_a("(())"), 0);
        assert_eq!(part_a("()()"), 0);
        assert_eq!(part_a("((("), 3);
        assert_eq!(part_a("(()(()("), 3);
        assert_eq!(part_a("))((((("), 3);
        assert_eq!(part_a("())"), -1);
        assert_eq!(part_a("))("), -1);
        assert_eq!(part_a(")))"), -3);
        assert_eq!(part_a(")())())"), -3);
    }

    #[test]
    fn day1b_examples() {
        assert_eq!(part_b(")"), 1);
        assert_eq!(part_b("()())"), 5);
    }
}
