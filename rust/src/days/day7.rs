use std::collections::HashMap;
pub fn part_ab(input: &str, day: usize) -> String {
  format!(
    "AOC 2015 Day {}\n Part 1: {}\n Part 2: {}\n",
    day,
    part_a(&input),
    part_b(&input)
  )
}

#[derive(PartialEq, Debug)]
enum Circuit<'a> {
  Assign(&'a str),
  And(&'a str, &'a str),
  Or(&'a str, &'a str),
  LShift(usize, &'a str),
  RShift(usize, &'a str),
  Not(&'a str),
}

fn parse_input(input: &str) -> HashMap<&str, Circuit> {
  let mut wires = HashMap::new();

  for line in input.lines() {
    // println!("Parsing: {}", line);
    match line.split_whitespace().collect::<Vec<&str>>().as_slice() {
      ["NOT", a, "->", name] => {
        wires.insert(*name, Circuit::Not(*a));
      }
      [a, "AND", b, "->", name] => {
        wires.insert(*name, Circuit::And(*a, *b));
      }
      [a, "OR", b, "->", name] => {
        wires.insert(*name, Circuit::Or(*a, *b));
      }
      [a, "RSHIFT", x, "->", name] => {
        wires.insert(*name, Circuit::RShift(x.parse().unwrap(), *a));
      }
      [a, "LSHIFT", x, "->", name] => {
        wires.insert(*name, Circuit::LShift(x.parse().unwrap(), *a));
      }
      [x, "->", name] => {
        wires.insert(*name, Circuit::Assign(x));
      }
      _ => panic!("line not recognized",),
    }
  }
  wires
}

pub fn part_a(i: &str) -> u16 {
  let mut wires = parse_input(&i);
  // println!("wires {:?}", wires);

  get_value(&mut wires, "a")
}

fn get_value(wires: &HashMap<&str, Circuit>, wire: &str) -> u16 {
  let mut cache = HashMap::new();

  let ga = |val: &str| {
    let p = val.parse::<u16>();
    match p {
      Ok(nr) => nr,
      Err(_) => {
        let lookup = cache.get(val);
        match lookup {
          None => {
            let r = get_value(wires, val);
            cache.insert(val, r);
            r
          }
          Some(r) => *r,
        }
      }
    }
  };
  let res = match wires.get(wire) {
    None => wire.parse().unwrap(),
    Some(c) => match c {
      Circuit::Assign(val) => ga(val),
      Circuit::And(a, b) => ga(&a) & ga(&b),
      Circuit::Or(a, b) => ga(&a) | ga(&b),
      Circuit::LShift(x, a) => ga(&a) << x,
      Circuit::RShift(x, a) => ga(&a) >> x,
      Circuit::Not(a) => !ga(&a),
    },
  };
  println!("Requesting {} to {}", wire, res);
  res
}

pub fn part_b(i: &str) -> isize {
  let _lines = parse_input(&i);
  0
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_parse_input() {
    let i = parse_input(&"123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i\n");
    let g = |x| i.get(x).unwrap();
    assert_eq!(&Circuit::Val(123), g("x"));
    assert_eq!(&Circuit::Val(456), g("y"));
    assert_eq!(&Circuit::And(&"x", &"y"), g("d"));
    assert_eq!(&Circuit::Or(&"x", &"y"), g("e"));
    assert_eq!(&Circuit::LShift(2, &"x"), g("f"));
    assert_eq!(&Circuit::RShift(2, &"y"), g("g"));
    assert_eq!(&Circuit::Not(&"x"), g("h"));
    assert_eq!(&Circuit::Not(&"y"), g("i"));
  }

  #[test]
  fn test_get_val() {
    let i = parse_input(&"123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i\n");
    // println!("i: {:?}", i);
    assert_eq!(72, get_value(&i, "d")); //d: 72
    assert_eq!(507, get_value(&i, "e")); //e: 507
    assert_eq!(492, get_value(&i, "f")); //f: 492
    assert_eq!(114, get_value(&i, "g")); //g: 114
    assert_eq!(65412, get_value(&i, "h")); //h: 65412
    assert_eq!(65079, get_value(&i, "i")); //i: 65079
    assert_eq!(123, get_value(&i, "x")); //x: 123
    assert_eq!(456, get_value(&i, "y")); //y: 456
  }
}
