// idea from https://github.com/MetroWind/advent2019/blob/8e259e44ed057b45e068d9741a634a234b6f5724/advent/meta.rs

use std::fs;
use std::path::Path;
use std::vec::Vec;

extern crate proc_macro;
use proc_macro::TokenStream;

extern crate regex;
use regex::Regex;

const DIR: &str = "src/days";

struct Day {
  mod_name: String,
  num: u8,
}

impl Day {
  fn new(file: &Path) -> Result<Day, ()> {
    let day_file_pattern = Regex::new(r"^day(\d{1,2})\.rs$").unwrap();
    let basename = file.file_name().ok_or(())?.to_str().ok_or(())?;

    let day = Day {
      mod_name: String::from(&basename[..basename.len() - 3]),
      num: day_file_pattern
        .captures(basename)
        .ok_or(())?
        .get(1)
        .ok_or(())?
        .as_str()
        .parse()
        .map_err(|_| ())?,
    };
    Ok(day)
  }

  fn vec_from_dir(dir: &str) -> Result<Vec<Day>, ()> {
    let days: Vec<Day> = fs::read_dir(dir)
      .map_err(|_| ())?
      .map(|entry| Day::new(&entry.map_err(|_| ())?.path()))
      .filter(|day_maybe| day_maybe.is_ok())
      .map(|day_maybe| {
        let day = day_maybe.unwrap();
        day
      })
      .collect();
    Ok(days)
  }
}

#[proc_macro]
pub fn import_days(_: TokenStream) -> TokenStream {
  let days = Day::vec_from_dir(DIR).unwrap();
  let code: String = format!(
    "mod days
{{
{}
}}",
    days
      .iter()
      .map(|day| format!("    pub mod {};", day.mod_name))
      .collect::<Vec<String>>()
      .join("\n")
  );

  code.parse().unwrap()
}

#[proc_macro]
pub fn define_run(item: TokenStream) -> TokenStream {
  let mut iter = item.into_iter();
  let day_var = iter.next().unwrap().to_string();
  let part_var = iter.next().unwrap().to_string();

  let days = Day::vec_from_dir(DIR).unwrap();
  let code: String = format!(
    "fn run({day}: u8, {part}: u8) -> String
{{
match {day}
{{
{body},
    _ => panic!(\"There's no day {{}}.\", {day}),
}}
}}",
    day = day_var,
    part = part_var,
    body = days
      .iter()
      .map(|day| format!("    {} => {}", day.num, match_part(&day, &part_var[..])))
      .collect::<Vec<String>>()
      .join(",\n")
  );

  // println!("The run() function is defined as\n\n{}\n", code);
  code.parse().unwrap()
}

fn match_part(day: &Day, part: &str) -> String {
  format!(
    "match {part}
    {{
        1 => days::{day}::part_a(&read_input({daynr})).to_string(),
        2 => days::{day}::part_b(&read_input({daynr})).to_string(),
        _ => days::{day}::part_ab(&read_input({daynr}),{daynr}),
    }}",
    day = day.mod_name,
    daynr = day.num,
    part = part,
  )
}
