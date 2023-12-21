pub fn run () {
    let steps = super::real_data(15).next().unwrap();

    let answer1 = part1(&steps);
    println!("{}", answer1);
    assert_eq!(answer1, 511215);

    let answer2 = part2(&steps);
    println!("{}", answer2);
    assert_eq!(answer2, 236057);
}

fn part1(steps: &str) -> u32 {
    steps.split(",")
        .map(hash)
        .map(|x| x as u32)
        .sum()
}

fn hash(s: &str) -> u8 {
    s.chars()
        .fold(0u8, |accum, c| accum.wrapping_add(c as u8).wrapping_mul(17))
}

type Facility<'a> = [LensBox<'a>; 256];
type LensBox<'a> = Vec<Lens<'a>>;
type Lens<'a> = (Label<'a>, Focal);
type Label<'a> = &'a str;
type Focal = u8;

fn part2(steps: &str) -> u32 {
    const REP: LensBox = LensBox::new();
    let mut facility = [REP; 256];
    for s in steps.split(",") {
        apply_step(&mut facility, parse_step(s));
    }

    total_focusing_power(&facility)
}

fn total_focusing_power(facility: &Facility) -> u32 {
    facility.into_iter()
        .enumerate()
        .flat_map(|(box_id, lens_box)| {
            lens_box.into_iter()
                .enumerate()
                .map(move |(slot_id, lens)| {
                    let (_, focal) = lens;
                    (1 + box_id as u32) * (1 + slot_id as u32) * (*focal) as u32
                })
        })
        .sum()
}

type Step<'a> = (Label<'a>, Action);

#[derive(Debug, PartialEq, Eq)]
enum Action {
    Remove,
    Add(Focal)
}

fn parse_step<'a>(s: &'a str) -> Step<'a> {
    if s.contains("=") {
        let mut parts = s.split("=");
        let label = parts.next().unwrap();
        let focal = parts.next().unwrap().parse().unwrap();

        (label, Action::Add(focal))
    }
    else {
        assert!(s.ends_with("-"));
        let label = &s[0..(s.len() - 1)];
        (label, Action::Remove)
    }
}

fn apply_step<'a>(facility: &mut Facility<'a>, step: Step<'a>) {
    let (label, action) = step;
    let box_id = hash(label) as usize;
    let lens_box = &mut facility[box_id];

    match action {
        Action::Add(focal) => add_lens(lens_box, (label, focal)),
        Action::Remove     => remove_lens(lens_box, label)
    }
}

fn find_label<'a>(lens_box: &LensBox<'a>, label: Label<'a>) -> Option<usize> {
    for i in 0..lens_box.len() {
        if lens_box[i].0 == label {
            return Some(i);
        }
    }
    return None;
}

fn remove_lens<'a>(lens_box: &mut LensBox<'a>, label: Label<'a>) {
    if let Some(i) = find_label(lens_box, label) {
        lens_box.remove(i);
    }
}

fn add_lens<'a>(lens_box: &mut LensBox<'a>, new_lens: Lens<'a>) {
    let (label, _focal) = new_lens;
    match find_label(lens_box, label) {
        Some(i) => lens_box[i] = new_lens,
        None    => lens_box.push(new_lens),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const TEST_DATA: &str = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";

    #[test]
    fn part1() {
        assert_eq!(super::part1(TEST_DATA), 1320);
    }

    #[test]
    fn parse_step() {
        assert_eq!(super::parse_step("rn=1"), ("rn", Action::Add(1)));
        assert_eq!(super::parse_step("cm-"), ("cm", Action::Remove));
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(TEST_DATA), 145);
    }
}
