use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryInto;

pub fn run () {
    let (workflow, presents) = parse(super::real_data(19));

    let answer1 = part1(&workflow, &presents);
    println!("{}", answer1);
    assert_eq!(answer1, 495298);

    //let answer2 = part2(instructions());
    //println!("{}", answer2);
    //assert_eq!(answer2, 134549294799713);
}

fn part1(workflow: &Workflow, parts: &[Xmas]) -> Value {
    let starting_rule = &workflow["in"];

    parts.iter()
        .filter(|xmas| starting_rule.eval_rule(**xmas, workflow))
        .map(|xmas| xmas.tot_rating())
        .sum()
}

#[derive(Copy, Clone)]
struct Xmas ([Value; 4]);

impl Xmas {
    fn x(&self) -> Value { self.0[0] }
    fn m(&self) -> Value { self.0[1] }
    fn a(&self) -> Value { self.0[2] }
    fn s(&self) -> Value { self.0[3] }

    fn tot_rating(&self) -> Value {
        self.0.iter().sum()
    }
}

type Value = u32;
type Field = fn(&Xmas) -> Value;

struct Condition {
    field: Field,
    ord:   Ordering,
    value: Value,
}

enum Outcome {
    Return(bool),
    Jump(Label)
}
use Outcome::*;

type Label = String;

struct Rule {
    cond_branches:  Vec<(Condition, Outcome)>,
    default_branch: Outcome,
}

type Workflow = HashMap<Label, Rule>;

impl Rule {
    fn eval_rule(&self, xmas: Xmas, workflow: &Workflow) -> bool {
        self.cond_branches
            .iter()
            .find(|(c, _)| c.eval_cond(xmas) == true)
            .map(|(_, o)| o)
            .unwrap_or(&self.default_branch)
            .eval_outcome(xmas, workflow)
    }
}

impl Condition {
    fn eval_cond(&self, xmas: Xmas) -> bool {
        (self.field)(&xmas).cmp(&self.value) == self.ord
    }
}

impl Outcome {
    fn eval_outcome(&self, xmas: Xmas, workflow: &Workflow) -> bool {
        match self {
            Return(res) => *res,
            Jump(label) => workflow[label].eval_rule(xmas, workflow),
        }
    }
}

fn parse<I: Iterator<Item=String>>(mut lines: I) -> (Workflow, Vec<Xmas>) {
    let mut rules = Workflow::new();
    // error[E0658]: `let` expressions in this position are unstable
    // see issue #53667 <https://github.com/rust-lang/rust/issues/53667> for more information
    while let Some(l) = lines.next() {
        if l.is_empty() {
            break;
        }
        let (name, rule) = parse_rule(&l);
        rules.insert(name, rule);
    }

    let mut presents = Vec::new();
    while let Some(l) = lines.next() {
        presents.push(parse_xmas(&l));
    }

    (rules, presents)
}

fn parse_rule(s: &str) -> (Label, Rule) {
    // px{a<2006:qkq,m>2090:A,rfg}
    let mut curly_braces = s.split("{");
    let label = curly_braces.next().unwrap().into();
    let rem = curly_braces.next().unwrap();
    let whole_rule = rem.strip_suffix("}").unwrap();

    // a<2006:qkq,m>2090:A,rfg
    let branches_str = whole_rule.split(",").collect::<Vec<_>>();
    let mut branches = Vec::new();
    for b_str in &branches_str[0..branches_str.len()-1] {
        // a<2006:qkq
        let mut colon = b_str.split(":");
        let cond_str = colon.next().unwrap();
        let outcome_str = colon.next().unwrap();
        assert_eq!(colon.next(), None);

        branches.push((parse_cond(cond_str), parse_outcome(outcome_str)));
    }

    // rfg
    let default_branch = parse_outcome(branches_str.last().unwrap());

    let rule = Rule {
        cond_branches: branches,
        default_branch
    };
    (label, rule)
}

fn parse_cond(s: &str) -> Condition {
    // a<2006
    assert!(s.is_ascii());
    let field_str = &s[0..1];
    let cmp_str = &s[1..2];
    let val_str = &s[2..];

    let field =
        match field_str {
            "x" => Xmas::x,
            "m" => Xmas::m,
            "a" => Xmas::a,
            "s" => Xmas::s,
            _   => panic!("Unrecognized field {:?}", field_str),
        };

    let ord =
        match cmp_str {
            ">" => Ordering::Greater,
            "<" => Ordering::Less,
            _   => panic!("Unrecognized ordering {:?}", cmp_str),
        };

    let value = val_str.parse().unwrap();

    Condition {
        field,
        ord,
        value
    }
}

fn parse_outcome(s: &str) -> Outcome {
    match s {
        "A" => Return(true),
        "R" => Return(false),
        _   => Jump(s.into()),
    }
}

fn parse_xmas(s: &str) -> Xmas {
    // {x=787,m=2655,a=1222,s=2876}
    let curly_brace = s.strip_prefix("{")
                        .and_then(|u| u.strip_suffix("}"))
                        .unwrap();
    let fields_str = curly_brace.split(",");
    let mut fields = Vec::new();
    // the trait `FromIterator<_>` is not implemented for `[u32; 4]`
    for f in fields_str {
        // x=787
        assert!(f.is_ascii());
        let nums = &f[2..];
        fields.push(nums.parse().unwrap());
    }

    Xmas ( fields.try_into().unwrap() )
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_data() -> (Workflow, Vec<Xmas>) {
        parse(super::super::test_data(19))
    }

    #[test]
    fn part1() {
        let (workflow, presents) = test_data();
        assert_eq!(super::part1(&workflow, &presents), 19114);
    }

    //#[test]
    //fn part2() {
        //assert_eq!(super::part2(test_data()), 952408144115);
    //}
}
