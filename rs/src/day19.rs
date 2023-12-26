use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryInto;

pub fn run () {
    let (workflow, presents) = parse(super::real_data(19));

    let answer1 = part1(&workflow, &presents);
    println!("{}", answer1);
    assert_eq!(answer1, 495298);

    let answer2 = part2(&workflow);
    println!("{}", answer2);
    assert_eq!(answer2, 132186256794011);
}

fn part1(workflow: &Workflow, parts: &[Xmas]) -> Value {
    let starting_rule = &workflow["in"];

    parts.iter()
        .filter(|xmas| starting_rule.eval_rule(xmas, workflow))
        .map(|xmas| xmas.tot_rating())
        .sum()
}

fn part2(workflow: &Workflow) -> usize {
    let starting_rule = &workflow["in"];
    let starting_range = XmasRange::new();

    starting_rule.count_combinations(starting_range, workflow)
}

#[derive(Clone)]
struct Xmas ([Value; 4]);

impl Xmas {
    const X_IDX: usize = 0;
    const M_IDX: usize = 1;
    const A_IDX: usize = 2;
    const S_IDX: usize = 3;

    fn tot_rating(&self) -> Value {
        self.0.iter().sum()
    }

    fn all(x: Value) -> Self {
        Xmas([x; 4])
    }
}

type Value = u32;

struct Condition {
    field_idx: usize,
    ord:       Ordering,
    value:     Value,
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
    fn eval_rule(&self, xmas: &Xmas, workflow: &Workflow) -> bool {
        self.cond_branches
            .iter()
            .find(|(c, _)| c.eval_cond(xmas) == true)
            .map(|(_, o)| o)
            .unwrap_or(&self.default_branch)
            .eval_outcome(xmas, workflow)
    }

    fn count_combinations(&self, range: XmasRange, workflow: &Workflow) -> usize {
        let (rem_range, cond_count) =
            self.cond_branches
                .iter()
                .fold(
                    (range, 0),
                    |(range_left, count_so_far), (cond, outcome)| {
                        let (range_yes, range_continue) = cond.split_range_on_cond(range_left);
                        let o_count = outcome.range_eval(range_yes, workflow);
                        (range_continue, o_count + count_so_far)
                    });

        let default_count = self.default_branch.range_eval(rem_range, workflow);

        cond_count + default_count
    }
}

impl Condition {
    fn eval_cond(&self, xmas: &Xmas) -> bool {
        xmas.0[self.field_idx].cmp(&self.value) == self.ord
    }

    fn split_range_on_cond(&self, range: XmasRange) -> (XmasRange, XmasRange) {
        match self.ord {
            /* *** * *****
             * ^^^ x xxxxx
             * yes      no
             */
            Ordering::Less => range.split_eq_right(self.field_idx, self.value),
            /* *** * *****
             * xxx x ^^^^^
             * no      yes
             */
            Ordering::Greater => swap(range.split_eq_left(self.field_idx, self.value)),

            _ => unreachable!(),
        }
    }
}

fn swap<T, U>(pair: (T, U)) -> (U, T) {
    match pair {
        (x, y) => (y, x)
    }
}

impl Outcome {
    fn eval_outcome(&self, xmas: &Xmas, workflow: &Workflow) -> bool {
        match self {
            Return(res) => *res,
            Jump(label) => workflow[label].eval_rule(xmas, workflow),
        }
    }

    fn range_eval(&self, range: XmasRange, workflow: &Workflow) -> usize {
        match self {
            Return(true)  => range.combinations(),
            Return(false) => 0,
            Jump(label)   => workflow[label].count_combinations(range, workflow),
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

    let field_idx =
        match field_str {
            "x" => Xmas::X_IDX,
            "m" => Xmas::M_IDX,
            "a" => Xmas::A_IDX,
            "s" => Xmas::S_IDX,
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
        field_idx,
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

// Both sides of the range are inclusive
#[derive(Clone)]
struct XmasRange {
    low:  Xmas,
    high: Xmas
}

impl XmasRange {
    fn new() -> Self {
        XmasRange {
            low:  Xmas::all(1),
            high: Xmas::all(4000),
        }
    }

    fn combinations(&self) -> usize {
        self.low.0
            .iter()
            .zip(self.high.0.iter())
            .map(|(a, b)| {
                b.checked_sub(*a)
                    .map(|c| c + 1)
                    .unwrap_or(0)
                    as usize
            })
            .product()
    }

    fn split_eq_left(self, field_idx: usize, value: Value) -> (XmasRange, XmasRange) {
        let mut split_low  = self.clone();
        let mut split_high = self;

        /* [***] [******]
         * low ^     high
         */
        assert!(value >= split_low.low.0[field_idx] && value <= split_low.high.0[field_idx]);
        split_low.high.0[field_idx] = value;
        split_high.low.0[field_idx] = value + 1;

        (split_low, split_high)
    }

    fn split_eq_right(self, field_idx: usize, value: Value) -> (XmasRange, XmasRange) {
        let mut split_low  = self.clone();
        let mut split_high = self;

        /* [***] [******]
         * low   ^   high
         */
        assert!(value >= split_low.low.0[field_idx] && value <= split_low.high.0[field_idx]);
        split_low.high.0[field_idx] = value - 1;
        split_high.low.0[field_idx] = value;

        (split_low, split_high)
    }
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

    #[test]
    fn part2() {
        let (workflow, _) = test_data();
        assert_eq!(super::part2(&workflow), 167409079868000);
    }
}
