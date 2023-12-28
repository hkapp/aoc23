use std::collections::{HashMap, HashSet};
use num::integer::lcm;
use std::collections::VecDeque;

pub fn run () {
    let mut system = parse(super::real_data(20));
    patch_ghost_destinations(&mut system);
    discover_conjunction_inputs(&mut system);

    let answer1 = part1(&mut system);
    println!("{}", answer1);
    assert_eq!(answer1, 825167435);

    reset(&mut system);
    let answer2 = part2(&mut system);
    println!("{}", answer2);
    assert_eq!(answer2, 225514321828633);
}

fn part1(system: &mut System) -> usize {
    let mut high_pulse_count = 0;
    let mut low_pulse_count = 0;

    for _ in 0..1000 {
        for (_, pulse, _) in Propagation::new(system) {
            match pulse {
                Pulse::Low => {
                    low_pulse_count += 1;
                },
                Pulse::High => {
                    high_pulse_count += 1;
                },
            }
        }
    }

    high_pulse_count * low_pulse_count
}

fn part2(system: &mut System) -> u64 {
    let (last_conjunction_name, last_conjunction_module) =
            system.iter()
                .find(|(_, module)| module.destinations.iter().any(|l| l == "rx"))
                .unwrap();

    let last_conjunction_name: Label = last_conjunction_name.into();
    println!("last = {}", last_conjunction_name);

    let mut rem_inputs = last_conjunction_module.mkind
                            .conjunction_inputs()
                            .unwrap()
                            .keys()
                            .cloned()
                            .collect::<HashSet<_>>();
    println!("starting inputs: {:?}", rem_inputs);

    let mut button_count = 0;
    let mut result = 1;

    while !rem_inputs.is_empty() {
        button_count += 1;
        for (src, _, _) in Propagation::new(system)
                            .filter(|(_, pulse, dst)| {
                                *pulse == Pulse::High
                                && dst == &last_conjunction_name
                            })
        {
            if rem_inputs.remove(&src) == true {
                println!("Found {} @ {}", src, button_count);
                result = lcm(result, button_count);
                //result *= button_count;  // also wrong
                println!("Result = {}", result);
            }
            else {
                println!("Skip {} @ {} ; should have common factors with {}",
                            src, button_count, result);
            }
        }
    }

    return result;
}

type System = HashMap<Label, Module>;

fn parse<I: Iterator<Item=String>>(lines: I) -> System {
    lines.map(|l| parse_line(&l))
        .collect()
}

fn parse_line(s: &str) -> (Label, Module) {
    let mut arrow = s.split(" -> ");
    let label_with_prefix = arrow.next().unwrap().into();
    let destinations = arrow.next()
                        .unwrap()
                        .split(", ")
                        .map(Into::into)
                        .collect();
    assert_eq!(arrow.next(), None);

    let (label, mkind) = parse_mkind(label_with_prefix);
    let module = Module {
        mkind,
        destinations,
    };

    (label, module)
}

enum FFS {
    On, Off
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum Pulse {
    High, Low
}

enum MKind {
    FlipFlop { state: FFS } ,
    Conjunction { memory: HashMap<Label, Pulse> } ,
    Broadcast,
}
use MKind::*;

type Label = String;

struct Module {
    mkind:        MKind,
    destinations: Vec<Label>
}

fn parse_mkind(label_with_prefix: &str) -> (Label, MKind) {
    assert!(label_with_prefix.is_ascii());
    let first_char = label_with_prefix.chars().next().unwrap();
    let rem_chars = &label_with_prefix[1..];
    match first_char {
        '%' => (rem_chars.into(), FlipFlop { state: FFS::Off } ),
        '&' => (rem_chars.into(), Conjunction { memory: HashMap::new() } ),
        _   => {
            assert_eq!(label_with_prefix, "broadcaster");
            (label_with_prefix.into(), Broadcast)
        }
    }
}

fn patch_ghost_destinations(system: &mut System) {
    let mut ghosts: HashSet<Label> = HashSet::new();

    for in_module in system.values() {
        for out_label in in_module.destinations.iter() {
            if !system.contains_key(out_label) {
                eprintln!("[WARNING] patching orphan destination {}", out_label);
                ghosts.insert(out_label.into());
            }
        }
    }

    for ghost_label in ghosts {
        let ghost_module = Module {
            mkind:        Broadcast,
            destinations: Vec::new()
        };
        system.insert(ghost_label, ghost_module);
    }
}

fn discover_conjunction_inputs(system: &mut System) {
    let mut in_out: Vec<(Label, Label)> = Vec::new();

    for (in_label, in_module) in system.iter() {
        for out_label in &in_module.destinations {
            if let Some(out_module) = system.get(out_label) {
                if out_module.mkind.is_conjunction() {
                    in_out.push((out_label.clone(), in_label.clone()));
                }
            }
            else {
                eprintln!("[WARNING] module {} not found", out_label);
            }
        }
    }

    for (out_label, in_label) in in_out {
        system.get_mut(&out_label)
            .unwrap()
            .mkind
            .conjunction_add_input(in_label);
    }
}

impl MKind {
    fn conjunction_add_input(&mut self, new_in: Label) {
        match self {
            Conjunction { memory } => { memory.insert(new_in, Pulse::Low); },
            _ => panic!(),
        }
    }

    fn is_conjunction(&self) -> bool {
        match self {
            Conjunction { .. } => true,
            _ => false,
        }
    }

    fn receive(&mut self, signal: Pulse, source: &Label) -> Option<Pulse> {
        match self {
            FlipFlop { state } => {
                match signal {
                    // If a flip-flop module receives a high pulse, it is ignored and nothing happens
                    Pulse::High => None,
                    // if a flip-flop module receives a low pulse, it flips between on and off.
                    Pulse::Low => {
                        match state {
                            // If it was off, it turns on and sends a high pulse.
                            FFS::Off => {
                                *state = FFS::On;
                                Some(Pulse::High)
                            }
                            // If it was on, it turns off and sends a low pulse
                            FFS::On => {
                                *state = FFS::Off;
                                Some(Pulse::Low)
                            }
                        }
                    }
                }
            }

            Conjunction { memory } => {
                // When a pulse is received, the conjunction module first updates its memory for that input.
                let mem_cell = memory.get_mut(source).unwrap();
                *mem_cell = signal;
                // Then, if it remembers high pulses for all inputs, it sends a low pulse;
                // otherwise, it sends a high pulse.
                let all_high_pulses = memory.values()
                                        .all(|p| *p == Pulse::High);
                if all_high_pulses {
                    Some(Pulse::Low)
                }
                else {
                    Some(Pulse::High)
                }
            }

            Broadcast => {
                // When it receives a pulse, it sends the same pulse to all of its destination modules.
                Some(signal)
            }
        }
    }

    fn reset(&mut self) {
        match self {
            FlipFlop { state } => {
                *state = FFS::Off;
            }

            Conjunction { memory } => {
                memory.values_mut()
                    .for_each(|x| *x = Pulse::Low)
            }

            Broadcast => {}
        }
    }

    fn conjunction_inputs(&self) -> Option<&HashMap<Label, Pulse>> {
        match self {
            Conjunction { memory } => Some(memory),
            _ => None,
        }
    }
}

type Signal = (Label, Pulse, Label);

struct Propagation<'a> {
    system:       &'a mut System,
    signal_queue: VecDeque<Signal>,
}

impl<'a> Propagation<'a> {
    fn new(system: &'a mut System) -> Self {
        let mut signal_queue = VecDeque::new();
        let init_signal = ("button".into(), Pulse::Low, "broadcaster".into());
        signal_queue.push_back(init_signal);

        Propagation {
            system,
            signal_queue
        }
    }
}

impl<'a> Iterator for Propagation<'a> {
    type Item = Signal;

    fn next(&mut self) -> Option<Self::Item> {
        match self.signal_queue.pop_front() {
            None => None,
            Some((src_label, pulse, dst_label)) => {
                let dst_module = self.system.get_mut(&dst_label).unwrap();
                let next_signal = dst_module.mkind.receive(pulse, &src_label);

                if let Some(next_pulse) = next_signal {
                    for next_dest in dst_module.destinations.iter() {
                        self.signal_queue.push_back((dst_label.clone(), next_pulse, next_dest.into()));
                    }
                }

                Some((src_label, pulse, dst_label))
            }
        }
    }
}

fn reset(system: &mut System) {
    system.values_mut()
        .for_each(|m| m.mkind.reset())
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_data(num: u8) -> System {
        let mut system = parse(super::super::test_data_num(20, num));
        patch_ghost_destinations(&mut system);
        discover_conjunction_inputs(&mut system);
        return system;
    }

    #[test]
    fn part1_1() {
        assert_eq!(super::part1(&mut test_data(1)), 32000000);
    }

    #[test]
    fn part1_2() {
        assert_eq!(super::part1(&mut test_data(2)), 11687500);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(&mut test_data(2)), 1);
    }
}
