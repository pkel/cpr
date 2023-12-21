use pyo3::prelude::*;
use rand::distributions::Bernoulli;
use rand::distributions::Distribution;

#[pyclass(get_all)]
#[derive(Debug)]
struct Observation {
    reward: u32,
    progress: u32,
    done: bool,
}

#[pymethods]
impl Observation {
    fn __repr__(&self) -> String {
        format!("{self:?}")
    }
}

#[derive(Debug)]
enum Fork {
    Irrelevant,
    Relevant,
    Active,
}

#[derive(Debug)]
enum Action {
    Wait,
    Override,
    Match,
    Adopt,
}

#[pyclass]
#[derive(Debug)]
struct FC16SSZwPT {
    a: u32,
    h: u32,
    fork: Fork,
    rv_mining: Bernoulli,
    rv_network: Bernoulli,
    rv_termination: Bernoulli,
    actions: Vec<Action>,
}

macro_rules! sample {
    ($rv:expr) => {
        $rv.sample(&mut rand::thread_rng())
    };
}

#[pymethods]
impl FC16SSZwPT {
    #[new]
    fn new(alpha: f64, gamma: f64, horizon: f64) -> Self {
        let mut obj = FC16SSZwPT {
            a: 0,
            h: 0,
            fork: Fork::Irrelevant,
            rv_mining: Bernoulli::new(alpha).unwrap(),
            rv_network: Bernoulli::new(gamma).unwrap(),
            rv_termination: Bernoulli::new(1. / horizon).unwrap(),
            actions: vec![],
        };

        obj.init();
        obj
    }

    fn init(&mut self) {
        let lucky_start = sample!(self.rv_mining);

        self.a = if lucky_start { 1 } else { 0 };
        self.h = if lucky_start { 0 } else { 1 };
        self.fork = Fork::Irrelevant;

        self.set_actions();
    }

    fn set_actions(&mut self) {
        self.actions.clear();
        self.actions.push(Action::Wait);
        self.actions.push(Action::Adopt);
        let a = self.a;
        let h = self.h;
        if a > h {
            self.actions.push(Action::Override);
        }
        if a >= h {
            self.actions.push(Action::Match);
        }
    }

    fn apply_non_active_wait(&mut self) -> Observation {
        if sample!(self.rv_mining) {
            self.a += 1;
            self.fork = Fork::Irrelevant;
        } else {
            self.h += 1;
            self.fork = Fork::Relevant;
        }
        Observation {
            reward: 0,
            progress: 0,
            done: false,
        }
    }

    fn apply_active_wait_and_match(&mut self) -> Observation {
        if sample!(self.rv_mining) {
            self.a += 1;
            self.fork = Fork::Active;
            Observation {
                reward: 0,
                progress: 0,
                done: false,
            }
        } else {
            self.fork = Fork::Relevant;
            if sample!(self.rv_network) {
                let h = self.h;
                self.a -= h;
                self.h = 1;
                Observation {
                    reward: h,
                    progress: h,
                    done: false,
                }
            } else {
                self.h += 1;
                Observation {
                    reward: 0,
                    progress: 0,
                    done: false,
                }
            }
        }
    }

    fn apply_override(&mut self) -> Observation {
        let h = self.h;
        if sample!(self.rv_mining) {
            self.a -= h;
            self.h = 0;
            self.fork = Fork::Irrelevant;
        } else {
            self.a -= h + 1;
            self.h = 1;
            self.fork = Fork::Relevant;
        }
        Observation {
            reward: h + 1,
            progress: h + 1,
            done: false,
        }
    }

    fn apply_adopt(&mut self) -> Observation {
        let h = self.h;
        if sample!(self.rv_mining) {
            self.a = 1;
            self.h = 0;
        } else {
            self.a = 0;
            self.h = 1;
        }
        self.fork = Fork::Irrelevant;
        Observation {
            reward: 0,
            progress: h,
            done: false,
        }
    }

    fn step(&mut self, a: usize) -> Observation {
        let mut obs = match self.actions[a] {
            Action::Wait => match self.fork {
                Fork::Active => self.apply_active_wait_and_match(),
                _ => self.apply_non_active_wait(),
            },
            Action::Override => self.apply_override(),
            Action::Match => self.apply_active_wait_and_match(),
            Action::Adopt => self.apply_adopt(),
        };

        // probabilistic termination; Bar-Zur @ AFT 20
        for _ in 0..obs.progress {
            if sample!(self.rv_termination) {
                obs.done = true;
                self.init();
                return obs;
            }
        }

        self.set_actions();
        obs
    }

    fn __repr__(&self) -> String {
        let a = self.a;
        let h = self.h;
        let f = &self.fork;
        let act = &self.actions;
        format!("FC16SSZwPT {{ a: {a}, h: {h}, fork: {f:?}, actions: {act:?} }}")
    }
}

#[pymodule]
fn cpr_gym_rs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Observation>()?;
    m.add_class::<FC16SSZwPT>()?;
    Ok(())
}
