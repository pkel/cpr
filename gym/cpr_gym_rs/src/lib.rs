use pyo3::prelude::*;
use rand::distributions::Bernoulli;
use rand::distributions::Distribution;

#[pyclass(get_all)]
#[derive(Debug)]
struct Observation {
    reward: u32,
    progress: u32,
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
struct FC16SSZ {
    a: u32,
    h: u32,
    fork: Fork,
    rv_mining: Bernoulli,
    rv_network: Bernoulli,
    actions: Vec<Action>,
}

macro_rules! sample {
    ($rv:expr) => {
        $rv.sample(&mut rand::thread_rng())
    };
}

#[pymethods]
impl FC16SSZ {
    #[new]
    fn new(alpha: f64, gamma: f64) -> Self {
        let rv_mining = Bernoulli::new(alpha).unwrap();
        let lucky_start = sample!(rv_mining);

        let mut obj = FC16SSZ {
            a: if lucky_start { 1 } else { 0 },
            h: if lucky_start { 0 } else { 1 },
            fork: Fork::Irrelevant,
            rv_mining,
            rv_network: Bernoulli::new(gamma).unwrap(),
            actions: vec![],
        };

        obj.set_actions();
        obj
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
        }
    }

    fn apply_active_wait_and_match(&mut self) -> Observation {
        if sample!(self.rv_mining) {
            self.a += 1;
            self.fork = Fork::Active;
            Observation {
                reward: 0,
                progress: 0,
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
                }
            } else {
                self.h += 1;
                Observation {
                    reward: 0,
                    progress: 0,
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
        }
    }

    fn step(&mut self, a: usize) -> Observation {
        let obs = match self.actions[a] {
            Action::Wait => match self.fork {
                Fork::Active => self.apply_active_wait_and_match(),
                _ => self.apply_non_active_wait(),
            },
            Action::Override => self.apply_override(),
            Action::Match => self.apply_active_wait_and_match(),
            Action::Adopt => self.apply_adopt(),
        };
        self.set_actions();
        obs
    }

    fn __repr__(&self) -> String {
        let a = self.a;
        let h = self.h;
        let f = &self.fork;
        let act = &self.actions;
        format!("FC16SSZ {{ a: {a}, h: {h}, fork: {f:?}, actions: {act:?} }}")
    }
}

#[pymodule]
fn cpr_gym_rs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Observation>()?;
    m.add_class::<FC16SSZ>()?;
    Ok(())
}
