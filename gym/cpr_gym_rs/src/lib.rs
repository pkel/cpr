use numpy::{array, IntoPyArray, PyArray1};
use pyo3::prelude::*;
use pyo3::types::PyDict;
use rand::distributions::{Bernoulli, Distribution};

#[derive(Copy, Clone, Debug)]
enum Fork {
    Irrelevant,
    Relevant,
    Active,
}

impl Fork {
    pub fn index(&self) -> usize {
        *self as usize
    }
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

    fn observe<'py>(&self, py: Python<'py>) -> &'py PyArray1<f64> {
        let mut obs = array![self.a as f64, self.h as f64, self.fork.index() as f64];

        // map 0..inf -> 0..1
        for item in obs.iter_mut() {
            *item = *item / (1. + *item);
        }

        obs.into_pyarray(py)
    }

    fn reset<'py>(&mut self, py: Python<'py>) -> (&'py PyArray1<f64>, &'py PyDict) {
        self.init();
        let obs = self.observe(py);
        let info = PyDict::new(py); // return python None value?!
        (obs, info)
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

    fn n_actions(&self) -> usize {
        self.actions.len()
    }

    fn describe_action(&self, a: usize) -> String {
        format!("{:?}", self.actions[a])
    }

    fn apply_non_active_wait(&mut self) -> (u32, u32) {
        if sample!(self.rv_mining) {
            self.a += 1;
            self.fork = Fork::Irrelevant;
        } else {
            self.h += 1;
            self.fork = Fork::Relevant;
        }
        (0, 0)
    }

    fn apply_active_wait_and_match(&mut self) -> (u32, u32) {
        if sample!(self.rv_mining) {
            self.a += 1;
            self.fork = Fork::Active;
            (0, 0)
        } else {
            self.fork = Fork::Relevant;
            if sample!(self.rv_network) {
                let h = self.h;
                self.a -= h;
                self.h = 1;
                (h, 0)
            } else {
                self.h += 1;
                (0, 0)
            }
        }
    }

    fn apply_override(&mut self) -> (u32, u32) {
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
        (h + 1, h + 1)
    }

    fn apply_adopt(&mut self) -> (u32, u32) {
        let h = self.h;
        if sample!(self.rv_mining) {
            self.a = 1;
            self.h = 0;
        } else {
            self.a = 0;
            self.h = 1;
        }
        self.fork = Fork::Irrelevant;
        (0, h)
    }

    fn step<'py>(
        &mut self,
        py: Python<'py>,
        a: usize,
    ) -> (&'py PyArray1<f64>, f64, bool, bool, &'py PyDict) {
        let a = if a < self.actions.len() { a } else { 0 };

        let (rew, progress) = match self.actions[a] {
            Action::Wait => match self.fork {
                Fork::Active => self.apply_active_wait_and_match(),
                _ => self.apply_non_active_wait(),
            },
            Action::Override => self.apply_override(),
            Action::Match => self.apply_active_wait_and_match(),
            Action::Adopt => self.apply_adopt(),
        };

        let mut term = false;

        // probabilistic termination; Bar-Zur @ AFT 20
        for _ in 0..progress {
            if sample!(self.rv_termination) {
                term = true;
                break;
            }
        }

        self.set_actions();

        let rew = rew as f64;
        let trunc = false;
        let info = PyDict::new(py); // return python None value?!
        let obs = self.observe(py);

        (obs, rew, term, trunc, info)
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
    m.add_class::<FC16SSZwPT>()?;
    Ok(())
}
