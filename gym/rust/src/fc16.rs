use numpy::{array, IntoPyArray};
use pyo3::prelude::*;
use rand::distributions::{Bernoulli, Distribution};
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, PartialEq)]
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
pub struct FC16SSZwPT {
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

type State = (u32, u32, Fork); // a, h, fork
type Transition = (u32, u32, Fork, u32, u32); // a, h, fork, reward, progress

impl FC16SSZwPT {
    fn set_actions(&mut self) {
        self.actions.clear();
        self.actions.push(Action::Wait);
        self.actions.push(Action::Adopt);
        if self.a > self.h {
            self.actions.push(Action::Override);
        }
        if self.a >= self.h {
            self.actions.push(Action::Match);
        }
    }

    fn observe(&self, py: Python) -> PyObject {
        let mut obs = array![0., 0., 0.];
        obs[0] = self.a as f64;
        obs[1] = self.h as f64;
        obs[2] = self.fork.index() as f64;

        // map 0..inf -> 0..1
        for item in obs.iter_mut() {
            *item = *item / (1. + *item);
        }

        obs.into_pyarray(py).into()
    }

    fn start(&self) -> State {
        if sample!(self.rv_mining) {
            (1, 0, Fork::Irrelevant)
        } else {
            (0, 1, Fork::Irrelevant)
        }
    }

    fn apply(&self, a: usize) -> Transition {
        match self.actions[a] {
            Action::Wait => match self.fork {
                Fork::Active => self.apply_active_wait_and_match(),
                _ => self.apply_non_active_wait(),
            },
            Action::Override => self.apply_override(),
            Action::Match => self.apply_active_wait_and_match(),
            Action::Adopt => self.apply_adopt(),
        }
    }

    fn apply_non_active_wait(&self) -> Transition {
        assert!(self.fork != Fork::Active);
        if sample!(self.rv_mining) {
            (self.a + 1, self.h, Fork::Irrelevant, 0, 0)
        } else {
            (self.a, self.h + 1, Fork::Relevant, 0, 0)
        }
    }

    fn apply_active_wait_and_match(&self) -> Transition {
        assert!(self.fork == Fork::Active || self.a >= self.h);
        if sample!(self.rv_mining) {
            (self.a + 1, self.h, Fork::Active, 0, 0)
        } else {
            if sample!(self.rv_network) {
                (self.a - self.h, 1, Fork::Relevant, self.h, 0)
            } else {
                (self.a, self.h + 1, Fork::Relevant, 0, 0)
            }
        }
    }

    fn apply_override(&self) -> Transition {
        assert!(self.a > self.h);
        if sample!(self.rv_mining) {
            (self.a - self.h, 0, Fork::Irrelevant, self.h + 1, self.h + 1)
        } else {
            (
                self.a - self.h - 1,
                1,
                Fork::Relevant,
                self.h + 1,
                self.h + 1,
            )
        }
    }

    fn apply_adopt(&self) -> Transition {
        if sample!(self.rv_mining) {
            (1, 0, Fork::Irrelevant, 0, self.h)
        } else {
            (0, 1, Fork::Irrelevant, 0, self.h)
        }
    }
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

        (obj.a, obj.h, obj.fork) = obj.start();
        obj.set_actions();
        obj
    }

    fn reset(&mut self, py: Python) -> (PyObject, HashMap<String, PyObject>) {
        (self.a, self.h, self.fork) = self.start();
        self.set_actions();

        let obs = self.observe(py);
        let info = HashMap::new();
        (obs, info)
    }

    fn n_actions(&self) -> usize {
        self.actions.len()
    }

    fn describe_action(&self, a: usize) -> String {
        format!("{:?}", self.actions[a])
    }

    fn step(
        &mut self,
        py: Python,
        a: usize,
    ) -> (PyObject, f64, bool, bool, HashMap<String, PyObject>) {
        let a = if a < self.actions.len() { a } else { 0 };

        let (reward, progress);

        (self.a, self.h, self.fork, reward, progress) = self.apply(a);

        // probabilistic termination; Bar-Zur @ AFT 20
        let mut terminate = false;
        for _ in 0..progress {
            if sample!(self.rv_termination) {
                terminate = true;
                break;
            }
        }

        self.set_actions();

        let truncate = false;
        let info = HashMap::new();
        let obs = self.observe(py);

        (obs, reward as f64, terminate, truncate, info)
    }

    fn __repr__(&self) -> String {
        format!(
            "FC16SSZwPT {{ a: {}, h: {}, fork: {:?}, actions: {:?} }}",
            self.a, self.h, self.fork, self.actions
        )
    }
}
