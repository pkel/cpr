use pyo3::prelude::*;
use std::collections::HashMap;

mod fc16;
mod generic;
mod proto;

use generic::{Action, Env};
use proto::nakamoto;

#[derive(Copy, Clone)]
#[pyclass]
enum Protocol {
    Nakamoto,
}

// #[pyclass] neither works on enum nor generic struct. I go an extra mile here and resolve the
// generics manually. I suspect there is some way to use a dynamic box or something, but
// investigating this is currently not worth the effort.

enum BoxedEnv {
    Nakamoto(Env<nakamoto::Protocol, nakamoto::Data, nakamoto::BaseObserver>),
}
// Boxed here means boxed type, not boxed data.

#[pyclass]
struct GenericEnv {
    env: BoxedEnv,
}

#[pymethods]
impl GenericEnv {
    #[new]
    fn new(p: Protocol, alpha: f32, gamma: f32, horizon: f32) -> Self {
        match p {
            Protocol::Nakamoto => GenericEnv {
                env: BoxedEnv::Nakamoto(Env::new(
                    nakamoto::Protocol {},
                    nakamoto::BaseObserver {},
                    alpha,
                    gamma,
                    horizon,
                )),
            },
        }
    }

    fn reset(&mut self, py: Python) -> (PyObject, HashMap<String, PyObject>) {
        match &mut self.env {
            BoxedEnv::Nakamoto(env) => env.py_reset(py),
        }
    }

    fn step(
        &mut self,
        py: Python,
        a: Action,
    ) -> (PyObject, f64, bool, bool, HashMap<String, PyObject>) {
        match &mut self.env {
            BoxedEnv::Nakamoto(env) => env.py_step(py, a),
        }
    }

    fn action_range(&self) -> (Action, Action) {
        match &self.env {
            BoxedEnv::Nakamoto(env) => env.action_range(),
        }
    }

    fn describe_action(&self, a: Action) -> String {
        match &self.env {
            BoxedEnv::Nakamoto(env) => env.describe_action(a),
        }
    }

    fn __repr__(&self) -> String {
        match &self.env {
            BoxedEnv::Nakamoto(env) => env.describe(),
        }
    }
}

#[pymodule]
#[pyo3(name = "_rust")]
fn cpr_gym_rs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<fc16::FC16SSZwPT>()?;
    m.add_class::<Protocol>()?;
    m.add_class::<GenericEnv>()?;
    Ok(())
}
