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
    // Core functionality

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
    ) -> (PyObject, f32, bool, bool, HashMap<String, PyObject>) {
        match &mut self.env {
            BoxedEnv::Nakamoto(env) => env.py_step(py, a),
        }
    }

    // Observation boundary

    fn low(&self, py: Python) -> PyObject {
        match &self.env {
            BoxedEnv::Nakamoto(env) => env.py_low(py),
        }
    }

    fn high(&self, py: Python) -> PyObject {
        match &self.env {
            BoxedEnv::Nakamoto(env) => env.py_high(py),
        }
    }

    // Action encoding and decoding

    fn encode_action_release(&self, i: u8) -> Action {
        generic::encode_action(generic::ActionHum::Release(i))
    }

    fn encode_action_consider(&self, i: u8) -> Action {
        generic::encode_action(generic::ActionHum::Consider(i))
    }

    fn encode_action_continue(&self) -> Action {
        generic::encode_action(generic::ActionHum::Continue)
    }

    fn describe_action(&self, a: Action) -> String {
        format!("{:?}", generic::decode_action(a))
    }

    // String representation

    fn __repr__(&self) -> String {
        match &self.env {
            BoxedEnv::Nakamoto(env) => env.describe(),
        }
    }
}

// Public Python module

#[pymodule]
#[pyo3(name = "_rust")]
fn cpr_gym_rs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<fc16::FC16SSZwPT>()?;
    m.add_class::<Protocol>()?;
    m.add_class::<GenericEnv>()?;
    Ok(())
}
