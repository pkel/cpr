use pyo3::prelude::*;

mod fc16;
mod generic;

#[pymodule]
#[pyo3(name = "_rust")]
fn cpr_gym_rs(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<fc16::FC16SSZwPT>()?;
    Ok(())
}
