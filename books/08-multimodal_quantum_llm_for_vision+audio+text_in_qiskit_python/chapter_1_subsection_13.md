# Basic Quantum Circuit Building with Qiskit

This section provides a foundational understanding of constructing quantum circuits using Qiskit, essential for implementing quantum machine learning algorithms.  We'll explore the core building blocks and demonstrate how to manipulate quantum bits (qubits) to perform simple computations.  A strong grasp of these fundamental principles is crucial for developing and understanding more sophisticated quantum machine learning models.

**1. Qubits and Quantum Gates:**

At the heart of quantum computation lies the qubit. Unlike classical bits, which exist in either a 0 or 1 state, qubits can exist in a superposition of both states simultaneously, represented as  α|0⟩ + β|1⟩, where α and β are complex numbers and |0⟩ and |1⟩ are the basis states.  This superposition is a key enabling factor in quantum computation's potential.

Qiskit provides a clear and intuitive way to manipulate qubits using quantum gates. These gates act on qubits, changing their superposition states.  Common quantum gates include:

* **`qiskit.circuit.QuantumCircuit.h` (Hadamard gate):**  Transforms a qubit from a definite state (|0⟩ or |1⟩) into a superposition.
* **`qiskit.circuit.QuantumCircuit.x` (Pauli-X gate, NOT gate):** Flips the state of a qubit (|0⟩ becomes |1⟩ and vice versa).
* **`qiskit.circuit.QuantumCircuit.y` (Pauli-Y gate):**  Applies a phase shift to a qubit.
* **`qiskit.circuit.QuantumCircuit.z` (Pauli-Z gate):**  Applies a phase shift to a qubit dependent on its state.
* **`qiskit.circuit.QuantumCircuit.cx` (CNOT gate):**  A controlled-NOT gate, where the second qubit flips if the first qubit is in the |1⟩ state.
* **`qiskit.circuit.QuantumCircuit.measure`:**  Measures the qubits and obtains classical bit results.


**2. Creating and Modifying Quantum Circuits:**

Qiskit's `QuantumCircuit` class is the primary tool for building quantum circuits.  We create a `QuantumCircuit` object, specifying the number of qubits and potentially classical bits for measurement.

```python
import qiskit
from qiskit import QuantumCircuit
