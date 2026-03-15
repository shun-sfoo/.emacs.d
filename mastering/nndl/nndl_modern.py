from __future__ import annotations

import gzip
import pickle
import random
from collections.abc import Iterable
import numpy as np
import numpy.typing as npt


# ---- 类型定义 (PEP 695) ----

type NDArrayFloat = npt.NDArray[np.float64]
type NDArrayInt = npt.NDArray[np.int64]

type MNISTRaw = tuple[
    tuple[NDArrayFloat, NDArrayInt],
    tuple[NDArrayFloat, NDArrayInt],
    tuple[NDArrayFloat, NDArrayInt],
]

type TrainingSample = tuple[NDArrayFloat, NDArrayFloat]
type EvalSample = tuple[NDArrayFloat, int]


# ---- 数据加载 ----


def load_data() -> MNISTRaw:
    with gzip.open("resources/mnist.pkl.gz", "rb") as f:
        training_data, validation_data, test_data = pickle.load(
            f, encoding="iso-8859-1"
        )
    return training_data, validation_data, test_data


def vectorized_result(j: int) -> NDArrayFloat:
    e: NDArrayFloat = np.zeros((10, 1))
    e[j] = 1.0
    return e


def load_data_wrapper() -> tuple[
    list[TrainingSample],
    list[EvalSample],
    list[EvalSample],
]:
    tr_d, va_d, te_d = load_data()

    training_inputs: list[NDArrayFloat] = [np.reshape(x, (784, 1)) for x in tr_d[0]]
    training_results: list[NDArrayFloat] = [vectorized_result(int(y)) for y in tr_d[1]]
    training_data = list(zip(training_inputs, training_results))

    validation_inputs: list[NDArrayFloat] = [np.reshape(x, (784, 1)) for x in va_d[0]]
    validation_data = list(zip(validation_inputs, va_d[1].tolist()))

    test_inputs: list[NDArrayFloat] = [np.reshape(x, (784, 1)) for x in te_d[0]]
    test_data = list(zip(test_inputs, te_d[1].tolist()))

    return training_data, validation_data, test_data


# ---- 数学函数 ----


def sigmoid(z: NDArrayFloat) -> NDArrayFloat:
    return 1.0 / (1.0 + np.exp(-z))


def sigmoid_prime(z: NDArrayFloat) -> NDArrayFloat:
    return sigmoid(z) * (1 - sigmoid(z))


# ---- 神经网络 ----


class Network:
    num_layers: int
    sizes: list[int]
    biases: list[NDArrayFloat]
    weights: list[NDArrayFloat]

    def __init__(self, sizes: list[int]) -> None:
        self.num_layers = len(sizes)
        self.sizes = sizes
        self.biases = [np.random.randn(y, 1) for y in sizes[1:]]
        self.weights = [np.random.randn(y, x) for x, y in zip(sizes[:-1], sizes[1:])]

    def feedforward(self, a: NDArrayFloat) -> NDArrayFloat:
        for b, w in zip(self.biases, self.weights):
            a = sigmoid(np.dot(w, a) + b)
        return a

    def SGD(
        self,
        training_data: list[TrainingSample],
        epochs: int,
        mini_batch_size: int,
        eta: float,
        test_data: list[EvalSample] | None = None,
    ) -> None:

        n = len(training_data)

        for j in range(epochs):
            random.shuffle(training_data)

            mini_batches: list[list[TrainingSample]] = [
                training_data[k : k + mini_batch_size]
                for k in range(0, n, mini_batch_size)
            ]

            for mini_batch in mini_batches:
                self.update_mini_batch(mini_batch, eta)

            if test_data is not None:
                n_test = len(test_data)
                print(f"Epoch {j}: {self.evaluate(test_data)} / {n_test}")
            else:
                print(f"Epoch {j} complete")

    def update_mini_batch(
        self,
        mini_batch: Iterable[TrainingSample],
        eta: float,
    ) -> None:

        nabla_b = [np.zeros(b.shape) for b in self.biases]
        nabla_w = [np.zeros(w.shape) for w in self.weights]

        for x, y in mini_batch:
            delta_nabla_b, delta_nabla_w = self.backprop(x, y)

            nabla_b = [nb + dnb for nb, dnb in zip(nabla_b, delta_nabla_b)]
            nabla_w = [nw + dnw for nw, dnw in zip(nabla_w, delta_nabla_w)]

        self.weights = [
            w - (eta / len(list(mini_batch))) * nw
            for w, nw in zip(self.weights, nabla_w)
        ]

        self.biases = [
            b - (eta / len(list(mini_batch))) * nb
            for b, nb in zip(self.biases, nabla_b)
        ]

    def backprop(
        self,
        x: NDArrayFloat,
        y: NDArrayFloat,
    ) -> tuple[list[NDArrayFloat], list[NDArrayFloat]]:

        nabla_b = [np.zeros(b.shape) for b in self.biases]
        nabla_w = [np.zeros(w.shape) for w in self.weights]

        activation = x
        activations: list[NDArrayFloat] = [x]
        zs: list[NDArrayFloat] = []

        for b, w in zip(self.biases, self.weights):
            z = np.dot(w, activation) + b
            zs.append(z)
            activation = sigmoid(z)
            activations.append(activation)

        delta = self.cost_derivative(activations[-1], y) * sigmoid_prime(zs[-1])

        nabla_b[-1] = delta
        nabla_w[-1] = np.dot(delta, activations[-2].T)

        for l in range(2, self.num_layers):
            z = zs[-l]
            sp = sigmoid_prime(z)
            delta = np.dot(self.weights[-l + 1].T, delta) * sp
            nabla_b[-l] = delta
            nabla_w[-l] = np.dot(delta, activations[-l - 1].T)

        return nabla_b, nabla_w

    def evaluate(self, test_data: list[EvalSample]) -> int:
        test_results = [
            (int(np.argmax(self.feedforward(x))), y) for (x, y) in test_data
        ]
        return sum(int(x == y) for (x, y) in test_results)

    def cost_derivative(
        self,
        output_activations: NDArrayFloat,
        y: NDArrayFloat,
    ) -> NDArrayFloat:
        return output_activations - y


if __name__ == "__main__":
    training_data, validation_data, test_data = load_data_wrapper()
    net = Network([784, 30, 10])
    net.SGD(training_data, 30, 10, 3.0, test_data=test_data)
