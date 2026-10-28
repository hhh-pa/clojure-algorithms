(ns clojure-algorithms.machine-learning.federated-averaging
  "Federated averaging (FedAvg) utilities.\n\nThis module provides a simple NumPy-based implementation of the FedAvg\naggregation algorithm. It supports equal weighting and custom non-negative\nweights that are normalized internally.\n\nDoctests\n========\n\nBasic equal-weight averaging across two \"clients\" with two tensors each\n(vector and 2x2 matrix):\n\n>>> A = [\n...     np.array([1.0, 2.0]),\n...     np.array([[1.0, 2.0], [3.0, 4.0]]),\n... ]\n>>> B = [\n...     np.array([3.0, 4.0]),\n...     np.array([[5.0, 6.0], [7.0, 8.0]]),\n... ]\n>>> eq = federated_average([A, B])\n>>> eq[0].tolist()\n[2.0, 3.0]\n>>> eq[1].tolist()\n[[3.0, 4.0], [5.0, 6.0]]\n\nWeighted averaging with weights [2, 1] (normalized to [2/3, 1/3]):\n\n>>> w = federated_average(\n...     [A, B],\n...     weights=np.array([2.0, 1.0]),\n... )\n>>> w[0].tolist()\n[1.6666666666666665, 2.6666666666666665]\n>>> w[1].tolist()\n[[2.333333333333333, 3.333333333333333], [4.333333333333333, 5.333333333333333]]\n\nError cases:\n\n- No clients\n\n>>> federated_average([])  # doctest: +ELLIPSIS\nTraceback (most recent call last):\n...\nValueError: client_models must be a non-empty list\n\n- Mismatched number of tensors per client\n\n>>> C = [np.array([1.0, 2.0])]  # only one tensor\n>>> federated_average([A, C])  # doctest: +ELLIPSIS\nTraceback (most recent call last):\n...\nValueError: All clients must have the same number of tensors\n\n- Mismatched tensor shapes across clients\n\n>>> C2 = [\n...     np.array([1.0, 2.0]),\n...     np.array([[1.0, 2.0]]),\n... ]  # second tensor has different shape\n>>> federated_average([A, C2])  # doctest: +ELLIPSIS\nTraceback (most recent call last):\n...\nValueError: Client 2 tensor shape (1, 2) does not match (2, 2)\n\n- Invalid weights: negative or wrong shape or zero-sum\n\n>>> federated_average([A, B], weights=np.array([1.0, -1.0]))  # doctest: +ELLIPSIS\nTraceback (most recent call last):\n...\nValueError: weights must be non-negative\n\n>>> federated_average([A, B], weights=np.array([0.0, 0.0]))  # doctest: +ELLIPSIS\nTraceback (most recent call last):\n...\nValueError: weights must sum to a positive value\n\n>>> federated_average(\n...     [A, B],\n...     weights=np.array([1.0, 2.0, 3.0]),\n... )  # doctest: +ELLIPSIS\nTraceback (most recent call last):\n...\nValueError: weights must have shape (2,)")

(declare -validate-clients -normalize-weights federated-average)

(defn -validate-clients
  [& _args]
  ;; stub: original requires np,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:shape" {:type :UnsupportedOperation})))

(defn -normalize-weights
  [& _args]
  ;; stub: original requires np,numpy-attr:any,numpy-attr:shape,numpy-attr:sum; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:any,numpy-attr:shape,numpy-attr:sum" {:type :UnsupportedOperation})))

(defn federated-average "Compute the weighted average of clients' model tensors.\n\nParameters\n----------\nclient_models : Sequence[Sequence[np.ndarray]]\n    A list of clients, each being a sequence of NumPy arrays (tensors).\n    All clients must have the same number of tensors with identical shapes.\nweights : np.ndarray | None, optional\n    A 1-D array of non-negative weights, one per client. If None,\n    equal weighting is used. Weights are normalized to sum to 1.\n\nReturns\n-------\nlist[np.ndarray]\n    The list of aggregated tensors with the same shapes as the inputs."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

;; NOTE: _validate_clients stubbed (np,numpy-attr:shape)
;; NOTE: _normalize_weights stubbed (np,numpy-attr:any,numpy-attr:shape,numpy-attr:sum)
;; NOTE: federated_average stubbed (np)
