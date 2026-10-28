(ns clojure-algorithms.machine-learning.multilayer-perceptron-classifier
  "Port of machine_learning/multilayer_perceptron_classifier.py" (:require [clojure-algorithms.runtime.python :refer :all]))

(declare make-Dataloader Dataloader-get-train-test-data Dataloader-shuffle-data poly-get-inout-dim Dataloader-get-inout-dim Dataloader-one-hot-encode make-MLP MLP-get-inout-dim MLP-initialize MLP-relu MLP-relu-derivative MLP-forward MLP-back-prop MLP-update-weights MLP-update-learning-rate MLP-accuracy MLP-loss MLP-get-acc-loss MLP-train rng)

(defrecord Dataloader [X y class-weights])

(defrecord MLP [learning-rate gamma epoch hidden-dim train-loss train-accuracy test-loss test-accuracy dataloader inter-variable weights1-list])

(defn make-Dataloader "Constructor for Dataloader. DataLoader class for handling dataset, including data shuffling,\none-hot encoding, and train-test splitting.\n\nExample usage:\n>>> X = [[0.0, 0.0], [1.0, 1.0], [1.0, 0.0], [0.0, 1.0]]\n>>> y = [0, 1, 0, 0]\n>>> loader = Dataloader(X, y)\n>>> len(loader.get_train_test_data())  # Returns train and test data\n4\n>>> loader.one_hot_encode([0, 1, 0], 2)  # Returns one-hot encoded labels\narray([[0.99, 0.  ],\n       [0.  , 0.99],\n       [0.99, 0.  ]])\n>>> loader.get_inout_dim()\n(2, 3)\n>>> loader.one_hot_encode([0, 2], 3)\narray([[0.99, 0.  , 0.  ],\n       [0.  , 0.  , 0.99]])"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn make-MLP "Constructor for MLP. A custom MLP class for implementing a simple multi-layer perceptron with\nforward propagation, backpropagation.\n\nAttributes:\n    learning_rate (float): Learning rate for gradient descent.\n    gamma (float): Parameter to control learning rate adjustment.\n    epoch (int): Number of epochs for training.\n    hidden_dim (int): Dimension of the hidden layer.\n    batch_size (int): Number of samples per mini-batch.\n    train_loss (List[float]): List to store training loss for each fold.\n    train_accuracy (List[float]): List to store training accuracy for each fold.\n    test_loss (List[float]): List to store test loss for each fold.\n    test_accuracy (List[float]): List to store test accuracy for each fold.\n    dataloader (Dataloader): DataLoader object for handling training data.\n    inter_variable (dict): Dictionary to store intermediate variables\n    for backpropagation.\n    weights1_list (List[Tuple[np.ndarray, np.ndarray]]):\n    List of weights for each fold.\n\nMethods:\n    get_inout_dim:obtain input dimension and output dimension.\n    relu: Apply the ReLU activation function.\n    relu_derivative: Compute the derivative of the ReLU function.\n    forward: Perform a forward pass through the network.\n    back_prop: Perform backpropagation to compute gradients.\n    update_weights: Update the weights using gradients.\n    update_learning_rate: Adjust the learning rate based on test accuracy.\n    accuracy: Compute accuracy of the model.\n    loss: Compute weighted MSE loss.\n    train: Train the MLP over multiple folds with early stopping."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defmulti poly-get-inout-dim (fn [s & _] (class (if (instance? clojure.lang.Atom s) @s s))))

(defn Dataloader-get-train-test-data "Splits the data into training and testing sets.\nHere, we manually split the data.\n\nReturns:\n    A tuple containing:\n    - Train data\n    - Train labels\n    - Test data\n    - Test labels"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn Dataloader-shuffle-data "Shuffles the data randomly.\n\nArgs:\n    paired_data: List of tuples containing data and corresponding labels.\n\nReturns:\n    A shuffled list of data-label pairs."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defmethod poly-get-inout-dim Dataloader [& _] (throw (ex-info "requires numpy-attr:shape" {:type :UnsupportedOperation})))

(defn Dataloader-one-hot-encode "Perform one-hot encoding for the given labels.\n\nArgs:\n    labels: List of integer labels.\n    num_classes: Total number of classes for encoding.\n\nReturns:\n    A numpy array representing one-hot encoded labels."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defmethod poly-get-inout-dim MLP
  ([self] (let [input-dim (atom nil) output-dim (atom nil)] (try (let [__d1 (vec (poly-get-inout-dim (atom (:dataloader @self))))] (reset! input-dim (nth __d1 0)) (reset! output-dim (nth __d1 1))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@input-dim @output-dim]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn MLP-initialize "Initialize weights using He initialization.\n\n:return: Tuple of weights (w1, w2) for the network.\n\n>>> X = [[0.0, 0.0], [1.0, 1.0], [1.0, 0.0], [0.0, 1.0]]\n>>> y = [0, 1, 0, 0]\n>>> loader = Dataloader(X, y)\n>>> mlp = MLP(loader, 10, 0.1)\n>>> w1, w2 = mlp.initialize()\n>>> w1.shape\n(3, 2)\n>>> w2.shape\n(2, 3)"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn MLP-relu "Apply the ReLU activation function element-wise.\n\n:param input_array: Input array.\n:return: Output array after applying ReLU.\n\n>>> mlp = MLP(None, 1, 0.1)\n>>> mlp.relu(np.array([[-1, 2], [3, -4]]))\narray([[0, 2],\n       [3, 0]])"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn MLP-relu-derivative "Compute the derivative of the ReLU function.\n\n:param input_array: Input array.\n:return: Derivative of ReLU function element-wise.\n\n>>> mlp = MLP(None, 1, 0.01)\n>>> mlp.relu_derivative(np.array([[-1, 2], [3, -4]]))\narray([[0., 1.],\n       [1., 0.]])"
  [& _args]
  ;; stub: original requires np,numpy-attr:astype; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:astype" {:type :UnsupportedOperation})))

(defn MLP-forward "Performs a forward pass through the neural network with one hidden layer.\n\nArgs:\n    input_data: Input data, shape (batch_size, input_dim).\n    w1: Weight matrix for input to hidden layer,\n    shape (input_dim + 1, hidden_dim).\n    w2: Weight matrix for hidden to output layer,\n    shape (hidden_dim, output_dim).\n    no_gradient: If True, returns output without storing intermediates.\n\nReturns:\n    Output of the network after forward pass, shape (batch_size, output_dim).\n\nExamples:\n    >>> mlp = MLP(None, 1, 0.1, hidden_dim=2)\n    >>> x = np.array([[1.0, 2.0, 1.0]])  # batch_size=1, input_dim=2 + bias\n    >>> w1 = np.array([[0.1, 0.2], [0.3, 0.4], [0.5, 0.6]])\n    >>> w2 = np.array([[0.7, 0.8], [0.9, 1.0]])\n    >>> output = mlp.forward(x, w1, w2)\n    >>> output.shape\n    (1, 2)"
  [& _args]
  ;; stub: original requires np,numpy-attr:dot; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:dot" {:type :UnsupportedOperation})))

(defn MLP-back-prop "Performs backpropagation to compute gradients for the weights.\n\nArgs:\n    input_data: Input data, shape (batch_size, input_dim).\n    true_labels: True labels, shape (batch_size, output_dim).\n    w2: Weight matrix for hidden to output layer,\n    shape (hidden_dim, output_dim).\n\nReturns:\n    Tuple of gradients (grad_w1, grad_w2) for the weight matrices.\nExamples:\n    >>> mlp = MLP(None, 1, 0.1, hidden_dim=2)\n    >>> x = np.array([[1.0, 2.0, 1.0]])  # batch_size=1, input_dim=2 + bias\n    >>> y = np.array([[0.0, 1.0]])  # batch_size=1, output_dim=2\n    >>> w1 = np.array([[0.1, 0.2], [0.3, 0.4], [0.5, 0.6]])\n    >>> w2 = np.array([[0.7, 0.8], [0.9, 1.0]])  # (hidden_dim=2, output_dim=2)\n    >>> _ = mlp.forward(x, w1, w2)  # Run forward to set inter_variable\n    >>> grad_w1, grad_w2 = mlp.back_prop(x, y, w2)\n    >>> grad_w1.shape\n    (3, 2)\n    >>> grad_w2.shape\n    (2, 2)"
  [& _args]
  ;; stub: original requires np,numpy-attr:T,numpy-attr:dot,numpy-attr:reshape,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:T,numpy-attr:dot,numpy-attr:reshape,numpy-attr:shape" {:type :UnsupportedOperation})))

(defn MLP-update-weights "Updates the weight matrices using the computed gradients and learning rate.\n\nArgs:\n    w1: Weight matrix for input to hidden layer,\n    shape (input_dim + 1, hidden_dim).\n    w2: Weight matrix for hidden to output layer,\n    shape (hidden_dim, output_dim).\n    grad_w1: Gradient for w1,\n    shape (input_dim + 1, hidden_dim).\n    grad_w2: Gradient for w2,\n    shape (hidden_dim, output_dim).\n    learning_rate: Learning rate for weight updates.\n\nReturns:\n    Updated weight matrices (w1, w2).\n\nExamples:\n    >>> mlp = MLP(None, 1, 0.1)\n    >>> w1 = np.array([[0.1, 0.2], [0.3, 0.4], [0.5, 0.6]])\n    >>> w2 = np.array([[0.7, 0.8], [0.9, 1.0]])\n    >>> grad_w1 = np.array([[0.1, 0.2], [0.3, 0.4], [0.5, 0.6]])\n    >>> grad_w2 = np.array([[0.7, 0.8], [0.9, 1.0]])\n    >>> lr = 0.1\n    >>> new_w1, new_w2 = mlp.update_weights(w1, w2, grad_w1, grad_w2, lr)\n    >>> new_w1==np.array([[0.09, 0.18], [0.27, 0.36], [0.45, 0.54]])\n    array([[ True,  True],\n           [ True,  True],\n           [ True,  True]])\n    >>> new_w2==np.array([[0.63, 0.72], [0.81, 0.90]])\n    array([[ True,  True],\n           [ True,  True]])"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn MLP-update-learning-rate "Updates the learning rate by applying the decay factor gamma.\n\nArgs:\n    learning_rate: Current learning rate.\n\nReturns:\n    Updated learning rate.\n\nExamples:\n    >>> mlp = MLP(None, 1, 0.1, gamma=0.9)\n    >>> round(mlp.update_learning_rate(0.1), 2)\n    0.09"
  [self learning-rate] (py-mul learning-rate (:gamma @self)))

(defn MLP-accuracy "Computes the accuracy of predictions by comparing predicted and true labels.\n\nArgs:\n    label: True labels, shape (batch_size, num_classes).\n    y_hat: Predicted outputs, shape (batch_size, num_classes).\n\nReturns:\n    Accuracy as a float between 0 and 1.\n\nExamples:\n    >>> mlp = MLP(None, 1, 0.01)\n    >>> label = np.array([[1, 0], [0, 1], [1, 0]])\n    >>> y_hat = np.array([[0.9, 0.1], [0.2, 0.8], [0.6, 0.4]])\n    >>> mlp.accuracy(label, y_hat)\n    np.float64(1.0)"
  [& _args]
  ;; stub: original requires np,numpy-attr:argmax,numpy-attr:mean; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:argmax,numpy-attr:mean" {:type :UnsupportedOperation})))

(defn MLP-loss "Computes the mean squared error loss between predictions and true labels.\n\nArgs:\n    output: Predicted outputs, shape (batch_size, num_classes).\n    label: True labels, shape (batch_size, num_classes).\n\nReturns:\n    Mean squared error loss as a float.\n\nExamples:\n    >>> mlp = MLP(None, 1, 0.1)\n    >>> output = np.array([[0.9, 0.1], [0.2, 0.8]])\n    >>> label = np.array([[1.0, 0.0], [0.0, 1.0]])\n    >>> round(mlp.loss(output, label), 3)\n    np.float64(0.025)"
  [& _args]
  ;; stub: original requires np,numpy-attr:shape,numpy-attr:sum; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:shape,numpy-attr:sum" {:type :UnsupportedOperation})))

(defn MLP-get-acc-loss "Returns the recorded test accuracy and test loss.\n\nReturns:\n    Tuple of (test_accuracy, test_loss) lists.\n\nExamples:\n    >>> mlp = MLP(None, 1, 0.1)\n    >>> mlp.test_accuracy = [0.8, 0.9]\n    >>> mlp.test_loss = [0.1, 0.05]\n    >>> acc, loss = mlp.get_acc_loss()\n    >>> acc\n    [0.8, 0.9]\n    >>> loss\n    [0.1, 0.05]"
  [self] [(:test-accuracy @self) (:test-loss @self)])

(defn MLP-train "Trains the MLP model using the provided dataloader\nfor multiple folds and epochs.\n\nSaves the best model parameters for each fold and records accuracy/loss.\n\nExamples:\n    >>> X = [[0.0, 0.0], [1.0, 1.0], [1.0, 0.0], [0.0, 1.0]]\n    >>> y = [0, 1, 0, 0]\n    >>> loader = Dataloader(X, y)\n    >>> mlp = MLP(loader, epoch=2, learning_rate=0.1, hidden_dim=2)\n    >>> mlp.train() # doctest: +ELLIPSIS\n    Test accuracy: ..."
  [& _args]
  ;; stub: original requires np,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:shape" {:type :UnsupportedOperation})))

(def Dataloader-get-inout-dim poly-get-inout-dim)

(def MLP-get-inout-dim poly-get-inout-dim)

(def rng nil)

;; NOTE: Dataloader __init__ stubbed (np)
;; NOTE: Dataloader.get_train_test_data stubbed (np)
;; NOTE: Dataloader.shuffle_data stubbed (np)
;; NOTE: Dataloader.get_inout_dim stubbed (numpy-attr:shape)
;; NOTE: Dataloader.one_hot_encode stubbed (np)
;; NOTE: MLP __init__ stubbed (np)
;; NOTE: MLP.initialize stubbed (np)
;; NOTE: MLP.relu stubbed (np)
;; NOTE: MLP.relu_derivative stubbed (np,numpy-attr:astype)
;; NOTE: MLP.forward stubbed (np,numpy-attr:dot)
;; NOTE: MLP.back_prop stubbed (np,numpy-attr:T,numpy-attr:dot,numpy-attr:reshape,numpy-attr:shape)
;; NOTE: MLP.update_weights stubbed (np)
;; NOTE: ... and 4 more
