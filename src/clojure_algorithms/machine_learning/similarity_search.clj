(ns clojure-algorithms.machine-learning.similarity-search
  "Similarity Search : https://en.wikipedia.org/wiki/Similarity_search\nSimilarity search is a search algorithm for finding the nearest vector from\nvectors, used in natural language processing.\nIn this algorithm, it calculates distance with euclidean distance and\nreturns a list containing two data for each vector:\n    1. the nearest vector\n    2. distance between the vector and the nearest vector (float)")

(declare euclidean similarity-search cosine-similarity)

(defn euclidean "Calculates euclidean distance between two data.\n:param input_a: ndarray of first vector.\n:param input_b: ndarray of second vector.\n:return: Euclidean distance of input_a and input_b. By using math.sqrt(),\n         result will be float.\n\n>>> euclidean(np.array([0]), np.array([1]))\n1.0\n>>> euclidean(np.array([0, 1]), np.array([1, 1]))\n1.0\n>>> euclidean(np.array([0, 0, 0]), np.array([0, 0, 1]))\n1.0"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn similarity-search ":param dataset: Set containing the vectors. Should be ndarray.\n:param value_array: vector/vectors we want to know the nearest vector from dataset.\n:return: Result will be a list containing\n        1. the nearest vector\n        2. distance from the vector\n\n>>> dataset = np.array([[0], [1], [2]])\n>>> value_array = np.array([[0]])\n>>> similarity_search(dataset, value_array)\n[[[0], 0.0]]\n\n>>> dataset = np.array([[0, 0], [1, 1], [2, 2]])\n>>> value_array = np.array([[0, 1]])\n>>> similarity_search(dataset, value_array)\n[[[0, 0], 1.0]]\n\n>>> dataset = np.array([[0, 0, 0], [1, 1, 1], [2, 2, 2]])\n>>> value_array = np.array([[0, 0, 1]])\n>>> similarity_search(dataset, value_array)\n[[[0, 0, 0], 1.0]]\n\n>>> dataset = np.array([[0, 0, 0], [1, 1, 1], [2, 2, 2]])\n>>> value_array = np.array([[0, 0, 0], [0, 0, 1]])\n>>> similarity_search(dataset, value_array)\n[[[0, 0, 0], 0.0], [[0, 0, 0], 1.0]]\n\nThese are the errors that might occur:\n\n1. If dimensions are different.\nFor example, dataset has 2d array and value_array has 1d array:\n>>> dataset = np.array([[1]])\n>>> value_array = np.array([1])\n>>> similarity_search(dataset, value_array)\nTraceback (most recent call last):\n    ...\nValueError: Wrong input data's dimensions... dataset : 2, value_array : 1\n\n2. If data's shapes are different.\nFor example, dataset has shape of (3, 2) and value_array has (2, 3).\nWe are expecting same shapes of two arrays, so it is wrong.\n>>> dataset = np.array([[0, 0], [1, 1], [2, 2]])\n>>> value_array = np.array([[0, 0, 0], [0, 0, 1]])\n>>> similarity_search(dataset, value_array)\nTraceback (most recent call last):\n    ...\nValueError: Wrong input data's shape... dataset : 2, value_array : 3\n\n3. If data types are different.\nWhen trying to compare, we are expecting same types so they should be same.\nIf not, it'll come up with errors.\n>>> dataset = np.array([[0, 0], [1, 1], [2, 2]], dtype=np.float32)\n>>> value_array = np.array([[0, 0], [0, 1]], dtype=np.int32)\n>>> similarity_search(dataset, value_array)  # doctest: +NORMALIZE_WHITESPACE\nTraceback (most recent call last):\n    ...\nTypeError: Input data have different datatype...\ndataset : float32, value_array : int32"
  [& _args]
  ;; stub: original requires np,numpy-attr:dtype,numpy-attr:ndim,numpy-attr:shape,numpy-attr:tolist; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:dtype,numpy-attr:ndim,numpy-attr:shape,numpy-attr:tolist" {:type :UnsupportedOperation})))

(defn cosine-similarity "Calculates cosine similarity between two data.\n:param input_a: ndarray of first vector.\n:param input_b: ndarray of second vector.\n:return: Cosine similarity of input_a and input_b. By using math.sqrt(),\n         result will be float.\n\n>>> cosine_similarity(np.array([1]), np.array([1]))\n1.0\n>>> cosine_similarity(np.array([1, 2]), np.array([6, 32]))\n0.9615239476408232"
  [& _args]
  ;; stub: original requires norm,np,numpy-attr:dot; pure-Clojure port not applicable
  (throw (ex-info "requires norm,np,numpy-attr:dot" {:type :UnsupportedOperation})))

;; NOTE: euclidean stubbed (np)
;; NOTE: similarity_search stubbed (np,numpy-attr:dtype,numpy-attr:ndim,numpy-attr:shape,numpy-attr:tolist)
;; NOTE: cosine_similarity stubbed (norm,np,numpy-attr:dot)
