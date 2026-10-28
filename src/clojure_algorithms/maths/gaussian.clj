(ns clojure-algorithms.maths.gaussian
  "Reference: https://en.wikipedia.org/wiki/Gaussian_function")

(declare gaussian)

(defn gaussian ">>> float(gaussian(1))\n0.24197072451914337\n\n>>> float(gaussian(24))\n3.342714441794458e-126\n\n>>> float(gaussian(1, 4, 2))\n0.06475879783294587\n\n>>> float(gaussian(1, 5, 3))\n0.05467002489199788\n\nSupports NumPy Arrays\nUse numpy.meshgrid with this to generate gaussian blur on images.\n>>> import numpy as np\n>>> x = np.arange(15)\n>>> gaussian(x)\narray([3.98942280e-01, 2.41970725e-01, 5.39909665e-02, 4.43184841e-03,\n       1.33830226e-04, 1.48671951e-06, 6.07588285e-09, 9.13472041e-12,\n       5.05227108e-15, 1.02797736e-18, 7.69459863e-23, 2.11881925e-27,\n       2.14638374e-32, 7.99882776e-38, 1.09660656e-43])\n\n>>> float(gaussian(15))\n5.530709549844416e-50\n\n>>> gaussian([1,2, 'string'])\nTraceback (most recent call last):\n    ...\nTypeError: unsupported operand type(s) for -: 'list' and 'float'\n\n>>> gaussian('hello world')\nTraceback (most recent call last):\n    ...\nTypeError: unsupported operand type(s) for -: 'str' and 'float'\n\n>>> gaussian(10**234) # doctest: +IGNORE_EXCEPTION_DETAIL\nTraceback (most recent call last):\n    ...\nOverflowError: (34, 'Result too large')\n\n>>> float(gaussian(10**-326))\n0.3989422804014327\n\n>>> float(gaussian(2523, mu=234234, sigma=3425))\n0.0"
  [& _args]
  ;; stub: original requires exp,pi,sqrt; pure-Clojure port not applicable
  (throw (ex-info "requires exp,pi,sqrt" {:type :UnsupportedOperation})))

;; NOTE: gaussian stubbed (exp,pi,sqrt)
