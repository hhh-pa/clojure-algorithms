(ns clojure-algorithms.maths.radix2-fft
  "Fast Polynomial Multiplication using radix-2 fast Fourier Transform."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare make-FFT FFT-dft FFT-multiply FFT-str)

(defrecord FFT [polyA polyB len-A len-B c-max-length root product])

(defn make-FFT "Constructor for FFT. Fast Polynomial Multiplication using radix-2 fast Fourier Transform.\n\nReference:\nhttps://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm#The_radix-2_DIT_case\n\nFor polynomials of degree m and n the algorithms has complexity\nO(n*logn + m*logm)\n\nThe main part of the algorithm is split in two parts:\n    1) __DFT: We compute the discrete fourier transform (DFT) of A and B using a\n    bottom-up dynamic approach -\n    2) __multiply: Once we obtain the DFT of A*B, we can similarly\n    invert it to obtain A*B\n\nThe class FFT takes two polynomials A and B with complex coefficients as arguments;\nThe two polynomials should be represented as a sequence of coefficients starting\nfrom the free term. Thus, for instance x + 2*x^3 could be represented as\n[0,1,0,2] or (0,1,0,2). The constructor adds some zeros at the end so that the\npolynomials have the same length which is a power of 2 at least the length of\ntheir product.\n\nExample:\n\nCreate two polynomials as sequences\n>>> A = [0, 1, 0, 2]  # x+2x^3\n>>> B = (2, 3, 4, 0)  # 2+3x+4x^2\n\nCreate an FFT object with them\n>>> x = FFT(A, B)\n\nPrint product\n>>> x.product  # 2x + 3x^2 + 8x^3 + 6x^4 + 8x^5\n[(-0-0j), (2+0j), (3-0j), (8-0j), (6+0j), (8+0j)]\n\n__str__ test\n>>> print(x)\nA = 0*x^0 + 1*x^1 + 0*x^2 + 2*x^3\nB = 2*x^0 + 3*x^1 + 4*x^2\nA*B = (-0-0j)*x^0 + (2+0j)*x^1 + (3-0j)*x^2 + (8-0j)*x^3 + (6+0j)*x^4 + (8+0j)*x^5"
  [& _args]
  ;; stub: original requires complex(),mpmath,np; pure-Clojure port not applicable
  (throw (ex-info "requires complex(),mpmath,np" {:type :UnsupportedOperation})))

(defn FFT-dft
  [self which] (let [current-root (atom nil) dft (atom nil) i (atom nil) j (atom nil) new-dft (atom nil) next-ncol (atom nil) root (atom nil)] (try (reset! dft (if (py-truthy (py-eq which "A")) (vec (for [x (py-iter (:polyA @self))] [x])) (vec (for [x (py-iter (:polyB @self))] [x])))) (when (py-truthy (py-le (count @dft) 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get @dft 0)}))) (reset! next-ncol (py-floordiv (:c-max-length @self) 2)) (while (py-truthy (py-gt @next-ncol 0)) (reset! new-dft (vec (for [i (py-iter (range @next-ncol))] []))) (reset! root (py-pow (:root @self) @next-ncol)) (reset! current-root 1) (doseq [__i1 (py-iter (range (py-floordiv (:c-max-length @self) (py-mul @next-ncol 2))))] (reset! j __i1) (doseq [__i2 (py-iter (range @next-ncol))] (reset! i __i2) (conj (vec (py-get @new-dft @i)) (py-add (py-get (py-get @dft @i) @j) (py-mul @current-root (py-get (py-get @dft (py-add @i @next-ncol)) @j))))) (swap! current-root py-mul @root)) (reset! current-root 1) (doseq [__i3 (py-iter (range (py-floordiv (:c-max-length @self) (py-mul @next-ncol 2))))] (reset! j __i3) (doseq [__i4 (py-iter (range @next-ncol))] (reset! i __i4) (conj (vec (py-get @new-dft @i)) (py-sub (py-get (py-get @dft @i) @j) (py-mul @current-root (py-get (py-get @dft (py-add @i @next-ncol)) @j))))) (swap! current-root py-mul @root)) (reset! dft @new-dft) (reset! next-ncol (py-floordiv @next-ncol 2))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get @dft 0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn FFT-multiply
  [& _args]
  ;; stub: original requires complex(),complex-attr; pure-Clojure port not applicable
  (throw (ex-info "requires complex(),complex-attr" {:type :UnsupportedOperation})))

(defn FFT-str
  [self] (let [a (atom nil) b (atom nil) c (atom nil)] (try (reset! a (py-add "A = " (clojure.string/join " + " (map py-str (doall (for [[i coef] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) (py-slice (:polyA @self) nil (:len-A @self) nil)))] (str (py-str coef) "*x^" (py-str i)))))))) (reset! b (py-add "B = " (clojure.string/join " + " (map py-str (doall (for [[i coef] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) (py-slice (:polyB @self) nil (:len-B @self) nil)))] (str (py-str coef) "*x^" (py-str i)))))))) (reset! c (py-add "A*B = " (clojure.string/join " + " (map py-str (doall (for [[i coef] (py-iter (map-indexed (fn [i x] [(+' i 0) x]) (:product @self)))] (str (py-str coef) "*x^" (py-str i)))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (str (py-str @a) "\n" (py-str @b) "\n" (py-str @c))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defmethod print-method FFT [o w] (.write w (FFT-str (atom o))))

;; NOTE: FFT __init__ stubbed (complex(),mpmath,np)
;; NOTE: FFT.__multiply stubbed (complex(),complex-attr)
