(ns clojure-algorithms.physics.fresnel-diffract
  "Fresnel diffraction of coherent, monochromatic 1D/2D wave fields via the
  transfer-function approach (adapted from Voelz, Computational Fourier Optics).

  Pure-Clojure port: the numpy/scipy FFT backend is replaced by a naive DFT on
  complex numbers represented as [real imag] pairs. Wave fields are vectors of
  numbers (1D) or vectors of row vectors (2D); results are complex vectors of
  the same shape.")

(defn- c+ [[a b] [c d]] [(+ a c) (+ b d)])

(defn- c* [[a b] [c d]] [(- (* a c) (* b d)) (+ (* a d) (* b c))])

(defn- cexp [theta] [(Math/cos theta) (Math/sin theta)])

(defn- ->complex [x] (if (sequential? x) (vec x) [(double x) 0.0]))

(defn- dft
  "Naive discrete Fourier transform. sign -1 = forward, +1 = inverse."
  [xs sign]
  (let [n (count xs)]
    (mapv (fn [k]
            (reduce c+ [0.0 0.0]
                    (map-indexed
                     (fn [j x]
                       (c* (->complex x)
                           (cexp (* sign 2 Math/PI (/ (* k j) (double n))))))
                     xs)))
          (range n))))

(defn- fft [xs] (dft xs -1))

(defn- ifft [xs]
  (let [n (count xs)]
    (mapv (fn [[r i]] [(/ r n) (/ i n)]) (dft xs 1))))

(defn- fftshift-1d
  "Rotates a vector by half its length (fftshift for even n)."
  [xs]
  (let [n (count xs) h (quot n 2)]
    (vec (concat (subvec (vec xs) h) (subvec (vec xs) 0 h)))))

(defn- ifftshift-1d
  "Inverse fftshift rotation."
  [xs]
  (let [n (count xs) h (quot (inc n) 2)]
    (vec (concat (subvec (vec xs) h) (subvec (vec xs) 0 h)))))

(defn- transpose [m] (apply mapv vector m))

(defn- fftshift-2d [m] (fftshift-1d (mapv fftshift-1d m)))

(defn- ifftshift-2d [m] (ifftshift-1d (mapv ifftshift-1d m)))

(defn- spectrum-2d
  "Forward 2D transform matching fft2(fftshift(wave)): shift, fft rows, fft cols."
  [m]
  (let [shifted (fftshift-2d m)
        rows (mapv fft shifted)]
    (transpose (mapv fft (transpose rows)))))

(defn- image-2d
  "Inverse 2D transform matching ifftshift(ifft2(f)): ifft rows, ifft cols, unshift."
  [m]
  (let [rows (mapv ifft m)
        cols (transpose (mapv ifft (transpose rows)))]
    (ifftshift-2d cols)))

(defn- transfer-1d
  "Transfer function exp(-i*pi*lambda*z*fx^2), fftshifted."
  [pixel-num pixel-size wavelength prop-dist]
  (let [side-length (* pixel-num pixel-size)
        fx0 (/ -1 (* 2 pixel-size))
        step (/ 1 side-length)]
    (fftshift-1d
     (mapv (fn [j]
             (let [fx (+ fx0 (* j step))
                   theta (* -1 Math/PI wavelength prop-dist fx fx)]
               (cexp theta)))
           (range pixel-num)))))

(defn fresnel-diffract-1d
  "Fresnel diffraction of a 1D wave field (vector of numbers).
  Returns a vector of [re im] complex pairs."
  [wavefunc-0 pixel-size wavelength prop-dist]
  (let [pixel-num (count wavefunc-0)
        transferf (transfer-1d pixel-num pixel-size wavelength prop-dist)
        f-wavefunc-0 (fft (fftshift-1d (mapv ->complex wavefunc-0)))
        prod (mapv c* transferf f-wavefunc-0)]
    (ifftshift-1d (ifft prod))))

(defn fresnel-diffract-2d
  "Fresnel diffraction of a 2D wave field (vector of row vectors).
  Returns a matrix of [re im] complex pairs."
  [wavefunc-0 pixel-size wavelength prop-dist]
  (let [pixel-num (count wavefunc-0)
        side-length (* pixel-num pixel-size)
        fx0 (/ -1 (* 2 pixel-size))
        step (/ 1 side-length)
        fx (mapv #(+ fx0 (* % step)) (range pixel-num))
        transferf (fftshift-2d
                   (mapv (fn [row-fy]
                           (mapv (fn [row-fx]
                                   (cexp (* -1 Math/PI wavelength prop-dist
                                            (+ (* row-fx row-fx) (* row-fy row-fy)))))
                                 fx))
                         fx))
        f-wavefunc-0 (spectrum-2d (mapv #(mapv ->complex %) wavefunc-0))
        prod (mapv (fn [tr fr] (mapv c* tr fr)) transferf f-wavefunc-0)]
    (image-2d prod)))

(defn fresnel-diffract
  "Fresnel diffraction of a 1D or 2D wave field.
  Throws ex-info for anything that is not 1D or 2D."
  [wavefunc-0 pixel-size wavelength prop-dist]
  (let [v (vec wavefunc-0)]
    (cond
      (and (seq v) (sequential? (first v)) (sequential? (first (first v))))
      (throw (ex-info (format "Expected a 1D or 2D wavefield, but got %s"
                              (str [(count v) (count (first v))
                                    (count (first (first v)))]))
                      {}))
      (and (seq v) (sequential? (first v)))
      (fresnel-diffract-2d v pixel-size wavelength prop-dist)
      :else
      (fresnel-diffract-1d v pixel-size wavelength prop-dist))))

(defn total-energy
  "Sum of squared magnitudes of a (possibly nested) complex wave field."
  [wave]
  (reduce + (map (fn [x]
                   (if (and (sequential? x) (number? (first x)))
                     (let [[r i] x] (+ (* r r) (* i i)))
                     (total-energy x)))
                 wave)))
