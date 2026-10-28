(ns clojure-algorithms.fractals.mandelbrot)

(defn get-distance
  "Relative distance (step / max-step) after which the complex number
  (x, y) diverges. Members of the Mandelbrot set return 1.0."
  [x y max-step]
  (loop [a (double x) b (double y) step 0]
    (let [a-new (+ (- (* a a) (* b b)) x)
          b-new (+ (* 2 a b) y)]
      (if (or (> (+ (* a-new a-new) (* b-new b-new)) 4)
              (= step (dec max-step)))
        (/ (double step) (dec max-step))
        (recur a-new b-new (inc step))))))

(defn get-black-and-white-rgb
  "Black for set members, white otherwise."
  [distance]
  (if (= distance 1) [0 0 0] [255 255 255]))

(defn- hsv-to-rgb
  "Convert h/s/v in [0,1] to an [r g b] triple in [0,1]."
  [h s v]
  (let [h (* (mod h 1.0) 6)
        i (int (Math/floor h))
        f (- h i)
        p (* v (- 1 s))
        q (* v (- 1 (* s f)))
        t (* v (- 1 (* s (- 1 f))))]
    (case (mod i 6)
      0 [v t p] 1 [q v p] 2 [p v t]
      3 [p q v] 4 [t p v] 5 [v p q])))

(defn get-color-coded-rgb
  "Black for set members, otherwise an HSV rainbow color by distance."
  [distance]
  (if (= distance 1)
    [0 0 0]
    (mapv #(Math/round (double (* % 255))) (hsv-to-rgb distance 1 1))))

(defn pixel-grid
  "Pure-data version of get-image (which needs PIL to build an image):
  returns rows of [r g b] vectors for the requested view."
  ([] (pixel-grid {}))
  ([{:keys [image-width image-height figure-center-x figure-center-y
            figure-width max-step use-distance-color-coding]
     :or {image-width 800 image-height 600 figure-center-x -0.6
          figure-center-y 0 figure-width 3.2 max-step 50
          use-distance-color-coding true}}]
   (let [color-fn (if use-distance-color-coding
                    get-color-coded-rgb get-black-and-white-rgb)
         figure-height (* (/ figure-width image-width) image-height)]
     (mapv (fn [image-y]
             (mapv (fn [image-x]
                     (let [figure-x (+ figure-center-x
                                       (* (- (/ image-x image-width) 0.5) figure-width))
                           figure-y (+ figure-center-y
                                       (* (- (/ image-y image-height) 0.5) figure-height))]
                       (color-fn (get-distance figure-x figure-y max-step))))
                   (range image-width)))
           (range image-height)))))

;; Note: PIL image construction/display (Image.new / img.show) is not
;; ported; use pixel-grid for the underlying color data.
