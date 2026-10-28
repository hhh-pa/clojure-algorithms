(ns clojure-algorithms.computer-vision.flip-augmentation
  "Flip augmentation bounding-box math. cv2 image IO is omitted; only the
  pure annotation transforms and the random filename helper are ported.")

(defn flip-bbox-h
  "Mirrors a normalized [class x y w h] box horizontally."
  [[cls x y w h]]
  [cls (- 1 x) y w h])

(defn flip-bbox-v
  "Mirrors a normalized [class x y w h] box vertically."
  [[cls x y w h]]
  [cls x (- 1 y) w h])

(defn flip-annos
  "Flips annotation boxes; flip-type 1 is horizontal, 0 is vertical."
  [annos flip-type]
  (mapv (if (= 1 flip-type) flip-bbox-h flip-bbox-v) annos))

(defn random-chars
  "Generates a random lowercase/digit string of length n."
  ([] (random-chars 32))
  ([n]
   (assert (> n 1) "The number of character should greater than 1")
   (apply str (repeatedly n #(rand-nth "abcdefghijklmnopqrstuvwxyz0123456789")))))
