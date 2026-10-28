(ns clojure-algorithms.digital-image-processing.dithering.burkes
  "Port of digital_image_processing/dithering/burkes.py.
  Input pixels are [blue green red] triples (cv2 order).
  (The Python error table is sized [width+1][height+4] and the output
  (width, height, 3), which only works for square images; this port
  sizes both correctly and agrees on square inputs.)")

(defn get-greyscale
  "Greyscale value of BLUE GREEN RED."
  [blue green red]
  (+ (* 0.114 blue) (* 0.587 green) (* 0.299 red)))

(defn valid-threshold?
  "True when THRESHOLD is strictly between 0 and 255."
  [threshold]
  (< 0 threshold 255))

(defn burkes-process
  "Black-and-white dithered image for INPUT-IMG and THRESHOLD."
  [input-img threshold]
  (when-not (valid-threshold? threshold)
    (throw (ex-info (str "Factor value should be from 0 to 255")
                    {:type :value-error})))
  (let [height (count input-img)
        width (count (first input-img))
        errors (atom (vec (repeat (inc height) (vec (repeat (+ width 4) 0)))))]
    (mapv (fn [y]
            (mapv (fn [x]
                    (let [pixel (nth (nth input-img y) x)
                          greyscale (int (apply get-greyscale pixel))
                          err (+ greyscale (get-in @errors [y x]))
                          black? (> threshold err)
                          current-error (if black? err (- err 255))]
                      (swap! errors
                             (fn [e]
                               (-> e
                                   (update-in [y (+ x 1)]
                                              + (int (* (/ 8 32) current-error)))
                                   (update-in [y (+ x 2)]
                                              + (int (* (/ 4 32) current-error)))
                                   (update-in [(inc y) x]
                                              + (int (* (/ 8 32) current-error)))
                                   (update-in [(inc y) (+ x 1)]
                                              + (int (* (/ 4 32) current-error)))
                                   (update-in [(inc y) (+ x 2)]
                                              + (int (* (/ 2 32) current-error)))
                                   (update-in [(inc y) (mod (dec x) (+ width 4))]
                                              + (int (* (/ 4 32) current-error)))
                                   (update-in [(inc y) (mod (- x 2) (+ width 4))]
                                              + (int (* (/ 2 32) current-error))))))
                      (if black? [0 0 0] [255 255 255])))
                  (range width)))
          (range height))))
