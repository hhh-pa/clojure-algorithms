(ns clojure-algorithms.conversions.rgb-hsv-conversion
  "Port of conversions/rgb_hsv_conversion.py.
  Converts between RGB and HSV color representations.")

(defn hsv-to-rgb
  "Convert HSV (hue 0-360, saturation/value 0-1) to [r g b] (0-255)."
  [hue saturation value]
  (when-not (<= 0 hue 360)
    (throw (ex-info "hue should be between 0 and 360" {:type :value-error})))
  (when-not (<= 0 saturation 1)
    (throw (ex-info "saturation should be between 0 and 1"
                    {:type :value-error})))
  (when-not (<= 0 value 1)
    (throw (ex-info "value should be between 0 and 1" {:type :value-error})))
  (let [chroma (* value saturation)
        hue-section (/ hue 60.0)
        second-largest (* chroma (- 1 (Math/abs (- (mod hue-section 2) 1))))
        match (- value chroma)
        c+ (+ chroma match) s+ (+ second-largest match)
        [r g b] (cond
                  (<= hue-section 1) [c+ s+ match]
                  (<= hue-section 2) [s+ c+ match]
                  (<= hue-section 3) [match c+ s+]
                  (<= hue-section 4) [match s+ c+]
                  (<= hue-section 5) [s+ match c+]
                  :else [c+ match s+])]
    [(long (Math/rint (* 255 r)))
     (long (Math/rint (* 255 g)))
     (long (Math/rint (* 255 b)))]))

(defn rgb-to-hsv
  "Convert RGB (0-255 each) to [hue saturation value]."
  [red green blue]
  (doseq [[v n] [[red "red"] [green "green"] [blue "blue"]]]
    (when-not (<= 0 v 255)
      (throw (ex-info (str n " should be between 0 and 255")
                      {:type :value-error}))))
  (let [fr (/ red 255.0) fg (/ green 255.0) fb (/ blue 255.0)
        value (max fr fg fb)
        chroma (- value (min fr fg fb))
        saturation (if (zero? value) 0 (/ chroma value))
        hue (cond
              (zero? chroma) 0.0
              (== value fr) (* 60 (+ 0 (/ (- fg fb) chroma)))
              (== value fg) (* 60 (+ 2 (/ (- fb fr) chroma)))
              :else (* 60 (+ 4 (/ (- fr fg) chroma))))]
    [(mod (+ hue 360) 360) saturation value]))

(defn approximately-equal-hsv
  "True when two HSV colors are approximately equal."
  [hsv-1 hsv-2]
  (and (< (Math/abs (- (nth hsv-1 0) (nth hsv-2 0))) 0.2)
       (< (Math/abs (- (nth hsv-1 1) (nth hsv-2 1))) 0.002)
       (< (Math/abs (- (nth hsv-1 2) (nth hsv-2 2))) 0.002)))
