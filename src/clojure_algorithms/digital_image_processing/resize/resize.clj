(ns clojure-algorithms.digital-image-processing.resize.resize
  "Port of digital_image_processing/resize/resize.py.
  Nearest-neighbour resizing over nested vectors.")

(defn make-resizer
  "Resizer state for IMG with destination size DST-W x DST-H."
  [img dst-width dst-height]
  (when (or (< dst-width 0) (< dst-height 0))
    (throw (ex-info "Destination width/height should be > 0"
                    {:type :value-error})))
  {:img img
   :src-w (count (first img))
   :src-h (count img)
   :dst-w dst-width
   :dst-h dst-height
   :ratio-x (/ (count (first img)) dst-width)
   :ratio-y (/ (count img) dst-height)})

(defn get-x
  "Parent X coordinate for destination X."
  [{:keys [ratio-x]} x]
  (int (* ratio-x x)))

(defn get-y
  "Parent Y coordinate for destination Y."
  [{:keys [ratio-y]} y]
  (int (* ratio-y y)))

(defn process
  "Nearest-neighbour resized image."
  [{:keys [img dst-w dst-h] :as resizer}]
  (mapv (fn [i]
          (mapv (fn [j]
                  (nth (nth img (get-y resizer i)) (get-x resizer j)))
                (range dst-w)))
        (range dst-h)))
