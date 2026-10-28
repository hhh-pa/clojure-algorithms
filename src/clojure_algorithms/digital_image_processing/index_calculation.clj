(ns clojure-algorithms.digital-image-processing.index-calculation
  "Port of digital_image_processing/index_calculation.py.
  Vegetation indices computed element-wise over scalars or nested
  band matrices. Bands are passed as a map with keys :red :green
  :blue :red-edge :nir.")

(defn- emap2
  "Element-wise binary F over scalars/nested vectors (broadcasts)."
  [f a b]
  (cond
    (and (sequential? a) (sequential? b)) (mapv #(emap2 f %1 %2) a b)
    (sequential? a) (mapv #(emap2 f % b) a)
    (sequential? b) (mapv #(emap2 f a %) b)
    :else (f a b)))

(defn- emap1
  "Element-wise unary F over a scalar or nested vectors."
  [f a]
  (if (sequential? a) (mapv #(emap1 f %) a) (f a)))

(def ^:private e+ #(emap2 + %1 %2))
(def ^:private e- #(emap2 - %1 %2))
(def ^:private e* #(emap2 * %1 %2))
(def ^:private e-div #(emap2 / %1 %2))
(def ^:private e** #(emap2 (fn [a b] (Math/pow (double a) (double b))) %1 %2))
(def ^:private e-abs #(emap1 (fn [a] (Math/abs (double a))) %))
(def ^:private e-atan #(emap1 (fn [a] (Math/atan (double a))) %))

(defn arv12 [{:keys [red nir]}]
  (e+ -0.18 (e* 1.17 (e-div (e- nir red) (e+ nir red)))))

(defn ccci [{:keys [red red-edge nir]}]
  (e-div (e-div (e- nir red-edge) (e+ nir red-edge))
         (e-div (e- nir red) (e+ nir red))))

(defn cvi [{:keys [red green nir]}]
  (e* nir (e-div red (e* green green))))

(defn gli [{:keys [red green blue]}]
  (e-div (e- (e* 2 green) (e+ red blue))
         (e+ (e* 2 green) (e+ red blue))))

(defn ndvi [{:keys [red nir]}]
  (e-div (e- nir red) (e+ nir red)))

(defn bndvi [{:keys [blue nir]}]
  (e-div (e- nir blue) (e+ nir blue)))

(defn red-edge-ndvi [{:keys [red red-edge]}]
  (e-div (e- red-edge red) (e+ red-edge red)))

(defn gndvi [{:keys [green nir]}]
  (e-div (e- nir green) (e+ nir green)))

(defn gbndvi [{:keys [green blue nir]}]
  (e-div (e- nir (e+ green blue)) (e+ nir (e+ green blue))))

(defn grndvi [{:keys [red green nir]}]
  (e-div (e- nir (e+ green red)) (e+ nir (e+ green red))))

(defn rbndvi [{:keys [red blue nir]}]
  (e-div (e- nir (e+ blue red)) (e+ nir (e+ blue red))))

(defn pndvi [{:keys [red green blue nir]}]
  (e-div (e- nir (e+ (e+ green red) blue))
         (e+ nir (e+ (e+ green red) blue))))

(defn atsavi
  ([bands] (atsavi bands 0.08 1.22 0.03))
  ([{:keys [red nir]} x a b]
   (e* a (e-div (e- (e- nir (e* a red)) b)
                (e+ (e- (e+ (e* a nir) red) (e* a b))
                    (e* x (e+ 1 (e* a a))))))))

(defn bwdrvi [{:keys [blue nir]}]
  (e-div (e- (e* 0.1 nir) blue) (e+ (e* 0.1 nir) blue)))

(defn ci-green [{:keys [green nir]}]
  (e- (e-div nir green) 1))

(defn ci-rededge [{:keys [red-edge nir]}]
  (e- (e-div nir red-edge) 1))

(defn ci [{:keys [red blue]}]
  (e-div (e- red blue) red))

(defn ctvi [bands]
  (let [ndvi-val (e+ (ndvi bands) 0.5)]
    (e* (e-div ndvi-val (e-abs ndvi-val)) (e** (e-abs ndvi-val) 0.5))))

(defn gdvi [{:keys [green nir]}]
  (e- nir green))

(defn evi [{:keys [red blue nir]}]
  (e* 2.5 (e-div (e- nir red)
                 (e+ (e- (e+ nir (e* 6 red)) (e* 7.5 blue)) 1))))

(defn gemi [{:keys [red nir]}]
  (let [n (e-div (e+ (e+ (e* 2 (e- (e* nir nir) (e* red red)))
                         (e* 1.5 nir))
                     (e* 0.5 red))
                 (e+ (e+ nir red) 0.5))]
    (e- (e* n (e- 1 (e* 0.25 n)))
        (e-div (e- red 0.125) (e- 1 red)))))

(defn gosavi
  ([bands] (gosavi bands 0.16))
  ([{:keys [green nir]} y]
   (e-div (e- nir green) (e+ (e+ nir green) y))))

(defn gsavi
  ([bands] (gsavi bands 0.5))
  ([{:keys [green nir]} n]
   (e* (e-div (e- nir green) (e+ (e+ nir green) n)) (e+ 1 n))))

(defn hue [{:keys [red green blue]}]
  (e-atan (e* (e-div (e- (e* 2 red) (e+ green blue)) 30.5)
              (e- green blue))))

(defn ivi
  "Ideal vegetation index. A (soil line slope) and B (intercept) are
  required (the Python defaults of nil crash)."
  [{:keys [red nir]} a b]
  (e-div (e- nir b) (e* a red)))

(defn ipvi [bands]
  (e* (e-div (:nir bands) (e-div (e+ (:nir bands) (:red bands)) 2))
      (e+ (ndvi bands) 1)))

(defn intensity [{:keys [red green blue]}]
  (e-div (e+ (e+ red green) blue) 30.5))

(defn rvi [{:keys [red nir]}]
  (e-div nir red))

(defn mrvi [bands]
  (let [r (rvi bands)]
    (e-div (e- r 1) (e+ r 1))))

(defn m-savi [{:keys [red nir]}]
  (e-div (e- (e+ (e* 2 nir) 1)
             (e** (e- (e** (e+ (e* 2 nir) 1) 2)
                      (e* 8 (e- nir red)))
                  0.5))
         2))

(defn norm-g [{:keys [red green nir]}]
  (e-div green (e+ (e+ nir red) green)))

(defn norm-nir [{:keys [red green nir]}]
  (e-div nir (e+ (e+ nir red) green)))

(defn norm-r [{:keys [red green nir]}]
  (e-div red (e+ (e+ nir red) green)))

(defn ngrdi [{:keys [red green]}]
  (e-div (e- green red) (e+ green red)))

(defn ri [{:keys [red green]}]
  (e-div (e- red green) (e+ red green)))

(defn s-index [{:keys [red green blue]}]
  (let [mx (apply max (mapcat #(if (sequential? %) (flatten %) [%])
                              [red green blue]))
        mn (apply min (mapcat #(if (sequential? %) (flatten %) [%])
                              [red green blue]))]
    (/ (- mx mn) mx)))

(defn shape-index [{:keys [red green blue]}]
  (e-div (e- (e* 2 red) (e+ green blue)) (e- green blue)))

(defn dvi [{:keys [red nir]}]
  (e-div nir red))

(defn tvi [bands]
  (e** (e+ (ndvi bands) 0.5) 0.5))

(defn ndre [{:keys [red-edge nir]}]
  (e-div (e- nir red-edge) (e+ nir red-edge)))

(def indices
  {"ARVI2" arv12 "CCCI" ccci "CVI" cvi "GLI" gli "NDVI" ndvi
   "BNDVI" bndvi "redEdgeNDVI" red-edge-ndvi "GNDVI" gndvi
   "GBNDVI" gbndvi "GRNDVI" grndvi "RBNDVI" rbndvi "PNDVI" pndvi
   "ATSAVI" atsavi "BWDRVI" bwdrvi "CIgreen" ci-green
   "CIrededge" ci-rededge "CI" ci "CTVI" ctvi "GDVI" gdvi "EVI" evi
   "GEMI" gemi "GOSAVI" gosavi "GSAVI" gsavi "Hue" hue "IVI" nil
   "IPVI" ipvi "I" intensity "RVI" rvi "MRVI" mrvi "MSAVI" m-savi
   "NormG" norm-g "NormNIR" norm-nir "NormR" norm-r "NGRDI" ngrdi
   "RI" ri "S" s-index "IF" shape-index "DVI" dvi "TVI" tvi
   "NDRE" ndre})

(defn calculation
  "Value of the index named INDEX for BANDS, or false when unknown."
  [bands index]
  (if (contains? indices index)
    (if-let [f (get indices index)]
      (f bands)
      (throw (ex-info "IVI needs soil-line parameters; call ivi directly"
                      {:type :value-error})))
    (do (println "Index not in the list!")
        false)))
