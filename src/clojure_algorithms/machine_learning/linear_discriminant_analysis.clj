(ns clojure-algorithms.machine-learning.linear-discriminant-analysis
  "Linear Discriminant Analysis\n\n\n\nAssumptions About Data :\n    1. The input variables has a gaussian distribution.\n    2. The variance calculated for each input variables by class grouping is the\n       same.\n    3. The mix of classes in your training set is representative of the problem.\n\n\nLearning The Model :\n    The LDA model requires the estimation of statistics from the training data :\n        1. Mean of each input value for each class.\n        2. Probability of an instance belong to each class.\n        3. Covariance for the input data for each class\n\n    Calculate the class means :\n        mean(x) = 1/n ( for i = 1 to i = n --> sum(xi))\n\n    Calculate the class probabilities :\n        P(y = 0) = count(y = 0) / (count(y = 0) + count(y = 1))\n        P(y = 1) = count(y = 1) / (count(y = 0) + count(y = 1))\n\n    Calculate the variance :\n        We can calculate the variance for dataset in two steps :\n            1. Calculate the squared difference for each input variable from the\n               group mean.\n            2. Calculate the mean of the squared difference.\n            ------------------------------------------------\n            Squared_Difference = (x - mean(k)) ** 2\n            Variance = (1 / (count(x) - count(classes))) *\n                (for i = 1 to i = n --> sum(Squared_Difference(xi)))\n\nMaking Predictions :\n    discriminant(x) = x * (mean / variance) -\n        ((mean ** 2) / (2 * variance)) + Ln(probability)\n    ---------------------------------------------------------------------------\n    After calculating the discriminant value for each class, the class with the\n    largest discriminant value is taken as the prediction.\n\nAuthor: @EverLookNeverSee"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare gaussian-distribution y-generator calculate-mean calculate-probabilities calculate-variance predict-y-values accuracy valid-input main)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-add py-sub py-repeat-vec py-mul py-div py-pow py-min py-min-by py-max py-max-by py-index py-float-str py-quote py-repr py-str py-strip py-just py-log py-rng py-rand py-seed py-gauss py-system)

(def ^:private py-no-dunder (Object.))

(defmulti py-dunder (fn [op a & _] [op (class a)]))

(defmethod py-dunder :default [& _] py-no-dunder)

(defn- py-truthy [v]
  (cond (nil? v) false
        (false? v) false
        (true? v) true
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (instance? clojure.lang.IRecord v)
        (let [r (py-dunder :bool v)]
          (if (identical? r py-no-dunder)
            (let [l (py-dunder :len v)]
              (if (identical? l py-no-dunder) true (not (zero? l))))
            (boolean (py-truthy r))))
        (coll? v) (not (empty? v))
        :else true))

(defn- py-eq [a b]
  (cond (and (number? a) (number? b)) (== a b)
        (and (string? a) (string? b)) (= a b)
        (and (instance? clojure.lang.IRecord a)
             (instance? clojure.lang.IRecord b)
             (= (class a) (class b)))
        (let [r (py-dunder :eq a b)]
          (if (identical? r py-no-dunder) (= a b) (boolean (py-truthy r))))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? (fn [[k v]] (and (contains? b k) (py-eq v (get b k)))) a))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map py-eq a b)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(defn- py-num-bool [x] (if (boolean? x) (if x 1 0) x))

(defn- py-cmp-safe [a b]
  (try (compare (py-num-bool a) (py-num-bool b))
       (catch ClassCastException e
         (throw (ex-info "'<' not supported between instances" {:type :TypeError})))))

(defn- py-lt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :lt a b)]
      (if (identical? r py-no-dunder) (neg? (compare a b)) (boolean (py-truthy r))))
    (neg? (py-cmp-safe a b))))

(defn- py-le [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :le a b)]
      (if (identical? r py-no-dunder) (not (pos? (compare a b))) (boolean (py-truthy r))))
    (not (pos? (py-cmp-safe a b)))))

(defn- py-gt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :gt a b)]
      (if (identical? r py-no-dunder) (pos? (compare a b)) (boolean (py-truthy r))))
    (pos? (py-cmp-safe a b))))

(defn- py-ge [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :ge a b)]
      (if (identical? r py-no-dunder) (not (neg? (compare a b))) (boolean (py-truthy r))))
    (not (neg? (py-cmp-safe a b)))))

(defn- py-in [x c]
  (cond (nil? c) false
        (string? c) (if (string? x)
                      (clojure.string/includes? c x)
                      (throw (ex-info "a string is required" {:type :TypeError})))
        (map? c) (contains? c x)
        (set? c) (contains? c x)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :contains c x)]
          (if (identical? r py-no-dunder) false (boolean (py-truthy r))))
        :else (boolean (some #(py-eq % x) c))))

(defn- py-iter [c]
  (cond (instance? clojure.lang.Atom c) (py-iter @c)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :iter c)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "object is not iterable" {:type :TypeError})) r))
        (nil? c) []
        (map? c) (keys c)
        :else c))

(defn- py-index-norm [k n]
  (let [i (cond (boolean? k) (if k 1 0)
                (integer? k) (long k)
                :else (throw (ex-info "indices must be integers" {:type :TypeError})))
        ii (if (neg? i) (+ n i) i)]
    (when (or (neg? ii) (>= ii n))
      (throw (ex-info "index out of range" {:type :IndexError})))
    ii))

(defn- py-get [c k]
  (cond (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :getitem c k)]
          (if (identical? r py-no-dunder)
            (if (and (keyword? k) (contains? c k)) (get c k)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r))
        (map? c) (if (contains? c k) (get c k)
                     (throw (ex-info (str "key: " (py-repr k)) {:type :KeyError})))
        (string? c) (str (nth c (py-index-norm k (count c))))
        :else (nth (vec (seq c)) (py-index-norm k (count c)))))

(defn- py-add [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (+' a b)
          (and (string? a) (string? b)) (str a b)
          (and (sequential? a) (sequential? b)) (vec (concat a b))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :add a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :add-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for +" {:type :TypeError})))))

(defn- py-sub [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (-' a b)
          (and (set? a) (set? b)) (clojure.set/difference a b)
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :sub a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :sub-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for -" {:type :TypeError})))))

(defn- py-repeat-vec [v n]
  (vec (apply concat (repeat n (seq v)))))

(defn- py-mul [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (*' a b)
          (and (string? a) (integer? b))
          (apply str (repeat (max 0 (long b)) a))
          (and (integer? a) (string? b))
          (apply str (repeat (max 0 (long a)) b))
          (and (sequential? a) (integer? b)) (py-repeat-vec a (max 0 (long b)))
          (and (integer? a) (sequential? b)) (py-repeat-vec b (max 0 (long a)))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :mul a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :mul-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for *" {:type :TypeError})))))

(defn- py-div [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :div a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :div-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (and (number? b) (zero? b))
                  (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
                (if (and (number? a) (number? b))
                  (/ (double a) (double b))
                  (throw (ex-info "unsupported operand for /" {:type :TypeError}))))))

(defn- py-pow [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :pow a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :pow-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (or (not (number? a)) (not (number? b)))
                  (throw (ex-info "unsupported operand for **" {:type :TypeError})))
                (when (and (zero? a) (number? b) (neg? b))
                  (throw (ex-info "0.0 cannot be raised to a negative power" {:type :ZeroDivisionError})))
                (if (and (integer? a) (integer? b) (not (neg? b)))
                  (bigint (.pow (biginteger a) (int b)))
                  (Math/pow (double a) (double b))))))

(defn- py-min [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt b a) b a)) s)))

(defn- py-min-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt (k b) (k a)) b a)) s)))

(defn- py-max [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt b a) b a)) s)))

(defn- py-max-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt (k b) (k a)) b a)) s)))

(defn- py-index [c x]
  (cond (string? c) (let [i (.indexOf ^String c ^String (str x))]
                      (if (neg? i)
                        (throw (ex-info "substring not found" {:type :ValueError})) i))
        (sequential? c) (let [i (first (keep-indexed (fn [j e] (when (py-eq e x) j)) c))]
                          (if (nil? i)
                            (throw (ex-info "not in list" {:type :ValueError})) i))
        :else (throw (ex-info "no attribute 'index'" {:type :AttributeError}))))

(defn- py-float-str [v]
  (let [s (str (double v))]
    (if-let [[_ mant exp] (re-matches #"^(-?\d(?:\.\d+)?)E([+-]?\d+)$" s)]
      (let [e (Long/parseLong exp)]
        (str mant "e" (if (neg? e) "-" "+") (format "%02d" (Math/abs e))))
      s)))

(defn- py-quote [s]
  (let [esc (fn [q] (-> s
                        (clojure.string/replace "\\" "\\\\")
                        (clojure.string/replace "\n" "\\n")
                        (clojure.string/replace "\r" "\\r")
                        (clojure.string/replace "\t" "\\t")
                        (clojure.string/replace q (str "\\" q))))]
    (if (and (clojure.string/includes? s "'") (not (clojure.string/includes? s "\"")))
      (str "\"" (esc "\"") "\"")
      (str "'" (esc "'") "'"))))

(defn- py-repr [v]
  (cond (nil? v) "None"
        (true? v) "True"
        (false? v) "False"
        (string? v) (py-quote v)
        (float? v) (py-float-str v)
        (map? v) (str "{" (clojure.string/join ", "
                                               (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
        (set? v) (if (empty? v) "set()"
                     (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
        (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
        :else (str v)))

(defn- py-str
  ([] "")
  ([v]
   (cond (nil? v) "None"
         (true? v) "True"
         (false? v) "False"
         (string? v) v
         (float? v) (py-float-str v)
         (map? v) (str "{" (clojure.string/join ", "
                                                (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
         (set? v) (if (empty? v) "set()"
                      (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
         (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
         :else (str v))))

(defn- py-strip [s chars side]
  (if (nil? chars)
    (case side :left (clojure.string/replace s #"^\s+" "") :right (clojure.string/replace s #"\s+$" "") (clojure.string/trim s))
    (let [cs (set (seq chars))
          drop-l (fn [sq] (drop-while cs sq))
          drop-r (fn [sq] (reverse (drop-while cs (reverse sq))))]
      (apply str (case side :left (drop-l (seq s)) :right (drop-r (seq s)) (drop-r (drop-l (seq s))))))))

(defn- py-just [side s w fill]
  (let [s (str s) w (long w) f (str (first (str fill)))
        pad (max 0 (- w (count s)))]
    (case side
      :left (str s (apply str (repeat pad f)))
      :right (str (apply str (repeat pad f)) s)
      :center (let [l (quot pad 2) r (- pad l)]
                (str (apply str (repeat l f)) s (apply str (repeat r f)))))))

(defn- py-log
  ([x] (if (and (number? (py-num-bool x)) (pos? (double x))) (Math/log (double x))
           (throw (ex-info "math domain error" {:type :ValueError}))))
  ([x b] (let [b (double b)]
           (when (or (neg? b) (zero? b)) (throw (ex-info "math domain error" {:type :ValueError})))
           (when (== b 1.0) (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
           (/ (py-log x) (Math/log b)))))

(def ^:private py-rng (atom (java.util.Random.)))

(defn- py-rand [] (.nextDouble ^java.util.Random @py-rng))

(defn- py-seed [s]
  (reset! py-rng (if (nil? s) (java.util.Random.)
                     (java.util.Random. (long (if (integer? s) s (hash s))))))
  nil)

(defn- py-gauss [mu sigma]
  (let [u1 (max 1e-300 (py-rand)) u2 (py-rand)
        z (* (Math/sqrt (* -2.0 (Math/log u1))) (Math/cos (* 2.0 Math/PI u2)))]
    (+ (double mu) (* (double sigma) z))))

(defn- py-system [cmd]
  (try (.waitFor (.start (doto (ProcessBuilder. ^java.util.List ["sh" "-c" (str cmd)]) (.inheritIO))))
       (catch Exception _ 0)))

(defn gaussian-distribution "Generate gaussian distribution instances based-on given mean and standard deviation\n:param mean: mean value of class\n:param std_dev: value of standard deviation entered by usr or default value of it\n:param instance_count: instance number of class\n:return: a list containing generated values based-on given mean, std_dev and\n    instance_count\n\n>>> gaussian_distribution(5.0, 1.0, 20) # doctest: +NORMALIZE_WHITESPACE\n[6.288184753155463, 6.4494456086997705, 5.066335808938262, 4.235456349028368,\n 3.9078267848958586, 5.031334516831717, 3.977896829989127, 3.56317055489747,\n  5.199311976483754, 5.133374604658605, 5.546468300338232, 4.086029056264687,\n   5.005005283626573, 4.935258239627312, 3.494170998739258, 5.537997178661033,\n    5.320711100998849, 7.3891120432406865, 5.202969177309964, 4.855297691835079]"
  [mean std-dev instance-count] (try (py-seed 1) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [--v (py-iter (range instance-count))] (py-gauss mean std-dev)))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn y-generator "Generate y values for corresponding classes\n:param class_count: Number of classes(data groupings) in dataset\n:param instance_count: number of instances in class\n:return: corresponding values for data groupings in dataset\n\n>>> y_generator(1, [10])\n[0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n>>> y_generator(2, [5, 10])\n[0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]\n>>> y_generator(4, [10, 5, 15, 20]) # doctest: +NORMALIZE_WHITESPACE\n[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,\n 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]"
  [class-count instance-count] (vec (for [k (py-iter (range class-count)) --v (py-iter (range (py-get instance-count k)))] k)))

(defn calculate-mean "Calculate given class mean\n:param instance_count: Number of instances in class\n:param items: items that related to specific class(data grouping)\n:return: calculated actual mean of considered class\n\n>>> items = gaussian_distribution(5.0, 1.0, 20)\n>>> calculate_mean(len(items), items)\n5.011267842911003"
  [instance-count items] (py-div (reduce +' 0 items) instance-count))

(defn calculate-probabilities "Calculate the probability that a given instance will belong to which class\n:param instance_count: number of instances in class\n:param total_count: the number of all instances\n:return: value of probability for considered class\n\n>>> calculate_probabilities(20, 60)\n0.3333333333333333\n>>> calculate_probabilities(30, 100)\n0.3"
  [instance-count total-count] (py-div instance-count total-count))

(defn calculate-variance "Calculate the variance\n:param items: a list containing all items(gaussian distribution of all classes)\n:param means: a list containing real mean values of each class\n:param total_count: the number of all instances\n:return: calculated variance for considered dataset\n\n>>> items = gaussian_distribution(5.0, 1.0, 20)\n>>> means = [5.011267842911003]\n>>> total_count = 20\n>>> calculate_variance([items], means, total_count)\n0.9618530973487491"
  [items means total-count] (let [i (atom nil) j (atom nil) n-classes (atom nil) squared-diff (atom nil)] (try (reset! squared-diff []) (doseq [__i1 (py-iter (range (count items)))] (reset! i __i1) (doseq [__i2 (py-iter (range (count (py-get items @i))))] (reset! j __i2) (swap! squared-diff conj (py-pow (py-sub (py-get (py-get items @i) @j) (py-get means @i)) 2)))) (reset! n-classes (count means)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-div 1 (py-sub total-count @n-classes)) (reduce +' 0 @squared-diff))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn predict-y-values "This function predicts new indexes(groups for our data)\n:param x_items: a list containing all items(gaussian distribution of all classes)\n:param means: a list containing real mean values of each class\n:param variance: calculated value of variance by calculate_variance function\n:param probabilities: a list containing all probabilities of classes\n:return: a list containing predicted Y values\n\n>>> x_items = [[6.288184753155463, 6.4494456086997705, 5.066335808938262,\n...                4.235456349028368, 3.9078267848958586, 5.031334516831717,\n...                3.977896829989127, 3.56317055489747, 5.199311976483754,\n...                5.133374604658605, 5.546468300338232, 4.086029056264687,\n...                5.005005283626573, 4.935258239627312, 3.494170998739258,\n...                5.537997178661033, 5.320711100998849, 7.3891120432406865,\n...                5.202969177309964, 4.855297691835079], [11.288184753155463,\n...                11.44944560869977, 10.066335808938263, 9.235456349028368,\n...                8.907826784895859, 10.031334516831716, 8.977896829989128,\n...                8.56317055489747, 10.199311976483754, 10.133374604658606,\n...                10.546468300338232, 9.086029056264687, 10.005005283626572,\n...                9.935258239627313, 8.494170998739259, 10.537997178661033,\n...                10.320711100998848, 12.389112043240686, 10.202969177309964,\n...                9.85529769183508], [16.288184753155463, 16.449445608699772,\n...                15.066335808938263, 14.235456349028368, 13.907826784895859,\n...                15.031334516831716, 13.977896829989128, 13.56317055489747,\n...                15.199311976483754, 15.133374604658606, 15.546468300338232,\n...                14.086029056264687, 15.005005283626572, 14.935258239627313,\n...                13.494170998739259, 15.537997178661033, 15.320711100998848,\n...                17.389112043240686, 15.202969177309964, 14.85529769183508]]\n\n>>> means = [5.011267842911003, 10.011267842911003, 15.011267842911002]\n>>> variance = 0.9618530973487494\n>>> probabilities = [0.3333333333333333, 0.3333333333333333, 0.3333333333333333]\n>>> predict_y_values(x_items, means, variance,\n...                  probabilities)  # doctest: +NORMALIZE_WHITESPACE\n[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,\n1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,\n2, 2, 2, 2, 2, 2, 2, 2, 2]"
  [x-items means variance probabilities] (let [i (atom nil) j (atom nil) k (atom nil) results (atom nil) temp (atom nil)] (try (reset! results []) (doseq [__i1 (py-iter (range (count x-items)))] (reset! i __i1) (doseq [__i2 (py-iter (range (count (py-get x-items @i))))] (reset! j __i2) (reset! temp []) (doseq [__i3 (py-iter (range (count x-items)))] (reset! k __i3) (swap! temp conj (py-add (py-sub (py-mul (py-get (py-get x-items @i) @j) (py-div (py-get means @k) variance)) (py-div (py-pow (py-get means @k) 2) (py-mul 2 variance))) (py-log (py-get probabilities @k))))) (swap! results conj @temp))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [result (py-iter @results)] (py-index result (py-max result))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn accuracy "Calculate the value of accuracy based-on predictions\n:param actual_y:a list containing initial Y values generated by 'y_generator'\n    function\n:param predicted_y: a list containing predicted Y values generated by\n    'predict_y_values' function\n:return: percentage of accuracy\n\n>>> actual_y = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,\n... 1, 1 ,1 ,1 ,1 ,1 ,1]\n>>> predicted_y = [0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0,\n... 0, 0, 1, 1, 1, 0, 1, 1, 1]\n>>> accuracy(actual_y, predicted_y)\n50.0\n\n>>> actual_y = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,\n... 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]\n>>> predicted_y = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,\n... 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]\n>>> accuracy(actual_y, predicted_y)\n100.0"
  [actual-y predicted-y] (let [correct (atom nil)] (try (reset! correct (reduce +' 0 (doall (for [[i j] (py-iter (map vector actual-y predicted-y)) :when (py-truthy (py-eq i j))] 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-div @correct (count actual-y)) 100)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn valid-input "Ask for user value and validate that it fulfill a condition.\n\n:input_type: user input expected type of value\n:input_msg: message to show user in the screen\n:err_msg: message to show in the screen in case of error\n:condition: function that represents the condition that user input is valid.\n:default: Default value in case the user does not type anything\n:return: user's input"
  ([input-type input-msg err-msg] (valid-input input-type input-msg err-msg (fn [--v] true) nil))
  ([input-type input-msg err-msg condition] (valid-input input-type input-msg err-msg condition nil))
  ([input-type input-msg err-msg condition default] (let [user-input (atom nil)] (try (while (py-truthy true) (try (try (reset! user-input (input-type (let [__b2 (py-strip (do (print input-msg) (flush) (read-line)) nil :both)] (if (py-truthy __b2) __b2 default)))) (if (py-truthy (condition @user-input)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value @user-input}))) (do (println (clojure.string/join " " [(py-str (str (py-str @user-input) ": " (py-str err-msg)))])) (throw (ex-info "__continue__" {:__control__ true :__continue__ true})))) (catch clojure.lang.ExceptionInfo __eh (if (:__control__ (ex-data __eh)) (throw __eh) (let [__t (:type (ex-data __eh))] (cond (contains? #{:ValueError} __t) (do (println (clojure.string/join " " [(py-str (str (py-str @user-input) ": Incorrect input type, expected " (py-str (pr-str (:--name-- input-type)))))]))) :else (throw __eh)))))) (catch clojure.lang.ExceptionInfo __e (if (:__continue__ (ex-data __e)) nil (throw __e))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn main "This function starts execution phase"
  [] (let [a (atom nil) actual-mean (atom nil) actual-means (atom nil) count-v (atom nil) counts (atom nil) i (atom nil) n-classes (atom nil) pre-indexes (atom nil) probabilities (atom nil) probability (atom nil) std-dev (atom nil) user-count (atom nil) user-mean (atom nil) user-means (atom nil) variance (atom nil) x (atom nil) y (atom nil)] (try (while (py-truthy true) (println (clojure.string/join " " [(py-str (py-just :center " Linear Discriminant Analysis " 50 "*"))])) (println (clojure.string/join " " [(py-str (py-mul "*" 50)) (py-str "\n")])) (println (clojure.string/join " " [(py-str "First of all we should specify the number of classes that")])) (println (clojure.string/join " " [(py-str "we want to generate as training dataset")])) (reset! n-classes (valid-input nil nil nil (fn [--v] true) nil)) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (reset! std-dev (valid-input nil nil nil (fn [--v] true) nil)) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (reset! counts []) (doseq [__i1 (py-iter (range @n-classes))] (reset! i __i1) (reset! user-count (valid-input nil nil nil (fn [--v] true) nil)) (swap! counts conj @user-count)) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (reset! user-means []) (doseq [__i2 (py-iter (range @n-classes))] (reset! a __i2) (reset! user-mean (valid-input nil nil nil (fn [--v] true) nil)) (swap! user-means conj @user-mean)) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (println (clojure.string/join " " [(py-str "Standard deviation: ") (py-str @std-dev)])) (doseq [[__i3 __i4] (py-iter (map-indexed (fn [i x] [(+' i 1) x]) @counts))] (reset! i __i3) (reset! count-v __i4) (println (clojure.string/join " " [(py-str (str "Number of instances in class_" (py-str @i) " is: " (py-str @count-v)))]))) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (doseq [[__i5 __i6] (py-iter (map-indexed (fn [i x] [(+' i 1) x]) @user-means))] (reset! i __i5) (reset! user-mean __i6) (println (clojure.string/join " " [(py-str (str "Mean of class_" (py-str @i) " is: " (py-str @user-mean)))]))) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (reset! x (vec (for [j (py-iter (range @n-classes))] (gaussian-distribution (py-get @user-means j) @std-dev (py-get @counts j))))) (println (clojure.string/join " " [(py-str "Generated Normal Distribution: \n") (py-str @x)])) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (reset! y (y-generator @n-classes @counts)) (println (clojure.string/join " " [(py-str "Generated Corresponding Ys: \n") (py-str @y)])) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (reset! actual-means (vec (for [k (py-iter (range @n-classes))] (calculate-mean (py-get @counts k) (py-get @x k))))) (doseq [[__i7 __i8] (py-iter (map-indexed (fn [i x] [(+' i 1) x]) @actual-means))] (reset! i __i7) (reset! actual-mean __i8) (println (clojure.string/join " " [(py-str (str "Actual(Real) mean of class_" (py-str @i) " is: " (py-str @actual-mean)))]))) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (reset! probabilities (vec (for [i (py-iter (range @n-classes))] (calculate-probabilities (py-get @counts i) (reduce +' 0 @counts))))) (doseq [[__i9 __i10] (py-iter (map-indexed (fn [i x] [(+' i 1) x]) @probabilities))] (reset! i __i9) (reset! probability __i10) (println (clojure.string/join " " [(py-str (str "Probability of class_" (py-str @i) " is: " (py-str @probability)))]))) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (reset! variance (calculate-variance @x @actual-means (reduce +' 0 @counts))) (println (clojure.string/join " " [(py-str "Variance: ") (py-str @variance)])) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (reset! pre-indexes (predict-y-values @x @actual-means @variance @probabilities)) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (println (clojure.string/join " " [(py-str (str "Accuracy: " (py-str (accuracy @y @pre-indexes))))])) (println (clojure.string/join " " [(py-str (py-mul "-" 100))])) (println (clojure.string/join " " [(py-str (py-just :center " DONE " 100 "+"))])) (when (py-truthy (py-eq (clojure.string/lower-case (py-strip (do (print "Press any key to restart or 'q' for quit: ") (flush) (read-line)) nil :both)) "q")) (println (clojure.string/join " " [(py-str (py-add (py-add "\n" (py-just :center "GoodBye!" 100 "-")) "\n"))])) (throw (ex-info "__break__" {:__control__ true :__break__ true}))) (py-system (if (py-truthy (py-eq (System/getProperty "os.name") "nt")) "cls" "clear"))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) nil (throw __e)))) nil))
