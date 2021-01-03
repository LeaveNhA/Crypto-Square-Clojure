(ns crypto-square
  (:require [clojure.string]))

(def ^{:private true}
  white-list-chars
  (apply hash-set (concat
                   (range 65 91)
                   (range 97 123)
                   (range 49 58))))

(defn ^{:private true}
  filter-with-white-list [white-list text]
  (filter (comp white-list int) text))

(def ^{:private true} alphanumeric-filter (partial filter-with-white-list white-list-chars))

(defn normalize-plaintext [plain-text]
  (->> plain-text
       alphanumeric-filter
       vec
       (apply str)
       clojure.string/lower-case))

(defn ^{:private true}
  calculate-square-dim [plain-text]
  (let [text-count (count plain-text)]
    (first (reverse
            (for [c (range 1 text-count)
                  r (range text-count)
                  :when (and (>= c r)
                             (<= (- c r) 1)
                             (> r
                                (- (* c r)
                                   text-count)))]
              [c r])))))

(defn square-size [plain-text]
  (first (calculate-square-dim plain-text)))

(defn ^{:private true}
  fill-with-space [pred-n coll]
  (let [diff-n (- pred-n
                  (count coll))]
    (if (pos? diff-n)
      (concat coll (take diff-n (repeat \space)))
      coll)))

(defn ^{:private true}
  plaintext->segments [plain-text]
  (let [normalized-text (normalize-plaintext plain-text)
        [square-size-of-text row] (calculate-square-dim normalized-text)]
    (map (comp #(apply str %) vec (partial fill-with-space square-size-of-text)) (partition-all square-size-of-text normalized-text))))

(defn plaintext-segments [plain-text]
  (map (comp #(apply str %) vec (partial filter (complement #{\space}))) (plaintext->segments plain-text)))

(defn ciphertext [plain-text]
  (->> plain-text
       plaintext->segments
       (apply map str)
       (map (comp #(apply str %) #(filter (complement #{\space}) %)))
       (apply str)))

(defn normalize-ciphertext [cipher-text]
  (->> cipher-text
       plaintext->segments
       (apply map str)
       (map (comp #(apply str %) #(filter (complement #{\space}) %)))
       (clojure.string/join " ")))
