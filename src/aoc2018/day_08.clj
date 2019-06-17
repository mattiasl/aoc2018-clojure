(ns aoc2018.day-08
  (:require [ysera.collections :refer [seq-contains?]]
            [ysera.test :refer [is= is is-not deftest error?]]
            [clojure.string :as string]))

(defn get-puzzle-input []
  (as-> (slurp "src/aoc2018/day_08.txt") $
        (string/replace $ #"\n" "")
        (string/split $ #" ")
        (map read-string $)))


(declare get-node)


(defn get-children [number-of-children numbers]
  (reduce (fn [[children numbers] _]
            (let [[node numbers] (get-node numbers)]
              [(conj children node) numbers]))
          [[] numbers]
          (range number-of-children)))


(defn get-metadata [metadata numbers]
  [(take metadata numbers) (drop metadata numbers)])


(defn get-node
  {:test (fn []
           (is= (get-node [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])
                [{:children [{:children []
                              :metadata [10 11 12]}
                             {:children [{:children []
                                          :metadata [99]}]
                              :metadata [2]}]
                  :metadata [1 1 2]}
                 []]))}
  [numbers]
  (let [[number-of-children metadata & numbers] numbers
        [children numbers] (get-children number-of-children numbers)
        [metadata numbers] (get-metadata metadata numbers)]
    [{:children children
      :metadata metadata}
     numbers]))


(defn sum-metadata-part-1
  {:test (fn []
           (is= (-> (get-node [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])
                    (first)
                    (sum-metadata-part-1))
                138))}
  [node]
  (apply + (concat (:metadata node)
                   (map sum-metadata-part-1 (:children node)))))


(deftest puzzle-part-1
         (is= (->> (get-puzzle-input)
                   (get-node)
                   (first)
                   (sum-metadata-part-1))
              35852))


(defn sum-metadata-part-2
  {:test (fn []
           (is= (-> (get-node [1 1 0 1 99 2])
                    (first)
                    (sum-metadata-part-2))
                0)
           (is= (-> (get-node [0 3 10 11 12])
                    (first)
                    (sum-metadata-part-2))
                33)
           (is= (-> (get-node [0 1 99])
                    (first)
                    (sum-metadata-part-2))
                99)
           (is= (-> (get-node [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])
                    (first)
                    (sum-metadata-part-2))
                66))}
  [node]
  (if (empty? (:children node))
    (apply + (:metadata node))
    (let [children-sums (map sum-metadata-part-2 (:children node))]
      (apply + (->> (:metadata node)
                    (map (fn [index]
                           (when (< index (count children-sums))
                             (nth children-sums (dec index)))))
                    (remove nil?))))))


(deftest puzzle-part-1
         (is= (->> (get-puzzle-input)
                   (get-node)
                   (first)
                   (sum-metadata-part-2))
              0))
