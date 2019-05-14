(ns aoc2018.day-07
  (:require [ysera.test :refer [is= is is-not deftest]]
            [clojure.string :as string]))

(defn get-puzzle-input []
  (->> (slurp "src/aoc2018/day_07.txt")
       (string/split-lines)))

(def test-data ["Step C must be finished before step A can begin."
                "Step C must be finished before step F can begin."
                "Step A must be finished before step B can begin."
                "Step A must be finished before step D can begin."
                "Step B must be finished before step E can begin."
                "Step D must be finished before step E can begin."
                "Step F must be finished before step E can begin."])

(defn string-data->map-data
  {:test (fn []
           (is= (string-data->map-data "Step C must be finished before step A can begin.")
                {:prerequisite "C" :task "A"}))}
  [string-data]
  (let [pattern (re-pattern "Step (\\w) must be finished before step (\\w) can begin.")]
    (let [[_ p t] (re-find pattern string-data)]
      {:prerequisite p :task t})))

(defn get-prerequisites
  {:test (fn []
           (is= (get-prerequisites test-data "E")
                ["B" "D" "F"])
           (is= (get-prerequisites test-data "A")
                ["C"])
           (is= (get-prerequisites test-data "C")
                []))}
  [data task]
  (->> data
       (map string-data->map-data)
       (filter (fn [{p :prerequisite t :task}]
                 (= t task)))
       (map (fn [{p :prerequisite}] p))))

(defn create-state
  {:test (fn []
           (is= (create-state test-data)
                {:done-tasks    []
                 :undone-tasks   ["A" "B" "C" "D" "E" "F"]
                 :prerequisites {"A" ["C"]
                                 "B" ["A"]
                                 "D" ["A"]
                                 "E" ["B" "D" "F"]
                                 "F" ["C"]}}))}
  [data]
  {:done-tasks    []
   :undone-tasks  (->> data
                       (map string-data->map-data)
                       (reduce (fn [a {p :prerequisite t :task}]
                                 (conj a p t))
                               #{})
                       (into [])
                       (sort))
   :prerequisites (->> data
                       (map string-data->map-data)
                       (reduce (fn [a {p :prerequisite t :task}]
                                 (if (contains? a t)
                                   (update a t conj p)
                                   (assoc a t [p])))
                               {}))})

