(ns aoc2018.day-07
  (:require [ysera.collections :refer [seq-contains?]]
            [ysera.test :refer [is= is is-not deftest error?]]
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
                {:prerequisite "C"
                 :task         "A"}))}
  [string-data]
  {:post [(not (nil? (:task %))) (not (nil? (:prerequisite %)))]}
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
                {:time          0
                 :done-tasks    []
                 :undone-tasks  ["A" "B" "C" "D" "E" "F"]
                 :prerequisites {"A" ["C"]
                                 "B" ["A"]
                                 "D" ["A"]
                                 "E" ["B" "D" "F"]
                                 "F" ["C"]}}))}
  [data]
  {:time          0
   :done-tasks    []
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


(defn do-one-task
  {:test (fn []
           (is= (-> (create-state test-data)
                    (do-one-task))
                {:time          0
                 :done-tasks    ["C"]
                 :undone-tasks  ["A" "B" "D" "E" "F"]
                 :prerequisites {"B" ["A"]
                                 "D" ["A"]
                                 "E" ["B" "D" "F"]}}))}
  [state]
  (loop [[undone-task & rest-of-undone-tasks] (:undone-tasks state)]
    (if (contains? (:prerequisites state) undone-task)
      (recur rest-of-undone-tasks)
      (-> state
          (update :done-tasks conj undone-task)
          (update :undone-tasks (fn [undone-tasks]
                                  (remove (fn [u-t]
                                            (= u-t undone-task))
                                          undone-tasks)))
          (update :prerequisites (fn [prerequisites]
                                   (reduce-kv (fn [a k v]
                                                (if-not (seq-contains? v undone-task)
                                                  a
                                                  (if (= (count (get a k)) 1)
                                                    (dissoc a k)
                                                    (update a k (fn [values]
                                                                  (remove (fn [v]
                                                                            (= v undone-task))
                                                                          values))))))
                                              prerequisites
                                              prerequisites)))))))

(defn do-all-tasks
  {:test (fn []
           (is= (-> (create-state test-data)
                    (do-all-tasks))
                {:time          0
                 :done-tasks    ["C" "A" "B" "D" "F" "E"]
                 :undone-tasks  []
                 :prerequisites {}}))}
  [state]
  (let [state (do-one-task state)]
    (if (empty? (:undone-tasks state))
      state
      (recur state))))

(deftest puzzle-part-1
         (is= (->> (get-puzzle-input)
                   (create-state)
                   (do-all-tasks)
                   (:done-tasks)
                   (string/join ""))
              "IJLFUVDACEHGRZPNKQWSBTMXOY"))


(defn available-tasks
  {:test (fn []
           (is= (-> (create-state test-data)
                    (do-one-task)
                    (available-tasks))
                ["A" "F"]))}
  [state]
  (->> (clojure.set/difference (set (:undone-tasks state))
                               (set (keys (:prerequisites state))))
       (into [])
       (sort)))

(defn get-task-time
  {:test (fn []
           (is= (get-task-time "A" 0) 1)
           (is= (get-task-time "Z" 0) 26)
           (is= (get-task-time "A" 60) 61)
           (is= (get-task-time "Z" 60) 86))}
  [task delay]
  {:pre [(re-matches (re-pattern "[A-Z]") task)]}
  (+ delay (- (int (first task)) 64)))

(defn do-all-tasks-simultaneously
  {:test (fn []
           (is= (-> (create-state test-data)
                    (do-all-tasks-simultaneously 2)
                    (:done-tasks))
                ["C" "A" "B" "F" "D" "E"]))}
  [state number-of-workers]
  (loop [state state
         workers (reduce (fn [a v] (assoc a v nil))
                         {}
                         (range number-of-workers))]
    (let [tasks (available-tasks state)]
      (reduce (fn [a v] (cond (nil? (get-in a [:workers v]))
                              (if-let [first-task (first (:tasks a))]
                                (-> a
                                    (assoc-in [:workers v] {:task      first-task
                                                            :time-left (get-task-time first-task 0)})
                                    (update :tasks rest))
                                a)

                              (= 1 (get-in a [:workers v :time-left]))
                              (let [a (update a :done-tasks conj (get-in a [:workers v :task]))]
                                (if-let [first-task (first (:tasks a))]
                                  (-> a
                                      (assoc-in [:workers v] {:task      first-task
                                                              :time-left (get-task-time first-task 0)})
                                      (update :tasks rest))
                                  (assoc-in a [:workers v] nil)))

                              :else
                              (update-in a [:workers v :time-left] dec)))
              {:workers    workers
               :tasks      tasks
               :done-tasks (:done-tasks state)}
              (keys workers)
              ))
    ))









