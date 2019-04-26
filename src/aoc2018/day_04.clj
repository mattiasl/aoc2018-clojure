(ns aoc2018.day-04
  (:require [ysera.test :refer [is= is is-not deftest]]))


(defn get-puzzle-input []
  (->> (slurp "src/aoc2018/day_04.txt")
       (clojure.string/split-lines)))

(defn sort-logs
  {:test (fn []
           (is= (sort-logs ["[1518-07-01 00:47] wakes up"
                            "[1518-10-17 00:13] falls asleep"
                            "[1518-10-12 00:31] wakes up"
                            "[1518-09-24 00:03] Guard #2137 begins shift"
                            "[1518-10-11 00:35] wakes up"
                            "[1518-07-10 00:59] wakes up"])
                ["[1518-07-01 00:47] wakes up"
                 "[1518-07-10 00:59] wakes up"
                 "[1518-09-24 00:03] Guard #2137 begins shift"
                 "[1518-10-11 00:35] wakes up"
                 "[1518-10-12 00:31] wakes up"
                 "[1518-10-17 00:13] falls asleep"]))}
  [logs]
  (sort logs))

(comment (->> (get-puzzle-input)
              (sort-logs)))

(defn guard-begins-shift?
  {:test (fn []
           (is (guard-begins-shift? "[1518-11-01 00:00] Guard #10 begins shift"))
           (is-not (guard-begins-shift? "[1518-11-01 00:05] falls asleep"))
           (is-not (guard-begins-shift? "[1518-11-01 00:25] wakes up")))}
  [log]
  (clojure.string/index-of log "begins shift"))


(defn guard-falls-asleep?
  {:test (fn []
           (is-not (guard-falls-asleep? "[1518-11-01 00:00] Guard #10 begins shift"))
           (is (guard-falls-asleep? "[1518-11-01 00:05] falls asleep"))
           (is-not (guard-falls-asleep? "[1518-11-01 00:25] wakes up")))}
  [log]
  (clojure.string/index-of log "falls asleep"))


(defn get-guard
  {:test (fn []
           (is= (get-guard "[1518-11-01 00:00] Guard #10 begins shift")
                "#10"))}
  [log]
  (re-find (re-pattern "#[0-9]+") log))

(defn get-time-stamp
  {:test (fn []
           (is= (get-time-stamp "[1518-11-01 00:00] Guard #10 begins shift")
                "1518-11-01 00:00"))}
  [log]
  (re-find (re-pattern "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}") log))

(defn logs->data
  {:test (fn []
           (is= (logs->data ["[1518-11-01 00:00] Guard #10 begins shift"
                             "[1518-11-01 00:05] falls asleep"
                             "[1518-11-01 00:25] wakes up"
                             "[1518-11-01 00:30] falls asleep"
                             "[1518-11-01 00:55] wakes up"
                             "[1518-11-01 23:58] Guard #99 begins shift"
                             "[1518-11-02 00:40] falls asleep"
                             "[1518-11-02 00:50] wakes up"
                             "[1518-11-03 00:05] Guard #10 begins shift"
                             "[1518-11-03 00:24] falls asleep"
                             "[1518-11-03 00:29] wakes up"
                             "[1518-11-04 00:02] Guard #99 begins shift"
                             "[1518-11-04 00:36] falls asleep"
                             "[1518-11-04 00:46] wakes up"
                             "[1518-11-05 00:03] Guard #99 begins shift"
                             "[1518-11-05 00:45] falls asleep"
                             "[1518-11-05 00:55] wakes up"])
                {"#10" {"1518-11-01 00:00" [["1518-11-01 00:05" "1518-11-01 00:25"]
                                            ["1518-11-01 00:30" "1518-11-01 00:55"]]
                        "1518-11-03 00:05" [["1518-11-03 00:24" "1518-11-03 00:29"]]}
                 "#99" {"1518-11-01 23:58" [["1518-11-02 00:40" "1518-11-02 00:50"]]
                        "1518-11-04 00:02" [["1518-11-04 00:36" "1518-11-04 00:46"]]
                        "1518-11-05 00:03" [["1518-11-05 00:45" "1518-11-05 00:55"]]}}))}
  [logs]
  (-> (reduce (fn [{data :data guard :guard shift :shift asleep :asleep} log]
                (let [time-stamp (get-time-stamp log)]
                  (cond (guard-begins-shift? log)
                        (let [guard (get-guard log)]
                          {:data  (-> (if (contains? data guard)
                                        data
                                        (assoc data guard {}))
                                      (assoc-in [guard time-stamp] []))
                           :guard guard
                           :shift time-stamp})

                        (guard-falls-asleep? log)
                        {:data   data
                         :guard  guard
                         :shift  shift
                         :asleep time-stamp}

                        :else
                        {:data  (update-in data
                                           [guard shift]
                                           (fn [value]
                                             (conj value [asleep time-stamp])))
                         :guard guard
                         :shift shift})))
              {}
              (sort-logs logs))
      (:data)))

(defn get-interval-time
  {:test (fn []
           (is= (get-interval-time ["1518-11-01 00:05" "1518-11-01 00:25"])
                20))}
  [interval]
  (as-> interval $
        (map (fn [x] (.getTime (.parse (java.text.SimpleDateFormat. "yyyy-mm-dd hh:mm") x))) $)
        (apply - $)
        (/ $ (* 1000 60))
        (- $)))

(defn get-minutes-asleep
  {:test (fn []
           (is= (get-minutes-asleep {"1518-11-01 00:00" [["1518-11-01 00:05" "1518-11-01 00:25"]
                                                         ["1518-11-01 00:30" "1518-11-01 00:55"]]
                                     "1518-11-03 00:05" [["1518-11-03 00:24" "1518-11-03 00:29"]]})
                50))}
  [data]
  (->> (vals data)
       (flatten)
       (partition 2)
       (map get-interval-time)
       (apply +)))

(defn find-guard-with-most-minutes-asleep
  {:test (fn []
           (is= (find-guard-with-most-minutes-asleep {"#10" {"1518-11-01 00:00" [["1518-11-01 00:05" "1518-11-01 00:25"]
                                                                                 ["1518-11-01 00:30" "1518-11-01 00:55"]]
                                                             "1518-11-03 00:05" [["1518-11-03 00:24" "1518-11-03 00:29"]]}
                                                      "#99" {"1518-11-01 23:58" [["1518-11-02 00:40" "1518-11-02 00:50"]]
                                                             "1518-11-04 00:02" [["1518-11-04 00:36" "1518-11-04 00:46"]]
                                                             "1518-11-05 00:03" [["1518-11-05 00:45" "1518-11-05 00:55"]]}})
                "#10"))}
  [data]
  (-> (reduce-kv (fn [leader k v]
                   (let [minutes-asleep (get-minutes-asleep v)]
                     (if (> minutes-asleep (:minutes-asleep leader))
                       {:guard          k
                        :minutes-asleep minutes-asleep}
                       leader)))
                 {:guard          nil
                  :minutes-asleep 0}
                 data)
      (:guard)))


(deftest puzzle-part-1
         (is= (->> (get-puzzle-input)
                   (logs->data)
                   (find-guard-with-most-minutes-asleep))
              "#1777"))

(deftest puzzle-part-2
         nil)