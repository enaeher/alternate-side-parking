(ns asp
  (:require [clojure.math.combinatorics :as combo])
  (:import
   (java.time
    DayOfWeek
    LocalDate)
   (java.time.temporal
    ChronoUnit)))

;; Data

(def year 2022)

(def asp-holidays-2022
  (into
   #{}
   (map (fn [[month day]] (LocalDate/of year month day))
        [[1 1]                 ; New Year's Day
         [1 6]                 ; Three Kings' Day
         [1 17]                ; Martin Luther King, Jr.'s Birthday
         [1 31]                ; Lunar New Year's Eve
         [2 1]                 ; Lunar New Year
         [2 12]                ; Lincoln's Birthday
         [2 21]                ; Washington's Birthday/Presidents Day
         [3 3]                 ; Ash Wednesday
         [3 17]                ; Purim
         [4 14]                ; Holy Thursday
         [4 15]                ; Good Friday
         [4 16]                ; Passover
         [4 21]                ; Holy Thursday (Orthodox)
         [4 22]                ; Passover, Good Friday (Orthodox)
         [4 23]                ; Passover
         [5 2]                 ; Idul-Fitr (Eid Al-Fitr)
         [5 3]                 ; Idul-Fitr (Eid Al-Fitr)
         [5 4]                 ; Idul-Fitr (Eid Al-Fitr)
         [5 26]                ; Solemnity of the Ascension
         [5 30]                ; Memorial Day
         [6 06]                ; Shavuot
         [6 20]                ; Juneteenth (observed)
         [7 4]                 ; Independence Day
         [7 9]                 ; Idul-Adha (Eid Al-Adha)
         [7 10]                ; Idul-Adha (Eid Al-Adha)
         [7 11]                ; Idul-Adha (Eid Al-Adha)
         [8 15]                ; Feast of the Assumption
         [9 5]                 ; Labor Day
         [9 26]                ; Rosh Hashanah
         [9 27]                ; Rosh Hashanah
         [10 5]                ; Yom Kippur
         [10 10]               ; Columbus Day/Succoth
         [10 11]               ; Succoth
         [10 17]               ; Shemini Atzeroth
         [10 18]               ; Simchas Torah
         [10 24]               ; Diwali
         [11 1]                ; All Saints' Day
         [11 8]                ; Election Day
         [11 11]               ; Veterans Day
         [11 24]               ; Thanksgiving Day
         [12 8]                ; Immaculate Conception
         [12 26]]              ; Christmas Day (observed)
        )))

(def positions
  {:m-th #{DayOfWeek/MONDAY DayOfWeek/THURSDAY}
   :tu-f #{DayOfWeek/TUESDAY DayOfWeek/FRIDAY}})

(defn next-day [date]
  (.plus date 1 ChronoUnit/DAYS))

(def days
  (loop [date (LocalDate/of year 01 01)
         dates []]
    (if (= year (.getYear date))
      (recur (next-day date)
             (conj dates date))
      dates)))

;; Helpers

(defn must-move? [date pos]
  (and (not (contains? asp-holidays-2022 date))
       (contains? (get positions pos) (.getDayOfWeek date))))

(defn days-until-move [date pos]
  (loop [i 0
         date date]
    (if (must-move? date pos)
      i
      (recur (inc i) (next-day date)))))

;; move strategy functions

(defn never-move
  "Always stay in the same position."
  [_date pos]
  pos)

(defn alternate
  "Each time you must move the car, switch to the other side of the street."
  [_date pos]
  (if (= :m-th pos)
    :tu-f
    :m-th))

(defn random
  "Each time you must move the car, randomly choose a side of the street."
  [_date _pos]
  (rand-nth (keys positions)))

(defn lookahead
  "Each time you must move the car, choose the side of the street where
  you have the greatest number of days until you must move it again."
  [date _pos]
  (apply max-key #(days-until-move (next-day date) %) (keys positions)))

;; move finder

(defn find-moves [start-date end-date start-pos move-strategy-fn]
  (loop [date start-date
         pos start-pos
         moves []]
    (cond (= date end-date)
          moves

          (must-move? date pos)
          (let [new-pos (move-strategy-fn date pos)]
            (recur (next-day date) new-pos (conj moves [date new-pos])))

          :else
          (recur (next-day date) pos moves))))

;; report

(defn report [start-date end-date]
  (let [format-str "%-20s | %-20s | %-3s"]
    (println (format format-str "Move strategy" "Initial position" "Number of moves"))
    (run! (fn [[start-pos move-strategy-fn]]
            (println (format
                      format-str
                      (-> move-strategy-fn meta :name)
                      start-pos
                      (count (find-moves start-date end-date start-pos move-strategy-fn)))))
          (combo/cartesian-product
           (keys positions)
           [#'never-move #'alternate #'random #'lookahead]))))
