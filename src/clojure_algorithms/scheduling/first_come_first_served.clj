(ns clojure-algorithms.scheduling.first-come-first-served
  "First Come First Served (FCFS) scheduling calculations.

  Port of TheAlgorithms/Python scheduling/first_come_first_served.py.
  Processes run in arrival order; these helpers derive waiting times,
  turnaround times, and their averages from burst durations.")

(defn calculate-waiting-times
  "Return a vector with the waiting time of each process: process 0 waits 0,
  every other process waits for all previous durations to finish."
  [duration-times]
  (if (empty? duration-times)
    []
    (vec (reductions + 0 (butlast duration-times)))))

(defn calculate-turnaround-times
  "Return a vector of waiting-time + duration-time per process."
  [duration-times waiting-times]
  (mapv + duration-times waiting-times))

(defn calculate-average-turnaround-time
  "Return the mean of `turnaround-times` as a double."
  [turnaround-times]
  (double (/ (reduce + turnaround-times) (count turnaround-times))))

(defn calculate-average-waiting-time
  "Return the mean of `waiting-times` as a double."
  [waiting-times]
  (double (/ (reduce + waiting-times) (count waiting-times))))
