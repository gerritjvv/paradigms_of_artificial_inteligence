(ns
  ^{:doc "Implement first version of 4.1 of GPS
          This contains my first go at it without getting too complicated.

          Deviations: there's allot from the 4.1 impl.

          I've decided to return a map that associates goals with an operation or nil
          A map satisfies the goals if the operation is nil in the map


            I've included the use of prismatic schema as part of good design, and instead
            of creating records up front just validate that the structure added as an operation complies with what
            is expected.
         "}
  ch4.gps-v1
  (:require [schema.core :as s]
            [clojure.set :as clj-set]))

;;;;;;;;;;;;;;;;
;;;;;;;; Schemas and records


(def OPSchema {:preconds #{s/Any} :add-list #{s/Any} :remove-list #{s/Any} :action s/Str
               ;;include any any so that the map can contain any other keys, but we require it to have at least
               ;;the previous three
               s/Any s/Any})


;; TODO Rewrite as a Graph solution

;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Private functions

;(defn appropriate? [op goal]
;  ((:add-list op) goal))
;
;(defn apply-op-state [state {:keys [add-list remove-list]}]
;  (clj-set/union (clj-set/difference state remove-list) add-list))
;
;
;(defn apply-ops-state [state ops]
;  (reduce apply-op-state state ops))
;
;(defn find-appropriate [ops goal]
;  (filter #(appropriate? % goal) ops))
;
;(defn apply? [state ops {:keys [preconds add-list remove-list action] :as op}]
;  (cond
;    (clj-set/subset? add-list state)                          [:OK [op]]
;    (or (empty? preconds) (clj-set/subset? preconds state))   [:OK [op]]
;    :else
;
;    ;;precond-ops [ [precond applyable-ops] ....] if a precond was not met it would not appear in this sequence
;    ;;the fastest way to check is to create a set and the compare against the preconds using
;    (let [precond-ops (for [precond-goal preconds
;                            precond-op (find-appropriate ops precond-goal)
;                            :let [[status app-op :as v] (apply? state ops precond-op)]
;                            :when (= status :OK)]
;                        [precond-goal app-op])]
;
;      (let [app-ops (mapcat second precond-ops)
;            app-add-list (set (mapcat :add-list app-ops)) ]
;        (prn "app-ops " "  " preconds " >> " (set (mapcat :add-list app-ops)) " == " (or (empty? (clj-set/difference preconds add-list))
;                                                                                         (clj-set/subset? preconds app-add-list)))
;
;        [(if (or (empty? (clj-set/difference preconds add-list))
;                 (clj-set/subset? preconds app-add-list)) :OK :FAIL)
;         (conj app-ops op)]))))

;;;;;;;;;;;;;;;;;;
;;;;;;;; Public API

(defn make-op [action preconds add-list remove-list]
  {:action action :preconds (set preconds) :add-list (set add-list) :remove-list (set remove-list)})


;(defn gps
;  "Search for operations that can meet the goals
;   Returns [status [action1, action2 ...] status can be :OK, NOOP or NIL"
;  [state ops goals]
;  {:pre [(set? state) (map #(s/validate OPSchema %) ops) (set? goals)]}
;  (if (clj-set/subset? goals state)
;    [:NOOP nil]
;    (let [[status app-ops] (mapcat #(apply? state ops %) (find-ops ops goals))]
;      [(if (empty? app-ops) :NOOP :OK) (map :action app-ops)])))

