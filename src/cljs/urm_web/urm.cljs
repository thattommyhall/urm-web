(ns urm-web.urm
  (:refer-clojure :exclude [inc pop])
  (:require
   [reagent.core :as reagent :refer [atom]]))

(def registers (atom {0 1
                      1 3
                      2 4
                      }))

(declare decode-pair code-pair)

(defn inc [register jump-to]
  [:inc register jump-to])

(defn deb [register jump-to branch-on-zero]
  [:deb register jump-to branch-on-zero])

(defn end []
  [:end])

(defn copy [from to goto]
  [:copy from to goto])

(defn push [from to goto]
  [:push from to goto])

(defn pop [from to goto branch]
  [:pop from to goto branch])

(defn apply-statement [[instruction & args :as statement] state]
  (case instruction
    :inc  (let [[register jump-to] args
                current (get-in state [:registers register] 0)]
            (-> state
                (assoc-in [:registers register] (+ 1 current))
                (assoc :position jump-to)))
    :deb  (let [[register jump-to branch-on-zero] args
                current (get-in state [:registers register] 0)
                branch? (== current 0)]
            (-> state
                (assoc-in [:registers register] (if branch? current (dec current)))
                (assoc :position (if branch? branch-on-zero jump-to))))
    :copy (let [[from to exit] args
                from-value (get-in state [:registers from] 0) ]
            (-> state
                (assoc-in [:registers to] from-value)
                (assoc :position exit)))
    :push (let [[from to exit] args
                from-value (get-in state [:registers from] 0)
                to-value (get-in state [:registers to] 0)]
            (-> state
                (assoc-in [:registers to] (code-pair from-value to-value))
                (assoc-in [:registers from] 0)
                (assoc :position exit)))
    :pop  (let [[from to halt exit] args
                from-value (get-in state [:registers from] 0)
                exit? (== 0 from-value)]
            (if exit?
              (assoc state :position exit)
              (let [[h t] (decode-pair from-value)]
                (-> state
                    (assoc-in [:registers from] t)
                    (assoc-in [:registers to] h)
                    (assoc :position halt)))))
    :end state))

(defn next-state [{:keys [position program] :as state}]
  (let [next-statement (nth program position)]
    (apply-statement next-statement state)))

(defn zero [register]
  [(deb register 0 1)
   (end)])

(defn divides? [n div]
  (== 0 (rem n div)))

(defn factors-of-2
  ([n] (factors-of-2 n 0))
  ([n so-far]
   (if (divides? n 2)
     (recur (/ n 2)
            (+ 1 so-far))
     so-far)))

(defn expt [x y]
  (.pow js/Math x y))

(defn code-pair [x y]
  (* (expt 2 x)
     (+ (* 2 y) 1)))

(defn decode-pair [n]
  (let [x (factors-of-2 n)
        y (/ (dec (/ n (expt 2 x)))
             2)]
    [x y]))

(defn code-list [[h & t :as number-list]]
  (if (empty? number-list)
    0
    (code-pair h (code-list t))))

(defn decode-list [code]
  (if (== code 0)
    '()
    (let [[h code'] (decode-pair code)]
      (cons h
            (decode-list code')))))

(defn code-instruction [[instruction register jump-to branch-on-zero]]
  (case instruction
    :inc (code-pair (* 2 register) jump-to)
    :deb (code-pair (+ (* 2 register) 1)
               (code-pair jump-to branch-on-zero))
    :end 0))

(defn decode-instruction [code]
  (if (== code 0)
    (end)
    (let [[y z] (decode-pair code)]
      (if (even? y)
        (inc (/ y 2) z)
        (let [[j k] (decode-pair z)]
          (deb (/ (dec y) 2)
               j
               k))))))

(defn code-program [instructions]
  (code-list (map code-instruction instructions)))

(defn decode-program [code]
  (map decode-instruction (decode-list code)))

(def program 1)
;; (def registers 2)
(def position 3)
(def current-instruction 4)
(def current-instruction-type 5)
(def current-register 6)
(def s 7)
(def t 8)
(def z 9)

(def uurm
  [(copy program t 1)
   (pop t current-instruction 2 14)
   (deb position 1 3)
   (pop current-instruction current-instruction-type 4 14)
   (pop registers current-register 5 5)
   (deb current-instruction-type 6 8)
   (deb current-instruction-type 7 10)
   (push current-register s 4)
   (inc current-register 9)
   (copy current-instruction position 11)
   (pop current-instruction position 12 12)
   (push current-register registers 13)
   (deb current-register 11 9)
   (pop s current-register 11 0)
   (pop registers 0 15 15)
   (end)])

(def add [(deb 2 1 2)
          (inc 0 0)
          (deb 1 3 4)
          (inc 0 2)
          (end)])

(def args {0 0
           1 7
           2 3})

(def current-state (atom {:program add
                          :position 0
                          :registers args}))

(js/setInterval #(swap! current-state next-state) 1000)
