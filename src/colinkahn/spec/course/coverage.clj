(ns colinkahn.spec.course.coverage
  (:require [clojure.spec.alpha :as s]
            [colinkahn.spec.course.coverage.parse :as parse]
            [clojure.set]))

(defn prewalk
  ([f form]
   (prewalk (atom 0) f form))
  ([depth f form]
   (clojure.walk/walk (partial prewalk depth f) identity (f (swap! depth inc) form))))

(declare instrument-multi)

(defn resolve-symbol [sym]
  (when-some [m (meta (resolve sym))]
    (symbol (str (:ns m)) (name (:name m)))))

(defn body [& forms])

(defn branch? [x]
  (when (and (seq? x)
             (symbol? (first x)))
    (if (= (first x) 'if)
      (first x)
      (let [sym (resolve-symbol (first x))]
        (when (contains? (methods instrument-multi) sym)
          sym)))))

(def ^:dynamic *report* nil)

(def registry-atom (atom {}))

(defn registry []
  (deref registry-atom))

(defn unregister! [id]
  (swap! registry-atom dissoc id))

(defn register! [id tag depth reason parent]
  (let [rid {:depth depth :tag tag}]
    (swap! registry-atom #(assoc-in % [id rid] (merge {:failed reason :form parent} rid)))))

(defn covered! [id tag depth]
  (assert (contains? (get (registry) id) {:depth depth :tag tag}))
  (some-> *report* (swap! update id (fnil conj #{}) {:depth depth :tag tag})))

(defn cov [id tag depth expr reason parent]
  (register! id tag depth reason parent)
  `(do (covered! '~id '~tag ~depth) ~expr))

(def instrument-multi nil)
(defmulti instrument-multi (fn [id depth form] (branch? form)))

(defmethod instrument-multi `body
  [id depth [_ & body :as form]]
  (cov id :root depth `(do ~@body) `(do ~@body) nil))

(defmethod instrument-multi 'if
  [id depth [_ test then else :as form]]
  `(if ~test
     ~(cov id :then depth then (list 'when test then) form)
     ~(cov id :else depth else (list 'when (list 'not test) else) form)))

(defmethod instrument-multi `when
  [id depth [_ test & body :as form]]
  `(if ~test
     ~(cov id :then depth `(do ~@body) (list* 'when test body) form)
     ~(cov id :else depth `(do nil) (list* 'when (list 'not test) body) form)))

(defmethod instrument-multi `fn
  [id depth [_ & args :as form]]
  (let [{:keys [fn-name bodies]} (parse/parsed-fn args)]
    `(fn ~fn-name
       ~@(mapv (fn [{:keys [params body prepost]}]
                 `(~params ~prepost ~(cov id params depth `(do ~@body) `(do ~@body) form)))
               bodies))))

(defmethod instrument-multi `case
  [id depth [_ expr & clauses :as form]]
  `(case ~expr
     ~@(let [pairs (->> clauses
                        (partition 2)
                        (mapv (fn [[a b]] [a (cov id a depth b (list 'case expr a b) form)])))
             else (when (odd? (count clauses))
                    [(cov id :else depth (last clauses) (last clauses) form)])]
         (concat (mapcat identity pairs) else))))

(defmethod instrument-multi `cond
  [id depth [_ & clauses :as form]]
  `(cond 
     ~@(let [pairs (->> clauses
                        (partition 2)
                        (mapv (fn [[a b]] [a (cov id a depth b (list 'when a b) form)])))]
         (mapcat identity pairs))))

;; support test-expr :>> result-fn
(defmethod instrument-multi `condp
  [id depth [_ pred expr & clauses :as form]]
  `(condp ~pred ~expr
     ~@(let [pairs (->> clauses
                        (partition 2)
                        (mapv (fn [[a b]] [a (cov id a depth b (list 'when (list pred a expr) b) form)])))
             else (when (odd? (count clauses))
                    [(cov id :else depth (last clauses) (last clauses) form)])]
         (concat (mapcat identity pairs) else))))

(defmethod instrument-multi `and
  [id depth [_ & xs :as form]]
  `(and
     ~@(->> xs
            (map-indexed (fn [i x] (cov id i depth x
                                        (list 'when (let [failed (take i xs)]
                                                      (condp < (count failed)
                                                        1 (list* 'and failed)
                                                        (first failed)))
                                              x)
                                        form)))
            (vec))))

(defmethod instrument-multi `or
  [id depth [_ & xs :as form]]
  `(or
     ~@(->> xs
            (map-indexed (fn [i x] (cov id i depth x
                                        (list 'when (list 'not (last (take i xs))) x)
                                        form)))
            (vec))))

; TODO: fix report for case, cond and condp
; while, loop
; if-not, if-some, if-let, when-not, when-some, when-not
; let-fn
; cond->, cond->>, some->, some->> (are these possible?)

(defmethod instrument-multi :default
  [_ _ x] x)

(defn instrument [id form]
  (prewalk (partial instrument-multi id) form))

(defmacro i [id form]
  (unregister! id)
  (instrument id `(body ~form)))

(defn report [ids]
  (some->> ids
           (keep (fn [id]
                   (let [registered (get @registry-atom id)
                         covered (get @*report* id)]
                     (when-some [missed (seq (clojure.set/difference
                                               (set (keys registered))
                                               covered))]
                       [id (->> missed
                                (mapv registered)
                                (group-by :depth)
                                (into (sorted-map-by <))
                                (first)
                                (val)
                                (mapv #(select-keys % [:failed :form])))]))))
           (seq)
           (into {})))

(defmacro with-report [ids & body]
  `(binding [*report* (atom {})]
     ~@body
     (report ~ids)))

(defmacro defnc [& defn-args]
  (let [{:keys [fn-name fn-symbol meta docstring bodies]} (parse/parsed-defn defn-args)
        _ (unregister! fn-symbol)]
    `(defn ~fn-name ~docstring ~meta
       ~@(mapv (fn [{:keys [params body prepost]}]
                 `(~params ~prepost ~(instrument fn-symbol `(body ~@body))))
               bodies))))

(comment
  (reset! registry-atom {})
  (registry)

  (defn foo []
    (i ::foo 1))

  (defn foo [x]
    (i ::foo (when (odd? x) x)))

  (macroexpand-1 '(defnc foo [x] (when (odd? x) x)))
 
  (defnc foo [x] (when (odd? x) x))

  (defnc bar [y]
    (let [z (fn [a] a)]
      (if (odd? y)
        (z y))))

  (defnc baz [x]
    (case x
      :a 1
      :b 2
      3))

  (with-report [`baz]
    (baz :a)
    (baz :b)
    (baz :c))

  (defnc waz [x]
    (cond 
      (= x :a) 1
      (= x :b) 2
      :else 3))

  (with-report [`waz]
    (waz :a)
    (waz :b)
    (waz :c))

  (defnc raz [x]
    (condp = x
      :a 1
      :b 2
      3))

  (with-report [`raz]
    (raz :a)
    (raz :b)
    #_(raz :c))

  (defnc and-test [x y]
    (and y (and x y :c)))

  (with-report [`and-test]
    (and-test false true))

  (defnc or-test [x y]
    (or y (or false x y)))

  (with-report [`or-test]
    (or-test true false))

  (get (registry) `bar)

  (macroexpand-1 '(defnc bar [y]
    (let [z (fn [a] a)]
      (if (odd? y)
        (z y)))))

  (with-report [`bar]
    (bar 1)
    #_(bar 2))

  (with-report [`foo]
    (foo 1)
    (foo 2))
  )
