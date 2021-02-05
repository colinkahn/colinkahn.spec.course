(ns colinkahn.spec.course.coverage
  (:require [clojure.spec.alpha :as s]
            [clojure.set]))

(declare instrument-multi)

(defn resolve-symbol [sym]
  (when-some [m (meta (resolve sym))]
    (symbol (str (:ns m)) (name (:name m)))))

(defn branch? [x]
  (when (and (seq? x)
             (symbol? (first x)))
    (if (= (first x) 'if)
      (first x)
      (let [sym (resolve-symbol (first x))]
        (when (contains? (methods instrument-multi) sym)
          sym)))))

(def ^:dynamic *instrument-ctx* nil)
(def ^:dynamic *coverage-ctx* nil)

(def registry-atom (atom {}))

(defn registry []
  (deref registry-atom))

(defn unregister! [id]
  (swap! registry-atom dissoc id))

(defn register! [id i exprs]
  (swap! registry-atom
         (fn [m]
           (-> m
               (assoc-in [id :exprs i] exprs)
               (update-in [id :indexes] (fnil conj #{}) i)))))

(defn covered! [id i]
  (some-> *coverage-ctx* (swap! update-in [id i] (fnil inc 0))))

(defn cov [& exprs]
  (let [id (:id *instrument-ctx*)
        i (swap! (:index *instrument-ctx*) inc)
        _ (register! id i exprs)]
    `(do (covered! '~id ~i) ~@exprs)))

(defmulti instrument-multi branch?)

(defmethod instrument-multi 'if
  [[_ test & exprs]]
  `(if ~test
     ~@(mapv (fn [expr] (cov expr)) exprs)))

(defmethod instrument-multi `when
  [[_ test & body]]
  `(when ~test
     ~(apply cov body)))

(defmethod instrument-multi :default
  [x] x)

(defn instrument [xs ctx]
  (binding [*instrument-ctx* (merge ctx {:index (atom 0)})]
    (clojure.walk/prewalk instrument-multi `(when true ~@xs))))

(defmacro i [id & body]
  (unregister! id)
  (instrument body {:id id}))

(defn report [ids]
  (some->> ids
           (keep (fn [id]
                   (let [{:keys [indexes exprs]} (get @registry-atom id)
                         cov-map (get @*coverage-ctx* id)]
                     (when-some [uncovered (seq (clojure.set/difference
                                                  indexes
                                                  (set (keys cov-map))))]
                       [id (mapv #(exprs %) uncovered)]))))
           (seq)
           (into {})))

(defmacro with-report [ids & body]
  `(binding [*coverage-ctx* (atom {})]
     ~@body
     (report ~ids)))

(defn parsed-defn
  [defn-args]
  (let [parsed (s/conform :clojure.core.specs.alpha/defn-args defn-args)
        {[arity-tag fn-tail] :fn-tail :keys [fn-name meta docstring]} parsed
        bodies (case arity-tag
                 :arity-n (:bodies fn-tail)
                 :arity-1 (vector fn-tail))
        bodies (map (fn [{:keys [params] [body-tag body] :body}]
                      (let [params (vec (s/unform :clojure.core.specs.alpha/param-list params))]
                        (case body-tag
                          :body {:params params :body body :prepost {}}
                          :prepost+body (merge {:params params} body))))
                    bodies)]
    {:fn-name fn-name
     :fn-symbol (symbol (str *ns*) (name fn-name))
     :meta (merge meta {})
     :docstring (str "" docstring)
     :bodies bodies}))

(defmacro defnc [& defn-args]
  (let [{:keys [fn-name fn-symbol meta docstring bodies]} (parsed-defn defn-args)
        _ (unregister! fn-symbol)]
    `(defn ~fn-name ~docstring ~meta
       ~@(mapv (fn [{:keys [params body prepost]}]
                 `(~params ~prepost ~(instrument body {:id fn-symbol})))
               bodies))))
