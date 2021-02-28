(ns colinkahn.spec.course.coverage.parse
  (:require [clojure.spec.alpha :as s]))

(defn- normalized-bodies [parsed]
  (let [{[arity-tag fn-tail] :fn-tail} parsed]
    (map (fn [{:keys [params] [body-tag body] :body}]
           (let [params (vec (s/unform :clojure.core.specs.alpha/param-list params))]
             (case body-tag
               :body {:params params :body body :prepost {}}
               :prepost+body (merge {:params params} body))))
         (if (vector? fn-tail)
           fn-tail
           (case arity-tag
             :arity-n (:bodies fn-tail)
             :arity-1 (vector fn-tail))))))

(defn parsed-fn
  [fn-args]
  (let [parsed (s/conform (:args ((s/registry) `fn)) fn-args)
        _ (println parsed)
        {:keys [fn-name] :or {fn-name (gensym)}} parsed
        bodies (normalized-bodies parsed)]
    {:fn-name fn-name
     :bodies bodies}))

(defn parsed-defn
  [defn-args]
  (let [parsed (s/conform :clojure.core.specs.alpha/defn-args defn-args)
        {:keys [fn-name meta docstring]} parsed
        bodies (normalized-bodies parsed)]
    {:fn-name fn-name
     :fn-symbol (symbol (str *ns*) (name fn-name))
     :meta (merge meta {})
     :docstring (str "" docstring)
     :bodies bodies}))
