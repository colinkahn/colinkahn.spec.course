(ns colinkahn.spec.course
  (:require [clojure.spec.alpha :as s :refer [Specize Spec]]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]))

(defprotocol ISampled
  (sampled* [spec] "returns the internal atom of sampled values for spec"))

(defprotocol IConstrain
  (constrain* [spec constraint] "updates spec constraining it by contraint"))

(defn- specize-constraint [x]
  (s/specize* (or x any?)))

(defn track-spec-impl [form]
  (let [sampled (atom #{})
        constraint (atom nil)]
    (reify
      Specize
      (specize* [s] s)
      (specize* [s _] s)

      ISampled
      (sampled* [_] sampled)

      IConstrain
      (constrain* [_ spec]
        (reset! constraint spec))

      Spec
      (conform* [_ x]
        (swap! sampled conj x)
        (s/conform* (specize-constraint @constraint) x))
      (unform* [_ x]
        (s/unform* (specize-constraint @constraint) x))
      (explain* [_ path via in x]
        (s/explain* (specize-constraint @constraint) path via in x))
      (gen* [_ _ _ _]
        (if-some [xs (seq @sampled)]
          (gen/elements xs)
          (throw (IllegalStateException. "no samples collected"))))
      (with-gen* [_ _] nil)
      (describe* [_] form))))

(defmacro track-spec []
  `(track-spec-impl '(track-spec)))

(defmacro track [sym]
  `(-> (s/def ~sym (s/fspec :args (track-spec) :fn (track-spec) :ret (track-spec)))
       (st/instrument)))

(defmacro untrack [sym]
  `(-> (s/def ~sym nil)
       (st/unstrument)))

(defn samples [sym k]
  (if-some [xs (some-> (s/get-spec sym) k sampled* deref)]
    xs
    (throw (IllegalStateException. "sym is not tracked"))))

(defn vary-samples [sym k f & args]
  (if-some [samp (some-> (s/get-spec sym) k sampled*)]
    (apply swap! samp f args)
    (throw (IllegalStateException. "sym is not tracked"))))

(defn reset-samples [sym & ks]
  (doseq [k ks]
    (vary-samples sym k (constantly #{}))))

(defn explain-sampled-data [sym k spec]
  (some #(s/explain-data spec %) (samples sym k)))

(defn add-constraint [sym k spec]
  (constrain* (some-> (s/get-spec sym) k) spec))

(defn remove-constraint [sym k]
  (add-constraint sym k nil))
