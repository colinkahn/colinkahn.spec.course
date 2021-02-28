(ns colinkahn.spec.course.coverage-test
  (:require [clojure.test :refer :all]
            [colinkahn.spec.course.coverage.parse :as parse]
            [colinkahn.spec.course.coverage :as cov :refer [defnc]]))

(deftest parsed-defn-test
  (is (= (parse/parsed-defn '(foo "" {}  [x] {} :body))
         (parse/parsed-defn '(foo "" {} ([x] {} :body)))
         (parse/parsed-defn '(foo ""     [x] {} :body))
         (parse/parsed-defn '(foo        [x] {} :body))
         (parse/parsed-defn '(foo        [x]    :body)))))

(deftest parsed-fn-test
  (is (= (parse/parsed-fn '(foo  [x] {} :body))
         (parse/parsed-fn '(foo ([x] {} :body)))
         (parse/parsed-fn '(foo  [x]    :body))))
  (is (= (keys (parse/parsed-fn '(foo [x] {} :body)))
         (keys (parse/parsed-fn '(    [x] {} :body))))))

(deftest branch-test
  (is (= (cov/branch? '(if 1 2 3)) 'if))
  (is (= (cov/branch? '(when 1 2)) 'clojure.core/when))
  (is (= (cov/branch? '(fn [x] x))) 'clojure.core/fn)
  (is (nil? (cov/branch? nil))))

(defnc foo [x]
  (if (pos? x)
    2
    (when (even? x) 4)))

(defnc bar [x]
  (if (odd? x)
    :odd
    (foo x)))

(deftest integration-test-1
  (is (nil?  (cov/with-report [`bar `foo] (bar 1) (bar 2) (bar -2))))
  (is (some? (cov/with-report [`bar `foo] (bar 1) (bar 2))))
  (is (some? (cov/with-report [`foo]))))

(defnc baz [x]
  (let [f (fn [y] y)]
    (when (pos? x)
      (f x))))

(deftest integration-test-2
  (is (nil?  (cov/with-report [`baz] (baz -1) (baz 1))))
  (is (some? (cov/with-report [`baz])))
  (is (some? (cov/with-report [`baz] (baz -1))))
  (is (some? (cov/with-report [`baz] (baz 1)))))

(comment
  (clojure.test/run-tests)

  (require '[clojure.spec.test.alpha :as st])
  (require '[clojure.spec.alpha :as s])

  (defnc options [x]
    (case x
      (:foo :bar) 1
      :baz 2
      :waz 3
      4))

  (cov/with-report [`options]
    (st/check-fn options (s/fspec :args (s/cat :x (s/or :k #{:foo :baz :waz} :any any?))))))
