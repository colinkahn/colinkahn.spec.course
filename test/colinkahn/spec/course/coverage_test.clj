(ns colinkahn.spec.course.coverage-test
  (:require [clojure.test :refer :all]
            [colinkahn.spec.course.coverage :as cov :refer [defnc]]))

(deftest parsed-defn-test
  (is (= (cov/parsed-defn '(foo "" {}  [x] {} :body))
         (cov/parsed-defn '(foo "" {} ([x] {} :body)))
         (cov/parsed-defn '(foo ""     [x] {} :body))
         (cov/parsed-defn '(foo        [x] {} :body))
         (cov/parsed-defn '(foo        [x]    :body)))))

(deftest branch-test
  (is (= (cov/branch? '(if 1 2 3)) 'if))
  (is (= (cov/branch? '(when 1 2)) 'clojure.core/when))
  (is (nil? (cov/branch? nil))))

(defnc foo [x]
  (if (pos? x)
    2
    (when 3 4)))

(defnc bar [x]
  (if (odd? x)
    :odd
    (foo x)))

(deftest integration-test
  (is (nil? (cov/with-report [`bar `foo] (bar 1) (bar 2) (bar -2))))
  (is (= (cov/with-report [`bar `foo] (bar 1) (bar 2)) {`foo '[(4) ((when 3 4))]}))
  (is (= (cov/with-report [`foo]) {`foo '[((if (pos? x) 2 (when 3 4))) (4) ((when 3 4)) (2)]})))
