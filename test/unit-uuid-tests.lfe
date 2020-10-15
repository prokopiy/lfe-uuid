; https://github.com/lfex/ltest

(defmodule unit-uuid-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest is
  (is 'true)
  (is (not 'false))
  (is (not (not 'true))))


(deftest is-equal
  (is-equal 2 (+ 1 1)))


(deftest is-vvv
  (is-equal 4 (+ 2 2)))