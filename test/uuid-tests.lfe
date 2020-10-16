; https://github.com/lfex/ltest

(defmodule uuid-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")


(deftest is
  (is (uuid:is-v3 "b6602c76-f86e-32a2-ab42-71dd055ad3cc"))
  (is (uuid:is-v4 "c5a7d796-006f-4f94-9cb6-8d985bcd1ce8"))
  (is (uuid:is-v5 "ba8ace1d-2df8-507b-b624-352ce199a9c1"))
  (is (uuid:is-v3 (uuid:to-string (uuid:v3 'nil "abcd"))))
  (is (uuid:is-v4 (uuid:to-string (uuid:v4))))
  (is (uuid:is-v5 (uuid:to-string (uuid:v5 'nil "abcd"))))
  (is 'true))


(deftest is-equal
  (is-equal "f93248f8-9a28-3fff-872a-3581d0b86eca" (uuid:to-string (uuid:v3 'dns "abcd")))
  (is-equal "73ee125b-19ac-549c-9f5d-9dc9c198112b" (uuid:to-string (uuid:v5 'dns "abcd")))
  (is-equal (uuid:v3 'url "abcd") (uuid:to-binary (uuid:to-string (uuid:v3 'url "abcd"))))
  (is-equal (uuid:v5 'url "abcd") (uuid:to-binary (uuid:to-string (uuid:v5 'url "abcd")))))


