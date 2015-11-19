(ns qbits.benco.test
  (:require
   [pixie.test :as t]
   [qbits.benco :refer :all]))

(t/deftest test-bytestring
  (t/assert= "bar"
             (decode "3:bar"))
  (t/assert= "quizzically"
             (decode "11:quizzically"))
  (t/assert= (encode :bar)
             "3:bar"))

(t/deftest test-int
  (t/assert= 42
             (decode "i42e"))
  (t/assert= -42
             (decode "i-42e"))

  (t/assert= "i42e" (encode 42))
  (t/assert= "i-42e" (encode -42)))

(t/deftest test-coll
  (t/assert= (encode {"z" "1" "a" "foo" })
             "d1:a3:foo1:z1:1e")
  (t/assert= (decode "d1:a3:foo1:z1:1e ")
            {"z" "1" "a" "foo" })
  (t/assert= {"bar" 2, "test" {"a" "foo" }}
             (decode "d3:bari2e4:testd1:a3:fooee")))
