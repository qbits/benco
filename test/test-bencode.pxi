(ns qbits.bencode.test
  (:require
   [pixie.test :as t]
   [qbits.bencode :refer :all]))

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
  (t/assert= "l1:a1:bi1ee"
             (encode ["a" :b 1]))

  (t/assert= {"bar" 2, "test" {"a" "foo"}}
             (decode "d3:bari2e4:testd1:a3:fooee")))
