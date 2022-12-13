(ns assignment.core-test
  (:require [clojure.test :refer :all]
            [assignment.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest Ascii-converstion-test
  (testing "Ascii alphabet conversion"
     (is (= (ASCIIConvert "a") ".-")) ; <- mosrse for a
     (is (not(= (ASCIIConvert "b") ".-")))
     ;;(is (= (ASCIIConvert "hello world") ".... . .-.. .-.. --- / .-- --- .-. .-.. -.."))
   )
  )