(ns assignment.core-test
  (:require [clojure.test :refer :all]
            [assignment.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest Ascii-conversion-test
  (testing "Ascii alphabet conversion"
     (is (= (ASCIIConvert "a") ".-")) ; <- mosrse for a
     (is (= (ASCIIConvert "1") ".----")) ; <- mosrse for 1
     (is (not(= (ASCIIConvert "b") ".-")))
     (is (= (ASCIIConvert "hello world") "....   .   .-..   .-..   ---       .--   ---   .-.   .-..   -.."))
   )
  )

(deftest Morse-conversion-test
  (testing "Morse aplhabet conversion"
    (is (= (MorseConvert "--..") "z"))
    (is (= (MorseConvert "--..") "z"))
    (is (not (= (MorseConvert ".-") "z")))
    (is (= (MorseConvert "....   .   .-..   .-..   ---       .--   ---   .-.   .-..   -..") "hello world"))
    )
  )