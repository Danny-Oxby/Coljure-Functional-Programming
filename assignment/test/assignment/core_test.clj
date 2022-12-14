(ns assignment.core-test
  (:require [clojure.test :refer :all]
            [assignment.core :refer :all]))

; (deftest a-test
;   (testing "FIXME, I fail."
;     (is (= 1 1))))

(deftest Ascii-conversion-test
  (testing "Ascii alphabet conversion"
     (is (= (ASCIIConvert "a") ".-")) ; <- letter check
     (is (= (ASCIIConvert "1") ".----")) ; <- number check
     (is (not(= (ASCIIConvert "b") ".-"))) ; <- b is not a
     (is (= (ASCIIConvert "hello world") "....   .   .-..   .-..   ---       .--   ---   .-.   .-..   -..")) ; space check
     (is (= (ASCIIConvert "hElLo WoRlD") "....   .   .-..   .-..   ---       .--   ---   .-.   .-..   -..")) ; capital check
   )
  )

(deftest Morse-conversion-test
  (testing "Morse aplhabet conversion"
    (is (= (MorseConvert "--..") "z")) ; <- letter check
    (is (= (MorseConvert "----.") "9")) ; <- number check
    (is (not (= (MorseConvert ".-") "z"))) ; <- b is not z
    (is (= (MorseConvert "....   .   .-..   .-..   ---       .--   ---   .-.   .-..   -..") "hello world")) ; space check
    )
  )