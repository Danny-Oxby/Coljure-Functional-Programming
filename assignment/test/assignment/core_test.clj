(ns assignment.core-test
  (:require [assignment.core :refer :all]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(deftest Ascii-conversion-test
  (testing "Ascii alphabet conversion"
     (is (= (ASCIIConvert "a") ".-")) ; <- letter check
     (is (= (ASCIIConvert "1") ".----")) ; <- number check
     (is (not(= (ASCIIConvert "b") ".-"))) ; <- b is not a
     (is (= (ASCIIConvert "hello world") "....   .   .-..   .-..   ---       .--   ---   .-.   .-..   -..")) ; space check
     ;(is (= (ASCIIConvert "hElLo WoRlD") :clojure.spec.alpha/invalid)) ; capital check
     ;(is (not (= (ASCIIConvert "hElLo WoRlD") "....   .   .-..   .-..   ---       .--   ---   .-.   .-..   -.."))) ; capital check
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

;; //////////////// question setup ///////////////

(deftest Intermediate-weather-methods-test
  (testing "Daily method expected returns"
    (is (= (GetDailyData (str/split (slurp "oneyeardata.txt") #"\r\n") 0) [3.2 2.0 2.7 2.7 1.5 2.2 2.5 0.0 0.0 4.5 6.2 5.2 2.5 1.7 3.0 2.0 -1.8 -1.3 -1.8 -1.0 -0.6 1.5 1.2 0.5 1.2 1.5 0.0 1.5 -3.3 -1.0 -0.8])) ; Jan
    (is (= (GetDailyData (str/split (slurp "oneyeardata.txt") #"\r\n") 4) [8.7 7.7 8.4 9.6 13.3 11.3 10.6 8.4 6.7 5.9 8.7 11.1 11.6 7.4 10.1 13.7 12.1 7.9 9.2 7.4 9.6 11.1 13.6 11.8 13.1 10.8 7.7 11.6 11.3 12.1 10.8])) ; March
    (is (= (GetDailyData (str/split (slurp "oneyeardata.txt") #"\r\n") 11) [11.2 6.2 6.0 4.7 5.0 4.2 3.2 5.5 6.2 10.0 4.5 6.7 8.2 7.7 7.0 8.7 9.0 6.0 11.2 9.7 6.7 1.0 -2.8 0.7 -4.0 -0.8 0.5 1.0 1.2 1.5 2.2])) ; December
  )
  (let [Jan1772 (->Monthlyweatherdata 1772 "Jan" [3.2 2.0 2.7 2.7 1.5 2.2 2.5 0.0 0.0 4.5 6.2 5.2 2.5 1.7 3.0 2.0 -1.8 -1.3 -1.8 -1.0 -0.6 1.5 1.2 0.5 1.2 1.5 0.0 1.5 -3.3 -1.0 -0.8])
        Dec1772 (->Monthlyweatherdata 1772 "Dec" [11.2 6.2 6.0 4.7 5.0 4.2 3.2 5.5 6.2 10.0 4.5 6.7 8.2 7.7 7.0 8.7 9.0 6.0 11.2 9.7 6.7 1.0 -2.8 0.7 -4.0 -0.8 0.5 1.0 1.2 1.5 2.2])
        Jan1782 (->Monthlyweatherdata 1782 "Jan" [2.4 9.7 6.2 10.2 6.9 7.6 7.0 6.8 8.2 2.0 -1.2 3.6 4.0 5.8 3.8 5.6 1.9 0.5 7.3 8.9 3.6 6.9 10.4 10.6 3.8 3.3 4.2 4.5 1.7 2.9 0.9])
        Jun1782 (->Monthlyweatherdata 1782 "Jun" [9.9 8.7 10.0 10.2 12.5 12.5 12.6 15.9 14.8 14.5 12.7 14.2 12.2 14.7 17.6 17.3 16.1 19.4 16.8 17.6 15.5 17.2 16.2 17.7 17.6 20.1 14.8 15.0 16.0 15.4 -99.9])
        Jan2022 (->Monthlyweatherdata 2022 "Jan" [12.6 9.8 8.6 3.1 2.9 1.3 3.3 4.9 4.7 5.3 6.7 3.5 3.8 2.0 1.7 3.8 4.5 2.0 3.5 2.1 1.1 3.4 4.4 3.6 2.9 4.9 7.8 6.0 8.1 4.0 5.4])
        Dec2022 (->Monthlyweatherdata 2022 "Dec" [-99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9 -99.9])]
  (testing "Monthly method expected return"
    (is (= (first (GetMonthData (str/split (slurp "oneyeardata.txt") #"\r\n"))) Jan1772)) ; check it works with small data
    (is (= (first (GetMonthData (nth (partition 31 (str/split (slurp "weatherdata.txt") #"\r\n")) 0 "year has no data"))) Jan1772)) ; check it works with large data
    (is (= (first (GetMonthData (nth (partition 31 (str/split (slurp "weatherdata.txt") #"\r\n")) 10 "year has no data"))) Jan1782)) ; check it works with random partition sample
    (is (= (nth (GetMonthData (nth (partition 31 (str/split (slurp "weatherdata.txt") #"\r\n")) 10 "year has no data")) 5 "month not found") Jun1782)) ; check it works with random partition month
    (is (= (first (GetMonthData (last (partition 31 (str/split (slurp "weatherdata.txt") #"\r\n"))))) Jan2022)) ; check the last blocks data
    (is (= (last (GetMonthData (nth (partition 31 (str/split (slurp "weatherdata.txt") #"\r\n")) 0 "year has no data"))) Dec1772))
    (is (= (last (GetMonthData (last (partition 31 (str/split (slurp "weatherdata.txt") #"\r\n"))))) Dec2022))
    )
    (testing "Yearly Method expected return"
      (is (= (first (first (ReadYearlyColumn "weatherdata.txt"))) Jan1772)) ; check the monthly methods work in when using the yearly method
      (is (= (first (last (ReadYearlyColumn "weatherdata.txt"))) Jan2022))
      (is (= (nth (nth (ReadYearlyColumn "weatherdata.txt") 10 "year has no data") 5 "month not found") Jun1782))
      )
  )
)

;; //////////////// question 2.1 ///////////////

(deftest Find-Warmest-test
  (let [value (FindWarmestInMonth (ReadYearlyColumn "weatherdata.txt"))]
   (testing "Context of the test assertions"
    (is (= (first value) (->WarmestData 2022 "Jan" 1 12.6)))
    (is (= (nth value 5) (->WarmestData 1947 "Jun" 3 23.0)))
    (is (= (last value) (->WarmestData 2015 "Dec" 19 13.1)))
     )
    )
  )
;; //////////////// question 2.2 ///////////////

(deftest Find-Warmest-Coldest-test ; works on the assumption that the first total invalid month signals the end of the data
  (testing "Find the year average"
    (is (= (FindYearAvg (GetMonthData (str/split (slurp "oneyeardata.txt") #"\r\n"))) (float 9.149081))) ; need to be cast to float since float != num
    (is (= (FindYearAvg (GetMonthData (str/split (slurp "lastyeardata.txt") #"\r\n"))) (float 7.4))) ; hardcoded value test
    )
  (testing "Find Warmest and Coldest"
    (is (= (:Coldest (FindAvgWarmestAndColdestYears (ReadYearlyColumn "weatherdata.txt"))) (->YearData 2022 (float 7.065614))))
    (is (= (:Warmest (FindAvgWarmestAndColdestYears (ReadYearlyColumn "weatherdata.txt"))) (->YearData 2014 (float 10.923573))))
    (is (= (:Coldest (FindAvgWarmestAndColdestYears (ReadYearlyColumn "fiveyeardata.txt"))) (->YearData 1776 (float 9.00098))))
    (is (= (:Warmest (FindAvgWarmestAndColdestYears (ReadYearlyColumn "fiveyeardata.txt"))) (->YearData 1775 (float 10.079941))))
    )
  )

;; //////////////// question 2.3 ///////////////

(deftest Find-Monthly-Mean-test ; used excel to validate these numbers
  (testing "Find specific month mean temp"
    (is (= (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 0) (float 3.457062))) ; ignore the -99.9 values
    (is (= (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 11) (float 4.262929)))
    (is (= (MeanMonthTemp (ReadYearlyColumn "fiveyeardata.txt") 0) (float 1.7860215)))
    (is (= (MeanMonthTemp (ReadYearlyColumn "fiveyeardata.txt") 5) (float 14.935555)))
    (is (= (MeanMonthTemp (ReadYearlyColumn "fiveyeardata.txt") 11) (float 3.92957))))
  (testing "Find the nearest months to each value"
    (is (= (MonthlyTempVariation (ReadYearlyColumn "weatherdata.txt") 0 (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 0))
           (->MonthlyMeanAndVarience "Jan" (float 3.457062) (->YearData 1797 (float 3.4612904))  (->YearData 1795 (float -3.0709677)))))
    (is (= (MonthlyTempVariation (ReadYearlyColumn "weatherdata.txt") 11 (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 11))
           (->MonthlyMeanAndVarience "Dec" (float 4.262929) (->YearData 1790 (float 4.287097))  (->YearData 2015 (float 9.674193)))))
    (is (= (MonthlyTempVariation (ReadYearlyColumn "fiveyeardata.txt") 11 (MeanMonthTemp (ReadYearlyColumn "fiveyeardata.txt") 11))
           (->MonthlyMeanAndVarience "Dec" (float 3.92957) (->YearData 1773 (float 3.8129032))  (->YearData 1777 (float 2.6451614)))))))