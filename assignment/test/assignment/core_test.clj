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
     (is (= (ASCIIConvert "john went to the game") 
            ".---   ---   ....   -.       .--   .   -.   -       -   ---       -   ....   .       --.   .-   --   .")) ; sentence check
     (is (thrown? java.lang.AssertionError (ASCIIConvert "hElLo WoRlD"))) ; capital check
     ;(is (not (= (ASCIIConvert "hElLo WoRlD") "....   .   .-..   .-..   ---       .--   ---   .-.   .-..   -.."))) ; capital check
   )
  )

(deftest Morse-conversion-test
  (testing "Morse aplhabet conversion"
    (is (= (MorseConvert "--..") "z")) ; <- letter check
    (is (= (MorseConvert "----.") "9")) ; <- number check
    (is (= (MorseConvert ".---   ---   ....   -.       .--   .   -.   -       -   ---       -   ....   .       --.   .-   --   .")
            "john went to the game")) ; sentence check
    (is (not (= (MorseConvert ".-") "z"))) ; <- b is not z
    (is (thrown? java.lang.AssertionError (MorseConvert "abc"))) ; <- b is not z
    (is (= (MorseConvert "....   .   .-..   .-..   ---       .--   ---   .-.   .-..   -..") "hello world")) ; space check
    )
  )

;; //////////////// question setup ///////////////

(deftest Intermediate-weather-methods-test
  (testing "Daily method expected returns"
    (is (= (GetDailyData (str/split (slurp "oneyeardata.txt") #"\r\n") 0) [3.2 2.0 2.7 2.7 1.5 2.2 2.5 0.0 0.0 4.5 6.2 5.2 2.5 1.7 3.0 2.0 -1.8 -1.3 -1.8 -1.0 -0.6 1.5 1.2 0.5 1.2 1.5 0.0 1.5 -3.3 -1.0 -0.8])) ; Jan
    (is (= (GetDailyData (str/split (slurp "oneyeardata.txt") #"\r\n") 4) [8.7 7.7 8.4 9.6 13.3 11.3 10.6 8.4 6.7 5.9 8.7 11.1 11.6 7.4 10.1 13.7 12.1 7.9 9.2 7.4 9.6 11.1 13.6 11.8 13.1 10.8 7.7 11.6 11.3 12.1 10.8])) ; March
    (is (= (GetDailyData (str/split (slurp "oneyeardata.txt") #"\r\n") 11) [11.2 6.2 6.0 4.7 5.0 4.2 3.2 5.5 6.2 10.0 4.5 6.7 8.2 7.7 7.0 8.7 9.0 6.0 11.2 9.7 6.7 1.0 -2.8 0.7 -4.0 -0.8 0.5 1.0 1.2 1.5 2.2])) ; December
    (is (thrown? java.lang.AssertionError (GetDailyData 123 1))) ; invalid input test
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
    (is (thrown? java.lang.AssertionError (GetMonthData 123))) ; invalid input test
    )
    (testing "Yearly Method expected return"
      (is (= (first (first (ReadYearlyColumn "weatherdata.txt"))) Jan1772)) ; check the monthly methods work in when using the yearly method
      (is (= (first (last (ReadYearlyColumn "weatherdata.txt"))) Jan2022))
      (is (= (nth (nth (ReadYearlyColumn "weatherdata.txt") 10 "year has no data") 5 "month not found") Jun1782))
      (is (thrown? java.lang.AssertionError (ReadYearlyColumn 123))) ; invalid input test
      )
  )
)

(deftest Filter-test
      (testing "Testing Filter99 works as intended"
        (is (= (Filter99 [4.7774196 3.8129032 3.4935484 4.4612904 4.387097 2.6451614 6.067742 3.0709677 3.248387 5.351613 2.7774193 2.6903226 0.29032257 2.7741935 2.8 3.7548387 -0.29677418 6.0580645 4.287097 1.0935484 4.3483872 5.3 3.667742 6.632258 -0.2548387 4.822581 1.5225806 1.2870967 3.2903225 1.4612904 3.5677419 4.3645163 2.083871 3.6 6.8 1.8903226 2.216129 4.112903 3.5677419 3.135484 1.7064517 2.8129032 4.3 2.3064516 3.083871 2.4870968 3.6193547 1.383871 4.716129 6.416129 1.6032258 4.819355 5.0935483 4.6 5.7967744 6.883871 7.4258065 1.4096774 1.8032258 5.783871 5.2193546 6.8774195 5.612903 3.0870967 4.1032257 5.303226 4.0225806 3.6903226 1.2870967 4.4129033 7.216129 7.4064517 0.39354845064516 6.283871 3.6806452 5.822581 6.148387 3.4290323 7.1870966 2.8806453 0.61612904 3.5709677 5.3290324 5.3096776 -0.19354838 4.1903224 6.0064516 4.6580644 -0.30322582 0.7 5.0903225 3.9064517 3.903226 4.596774 4.3967743 3.7064517 1.8935484 2.596774 4.8967743 3.2935483 -0.8 4.096774 1.8129033 4.806452 5.1 3.8935485 3.8935485 4.693548 7.3 2.1903226 7.2032256 3.403226 4.6 3.2935483 3.7064517 4.8967743 3.0032258 4.616129 3.903226 3.9064517 6.403226 6.2064514 6.693548 5.1064515 4.6032257 5.3 1.9032258 2.3 6.9064517 5.4903226 4.196774 6.5064516 5.803226 3.8064516 6.7967744 2.7806451 4.180645 2.1064515 3.3967743 5.8096776 4.2967744 5.306452 5.6903224 1.5903226 8.093549 2.73 6.803226 5.4 5.6903224 4.5 4.7032256 5.996774 3.8967743 2.2032259 1.8 2.6032257 3.596774 4.696774 5.4935484 4.196774 3.0 3.2935483 4.3 6.6064515 5.7935486 4.903226 8.0903225 5.2580647 2.016129 6.0870967 3.9451613 5.822581 5.6096773 0.29677418 4.383871 5.5741935 5.248387 6.306452 6.245161 5.596774 7.4774194 4.870968 4.303226 4.6580644 3.564516 5.5096774 6.419355 2.2774193 2.919355 5.774194 5.5387096 4.9741936 5.8096776 3.564516 5.709677 4.770968 5.383871 4.435484 6.470968 4.935484 3.5258064 3.1032257 -0.6935484 5.9580646 4.783871 6.3290324 5.148387 9.674193 5.9741936 4.7419353 6.8935485 5.787097 4.9741936 6.3645163 -99.9])
               [4.7774196 3.8129032 3.4935484 4.4612904 4.387097 2.6451614 6.067742 3.0709677 3.248387 5.351613 2.7774193 2.6903226 0.29032257 2.7741935 2.8 3.7548387 -0.29677418 6.0580645 4.287097 1.0935484 4.3483872 5.3 3.667742 6.632258 -0.2548387 4.822581 1.5225806 1.2870967 3.2903225 1.4612904 3.5677419 4.3645163 2.083871 3.6 6.8 1.8903226 2.216129 4.112903 3.5677419 3.135484 1.7064517 2.8129032 4.3 2.3064516 3.083871 2.4870968 3.6193547 1.383871 4.716129 6.416129 1.6032258 4.819355 5.0935483 4.6 5.7967744 6.883871 7.4258065 1.4096774 1.8032258 5.783871 5.2193546 6.8774195 5.612903 3.0870967 4.1032257 5.303226 4.0225806 3.6903226 1.2870967 4.4129033 7.216129 7.4064517 0.39354845064516 6.283871 3.6806452 5.822581 6.148387 3.4290323 7.1870966 2.8806453 0.61612904 3.5709677 5.3290324 5.3096776 -0.19354838 4.1903224 6.0064516 4.6580644 -0.30322582 0.7 5.0903225 3.9064517 3.903226 4.596774 4.3967743 3.7064517 1.8935484 2.596774 4.8967743 3.2935483 -0.8 4.096774 1.8129033 4.806452 5.1 3.8935485 3.8935485 4.693548 7.3 2.1903226 7.2032256 3.403226 4.6 3.2935483 3.7064517 4.8967743 3.0032258 4.616129 3.903226 3.9064517 6.403226 6.2064514 6.693548 5.1064515 4.6032257 5.3 1.9032258 2.3 6.9064517 5.4903226 4.196774 6.5064516 5.803226 3.8064516 6.7967744 2.7806451 4.180645 2.1064515 3.3967743 5.8096776 4.2967744 5.306452 5.6903224 1.5903226 8.093549 2.73 6.803226 5.4 5.6903224 4.5 4.7032256 5.996774 3.8967743 2.2032259 1.8 2.6032257 3.596774 4.696774 5.4935484 4.196774 3.0 3.2935483 4.3 6.6064515 5.7935486 4.903226 8.0903225 5.2580647 2.016129 6.0870967 3.9451613 5.822581 5.6096773 0.29677418 4.383871 5.5741935 5.248387 6.306452 6.245161 5.596774 7.4774194 4.870968 4.303226 4.6580644 3.564516 5.5096774 6.419355 2.2774193 2.919355 5.774194 5.5387096 4.9741936 5.8096776 3.564516 5.709677 4.770968 5.383871 4.435484 6.470968 4.935484 3.5258064 3.1032257 -0.6935484 5.9580646 4.783871 6.3290324 5.148387 9.674193 5.9741936 4.7419353 6.8935485 5.787097 4.9741936 6.3645163]))
        (is (thrown? java.lang.AssertionError (Filter99 ["a" "b" "c"]))) ; invalid input test
        )) 

;; //////////////// question 2.1 ///////////////

(deftest Find-Warmest-test ; used excel to check these values
  (let [value (FindWarmestInMonth (ReadYearlyColumn "weatherdata.txt"))]
   (testing "Context of the test assertions"
    (is (= (first value) (->WarmestData 2022 "Jan" 1 12.6)))
    (is (= (nth value 5) (->WarmestData 1947 "Jun" 3 23.0)))
    (is (= (last value) (->WarmestData 2015 "Dec" 19 13.1)))
    (is (thrown? java.lang.AssertionError (FindHottestDay 123 11))) ; invalid input test
    (is (thrown? java.lang.AssertionError (FindWarmestInMonth [1 2 3 4 5]))) ; invalid input test
     )
    )
  )
;; //////////////// question 2.2 ///////////////

(deftest Find-Warmest-Coldest-test ; works on the assumption that the first total invalid month signals the end of the data
  (testing "Find the year average"
    (is (= (FindYearAvg (GetMonthData (str/split (slurp "oneyeardata.txt") #"\r\n"))) (float 9.149081))) ; need to be cast to float since float != num
    (is (= (FindYearAvg (GetMonthData (str/split (slurp "lastyeardata.txt") #"\r\n"))) (float 7.4))) ; hardcoded value test
    (is (thrown? java.lang.AssertionError (FindYearAvg 123))) ; invalid input test
    )
  (testing "Find Warmest and Coldest"
    (is (= (:Coldest (FindAvgWarmestAndColdestYears (ReadYearlyColumn "weatherdata.txt"))) (->YearData 2022 (float 7.065614))))
    (is (= (:Warmest (FindAvgWarmestAndColdestYears (ReadYearlyColumn "weatherdata.txt"))) (->YearData 2014 (float 10.923573))))
    (is (= (:Coldest (FindAvgWarmestAndColdestYears (ReadYearlyColumn "fiveyeardata.txt"))) (->YearData 1776 (float 9.00098))))
    (is (= (:Warmest (FindAvgWarmestAndColdestYears (ReadYearlyColumn "fiveyeardata.txt"))) (->YearData 1775 (float 10.079941))))
    (is (thrown? java.lang.AssertionError (FindAvgWarmestAndColdestYears [1 2 3 4 5]))) ; invalid input test
    )
  )

;; //////////////// question 2.3 ///////////////

(deftest Find-Monthly-Mean-test
  (testing "Find specific month mean temp" ; used excel to validate these numbers
    (is (= (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 0) (float 3.457062))) ; ignore the -99.9 values
    (is (= (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 11) (float 4.262929)))
    (is (= (MeanMonthTemp (ReadYearlyColumn "fiveyeardata.txt") 0) (float 1.7860215)))
    (is (= (MeanMonthTemp (ReadYearlyColumn "fiveyeardata.txt") 5) (float 14.935555)))
    (is (= (MeanMonthTemp (ReadYearlyColumn "fiveyeardata.txt") 11) (float 3.92957)))
    (is (thrown? java.lang.AssertionError (MeanMonthTemp 123 0))) ; invalid input test
    )
  (testing "Find the nearest months to each value"
    (is (= (MonthlyTempVariation (ReadYearlyColumn "weatherdata.txt") 0 (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 0))
           (->MonthlyMeanAndVarience "Jan" (float 3.457062) (->YearData 1797 (float 3.4612904))  (->YearData 1795 (float -3.0709677)))))
    (is (= (MonthlyTempVariation (ReadYearlyColumn "weatherdata.txt") 11 (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 11))
           (->MonthlyMeanAndVarience "Dec" (float 4.262929) (->YearData 1790 (float 4.287097))  (->YearData 2015 (float 9.674193)))))
    (is (= (MonthlyTempVariation (ReadYearlyColumn "fiveyeardata.txt") 11 (MeanMonthTemp (ReadYearlyColumn "fiveyeardata.txt") 11))
           (->MonthlyMeanAndVarience "Dec" (float 3.92957) (->YearData 1773 (float 3.8129032))  (->YearData 1777 (float 2.6451614)))))
    (is (thrown? java.lang.AssertionError (MonthlyTempVariation 123 0 (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 0)))) ; invalid input test
    )
  (testing "End Method Test"
    (is (= (first (MonthTempData (ReadYearlyColumn "weatherdata.txt")))
           (->MonthlyMeanAndVarience "Jan" (float 3.457062) (->YearData 1797 (float 3.4612904))  (->YearData 1795 (float -3.0709677)))))
    (is (= (last (MonthTempData (ReadYearlyColumn "weatherdata.txt")))
           (->MonthlyMeanAndVarience "Dec" (float 4.262929) (->YearData 1790 (float 4.287097))  (->YearData 2015 (float 9.674193)))))
    (is (thrown? java.lang.AssertionError (MonthTempData 123))) ; invalid input test
    ))