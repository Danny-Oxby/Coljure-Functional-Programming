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

(deftest Intermediate-weather-methods
  (testing "Daily method expected returns"
    (is (= (GetDailyData (str/split (slurp "oneyeardata.txt") #"\r\n") 0) [32 20 27 27 15 22 25 0 0 45 62 52 25 17 30 20 -18 -13 -18 -10 -6 15 12 5 12 15 0 15 -33 -10 -8])) ; Jan
    (is (= (GetDailyData (str/split (slurp "oneyeardata.txt") #"\r\n") 4) [87 77 84 96 133 113 106 84 67 59 87 111 116 74 101 137 121 79 92 74 96 111 136 118 131 108 77 116 113 121 108])) ; March
    (is (= (GetDailyData (str/split (slurp "oneyeardata.txt") #"\r\n") 11) [112 62 60 47 50 42 32 55 62 100 45 67 82 77 70 87 90 60 112 97 67 10 -28 7 -40 -8 5 10 12 15 22])) ; December
  )
  (let [Jan1772 (->Monthlyweatherdata 1772 "Jan" [32 20 27 27 15 22 25 0 0 45 62 52 25 17 30 20 -18 -13 -18 -10 -6 15 12 5 12 15 0 15 -33 -10 -8])
        Dec1772 (->Monthlyweatherdata 1772 "Dec" [112 62 60 47 50 42 32 55 62 100 45 67 82 77 70 87 90 60 112 97 67 10 -28 7 -40 -8 5 10 12 15 22])
        Jan1782 (->Monthlyweatherdata 1782 "Jan" [24 97 62 102 69 76 70 68 82 20 -12 36 40 58 38 56 19 5 73 89 36 69 104 106 38 33 42 45 17 29 9])
        Jun1782 (->Monthlyweatherdata 1782 "Jun" [99 87 100 102 125 125 126 159 148 145 127 142 122 147 176 173 161 194 168 176 155 172 162 177 176 201 148 150 160 154 -999])
        Jan2022 (->Monthlyweatherdata 2022 "Jan" [126 98 86 31 29 13 33 49 47 53 67 35 38 20 17 38 45 20 35 21 11 34 44 36 29 49 78 60 81 40 54])
        Dec2022 (->Monthlyweatherdata 2022 "Dec" [-999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999])]
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
      (is (= (first (first (ReadYearlyColumn))) Jan1772)) ; check the monthly methods work in when using the yearly method
      (is (= (first (last (ReadYearlyColumn))) Jan2022))
      (is (= (nth (nth (ReadYearlyColumn) 10 "year has no data") 5 "month not found") Jun1782))
      )
  )
)
;{:Year 1772, :Month Jan, :DayList [32 20 27 27 15 22 25 0 0 45 62 52 25 17 30 20 -18 -13 -18 -10 -6 15 12 5 12 15 0 15 -33 -10 -8]}
