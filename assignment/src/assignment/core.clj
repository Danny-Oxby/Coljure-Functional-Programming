(ns assignment.core
  (:gen-class)) ; NEVER DELETE THIS
(require '[clojure.string :as str]) ; used in the morse conversion
(require '[clojure.spec.alpha :as s]) ; used in the specifications

(defn -main ; main is required entry point
  [& args])

; morse code has 3 space "   " seperators between the letters and 7 space "       " decomes a "space" charactor
(def ConvertMap {"a" ".-" "b" "-..." "c" "-.-." "d" "-.." "e" "." ;; ascii values here
                 "f" "..-." "g" "--." "h" "...." "i" ".." "j" ".---" "k" "-.-"
                 "l" ".-.." "m" "--" "n" "-." "o" "---" "p" ".--." "q" "--.-"
                 "r" ".-." "s" "..." "t" "-" "u" "..-" "v" "...-" "w" ".--"
                 "x" "-..-" "y" "-.--" "z" "--.." "0" "-----" "1" ".----"
                 "2" "..---" "3" "...--" "4" "....-" "5" "....." "6" "-...."
                 "7" "--..." "8" "---.." "9" "----." ;; space is NOT handeled by the map
                 ".-" "a" "-..." "b" "-.-." "c" "-.." "d" "." "e" ;; morse values here
                 "..-." "f" "--." "g" "...." "h" ".." "i" ".---" "j" "-.-" "k"
                 ".-.." "l" "--" "m" "-." "n" "---" "o" ".--." "p" "--.-" "q"
                 ".-." "r" "..." "s" "-" "t" "..-" "u" "...-" "v" ".--" "w"
                 "-..-" "x" "-.--" "y" "--.." "z" "-----" "0" ".----" "1"
                 "..---" "2" "...--" "3" "....-" "4" "....." "5" "-...." "6"
                 "--..." "7" "---.." "8" "----." "9"}); map the values to their morse or ascii counter parts


(s/def ::ASCIIInput (s/and string?
                           #(re-matches #"^[a-z0-9 ]+$" %)))

(defn ASCIIConvert [input]
  {:pre [(s/valid? ::ASCIIInput input)] ; the input must always be a letter or number
   :post [(s/conform string? %)]} ; the output must always be a string
  (loop [index 0 morsestr ""] ; store the current position in the input and the current output string
    (if (= index (count input)) ; if the current index is the same as the input length return the convered string
      (apply str morsestr) ; return the morse string
      (recur ; loop begins here 
       (inc index) ; increase the index for the next char in the string 
       (if (= (+ index 1) (count input)) ; if is the last item
         (concat morsestr (ConvertMap (subs (str/lower-case input) index (+ index 1)))) ; dont add the last space
         (if (= (subs input index (+ index 1)) " ") ; is this or the next value a space
           (concat morsestr "    ") ; convert the space to 4 (since its gets + 3 from the previouse letter to make 7 long)
           (concat morsestr (ConvertMap (subs (str/lower-case input) index (+ index 1))) "   ") ; three space between letters ; return the next index of the sting and the converted ascii value
)))))) ; the str/lower-case is un-needed here because of the :pre spec

; (defn AsciiTest [input] <-above alternative options <- not working yet
;   (mapv ConvertMap input))
; (println (AsciiTest "abc de"))

; (println (string? (ASCIIConvert "ab")))
; (println (s/conform ::ASCIIInput "AbB Hi"))
; (println (ASCIIConvert "ab"))
; (println (ASCIIConvert "abb hi")) ; input string here .-   -...   -...       ....   ..
; (println (ASCIIConvert "AbB Hi")) ; input string here .-   -...   -...       ....   ..

;; morse to ascii  (.-   -...   -...) -> "abb"
;; find the morse block (either the next " " of the end of input) <- split string at
;; convert to ascii (.-) -> "a"
;; removed used part of the morse blacok (   -...   -...)
;; loop with new ascii value;
(defn MorseConvert [input]
  (loop [wordstr (str/split input #"       ") ; split the input on every 7/word space, the # begins the regex
         asciistr ""]
    (if (= wordstr [])
      (apply str asciistr)
      (recur
       (subvec wordstr 1) ; remove the first item in the word vector
       (concat asciistr
               (loop [mosrsestr (str/split (first wordstr) #"   ") ; split the input on every 3/letter space, the # begins the regex
                      output ""]
                 (if (= mosrsestr []) ; is the vector empty yet
                   (apply str output)
                   (recur ; loop it 
                    (subvec mosrsestr 1) ; remove the first item in the vector
                    (concat output (ConvertMap (first mosrsestr)))) ; convert the first item to ascii value and add to string 
                   ))
               (if (= (count wordstr) 1)
                 ""
                 " ")) ; add sapce for word seperation
       ))))
; (println (MorseConvert ".-   -...   -.-.   -..   .       .       .   .")) ; <- abcde e ee
; (println (MorseConvert ".-   -...   -.-.   -..   .       -...   .   -..")) ; <- abcde bed



;; weather data notes
;; in the format of Year Day Jan Feb March etc
;; the value -999 is null / not there so that should be ignored
;; that value are to the 10th of a degree so need to be multiplied by 10

;; file format first 4 number values are the year
;; date is after 3 spaces (4 for 1 length dates and 3 for two length)
;; each value column is 4 long
;; each value colums is seperated by 1 space minium 
;; the end of line is 10 spaces followed by a \n << excepty 2022 data with only ends in \n
;; position 0 is always an empty space


;; storage options
;; record of year {month : "Jan" {daylist ["1" : "12.3" "2" : "3.12"]}} <- easiest manipulation <- try this one
;; record of year {day : "1" {monthlist [Jan : "12.3" "Feb" : "5.23"]}} <- easiest input

(defrecord Monthlyweatherdata [Year Month DayList])

(def MonthList ["Jan" "Feb" "Mar" "Arp" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(defn GetDailyData [year monthindex] ; conform seq? returned
  (loop [datarow year values []]
    (if (= (count datarow) 0) ; for all 31 data peices
      values
      (recur
       (rest datarow)
       (conj values ; add the current value to the list
             (double (/ (Integer. (str/trim ; remove the leading spaces and convert to integer, then to number else I get issues
                                   (subs (first datarow) (+ 11 (* 5 monthindex)) (+ 15 (* 5 monthindex))))) 10))))))) ; for each column (11 is the first data column)
; (println (GetDailyData (str/split (slurp "oneyeardata.txt") #"\r\n") 0))

(defn GetMonthData [year]
  (loop [monthindex 0 valuedata []]
    (if (= monthindex 12) ; for all 12 months
      valuedata
      (recur
       (inc monthindex)
       (conj valuedata
             (Monthlyweatherdata. ; create the record
              (Integer. (str/trim (subs (first year) 0 5))) ;find the year by looking at the firs column of the 31 inputs
              (nth MonthList monthindex) ; ge the month by converting the index to the monthList
              (GetDailyData year monthindex)))))))
;(println (GetMonthData (str/split (slurp "oneyeardata.txt") #"\r\n")))

(defn ReadYearlyColumn []
  (let [info (partition 31 (str/split (slurp "weatherdata.txt") #"\r\n")) ; split on every line remove the trailling whate space
        yearvalue (loop [year info values []]
                    (if (= (count year) 0)
                      values
                      (recur
                       (rest year)
                       (conj values (GetMonthData (first year))))))]
    yearvalue) ; output
  )
; (println (ReadYearlyColumn))

;;;; //////////////////////////////////// Question 2.1 //////////////////////////////////////////
(defrecord WarmestData [Year Month Day Value])

(defn FindHottestDay [datainput month]
  (loop [yearlist datainput
         warmestsofar (WarmestData. 0001 "Jan" 1 -99.9)] ; check every year for the wamests in that month
    (if (= (count yearlist) 0)
      warmestsofar ; when no more years to check return
      (recur
       (rest yearlist)
       (let [value (reduce max
                           (:DayList (nth (first yearlist) month)))] ; dont care about the -99.9 in this method since we find max not avg
         (if (< (:Value warmestsofar) value) ; if the new value is bigger than the last one update
           (WarmestData.
            (:Year (nth (first yearlist) month)) ;year
            (:Month (nth (first yearlist) month)) ;month
            (+ (.indexOf (:DayList (nth (first yearlist) month)) value) 1) ;day ; find where the value was in the list +1 for 0 index
            value) ;value
           warmestsofar)))))) ; new is smaller keep old value

(defn FindWarmestInMonth [datainput]
  (loop [monthindex 0 warmestlist []]
    (if (= monthindex 12) ; for all twelve months
      warmestlist ; return list once all 12 are found
      (recur
       (inc monthindex)
       (conj warmestlist
             (FindHottestDay datainput monthindex))))))
; (println (FindWarmestInMonth (ReadYearlyColumn)))
; (println (.indexOf (:DayList (nth (first (ReadYearlyColumn)) 0)) 6.2 ) )
; (println (FindHottestDay (ReadYearlyColumn) 11))

;;;; //////////////////////////////////// Question 2.2 //////////////////////////////////////////

(defrecord YearData [Year Value])
(defrecord TempData [Warmest Coldest])

(defn FindYearAvg [yearinput]
  (loop [monthindex 0 currentavg 0 stop false] ; stop is for 2022 when all data is invalid
    (if (or (= monthindex 12) stop)
      (float (/ currentavg (if stop
                             (- monthindex 1)
                             monthindex)))
      (let [listval (filter (fn [x] ; add the new value to the current total
                              (not (= -99.9 x))) ; ignore the invalid values
                            (:DayList (nth yearinput monthindex)))]
        (recur ; for every month
         (inc monthindex)
         (+ currentavg
            (if (= listval [])
              0 ; return 0 if the month has no valid values
              (/ (reduce + listval) (count listval))))
         (if (= listval [])
           true
           false)))))) ; dividate thie months total with the number of valid days
; (println (FindYearAvg (GetMonthData (str/split (slurp "lastyeardata.txt") #"\r\n"))))

(defn FindAvgWarmestAndColdestYears [datainput] ; find the average of each year's temp, ignoring -99.9, and sees if its the coldest or warmest year 
  (loop [yearlist datainput
         WarmestData (YearData. 0001 -99.9) ; the warmest possible value
         ColdestData (YearData. 0001 99.9)] ;the coldest possible value
    (if (= (count yearlist) 0)
      (TempData. WarmestData ColdestData)
      (let [year (FindYearAvg (first yearlist))]
        (recur
         (rest yearlist)
         (if (< (:Value WarmestData) year)
           (YearData. (:Year (first (first yearlist))) year) ; get the year from the jan object of that year
           WarmestData
         )
         (if (> (:Value ColdestData) year)
           (YearData. (:Year (first (first yearlist))) year) ; get the year from the jan object of that year
           ColdestData
         ))))))

(println (FindAvgWarmestAndColdestYears (ReadYearlyColumn)))

;;;; //////////////////////////////////// Question 2.3 //////////////////////////////////////////


; (def month (Monthlyweatherdata. 1772 "Jan" [1 2 3 4 5]))
; (defrecord Yearlyweatherdata [Year MonthData])
; (defrecord MonthlyData [Month DayList])

; Chris said I can manipulate teh data file e.g.add the extra 10 spaces to the 2022 values to make the data semetric and clean
; but I MUST mention my change in the recording


; (s/def ::AsciiValueSpec #{"a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r"
;                            "s" "t" "u" "v" "w" "x" "y" "z" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" " "}) ;only allow number or lowercase letters as valid inputs
; (s/def ::MorseVAleuSpec #{"." "-" " "}) ; only allow '.''-'' ' as valid inputs

; (s/def ::year number?) ; is the year a number
; (s/def ::dayvalue number?) ; give me the day
; ; (s/def ::daylist (s/coll-of ::day :kind (s/map-of ::day int) :distinct true)) ; Store all the days
; (s/def ::daylist (s/coll-of ::dayvalue :kind seq? :distinct false)) ; Store all the days
; ;;(s/def ::daylist (s/coll-of ::day :kind map? (s/map-of ::day int) :distinct true)) ; Store all the days
; (s/def ::datapoint 
;   (s/keys :req [::year ::month ::daylist]))

; (defn ReadFile[] (println (slurp "oneyeardata.txt"))) ; reads the whole txt file
; (ReadFile)
;(defn SplitFile[] (println (str/split (slurp "oneyeardata.txt") #"          "))) ; split on ever space
; (SplitFile)
; (defn SplitFile[] (println (str/split (slurp "oneyeardata.txt") #"\n"))) ; split on ever space
; (defn SplitFile[] (println (count (str/split (slurp "oneyeardata.txt") #"\r\n ")))) ; split on ever space
; (SplitFile)
; (defn Readlistnumber[] (
;   let[info (str/split (slurp "oneyeardata.txt") #"          ") ; split on ever space
;       number(count info)]
;   (println number)) ; output
;   )
; (Readlistnumber)
; (defn ReadDayColumn[] (
;   let[info (str/split (slurp "oneyeardata.txt") #"          ") ; split on ever space
;       daycolumn (loop [row info]
;                   (if (= (count row) 0)
;                     "done"
;                     (recur
;                      (rest row))))]
;   (println daycolumn)) ; output
;   )
; (ReadDayColumn)

;(println (str/trim (subs (first (str/split (slurp "oneyeardata.txt") #"\r\n")) 0 5)))

; (defn SplitFile2[] (println (str/split (slurp "partitiontest.txt") #"\r\n ")))
; (SplitFile2)

; (defn SplitFile [] (println
;                     (loop [year (partition 31 (str/split (slurp "fiveyeardata.txt") #"\r\n ")) values []]
;                       (if (= (count year) 0)
;                         values
;                         (recur
;                          (rest year)
;                          (conj values (str/trim (subs (nth (first year) 0) 0 5))))))))
;(SplitFile)

; (defn ReadDayColumn[] (
;   let[info (str/split (slurp "oneyeardata.txt") #"\r\n") ; split on every line remove the trailling whate space
;       daycolumn (loop [row info values []]
;                   (if (= (count row) 0)
;                     values
;                     (recur
;                      (rest row)
;                      (conj values (str/trim ;;remove the leading spaces
;                                    (subs (first row) 5 10) ; get the date values
;                                    )))))]
;   (println daycolumn)) ; output
;   )
; (ReadDayColumn)
; (defn ReadMonthColumn[] (
;   let[info (str/split (slurp "oneyeardata.txt") #"\r\n") ; split on every line remove the trailling whate space
;       monthlist (Monthlyweatherdata.
;                  1772
;                  "Jan"
;                  (loop [row info values []] ; find dayly data
;                   (if (= (count row) 0)
;                     values
;                     (recur
;                      (rest row)
;                      (conj values (str/trim ;;remove the leading spaces
;                                    (subs (first row) 11 15) ; get the date values
;                                    ))))))]
;   (println monthlist)) ; output
;   )
; (ReadMonthColumn)
; (defn ReadYearlyColumn[] (
  ; let[info (str/split (slurp "oneyeardata.txt") #"\r\n") ; split on every line remove the trailling whate space
  ;     yearvalue (loop [monthindex 0 valuedata []]
  ;                 (if (= monthindex 12)
  ;                   valuedata
  ;                   (recur
  ;                    (inc monthindex)
  ;                    (conj valuedata
  ;                          (Monthlyweatherdata.
  ;                           1772
  ;                           (nth MonthList monthindex)
  ;                           (loop [row info values []] ; find dayly data
  ;                             (if (= (count row) 0)
  ;                               values
  ;                               (recur
  ;                                (rest row)
  ;                                (conj values (str/trim ;;remove the leading spaces
  ;                                              (subs (first row) (+ 11 (* 5 monthindex)) (+ 15 (* 5 monthindex))) ; get the date values
  ;                                              )))))))
  ;                    )
  ;                   ))]
  ; (println yearvalue)) ; output
  ; )

; (println (/(reduce + (filter (fn [x] ; add the new value to the current total
;                               (not (= -99.9 x))) ; ignore the invalid values
;                             (:DayList (nth (GetMonthData (str/split (slurp "lastyeardata.txt") #"\r\n")) 0))))5))
; (println (float (* (/(reduce + (filter (fn [x] ; add the new value to the current total
;                               (not (= -99.9 x))) ; ignore the invalid values
;                             (:DayList (nth (GetMonthData (str/split (slurp "lastyeardata.txt") #"\r\n")) 0)))) 5) 11)))
; (println (float (* (/ (reduce + (filter (fn [x] ; add the new value to the current total
;                               (not (= -99.9 x))) ; ignore the invalid values
;                             (:DayList (nth (GetMonthData (str/split (slurp "lastyeardata.txt") #"\r\n")) 0)))) 5) 12)))
; (println (float (* 12 7.4)))