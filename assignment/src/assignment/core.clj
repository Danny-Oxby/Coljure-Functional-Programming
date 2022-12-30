(ns assignment.core
  (:gen-class)) ; NEVER DELETE THIS
(require '[clojure.string :as str]) ; used in the morse conversion
(require '[clojure.spec.alpha :as s]) ; used in the specifications

(defn -main ; main is required entry point
  [& args])

;; MENTION IN REPORT
; For my morse and ascii I has used lowercase letters rather than upper case, this is becasue there is no
; diffrence in morse between upper and lowercase, and morese normally only uses one case. In my case I used lower case
; since the converter I based of used lowercase

; morse code has 3 space "   " seperators between the letters and 7 space "       " decomes a "space" charactor
(def ConvertMap {"a" ".-" "b" "-..." "c" "-.-." "d" "-.." "e" "." ;; ascii values here
                 "f" "..-." "g" "--." "h" "...." "i" ".." "j" ".---" "k" "-.-"
                 "l" ".-.." "m" "--" "n" "-." "o" "---" "p" ".--." "q" "--.-"
                 "r" ".-." "s" "..." "t" "-" "u" "..-" "v" "...-" "w" ".--"
                 "x" "-..-" "y" "-.--" "z" "--.." "0" "-----" "1" ".----"
                 "2" "..---" "3" "...--" "4" "....-" "5" "....." "6" "-...."
                 "7" "--..." "8" "---.." "9" "----." " " " ";; space is space in the map
                 ".-" "a" "-..." "b" "-.-." "c" "-.." "d" "." "e" ;; morse values here
                 "..-." "f" "--." "g" "...." "h" ".." "i" ".---" "j" "-.-" "k"
                 ".-.." "l" "--" "m" "-." "n" "---" "o" ".--." "p" "--.-" "q"
                 ".-." "r" "..." "s" "-" "t" "..-" "u" "...-" "v" ".--" "w"
                 "-..-" "x" "-.--" "y" "--.." "z" "-----" "0" ".----" "1"
                 "..---" "2" "...--" "3" "....-" "4" "....." "5" "-...." "6"
                 "--..." "7" "---.." "8" "----." "9"}); map the values to their morse or ascii counter parts


(s/def ::ASCIIInput (s/and string?
                           #(re-matches #"^[a-z0-9 ]+$" %)))

(s/def ::MorseInput (s/and string?
                           #(re-matches #"^[.\- ]+$" %)))

(defn ASCIIConvert [input] ; this methods assumes that any output can't end in a space (since it makes no gramatical sence)
  {:pre [(s/valid? ::ASCIIInput input)] ; the input must always be a letter or number
   :post [(s/valid? ::MorseInput %)]} ; the output must always be a morse string
  (let [value (str/trim input)]
    (loop [index 0 morsestr ""] ; store the current position in the input and the current output string
    (if (= index (count value)) ; if the current index is the same as the input length return the convered string
      (str/trim (apply str morsestr)) ; return the morse string <- removing the trailing spaces
      (recur ; loop begins here 
       (inc index) ; increase the index for the next char in the string 
       (concat morsestr (ConvertMap (subs value index (+ index 1))) "   "))))))

; (println (ASCIIConvert "ab"))
; (println (ASCIIConvert "john went to the game"))
; (println (ASCIIConvert "  abb hi ")) ; input string here .-   -...   -...       ....   ..
; (println (ASCIIConvert "AbB Hi")) ; input string here should fail

; (defn ASCIIConvert [input] ; this methods assumes that any output can't end in a space (since i tmake no gramatical sence)
;   {:pre [(s/valid? ::ASCIIInput input)] ; the input must always be a letter or number
;    :post [(s/valid? ::MorseInput %)]} ; the output must always be a morse string
;   (let [values (mapv #(ConvertMap (str %)) input)] ; convert the input to a list
;     (loop [morsestr "" asciilist values]
;       (if (= (count asciilist) 0)
;         (str/trim (apply str morsestr)) ; return the list as a sting and remove trailing spaces
;         (recur
;          (concat morsestr (first asciilist) "   ") ; ass the 3 "space" word space
;          (rest asciilist)))))) ; shortern the list
; (println (ASCIIConvert "abc de"))
; (println (ConvertMap (str (first "abcde"))))

;; morse to ascii  (.-   -...   -...) -> "abb"
;; find the morse block (either the next " " of the end of input) <- split string at
;; convert to ascii (.-) -> "a"
;; removed used part of the morse blacok (   -...   -...)
;; loop with new ascii value;
(defn MorseConvert [input]
  {:pre [(s/valid? ::MorseInput input)] ; the input must always be a morse value
   :post [(s/valid? ::ASCIIInput %)]} ; the output must always be a string letter or number
  (loop [wordstr (str/split (str/trim input) #"       ") ; split the input on every 7/word space, the # begins the regex
         asciistr ""]
    (if (= wordstr [])
      (apply str asciistr)
      (recur
       (rest wordstr) ; remove the first item in the word vector
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
                 "" ; dont add a trailing space (for gramatical reasons)
                 " ")) ; add sapce for word seperation
       ))))
; (println (MorseConvert "   .-   -...   -.-.   -..   .       .       .   . ")) ; <- abcde e ee
; (println (MorseConvert ".-   -...   -.-.   -..   .       -...   .   -..")) ; <- abcde bed
; (println (MorseConvert "abc")) ; <- should fail


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


;; storage 
;; record of year {month : "Jan" {daylist ["12.3" "3.12"]}} <- easiest manipulation <- try this one

; Chris said I can manipulate the data file e.g.add the extra 10 spaces to the 2022 values to make the data semetric and clean
; but I MUST mention my change in the recording /\ I did add the spaces and a ending new line

(defrecord Monthlyweatherdata [Year Month DayList])
(def MonthList ["Jan" "Feb" "Mar" "Arp" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
;; My specs use a matching name and the req-un to allow for the (conform (-> record)) method set to work
(s/def ::Year (s/and integer?
                     #(< 0001 %)
                     #(> 9999 %)))
(s/def ::Month string?)
(s/def ::DayList (s/coll-of double? :kind sequence))
(s/def ::Monthlyweatherdata
  (s/keys :req-un [::Year ::Month ::DayList]))
(s/def ::YearWeatherData (s/coll-of ::Monthlyweatherdata)) ; a list of valid weatherdata 
(s/def ::ListofYearWeatherData (s/coll-of ::YearWeatherData)) ; a list of valid YearWeatherData <-highest leve spec needed for most pre conditions

(defn GetDailyData [year monthindex] ; return a list of double values for the days
  {:pre [(s/valid? number? monthindex)
         (s/valid? (s/coll-of string?) year)]
   :post [(s/valid? ::DayList %)]}
  (loop [datarow year values []]
    (if (= (count datarow) 0) ; for all 31 data peices
      (s/conform ::DayList values) ; make sure the return is a list of doubles
      (recur
       (rest datarow)
       (conj values ; add the current value to the list
             (double (/ (Integer. (str/trim ; remove the leading spaces and convert to integer, then to number else I get issues
                                   (subs (first datarow) (+ 11 (* 5 monthindex)) (+ 15 (* 5 monthindex))))) 10))))))) ; for each column (11 is the first data column)
; (println (GetDailyData (str/split (slurp "oneyeardata.txt") #"\r\n") 0))
; (println (GetDailyData 123 0))

(defn GetMonthData [year]
  {:pre [(s/valid? (s/coll-of string?) year)]
   :post [(s/valid? ::YearWeatherData %)]}
  (loop [monthindex 0 valuedata []]
    (if (= monthindex 12) ; for all 12 months
      valuedata
      (recur
       (inc monthindex)
       (conj valuedata
             (s/conform ::Monthlyweatherdata (->Monthlyweatherdata ; create the record
                                              (Integer. (str/trim (subs (first year) 0 5))) ;find the year by looking at the firs column of the 31 inputs
                                              (nth MonthList monthindex) ; get the month by converting the index to the monthList
                                              (GetDailyData year monthindex))))))))

; (println (GetMonthData (str/split (slurp "oneyeardata.txt") #"\r\n")))
; (println (GetMonthData 123))

(defn ReadYearlyColumn [filename]
  {:pre [(s/valid? string? filename)]
   :post [(s/valid? ::ListofYearWeatherData %)]}
  (let [info (partition 31 (str/split (slurp filename) #"\r\n")) ; split on every line remove the trailling whate space
        yearvalue (loop [year info values []]
                    (if (= (count year) 0)
                      values
                      (recur
                       (rest year)
                       (conj values (s/conform ::YearWeatherData
                                               (GetMonthData (first year)))))))]
    yearvalue) ; output
  )
; (println (ReadYearlyColumn "weatherdata.txt"))
; (println (ReadYearlyColumn "fiveyeardata.txt"))
; (println (ReadYearlyColumn 123))

(defn Filter99 [list] ; extracted filter function since it's used in multiple places
  {:pre [(s/valid? ::DayList list)]} ; only pass in a list of days
  (filter (fn [x] ; add the new value to the current total
            (not (= -99.9 x))) ; ignore the invalid values
          list))
; (println (Filter99 ["a" "b" "c"]))
; (println (Filter99 [4.7774196 3.8129032 3.4935484 4.4612904 4.387097 2.6451614 6.067742 3.0709677 3.248387 5.351613 2.7774193 2.6903226 0.29032257 2.7741935 2.8 3.7548387 -0.29677418 6.0580645 4.287097 1.0935484 4.3483872 5.3 3.667742 6.632258 -0.2548387 4.822581 1.5225806 1.2870967 3.2903225 1.4612904 3.5677419 4.3645163 2.083871 3.6 6.8 1.8903226 2.216129 4.112903 3.5677419 3.135484 1.7064517 2.8129032 4.3 2.3064516 3.083871 2.4870968 3.6193547 1.383871 4.716129 6.416129 1.6032258 4.819355 5.0935483 4.6 5.7967744 6.883871 7.4258065 1.4096774 1.8032258 5.783871 5.2193546 6.8774195 5.612903 3.0870967 4.1032257 5.303226 4.0225806 3.6903226 1.2870967 4.4129033 7.216129 7.4064517 0.39354845064516 6.283871 3.6806452 5.822581 6.148387 3.4290323 7.1870966 2.8806453 0.61612904 3.5709677 5.3290324 5.3096776 -0.19354838 4.1903224 6.0064516 4.6580644 -0.30322582 0.7 5.0903225 3.9064517 3.903226 4.596774 4.3967743 3.7064517 1.8935484 2.596774 4.8967743 3.2935483 -0.8 4.096774 1.8129033 4.806452 5.1 3.8935485 3.8935485 4.693548 7.3 2.1903226 7.2032256 3.403226 4.6 3.2935483 3.7064517 4.8967743 3.0032258 4.616129 3.903226 3.9064517 6.403226 6.2064514 6.693548 5.1064515 4.6032257 5.3 1.9032258 2.3 6.9064517 5.4903226 4.196774 6.5064516 5.803226 3.8064516 6.7967744 2.7806451 4.180645 2.1064515 3.3967743 5.8096776 4.2967744 5.306452 5.6903224 1.5903226 8.093549 2.73 6.803226 5.4 5.6903224 4.5 4.7032256 5.996774 3.8967743 2.2032259 1.8 2.6032257 3.596774 4.696774 5.4935484 4.196774 3.0 3.2935483 4.3 6.6064515 5.7935486 4.903226 8.0903225 5.2580647 2.016129 6.0870967 3.9451613 5.822581 5.6096773 0.29677418 4.383871 5.5741935 5.248387 6.306452 6.245161 5.596774 7.4774194 4.870968 4.303226 4.6580644 3.564516 5.5096774 6.419355 2.2774193 2.919355 5.774194 5.5387096 4.9741936 5.8096776 3.564516 5.709677 4.770968 5.383871 4.435484 6.470968 4.935484 3.5258064 3.1032257 -0.6935484 5.9580646 4.783871 6.3290324 5.148387 9.674193 5.9741936 4.7419353 6.8935485 5.787097 4.9741936 6.3645163 -99.9]))

;;;; //////////////////////////////////// Question 2.1 //////////////////////////////////////////
(defrecord WarmestData [Year Month Day Value])
(s/def ::Day number?)
(s/def ::Value number?)
(s/def ::WarmestData
  (s/keys :req-un [::Year ::Month ::Day ::Value]))
(s/def ::ListofWarmestData (s/coll-of ::WarmestData))

(defn FindHottestDay [datainput month]
  {:pre [(s/valid? ::ListofYearWeatherData datainput) ; is input the correct format
         (s/valid? number? month)] ; is the month value a number
   :post [(s/conform ::WarmestData %)]} ; is the return the correct format
  (loop [yearlist datainput
         warmestsofar (WarmestData. 0001 "Jan" 1 -99.9)] ; check every year for the wamests in that month
    (if (= (count yearlist) 0)
      (s/conform ::WarmestData warmestsofar) ; when no more years to check return
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
  {:pre [(s/valid? ::ListofYearWeatherData datainput)]
   :post [(s/valid? ::ListofWarmestData %)]}
  (loop [monthindex 0 warmestlist []]
    (if (= monthindex 12) ; for all twelve months
      (s/conform ::ListofWarmestData warmestlist) ; return list once all 12 are found
      (recur
       (inc monthindex)
       (conj warmestlist
             (FindHottestDay datainput monthindex))))))
; (println (FindWarmestInMonth [1 2 3 4 5]))
; (println (FindWarmestInMonth (ReadYearlyColumn "weatherdata.txt")))
; (println (.indexOf (:DayList (nth (first (ReadYearlyColumn "weatherdata.txt")) 0)) 6.2 ) )
; (println (FindHottestDay (ReadYearlyColumn "weatherdata.txt") 11))
; (println (FindHottestDay 123 11))

;;;; //////////////////////////////////// Question 2.2 //////////////////////////////////////////

(defrecord YearData [Year Value])
(s/def ::YearData
  (s/keys :req-un [::Year ::Value]))
(defrecord TempData [Warmest Coldest])
(s/def ::Warmest ::YearData)
(s/def ::Coldest ::YearData)
(s/def ::TempData
  (s/keys :req-un [::Warmest ::Coldest]))

(defn FindYearAvg [yearinput]
  {:pre [(s/valid? ::YearWeatherData yearinput)]
   :post [(s/valid? ::Value %)]}
  (loop [monthindex 0 currentavg 0 stop false] ; stop is for 2022 when all data is invalid
    (if (or (= monthindex 12) stop)
      (s/conform ::Value (float (/ currentavg (if stop
                                                (- monthindex 1)
                                                monthindex))))
      (let [listval (Filter99 (:DayList (nth yearinput monthindex)))]
        (recur ; for every month
         (inc monthindex)
         (+ currentavg
            (if (= listval [])
              0 ; return 0 if the month has no valid values
              (/ (reduce + listval) (count listval))))
         (if (= listval [])
           true
           false)))))) ; divide the months total with the number of valid days
; (println (FindYearAvg (GetMonthData (str/split (slurp "oneyeardata.txt") #"\r\n"))))
; (println (FindYearAvg (GetMonthData (str/split (slurp "lastyeardata.txt") #"\r\n"))))
; (println (FindYearAvg 123))
; (println (FindYearAvg (nth (ReadYearlyColumn "fiveyeardata.txt") 0))) ; 9.149081
; (println (FindYearAvg (nth (ReadYearlyColumn "fiveyeardata.txt") 1))) ; 9.24301
; (println (FindYearAvg (nth (ReadYearlyColumn "fiveyeardata.txt") 2))) ; 9.052168
; (println (FindYearAvg (nth (ReadYearlyColumn "fiveyeardata.txt") 3))) ; 10.079941
; (println (FindYearAvg (nth (ReadYearlyColumn "fiveyeardata.txt") 4))) ; 9.00098
; (println (FindYearAvg (nth (ReadYearlyColumn "fiveyeardata.txt") 5))) ; 9.067841

(defn FindAvgWarmestAndColdestYears [datainput] ; find the average of each year's temp, ignoring -99.9, and sees if its the coldest or warmest year 
  {:pre [(s/valid? ::ListofYearWeatherData datainput)]
   :post [(s/valid? ::TempData %)]}
  (loop [yearlist datainput
         WarmestData (YearData. 0001 -99.9) ; the warmest possible value
         ColdestData (YearData. 0001 99.9)] ;the coldest possible value
    (if (= (count yearlist) 0)
      (s/conform ::TempData (->TempData WarmestData ColdestData))
      (let [year (FindYearAvg (first yearlist))]
        (recur
         (rest yearlist)
         (if (< (:Value WarmestData) year)
           (YearData. (:Year (first (first yearlist))) year) ; get the year from the jan object of that year
           WarmestData)
         (if (> (:Value ColdestData) year)
           (YearData. (:Year (first (first yearlist))) year) ; get the year from the jan object of that year
           ColdestData))))))
; (println (FindAvgWarmestAndColdestYears (ReadYearlyColumn "weatherdata.txt")))

;;;; //////////////////////////////////// Question 2.3 //////////////////////////////////////////

(defn MeanMonthTemp [datainput monthindex] ; find the mean temperature for all "Jan" in a year
  {:pre [(s/valid? ::ListofYearWeatherData datainput)
         (s/valid? number? monthindex)]
   :post [(s/valid? ::Value %)]}
  (loop [yearlist datainput, avgtotal 0, invalidmonths 0] ; invalid months count any month where the list value is []
    (if (= yearlist []) ; for all of the years
      (s/conform ::Value (float (/ avgtotal (- (count datainput) invalidmonths)))) ; return list once all 12 are found
      (let [listval (Filter99 (:DayList (nth (first yearlist) monthindex)))]
        (recur
         (rest yearlist)
         (+ avgtotal (if (= listval [])
                       0 ; return 0 if the month has no valid values
                       (float (/ (reduce + listval) (count listval)))))
         (if (= listval [])
           (inc invalidmonths)
           invalidmonths ; do nothing is month is valid
           ))))))
; (println (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 11))

(defrecord MonthlyMeanAndVarience [Month Mean Nearest Furthest])
(s/def ::Mean ::Value)
(s/def ::Nearest ::YearData) ; match the names so that the conform check is quicker
(s/def ::Furthest ::YearData)
(s/def ::MonthlyMeanAndVarience
  (s/keys :req-un [::Month ::Mean ::Nearest ::Furthest]))
(s/def ::ListofMonthlyMeanAndVarience (s/coll-of ::MonthlyMeanAndVarience))
;(defrecord YearData [Year Value]) << already exists this is a reminder

(defn MonthlyTempVariation [datainput monthindex targetavg] ; find the closes and farthers temp avg from the target
  {:pre [(s/valid? ::ListofYearWeatherData datainput)
         (s/valid? number? monthindex)
         (s/valid? ::Value targetavg)]
   :post [(s/valid? ::MonthlyMeanAndVarience %)]}
  (loop [yearlist datainput avgvalues []]
    (let [listval (Filter99 (:DayList (nth (first yearlist) monthindex)))] ; remove the invalid -99.9 from the daylist
      (if (= yearlist []) ; for all of the years
        ;avgvalues
        (s/conform ::MonthlyMeanAndVarience (->MonthlyMeanAndVarience
                                             (MonthList monthindex)
                                             targetavg ; code snippit for abs found from https://groups.google.com/g/clojure/c/quEzEM_ndCY
                                             (apply min-key #(abs (- (:Value %) targetavg)) avgvalues) ; search for (:Value %) since only models with value list will enter this part
                                             (apply max-key #(abs (- (:Value %) targetavg)) avgvalues))) ; remove any -99.9 added in the recur
        (recur
         (rest yearlist)
         (if (= listval [])
           avgvalues ; if the list has no valid values dont add it to the list
           (conj avgvalues (YearData.
                            (:Year (first (first yearlist)))
                            (float (/ (reduce + listval) (count listval)))))))))))
; (println (MonthlyTempVariation (ReadYearlyColumn "fiveyeardata.txt") 11 (MeanMonthTemp (ReadYearlyColumn "fiveyeardata.txt") 11)))
; (println (count (MonthlyTempVariation (ReadYearlyColumn "weatherdata.txt") 0 (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 0))))
; (println  (MeanMonthTemp (ReadYearlyColumn "weatherdata.txt") 11))

(defn MonthTempData [datainput]
  {:pre [(s/valid? ::ListofYearWeatherData datainput)]
   :post [(s/valid? ::ListofMonthlyMeanAndVarience %)]}
  (loop [monthindex 0 monthlist []]
    (if (= monthindex 12)
      (s/conform ::ListofMonthlyMeanAndVarience monthlist)
      (recur
       (inc monthindex)
       (conj monthlist (MonthlyTempVariation datainput monthindex (MeanMonthTemp datainput monthindex)))))))
; (println (MonthTempData (ReadYearlyColumn "weatherdata.txt")))