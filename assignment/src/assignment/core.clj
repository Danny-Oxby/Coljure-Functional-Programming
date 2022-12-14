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

(s/def ::AsciiValueSpec #{"a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r"
                           "s" "t" "u" "v" "w" "x" "y" "z" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" " "}) ;only allow number or lowercase letters as valid inputs
(s/def ::MorseVAleuSpec #{"." "-" " "}) ; only allow '.''-'' ' as valid inputs

(s/def ::LetterSpec #{"a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r"
                           "s" "t" "u" "v" "w" "x" "y" "z" " "}) ; only allow letters as valid inputs
(s/def ::NumberSepc #{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"}) ; only allow numbers as valid inputs
(def RegexAsciiSpec #"^[a-z0-9]$")

(s/def ::asdf (s/and string?
          #(re-matches RegexAsciiSpec %)))

(println (s/valid? (s/cat :unit ::AsciiValueSpec) "asdfghj"))
;;(println (s/valid? [::AsciiValueSpec] "asdfghj"))
(println (s/valid? ::AsciiValueSpec "a"))

(println (s/valid? ::asdf "asdfghj"))
(println (s/valid? ::asdf "a"))

(defn ASCIIConvert [input]
  ; {:pre [(s/valid? ::AsciiValueSpec input)] ; the input must always be a letter or number
  ;  :post [(s/valid? string? %)]} ; the output must always be a string
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
                 " ")
               ) ; add sapce for word seperation
       ))))
; (println (MorseConvert ".-   -...   -.-.   -..   .       .       .   .")) ; <- abcde e ee
; (println (MorseConvert ".-   -...   -.-.   -..   .       -...   .   -..")) ; <- abcde bed



;; weather data notes
;; in the format of Year Day Jan Feb March etc
;; the value -999 is null / not there so that should be ignored
;; that value are to the 10th of a degree so need to be multiplied by 10

;; file format first 4 values are the year
;; date is after 3 spaces (4 for 1 length dates and 3 for two length)
;; each value column is 4 long
;; each value colums is seperated by 1 space minium 
;; the end of line is 10 soaces followed by a \n


;; storage options
;; record of year {month : "Jan" {daylist ["1" : "12.3" "2" : "3.12"]}} <- easiest manipulation <- try this one
;; record of year {day : "1" {monthlist [Jan : "12.3" "Feb" : "5.23"]}} <- easiest input

(s/def ::year number?) ; is the year a number
(s/def ::month #{"Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"}) ; what month is it
(s/def ::dayvalue number?) ; give me the day
; (s/def ::day #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 32 24 25 26 27 28 29 30 31}) ; give me the day
; (s/def ::daylist (s/coll-of ::day :kind (s/map-of ::day int) :distinct true)) ; Store all the days
(s/def ::daylist (s/coll-of ::dayvalue :kind seq? :distinct false)) ; Store all the days
;;(s/def ::daylist (s/coll-of ::day :kind map? (s/map-of ::day int) :distinct true)) ; Store all the days
(s/def ::datapoint 
  (s/keys :req [::year ::month ::daylist]))
(defrecord Days [day1 day2 day3 day4 day5 day6 day7 day8 day9 day10 
                 day11 day12 day13 day14 day15 day16 day17 day18 day19 day20
                 day21 day22 day23 day24 day25 day26 day27 day28 day29 day30 day31])
(defrecord Monthlyweatherdata [Year Month Daylist])

(def JanTest 
  (Monthlyweatherdata. 2000 "Jan" (Days. 1.01 1.02 1.03 1.04 1.05 1.06 1.07 1.08 1.09 1.10
                                   1.11 1.12 1.13 1.14 1.15 1.16 1.17 1.18 1.19 1.20
                                   1.21 1.22 1.23 1.24 1.25 1.26 1.27 1.28 1.29 1.30
                                   1.31)))

; (defn ReadDataFile[]
;   (let [datarow (str/split (slurp "oneyeardata") #"          \n")
;         createdata (loop [row datarow value Monthlyweatherdata]; 10 spaces followed by new line
;                      (if (= row empty?)
;                        (value)
;                        (recur
;                         (rest row) ;;get the next row value
;                         (value (:Year (subs row 5 9)))
;                         ;;(value (assoc (:Year (subs row 0 4)) (:Month "Jan") (:Daylist (conj value(:Daylist) (subs row 5 9)))))
;                         )
;                       ))]
;   (createdata)) ; return the data
;   )

(defn ReadDataFile[] (println (slurp "oneyeardata.txt")))

(println (slurp "oneyeardata.txt"))

; (def JanTest 
;   (Monthlyweatherdata. :Year 2000 :Month "Jan" :Daylist (Days. 1.01 1.02 1.03 1.04 1.05 1.06 1.07 1.08 1.09 1.10
;                                    1.11 1.12 1.13 1.14 1.15 1.16 1.17 1.18 1.19 1.20
;                                    1.21 1.22 1.23 1.24 1.25 1.26 1.27 1.28 1.29 1.30
;                                    1.31)))
; (println (s/explain ::datapoint JanTest))