(ns assignment.core
  (:gen-class)) ; NEVER DELETE THIS
(require '[clojure.string :as str]) ; used in the morse conversion
(require '[clojure.spec.alpha :as s]) ; used in the spec

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

(defn ASCIIConvert [input]
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
           ))))))
(println (ASCIIConvert "abb hi")) ; input string here .-   -...   -...       ....   ..

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
(println (MorseConvert ".-   -...   -.-.   -..   .       .       .   .")) ; <- abcde e ee
(println (MorseConvert ".-   -...   -.-.   -..   .       -...   .   -..")) ; <- abcde bed

;; weather data notes
;; in the format of Year Day Jan Feb March etc
;; the value -999 is null / not there so that should be ignored
;; that value are to the 10th of a degree so need to be multiplied by 10

;; storage options
;; record of year {month : "Jan" {daylist ["1" : "12.3" "2" : "3.12"]}} <- easiest manipulation <- try this one
;; record of year {day : "1" {monthlist [Jan : "12.3" "Feb" : "5.23"]}} <- easiest input