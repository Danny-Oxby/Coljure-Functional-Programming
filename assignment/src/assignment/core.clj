(ns assignment.core
  (:gen-class)) ; NEVER DELETE THIS
(require '[clojure.string :as str]) ; used in the morse conversion

(defn -main ; main is required entry point
  [& args])

; morse code has 3 space "   " seperators between the letters and 7 space "       " decomes a "space" charactor

(def ConvertMap {"a" ".-" "b" "-..." "c" "-.-." "d" "-.." "e" "." ;; ascii values here
                 "f" "..-." "g" "--." "h" "...." "i" ".." "j" ".---" "k" "-.-"
                 "l" ".-.." "m" "--" "n" "-." "o" "---" "p" ".--." "q" "--.-"
                 "r" ".-." "s" "..." "t" "-" "u" "..-" "v" "...-" "w" ".--"
                 "x" "-..-" "y" "-.--" "z" "--.." "0" "-----" "1" ".----"
                 "2" "..---" "3" "...--" "4" "....-" "5" "....." "6" "-...."
                 "7" "--..." "8" "---.." "9" "----." 
                 " " "       " "       " " "; 1 space equals 7 in morse code
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
          (if (or (= (subs input index (+ index 1)) " ") (= (subs input (+ index 1) (+ index 2)) " ")) ; is this or the next value a space
            (concat morsestr (ConvertMap (subs (str/lower-case input) index (+ index 1)))) ; id so don't add the extra 3 spaces
            (concat morsestr (ConvertMap (subs (str/lower-case input) index (+ index 1))) "   ") ; three space between letters ; return the next index of the sting and the converted ascii value
            )
          )
      )
    ) 
  )
)
(println (ASCIIConvert "abb hi")) ; input string here .-   -...   -...       ....   ..

;; morse to ascii  (.-   -...   -...) -> "abb"
;; find the morse block (either the next " " of the end of input) <- split string at
;; convert to ascii (.-) -> "a"
;; removed used part of the morse blacok (   -...   -...)
;; loop with new ascii value;
(defn MorseConvert [input]
    (loop [mosrsestr (str/split input #"   ") ; split the input on every space, the # begins the regex
            output ""]
            (if (= mosrsestr []) ; is the vector empty yet
              (apply str output)
              (recur ; loop it 
               (subvec mosrsestr 1) ; remove the first item in the vector
               (concat output (ConvertMap (first mosrsestr)))) ; convert the first item to ascii value and add to string 
              )
    )
)
(println (MorseConvert ".-   -...   -.-.   -..   .       -...   .   -..")) ; <- abcde bed
(println (str/split ".-   -...      -.-." #"   "))

;; weather data notes
;; in the format of Year Day Jan Feb March etc
;; the value -999 is null / not there so that should be ignored
;; that value are to the 10th of a degree so need to be multiplied by 10

;; storage options
;; record of year {month : "Jan" {daylist ["1" : "12.3" "2" : "3.12"]}} <- easiest manipulation <- try this one
;; record of year {day : "1" {monthlist [Jan : "12.3" "Feb" : "5.23"]}} <- easiest input