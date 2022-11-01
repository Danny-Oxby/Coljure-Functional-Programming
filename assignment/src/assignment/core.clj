(ns assignment.core
  (:gen-class)) ; NEVER DELETE THIS

; morse code has "space" seperators between the letters and \ decomes a "space" charactor

(def ASCIIMap {:a ".-" :b "-..." :c "-.-."}) ; map the ascii values to their morse counter parts

(defn ASCIIConvert [input]
  (loop [index 0 morsestr ""]
    (if (= index count (input)) ; if the current index is the same as the input length return the convered string
      morsestr
      (recur (inc index) (ASCIIMap [nth (input) index]))) ; return the next index of the sting and the converted ascii value
)) ; not working yet

(println ASCIIConvert "abb")

; version used on codio
;; (def ASCIIMap {:a ".-" :b "-..." :c "-.-."}) ; map the ascii values to their morse counter parts

;; (defn ASCIIConvert [input]
;;   (loop [index 0 morsestr ""]
;;     (if (= index count (input)) ; if the current index is the same as the input length return the convered string
;;       morsestr
;;       (recur (inc index) (concat morsestr (ASCIIMap [nth (input) index])))) ; return the next index of the sting and the converted ascii value
;;     ))

;; (println ASCIIConvert "abb")