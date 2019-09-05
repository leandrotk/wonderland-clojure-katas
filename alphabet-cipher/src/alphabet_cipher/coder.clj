(ns alphabet-cipher.coder)

(defn repeat-keyword [keyword message]
  (let [keyword-count (count keyword)
        message-count (count message)]
    (if (>= keyword-count message-count)
      (subs keyword 0 message-count)
      (recur (str keyword keyword) message))))

(defn is-at-least-one-empty? [string-list-1 string-list-2]
  (or
   (empty? string-list-1)
   (empty? string-list-2)))

(defn fix-mod-boundary [value]
  (if (< value 25)
    (+ value 97)
    value))

(defn encode-letter [letter-1 letter-2]
  (let [int-letter-1 (int letter-1)
        int-letter-2 (int letter-2)]
    (-> int-letter-2
        (mod 97)
        (+ int-letter-1)
        (mod 123)
        fix-mod-boundary
        char)))

(defn decode-letter [letter-1 letter-2]
  (let [int-letter-1 (int letter-1)
        int-letter-2 (int letter-2)
        mod-123      (mod 123 int-letter-1)]
    (-> int-letter-2
        (- 97)
        (+ 97 mod-123)
        (mod 123)
        fix-mod-boundary
        char)))

(defn decipher-letter [letter-1 letter-2]
  (let [int-letter-1 (int letter-1)
        int-letter-2 (int letter-2)]
    (if (> int-letter-2 int-letter-1)
      (char (+ 97 (- int-letter-1 97) (- 123 int-letter-2)))
      (char (+ 97 (- int-letter-1 int-letter-2))))))

(defn repeats? [pattern input]
  (empty?
   (filter
    #(not (every? identity (map = pattern %)))
    (partition-all (count pattern) input))))

(defn extract [value]
  (loop [output (str (first value))
         r      (rest value)]
    (if (or (empty? r) (repeats? output r))
      output
      (recur
       (str output (first r))
       (rest r)))))

(defn transform [string-1 string-2 transform-letter]
  (loop [string-list-1 (seq string-1)
         string-list-2 (seq string-2)
         result        ""]
    (if (is-at-least-one-empty? string-list-1 string-list-2)
      result
      (do
        (let [letter-1 (first string-list-1)
              letter-2 (first string-list-2)
              generated-letter (transform-letter letter-1 letter-2)]
          (recur
           (rest string-list-1)
           (rest string-list-2)
           (str result generated-letter)))))))

(defn encode [keyword message]
  (-> keyword
      (repeat-keyword message)
      (transform message encode-letter)))

(defn decode [keyword message]
  (-> keyword
      (repeat-keyword message)
      (transform message decode-letter)))

(defn decipher [cipher message]
  (-> cipher
      (transform message decipher-letter)
      extract))
