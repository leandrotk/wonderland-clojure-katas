(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]))

(deftest test-repeat-keyword
  (testing "repeat keyword to have the same length as the message"
    (is (= "vigilancevigilancevigilancevi"
           (repeat-keyword "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "vigilancevigila"
           (repeat-keyword "vigilancevigila" "meetmebythetree")))))

(deftest test-encode
  (testing "can encode a message with a secret keyword"
    (is (= "hmkbxebpxpmyllyrxiiqtoltfgzzv"
           (encode "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "egsgqwtahuiljgs"
           (encode "scones" "meetmebythetree")))))

(deftest test-decode
  (testing "can decode a message given an encoded message and a secret keyword"
    (is (= "meetmeontuesdayeveningatseven"
           (decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv")))
    (is (= "meetmebythetree"
           (decode "scones" "egsgqwtahuiljgs")))))

(deftest test-decipher
  (testing "can extract the secret keyword given an encrypted message and the original message"
    (is (= "vigilance"
           (decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")))
    (is (= "scones"
           (decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs")))
    (is (= "abcabcx"
           (decipher "hfnlphoontutufa" "hellofromrussia")))))

(deftest test-decipher-letter
  (testing "decipher the letter"
    (is (= \v (decipher-letter \o \t)))
    (is (= \i (decipher-letter \p \h)))
    (is (= \g (decipher-letter \k \e)))))
