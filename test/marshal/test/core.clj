;; Copyright (c) Russell Christopher All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse Public
;; License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found
;; in the file epl-v10.html at the root of this distribution. By using this
;; software in any fashion, you are agreeing to be bound by the terms of
;; this license. You must not remove this notice, or any other, from this
;; software.

(ns marshal.test.core
  (:use [marshal.core] :reload-all)
  (:use [clojure.java.io])
  (:use [clojure.test])
  (:import [java.nio ByteOrder]
           [java.io ByteArrayOutputStream])) 

;(set! *warn-on-reflection* true)

(deftest bigend
  (binding [*byte-order* ByteOrder/BIG_ENDIAN]
    (is (= 1  (read (input-stream (byte-array 4 (map byte [0 0 0 1]))) uint32)))
    (is (= 1  (read (input-stream (byte-array 4 (map byte [0 0 0 1]))) sint32)))
    (is (= 1  (read (input-stream (byte-array 8 (map byte [0 0 0 0 0 0 0 1]))) uint64)))
    (is (= 1  (read (input-stream (byte-array 8 (map byte [0 0 0 0 0 0 0 1]))) sint64)))
    (is (= 4294967295 (read (input-stream (byte-array 4 (map byte [-1 -1 -1 -1]))) uint32)))
    (is (= 18446744073709551615N (read (input-stream (byte-array 8 (map byte [-1 -1 -1 -1 -1 -1 -1 -1]))) uint64)))
    (is (= -1 (read (input-stream (byte-array 4 (map byte [-1 -1 -1 -1])))sint32)))
    (is (= -1 (read (input-stream (byte-array 8 (map byte [-1 -1 -1 -1 -1 -1 -1 -1]))) sint64)))
    (is (= 255 (read (input-stream (byte-array 1 [(byte -1)])) ubyte)))
    (is (= -1 (read (input-stream (byte-array 1 [(byte -1)])) sbyte)))
    (is (= 1 (read (input-stream (byte-array 1 [(byte 1)])) ubyte)))
    (is (= 1 (read (input-stream (byte-array 1 [(byte 1)])) sbyte)))
    (is (= 1 (read (input-stream (byte-array 2 (map byte [0 1]))) ushort)))
    (is (= 1 (read (input-stream (byte-array 2 (map byte [0 1]))) sshort)))
    (is (= 65535 (read (input-stream (byte-array 2 (map byte [-1 -1]))) ushort)))
    (is (= -1 (read (input-stream (byte-array 2 (map byte [-1 -1]))) sshort)))))

(deftest littleend
  (binding [*byte-order* ByteOrder/LITTLE_ENDIAN]
    (is (= 1  (read (input-stream (byte-array 4 (map byte [1 0 0 0]))) uint32)))
    (is (= 1  (read (input-stream (byte-array 4 (map byte [1 0 0 0]))) sint32)))
    (is (= 1  (read (input-stream (byte-array 8 (map byte [1 0 0 0 0 0 0 0]))) uint64)))
    (is (= 1  (read (input-stream (byte-array 8 (map byte [1 0 0 0 0 0 0 0]))) sint64)))
    (is (= 4294967295 (read (input-stream (byte-array 4 (map byte [-1 -1 -1 -1]))) uint32)))
    (is (= 18446744073709551615N (read (input-stream (byte-array 8 (map byte [-1 -1 -1 -1 -1 -1 -1 -1]))) uint64)))
    (is (= -1 (read (input-stream (byte-array 4 (map byte [-1 -1 -1 -1]))) sint32)))
    (is (= -1 (read (input-stream (byte-array 8 (map byte [-1 -1 -1 -1 -1 -1 -1 -1]))) sint64)))
    (is (= 255 (read (input-stream (byte-array 1 [(byte -1)])) ubyte)))
    (is (= -1 (read (input-stream (byte-array 1 [(byte -1)])) sbyte)))
    (is (= 1 (read (input-stream (byte-array 1 [(byte 1)])) ubyte)))
    (is (= 1 (read (input-stream (byte-array 1 [(byte 1)])) sbyte)))
    (is (= 1 (read (input-stream (byte-array 2 (map byte [1 0]))) ushort)))
    (is (= 1 (read (input-stream (byte-array 2 (map byte [1 0]))) sshort)))
    (is (= 65535 (read (input-stream (byte-array 2 (map byte [-1 -1]))) ushort)))
    (is (= -1 (read (input-stream (byte-array 2 (map byte [-1 -1]))) sshort)))))

(deftest bigend-bool
  (binding [*byte-order* ByteOrder/BIG_ENDIAN]
    (let [os (ByteArrayOutputStream.)
          _ (write os bool8 true)
          _ (write os bool8 nil)
	  _ (write os bool8 1)
          _ (write os bool32 true)
          _ (write os bool32 nil)
	  _ (write os bool32 1)
          in (input-stream (.toByteArray os))]
      (is (= true (read in bool8)))
      (is (= false (read in bool8)))
      (is (= true (read in bool8)))
      (is (= true (read in bool32)))
      (is (= false (read in bool32)))
      (is (= true (read in bool32))))))

(deftest littleend-bool
  (binding [*byte-order* ByteOrder/LITTLE_ENDIAN]
    (let [os (ByteArrayOutputStream.)
          _ (write os bool8 true)
          _ (write os bool8 nil)
	  _ (write os bool8 1)
          _ (write os bool32 true)
          _ (write os bool32 nil)
	  _ (write os bool32 1)
          in (input-stream (.toByteArray os))]
      (is (= true (read in bool8)))
      (is (= false (read in bool8)))
      (is (= true (read in bool8)))
      (is (= true (read in bool32)))
      (is (= false (read in bool32)))
      (is (= true (read in bool32))))))

(deftest bigend-float-test
  (binding [*byte-order* ByteOrder/BIG_ENDIAN]
    (let [os (ByteArrayOutputStream. (+ (* 2  ( sizeof double)) (sizeof float)))
          _ (write os double 1.0)
          _ (write os double Math/PI)
	  _ (write os float 2.0)
          in (input-stream (.toByteArray os))]
      (is (= 1.0 (read in double)))
      (is (= Math/PI (read in double)))
      (is (= 2.0 (read in float))))))

(deftest littleend-float-test
  (binding [*byte-order* ByteOrder/LITTLE_ENDIAN]
    (let [os (ByteArrayOutputStream. (+ (* 2 (sizeof double)) (sizeof float)))
          _ (write os double 1.0)
          _ (write os double Math/PI)
	  _ (write os float 2.0)
          in (input-stream (.toByteArray os))]
      (is (= 1.0 (read in double)))
      (is (= Math/PI (read in double)))
      (is (= 2.0 (read in float))))))

(def arr "marshal 2 element array of uint32"
  (array uint32 2))

(deftest test-arr
  (binding [*byte-order* ByteOrder/BIG_ENDIAN]
    (let [os (ByteArrayOutputStream. (sizeof arr))
	  bytes (write os arr [1 2])
          in (input-stream (.toByteArray os))
	  [x y] (read in arr)]
      (is (= 1 x))
      (is (= 2 y))
      (is (= bytes (sizeof arr))))))

(def s "marshals struct a uint32 named :x followed by uint32 named :y" (struct :x uint32 :y uint32))

(deftest test-struct
  (binding [*byte-order* ByteOrder/BIG_ENDIAN]
    (let [os (ByteArrayOutputStream. (sizeof s))
	  bytes (write os s {:x 1 :y 2})
	  in (input-stream (.toByteArray os))
	  {x :x y :y} (read in s)]
      (is (= 1 x))
      (is (= 2 y))
      (is (= bytes (sizeof s))))))

(def s-array "marshals an array of s" (array s 2))

(deftest test-s-array
  (binding [*byte-order* ByteOrder/BIG_ENDIAN]
    (let [os (ByteArrayOutputStream. (sizeof s-array))
	  bytes (write os s-array [{:x 1 :y 2} {:x 3 :y 4}])
          in (input-stream (.toByteArray os))
	  [{x1 :x y1 :y} {x2 :x y2 :y}] (read in s-array)]
      (is (= 1 x1))
      (is (= 2 y1))
      (is (= 3 x2))
      (is (= 4 y2))
      (is (= bytes (sizeof s-array))))))

(def variable-length-array-in-struct (struct
                                      :size uint32
                                      :vla (array s :size)))

(deftest test-variable-length-array-in-struct
  (binding [*byte-order* ByteOrder/BIG_ENDIAN]
    (let [os (ByteArrayOutputStream.)
          vla-in [{:x 1 :y 2} {:x 3 :y 4}]
	  bytes (write os variable-length-array-in-struct {:size (count vla-in) :vla vla-in})
          in (input-stream (.toByteArray os))
	  {size :size vla :vla} (read in variable-length-array-in-struct)
	  [{x1 :x y1 :y} {x2 :x y2 :y}] vla]
      (is (= 2 size))
      (is (= 1 x1))
      (is (= 2 y1))
      (is (= 3 x2))
      (is (= 4 y2)))))

(def variable-length-array-in-struct2 (struct
                                       :start uint32
                                       :stop uint32
                                      :vla (array s (fn [this] (- (:stop this) (:start this))))))

(deftest test-variable-length-array-in-struct2
  (binding [*byte-order* ByteOrder/BIG_ENDIAN]
    (let [os (ByteArrayOutputStream.)
          vla-in [{:x 1 :y 2} {:x 3 :y 4}]
	  bytes (write os variable-length-array-in-struct2 {:start 0 :stop 2 :vla vla-in})
          in (input-stream (.toByteArray os))
	  {size :size vla :vla} (read in variable-length-array-in-struct2)
	  [{x1 :x y1 :y} {x2 :x y2 :y}] vla]
      (is (= 1 x1))
      (is (= 2 y1))
      (is (= 3 x2))
      (is (= 4 y2)))))

(def as (ascii-string 10))

(deftest test-ascii-string
  (let [os (ByteArrayOutputStream. (sizeof as))
        bytes (write os as "1234567890")
        in (input-stream (.toByteArray os))
        str (read in as)]
    (is (= str "1234567890"))))

(deftest test-ascii-string-pad
    (let [os (ByteArrayOutputStream. (sizeof as))
	  bytes (write os as "1234567")
	  in (input-stream (.toByteArray os))
	  str (read in as)]
      (is (= (.trim str) "1234567"))))

(deftest test-ascii-string-truncated
    (let [os (ByteArrayOutputStream. (sizeof as))
	  bytes (write os as "12345678901")
	  in (input-stream (.toByteArray os))
	  str (read in as)]
      (is (= str "1234567890"))))

;; pad with *
(def as* (ascii-string 10 \*))

(deftest test-ascii-string-pad*
    (let [os (ByteArrayOutputStream. (sizeof as*))
	  bytes (write os as* "1234567")
	  in (input-stream (.toByteArray os))
	  str (read in as*)]
      (is (= str "1234567***"))))

(def struct-with-strings (struct :s1 (ascii-string 10) :s2 (ascii-string 20)))

(deftest test-ascii-struct
  (let [os (ByteArrayOutputStream.)
	v {:s1 "abc" :s2 "def"}
	bytes [(write os struct-with-strings v)]
	  in (input-stream (.toByteArray os))
	  res (read in struct-with-strings)]
    (is (= (:s1 res) (apply str (apply conj (vec (:s1 v)) (repeat 7 (char 0)))))
	(= (:s2 res) (apply str (apply conj (vec (:s2 v)) (repeat 17 (char 0))))))))

(def ascii-in-struct (struct
                      :size uint32
                      :ascii (ascii-string :size)))

(deftest test-ascii-in-struct
  (let [os (ByteArrayOutputStream.)
        v {:size 4 :ascii "1234"}
        bytes (write os ascii-in-struct v)
        in (input-stream (.toByteArray os))
        res (read in ascii-in-struct)]
    (is (= (:ascii res) (:ascii v)))))
