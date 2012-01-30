;; Copyright (c) Russell Christopher All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse Public
;; License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found
;; in the file epl-v10.html at the root of this distribution. By using this
;; software in any fashion, you are agreeing to be bound by the terms of
;; this license. You must not remove this notice, or any other, from this
;; software.

;;(set! *warn-on-reflection* true)

(ns marshal.core
  (:refer-clojure :exclude [read struct float double])
  (:import [java.nio ByteOrder]
           [java.io InputStream OutputStream]))

(def ^:dynamic *byte-order*  ByteOrder/LITTLE_ENDIAN)

(defn big-endian? [^ByteOrder order]
  "returns true if big endian is the marshal byte order"
  (= ByteOrder/BIG_ENDIAN order))

;;repl convenience funcs
(defn set-little-endian! []
  "change root *byte-order* to little endian"
  (alter-var-root #'*byte-order* (constantly ByteOrder/LITTLE_ENDIAN)))

(defn set-big-endian! []
  "change root *byte-order* to big endian"
  (alter-var-root #'*byte-order* (constantly ByteOrder/BIG_ENDIAN)))

(defprotocol MarshalBytes
  "marshal bytes from InputStream and to OutputStream"
  (m-uval [s arg])
  (m-sval [s arg])
  (m-float [s arg])
  (m-double [s arg])
  (m-array [s arg])
  (m-ascii-string [s arg])
  (m-struct [s arg]))

(defprotocol Marshal
  "marshal interface"
  (m-size [o])
  (m-read [o])
  (m-write [o]))
  
(defn sizeof
  "returns the number of bytes"
  [x] (if (seq x)
	 (reduce #(+ %1 (sizeof %2)) 0 x)
	 ((m-size x))))
  
(def ^:private byte-offsets-le (map #(* 8 %) (range 8)))

(def ^:private  byte-offsets-be (reverse byte-offsets-le))

(defn- byte-offsets [sz]
  (if (big-endian? *byte-order*) (drop (- 8 sz) byte-offsets-be) (take sz byte-offsets-le)))

(defn- to-bytes
  ([v sz] (to-bytes v sz (byte-offsets sz)))
  ([v sz offsets] (map #(bit-and 0xFF (bit-shift-right v (int %))) offsets )))

(defn- write-val [^OutputStream o v sz]
  (let [shift (byte-offsets sz)]
    (count (for [s shift] (.write o (int (bit-and 0XFF (bit-shift-right v s))))))))

(extend-protocol MarshalBytes
  InputStream
  (m-uval [s sz]
    (let [v (byte-array sz)
          n (.read s v)
          shift (byte-offsets sz)]
      (if (<= sz 4)
        (reduce bit-or (map #(bit-shift-left (bit-and 0xFF %1) %2) v shift))
        (reduce +' (map #(*' (bit-and 0xFF %1) (bit-shift-left 1 %2)) v shift)))))
      
  (m-sval [s sz]
    (let [v (byte-array sz)
          n (.read s v)
          shift (byte-offsets sz)]
      (if (= sz 1)
        (first v)
        (if (big-endian? *byte-order*)
          (bit-or (bit-shift-left (first v) (first shift))
                  (reduce bit-or (map #(bit-shift-left (bit-and 0xFF %1) %2) (next v) (next shift))))
          (bit-or (bit-shift-left (last v) (last shift))
                  (reduce bit-or (map #(bit-shift-left (bit-and 0xFF %1) %2) (take (dec sz)  v) (take (dec sz)  shift))))))))

        
      ;;(reduce bit-or (map #(bit-shift-left %1 %2) v shift))))

  (m-float [s _]
    (let [v (byte-array 4)
          n (.read s v)
          shift (byte-offsets 4)]
      (Float/intBitsToFloat (reduce + (map #(bit-shift-left (bit-and 0xFF %1) %2) v shift)))))

  (m-double [s _]
    (let [v (byte-array 8)
          n (.read s v)
          shift (byte-offsets 8)]
      (Double/longBitsToDouble (reduce + (map #(bit-shift-left (bit-and 0xFF %1) %2) v shift)))))

  (m-array [s [o sz]]
    (loop [i 0 res (transient [])]
      (if (< i sz)
        (recur (inc i) (conj! res ((if (fn? o) o (m-read o)) s)))
        (persistent! res))))

  (m-struct [s struct-descr]
    (loop [coll (seq struct-descr) res (transient {})]
      (if coll
        (let [[[k o] & xs] coll]
          (recur xs (assoc! res k ((m-read o) s res))))
        (persistent! res))))

  (m-ascii-string [s sz]
    (let [^StringBuffer sb (StringBuffer.)]
      (dotimes [_ sz]
        (.append sb (char (.read s))))
      (.trim (str sb))))

  OutputStream
  (m-uval [s [v sz]]
    (write-val s v sz))
  
  (m-sval [s [v sz]]
    (write-val s v sz))
  
  (m-float [s [v _]]
    (let [fv (Float/floatToIntBits v)
          ba (to-bytes fv 4)]
      (count (for [x ba] (.write s (int x))))))
  
  (m-double [s [v _]]
    (let [dv (Double/doubleToLongBits v)
          ba (to-bytes dv 8)]
      (count (for [x ba] (.write s (int x))))))
  
  (m-array [s [v o]]
      (reduce + (map #((if (fn? o) o (m-write o)) s %) v)))
  
  (m-struct [s [v d]]
    (reduce + (map (fn [[n f]] ((m-write f) s (v n))) d)))

  (m-ascii-string [s v]
    (count (for [x v] (.write s (bit-and 0xFF (int x)))))))

(defmacro size-f [size]
  `(fn [& _#] ~size))

(defmacro read-f [f arg]
  `(fn [s# & _#] (~f s# ~arg)))

(defmacro write-f [f arg]
  `(fn [s# v#]
     (~f s# [v# ~arg])))

(defmacro add-print-method [class]
  `(defmethod print-method ~class [o# w#] (.write w# (str o#))))

(defmacro primitive [name f size]
  `(defonce ~name
     (let [obj# (reify
		Object
		(toString [_#] ~(str name))
		clojure.lang.Seqable
		(seq [_] nil)
		Marshal
		(m-size [_#] (size-f ~size))
		(m-read [_#] (read-f ~f ~size))
		(m-write [_#] (write-f ~f ~size)))]
       (add-print-method (class obj#))
       obj#)))

;;primitive marshal types
(primitive ubyte m-uval 1)
(primitive sbyte m-sval 1)
(primitive ushort m-uval 2)
(primitive sshort m-sval 2)
(primitive uint32 m-uval 4)
(primitive sint32 m-sval 4)
(primitive uint64 m-uval 8)
(primitive sint64 m-sval 8)
(primitive float m-float 4)
(primitive double m-double 8)

(defmacro primitive-bool [name f size]
  `(defonce ~name
     (let [obj# (reify
		Object
		(toString [_#] ~(str name))
		clojure.lang.Seqable
		(seq [_] nil)
		Marshal
		(m-size [_#] (size-f ~size))
		(m-read [_#] (fn [s# & __#] (> (~f s# ~size) 0)))
		(m-write [_#] (fn [s# v#] (let [v'# (if v# 1 0)]
                                              (~f s# [v'# ~size])))))]
       (add-print-method (class obj#))
       obj#)))

(primitive-bool bool8 m-uval 1)
(primitive-bool bool32 m-uval 4)

(defn array 
  "marshals homogeneous fixed or variable length array; VLA embed in structs in which case sz is either fixed or function or keyword returning the size of the array from map member(s)"
  [o sz]
  (if (or (keyword? sz) (fn? sz))
     (let [obj (reify
		Object
		(toString [_] (format "array [%s * ?]" o))
		clojure.lang.Seqable
		(seq [_] nil)
		Marshal
		(m-size [_] (fn [& args]
			      (if-let [m (first args)]
		 		(* (sz m) (sizeof o))
				(sizeof o))))
		(m-read [_] (fn [s m] (let [n (sz m)]
                                       (if (> n 0 )
                                         (m-array s [o (sz m)])
                                         0))))
		(m-write [_] (fn [s v] (m-array s [v o]))))]
       (add-print-method (class obj))
       obj)
    (let [size (* (sizeof o) sz)
	  obj (reify
	       Object
	       (toString [this] (str (format "array [%s * %d]" o sz)))
	       clojure.lang.Seqable
	       (seq [_] nil)
	       Marshal
	       (m-size [_] (size-f size))
	       (m-read [_] (read-f m-array [o sz]))
	       (m-write [_] (write-f m-array o)))]
       (add-print-method (class obj))
       obj)))


(defn- pad-ascii-string [^String val ^Integer sz p]
  (let [pad (if p (first p) 0)
	l (count val)]
        (cond
         (= l sz) val
         (< l sz) (concat  val (take (- sz l) (repeat pad)))
         :else (.substring val 0 sz))))

(defn ascii-string [sz & pad]
  "marshals a padded fixed width or variable length ASCII string; the default pad char is (char 0); variable length embed in structs in which case sz is either fixed or function or keyword returning the size of the string from map member(s)"
  (if (or (keyword? sz) (fn? sz))
    (let [obj (reify
		Object
		(toString [_] "ascii variable length")
		clojure.lang.Seqable
		(seq [_] nil)
		Marshal
		(m-size [_] (fn [& args]
			      (if-let [m (first args)]
		 		(* (sz m))
				1)))
		(m-read [_] (fn [s m] (m-ascii-string s (sz m))))
		(m-write [_] (fn [s v] (m-ascii-string s v))))]
      (add-print-method (class obj))
      obj)
    (let [obj (reify
                Object
                (toString [_] (format "ascii length:%d" sz))
                clojure.lang.Seqable
                (seq [_] nil)
                Marshal
                (m-size [_] (size-f sz))
                (m-read [_] (read-f m-ascii-string sz))
                (m-write [_] (fn [s v] (m-ascii-string s (pad-ascii-string v sz pad)))))]
      (add-print-method (class obj))
      obj)))

(defn struct
  "marshals ordered (marshaling order) key/value pairs"
  ([& args]
     (let [first (first args)
           arr (if (or (vector? first) (list? first))
                 first
                 args)]
      (if (odd? (count arr)) (throw (IllegalArgumentException. "struct requires an even number of argumenta")))
      (let [fields (take-nth 2 arr)
            types (take-nth 2 (rest arr))
            m (partition 2 arr)
            obj (reify
                  Object
                  (toString [_] (str "struct {" (let [s (print-str arr)
                                                      l (- (count s) 1)]
                                                  (subs s 1 l))
                                     "}"))
                  clojure.lang.Seqable
                  (seq [_] (seq types))
                  Marshal
                  (m-size [_] (fn [& args]
                                (reduce + (map #(apply (m-size %) args) types)))) 
                  (m-read [_] (read-f m-struct m))
                  (m-write [_] (write-f m-struct m)))]
        (add-print-method (class obj))
        obj))))

(defn read [^InputStream s o]
  "marshal value of type o from inputstream s"
  ((m-read o) s))

(defn write [^OutputStream s o v]
  "marshal value v of type o to output stream s"
  ((m-write o) s v))
