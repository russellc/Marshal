(ns user
  "Init script for Leiningen REPL."
  (:require [marshal.core :as m]
            [clojure.java.io]))

(defn baos
  ([] (java.io.ByteArrayOutputStream. ))
  ([sz] (java.io.ByteArrayOutputStream. sz)))

(defn is [os]
  (clojure.java.io/input-stream (.toByteArray os)))

(def dbheaderfield (m/struct :name (m/ascii-string 11)
                             :type m/ubyte
                             :offset m/uint32
                             :field_size m/ubyte
                             :field_dec m/ubyte
                             :res1 (m/array  m/ubyte 2)
                             :work_area_id m/ubyte
                             :res2 (m/array m/ubyte 10)
                             :prod_index m/ubyte))

(def dbheader (m/struct :type  m/ubyte
                        :mod_year m/ubyte
                        :mod_month m/ubyte
                        :mod_day m/ubyte
                        :rec_count m/uint32
                        :header_size m/ushort
                        :rec_size m/ushort
                        :res1 (m/array m/ubyte 2)
                        :incomplete_trans m/ubyte
                        :encryption m/ubyte
                        :multi_user (m/array m/ubyte 12)
                        :prod_index m/ubyte
                        :lang_id m/ubyte
                        :res2 (m/array m/ubyte 2)
                        :fields (m/array dbheaderfield (fn [this] (/ (- (:header_size this) 32 1) (m/sizeof dbheaderfield))))
                        :terminator m/ubyte))

(defn read-column [s m field]
  (let [sz (:field_size field)
        name (keyword (.trim (:name field)))
        val (.trim (m/read s (m/ascii-string sz)))]
    (condp = (char (:type field))
        \N (assoc m name (Integer/parseInt (.trim val)))
        \C (assoc m name val))))

(defn read-records [s]
  (let [header (m/read s dbheader)
        fields (:fields header)]
    (for [_ (range (:rec_count header))]
      (if (= 0x20 (.read s))
        (reduce #(read-column s %1 %2) {:deleted false} fields)
        (reduce #(read-column s %1 %2) {:deleted true} fields)))))
 
(defn dbf [filename]
  (binding [m/*byte-order* java.nio.ByteOrder/LITTLE_ENDIAN]
    (with-open [s (clojure.java.io/input-stream filename)]
      (doall (read-records s)))))
