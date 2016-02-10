(ns friendly-xml-map.core
  (:require [clojure.xml :as xml]
            [schema.core :as s]))

;; (defn foo
;;   "I don't do a whole lot."
;;   [x]
;;   (println x "Hello, World!"))

(def test-xml "<what attr-one=\"who\"><when>one</when></what>")

(def What
  {:attr-one s/Str
   :when s/Str})

(defn parse-str [xml-str]
  (xml/parse (java.io.ByteArrayInputStream. (.getBytes xml-str))))


(defn friendly-map [xml-map schema]
  (let [valid-attrs (filter #(and (contains? schema (first %))
                                  true ;; also validate type?
                                  )
                            (:attrs xml-map))
        content-filter-func (fn [x]
                              ;; (println "x = " x)
                              (and (map? x)
                                   (contains? schema (:tag x))
                                   (= 1 (count (:content x)))))
        convert-content-to-key-val (fn [content-map]
                                     [(:tag content-map) (-> content-map :content first)])
        valid-content-xf (comp
                          (filter content-filter-func)
                          (map convert-content-to-key-val))
        ;; valid-content (->> (:content xml-map)
        ;;                    (filter content-filter-func)
        ;;                    (map convert-content-to-key-val))
        valid-content (into [] valid-content-xf (:content xml-map))]
    (into {} (concat valid-attrs valid-content))))
