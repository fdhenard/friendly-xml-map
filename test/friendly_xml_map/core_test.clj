(ns friendly-xml-map.core-test
  (:require [clojure.test :refer :all]
            [friendly-xml-map.core :refer :all]
            [schema.core :as s]
            [clojure.data :refer [diff]]
            [clojure.pprint :as pp]))

;; (deftest a-test
;;   (testing "FIXME, I fail."
;;     (is (= 0 1))))

(defn- map-is-same? [map1 map2]
  (let [the-diff (diff map1 map2)]
    (and (nil? (first the-diff)) (nil? (nth the-diff 1)))))


(deftest simple
  (let [What {:attr-one s/Str
              :when s/Str}
        the-xml "<what attr-one=\"who\"><when>one</when></what>"
        xml-map (parse-str the-xml)
        friendly-map (friendly-map-two xml-map What)]
    (is (map-is-same? friendly-map {:attr-one "who"
                                    :when "one"}))))

(deftest list-of-strings-two
  (let [What {:attr-one s/Str
              :attr-two [s/Str]}
        the-xml "<what attr-one=\"who\"><attr-two>one</attr-two><attr-two>two</attr-two></what>"
        xml-map (parse-str the-xml)
        friendly-map (friendly-map-two xml-map What)]
    (println (format "xml-map = %s" (with-out-str (pp/pprint xml-map))))
    (println (format "friendly-map = %s" (with-out-str (pp/pprint friendly-map))))
    (is (map-is-same? friendly-map {:attr-one "who"
                                    :attr-two ["one", "two"]}))))
