(ns ch4.test.gpc-v1-test
  (:require [ch4.gps-v1 :refer [make-op gps satisfy-goals? ] :as v1]
            [clojure.test :refer :all]))


(def ops
  [(v1/make-op :drive-son-to-school [:son-at-home :car-works] [:son-at-school] [:son-at-home])
   (v1/make-op :shop-install-battery [:car-needs-battery :shop-knows-problem :shop-has-money] [:car-works] [])
   (v1/make-op :tel-1-shop-problem [:in-communication-with-shop] [:shop-knows-problem] [])
   (v1/make-op :telephone-shop [:know-phone-number] [:in-communication-with-shop] [])
   (v1/make-op :look-up-number [:have-phone-book] [:know-phone-number] [])
   (v1/make-op :give-shop-money [:have-money] [:shop-has-money] [:have-money])])


;(v1/gps #{:son-at-home :car-needs-battery :have-money :have-phone-book} ops #{:son-at-school})