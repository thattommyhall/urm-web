(ns urm-web.prod
  (:require [urm-web.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
