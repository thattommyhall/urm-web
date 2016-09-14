(ns urm-web.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [urm-web.urm :as urm]))

;; -------------------------
;; Views


(defn draw-sim [state]
  [:div
   [:div [:h2 "Registers"]]
   [:table
    (for [[register-number register-content] (:registers state)]
      [:tr
       [:td register-number]
       [:td  register-content]])
    ]
   [:div [:h2 "Program"]]
   [:ul
    (for [line (range (count (:program state)))]
      (let [style (if (= (:position state) line)
                    {:style {:background "#60B5CC"}}
                    {})]
        [:li style
         (str (get (:program state) line))]))
    ]])

(defn home-page []
  (draw-sim @urm/current-state))

(defn about-page []
  [:div [:h2 "About urm-web"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (secretary/dispatch! path))
    :path-exists?
    (fn [path]
      (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
