(ns ulduar.examples.profile
  (:require [clj-time.core :refer [now year date-time]]
            [clojure.pprint :refer [pprint]]
            [ulduar.core :refer :all]))

(comment
  ;; Profiles
  (defrecord Profile [name dob interests friends])

  (defn friends-only [profileA profileB]
    (let [profileA (value profileA)
          profileB (value profileB)]
      (if (some #{(-> profileB :name)} (-> profileA :friends))
        profileA)))

  (def friends-graph
    {"Matt" (->
              (map->Profile {:name "Matt"
                             :dob (date-time 1974 4 4)
                             :interests ["reading" "bicycling"]
                             :friends ["Moe"]})
              (sensitive #'friends-only))

     "Moe" (->
             (map->Profile {:name "Moe"
                            :dob (date-time 1980 03 16)
                            :interests ["skydiving" "coffeeshops"]
                            :friends ["Matt"]})
             (sensitive #'friends-only))

     "George" (->
                (map->Profile {:name "Moe"
                               :dob (date-time 1980 03 16)
                               :interests ["skydiving" "coffeeshops"]
                               :friends ["Matt"]})
                (sensitive #'friends-only))})

  (defn age-diff [p1 p2]
    (with-sensitive [{p1-dob :dob :as p1} p1
                     {p2-dob :dob :as moe} p2]
      (let [p1-age (- 2014 (year p1-dob))
            p2-age (- 2014 (year p2-dob))
            age-diff (- p2-age p1-age)]
        (assoc p1 :age-diff age-diff))))

  (declare-pure clj-time.core/year)

  (with-sensitive [matt (get friends-graph "Matt")
                   moe (get friends-graph "moe")]
    (+ 10 10))
  
  (with-sensitive [matt (get friends-graph "Matt")
                   moe (get friends-graph "Moe") ]
    (let [{matt-dob :dob} matt]
      (+ 10 10)))

  (defn age-diff [p1 p2]
    (with-sensitive [p1 p1, p2 p2]
      (let [p1-age (- 2014 (-> p1 :dob year))
            p2-age (- 2014 (-> p2 :dob year))
            age-diff (- p2-age p1-age)]
        (assoc p1 :age-diff age-diff))))

  ;; should not be allowed because Moe & George are not friends
  (->
   (age-diff (get friends-graph "Matt") (get friends-graph "Moe"))
    (show (get friends-graph "George")))

  )
