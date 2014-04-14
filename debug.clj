(ns user
  (:require [clj-time.core :refer [now year date-time]]
            [clojure.pprint :refer [pprint]]
            [ulduar.core :refer :all]))


(comment
  (defn friends-only-policy [val ctx] (if (-> ctx :friend?) val))
  (defn pals-only-policy [val ctx] (if (-> ctx :pals?) val))
  (defn public-policy [val ctx] (constantly true))

  (def joe {:friend? true})
  (def joeschmoe {:friend? false})

  (def foo (sensitive {:a "foo"} friends-only-policy))
  (def bar (sensitive {:a "bar"} pals-only-policy))

  (merge-with #(and %1 %2) {:a 1} {:b 2})

  (->
    (with-sensitive [foo foo]
      (assoc foo :b "X"))
    (show joeschmoe))

  (->
    (with-sensitive [foo foo
                     bar bar]
      (str foo bar))
    (show joe))

  (->
    (with-sensitive foo
      (+ 1 2 3)
      (/ 64 (* 4 (+ 2 4 (- foo)))))

    (show joe))

  (->
    (with-sensitive foo
      (if (> 9 foo)
        "foo is > 9"
        "foo is <= 9"))
    (show joe))

  (println foo)

  (defn print-foo [foo]
    (with-sensitive [foo foo]
      (println foo)))

  (->
    (with-sensitive foo
      (/ 64 (* 4 (+ 2 (- foo)))))

    (show joeschmoe))


  (let [a (with-sensitive foo (- foo))
        b (with-sensitive a (+ 2 a))
        c (with-sensitive b (* 4 b))
        d (with-sensitive c (/ 64 c))]
    [(show a joeschmoe)
     (show b joe)
     (show c joeschmoe)
     (show d joe)])


  (->
    (with-sensitive foo
      (if (= 11) (+ foo 1)
        (< 2 foo)))

    (show nil))


  (->
    (with-sensitive foo
      (def blah2 (fn [] (str "Stole it! " foo))))

    (show nil))


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

  (->
    (age-diff matt moe)
    (show moe))

(->
  (age-diff moe matt)
  (show matt))

(def age-diff-matt
  (with-sensitive [{matt-dob :dob :as matt} matt
                   {moe-dob :dob} moe]
    (let [matt-age (- 2014 (year matt-dob))
          moe-age (- 2014 (year moe-dob))
          age-diff (- moe-age matt-age)]
      (assoc matt :age-diff age-diff))))
(->
  age-diff
  (show {:friend? true})
  (show {:friend? true}))





;; Papers
(defrecord Paper [title author reviews accepted?])

(defn title-policy [paper ctx]
  "Allows the title of the paper to be visible only to the authors, reviewers, and program committee members, and only visible to the public after it is public."
  (if-not
    (or (= (-> ctx :viewer) (-> paper :author))
        (= (-> ctx :viewer :role) :reviewer)
        (= (-> ctx :viewer :role) :pc)
        (and (= (-> ctx :stage) :public) (-> paper :accepted?)))
    (assoc-in paper [:title] nil)
    paper))

(defn accepted-policy
  "Allows reviewers and program committee members to see whether a paper has been accepted, and for others to see this field only if the stage is public."
  [paper ctx]
  (if-not
    (or (= (-> ctx :viewer :role) :reviewer)
        (= (-> ctx :viewer :role) :pc)
        (= (-> ctx :stage) :public))
    (assoc-in paper [:accepted?] nil)
    nil))

(defn author-policy
  "To see the author name, the user must be an author or the paper must be publicly accepted."
  [paper ctx]
  (if-not
    (or (= (-> paper :author) (-> ctx :user))
        (and (-> ctx :stage) :public
             (-> paper :accepted?)))
    (assoc-in paper [:author] nil)
    nil))


(def paper-policies [title-policy accepted-policy author-policy])

(def draft (-> {:title "A Language for Automatically Enforcing Privacy Policies"
                :author "Jean Yang"
                :reviews []
                :accepted? false}
               (sensitive paper-policies)))

(def published (-> {:title "A Language for Automatically Enforcing Privacy Policies"
                    :author "Jean Yang"
                    :reviews []
                    :accepted? true}
                   (sensitive paper-policies)))

(def reviewer {:viewer {:role :reviewer}
               :stage :review})

(def thepublic {:viewer {:role :public}
                :stage :review})

(show draft reviewer)

(show draft thepublic)

(show published reviewer)

(show published thepublic)

)
