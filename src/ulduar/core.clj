(ns ulduar.core
  (:require [clojure.algo.monads :refer [defmonad with-monad domonad] :as m]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [union]]))

(declare sensitive whitelisted-symbol? whitelisted-symbols)

(def ^:private pure-fns (atom #{}))

(defn pure?
 "Checks whether a function is pure."
 [f]
 (or (keyword? f) (contains? @pure-fns f)))

(defn mark-pure
  "Marks functions as being free from side-effects."
  [& fns]
  (let [getvar (fn [v] (if  (var? v) (var-get v) v))
        vars (map getvar fns)]
    (swap! pure-fns (fn [fns] (into #{} (union fns vars)))))
  nil)

(defmacro declare-pure
  "Marks functions as being free from side-effects."
  [& fns]
  (cons mark-pure fns))

(defprotocol ^:private ISensitive
  ""
  (evaluate [_ f])
  (show [_ ctx])
  (value [_])
  (policies [_])
  (depend [_ sens]))

(deftype ^:private Sensitive [value policies dep]
  ISensitive
  (evaluate [_ f]
    (println "Evaluated: " (f value))
    (f value))

  (show [_ ctx]
    (println "Showing " value " to " ctx)
    (let [d (if dep (show dep ctx))
          v (->> policies
                 (map (fn [f] (f value ctx)))
                 (apply merge-with #(and %1 %2)))]
      (println "Merging d: " d " with v: " v " using p: " policies)
      (merge-with #(and %1 %2) v d)))
  (value [_] {:value value :policies policies :dep dep})
  (policies [_] policies)
  (depend [_ dep] (sensitive value policies dep)))

(defn sensitive
  "Wraps a value making it sensitive."
  ([value policies]
   (sensitive value policies nil))
  ([value policies dep]
   (Sensitive. value 
               (conj (if (sequential? policies) policies [policies])
                     (fn [v c] v))
               dep)))

(concat [0] [1])

(->> [identity]
     (map (fn [f] (f {:a 1 :b 2} nil)))
     (apply merge-with #(and %1 %2)))

(defmonad sensitive-m
  [m-result (fn [v]
              (println "m-result:")
              (clojure.pprint/pprint v)
              (sensitive v []))
   m-bind (fn [v f]
            (println "m-bind in:")
            (clojure.pprint/pprint (value v))
            (let [e (evaluate v f)]
              (println "m-bind out:")
              (clojure.pprint/pprint (value (depend e v)))
              (depend e v)))])

(defn- purify [form]
  (if-not (list? form)
    form
    (let [action (first form)]
      (try
        (cond (whitelisted-symbol? action) form
              (pure? (eval action)) form
              :else (throw (Exception.)))
        (catch Exception e
          (throw (Exception. (str "Action not allowed on sensitive data: "
                                  action))))))))

(defmacro with-sensitive
  [bindings & body]
  `(domonad sensitive-m ~(if (sequential? bindings) bindings [bindings bindings])
            (do ~@(postwalk purify body))))


(defn- whitelisted-symbol? [m] (some #{m} whitelisted-symbols))
(def ^:private whitelisted-symbols
  ['->
   '->>
   '..
   'and
   'binding
   'case
   'try
   'catch
   'comment
   'cond
   'cond->
   'cond->>
   'condp
   'doall
   'dorun
   'doseq
   'dosync
   'dotimes
   'doto
   'for
   'if
   'if-let
   'if-not
   'let
   'letfn
   'loop
   'or
   'some->
   'some->>
   'when
   'when-first
   'when-let
   'when-not
   'while
   'with-bindings
   'with-bindings*
   'with-in-str
   'with-loading-context
   'with-local-vars
   'with-meta
   'with-open
   'with-out-str
   'with-precision
   'group-by
   'hash
   'amap
   'areduce
   'lazy-cat
   'lazy-seq
   'pvalues])

(declare-pure
  *
  *'
  +
  +'
  -
  -'
  /
  <
  <=
  =
  ==
  >
  >=
  aget
  alength
  apply
  array-map
  aset
  aset-boolean
  aset-byte
  aset-char
  aset-double
  aset-float
  aset-int
  aset-long
  aset-short
  assoc
  assoc!
  assoc-in
  associative?
  bigdec
  bigint
  biginteger
  bit-and
  bit-and-not
  bit-clear
  bit-flip
  bit-not
  bit-or
  bit-set
  bit-shift-left
  bit-shift-right
  bit-test
  bit-xor
  boolean
  boolean-array
  booleans
  butlast
  byte
  byte-array
  bytes
  char
  char-array
  char?
  chars
  coll?
  commute
  comp
  comparator
  compare
  complement
  concat
  conj
  cons
  constantly
  contains?
  count
  counted?
  cycle
  dec
  dec'
  decimal?
  denominator
  disj
  dissoc
  distinct
  distinct?
  double
  double-array
  doubles
  drop
  drop-last
  drop-while
  empty
  empty?
  even?
  every-pred
  every?
  false?
  ffirst
  filter
  filterv
  find
  first
  flatten
  float
  float-array
  float?
  floats
  fnext
  fnil
  format
  frequencies
  get
  get-in
  hash-combine
  hash-map
  hash-set
  identical?
  identity
  inc
  inc'
  instance?
  int
  int-array
  integer?
  interleave
  interpose
  into
  into-array
  ints
  iterate
  iterator-seq
  juxt
  keep
  keep-indexed
  key
  keys
  keyword
  keyword?
  last
  line-seq
  list
  list*
  list?
  long
  long-array
  longs
  make-array
  map
  map-indexed
  map?
  mapcat
  mapv
  max
  max-key
  merge
  merge-with
  min
  min-key
  mod
  neg?
  newline
  next
  nfirst
  nil?
  nnext
  not
  not-any?
  not-empty
  not-every?
  not=
  nth
  nthnext
  nthrest
  num
  number?
  numerator
  object-array
  odd?
  partial
  partition
  partition-all
  partition-by
  pcalls
  peek
  persistent!
  pmap
  pop
  pop!
  pos?
  quot
  rand
  rand-int
  rand-nth
  range
  ratio?
  rational?
  rationalize
  re-find
  re-groups
  re-matcher
  re-matches
  re-pattern
  re-seq
  reduce
  reduce-kv
  reduced
  reduced?
  reductions
  rem
  remove
  repeat
  repeatedly
  replace
  rest
  reverse
  reversible?
  rseq
  rsubseq
  satisfies?
  second
  select-keys
  seq
  seq?
  seque
  sequence
  sequential?
  set?
  short
  short-array
  shorts
  shuffle
  some
  some-fn
  sort
  sort-by
  sorted-map
  sorted-map-by
  sorted-set
  sorted-set-by
  sorted?
  split-at
  split-with
  str
  string?
  subs
  subseq
  subvec
  take
  take-last
  take-nth
  take-while
  to-array
  to-array-2d
  transient
  tree-seq
  true?
  unchecked-add
  unchecked-add-int
  unchecked-byte
  unchecked-char
  unchecked-dec
  unchecked-dec-int
  unchecked-divide-int
  unchecked-double
  unchecked-float
  unchecked-inc
  unchecked-inc-int
  unchecked-int
  unchecked-long
  unchecked-multiply
  unchecked-multiply-int
  unchecked-negate
  unchecked-negate-int
  unchecked-remainder-int
  unchecked-short
  unchecked-subtract
  unchecked-subtract-int
  update-in
  val
  vals
  vec
  vector
  vector-of
  vector?
  xml-seq
  zero?
  zipmap)

