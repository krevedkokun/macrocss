(ns stylo.rule
  "hello there"
  (:require [clojure.pprint]))

(defmulti rule (fn [k & _] k))

(defmethod rule :default
  [k & types]
  [[:& {k (pr-str types)}]])

(defn defrules
  ([rules]
  (doseq [[k v] rules]
    (if (associative? v)
     (defmethod rule k [_]
       [[:& v]])
     (defmethod rule k [_ x]
       [[:& (v x)]]))))
  ([rules pseudo-element-key]
   (doseq [[k v] rules]
    (if (associative? v)
     (defmethod rule k [_]
       [[(str ":&:" pseudo-element-key) v]])
     (defmethod rule k [_ x]
       [[(str ":&:" pseudo-element-key) (v x)]])))))

(defn merge-by-selector
  [exps]
  (->> exps
       (group-by first)
       (mapv
         (fn [[selector exps]]
           (let [style (reduce merge (map second exps))
                 children (reduce concat (map #(drop 2 %) exps))]
             (cond-> [selector]
               (seq style) (conj style)
               (seq children) (into children)))))))

(defn join-rules
  [rules]
  (->> rules
       (mapcat
         #(cond
            (and (sequential? %) (every? sequential? %)) %
            (sequential? %) (apply rule %)
            (map? %) [[:& %]]
            :else (rule %)))
       (merge-by-selector)))

(defmacro defrule
  [k & body]
  (let [[attrs body] (if (map? (first body))
                       [(first body) (next body)]
                       [nil body])]
    `(. stylo.rule/rule
        clojure.core/addMethod
        ~(keyword k)
        (cond-> (fn ~k ~@body)
          ~attrs (with-meta (assoc ~attrs :rule ~(keyword k)))))))

(defrule border
  {:doc "hi"
   :category :border
   :args '[[:const 1]
           [:integer :color-kw]]}
  ([_] (border :border 1))
  ([_ & props]
   [[:& (->> props
             (reduce (fn [acc x]
                       acc)
                     {}))]]))

(defrule font-bold
  {:doc "hello there"
   :category :typography
   :usage [:div {:class "hello"}]}
  [& _]
  [[:& {:font-weight 700}]])

(defrule text-lg
  {:doc "hello there"
   :category :typography
   :usage [:div {:class "hello"}]}
  [& _]
  [[:& {:font-size "1.125rem"}]])

(defrule flex
  {:doc "hello there"
   :category :flex
   :usage [:div {:class "hello"}]}
  [& _]
  [[:& {:display "flex"}]])

(defrule items-start
  {:doc "hello there"
   :category :flex
   :usage [:div {:class "hello"}]}
  [& _]
  [[:& {:align-items "flex-start"}]])

(defrule w-px
  {:doc "hello there"
   :category :sizing
   :usage [:div {:class "hello"}]}
  [& _]
  [[:& {:width "1px"}]])

(defrule h-px
  {:doc "hello there"
   :category :sizing
   :usage [:div {:class "hello"}]}
  [& _]
  [[:& {:height "1px"}]])

(defrule grid
  {:doc "hello there"
   :category :grid
   :usage [:div {:class "hello"}]}
  [& _]
  [[:& {:display "grid"}]])

(defrule gap-px
  {:doc "hello there"
   :category :grid
   :usage '[:div {:class (c [:gap-px 1])}]}
  [& _]
  [[:& {:gap "1px"}]])

(comment

;;; class: properties
;;; basic usage

  ((fn a
     ([^String b])
     ([^String b ^Integer c ^clojure.lang.Keyword d]
      (map (partial map (comp :tag meta)) (-> a meta :arglists))))
   "a" 1 :a)

  (->> (methods stylo.rule/rule)
       (keep (comp meta second))
       (group-by :category)
       (reduce-kv
        (fn [acc k v]
          (-> acc
              (assoc-in [k]
                        (mapcat
                         (fn [{:keys [rule args] :as v}]
                           (map (partial hash-map :usage v rule) args))
                         v))))
        {}))

  )
