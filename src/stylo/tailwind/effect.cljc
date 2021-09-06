(ns stylo.tailwind.effect
  (:require
   [stylo.rule :refer [rule defrules]]
   [stylo.util :refer [as-unit]]))


;; https://tailwindcss.com/docs/box-shadow/#app


(def box-shadow {:shadow  {:box-shadow "0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)"}
                 :shadow-xs  {:box-shadow "0 0 0 1px rgba(0, 0, 0, 0.05)"}
                 :shadow-sm  {:box-shadow "0 1px 2px 0 rgba(0, 0, 0, 0.05)"}
                 :shadow-md  {:box-shadow "0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)"}
                 :shadow-lg  {:box-shadow "0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05)"}
                 :shadow-xl  {:box-shadow "0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04)"}
                 :shadow-2xl  {:box-shadow "0 25px 50px -12px rgba(0, 0, 0, 0.25)"}
                 :shadow-inner  {:box-shadow "inset 0 2px 4px 0 rgba(0, 0, 0, 0.06)"}
                 :shadow-outline  {:box-shadow "0 0 0 3px rgba(66, 153, 225, 0.5)"}
                 :shadow-none  {:box-shadow "none"}})

(defrules box-shadow)

;; https://tailwindcss.com/docs/opacity/#app
;;
(def opacity
  {:opacity (fn [x] {:opacity (as-unit x :percent)})})

(defrules opacity)
