(ns app.render
  (:require [stylo.core :refer [c]]))

(defn gen-key [] (gensym "key-"))

(defn k [] {:key (gen-key)})

(defn with-key [m] (merge (k) m))

(defn p
  ([content]
   [:p (with-key {:class (c [:mt 1] :text-base :text-gray-500)}) content])
  ([content & other-content]
   [:p (with-key {:class (c [:mt 1] :text-base :text-gray-500)}) content
    other-content]))

(defn pre-bash
  [content]
  [:div
   (with-key
     {:style {:background-color :black, :width :min-content, :border-radius "6px"},
      :class (c [:m 1])})
   [:pre (with-key {:class (c [:text :white] [:mt 1] :text-base)}) content]])

(defn heading
  [& content]
  [:div (with-key {:class (c :content-center [:mt 8])})
   [:div
    (with-key {:class (c :box-border [:pb 10] [:mb 10] [:border-b :gray-200])})
    [:h1
     (with-key {:class (c [:m 1]
                          :text-3xl
                          :inline-block
                          :extrabold
                          [:text :gray-900]
                          :tracking-tight)}) content]]])

(defn a
  ([link] (a link link))
  ([link description]
   [:a (with-key {:href link, :class (c [:text :blue-300] :underline)})
    description]))

(defn about
  []
  [:div
   (heading
     "Philosophy of library."
     (p
       "StyloCSS develops an idea of storing all css in one place without actually touching any .css file.")
     (p "It was inspired by Tailwind CSS. But we want to go far beyond.")
     (p
       "Library develops an idea of storing all css in one place without actually touching any .css file.")
     (p
       " Isntall the Stylo library and keep your focus on styling, not typing. Let the macro do the rest of routine. ")
     (p
       " P.S. library is based on macro, so we need some alchemy to make it work in ClojureScript environment, ")
     (p " but we prepared " "ADD ROUTING" "installation guide"))
   (heading "Version and compatibility. "
            (p "Latest version is 0.1.0")
            (p "Tested with: ")
            (p "Clojure 1.10.0")
            (p "ClojureScript 10.10.866")
            (p "Status: usable alpha."))
   (heading
     "Distribution and license: "
     (p "Copyright © belongs to HealthSamurai and contributors.")
     (p "Distributed under the Eclipse Public License 2.0")
     (p
       "Logo is a property of HealthSamurai, but you can make a T-shirt with it free of charge."))])

(defn installation
  []
  [:div
   (heading "Installation"
            (p
              "Learn to set up shadow-cljs from the scratch in your project. "))
   (heading
     "shadow-cljs"
     (p "1. We create a shadowcljs template:")
     (pre-bash "lein new shadow-cljs shadow-example +reagent")
     (p "Don't forget to install all necessary js packages: ")
     (pre-bash "npm install")
     (p "Other available options of shadow-cljs are given here: "
        (a "https://github.com/shadow-cljs/lein-template"))
     (p "2. Add  into dependencies: ")
     (pre-bash "[stylo-css \"0.1.0 \"]")
     (p " 3. Open up your shadow-cljs.edn configuration file and add")
     (pre-bash ":build-hooks [(stylo.shadow/reload {PATH-TO-CSS})]")
     (p
       "  into the :app configuration. {PATH-TO-CSS} - is a path where the css file will be generated.")
     (p "  Our configuration should look like this: ")
     (pre-bash
       "{... \n :builds \n {:app \n \n {... \n \n :build-hooks [(stylo.shadow/reload \"public/out/stylo/css/stylo.css\")]}}}")
     (p
       "4. Open public/index.html file, it is generated by shadow-cljs by default. We should add the new source of css into it. ")
     (p "Add the following into the <head> </head> section: ")
     (pre-bash " <link href= {PATH-TO-CSS} rel= \"stylesheet\">")
     (p
       "5. So, you it is time to use the library.
           Comprehensive documetation may be read by the link:  "
       "ADD ROUTING: documentation")
     (p "  The basic syntax is the following: ")
     (pre-bash "[:div {:class (c [:pt 8] :h-screen)}] ")
     (p "c - is our macro, it waits for classes alias as arguments ")
     (p ":h-screen [:pt 8] - class alias ")
     (p "when class needs some configuration - it is passed as a vector [:pt 8] - where :pt is class alias and 6 - it's value "))
   (heading "figwheel" (p "Documentation for figwheel is in progress."))])

(defn documentation
  []
  [:div
   (heading
     "Documentation"
     (p
       "As being inspired by Tailwind CSS library - our documentation implements the same logic.")
     (p "Table of contents:")
     (p "Accessibibility.")
     (p "Background.")
     (p "Border.")
     (p "It must be links."))])

(def components
  {:about (about),
   :installation (installation),
   :documentation (documentation)})

(defn render-default [component-key] (component-key components))

(defn render-doc [component-key] (:installation components))
