(ns components.default
  (:require [components.hiccup :refer [h1 h3
                                       p1 p3 a
                                       pre-bash code
                                       block code-span]]
            [stylo.core :refer [c]]))

(defn about
  []

  (block

   (block
   (h1
    "Philosophy of library.")
    (p1
     "StyloCSS develops an idea of storing all css in one place without actually touching any .css file.")
    (p1 "It was inspired by Tailwind CSS. But we want to go far beyond.")
    (p1
     "Library develops an idea of storing all css in one place without actually touching any .css file.")
    (p1 "Install the Stylo library and keep your focus on styling, not typing. Let the macro do the rest of routine. ")
    (p1
     " P.S. library is based on macro, so we need some alchemy to make it work in ClojureScript environment, ")
    (p1 " but we prepared " (a "#installation" "installation") " guide"))

  (block
   (h1 "Version and compatibility. ")
      (p1 "Latest version is 0.1.0")
      (p1 "Tested with: ")
      (p1 "Clojure 1.10.0")
      (p1 "ClojureScript 10.10.866")
      (p1 "Status: usable alpha."))

  (block
    (h1 "Distribution and license: ")
    (p1 "Copyright © belongs to HealthSamurai and contributors.")
    (p1 "Distributed under the Eclipse Public License 2.0")
    (p1 "Logo is a property of HealthSamurai, but you can make a T-shirt with it free of charge."))))

(defn installation
  []
  (block
   (block
    (h1 "Installation")
    (p1 "Learn to set up shadow-cljs from the scratch in your project. "))
   (block
    (h3 "Installing using shadow-cljs and Leiningen")
    (p3 "At first we need to create a new shadow-cljs template.")
    (pre-bash "lein new shadow-cljs shadow-example +reagent")
    (p3 "Don't forget to install all necessary js packages: ")
    (pre-bash "npm install")
    (p3 "Other available options of shadow-cljs are given here: "
       (a "https://github.com/shadow-cljs/lein-template"))
    (p3 "Add  into " [:span {:class (c :font-bold)} "project.clj"] " dependencies: ")
    (code "[stylo-css \"0.1.0 \"]")
    (p3 "Open up your" [:span {:class (c :font-bold)} "shadow-cljs.edn"] "configuration file and add")
    (code "{ :build-hooks [(stylo.shadow/reload \"public/out/stylo/css/stylo.css\")]}")
    (p3 "  into the :app configuration."
        [:span {:class (c :font-bold)} "NB!"]
        "Path written below is the path where the css file will be generated. Do not just copy and paste, find your own location.")
    (p3 " Our configuration should look like this: ")
    (code
     "{... \n :builds \n {:app \n \n {... \n \n :build-hooks [(stylo.shadow/reload \"public/out/stylo/css/stylo.css\")]}}}")
    (p3
     "Open public/index.html file, it is generated by shadow-cljs by default. We should add the new source of css into it. ")
    (p3 "Add the following into the <head> </head> section: ")
    (code " <link href= \"out/stylo/css/stylo.css\" rel= \"stylesheet\">")
    (block
     (h3 "Lets go!")
     (p3
     "So, it is time to use the library!
        Comprehensive documetation may be read "  (a "/documentation" "here") ".")
     (p3 "This landing is done using macroCSS library, so it may explain some conceptions via code: "
         (a "https://github.com/HealthSamurai/macrocss/tree/master/examples" "github/examples."))))
  (block
    (h3 "figwheel")
    (p3 "Documentation for figwheel is in progress."))))

(defn basic-syntax
  []
  (block
   (block
    (h1 "Basic Syntax.")
    (p1 " Essential library usage scenarios."))
  (block
   (h3 "Actually, it is not a rocket science.")
   (p3 "You just require library and do some little magic to make macros work in ClojureScript: ")
    (code "(ns wonderful-ns.core \n
             (:require-macros \n
                  [stylo.core :refer [c]])) \n
            ;; c - hey, it's out macro, it waits for class alias as arguments \n
           [:div {:class (c [:pt 8] :h-screen)}] \n
  ;; :h-screen and [:pt 8] - those keywords are just our version \n
  ;; of Tailwind classnames")

    (p3 "Maybe you have already started to suspect, but when class needs some extra input (e.g. size, color, weight etc.) \n - it is passed as a vector.
    Let us try to explain what happens here: ")
    (code "(c [:pt 8])  \n
           ;; once used inside a component \n
           ;; it generates css with unique classname \n
           ;; (c [:pt 8]) will always return class :c-1581282564 \n
           ;; and properties {'padding-top:' 8rem}  \n
           ;; classname is not random, it's a hash (sic!) \n
           ;; every component with same set of properties \n
           ;; will have the same class name \n
           ;; isn't it more convenient than 'pt-8' in Tailwind? \n
           ;; when you do not use class inside a component \n
           ;; it is swiped out from css file - no useless or dead CSS."))
  (block
   (h3 "Use it as constructor!")
   (p3 "Whether you face situations when you have not found needed css rules \n
        or you have found some inconvenience using pre-defined css - \n
        adding your own is not a big deal.")
   (p3 "Adding a single style may be done by registering new method, we call it " (code-span "rule") ".")
   (p3 "For example - you want to make special style for Apple Iphone XR (c) users: ")
   (code "(ns your-wonderful-code.core\n
            (:require [stylo.rule :refer [rule]])) \n
         ;; Add your custom rule (if styles do not appear after hot-reload \n
         ;; - we recommend to recompile project) \n
         ;; If styles are not added even after - clean \n
         ;; .cpcache .shadow-cljs and public/out folders \n
           (defmethod rule :w-max-iphone-xr [_] \n
              [[:& {:max-width \"414px\"}]])
         ;; (defn apple-geek-div [] \n
              [:div {:class (c :w-max-iphone-xr)}])\n
         ;; And it just works. Seemlessly.")
   (p3 "But what if we want to add rules for a bunch of different phones. Not just a single model?")
   (p3 "We have a solution for this case. Luke, use "  (code-span "defrules") "!")
   (code "(ns your-wonderful-code.core \n
           (:require [stylo.rule :refer [defrules]])) \n
          ;; Defrules receives [k v] structure as an argument. \n
          ;; Hash-map is a perfect solution for it, isn't it? \n
            (def iphones-wmax {:w-max-iphone-12 {:max-width \"390px\"} \n
                               :w-max-iphone-11 {:max-width \"414px\"} \n
                               :w-max-iphone-x {:max-wdith \"375px\"}}) \n
          ;; So let's use defrules \n
            (defrules iphone-wmax) \n
          ;; And yes. It just works. Seemlessly. \n
              (defn one-more-apple-geek-div [] \n
                 [:div {:class (c :w-max-iphone-12)}])"))))

(defn documentation
  []
  [:div
   (block
    (h1 "Documentation")
    (p1 "As being inspired by Tailwind CSS library - our documentation implements the same logic.")
    (p1 "Table of contents:")
    (p1 (a "/accessibility" "Accessibility."))
    (p1 "Background.")
    (p1 "Border.")
    (p1 "It must be links."))])
