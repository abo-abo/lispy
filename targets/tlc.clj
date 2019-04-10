(defn disable-illegal-access-warnings []
  (let [the-unsafe (. (Class/forName "sun.misc.Unsafe") getDeclaredField "theUnsafe")
        u (do
            (. the-unsafe setAccessible true)
            (. the-unsafe get nil))
        cls (Class/forName "jdk.internal.module.IllegalAccessLogger")
        logger (. cls getDeclaredField "logger")]
    (. u putObjectVolatile cls (. u staticFieldOffset logger) nil)))

(defn print-versions []
  (println (str "Clojure: " (clojure-version)))
  (println (str "JVM: " (System/getProperty "java.version"))))

(print-versions)
(disable-illegal-access-warnings)
(load-file "lispy-clojure.clj")
