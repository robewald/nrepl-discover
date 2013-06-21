(ns nrepl.discover
  (:require [clojure.tools.nrepl.transport :as t]
            [clojure.tools.nrepl.misc :as m]
            [clojure.tools.trace :as trace]
            [clojure.test :as test]
            [clojure.walk :as w]
            [clojure.repl :as repl]
            [clojure.java.javadoc :as javadoc]
            [clojure.java.io :as io]))

(declare discover)

(def ops
  (delay (into {"discover" #'discover}
               (for [n (all-ns)
                     [_ v] (ns-publics n)
                     :when (:nrepl/op (meta v))]
                 [(:name (:nrepl/op (meta v))) v]))))

(defn- ^{:nrepl/op {:name "discover"
                    :doc "Discover known operations."}}
  discover [{:keys [transport] :as msg}]
  (t/send transport (m/response-for msg :status :done :value
                                    (for [[_ op-var] @ops]
                                      (-> op-var meta :nrepl/op
                                          w/stringify-keys)))))

(defn wrap-discover [handler]
  (fn [msg] ((@ops (:op msg) handler) msg)))

;; example nrepl op for tools.trace
(defn ^{:nrepl/op {:name "toggle-trace" :args [["var" "var" "Trace: "]]
                   :doc "Toggle tracing of a given var."}}
  nrepl-op
  [{:keys [transport var] :as msg}]
  (try
    (if-let [v (resolve (symbol var))]
      (if (-> v meta :clojure.tools.trace/traced)
        (do (trace/untrace-var* v)
            (t/send transport (m/response-for msg :status :done
                                              :message (str var " untraced."))))
        (do (trace/trace-var* v)
            (t/send transport (m/response-for msg :status :done
                                              :message (str var " traced.")))))
      (t/send transport (m/response-for msg :status #{:error :done}
                                        :message "no such var")))
    (catch Exception e
      (#'trace/tracer "Error" (.getMessage e))
      (t/send transport (m/response-for msg :status #{:error :done}
                                        :message (.getMessage e))))))

;; example nrepl op for clojure.test
(defn- report [old-report event]
  (when-let [current-test (last test/*testing-vars*)]
    (alter-meta! current-test
                 assoc :status (conj (:status (meta current-test))
                                     [(:type event) (:message event)
                                      (str (:expected event))
                                      (str (:actual event))
                                      ((test/file-position 2) 1)])))
  (old-report event))

(defn- test-results [namespace]
  (filter :status (map meta (vals (ns-interns (symbol namespace))))))

(defn- overlay-for [{:keys [status line]}]
  (cond (some (comp #{:fail} first) status) ["red" line]
        (some (comp #{:error} first) status) ["orange" line]
        :else ["green" line]))

(defn summary-response [msg ns summary results]
  (let [message (if (pos? (+ (:error summary) (:fail summary)))
                  "Failed" "Passed")]
    (doseq [r results]
      (t/send (:transport msg) (m/response-for msg :overlay (overlay-for r))))
    (t/send (:transport msg) (m/response-for msg :status :done
                                             :message message))))

(defn ^{:nrepl/op {:name "run-tests"
                   :doc "Run tests for a namespace"}}
  run-tests
  [{:keys [transport ns] :as msg}]
  (try
    (t/send transport (m/response-for msg :clear-overlays "true"))
    (let [summary (with-redefs [test/report (partial report test/report)]
                    (test/run-tests (symbol ns)))]
      (summary-response msg ns summary (test-results ns)))
    (catch Exception e
      (t/send transport (m/response-for msg :status #{:error :done}
                                        :message (.getMessage e))))))

(defn test-one-var [test-ns test-name]
  (let [v (ns-resolve (symbol test-ns) (symbol test-name))
        m (meta (:ns (meta v)))
        once-fixture-fn (test/join-fixtures (:clojure.test/once-fixtures m))
        each-fixture-fn (test/join-fixtures (:clojure.test/each-fixtures m))]
    (once-fixture-fn
     (fn []
       (when (:test (meta v))
         (each-fixture-fn (fn [] (test/test-var v))))))))

(defn test-one [ns test-name]
  (binding [test/*report-counters* (ref test/*initial-report-counters*)]
    (let [ns-obj (the-ns (symbol ns))]
      (test/do-report {:type :begin-test-ns, :ns ns-obj})
      ;; If the namespace has a test-ns-hook function, call that:
      (if-let [v (find-var (symbol (str (ns-name ns-obj)) "test-ns-hook"))]
        ((var-get v))
        ;; Otherwise, just test every var in the namespace.
        (test-one-var ns test-name))
      (test/do-report {:type :end-test-ns, :ns ns-obj}))
    (test/do-report (assoc @test/*report-counters* :type :summary))
    @test/*report-counters*))

(defn ^{:nrepl/op {:name "run-test"
                   ;; TODO: need to get the list of deftests at runtime; eep
                   :args [["test" "var" "Test: "]]
                   :doc "Run a single test."}}
  run-test
  [{:keys [transport test ns] :as msg}]
  (try
    (t/send transport (m/response-for msg :clear-overlays "true"))
    (let [summary (with-redefs [test/report (partial report test/report)]
                    (test-one ns test))]
      (summary-response msg ns summary [(meta (resolve (symbol test)))]))
    (catch Exception e
      (t/send transport (m/response-for msg :status #{:error :done}
                                        :message (.getMessage e))))))

;; TODO: next/prev failure? (might need hard-coding)

(defn ^{:nrepl/op {:name "ds-doc"
                   :args [["var" "var" "Doc: "]]
                   :doc "Display the doc of the given var."}}
  ds-doc
  [{:keys [transport var] :as msg}]
  (try
    (when-let [response (with-out-str (repl/doc var))]
      (t/send transport (m/response-for msg
                                        :status :done
                                        :text (str response))))
    (catch Exception e
      (t/send transport (m/response-for msg
                                        :statue #{:error :done}
                                        :message (.getMessage e))))))

(defn ^{:nrepl/op {:name "ds-javadoc"
                   :args [["symbol-name" "string" "Javadoc:"]]
                   :doc "Display the javadoc of the given var."}}
  ds-javadoc
  [{:keys [transport symbol-name ns] :as msg}]
  (try
    (let [class-fq-name (ns-resolve (find-ns (symbol ns))
                                    (symbol symbol-name))
          url (#'javadoc/javadoc-url (.getName class-fq-name))]
      (when url
        (t/send transport (m/response-for msg
                                          :status :done
                                          :url url))))
    (catch Exception e
      (t/send transport (m/response-for msg
                                        :status #{:error :done}
                                        :message (.getMessage e))))))

(defn ^{:nrepl/op {:name "ds-src"
                   :args [["symbol-name" "string" "Symbol:"]]
                   :doc "Display the source of the given symbol."}}
  ds-src
  [{:keys [transport symbol-name ns] :as msg}]
  (try
    (let [sym (ns-resolve (find-ns (symbol ns))
                                     (symbol symbol-name))]
      (t/send transport (m/response-for msg
                                       :status :done
                                       :text (clojure.repl/source sym))))
    (catch Exception e
      (t/send transport (m/response-for msg
                                        :status #{:error :done}
                                        :message (.getMessage e))))))

(defn ^{:nrepl/op {:name "ds-locate"
                   :args [["var-name" "var" "Var:"]]
                   :doc "Locate of the source of the var."}}
  ds-locate
  [{:keys [transport var ns] :as msg}]
  (clojure.pprint/pprint msg)
  (try
    (let [ns-symbol    (symbol ns)
          ns-var       (symbol var)
          ns-file      (comp :file meta second first ns-publics)
          resource-str (comp str io/resource ns-file)
          file-str     (comp str io/file ns-file)]
      (cond ((ns-aliases ns-symbol) ns-var)
            (let [resolved-ns ((ns-aliases ns-symbol) ns-var)]
              (println "first: " resolved-ns)
              (t/send transport
                      (m/response-for msg
                                      :status :done
                                      :position (str
                                                 (file-str resolved-ns)
                                                 ":1"))))
            (find-ns ns-var)
            (do
              (println "second: " ns-var)
              (t/send transport
                      (m/response-for msg
                                     :status :done
                                     :position (str
                                                (file-str file-str ns-var)
                                                ":1"))))

            (ns-resolve ns-symbol ns-var)
            (let [position (apply str
                                  ((juxt
                                    (comp str io/file :file)
                                    (fn [_] ":")
                                    :line)
                                   (meta (ns-resolve ns-symbol ns-var))))]
              (println "third. ")
              (t/send transport
                      (m/response-for msg
                                      :status :done
                                      :position position)))))))
