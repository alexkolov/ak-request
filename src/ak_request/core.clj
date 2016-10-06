(ns ak-request.core
  (:require
   [cheshire.core :as json]
   [clj-http.client :as http]
   [clojure.spec :as sp]
   [clojure.spec.test :as st]
   [dbg.core :refer :all]))

(sp/def ::minimal-request (sp/and
                           (sp/keys :req-un [::method ::url]
                                    :opt-un [])
                           #(if (= (:method %) :post)
                              (contains? % :body)
                              true)))

(defn call! [{:keys [method] :as opts}]
  {:pre [(sp/valid? ::minimal-request opts)]}

  (let [predefined (if (= method :get)
                     {:throw-exceptions false}
                     {:content-type :json :throw-exceptions false})
        opts* (merge predefined opts)
        resp (http/request opts*)
        status (:status resp)
        body (if (= method :get)
               (:body resp)
               (json/parse-string (:body resp) true))]

    {:stat :ok :body body :status status}))

(sp/def ::min-pre-request (sp/keys :req-un [::url]))

(defn call-get! [{:keys [] :as opts}]
  {:pre [(sp/valid? ::min-pre-request opts)]}

  (let [opts* (assoc opts :method :get)]
    (call! opts*)))

(sp/def ::valid-body (fn [{:keys [body] :as m}]
                       (or (nil? body) (string? body) (map? body))))

(defn call-post! [{:keys [body] :as opts}]
  {:pre [(sp/valid? ::min-pre-request opts)
         (sp/valid? ::valid-body opts)]}

  (let [body* (if (string? (:body opts))
                (:body opts)
                (json/generate-string (:body opts)))
        opts* (assoc opts :method :post :body body*)]
    (call! opts*)))
