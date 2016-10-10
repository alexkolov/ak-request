(ns ak-request.core
  (:require
   [ak-dbg.core :refer :all]
   [cheshire.core :as json]
   [clj-http.client :as http]
   [clojure.spec :as sp]
   [clojure.spec.test :as st]))

(sp/def ::min-request (sp/and (sp/keys :req-un [::method ::url]
                                       :opt-un [])

                              #(if (= (:method %) :post)
                                 (contains? % :body)
                                 true)))

(defn protocol-ensure [url]
  (as-> (.toLowerCase url) X
    (if (and (not (.startsWith X "http://"))
             (not (.startsWith X "https://")))
      (str "http://" X)
      url)))

(defn call! [{:keys [url method] :as opts}]
  {:pre [(sp/valid? ::min-request opts)]}

  (let [predefined (if (= method :get)
                     {:throw-exceptions false}
                     {:content-type :json :throw-exceptions false})
        url* (protocol-ensure url)
        opts* (-> (merge predefined opts)
                  (assoc :url url*))
        resp (http/request opts*)
        status (:status resp)
        body (if (= method :get)
               (:body resp)
               (json/parse-string (:body resp) true))]

    {:stat :ok :body body :status status}))

(sp/def ::min-pre-request (sp/keys :req-un [::url]))

(defn get-call! [{:keys [] :as opts}]
  {:pre [(sp/valid? ::min-pre-request opts)]}

  (let [opts* (assoc opts :method :get)]
    (call! opts*)))

(sp/def ::valid-body (fn [{:keys [body] :as m}]
                       (or (nil? body) (string? body) (map? body))))

(defn post-call! [{:keys [body] :as opts}]
  {:pre [(sp/valid? ::min-pre-request opts)
         (sp/valid? ::valid-body opts)]}

  (let [body* (if (string? (:body opts))
                (:body opts)
                (json/generate-string (:body opts)))
        opts* (assoc opts :method :post :body body*)]
    (call! opts*)))
