(ns ak-request.template
  (:require
   [ak-dbg.core :refer :all]
   [ak-request.core :as core]
   [clojure.spec :as sp]
   [clojure.spec.test :as st]))

(sp/def ::min-remote-req (sp/and (sp/keys :req-un [::params ::request-method])

                                 #(or (= (:request-method %) :get)
                                      (= (:request-method %) :post))))

(sp/def ::min-remote-params (sp/and (sp/keys :req-un [::lang ::type ::theme ::ssl ::url ::port])

                                    #(->> (select-keys % [:lang :type :theme :ssl :url :port])
                                          (vals)
                                          (every? string?))))

(defn remote! [{:keys [params request-method] :as req}]
  {:pre [(sp/valid? ::min-remote-req req)
         (sp/valid? ::min-remote-params params)]}

  (let [{:keys [lang type theme ssl url port]} params
        post? (or (= request-method :post) false)
        scheme (if (= ssl "true") "https://" "http://")
        url (if post?
             (str scheme
                  url ":"
                  port
                  "/template")
              (str scheme
                   url ":"
                   port
                   "/template"
                   "?type=" type
                   "&theme=" theme
                   "&lang=" lang))
        opts {:method request-method
              :url url
              :params params}
        resp (core/call! opts)
        status (:status resp)
        body (:body resp)]

    (if (= status 200)
      {:stat :ok :res body}
      {:stat :err})))
