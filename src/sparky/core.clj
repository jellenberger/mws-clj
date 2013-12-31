(ns sparky.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [org.httpkit.client :as http]
            [clojure.xml :as xml])
  (:use clojure.pprint) ; used for dev only
  (:import
   java.io.ByteArrayInputStream
   java.net.URLEncoder
   java.text.SimpleDateFormat
   java.util.Calendar
   java.util.TimeZone
   javax.crypto.spec.SecretKeySpec
   javax.crypto.Mac
   org.apache.commons.codec.binary.Base64)
  (:gen-class))

(def UTF8_CHARSET  "UTF-8")
(def HMAC_SHA256_ALGORITHM "HmacSHA256")
(def REQUEST_METHOD "POST")
(def USER_AGENT_STRING "Sparky/0.1.0 (Language=Clojure/1.5)")

(def pt-id "A2TKMPXX6CQRJI")
(def br-id "A24TT5ZXHOK2T8")


;; Utility functions

(defn parse-xml
  "Parses an XML string."
  [s] (xml/parse (ByteArrayInputStream. (.getBytes s UTF8_CHARSET))))

(defn read-text-inputstream
  "Reads a text file input stream, returning a vector of lines."
  [stream]
  (with-open [rdr (io/reader stream :encoding UTF8_CHARSET)]
    (vec (line-seq rdr))))

(defn encodeRfc3986
  "Encodes a string to RFC3986."
  [s]
  (-> s
      (URLEncoder/encode)
      (.replace "+" "%20")
      (.replace "*" "%2A")
      (.replace "~" "%7E")
      (.replace "," "%2C")
      (.replace ":" "%3A")))

(defn qmap->qstring
  "Converts a map of query parameters to a sorted RFC3986 encoded string."
  [q-map]
  (string/join "&" (for [[k v] (sort q-map)]
                     (str (encodeRfc3986 (name k)) "=" (encodeRfc3986 v)))))

(defn gen-timestamp
  "Generates an ISO8601 timestamp."
  []
  (.format
   (doto (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss'Z'")
     (.setTimeZone (TimeZone/getTimeZone "GMT")))
   (.getTime (Calendar/getInstance))))

(defn sign
  "Generates a signature for a string, given a secret key."
  [s secret]
  (let [secretKeyBytes (.getBytes secret UTF8_CHARSET)
        secretKeySpec (new SecretKeySpec secretKeyBytes HMAC_SHA256_ALGORITHM)
        mac (doto (Mac/getInstance HMAC_SHA256_ALGORITHM)
              (.init secretKeySpec))
        sbytes (.getBytes s UTF8_CHARSET)
        rawHmac (.doFinal mac sbytes)
        encoder (new Base64)]
    (encodeRfc3986 (new String (.encode encoder rawHmac)))))


;; Generic API request functions *****************************

(defn gen-request
  [merchant-id api-params]
  (let [access-key "AKIAIZUW23CYRGDJ3CHQ"
        secret-key "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"
        marketplace-id "ATVPDKIKX0DER"
        service-host "mws.amazonservices.com"
        {:keys [request-path
                service-version
                request-params]} api-params
        query-map (conj request-params
                        {:AWSAccessKeyId access-key
                         :SellerId merchant-id
                         :SignatureMethod "HmacSHA256"
                         :SignatureVersion "2"
                         :Timestamp (gen-timestamp)
                         :Version service-version})
        query-string (qmap->qstring query-map)
        signature (sign (string/join "\n" ["GET"
                                           service-host
                                           request-path
                                           query-string])
                        secret-key)
        request-string (str "https://" service-host request-path "?"
                            query-string "&Signature=" signature)]
    request-string))

(defn submit-request
  [request-string]
  @(http/get request-string {:user-agent USER_AGENT_STRING}))


;; Report API functions *****************************

(defn reports-request
  [merchant-id params]
  (let [request-string (gen-request merchant-id
                                    {:request-path "/"
                                     :service-version "2009-01-01"
                                     :request-params params})]
    (submit-request request-string)))

(defn get-reportlist
  [merchant-id]
  (reports-request merchant-id {:Action "GetReportList"
                                :MaxCount "100"}))

(defn parse-reportlist
  [body]
  (let [xml (parse-xml body)
        report-id #(-> % :content (nth 0) :content first)
        report-type #(-> % :content (nth 1) :content first)
        report-date #(-> % :content (nth 3) :content first)]
    (for [r (xml-seq xml) :when (= (:tag r) :ReportInfo)]
      [(report-id r) (report-type r) (report-date r)])))

(defn get-report
  [merchant-id report-id]
  (reports-request merchant-id {:Action "GetReport"
                                :ReportId report-id}))


;; Product API functions **********************************

(defn products-request
  [merchant-id params]
  (let [request-string (gen-request merchant-id
                                    {:request-path "/Products/2011-10-01"
                                     :service-version "2011-10-01"
                                     :request-params params})]
    (submit-request request-string)))

(defn get-matching-product
  [merchant-id asin]
  (products-request merchant-id {:Action "GetMatchingProduct"
                                 :ASINList.ASIN.1 asin
                                 :MarketplaceId "ATVPDKIKX0DER"}))


;; Main function ********************************************

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
