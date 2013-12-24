(ns sparky.core
  (:require [org.httpkit.client :as http]
            [clojure.xml :as xml])
  (:use clojure.pprint) ; used for dev only
  (:import
   java.io.ByteArrayInputStream
   java.net.URLEncoder
   java.util.Calendar
   java.util.TimeZone
   java.text.SimpleDateFormat
   javax.crypto.spec.SecretKeySpec
   javax.crypto.Mac
   org.apache.commons.codec.binary.Base64)
  (:gen-class))

(def UTF8_CHARSET  "UTF-8")
(def HMAC_SHA256_ALGORITHM "HmacSHA256")

(defn parse-XML
  [s]
  (xml/parse (ByteArrayInputStream. (.getBytes s))))

(defn encodeRfc3986
  "Encode a string to RFC3986."
  [s]
  (-> s
      (URLEncoder/encode)
      (.replace "+" "%20")
      (.replace "*" "%2A")
      (.replace "~" "%7E")
      (.replace "," "%2C")
      (.replace ":" "%3A")))

(defn qmap->qstring
  "Transform map to encoded URI string"
  [uri-map]
  (apply str
         (interpose
          "&"
          (for [[k v] (sort uri-map)] (str
                                       (encodeRfc3986 (name k))
                                       "="
                                       (encodeRfc3986 v))))))

(defn get-timestamp
  "Get formatted timestamp for current time"
  []
  (let [cal (Calendar/getInstance)
        dfm (new SimpleDateFormat "yyyy-MM-dd'T'HH:mm:ss'Z'")
        void (.setTimeZone dfm (TimeZone/getTimeZone  "GMT"))]
    (.format dfm (.getTime cal))))

(defn sign
  "Create the signature for the request string"
  [string-to-sign secret]
  (let [secretKeyBytes (.getBytes secret UTF8_CHARSET)
        secretKeySpec (new SecretKeySpec secretKeyBytes HMAC_SHA256_ALGORITHM)
        mac (Mac/getInstance HMAC_SHA256_ALGORITHM)
        void (.init mac secretKeySpec)
        data (.getBytes string-to-sign UTF8_CHARSET)
        rawHmac (.doFinal mac data)
        encoder (new Base64)]
    (encodeRfc3986 (new String (.encode encoder rawHmac)))))

(defn build-request ; TODO modularize request process by api, merchant
  "Create an encoded and signed URL request for the MWS API"
  [domain
   marketplace-id
   access-key
   secret-key
   seller-id
   params]
  (let [http-verb "POST"
        host-header domain ;TODO duplicate var: clean up
        request-uri "/Orders/2011-01-01"
        query-string (qmap->qstring (conj params
                                          {:AWSAccessKeyId access-key
                                           :SellerId seller-id
                                           :SignatureMethod "HmacSHA256"
                                           :SignatureVersion "2"
                                           :Timestamp (get-timestamp)
                                           :Version "2011-01-01"}))
        signature (sign (str http-verb "\n"
                             host-header "\n"
                             request-uri "\n"
                             query-string)
                        secret-key)
        request-string (str "https://"
                            host-header
                            request-uri
                            "?"
                            query-string
                            "&Signature="
                            signature)]
    request-string))

(defn submit-request
  []
  (let [domain "mws.amazonservices.com"
        marketplace-id "ATVPDKIKX0DER"
        access-key "AKIAIZUW23CYRGDJ3CHQ"
        secret-key "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"
        seller-id "A24TT5ZXHOK2T8"]
    @(http/post (build-request ; synchronous, requires deref
                domain
                marketplace-id
                access-key
                secret-key
                seller-id
                {:Action "GetServiceStatus"})
               {:user-agent "Sparky/0.1 (Testing; Clojure)"})))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
