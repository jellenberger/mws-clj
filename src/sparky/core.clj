(ns sparky.core
  (:require [clj-http.client :as http])
  (:import
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

(defn encodeRfc3986
  "Encode a string to RFC3986."
  [value]
  (-> value
      (URLEncoder/encode)
      (.replace "+" "%20")
      (.replace "*" "%2A")
      (.replace "~" "%7E")
      (.replace "," "%2C")
      (.replace ":" "%3A")))

(defn encode-signature
  "Change the signature to encode plus and equal signs"
  [value]
    (-> value
      (.replace "+" "%2B")
      (.replace "=" "%3D")))

(defn map-to-uri
  "Transform map to encoded URI string"
  ([uri-map]
     (apply str
            (interpose
             "&"
             (for [[k v] (sort uri-map)] (str
                                          (encodeRfc3986 (name k))
                                          "="
                                          (encodeRfc3986 v)))))))

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
    (new String (.encode encoder rawHmac))))

(defn make-request
  "Create an encoded and signed URL request for the MWS API"
  [domain access-key secret assoc-tag params]
  (let [uri (map-to-uri (conj params {:Service "AWSECommerceService"
                                      :AWSAccessKeyId access-key
                                      :AssociateTag assoc-tag
                                      :Timestamp (get-timestamp)}))
        signature (encode-signature
                   (sign
                    (str "GET\n" domain "/\n" uri) secret))]
    (str "http://" domain "/?" uri "&Signature=" signature)))

(defn print-request
  []
  (let [domain "mws.amazonservices.com"
        access-key "enter-your-amazon-access-key"
        secret-key "enter-your-amazon-secret-key"
        associate-id "enter-your-amazon-associate-key"]
    ;; (http/get
    (println (make-request
                 domain
                 access-key
                 secret-key
                 associate-id
                 {:Operation "ItemLookup", :ItemId "0679722769"}))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
