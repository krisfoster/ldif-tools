(ns ldif.core
  (:gen-class)
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string])
  (:import (java.io File)
           (com.unboundid.ldif LDIFWriter)
           (com.unboundid.ldap.sdk Entry)))

;;;
;;; Definitions
;;;

(def obj-class "objectclass")
(def obj-class-top "top")
(def obj-class-group-unique-names "groupOfUniqueNames")
(def obj-class-orcl-group "orclGroup")
(def obj-class-group-of-urls "groupOfURLs")
(def obj-description "description")
(def obj-cn "cn")
(def obj-display-name "displayname")
(def obj-orcl-norm-dn "orclnormdn")
(def group-comment-sym "#")

;;;
;;; Implementation logic
;;;

(defn create-entry [schema dn-base cn & {:keys [display-name description]}]
  (let [dn-full (str "cn=" cn "," dn-base)
        display-name (if display-name display-name cn)
        description (if description description cn)]
      (cond
         (= :oid schema) (let [classes (list obj-class-top obj-class-group-unique-names obj-class-orcl-group)]
                             (doto (new Entry dn-full)
                               (.setAttribute obj-class classes)
                               (.setAttribute obj-description description)
                               (.setAttribute obj-cn cn)
                               (.setAttribute obj-display-name display-name)))
         (= :embedded schema) (let [classes (list obj-class-top obj-class-group-unique-names obj-class-group-of-urls)]
                                (doto (new Entry dn-full)
                                  (.setAttribute obj-class classes)
                                  (.setAttribute obj-description description)
                                  (.setAttribute obj-cn cn)))
         :else (throw (Exception. (apply str "An unknown schema was specified:: " schema))))))

(defn write-ldif [stream entries]
  (let [writer (new LDIFWriter stream)]
    (doall
       (for [entry entries]
          (.writeEntry writer entry)))
    (.close writer)))

(defn write-groups-ldif [schema groups base-dn file]
  (write-ldif (new File file)
              (for [row (map #(list base-dn %) groups)]
                 (create-entry schema (first row) (last row)))))

(use 'clojure.java.io)


(defn read-groups-file [file]
  ;; read in the groups and ignore lines that are commented out
  (filter #(not (.startsWith group-comment-sym (string/trim %)))
    (with-open [rdr (reader file)]
        (doall (line-seq rdr)))))

;;;
;;; Command line integration and main method
;;;

;;;
;;; Command line arguments that are accepted
;;;
(def cli-options
  [["-f" "--file FILE" "File Path to file containing groups"]
   ["-of" "--output-file OUTFILE" "File Path to output ldif to"]
   ["-dn" "--base-dn BASEDN" "Base DN to apply to groups to be created"]
   ["-e" "--embedded-ldap" "Use this to change the schema to the one used by the embedded ldap server within WLS"]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Generates OID groups LDiF import file from a list of group name"
        ""
        "Usage: program-name [options]"
        ""
        "Options:"
        options-summary]
       (string/join \newline)))

(defn exit [status msg]
  (println msg)
  (System/exit status))


;;;
;;; Main method
;;;
(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
     (:help options) (exit 0 (usage summary))
     (nil? (:file options)) (exit 1 (usage summary))
     (nil? (:output-file options)) (exit 1 (usage summary))
     (nil? (:base-dn options)) (exit 1 (usage summary)))
    (let [file (:file options)
          groups (read-groups-file file)
          out-file (:output-file options)
          base-dn (:base-dn options)
          schema (if (:embedded-ldap options) :embedded :oid)]
      (println "Generating LDIF output with following params....")
      (println "input file :: " file)
      (println "output file:: " (:output-file options))
      (println "base dn    :: " (:base-dn options))
      (write-groups-ldif schema groups base-dn out-file)
      (println "Output file generated."))))


