(ns markovn.core
  (:require [clojure.string :as string]))

(defn get-corpus
  "Get the contents of the specified file, splitting it on whitespace,
  and return it as a seq."
  [file-name]
  (string/split (slurp file-name) #"\s+"))

(defn create-chains
  "Create a series of Markov Chains for the given collection of words. The keys
  are of prefix-length."
  [corpus prefix-length]
  (loop [remaining-corpus (drop prefix-length corpus)
         words (take prefix-length corpus)
         chains {}]
    (if (nil? remaining-corpus)
      chains
      (let [chain (chains words)
            next-word (first remaining-corpus)]
        (recur (next remaining-corpus)
               (conj (drop 1 words) next-word)
               (assoc chains words (conj chain next-word)))))))

(defn generate
  "Generate a new series of words of number-of-words length, given the supplied
  Markov Chains."
  [chains number-of-words]
  (let [map-keys (keys chains)
        random-key (rand-int (count map-keys))
        starting-words (vec (nth map-keys random-key))]
    (loop [i 0
           current-words starting-words
           words starting-words]
      (if (= i number-of-words)
        words
        (let [chain (chains current-words)
              next-word (nth chain (rand-int (count chain)))]
          (recur (inc i)
                 (conj (rest current-words) next-word)
                 (conj words next-word)))))))

(defn format-text
  "Join the words in the seq to create a string."
  [text]
  (string/join " " text))

(defn -main [& args]
  (when-not (= (count args) 3)
    (println "Usage: markov <file-name> <prefix-len> <max-words>")
    (System/exit 1))
  (let [source-file (first args)
        prefix-length (Integer/parseInt (second args))
        max-words (Integer/parseInt (nth args 2))
        corpus (get-corpus source-file)
        chains (create-chains corpus prefix-length)]
    (println (format "Words: %d; Chains: %d", (count corpus) (count chains)))
    (println (format-text (generate chains max-words)))))