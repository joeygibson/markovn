(ns markovn.core
  (:require [clojure.string :as string]))

(defn get-corpus [file-name]
  (string/split (slurp file-name) #"\s+"))

(defn create-chains [corpus prefix-length]
  (loop [remaining-corpus (drop prefix-length corpus)
         words (take prefix-length corpus)
         chains {}]
    (if (nil? remaining-corpus)                             ;; This may drop the last couple of words.
      chains
      (let [chain (get chains words)
            next-word (first remaining-corpus)]
        (recur (next remaining-corpus)
               (conj (drop 1 words) next-word)
               (assoc chains words (conj chain next-word)))))))

(defn generate [chains number-of-words]
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
    (println (generate chains max-words))))