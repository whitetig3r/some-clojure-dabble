(require '[clojure.string :as str])

(def MENU-STRING "
*** Sales Menu ***
------------------

1. Display Customer Table
2. Display Product Table
3. Display Sales Table
4. Total Sales for Customer
5. Total Count for Product
6. Exit

Enter an option?"
)

(defn tuple-splitter[item]
    (str/split item #"\|")
)

(def SALE-SLURP
    (map tuple-splitter (str/split-lines(slurp "sales.txt")))
)

(def CUSTOMER-SLURP
    (map tuple-splitter (str/split-lines(slurp "cust.txt")))
)

(def PRODUCT-SLURP
    (map tuple-splitter (str/split-lines(slurp "prod.txt")))
)

(defn generic-splitter [slurp-to-use]
    (apply merge 
        (map
            (fn [item] 
                (sorted-map (Long. (first item)) (vec (rest item)) )
            )
            slurp-to-use
        )
    )
)

(def CUSTOMER-HASH
    (generic-splitter CUSTOMER-SLURP)
)

(def PRODUCT-HASH
    (generic-splitter PRODUCT-SLURP)
)

(defn transform-sales-props[cust-id, prod-id, item-count, index-for-prod-hash]
    (conj [] 
        (get (get CUSTOMER-HASH (Long. cust-id)) 0) 
        (get (get PRODUCT-HASH (Long. prod-id)) index-for-prod-hash) 
        item-count 
    )
)

(defn sales-splitter [index-for-prod-hash]
    (apply merge 
        (map
            (fn [item] 
                (sorted-map 
                    (Long. (first item)) 
                    (apply 
                        transform-sales-props 
                        (concat 
                            (rest item) 
                            (conj [] index-for-prod-hash) 
                        ) 
                    )
                )
            )
            SALE-SLURP
        )
    )
)

(def SALE-HASH-PRICE
    (sales-splitter 1)
)

(def SALE-HASH-NAME
    (sales-splitter 0)
)

(defn print-table [hash-to-print]
    (println
        (str/join "\n"
            (map
                (fn [[id, props]] (str/join " : " [id props]))
                hash-to-print
            )
        )   
    )   
)

(defn get-filtered-entity [entity-name, hash-to-filter, index-to-extract]
    (filter 
        (fn [sales-props-list] 
            (= (get sales-props-list index-to-extract) entity-name)
        )
        (vals hash-to-filter)
    )
)

(defn calculate-customer-total [customer-name]
    (reduce +
        (map
            (fn [prop] 
                (* 
                    (Float. (get prop 1)) 
                    (Float. (get prop 2)) 
                ) 
            )
            (get-filtered-entity customer-name SALE-HASH-PRICE 0)
        )
    )
)

(defn get-customer-total [customer-name]
    (str
        customer-name
        ": $ "
        (let [total (calculate-customer-total customer-name)]
            (case total
                0 "0.00"
                (format "%.2f" total)
            )
        )
    )
)   

(defn get-input [prompt]
    (println prompt) 
    (let [ inp (read-line) ]
        inp
    )
)

(defn get-item-total [item]
    (str
        item
        ": "
        (reduce +
            (map
                (fn [prop] (read-string (get prop 2)) )
                (get-filtered-entity item SALE-HASH-NAME 1)
            )
        )
    )
)

(defn MenuDriver [recursive-nil]
    (println MENU-STRING)
    (let [selected-option (read-line)]
        (case selected-option 
            "1" (-> (print-table CUSTOMER-HASH) (MenuDriver))
            "2" (-> (print-table PRODUCT-HASH) (MenuDriver))
            "3" (-> (print-table SALE-HASH-NAME) (MenuDriver))
            "4" (-> (get-input "Enter customer name:") (get-customer-total) (println) (MenuDriver))
            "5" (-> (get-input "Enter item name:") (get-item-total) (println) (MenuDriver))
            "6" (println "GoodBye!")
            (-> (println "Invalid option selected.") (MenuDriver))
        )
    )
)

(-> (print) (MenuDriver))