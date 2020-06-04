(require '[clojure.string :as str])

(def MENU_STRING "
*** Sales Menu ***
------------------

1. Display Customer Table
2. Display Product Table
3. Display Sales Table
4. Total Sales for Customer
5. Total Count for Product
6. Exit

Enter an option?
")

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
                (sorted-map (Long. (first item)) (vec(rest item)) )
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

(defn transform-sales-props[custID, prodID, itemCount]
    (conj [] (get(get CUSTOMER-HASH (Long. custID)) 0) (get (get PRODUCT-HASH (Long. prodID)) 1) itemCount )
)

(defn transform-sales-props-name[custID, prodID, itemCount]
    (conj [] (get(get CUSTOMER-HASH (Long. custID)) 0) (get (get PRODUCT-HASH (Long. prodID)) 0) itemCount )
)

(defn sales-splitter []
    (apply merge 
        (map
            (fn [item] 
                (sorted-map (Long. (first item)) (apply transform-sales-props (rest item)) )
            )
            SALE-SLURP
        )
    )
)

(defn modified-sales-splitter []
    (apply merge 
        (map
            (fn [item] 
                (sorted-map (Long. (first item)) (apply transform-sales-props-name (rest item)) )
            )
            SALE-SLURP
        )
    )
)

(def SALE-HASH-PRICE
    (sales-splitter)
)

(def SALE-HASH-NAME
    (modified-sales-splitter)
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

(defn print-sales-table []
    (println
        (str/join "\n"
            (map
                (fn [[id, props]] (str/join " : " [id props]))
                SALE-HASH-NAME
            )
        )   
    )   
)

(defn get-required-customer [customer-name]
    (filter 
        (fn [sales-props-list] 
            (= (get sales-props-list 0) customer-name)
        )
        (vals SALE-HASH-PRICE)
    )
)

(defn get-required-item [item-name]
    (filter 
        (fn [sales-props-list] 
            (= (get sales-props-list 1) item-name)
        )
        (vals SALE-HASH-NAME)
    )
)

(defn get-customer-total [customer-name]
    (str
        customer-name
        ": $ "
        (format "%.2f"
            (reduce +
                (map
                    (fn [prop] (* (Float. (get prop 1)) (Float. (get prop 2)) ) )
                    (get-required-customer customer-name)
                )
            )
        )
    )
)   

(defn get-customer-name []
    (do (println "Enter customer name:") (read-line) )
)

(defn get-item-name []
    (do (println "Enter item name:") (read-line) )
)

(defn get-item-total [item]
    (str
        item
        ": "
        (reduce +
            (map
                (fn [prop] (read-string (get prop 2)) )
                (get-required-item item)
            )
        )
    )
)

(defn MenuDriver []
    (println MENU_STRING)
    (let [selectedOption (read-line)]
        (case selectedOption 
        "1" (do (print-table CUSTOMER-HASH) (MenuDriver))
        "2" (do (print-table PRODUCT-HASH) (MenuDriver))
        "3" (do (print-sales-table) (MenuDriver))
        "4" (do (-> (get-customer-name) (get-customer-total) (println)) (MenuDriver))
        "5" (do (-> (get-item-name) (get-item-total) (println)) (MenuDriver))
        "6" (println "GoodBye!")
        (do (println "Invalid option selected.") (MenuDriver))
        )
    )
)

(MenuDriver)