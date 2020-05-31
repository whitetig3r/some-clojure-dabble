(require '[clojure.string :as str])

(def MENU_STRING "*** Sales Menu ***
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

(defn core-splitter[fileName]
    (apply merge 
        (map
            (fn [item] 
                (sorted-map (keyword (first item)) (into [] (rest item)))
            )
            (map tuple-splitter (str/split-lines(slurp fileName)))
        )
    )
)

(def CUSTOMERS_HASH
    (core-splitter "cust.txt")
)

(def PRODUCTS_HASH
    (core-splitter "prod.txt")
)

(defn transform-sales-props[custID, prodID, itemCount]
    (conj [] (get(get CUSTOMERS_HASH (keyword custID)) 0) (get (get PRODUCTS_HASH (keyword prodID)) 1) itemCount )
)

(defn transform-sales-props-name[custID, prodID, itemCount]
    (conj [] (get(get CUSTOMERS_HASH (keyword custID)) 0) (get (get PRODUCTS_HASH (keyword prodID)) 0) itemCount )
)

(defn sales-splitter []
    (apply merge 
        (map
            (fn [item] 
                (sorted-map (keyword (first item)) (apply transform-sales-props (rest item)) )
            )
            (map tuple-splitter (str/split-lines(slurp "sales.txt")))
        )
    )
)

(defn modified-sales-splitter []
    (apply merge 
        (map
            (fn [item] 
                (sorted-map (keyword (first item)) (apply transform-sales-props-name (rest item)) )
            )
            (map tuple-splitter (str/split-lines(slurp "sales.txt")))
        )
    )
)

(def SALES_HASH_WITH_PRICE
    (sales-splitter)
)

(def SALES_HASH_WITH_NAME
    (modified-sales-splitter)
)

(defn print-table [hash-to-print]
    (println
        (str/join "\n"
            (map
                (fn [[id, props]] (str/join " : " [(name id) props]))
                hash-to-print
            )
        )   
    )   
)

(defn print-sales-table []
    (println
        (str/join "\n"
            (map
                (fn [[id, props]] (str/join " : " [(name id) props]))
                SALES_HASH_WITH_NAME
            )
        )   
    )   
)

(defn get-required-customer [customer-name]
    (filter 
        (fn [sales-props-list] 
            (= (get sales-props-list 0) customer-name)
        )
        (vals SALES_HASH_WITH_PRICE)
    )
)

(defn get-required-item [item-name]
    (filter 
        (fn [sales-props-list] 
            (= (get sales-props-list 1) item-name)
        )
        (vals SALES_HASH_WITH_NAME)
    )
)

(defn get-customer-total [customer-name]
    (str
        customer-name
        ": $"
        (reduce +
            (map
                (fn [prop] (* (read-string(get prop 1)) (read-string (get prop 2)) ) )
                (get-required-customer customer-name)
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
        "1" (do (print-table CUSTOMERS_HASH) (MenuDriver))
        "2" (do (print-table PRODUCTS_HASH) (MenuDriver))
        "3" (do (print-sales-table) (MenuDriver))
        "4" (do (-> (get-customer-name) (get-customer-total) (println)) (MenuDriver))
        "5" (do (-> (get-item-name) (get-item-total) (println)) (MenuDriver))
        "6" (println "GoodBye!")
        (do (println "Invalid option selected.") (MenuDriver))
        )
    )
)

(MenuDriver)