(ns torai.core)

(def mvec [1 2 3 4 5 6])
(def mlist (range 0 20 2))
(def map-neh {:a "ajaib" :b "baik" :c "cekali"})

(defn cobain-deh
  ([x y z]
    (+ x y z))
  ([x y]
    (* x y))
  ([x]
    (- x))
  ([]
    "Selamat Datang di Clojure"))

(defn mutlak
  [x]
  (if (pos? x) x (- x)))

(defn apanih?
  [x]
  (if (> x 0)
    "+"
    (if (= x 0)
      "nol"
      "negateef")))

(defn tiga-kondisi
  [x]
  (cond (> x 0) "WOW"
    (= x 0) "w o w"
    (< x -5) "woow"
  :else "ga mungkin"))

(defn kecap
  [a b c]
  (let [adisk (Math/sqrt (- (* b b) (* 4 a c)))]
    [(/ (+ (- b) adisk) (* 2 a))
     (/ (- (- b) adisk) (* 2 a))]))

(defn ngasal1
  [x]
  (let [a (* x x)] (+ a x)))

(defn ngasal12
  [x]
  (let [a (* x x)]
    (let [b (+ a x)]
      (+ a b x))))

(defn kecap-baru
  [a b c x?]
  (let [adisk (Math/sqrt (- (* b b) (* 4 a c)))]
    (/ (+ (- b) (if (= x? 1) adisk (- adisk)))
       (* 2 a))))
  
(defn faktorial
  [n]
  (if (<= n 1)
    1
    (* n (faktorial (dec n)))))

(def fakt
  (fn [n]
    (if (<= n 1)
      1
      (* n (fakt (dec n))))))

(defn fakto [n]
  (let [fak (fn fak [n]
              (if (<= n 1)
                1
                (* n (fak (dec n)))))]
  (fak n)))

;(def cobaan [n]
;  (let [hajar (fn [n] (* n 2))]
;    hajar n))

(defn sum-wow
  [n]
  (if (= n ())
    0
    (+ (first n) (sum-wow (rest n)))))

(defn sum-lain
  [[x & xs]]
  (if x
  (+ x (sum-lain xs))
  0))

(defn product-wow
  [n]
  (if (= n ())
    1
    (* (first n) (product-wow (rest n)))))

(defn take-wow
  [n xs]
  (cond
    (zero? n) xs
    (empty? xs) '()
    :else (take-wow (dec n) (butlast xs))))

(defn great-drop
  [n xs]
  (loop [n n
         result []
         xs xs]
    (cond
      (zero? n) xs
      (empty? xs) '()
      :else (recur (dec n) (conj result (first xs)) (rest xs)))))

(defn great-take
  [n xs]
  (loop [n n
         result []
         xs xs]
    (cond
      (zero? n) result
      (empty? xs) result
      :else (recur (dec n) (conj result (first xs)) (rest xs)))))

; (great-take 2 [1 2 3 4]) = [1 [1] (2 3 4)]
; (great-take 1 [2 3 4]) = [0 [1 2] (3 4)]]
; (great-take 0

(defn drop-wow
  [n xs]
  (cond
    (zero? n) xs
    (empty? xs) '()
    :else (drop-wow (dec n) (rest xs))))

(defn doni-teriak
  [nama-orang]
  (str "Minggir dong, " nama-orang "!"))
  
(defn doni
  [& nama-orangs]
  (map doni-teriak nama-orangs))

(defn keren-banget-nih
  [name & things]
  (str "Hi, " name ", here are my favorite things: "
       (clojure.string/join ", " things)))

(defn my-first
  [[first-thing]]
  first-thing)

(defn chooser
  [[first-choice second-choice & unimportant-choices]]
  (println (str "Your first choice is " first-choice))
  (println (str "Your second choice is " second-choice))
  (println (str "We're ignoring the rest of your choices. "
                "Here they are in case you need to cry over them: "
                (clojure.string/join ", " unimportant-choices))))

(defn announce-treasure-location
  [{lat :lat lng :lng}]
  (println (str "Treasure lat: " lat))
  (println (str "Treasure lng: " lng)))

; Treasure lat: 28.22
; Treasure lng: 81.33

(defn foo
  [x]
  (when (> x 0)
    (conj (foo (dec x)) x)))

; foo 5 = (conj (foo 4) 5)
; foo 4 = (conj (foo 3) 4)
; foo 3 = (conj (foo 2) 3)
; foo 2 = (conj (foo 1) 2)
; foo 1 = (conj nil 1)

(defn go
  [x]
  (cond
    (<= x 0) 1
   :else (* x (go (dec x)))))

;(= (last (sort (rest (reverse [2 5 4 1 3 6]))))
;   (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (last)) 5)

;(loop [x 10]
;  (when (> x 1)
;    (println x)
;    (recur (- x 2))))

(defn joni [] 
  (loop [x 5 result []]
    (if (> x 0)
      (recur (dec x) (conj result (+ 2 x)))
      result)))

(defn cobain-ya []
  (loop [x 0
         result nil]
    (if (< x 10)
      (recur (inc x) (str result x))
      result)))

(defn fibo [n]
  (cond
    (= n 1) [1]
    (= n 2) [1 1]
    :else (flatten (conj [1 1]
                ((fn [y]
                  (loop [y (- y 2)
                         k 1
                         l 1
                         result []]
                    (if (> y 0)
                      (recur (dec y)
                             l
                             (+ k l)
                             (conj result (+ k l)))
                      result)))n)))))
                                        
;(defn koert []
;  (loop [x 10
;         k 1
;         l 1
;         result []]
;    (if (> x 0)
;      (recur (dec x)
;             l
;             (+ k l)
;             (conj result (+ k l)))
;      result)))

(defn eksponen [a b] (reduce * (replicate b a)))

(fn inc-maker [inc-by] #(+ % inc-by))

(def inc3 ((fn [inc-by] #(+ % inc-by)) 3))

(defn sum-even-numbers [nums]
  (if-let [nums (seq (filter even? nums))]
    (reduce + nums)
    "No even numbers found."))
  
;(time (sum-wow (filter even? (rest (filter #(< % 4000000) (fibo 45))))))
;"Elapsed time: 2.724075 msecs"
;4613732

(defn bagi-20 []
  (loop [pembagi 20
         start 1]
    (cond
      (= pembagi 1) start
      (zero? (rem start pembagi)) (recur start (dec pembagi))
      :else (recur (inc start) 20))))
  
; 1 = 1
; 2 = 1 2
; 3 = 1 3
; 4 = 1 2 (2 2)
; 5 = 1 5
; 6 = 1 2 3 (2 3)
; 7 = 1 7
; 8 = 1 2 (2 2) (2 2 2) = 2^3
; 9 = 1 3 (3 3) = 3^2
; 10 = 1 2 5 (2 5)
; 11 = 1 11
; 12 = 1 2 3 (2 2) (2 3) (2 2 3)
; 13 = 1 13
; 14 = 1 2 7 (2 7)
; 15 = 1 3 5 (3 5)
; 16 = 1 4 (2 2 2 2) = 2^4
; 17 = 1 17
; 18 = 1 2 3 (2 3) (3 3) (2 3 3) 
; 19 = 1 19
; 20 = 1 2 (2 2) 5 (2 5) (2 2 5)
 
; (8 9 5 7 11 13 17 19)

(defn palindrom? [n]
  (let [value (seq (str n))]
    (when (= value (reverse value))
      n)))
    
(defn project-4 []
  (loop [n 999 m 999]
    (if (palindrom? (* n m))
      [(* n m) n m]
      (recur (if (= m 100) (dec n) n) (if (= m 100) 999 (dec m))))))

(defn eul4 []
  (loop [n 900 m 900]
    (if (palindrom? (* n m))
      [(* n m) n m]
      (recur (cond
               (= (- n m) 10) (inc n)
               (= n 999) n
               :else n)
             (cond
               (= m 999) m
               :else (inc m))))))
    
(defn project-42 []
  (loop [n 100 m 100 x 0]
    (cond
      (palindrom? (* n m)) (recur (if (= n 1000) 999 (inc n))
                                  (if (= n 1000) (inc m) m)
                                  (* n m))
      (= m n 1000) x
      :else (recur (if (= n 1000) 999 (inc n))
                   (if (= n 1000) (inc m) m)
                   x))))

;(defn faktor-10nih? [n] (when (zero? (rem 10 n)) true))

;(defn factors [n]
;  (let [factor? (when (zero? (rem n %)) true)
;        test-range (range 1 (int (Math/sqrt n)))
;        final-range (into [] (filter factor? test-range))
;        generate [m] (quot n m)]
;    (flatten (reduce conj final-range (map generate final-range)))))
    
;(defn factors [n]
;  (let [factor? (when (zero? (rem n %)) true)
;        test-range (range 1 (int (Math/sqrt n)))
;        final-range (into [] (filter factor? test-range))]
;    "wow"))    
         
 (defn ratata [ns]
  (if (empty? ns)
    '()
    (conj (ratata (butlast ns)) (last ns))))
  
;(defn mesin [n]
;  (fn (quot n %)))

         
         
    
    
    
    
    
    
    
    
    
    
    












