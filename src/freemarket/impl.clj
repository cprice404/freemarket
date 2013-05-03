(ns freemarket.impl
  (:import  [java.util.concurrent ArrayBlockingQueue LinkedBlockingQueue]))

;; TODO docs
(def work-queue-sentinel-complete (Object.))

(defn iterator-fn->lazy-seq
  ;; TODO docs
  [f]
  {:pre  [(fn? f)]
   :post [(seq? %)]}
  (lazy-seq
    (let [next-item (f)]
      (if (nil? next-item)
        '()
        (cons next-item (iterator-fn->lazy-seq f))))))

(defn swap-and-return-old-val!
  ;; TODO: docs
  [a f & args]
  (loop []
    (let [old-value @a
          new-value (apply f old-value args)]
      (if (compare-and-set! a old-value new-value)
        old-value
        (recur)))))

(defn build-worker
  ;; TODO docs
  [work-seq-fn enqueue-fn]
  (future
    (let [work-count (atom 0)]
      (doseq [work (work-seq-fn)]
        (enqueue-fn work)
        (swap! work-count inc))
      @work-count)))

(gen-interface
  :name    freemarket.impl.workqueue
  :methods [[take []       Object]
            [put  [Object] void]])

(deftype WorkQueue [queue]
  freemarket.impl.workqueue
  (put [this item]
    (if (nil? item)
      (throw (IllegalArgumentException. "Error!  Queues do not support `nil`.")))
    (.put queue item))
  (take [this]
    (.take queue)))

(defn work-queue
  ;; TODO docs
  ([] (work-queue 0))
  ([size]
    {:pre  [(>= size 0)]
     :post [(instance? WorkQueue %)]}
    (if (= 0 size)
      (WorkQueue. (LinkedBlockingQueue.))
      (WorkQueue. (ArrayBlockingQueue. size)))))

(defn validate-consumer-work
  [result]
  (if (nil? result)
    (throw (IllegalStateException. "Consumer work function must not return `nil`.")))
  result)

