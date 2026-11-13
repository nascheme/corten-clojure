;;;; Richards (Martin Richards' task-scheduling simulation), translated
;;;; from the idiomatic Grail version
;;;; (grail-compat/benchmarks/richards/idiomatic.py).
;;;;
;;;; Six tasks (idle, worker, two handlers, two devices) pass packets
;;;; under a priority scheduler; the result is the hold and queue
;;;; counters, which must be 9297 and 23246.  Packets and per-task data
;;;; are records; each task kind's behaviour is a pure `step` (a protocol
;;;; method, the Python generic function) returning [data action].  An
;;;; action is a tagged vector ([:wait], [:hold], [:release target],
;;;; [:send packet]) interpreted with `case` (the Python `match`).  The
;;;; scheduler state is a record holding a persistent vector of Task
;;;; records; `scheduler-step` is a pure transition between snapshots.
;;;; Packet queues are vectors.  One iteration, single-threaded, as the
;;;; Python default.
;;;;
;;;; Run: corten --image clj.img --load bench/grail/richards.cljc

(ns bench.richards)

;; Task ids, in creation order; the scheduler's task list is in reverse
;; creation order.  Slot 0 is unused (0 plays the role of None).
(def i-idle 1)
(def i-work 2)
(def i-handler-a 3)
(def i-handler-b 4)
(def i-dev-a 5)
(def i-dev-b 6)
(def priority [0 0 1000 2000 3000 4000 5000])
(def next-task [0 0 1 2 3 4 5])

(def k-dev 1000)
(def k-work 1001)
(def bufsize 4)
(def letter-a 65)

(def expected-hold 9297)
(def expected-qpkt 23246)

;;; ---------------------------------------------------------------- data

;; ident: destination while queued for sending, sender once delivered.
;; datum: work packet's next index, or a device packet's letter.
(defrecord Packet [ident kind datum data])

(def empty-data (vec (repeat bufsize 0)))

(defprotocol TaskBehaviour
  (step [data packet]
    "One activation of a task: (task data, packet or nil) -> [data action]."))

(defrecord IdleData [control count]
  TaskBehaviour
  (step [d _]
    (let [count (dec count)]
      (cond
        (zero? count) [(IdleData. control count) [:hold]]
        (zero? (bit-and control 1))
        [(IdleData. (bit-shift-right control 1) count) [:release i-dev-a]]
        :else
        [(IdleData. (bit-xor (bit-shift-right control 1) 0xD008) count)
         [:release i-dev-b]]))))

(defrecord WorkerData [destination count]
  TaskBehaviour
  (step [w packet]
    (if (nil? packet)
      [w [:wait]]
      (let [dest (if (= destination i-handler-a) i-handler-b i-handler-a)
            counts (rest (iterate #(if (< % 26) (inc %) 1) count))
            letters (mapv #(+ letter-a % -1) (take bufsize counts))]
        [(WorkerData. dest (nth counts (dec bufsize)))
         [:send (->Packet dest (:kind packet) 0 letters)]]))))

(defrecord HandlerData [work-in device-in]
  TaskBehaviour
  (step [_ packet]
    (let [[work-in device-in] (cond
                                (nil? packet) [work-in device-in]
                                (= (:kind packet) k-work) [(conj work-in packet) device-in]
                                :else [work-in (conj device-in packet)])]
      (if (empty? work-in)
        [(HandlerData. work-in device-in) [:wait]]
        (let [work (work-in 0)
              i (:datum work)]
          (cond
            (>= i bufsize)              ; work packet used up: return it
            [(HandlerData. (subvec work-in 1) device-in) [:send work]]
            (empty? device-in)
            [(HandlerData. work-in device-in) [:wait]]
            :else
            (let [dev (device-in 0)]
              [(HandlerData. (assoc work-in 0 (assoc work :datum (inc i)))
                              (subvec device-in 1))
               [:send (assoc dev :datum ((:data work) i))]])))))))

(defrecord DeviceData [pending]
  TaskBehaviour
  (step [d packet]
    (cond
      packet [(DeviceData. packet) [:hold]]
      (nil? pending) [d [:wait]]
      :else [(DeviceData. nil) [:send pending]])))

(defrecord Result [hold-count qpkt-count])

;;; ----------------------------------------------------------- scheduler

;; queue: input packets, oldest first.  waiting: waiting for a packet.
;; holding: suspended until released.
(defrecord Task [data queue waiting holding])

;; tasks by id (slot 0 unused); current: task to consider next, 0 = done.
(defrecord SchedulerState [tasks current hold-count qpkt-count])

(defn initial-state
  "The tasks and packets as the original's Richards.run sets them up."
  []
  (let [work (->Packet 0 k-work 0 empty-data)
        dev-a (->Packet i-dev-a k-dev 0 empty-data)
        dev-b (->Packet i-dev-b k-dev 0 empty-data)]
    (->SchedulerState
     [nil
      (->Task (->IdleData 1 10000) [] false false)   ; idle starts running
      (->Task (->WorkerData i-handler-a 0) [work work] true false)
      (->Task (->HandlerData [] []) [dev-a dev-a dev-a] true false)
      (->Task (->HandlerData [] []) [dev-b dev-b dev-b] true false)
      (->Task (->DeviceData nil) [] true false)
      (->Task (->DeviceData nil) [] true false)]
     i-dev-b 0 0)))                                   ; DEVB heads the list

(defn- runnable? [task]
  (not (or (:holding task) (and (:waiting task) (empty? (:queue task))))))

(defn scheduler-step
  "The scheduler after its next task activation (or finished)."
  [state]
  (let [tasks (:tasks state)
        t (loop [t (:current state)]          ; skip tasks that cannot run
            (cond (runnable? (tasks t)) t
                  (zero? (next-task t)) 0
                  :else (recur (next-task t))))]
    (if (zero? t)
      (assoc state :current 0)
      (let [task (tasks t)
            take? (and (:waiting task) (seq (:queue task)))
            packet (when take? ((:queue task) 0))
            me (if take?
                 (assoc task :queue (subvec (:queue task) 1) :waiting false)
                 task)
            [data action] (step (:data task) packet)
            me (assoc me :data data)
            tasks (assoc tasks t me)]
        (case (action 0)
          :wait
          (assoc state :tasks (assoc-in tasks [t :waiting] true) :current t)
          :hold
          (assoc state
                 :tasks (assoc-in tasks [t :holding] true)
                 :hold-count (inc (:hold-count state))
                 :current (next-task t))
          :release
          (let [target (action 1)]
            (assoc state
                   :tasks (assoc-in tasks [target :holding] false)
                   :current (if (> (priority target) (priority t)) target t)))
          :send
          (let [p (action 1)
                dest (:ident p)
                was-empty (empty? (:queue (tasks dest)))]
            (assoc state
                   ;; The receiver sees the sender's id.
                   :tasks (update-in tasks [dest :queue] conj (assoc p :ident t))
                   :qpkt-count (inc (:qpkt-count state))
                   :current (if (and was-empty (> (priority dest) (priority t)))
                              dest t))))))))

(defn schedule
  "Run the scheduler to completion; return the hold and queue counters."
  []
  (loop [state (initial-state)]
    (if (zero? (:current state))
      (->Result (:hold-count state) (:qpkt-count state))
      (recur (scheduler-step state)))))

(defn check [r]
  (and (= (:hold-count r) expected-hold) (= (:qpkt-count r) expected-qpkt)))

(defn now-ms []
  #?(:corten (/ (corten.kernel/get-internal-real-time) 1000.0)
     :clj (/ (System/nanoTime) 1e6)))

(defn -main [& {:keys [reps] :or {reps 5}}]
  (let [r (schedule)]
    (println (str "richards: hold_count=" (:hold-count r)
                  " qpkt_count=" (:qpkt-count r)
                  (if (check r) " OK" " FAILED"))))
  (dotimes [_ reps]
    (let [t0 (now-ms)]
      (schedule)
      (println "richards:" (- (now-ms) t0) "ms"))))

(apply -main (when-let [[n] (seq *command-line-args*)] [:reps (parse-long n)]))
