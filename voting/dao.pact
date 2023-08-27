(namespace "free")

(define-keyset "free.ku-admin" (read-keyset "ku-admin"))
(define-keyset "free.ku-ops" (read-keyset "ku-ops"))

(module dao GOV

    (defcap GOV ()
    (enforce-guard (keyset-ref-guard "free.ku-admin" ))
       )

    (defcap OPS()
    (enforce-guard (keyset-ref-guard "free.ku-ops")))

(defschema vote-schema
    choice:string
    count:integer
    )

  (defschema proposal
    id:string
    name:string
    description:string
    tweetId:string
    choices:[string]
    start:time
    end:time
    completed:bool 
    threshold:integer
    votes:[object:{vote-schema}]
  )

  (deftable proposals:{proposal})
  
  (defun create-proposal 
    (
        name:string
        description:string
        choices:[string]
        end:time
        threshold:integer
        )
    (let* ((id:string (hash-id name))
    (ct:time (curr-time))
    (initialVotes (map (lambda (c) {"choice": c, "count": 0}) choices))  
 )
    ;  @doc "Create a new proposal"
    ;  (with-capability (OPS)
      (insert proposals id 
        {
        "id": id,
        "name": name,
        "description": description,
        "tweetId": "",
        "choices": choices,
        "start": ct,
        "end": end,
        "completed":false,
        "threshold": threshold,
        "votes": initialVotes
      }
    ;)
    )
    )
  )

  (defun custom-map (fn list extra)
  (fold 
    (lambda (acc el) 
      (+ acc [(fn el extra)])
    ) 
    [] 
    list
  )
)

(defun update-twitter-results:bool
    (proposalId:string
     results:[object:{vote-schema}]
    )
    @doc "Update the tally based on Twitter poll results"
    (let* ((proposal (read proposals proposalId))
           (ct (curr-time))
           (end-time (at 'end proposal)))
      (enforce (>= ct end-time) "Voting period must be ended before updating results")
  
      (let ((existingVotes (at 'votes proposal)))
        ; Merge Twitter results into existing votes
        (let ((mergedVotes (fold
                            (lambda (acc ev)
                              (let ((result (fold 
                                              (lambda (a e) 
                                                (if (and (not a) (= (at "choice" e) (at "choice" ev))) 
                                                  e 
                                                  a)
                                              ) 
                                              false 
                                              results)))
                                (if result
                                  (+ acc [{"choice": (at "choice" ev), "count": (+ (at "count" ev) (at "count" result))}])
                                  (+ acc [ev])
                                )
                              )
                            ) [] existingVotes)))
          (update proposals proposalId {"votes": mergedVotes})
        )
      )
      ; After updating, validate the votes
      (validate-votes proposalId)
    )
  )
  
  
  

  (defun close-vote:bool 
    (proposalId:string)
    @doc "Close the voting for a proposal"
    (let ((proposal (read proposals proposalId)))
      (validate-votes proposalId)  ; Validate votes before closing
    )
  )

  (defun validate-votes (proposalId:string)
  @doc "Validate votes and set completion status for a proposal"
  (let* (
    (proposal (read proposals proposalId))
    (votes (at 'votes proposal))
    (threshold (at 'threshold proposal))
    (total-votes (length votes))
    (satisfied-votes (fold increment-tally {} votes))
    (max-votes (at "max" (fold find-max {"max": 0} satisfied-votes))) 
    (percentage (/ (* 100 max-votes) total-votes))
  )
    (if (>= percentage threshold)
      (update proposals proposalId { "completed": true })  ; If the threshold is met
      (update proposals proposalId { "completed": false })  ; If the threshold is not met
    )
  )
)

(defun find-max (m vote)
  (let* (
    (choice (at "choice" vote))
    (count (at "count" vote))
    (max-so-far (if (at "max" m) (at "max" m) 0))
    (max-count (if (at "count" m) (at "count" m) 0))
  )
    (if (> count max-count)
      (update m {"max": choice, "count": count})
      m
    )
  )
)

(defun custom-find (predicate-fn list)
  (fold 
    (lambda (acc el)
      (if (and (not acc) (predicate-fn el))
        el
        acc
      )
    ) 
    false list
  )
)


(defun increment-tally (m vote)
  (let* ((choice (at "choice" vote))
         (count (if (at "count" vote) (at "count" vote) 1))
         (existingCount (if (at choice m) (at choice m) 0))
         (updatedCount (+ existingCount count)))
    (if (at choice m)
      (update m choice updatedCount)
      (insert m choice updatedCount)
    )
  )
)


(defun tally-votes:object
    (proposalId:string)
    @doc "Tally votes on a proposal"
    (let* ((proposal (read proposals proposalId))
           (ct (curr-time)))
      (enforce (>= ct (at 'start proposal)) "Voting period has not started")
      (enforce (<= ct (at 'end proposal)) "Voting period has ended")
  
      (with-read proposals proposalId {"votes":= existingVotes}
        (let ((tally (fold increment-tally {} existingVotes)))
          tally
        )
      )
    )
  )

  (defun vote:bool 
    (
     proposalId:string
     choice:integer
     voter:string
    )
    @doc "Vote on a proposal"  
    (let* ((proposal (read proposals proposalId))
           (ct (curr-time)))
      (enforce (>= ct (at 'start proposal)) "Voting period has not started")
      (enforce (<= ct (at 'end proposal)) "Voting period has ended")

      (with-read proposals proposalId {"votes":= existingVotes}
        (let ((existingVote (at voter existingVotes)))
          (enforce (not existingVote) "Already voted")
        )
        
        (insert proposals proposalId {
          "votes": {"voter":voter, "choice":choice}
        })
      )
    )
  )

;    (defun update-twitter-results:bool
;      (proposalId:string
;       results:[object:{vote-schema}]
;      )
;      @doc "Update the tally based on Twitter poll results"
;      (let* ((proposal (read proposals proposalId))
;             (ct (curr-time))
;             (end-time (at 'end proposal)))
;      ;    (enforce (= (at 'tweetId proposal) "") "Twitter ID must be set before updating results")
;        (enforce (>= ct end-time) "Voting period must be ended before updating results")

;        (let* ((existingVotes (at 'votes proposal))
;               (updatedVotes (map
;                              (lambda (r)
;                                {"voter": "twitter", "choice": (at "count" r)}
;                              ) results)))
;          (update proposals proposalId { "votes": (+ existingVotes updatedVotes) })
;        )
      
;        ; After updating, validate the votes
;        (validate-votes proposalId)
;      )
;    )


;    (defun increment-tally (m vote)
;    (let* ((choice (at "choice" vote))
;           (count (if (at choice m) (at choice m) 0))
;           (updated-count (+ 1 count)))
;      (if (at choice m)
;        (update m choice updated-count)
;        (insert m choice updated-count)
;      )
;    )
;  )


;    (defun close-vote:bool 
;      (proposalId:string)
;      @doc "Close the voting for a proposal"
;      (let* ((proposal (read proposals proposalId))
;             (ct (curr-time)))
;        (enforce (>= ct (at 'end proposal)) "Voting period has not yet ended")
      
;        ; Need to work our tally logic into this flow
  
;        ; Set completed flag to true
;        (update proposals proposalId {"completed": true})
;      )
;    )

  (defun update-tweet-id:string
    (proposalId:string tweetId:string)
    (update proposals proposalId {"tweetId": tweetId})
  )
  
  (defun update-tally:bool
    (proposalId:string newTally:string)
    (update proposals proposalId {"votes": newTally})
  )
  

  (defun hash-id:string
    (name:string)
    (format "d:{}" [(hash name)]
    )
  )

  (defun fun 
    ()(select proposals (constantly true)))

    (defun curr-time:time ()
    @doc "Returns current chain's block-time"
    (at 'block-time (chain-data))
  )
)

(if (read-msg "upgrade")
"Upgrade Complete"
[
(create-table proposals)
]
)