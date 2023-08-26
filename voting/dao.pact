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
    voter:string
    choice:integer
    )

  (defschema proposal
    id:string
    name:string
    description:string
    choices:[string]
    start:time
    end:time
    votes:{vote-schema}
  )

  (deftable proposals:{proposal})
  
  (defun create-proposal 
    (
        name:string
        description:string
        choices:[string]
        start:time 
        end:time
        )
    (let ((id:string (hash-id name)))
    ;  @doc "Create a new proposal"
    (with-capability (OPS)
      (insert proposals id {
        "id": id,
        "name": name,
        "description": description,
        "choices": [choices],
        "start": start,
        "end": end,
        "votes": {}
      }
      id
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


  (defun increment-tally (m vote)
  (let* ((choice (at "choice" vote))
         (count (if (at choice m) (at choice m) 0))
         (updated-count (+ 1 count)))
    (if (at choice m)
      (update m choice updated-count)
      (insert m choice updated-count)
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

  (defun hash-id:string
    (name:string)
    (format "d:{}"
    (hash name)
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