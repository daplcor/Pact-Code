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
    dao:string
    description:string
    tweetId:string
    choices:[string]
    start:time
    end:time
    passed:bool 
    threshold:integer
    votes:[object:{vote-schema}]
  )

  (deftable proposals:{proposal})

(defun create-proposal 
    (
        name:string 
        description:string
        dao:string 
        choices:[string]
        start:time 
        end:time 
        passed:bool
        threshold:integer
        votes:[object:{vote-schema}] 
        tweetId:string
        )
    (let* ((id:string (hash-id name)))
    (with-capability (OPS)
      (insert proposals id 
        {
        "id": id,
        "name": name,
        "description": description,
        "dao": dao,
        "tweetId": "",
        "choices": choices,
        "start": start,
        "end": end,
        "passed":passed,
        "threshold": threshold,
        "votes": votes,
        "tweetId": tweetId
        }
      )
    )
  )
)

  (defun update-tweet-id:string
    (proposalId:string tweetId:string)
    (with-capability (OPS)
    (update proposals proposalId {"tweetId": tweetId})
  )
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