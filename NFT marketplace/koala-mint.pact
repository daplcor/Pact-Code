(namespace "free")

(define-keyset "free.koalatk-ops" (read-keyset "koalatk-ops"))
(define-keyset "free.koalatk-admin" (read-keyset "koalatk-admin"))

(module koala-tk-policy GOVERNANCE
    
(defcap GOVERNANCE ()
(enforce-guard (keyset-ref-guard "free.koalatk-admin")))

(defcap OPS ()
    (enforce-guard (keyset-ref-guard "free.koalatk-ops"))
    (compose-capability (OPS_INTERNAL))
  )


  (defcap OPS_INTERNAL ()
    true
  )

  (implements kip.token-policy-v1)
(use kip.token-policy-v1 [token-info])


(defcap QUOTE:bool
       ( sale-id:string
          token-id:string
          amount:decimal
          price:decimal
          sale-price:decimal
          royalty-payout:decimal
          creator:string
          spec:object{quote-spec}
        )
        @doc "For event emission purposes"
        @event
        true
      )

(defschema quote-spec
  @doc "Quote data to include in payload"
  price:decimal
  recipient:string
  recipient-guard:guard
  )

(defschema quote-schema
  id:string
  spec:object{quote-spec})

  
(deftable quotes:{quote-schema})

(defschema policy-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    mint-amount:decimal
    max-supply:decimal
    mint-start-time:time
    royalty-rate:decimal
    mint-active:bool
  )

(defschema uri-schema
    nftid:string
    image:string
)

(defun get-all:[object:{uri-schema}] ()
(select uri (constantly true)))

(deftable uri:{uri-schema})

(deftable policies:{policy-schema})

(defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

    (defconst ROYALTY_SPEC "royalty_spec"
      @doc "Payload field for royalty spec")
      
 
(defun mint:bool 
    (token:string
     owner:string
     guard:guard
     )
    (with-capability (OPS_INTERNAL) 

    (marmalade.ledger.mint
        token
        owner
        guard
        1.0
      )
      ; Add NFT to the NFT table 
      (insert uri owner
        {
          "nftid": token,
          "image": "https://ipfs.io/ipfs/bafybeig4gbqi4cck46toanmlfgcf7e6guan4jcyyuqnwm5ik2kdwqtatwq"
        }
      )
      token
    )
)

; ============================================
; ==             Policies                   ==
; ============================================


  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
  )


  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
   )

(defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (let* ( (account-bal:decimal (try 0.0 (at 'balance (marmalade.ledger.details (at 'id token) account))))
            (policy-info:object{policy-schema} (get-policy token))
            (mint-amount:decimal (at 'mint-amount policy-info))
            (max-supply:decimal (at 'max-supply policy-info))
            (mint-active:bool (at 'mint-active policy-info))
            (mint-start-time:time (at 'mint-start-time policy-info)) )
      (enforce mint-active "Mint period has ended")
      (enforce (= account-bal 0.0) "Account has already minted")
      (enforce (= amount mint-amount) "Mint amount can only be 1.0")
      (enforce (>= (at 'block-time (chain-data))  mint-start-time ) "Mint has not started yet")
      (if (= max-supply 0.0)
        true
        (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
      )
    )
  )

    (defun update-mint-status (token-id:string active:bool) 
    (with-capability (GOVERNANCE) 
     (update policies token-id {
       "mint-active": active}
     )
    )
  ) 

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce false "Burn prohibited")
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (let* ( (spec:object{policy-schema} (read-msg TOKEN_SPEC))
            (fungible:module{fungible-v2} (at 'fungible spec))
            (creator:string (at 'creator spec))
            (creator-guard:guard (at 'creator-guard spec))
            (max-supply:decimal (at 'max-supply spec))
            (mint-amount:decimal (at 'mint-amount spec))
            (mint-start-time:time (at 'mint-start-time spec))
            (royalty-rate:decimal (at 'royalty-rate spec))
            (creator-details:object (fungible::details creator ))
            )
      (enforce (>= mint-amount 0.0) "Invalid mint-amount")
      (enforce (>= max-supply 0.0) "Invalid max-supply")
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "Invalid royalty rate")
      (insert policies (at 'id token)
        { 'fungible: fungible
        , 'creator: creator
        , 'creator-guard: creator-guard
        , 'max-supply: max-supply
        , 'mint-amount: mint-amount
        , 'mint-start-time:mint-start-time
        , 'royalty-rate: royalty-rate
        , 'mint-active: true}))
    true
  )

  
 
  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
       ,'royalty-rate:= royalty-rate:decimal
       ,'creator:= creator:string
      }
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient))
            (sale-price:decimal (* amount price))
            (royalty-payout:decimal
               (floor (* sale-price royalty-rate) (fungible::precision))) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price royalty-payout creator spec)))
      true
  )
  )


  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
      , 'creator:= creator:string
      , 'royalty-rate:= royalty-rate:decimal
      }
      (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          { 'price := price:decimal
          , 'recipient := recipient:string
          }
          (let* ((sale-price:decimal (* amount price))
                 (royalty-payout:decimal
                    (floor (* sale-price royalty-rate) (fungible::precision)))
                 (payout:decimal (- sale-price royalty-payout)) )
            (if
              (> royalty-payout 0.0)
              (fungible::transfer buyer creator royalty-payout)
              "No royalty")
            (fungible::transfer buyer recipient payout)))
            true
        ))
  )
  

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )


)

(if (read-msg "upgrade")
"Upgrade Complete"
[
  (create-table quotes)
  (create-table policies)
  (create-table uri)
]
)

