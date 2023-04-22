(namespace "free")

(define-keyset "free.ku-ops" (read-keyset "ku-ops"))
(define-keyset "free.ku-admin" (read-keyset "ku-admin"))

(module ku-tk-policy GOVERNANCE
    
(defcap GOVERNANCE ()
(enforce-guard (keyset-ref-guard "free.ku-admin")))

(defcap OPS ()
    (enforce-guard (keyset-ref-guard "free.ku-ops"))
    (compose-capability (OPS_INTERNAL))
  )

  (defcap KU_SALE ()
      true)

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
      platform-payout:decimal
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
  id:string
  fungible:module{fungible-v2}
  creator:string
  creator-guard:guard
  mint-guard:guard
  max-supply:decimal
  min-amount:decimal
  royalty-rate:decimal
  collection:string
)

(deftable policies:{policy-schema})

(defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

    (defconst ROYALTY_SPEC "royalty_spec"
      @doc "Payload field for royalty spec")
      
      (defconst PLATFORM_SPEC "platform_spec"
      @doc "Payload field for platform spec")

; ============================================
; ==             Policies                   ==
; ============================================

(defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
  )

(defun enforce-init:bool
  ( token:object{token-info}
  )
  (enforce-ledger)
  (let* ( (spec:object{policy-schema} (read-msg TOKEN_SPEC))
          (fungible:module{fungible-v2} (at 'fungible spec))
          (creator:string (at 'creator spec))
          (creator-guard:guard (at 'creator-guard spec))
          (mint-guard:guard (at 'mint-guard spec))
          (max-supply:decimal (at 'max-supply spec))
          (min-amount:decimal (at 'min-amount spec))
          (royalty-rate:decimal (at 'royalty-rate spec))
          (platform-rate:decimal (at 'platform-rate spec))
          (creator-details:object (fungible::details creator ))
          )
    (enforce (>= min-amount 0.0) "Invalid min-amount")
    (enforce (>= max-supply 0.0) "Invalid max-supply")
    (enforce (=
      (at 'guard creator-details) creator-guard)
      "Creator guard does not match")
    (enforce (and
      (>= royalty-rate 0.0) (<= royalty-rate 1.0))
      "Invalid royalty rate")
      (enforce (and
        (>= platform-rate 0.0) (<= platform-rate 1.0))
        "Invalid Platform rate")
    (insert policies (at 'id token)
      { 'fungible: fungible
      , 'creator: creator
      , 'creator-guard: creator-guard
      , 'id: (at 'id token)
      , 'mint-guard: mint-guard
      , 'max-supply: max-supply
      , 'min-amount: min-amount
      , 'royalty-rate: royalty-rate
      , 'platform-rate: platform-rate
      , 'collection: (get-collection token)
    }))
  true
)

(defun get-collection (token:object{token-info})
      (at "collection" (at "datum" (at 0 (at "data" (at "manifest" token)))))
    )

(defun get-token-collection (token:object{token-info})
      (at "collection" (at "datum" (at 0 (at "data" (at "manifest" token)))))
    )

    (defun get-collection-tokens (collection:string)
        (select policies ['id] (where 'collection (= collection)))
    )

  (defun enforce-ledger:bool 
    ()
    (enforce-guard (marmalade.ledger.ledger-guard))
  )

  ;  (defun enforce-mint:bool
  ;    ( token:object{token-info}
  ;      account:string
  ;      guard:guard
  ;      amount:decimal
  ;    )
  ;    (enforce-ledger)
  ;    (bind (get-policy token)
  ;      { 'mint-guard:=mint-guard:guard
  ;      , 'min-amount:=min-amount:decimal
  ;      , 'max-supply:=max-supply:decimal
  ;      }
  ;      (enforce-guard mint-guard)
  ;      (enforce (>= amount min-amount) "mint amount < min-amount")
  ;      (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
  ;  ))

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (enforce-guard (keyset-ref-guard "free.ku-ops"))
  )
  
  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce (= 1.0 amount) "Invalid Token Amount")
    (enforce-ledger)
    (enforce false "burn not implemented") 
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
       ,'platform-rate:= platform-rate:decimal
       ,'creator:= creator:string
      }
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient))
            (sale-price:decimal (* amount price))
            (royalty-payout:decimal
               (floor (* sale-price royalty-rate) (fungible::precision)))
               (platform-rate:decimal (if (try false (read-msg "KU_SALE")) (floor (* sale-price 0.9) (fungible::precision)) 0.0))
               )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price (+ royalty-payout platform-rate) creator spec)))
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
      , 'platform-rate:= platform-rate:decimal
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
                    (platform-payout:decimal (if (try false (read-msg "KU_SALE")) (floor (* sale-price 0.9) (fungible::precision)) 0.0))
                 (payout:decimal (- sale-price (+ platform-payout royalty-payout))) )
            (if
              (> royalty-payout 0.0)
              (fungible::transfer buyer creator royalty-payout)
              "No royalty")
              (if
                (> platform-rate 0.0)
                (fungible::transfer buyer creator platform-payout royalty-payout)
                "No comission")  
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
    true
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal 
    )
    (enforce (= 1.0 amount) "Invalid Token Amount")
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )


)

(if (read-msg "upgrade")
"Upgrade Complete"
[
  (create-table free.ku-tk-policy.quotes)
  (create-table free.ku-tk-policy.policies)
]
)


