(namespace "free")

(define-keyset "free.ku-ops" (read-keyset "ku-ops"))
(define-keyset "free.ku-admin" (read-keyset "ku-admin"))

(module ku-token-policy GOVERNANCE
    
  (defcap GOVERNANCE ()
  (enforce-guard (keyset-ref-guard "free.ku-admin")))
  
  (defcap OPS ()
      (enforce-guard (keyset-ref-guard "free.ku-ops"))
      (compose-capability (OPS_INTERNAL))
    )
  
    (defcap OPS_INTERNAL ()
      true
    )
  
  (implements kip.token-policy-v1)
    
  (use kip.token-policy-v1 [token-info])
  
  ; ============================================
  ; ==             Policies                   ==
  ; ============================================
  
  (defschema policy-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    royalty-rate:decimal
    collectionName:string
  )

  (deftable policies:{policy-schema})

    (defun enforce-init:bool
      ( token:object{token-info}
      )
      (enforce-ledger)
      true
    )
  
    (defun enforce-ledger:bool 
      ()
      (enforce-guard (marmalade.ledger.ledger-guard))
    )
  
    (defun enforce-mint:bool
      ( token:object{token-info}
        account:string
        guard:guard
        amount:decimal
      )
      (enforce-ledger)
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
        sale-id:string )
      (enforce-ledger)
      (enforce-sale-pact sale-id)
      true
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
      true
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
  (create-table free.ku-token-policy.policies)
]
)


