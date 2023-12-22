(namespace "free")
(define-keyset "free.pay-oracle-admin" (read-keyset "pay-oracle-admin"))
(define-keyset "free.pay-oracle-ops" (read-keyset "pay-oracle-ops"))

(module pay-oracle GOV

    (defcap GOV ()
        (enforce-guard (keyset-ref-guard "free.pay-oracle-admin")))
    
    (defcap OPS ()
        (enforce-guard (keyset-ref-guard "free.pay-oracle-ops"))
    )

    (defschema pricing-schema
        kda-usd-price:decimal
        updated:time
    )

    (deftable pricing-table:{pricing-schema})
    
    ;; Price functions

    (defun initg:bool ()
        @doc "Initializes the price table with default price"
        (with-capability (GOV)
        (insert pricing-table "kda-usd-price" 
        {"kda-usd-price": 1.0
        , "updated": (curr-time)
    }))true
    )

    (defun update-kda-usd-price:bool (new-price:decimal)
        @doc "Updates the KDA to USD price"
        (with-capability (OPS)
        (update pricing-table "kda-usd-price" { "kda-usd-price" : new-price, "updated": (curr-time)})
    )true)
  
    (defun get-kda-usd-price:object ()
        @doc "Returns the current KDA to USD price"
        (read pricing-table "kda-usd-price")
    )

    (defun curr-time:time ()
    @doc "Returns current chain's block-time"
    (at 'block-time (chain-data))
  )

)

(if (read-msg "upgrade")
"Upgrade Complete"
[
  (create-table pricing-table)
  (initg)
]
)
  