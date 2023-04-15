(namespace "free")

(define-keyset "free.ku-admin" (read-keyset "ku-admin"))
(define-keyset "free.ku-ops" (read-keyset "ku-ops"))

(module kadenai-pay GOVERNANCE
    
    (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.ku-admin" ))
       )

    (defcap OPS()
    (enforce-guard (keyset-ref-guard "free.ku-ops"))
       )

    (defcap PAYER()
    (enforce-guard (at 'creatorGuard (read collections collection ['creatorGuard ])))
      "Must have an active creatorGuard stored"
      (compose-capability (PAYMOD))
    )

    (defcap PAYMOD ()
    true
    )
 
    (defconst fungible "fungible")
  
    (defschema payment-table
      account:string
      cost:decimal
      name:string
      fungible:module{fungible-v2}
    )
  
    (deftable payments:{payment-table})
  
    (defschema collection
      name:string
      description:string
      creator:string
      creatorGuard:guard
      totalSupply:decimal
      paid:bool
      category:string
      fungible:module{fungible-v2}
    )
  
    (deftable collections:{collection})
  
    ;  (use poly-fungible-v2)
  
    ;; Functions
 (defun create-collection-with-payment (collectionData:object fungible:module{fungible-v2} account:string)
 (let*
   (
    (IMGCOST (get-image-fee))
     (collectionName (at "name" collectionData))
     (totalSupply (at "totalSupply" collectionData ))
     (collectionCost:decimal (floor (* totalSupply IMGCOST) 2))
   )
   (record-payment
     {
       "account": account,
       "cost": collectionCost,
       "name": collectionName,
       "fungible": fungible
     }
   )
   (create-collection collectionData fungible)
   (update-payment account collectionName collectionCost fungible)
 )
)

    (defun create-collection:string
       (collectionData:object
        fungible:module{fungible-v2})
        @doc "Creates a collection with the provided data"
      (insert collections (at "name" collectionData)
      (+
        {
          "fungible": fungible
        , "totalSupply": (at "totalSupply" collectionData)
        }
        collectionData
      )
    ))

(defun record-payment (payment:object{payment-table})
  (insert payments (at "name" payment) payment)
)

  (defun update-payment (account:string name:string cost:decimal fungible:module{fungible-v2})
  @doc "Updates the payment data for collection"
  (with-read payments name 
    { "name":= paymentName }
    (enforce (not (= account "")) "Account not found.")
    (let
      (
        (bank (get-bank))
        (collectionCost (calculate-cost paymentName))
        (totalSupply (with-read collections paymentName { "totalSupply":= totalSupply } totalSupply))
      )
      (enforce (> totalSupply 0.0) "Total supply should be greater than 0")
      (enforce (>= cost collectionCost) "Insufficient payment")
      (if (> cost 0.0)
          (fungible::transfer account bank collectionCost)
          []
      )
      (update payments name { "cost": collectionCost })
      (update-collection-payment-status paymentName true)
      )
    )
  )

  (defun update-collection-payment-status (name:string paid:bool)
  (with-read collections name 
    { "paid":= oldPaid }
    (enforce (not (= name "")) "Collection not found.")
    (update collections name { "paid": paid })
  )
)


    (defun get-payment (name:string)
      (read payments name)
    )


; Getters for the absolute win!

(defun get-collection-data:object{collection} (collection:string)
@doc "Get the collection data for the given collection"
(read collections collection)
)
  

  (defun calculate-cost (name:string)
  @doc "Calculate the cost for images"
  (let (
    (IMGCOST (get-image-fee)))
    (with-read collections name
    { "totalSupply":= totalSupply }
    (*  totalSupply IMGCOST)
  ))
)

    ;; -------------------------------
    ;;          Bank Details
    ;; -------------------------------
  
  
    (defschema value
      @doc "Stores string values"
      value:string
    )
    (deftable values:{value})
  
    (defun update-string-value (val-id:string value:string)
      @doc "Updates the account for the bank"
  
      (with-capability (OPS)
        (write values val-id
          { "value": value }
        )
      )
    )
  
    (defun get-string-value:string (val-id:string)
      @doc "Gets the value with the provided id"
  
      (at "value" (read values val-id ["value"]))
    )
  
    (defconst BANK_ACCOUNT:string "BANK")
  
    (defun get-bank:string ()
      (get-string-value BANK_ACCOUNT)
    )

  ;; -------------------------------
    ;;          Bank Details
    ;; -------------------------------
  
  
    (defschema fee
      @doc "Stores image fee"
      value:decimal
    )
    (deftable fees:{fee})
  
    (defun update-image-cost (valId:string value:decimal)
      @doc "Updates the account for the bank"
  
      (with-capability (OPS)
        (write fees valId
          { "value": value }
        )
      )
    )
  
    (defun get-image-cost:string (valId:string)
      @doc "Gets the cost with the provided id"
  
      (at "value" (read fees valId ["value"]))
    )
  
    (defconst IMAGE_COST:string "IMAGECOST")
  
    (defun get-image-fee:string ()
      (get-image-cost IMAGE_COST)
    )

)

  (if (read-msg "upgrade")
"Upgrade Complete"
[
  (create-table collections)
  (create-table payments)
  (create-table values)
  (create-table fees)
  ;  (init)
]
)
  