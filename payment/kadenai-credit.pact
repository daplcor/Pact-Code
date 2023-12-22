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

   
       (defcap CREDIT_EVENT
        (
          id:string
          account:string
          amount:decimal
        )
        @event true
      )
  
    (defschema payment-table
      id:string
      account:string
      account-guard:guard
      cost:decimal
      name:string
      fungible:module{fungible-v2}
    )
  
    (deftable payments:{payment-table})
  
    (defschema credit-schema
      id:string
      account:string
      amount:decimal
      fungible:module{fungible-v2}
    )

    (deftable credits:{credit-schema})


  
  
    ;; Functions
 (defun create-nft-with-payment (nftData:object fungible:module{fungible-v2} )
 (let* 
   (
     (IMGCOST:decimal (get-image-fee))
     (account:string (at "account" nftData))
     (precision:integer (at "precision" nftData))
     (uri:string (at "uri" nftData))
     (g:guard (at 'ks nftData))
     (collectionCost:decimal (floor (* totalSupply IMGCOST) 2))
     (time (get-time))
     (id:string (hash {"name": collectionName, "account": account, "totalSupply": totalSupply, "time": time}))
   )

  (update-payment id account collectionName collectionCost fungible collectionData)

   (record-payment
    {
     "id": id,
      "account": account,
      "cost": collectionCost,
      "name": collectionName,
      "fungible": fungible
    }
  )
  (emit-event (PAY_EVENT collectionName totalSupply account collectionCost))
  )
)

(defun record-payment (payment:object{payment-table})
  (insert payments (at "id" payment) payment)
)

(defun update-payment (id:string account:string name:string cost:decimal fungible:module{fungible-v2} collectionData:object)
  @doc "Updates the payment data for collection"
  
    (enforce (not (= account "")) "Account not found.")
    (let*
      (
        (bank (get-bank))
        (totalSupply (at "totalSupply" collectionData))
        (collectionCost (calculate-cost totalSupply))
      )
      (enforce (> totalSupply 0.0) "Total supply should be greater than 0")
      (enforce (>= cost collectionCost) "Insufficient payment")
      (if (> cost 0.0)
          (fungible::transfer account bank collectionCost)
          []
      )
   
      (insert collections (at "name" collectionData)
      (+ 
      {
                 "paid": true
                 , "fungible": fungible
        }
      collectionData 
      )
      )
    )
    )
   
    (defun get-payment (name:string)
      (read payments name)
    )

    (defun get-credit (id:string)
        (read credits id)
    )

    (defun add-credit (id:string account:string amount:decimal fungible:module{fungible-v2})
    @doc "Adds credit for the given account"
    (insert credits id
      {
        "id": id,
        "account": account,
        "amount": amount,
        "fungible": fungible
      }
    )
    (emit-event (CREDIT_EVENT id account amount))
  )
  

; Getters for the absolute win!

(defun get-collection-data:object{collection} (collection:string)
@doc "Get the collection data for the given collection"
(read collections collection)
)
  

  (defun calculate-cost (totalSupply:decimal)
  @doc "Calculate the cost for images"
  (let (
    (IMGCOST:decimal (get-image-fee)))
    (*  totalSupply IMGCOST)
  )
)

  (defun get-time ()
  (at "block-time" (chain-data)
  ))





    ;; -------------------------------
    ;;            Payments
    ;; -------------------------------

    (defun create-payment (amount:decimal account:string fungible:module{fungible-v2}  )
    (let*
      (
        (time (get-time))
        (bank (get-bank))
        (amt:decimal (floor (* amount 1.0) 2))
        (id (hash {"account": account, "amount": amt, "time": time}))
      )
   
      (if (> amount 0.0)
      (fungible::transfer account bank amt)
      []
  )
   
      (insert payments id
       {
         "account": account,
         "amount": amt,
         "fungible": fungible
       }
     )
     (emit-event (CREDIT_EVENT id account amt))
     )
   )
   
    ;; -------------------------------
    ;;          String Values
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
  
  
    (defschema conversion-schema
      @doc "Stores credit values"
      value:decimal
    )
    (deftable conversions:{conversion-schema})
  
    (defun update-image-cost (valId:string value:decimal)
      @doc "Updates the account for the bank"
  
      (with-capability (OPS)
        (write fees valId
          { "value": value }
        )
      )
    )
  
    (defun get-conversion-cost:decimal (valId:string)
      @doc "Gets the cost with the provided id"
  
      (at "value" (read fees valId ["value"]))
    )
  
    (defconst CREDIT_COST "CREDITCOST")
  
    (defun get-credit-conversion()
      (get-credit-cost CREDIT_COST)
    )

    ; Local Calls

    (defun getz ()
    (select payments (constantly true))
    )
)

  (if (read-msg "upgrade")
"Upgrade Complete"
[
  (create-table payments)
  (create-table conversions)
  (create-table credits)
  (create-table donate)
]
)
  