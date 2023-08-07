(namespace "free")

(define-keyset "free.ku-ops" (read-keyset "ku-ops"))
(define-keyset "free.ku-admin" (read-keyset "ku-admin"))

(module ki-auction GOV

(defcap GOV ()
    (enforce-guard (keyset-ref-guard "free.ku-admin")))
  
  (defcap OPS ()
      (enforce-guard (keyset-ref-guard "free.ku-ops"))
      (compose-capability (OPS_INTERNAL))
    )
  
    (defcap OPS_INTERNAL ()
      true
    )


    ; #####################################################

      (use fungible-quote-policy-v1)
      (use marmalade.ledger)
      
      (defschema auction
        id:string @doc "Primary key"
        token-id:string  
        seller:string
        start-time:time 
        end-time:time
        reserve-price:decimal
        highest-bid:decimal
        highest-bidder:string 
        completed:bool @doc "True when auction closed"
      )
    
      (deftable auctions:{auction} @doc "Auction data")
    
      (defun create-auction:bool 
        (token-id:string, seller:string, end-time:time, reserve-price:decimal)
        
        (insert auctions (format "auction-{}" [token-id])
          {
            "id": (format "auction-{}" [token-id])  
            "token-id": token-id,
            "seller": seller,
            "start-time": (at 'block-time (chain-data)),  
            "end-time": end-time,
            "reserve-price": reserve-price,
            "highest-bid": 0.0,
            "completed": false  
          }
        )
    
      )
    
      (defun bid:bool 
        (auction-id:string, bidder:string, bid-amount:decimal)
    
        (with-read auctions auction-id {
          "highest-bid":= current-high-bid,
          "reserve-price":= reserve  
        }
    
        (enforce (> bid-amount current-high-bid) "Bid too low")
        (enforce (>= bid-amount reserve) "Bid lower than reserve")
    
        (fungible-quote-policy-v1.place-bid 
          auction-id bidder (bid-escrow-account auction-id bidder)  
          1 bid-amount)
    
        (update auctions auction-id {
          "highest-bid": bid-amount,
          "highest-bidder": bidder
        })
    
      )
    
      (defun close-auction:bool (auction-id:string)
    
        (with-read auctions auction-id {
          "token-id":= token-id, 
          "seller":= seller,
          "highest-bidder":= winner,
          "highest-bid":= winning-bid
        }
    
        (if (> winning-bid 0.0)
          (begin
            (fungible-quote-policy-v1.accept-bid
              (bid-escrow-account auction-id winner)
              winner auction-id (escrow-account auction-id))
            (ledger.transfer token-id seller winner 1)
          )
        )
    
        (update auctions auction-id {"completed": true})
    
      )
    
    )
  
  
  
; #############################################
;                 ESCROW Account
; #############################################


(defcap ESCROW ()
@doc "Checks to make sure the guard for the given account name is satisfied"
true
)

(defun require-ESCROW ()
@doc "The function used when building the user guard for managed accounts"
(require-capability (ESCROW))
)

(defun create-ESCROW-guard ()
@doc "Creates the user guard"
(create-user-guard (require-ESCROW))
)

(defun get-ESCROW-account ()
(create-principal (create-ESCROW-guard))
)


(defun init ()
(with-capability (GOVERNANCE)
  ;  (coin.create-account KDA_BANK_ACCOUNT (kda-bank-guard))
  (coin.create-account (get-ESCROW-account) (create-ESCROW-guard))
)
)

  )

  (if (read-msg "upgrade")
  "Upgrade Complete"
  [
  (create-table ki-auction.auctions)
  (init)
  ]
  )