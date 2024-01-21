; Create a different example with a similar structure

(namespace "free")

(define-keyset "free.ku-ops" (read-keyset "ku-ops"))
(define-keyset "free.ku-admin" (read-keyset "ku-admin"))

(module kadenai-auction GOV

(defcap GOV ()
    (enforce-guard (keyset-ref-guard "free.ku-admin")))
  
  (defcap OPS ()
      (enforce-guard (keyset-ref-guard "free.ku-ops"))
      (compose-capability (OPS_INTERNAL))
    )
  
    (defcap OPS_INTERNAL ()
      true
    )

(implements marmalade.ledger)
(use marmalade.ledger)

    ; #####################################################

    (defschema auction-schema
      token-id:string
      seller:string
      start-time:time
      end-time:time
      current-bid:decimal
      current-bidder:string
      reserve-price:decimal
      status:integer
    )
    
    (defconst AUCTION-STATUS-OPEN 0)
    (defconst AUCTION-STATUS-CLOSED 1)
    (defconst AUCTION-STATUS-CANCELLED 2)
    
    (deftable auctions:{auction-schema})
    
    (defun start-auction:bool 
      ( token-id:string
        seller:string
        start-time:time
        end-time:time
        reserve-price:decimal )
      (with-capability (SALE token-id seller reserve-price end-time (pact-id))
        (insert auctions (pact-id) { 
          "token-id": token-id,
          "seller": seller,
          "start-time": start-time,
          "end-time": end-time,
          "current-bid": reserve-price,
          "current-bidder": "",
          "reserve-price": reserve-price,
          "status": AUCTION-STATUS-OPEN
        })
      )
    )
    
    (defun place-bid:bool 
      ( auction-id:string
        buyer:string
        bid:decimal )
      (with-read auctions auction-id {
        'token-id:= token-id,
        'seller:= seller,
        'end-time:= end-time,
        'current-bid:= current-bid,
        'status:= status
      }
        (enforce (and (sale-active end-time) (= status AUCTION-STATUS-OPEN)) "Auction is not open")
        (enforce (> bid current-bid) "Bid is lower than current bid")
        (place-bid token-id buyer buyer (bid-guard) bid (get-bid-id auction-id buyer))
        (update auctions auction-id { 
          "current-bid": bid,
          "current-bidder": buyer
        })
      )
    )
    
    (defun close-auction:bool 
      ( auction-id:string )
      (with-read auctions auction-id {
        'token-id:= token-id,
        'seller:= seller,
        'end-time:= end-time,
        'current-bid:= current-bid,
        'current-bidder:= current-bidder,
        'status:= status
      }
        (enforce (not (sale-active end-time)) "Auction is still active")
        (enforce (= status AUCTION-STATUS-OPEN) "Auction is not open")
        (if (= current-bidder "")
          (withdraw token-id seller current-bid)
          (accept-bid (get-bid-id auction-id current-bidder) current-bidder auction-id (sale-account) (create-capability-guard (SALE_PRIVATE auction-id)))
        )
        (update auctions auction-id { 
          "status": AUCTION-STATUS-CLOSED
        })
      )
    )
    
    (defun cancel-auction:bool 
      ( auction-id:string )
      (with-read auctions auction-id {
        'token-id:= token-id,
        'seller:= seller,
        'current-bid:= current-bid,
        'current-bidder:= current-bidder,
        'status:= status
      }
        (enforce (= status AUCTION-STATUS-OPEN) "Auction is not open")
        (if (not (= current-bidder ""))
          (withdraw-bid (get-bid-id auction-id current-bidder) auction-id)
        )
        (withdraw token-id seller current-bid)
        (update auctions auction-id { 
          "status": AUCTION-STATUS-CANCELLED
        })
      )
    )
    
)
  

  (if (read-msg "upgrade")
  "Upgrade Complete"
  [
  (create-table auctions)
  ]
  )