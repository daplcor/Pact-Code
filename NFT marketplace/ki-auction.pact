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

    (use free.util-random)

    ; #####################################################

      (defschema auction
      ;item:object{poly-fungible-v2}
      item:string
      amount:decimal
      fungible:module{fungible-v2}
      minPrice:decimal
      startTime:time
      endTime:time
      owner:string
      highestBid:decimal
      highestBidder:string
      escrowId:string
    )
  
    (deftable auctions:{auction})
  
    (defun list-item
      ( 
        listData:object
        amount:decimal
        minPrice:decimal
        fungible:module{fungible-v2}
       )
      @doc
        "Transfer the amount of items into an escrow account and store the start and end times of the auction along with the min amount and the fungible that is required for the purchase."
      (let* ( (escrowId (gen-uuid-rfc-4122-v4)) 
              (escrow-account (format "escrow-{}" (get-ESCROW-account)))
              (escrow-guard (get-ESCROW-guard))
              (item (at "item" listData))
              (owner (at "owner" listData))
              (startTime (at "startTime" listData))
              (endTime (at "endTime" listData))  
            )
        (fungible::create-account item escrow-account escrow-guard)
        (fungible::transfer item owner escrow-account amount)
        (insert auctions (at "item" listData)
            { 
                "item": item,
                "amount": amount,
                "fungible": fungible,
                "minPrice": minPrice,
                "startTime": startTime,
                "endTime": endTime,
                "highestBid": 0.0,
                "highestBidder": "",
                "escrowId": escrowId           
            }
                listData
            )
    )))
  
    (defun place-bid
      ( escrowId:string
        bidder:string
        bidAmount:decimal
      )
      @doc
        "Place a bid on an auction only if the current time is between the start/end times and the current bid is less than the bidAmount. Transfer the bidAmount of fungible-v2 into the escrow and transfer the current bid's fungible back to the owner."
      (let* ( (auction (require (read auctions escrowId) "Auction not found"))
              (current-time (now))
            )
        (enforce (and (>= current-time (at "startTime" auction))
                      (<= current-time (at "endTime" auction)))
                 "Auction is not active")
        (enforce (< (highestBid auction) bidAmount)
                 "Bid amount is not greater than the current highest bid")
        (fungible::transfer
          (fungible.id auction.purchase-fungible)
          bidder
          (format "escrow-{}" [escrowId])
          bidAmount)
        (when (not (empty? (highestBidder auction)))
          (fungible::transfer
            (purchase-fungible.id auction.purchase-fungible)
            (format "escrow-{}" [escrowId])
            (highestBidder auction)
            (highestBidder auction)))
        (update auctions escrowId { "highestBid": bidAmount, "highestBidder": bidder })
    ))
  
    (defun complete-auction
      ( escrowId:string
      )
      @doc
        "Complete the auction if the current time is after the endTime. Transfer the NFT to the highest bidder and the funds to the seller."
      (let* ( (auction (require (read auctions escrowId) "Auction not found"))
              (current-time (now))
            )
        (enforce (> current-time (at "endTime" auction))
                 "Auction is still active")
  
        (fungible::transfer
          (auction.item.id auction.item)
          (format "escrow-{}" [escrowId])
          (auction.highestBidder auction)
          (auction.amount auction))
  
        (fungible::transfer
          (purchase-fungible.id auction.purchase-fungible)
          (format "escrow-{}" [escrowId])
          (auction.item.owner auction.item)
          (highestBid auction))
  
        (delete auctions escrowId)
    ))
  
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