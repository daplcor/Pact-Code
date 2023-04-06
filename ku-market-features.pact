; This policy is designed to support listing NFT's for sale, auction, and bidding.
; We need to validate balance matches the bid/sale amount, and hold the funds and nft
; in escrow until the transaction completes.
; Items to make decisions on:
; 1. Do we want a new escrow for every transaction
; 2. How do we want to handle transaction fees
; 3. How do we want to handle royalty distribution(escrow to list provided)
; 4. Will the contract hold bids until expiration, return after being outbid, return on a schedule
; before the auction expires

(namespace "free")

(define-keyset "free.ku-ops" (read-keyset "ku-ops"))
(define-keyset "free.ku-admin" (read-keyset "ku-admin"))

(module ku-market-features GOV

    (defcap GOVERNANCE ()
  (enforce-guard (keyset-ref-guard "free.ku-admin")))
  
  (defcap OPS ()
      (enforce-guard (keyset-ref-guard "free.ku-ops"))
      (compose-capability (OPS_INTERNAL))
    )
  
    (defcap OPS_INTERNAL ()
      true
    )
    

  ; ============================================
  ; ==             Policies                   ==
  ; ============================================

    (defschema nft-sale
        token-id:string
        owner:string
        salePrice:decimal
        forSale:bool
        buyNow:bool
        saleStart:time
        saleEnd:time
        ;  offerPrice:decimal
        ;  offerAccount:string
        )

        (defschema escrow-schema
            tokenId:string
            bidder:string
            lockedAmount:decimal
            )
          
        (deftable escrow:(escrow-schema))
        (deftable sales:(nft-sale))




; Buy and sell

(defun open-sale (type:string id:string owner:string buy_now_price:decimal sale_start_days:integer sale_end_days:integer minimum_bid:decimal)
            (with-default-read sale id {"closed": true} {"closed":= current_sale_closed}
                (enforce (= true current_sale_closed) "Previous sale not closed")
                (let* (
                                (owner_details (coin.details owner))
                                (token_details (marmalade.ledger.details id owner))
                                (can_buy_now (and (> buy_now_price 0.0) (!= type "time")))
                                (sale_id (hash [id, owner (now)]))
                            )
                            (enforce (> (at "balance" token_details) 0.0) "Token balance is 0")
                        
                            (if (= type "time")
                                (enforce (and (>= sale_end_days 1) (<= sale_end_days 7)) "Auction should last between 1 and 7 days")
                                "Not a time auction"
                            )

                            (if (is-rotate-compatible id)
                                (marmalade.ledger.rotate id owner (nft-bank-guard))
                                (marmalade.ledger.transfer-create id owner NFT_BANK_ACCT (nft-bank-guard) 1.0)
                            )
                            
                            (emit-event (OPEN_SALE id sale_id type owner buy_now_price sale_start_days sale_end_days minimum_bid))
                            (write sales id {
                                "token-id": id,
                                "owner": owner,
                                "salePrice": buy_now_price,
                                "forSale": true,
                                "buyNow": can_buy_now,
                                "saleStart": (add-time (now) (days sale_start_days)),
                                "saleEnd": (add-time (now) (days sale_end_days))
                            })
                )
            )
        )


        (defun buy-nft (tokenId:string buyer:string price:decimal)
        (with-read sales tokenId
          { "token-id":=saleTokenId,
            "owner":=saleOwner,
            "salePrice":=salePrice,
            "forSale":=forSale,
            "buyNow":=buyNow,
            "saleStart":=saleStart,
            "saleEnd":=saleEnd }
          (enforce forSale "NFT not for sale")
          (enforce buyNow "Buy now not available")
          (enforce (>= (at 'balance (read-keyset buyer)) price) "Insufficient funds")
          (enforce (= salePrice price) "Incorrect sale price")
          (debit buyer price)
          (insert escrow tokenId
            { "tokenId": tokenId,
              "bidder": buyer,
              "lockedAmount": price })))

              (defun complete-transfer (tokenId:string owner:string receiver:string)
    (complete-transfer tokenId receiver owner)
  (enforce-keyset (read-keyset 'owner))
  (enforce-keyset (read-keyset 'receiver))
  (transfer-nft tokenId receiver)
  (credit owner (at 'lockedAmount (read escrow tokenId)))
  (delete escrow tokenId))

  (defun buy-sell-continuation (tokenId:string buyer:string seller:string price:decimal)
  (enforce-keyset 'buyer-ks)
  (enforce-keyset 'seller-ks)
  (buy-nft tokenId buyer price)
  (complete-transfer tokenId buyer seller))

      




  ; These may need to go with our token policy.  

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
  
    (with-read token-royalties ROYALTY
      { "royalties" := royalties
      , "market-fee" := market-fee-string
      }
      (let* ((market-fee-decimal (read-decimal market-fee-string)))
        (with-read quotes sale-id 
          { 'id:= qtoken
          , 'spec:= spec:object{quote-spec} 
          }
          (enforce (= qtoken (at 'id token)) "incorrect sale token")
          (bind spec
            { 'price := price:decimal
            , 'recipient := recipient:string
            , 'fungible := fungible:module{fungible-v2}
            }
            (let* 
              (
                (sale-price:decimal (* amount price))
                (market-fee:decimal (floor (* sale-price market-fee-decimal) (fungible::precision)))
                (sale-price-after-fee:decimal (- sale-price market-fee))
                (royalty-data:decimal (get-royalty-data))
                (royalty-payout:decimal
                  (floor (* sale-price-after-fee (at "percent" royalty-data)) (fungible::precision)))
                (payout:decimal (- sale-price-after-fee royalty-payout))
              )
              (if (> royalty-payout 0.0)
                (fungible::transfer buyer (at "account" royalty-data) royalty-payout)
                "No royalty"
              )
              (fungible::transfer buyer recipient payout)
            )
          )
        )
      )
    )
  )

;; Define the royalty recipients and their percentages
(define royalty-recipients
    [{"account": "recipient1", "percentage": 0.5},
     {"account": "recipient2", "percentage": 0.3},
     {"account": "recipient3", "percentage": 0.2}])
  
  ;; Define the marketplace fee percentage
  (define marketplace-fee-percentage 0.05)
  
  ;; Define a function to distribute royalties
  (defun distribute-royalties:(payment:decimal marketplace-fee:decimal)
    (enforce (> payment 0) "Payment must be greater than 0")
  
    ;; Calculate and distribute royalties
    (map (lambda (recipient)
            (let* ((recipient-account (at 'account recipient))
                   (recipient-percentage (at 'percentage recipient))
                   (royalty-amount (* payment recipient-percentage)))
              (debit-coin (sender) royalty-amount)
              (credit-coin recipient-account royalty-amount)
              (format "Royalty distributed to {}" [recipient-account])))
          royalty-recipients)
    (format "Marketplace fee collected: {}" [marketplace-fee]))
  
 
  

  (defschema token-royalty
    royalties:[object{royalty}]
    market-fee:string
  )
  

  (defun update-token-royalties:string 
    (
      token-id:string
      royalties:[object{royalty}]
      market-fee:string
    )
    @doc "Updates the royalties for the token"
    (with-capability (OPS)
      (write token-royalties ROYALTY
        { "token-id": ROYALTY
        , "royalties": royalties
        , "market-fee": market-fee
        }
      )
    )
  )
  


    )

    (if (read-msg "upgrade")
    "Upgrade Complete"
    [
    (create-table ku-market-features.sales)
    (create-table ku-market-features.escrow)

    ]
    )