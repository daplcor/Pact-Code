(namespace "free")

(define-keyset "free.ku-admin" (read-keyset "ku-admin"))
(define-keyset "free.ku-ops" (read-keyset "ku-ops"))
(define-keyset "free.ku-bank" (read-keyset "ku-bank"))

(module kadenai-create GOVERNANCE

    (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.ku-admin" ))
    (compose-capability (OPS_INTERNAL))
    )
  
    (defcap OPS()
    (enforce-guard (keyset-ref-guard "free.ku-ops"))
    (compose-capability (OPS_INTERNAL))
    (compose-capability (WLMOD))

    )

  (defcap CREATECOL()
  true
  )

  (defcap EDIT()
  (compose-capability (OPS))
  (compose-capability (CREATOR))
  )

  (defcap CREATOR()
  (enforce-guard (keyset-ref-guard "creatorGuard"))
  )

  (defcap OPS_INTERNAL ()
  (compose-capability (MINT))
  )

  (defcap WHITELIST_UPDATE ()
  true
)

;  (implements marmalade.collection-policy-v1)
;  (use marmalade.collection-policy-v1)


(defcap MINT ()
(compose-capability (WHITELIST_UPDATE))
(compose-capability (SPLITTER))
)

(defcap MINT_EVENT
(
  collection:string
  tierId:string
  account:string
  amount:integer
)
@event true
)

; Test WL Guard for Collections

;  (defcap WHITELIST:bool (collection:string)
;  	(enforce-guard (at 'creatorGuard (read collections collection ['creatorGuard])))
;  	(compose-capability (WLMOD)
;  ))

(defcap WLCREATOR:bool (collection:string)
    
      (enforce-guard (at 'creatorGuard (read collections collection ['creatorGuard ])))
     
    "Must be the collection creator or have OPS capability"
  
  (compose-capability (WLMOD))
)

(defcap WLMOD ()
	true
)

; #################################################################
; #                      Schema Details                           #
; #################################################################

; NFT collections are stored in the nft-collections table.
  ; An NFT collection is uniquely identified by its name.
  ; The creator is the k:account of the original creator.
  ; totalSupply is the total number of NFTs in this collection.
  ; categories are helpful for classifying collections.
  ; provenance hash needs to be pre-computed and is a cryptographic hash
  ; of the entire hash of tokens
  ; tiers hold mint prices, qty allowed to mint, and start/end times.
  ; sale-royalties define who gets paid when NFTs in this collection are sold.
  (defschema collection
    @doc "Stores the name of the collection, the tiers, \
    \ the total supply of the collection. \
    \ The id is the name of the collection."
    name:string
    totalSupply:integer
    creator:string
    creatorGuard:guard
    category:string
    provenance:string
    description:string
    currentIndex:integer
    fungible:module{fungible-v2}
    tiers:[object:{tier}]
  )

      (defschema minted-token
        @doc "Stores the data for a minted token. \
        \ The id is the collection, tierId, account, and token-id."
        collection:string
        account:string
        guard:guard
        token-id:integer
        marmToken:string
        revealed:bool
      )


      (defschema tier
        @doc "Stores the start time, end time, tier type (WL, PUBLIC), \
        \ tierId, cost for this tier to mint, \
        \ and the limit for each minter."
        tierId:string
        tierType:string
        startTime:time
        endTime:time
        cost:decimal
        limit:decimal
      )

      (defschema fungible-account
        @doc "account and guard information of a fungible"
        account:string
        guard:guard
      )

  (defschema tier-whitelist-data
  @doc "A data structure for the whitelist data for a tier"
  tierId:string
  accounts:[string]
  )

      (defschema whitelisted
        @doc "Stores the account of the whitelisted user, the tierId, \
        \ and amount they have minted. The id is 'collection:tierId:account'."
        account:string
        tierId:string
        mint-count:integer
      )

      (defschema nft
        id:string
        owner:string
      )
      
      (deftable nft-table:{nft})

  ; ============================================
  ; ==                 Tables                 ==
  ; ============================================


  (deftable collections:{collection})
  (deftable minted-tokens:{minted-token})
  (deftable whitelist-table:{whitelisted})
  (deftable tiers:{tier})
  (deftable tdata:{token-data})
  (deftable tier-data:{tier-whitelist-data})


  ; ============================================
  ; ==         Initialize Collection          ==
  ; ============================================

  (defun create-collection:string
    (
      collection-data:object
      fungible:module{fungible-v2}
    )
    @doc "Creates a collection with the provided data."
    (with-capability (CREATECOL)
      ; Validate the collection tiers
      (enforce (> (at "totalSupply" collection-data) 0.0) "Total supply must be greater than 0")
      (validate-tiers (at "tiers" collection-data))
  
      (let*
        (
          (collection-name:string (at "name" collection-data))
          (collection-size:integer (floor (at "totalSupply" collection-data)))
          (operator-guard:guard (at "creatorGuard" collection-data))
          (provenance:string (at "provenance" collection-data))
          (category:string (at "category" collection-data))
          (creator:string (at "creator" collection-data))
          (creatorGuard:guard (at "creatorGuard" collection-data))
        )
        (insert collections (at "name" collection-data)
        (+
            { "fungible": fungible
            , "currentIndex": 1
            , "totalSupply": collection-size
            , "provenance": provenance
            , "category": category
            , "creator": creator
            , "creatorGuard": creatorGuard
            }
            collection-data
          )
        )
  
        ; Call init-collection in the collection-policy-v1 contract with the required fields
        (n_42174c7f0ec646f47ba227ffeb24714da378f4d1.collection-policy-v1.create-collection
          collection-name
          collection-size
          operator-guard
        )
        "Collection successfully created" 

      )
    )
  )
  


  (defun update-collection-tiers
    (
      collection:string
      tiers:[object:{tier}]
    )
    @doc "Updates the tiers of the given collection"
    (with-capability (OPS)
      (validate-tiers tiers)
      (update collections collection
        { "tiers": tiers }
      )
    )
    true
  )


  (defun validate-tiers:bool (tiers:[object:{tier}])
  @doc "Validates the tier start and end time, ensuring they don't overlap \
  \ and that start is before end for each."
  (let*
    (
      (no-overlap
        (lambda (tier:object{tier} other:object{tier})
          ;; If the other is the same as the tier, don't check it
          (if (!= (at "tierId" tier) (at "tierId" other))
            (enforce
              (or
                ;; Start and end of other is before start of tier
                (and?
                  (<= (at "startTime" other))
                  (<= (at "endTime" other))
                  (at "startTime" tier)
                )
                ;; Start and end of other is after end of tier
                (and?
                  (>= (at "endTime" other))
                  (>= (at "startTime" other))
                  (at "endTime" tier)
                )
              )
              "Tiers overlap"
            )
            []
          )
        )
      )
      (validate-tier
        (lambda (tier:object{tier})
          ;; Enforce start time is before end time,
          ;; and that the tier type is valid
          (enforce
            (<= (at "startTime" tier) (at "endTime" tier))
            "Start must be before end"
          )
          (enforce
            (or
              (= (at "tierType" tier) TIER_TYPE_WL)
              (= (at "tierType" tier) TIER_TYPE_PUBLIC)
            )
            "Invalid tier type"
          )
          (enforce
            (>= (at "cost" tier) 0.0)
            "Cost must be greater than 0"
          )
          ;; Loop through all the tiers and ensure they don't overlap
          (map (no-overlap tier) tiers)

        )
      )
    )
    (map (validate-tier) tiers) 
  )
  true
)

;  (defun update-collection-uri
;    (
;      collection:string
;      uri:string
;    )
;    (with-capability (OPS)
;      (update collections collection
;        { "rootUri": uri }
;      )
;    )
;  )

; #######################################
;              Bank Info
; #######################################

  (defschema bank-info
    @doc "Stores string values"
    value:string
  )
  (deftable bankInfo:{bank-info})

  (defun update-bank (bankId:string value:string)
    @doc "Updates the account for the bank"

    (with-capability (OPS)
      (write bankInfo bankId
        { "value": value }
      )
    )
  )

  (defun get-bank-value:string (bankId:string)
    @doc "Gets the value with the provided id"

    (at "value" (read bankInfo bankId ["value"]))
  )

  (defconst BANK_ACCOUNT:string "BANK")
  ;  (defconst KU_BANK:string "KU_BANK")
  (defconst PERCENT_TO_CREATOR:decimal 0.9)
  (defun get-bank:string ()
    (get-bank-value BANK_ACCOUNT)
  )

  ; ============================================
  ; ==               Constants                ==
  ; ============================================

    (defconst Invalid_Collection_Name "Invalid_Collection_Name")
    (defconst Invalid_Collection_Size "Invalid_Collection_Size")
    (defconst Invalid_NFT_Price "Invalid_NFT_Price")
    (defconst Invalid_Mint_Time "Invalid_Mint_Time")
    (defconst TIER_TYPE_WL:string "WL")
    (defconst TIER_TYPE_FREEWL:string "FREEWL")
    (defconst TIER_TYPE_PUBLIC:string "PUBLIC")
    (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")
    (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")
    (defconst MINT_STATUS "mint-status")
    (defconst COLLECTIONS "collections")
    (defconst MINT_COMPLETED "mint-completed")
    (defconst MINT_STARTED "mint-started")
    (defconst Account_Exists "account-exists")
    (defconst category "category")
    (defconst creator "creator")
    (defconst creatorGuard "creatorGuard")
    (defconst description "description")
    (defconst fungible "fungible")
     (defconst totalSupply "totalSupply")
    (defconst status "status")
    (defconst COLLECTION_STATUS_WHITELIST "COLLECTION_STATUS_WHITELIST")
    (defconst collection-name "collection-name")
    (defconst cRate 0.95)
    (defconst bRate 0.05)
    (defconst KDA_BANK_ACCOUNT:string "kuBank" )
    (defconst KDA_BANK_GUARD_NAME:string "free.kBank")

  ; ============================================
  ; ==           Mint Functionality           ==
  ; ============================================



  (defun admin-mint:string
    (
      collection:string
      account:string
      guard:guard
      amount:integer
    )
    @doc "Requires OPS. Mints the given amount of tokens \
    \ for the account for free."
    (with-capability (OPS)
      (let*
        (
          (collection-data (read collections collection))
          (currentIndex (at "currentIndex" collection-data))
          (tier (get-current-tier (at "tiers" collection-data)))
          (tierId (at "tierId" tier))
        )

        (mint-internal
          collection
          account
          guard
          amount
          tierId
          currentIndex
        )
      )
    )
  )

  (defun mint:bool
    (
      collection:string
      account:string
      amount:integer
      recipients:[string]
    )
    @doc "Mints the given amount of tokens for the account. \
    \ Gets the current tier and tries to mint from it. \
    \ If the tier is a whitelist, checks that the account is whitelisted \
    \ and that the mint count is within the limit. \
    \ If the tier is public, it allows anyone to mint."
    (enforce (> amount 0) "Amount must be greater than 0")

    (with-capability (MINT)
      (with-read collections collection
        { "currentIndex":= currentIndex
        , "totalSupply":= totalSupply
        , "fungible":= fungible:module{fungible-v2}
        , "creator":= creator:string
        , "creatorGuard":= creatorGuard
        , "tiers":= tiers
        }
        (enforce
          (<= (+ (- currentIndex 1) amount) totalSupply)
          "Can't mint more than total supply"
        )

        (bind (get-current-tier tiers)
          { "cost":= cost
          , "tierType":= tierType
          , "tierId":= tierId
          , "limit":= mint-limit
          }
          (enforce (> cost 0.0) "Amount should be greater than 0.0")

          (let*
            (
              (mint-count (get-whitelist-mint-count collection tierId account))
              (bankAc (get-bank))
              (total-cost:decimal (* amount cost))
              (creator-amount:decimal (floor (* total-cost PERCENT_TO_CREATOR) 2)) 
              (bank-amount:decimal (- total-cost creator-amount))
            )
            ;; If the tier is public, anyone can mint
            ;; If the mint count is -1, the account is not whitelisted
            (enforce
              (or
                (= tierType TIER_TYPE_PUBLIC)
                (!= mint-count -1)
              )
              "Account is not whitelisted"
            )
            ;; If the mint limit is -1, there is no limit
            ;; If the mint count is less than the limit, the account can mint
            (enforce
              (or
                (= mint-limit -1.0)
                (<= (+ mint-count amount) (floor mint-limit))
              )
              "Mint limit reached"
            )

            ;; Transfer funds
            (let
              (
                (splitter-account (get-SPLITTER-account))
              )  
              ; Transfer funds to the splitter account
              (fungible::transfer account splitter-account total-cost)

              ; Install capabilities for the transfers from the splitter account to creator and bank accounts
              (install-capability (fungible::TRANSFER splitter-account creator creator-amount))
              (install-capability (fungible::TRANSFER splitter-account bankAc bank-amount))

              ; Transfer funds from the splitter account to creator and bank accounts
              (fungible::transfer splitter-account creator creator-amount)
              (fungible::transfer splitter-account bankAc bank-amount)
            )
            
            
            ;; Handle the mint
            (if (= tierType TIER_TYPE_WL)
              (update-whitelist-mint-count collection tierId account (+ mint-count amount))
              []
            )
            (mint-internal
              collection
              account
              (at "guard" (fungible::details account))
              amount
              tierId
              currentIndex
            )
          )
        )
      )
    )
  )

  ;  (defun mint-transfer-helper 
  ;    (
  ;      managed-account:string 
  ;      fungible:module{fungible-v2}
  ;      amount:decimal 
  ;      recipient:string
  ;    )
  ;    @doc "Private function used to transfer funds from \
  ;    \ an unguarded account to given recipient"
  ;    ;  (require-capability (MANAGED managed-account))

  ;    (install-capability (fungible::TRANSFER managed-account recipient amount))
  ;    (fungible::transfer managed-account recipient amount)
  ;    (concat ["Funds to " recipient " successfully"])
  ;  )

  (defun mint-internal:bool
    (
      collection:string
      account:string
      guard:guard
      amount:integer
      tierId:string
      currentIndex:integer
    )
    (require-capability (MINT))

    (update collections collection
      { "currentIndex": (+ currentIndex amount) }
    )
    (map
      (mint-token collection account guard)
      (map (+ currentIndex) (enumerate 0 (- amount 1)))
    )
    (emit-event (MINT_EVENT collection tierId account amount))
  )

  (defun mint-token:string
    (
      collection:string
      account:string
      guard:guard
      token-id:integer
    )
    @doc "Mints a single token for the account."
    (require-capability (MINT))
    (insert minted-tokens (get-mint-token-id collection token-id)
      { "collection": collection
      , "account": account
      , "guard": guard
      , "token-id": token-id
      , "marmToken": ""
      , "revealed": false
      }
    )
  )

(defschema token-data
  @doc "The information necessary to mint the token on marmalade"
  precision:integer
  uri:string
  policy:module{kip.token-policy-v2}
)

(defun create-marmalade-token:bool
  (
    account:string
    uri:string
    precision:integer
    mint-token-id:string
    policies:[module{kip.token-policy-v2}]
  )
  @doc "Requires Private OPS. Creates the token on marmalade using the supplied data"
  ;  (with-capability (OPS_INTERNAL))
; check and verify if I need to wrap this funciton in the require-cap
  (with-capability (MINT)

      (let*
      (
        (hash-id (hash-contents uri precision policies))
        (token-id (concat ["t:" hash-id]))
        (guard (at "guard"(coin.details account)))
      )
      ;  (update minted-tokens mint-token-id
      ;    { "revealed": true
      ;    , "marmToken": token-id
      ;    }
      ;  )

      (marmalade.ledger.create-token
        token-id
        precision
        uri
        policies
      )
      
      ;  (install-capability (marmalade.ledger.MINT token-id account 1.0))
      (marmalade.ledger.mint
        token-id
        account
        guard
        1.0
      )
      ; Add NFT to the NFT table 
      (insert nft-table token-id
        {
          "id": token-id,
          "owner": account
        }
      )
      token-id
    )
  )
  )


(defun get-unrevealed-tokens:object{minted-token} ()
  @doc "Returns a list of unrevealed tokens."
  (select minted-tokens (where "revealed" false))
)

;  (defun reveal-token:string
;    (
;      m-token:object{minted-token}
;      precision:integer
;      policy:module{kip.token-policy-v2}
;    )
;    @doc "Requires OPS. Reveals the token for the given account."
;    (with-capability (OPS)
;      (bind m-token
;        { "collection":= collection
;        , "token-id":= token-id
;        , "account":= account
;        , "guard":= guard
;        }

;        (create-marmalade-token
;          account
;          guard
;          (get-mint-token-id collection token-id)
;          (+
;            t-data
;            { "precision": precision
;            , "policy": policy
;            }
;          )
;        )
;      )
;    )
;  )

(defun get-mint-token-id:string
  (
    collection:string
    token-id:integer
  )
  (concat [collection "|" (int-to-str 10 token-id)])
)

  ; ============================================
  ; ==             Whitelisting               ==
  ; ============================================

  (defun add-whitelist-to-collection
    (
      collection:string
      tier-data:[object{tier-whitelist-data}]
    )
    @doc "Requires creator guard. Adds the accounts to the whitelist for the given tier."
    (with-capability (OPS)
      (let
        (
          (handle-tier-data
            (lambda (tier-data:object{tier-whitelist-data})
              (let
                (
                  (tierId (at "tierId" tier-data))
                  (whitelist (at "accounts" tier-data))
                )
                (map (add-to-whitelist collection tierId) whitelist)
              )
            )
          )
        )
        (map (handle-tier-data) tier-data)
      )
    )
  )

(defun add-whitelist-to-tier:[string]
(
  collection:string
  tier-data:object{tier-whitelist-data}
)
@doc "Requires creator guard. Adds the accounts to the whitelist for the given tier."
(with-capability (OPS)
  (let
    (
      (tierId (at "tierId" tier-data))
      (whitelist (at "whitelist" tier-data))
    )
    (map (add-to-whitelist collection tierId) whitelist)
  )
)
)

(defun add-whitelist-creator
  (
    collection:string
    tier-data:[object{tier-whitelist-data}]
  )
  @doc "Requires creator guard. Adds the accounts to the whitelist for the given tier."
  (with-capability (WLCREATOR collection)
    (let
      (
        (handle-tier-data
          (lambda (tier-data:object{tier-whitelist-data})
            (let
              (
                (tierId (at "tierId" tier-data))
                (whitelist (at "accounts" tier-data))
              )
              (map (add-to-whitelist collection tierId) whitelist)
            )
          )
        )
      )
      (map (handle-tier-data) tier-data)
    )
  )
)

(defun add-to-whitelist:string
(
  collection:string
  tierId:string
  account:string
)
@doc "Requires creator guard. Adds the account to the whitelist for the given tier."
(require-capability (WLMOD))

(insert whitelist-table (concat [collection ":" tierId ":" account])
  {
    "tierId": tierId
  , "account": account
  ,  "mint-count": 0
  }
)
)

(defun is-whitelisted:bool
(
  collection:string
  tierId:string
  account:string
)
@doc "Returns true if the account is whitelisted for the given tier."
(let
  (
    (whitelist-id (get-whitelist-id collection tierId account))
  )
  (with-default-read whitelist-table whitelist-id
    { "mint-count": -1 }
    { "mint-count":= mint-count }
    (!= mint-count -1)
  )
)
)

(defun get-whitelist-mint-count:integer
(
  collection:string
  tierId:string
  account:string
)
(let
  (
    (whitelist-id (get-whitelist-id collection tierId account))
  )
  (with-default-read whitelist-table whitelist-id
    { "mint-count": -1 }
    { "mint-count":= mint-count }
    mint-count
  )
)
)

(defun get-whitelist-id:string
(
  collection:string
  tierId:string
  account:string
)
(concat [collection ":" tierId ":" account])
)

(defun update-whitelist-mint-count
(
  collection:string
  tierId:string
  account:string
  count:integer
)
@doc "Requires Whitelist Update. Updates the mint count for the given account in the whitelist."
(require-capability (WHITELIST_UPDATE))

(let
  (
    (whitelist-id (get-whitelist-id collection tierId account))
  )
  (update whitelist-table whitelist-id
    { "mint-count": count }
  )
)
)


; Auctions

;  (deftable auctions:{auction-schema})
;  (deftable auction-bids:{auction-bid-schema})

;  (defschema auction-schema
;    token-id:string
;    start-time:time
;    end-time:time
;    seller:string
;    high-bid:decimal
;    high-bidder:string
;    status:string
;  )

;  (defschema auction-bid-schema
;    auction-id:string
;    bidder:string
;    bid:decimal
;    timestamp:time
;  )

;  (defun start-auction:bool
;    ( token-id:string
;      start-time:time
;      end-time:time
;      seller:string
;      amount:decimal
;    )
;    (insert auctions token-id { "token-id": token-id, "start-time": start-time, "end-time": end-time, "seller": seller, "high-bid": 0.0, "high-bidder": "", "status": "open"  })
;      ;  (marmalade.ledger.sale 
;      ;    token-id 
;      ;    seller 
;      ;    amount 
;      ;    end-time
;      ;    )
;    )



;  (defun place-bid:bool
;    ( tokenId:string
;      auction-id:string
;      bidder:string
;      bid:decimal
;    )
;    (let* (
;      (auction:object{auction-schema} (read auctions auction-id))
;      (high-bid:decimal (at 'high-bid auction))
;      (start-time:time (at 'start-time auction))
;      (end-time:time (at 'end-time auction))
;      (current-time:time (curr-time))
;      (status:string (at 'status auction))
;      (buyerGuard (at "guard"(coin.details bidder)))
;    )
;      (enforce (> bid high-bid) "Bid must be higher than current bid")
;      (enforce (and (>= current-time start-time) (<= current-time end-time)) "Bid can only be placed during the auction period")
;      (enforce (= status "open") "Auction is not open")
;      (update auctions auction-id { "high-bid": bid, "high-bidder": bidder })
;      ;; Transfer NFT from bidder to escrow account
;      ;; Call a function like `transfer-to-escrow` here
;      (insert auction-bids bidder { "auction-id": auction-id, "bidder": bidder, "bid": bid, "timestamp": current-time })
;    (marmalade.fungible-quote-policy-v1.place-bid
;      tokenId
;      bidder
;      buyerGuard
;      1.0
;      bid
;      auction-id
;      )
;      )
;  )

;  (defun end-auction:bool
;    (auction-id:string)
;    (let* (
;      (auction:object{auction-schema} (read auctions auction-id))
;      (end-time:time (at 'end-time auction))
;      (high-bidder:string (at 'high-bidder auction))
;      (seller:string (at 'seller auction))
;      (high-bid:decimal (at 'high-bid auction))
;      (status:string (at 'status auction))
;    )
;      (enforce (>= (curr-time) end-time) "Auction is not yet over")
;      (enforce (= status "open") "Auction is not open")
;      ;; Transfer ownership of NFT to high bidder
;      ;; Call a function like `transfer-from-escrow` here
;      ;; Transfer high bid to seller here
;      ;; Call a function like `transfer-funds` here
;      (update auctions auction-id { "status": "completed" })
;    )
;  )

;  (use fungible-quote-policy-v1)
(use marmalade.ledger)

(defschema auction
  id:string ; Primary key "sale-id"
  token-id:string  
  seller:string
  start-time:time 
  end-time:time
  reserve-price:decimal
  highest-bid:decimal
  highest-bidder:string 
  completed:bool ; True when auction closed
)

(deftable auctions:{auction} @doc "Auction data")

(defun create-auction:bool 
  (token-id:string
   seller:string
   end-time:time
   reserve-price:decimal
   )
   (let
  ((id:string (hash-sale token-id seller end-time)))
  (insert auctions id
    {
      "id": id,  
      "token-id": token-id,
      "seller": seller,
      "start-time": (at 'block-time (chain-data)),  
      "end-time": end-time,
      "reserve-price": reserve-price,
      "highest-bid": 0.0,
      "highest-bidder": "",
      "completed": false  
    }
  )
   )
  (marmalade.ledger.sale 
    token-id
    seller
    1.0
    end-time
    )
)

(defun bid:bool 
  (id:string
  bidder:string
  bid-amount:decimal)

  (with-read auctions id {
    "highest-bid":= current-high-bid,
    "reserve-price":= reserve,
    "token-id":= token-id 
  }
    
  (enforce (> bid-amount current-high-bid) "Bid too low")
  (enforce (>= bid-amount reserve) "Bid lower than reserve")
  ;  (marmalade.fungible-quote-policy-v1.place-bid 
  ;    token-id
  ;    bidder 
  ;    (at "guard" (coin.details bidder))
  ;    1.0
  ;    bid-amount
  ;    id
  ;  )
  
  (let* (
    (escrow-account:object{fungible-account} (marmalade.policy-manager.get-escrow-account id))
    )
  ;  (coin.TRANSFER bidder escrow bid-amount)

  (coin.transfer bidder (at 'account escrow-account) bid-amount)
  
; if auction time > end-time & highest-bid > reserve-price then close-auction
; else update-auction and get dat nft money.

  (update auctions id {
    "highest-bid": bid-amount,
    "highest-bidder": bidder
  })
  )
)
)

(defun close-auction:bool (auction-id:string)

  (with-read auctions auction-id {
    "id":= id,
    "token-id":= token-id,  
    "seller":= seller,
    "highest-bidder":= winner, 
    "highest-bid":= winning-bid
  }
  ;  (let* (
  ;    (bid-id:string (marmalade.fungible-quote-policy-v1.get-bid-id id winner))
  ;    (escrow:string (marmalade.fungible-quote-policy-v1.bid-escrow-account id))
  ;    (escrow-guard:guard (create-capability-guard (marmalade.fungible-quote-policy-v1.BID_PRIVATE id)))
  ;    )
  ;  ;  (if (> winning-bid 0.0)

  ;  (marmalade.fungible-quote-policy-v1.accept-bid
  ;   bid-id
  ;   winner
  ;   id
  ;   escrow
  ;   escrow-guard
  ;  )
  ;  ;  )
  ;    (marmalade.ledger.transfer token-id seller winner 1)

    (update auctions auction-id {"completed": true})
  ;  )
)
)

  (defun auc 
    ()(select auctions (constantly true)))


  ; ============================================
  ; ==         Get Detail Functions           ==
  ; ============================================


  (defun get-current-tier-for-collection:object{tier} (collection:string)
    @doc "Gets the current tier for the collection"
    (with-read collections collection
      { "tiers":= tiers}
      (get-current-tier tiers)
    )
  )
  (defun curr-time:time ()
  @doc "Returns current chain's block-time"
  (at 'block-time (chain-data))
)

  (defun get-current-tier:object{tier} (tiers:[object:{tier}])
    @doc "Gets the current tier from the list based on block time"
    (let*
      (
        (now (at "block-time" (chain-data)))
        (filter-tier
          (lambda (tier:object{tier})
            (if (= (at "startTime" tier) (at "endTime" tier))
              (>= now (at "startTime" tier))
              (and?
                (<= (at "startTime" tier))
                (> (at "endTime" tier))
                now
              )
            )
          )
        )
        (filtered-tiers (filter (filter-tier) tiers))
      )
      (enforce (> (length filtered-tiers) 0) (format "No tier found: {}" [now]))
      (at 0 filtered-tiers)
    )
  )

  (defun get-unrevealed-tokens-for-collection:[object:{minted-token}]
    (
      collection:string
    )
    @doc "Returns a list of unrevealed tokens."
    (select minted-tokens
      (and?
        (where "revealed" (= false))
        (where "collection" (= collection))
      )
    )
  )

  (defun get-owned:[object:{minted-token}]
    (
      account:string
    )
    @doc "Returns a list of tokens owned by the account."
    (select minted-tokens (where "account" (= account)))
  )

  (defun get-owned-for-collection:[object:{minted-token}]
    (
      account:string
      collection:string
    )
    @doc "Returns a list of tokens owned by the account."
    (select minted-tokens
      (and?
        (where "account" (= account))
        (where "collection" (= collection))
      )
    )
  )

  (defun get-all-revealed:[object:{minted-token}]()
    @doc "Returns a list of all revealed tokens."
    (select minted-tokens (where "revealed" (= true))
    ))

  (defun get-all-nft ([object:{minted-token}])
        @doc "Returns a list of all tokens."
    (keys minted-tokens)
     )

     (defun get-col-owner:object{collection} (creatorGuard:guard)
     (select collections
      (where "creatorGuard" (= creatorGuard)
      )
     ))

  (defun get-wl-collection ([object:{whitelisted}])
    @doc "pull list of whitelist ID's for all collections."
        (keys whitelist-table)
  )

  (defun get-wl-by-name:[object:{whitelisted}]
    (collection:string)
    @doc "Returns list of whitelist ID's by collection name."
      (select whitelist-table
        (where "collection" (= collection)))
    )

(defun get-collection-data:object{collection} (collection:string)
  (read collections collection)
)

(defun get-all-collection-names:object{collection} ()
  @doc "Returns a list of all collections."
  (keys collections)
)
(defconst name "name")

       (defun get-all-collections3 ()
       @doc "Returns a list of all collections."
     (select collections (constantly true))
   )


(defun get-totalSupply-for-collection:decimal (collection:string)
(at "totalSupply" (read collections collection ["totalSupply"]))
)

(defun get-currentIndex-for-collection:integer (collection:string)
(at "currentIndex" (read collections collection ["currentIndex"]))
)

; Look up all collection details.  Query with /local
  (defun get-details()
  (keys collections)
)

; Look up NFT collection by name.  Query with /local
(defun get-nft-collection:object{collection}
    ( name:string
    )
    (read collections name)
  )

  (defun get-nfts()
(keys nft-table)
)

  (defun get-category:object{collection} (name:string)
    (with-read collections COLLECTIONS {
      'category:= category
    }
    category
    ))

; #############################################
;                 Splitter Account
; #############################################


  (defcap SPLITTER ()
    @doc "Checks to make sure the guard for the given account name is satisfied"
   true
  )

  (defun require-SPLITTER ()
    @doc "The function used when building the user guard for managed accounts"
    (require-capability (SPLITTER))
  )

  (defun create-SPLITTER-guard ()
    @doc "Creates the user guard"
    (create-user-guard (require-SPLITTER))
  )

  (defun get-SPLITTER-account ()
    (create-principal (create-SPLITTER-guard))
  )

; #############################################
;        Offline Collection Creation
; #############################################

;  (defun create-offchain-multiple (array:[object])
;  (map (offchain-token-id-and-manifest) array)
;  )

(defun hash-contents:string
  ( uri:string
    precision:integer
    policies:[module{kip.token-policy-v2}]
  )
  (hash {'uri: uri, 'precision:precision, 'policies:policies})
)

(defun hash-sale:string 
  (token-id:string
    seller:string
    end-time:time
    )
    (let ((ct:time (curr-time)))
    (hash {'token-id: token-id, 'seller:seller, 'policies:end-time, 'current-time:ct})
    )
    )

  (defun init ()
    (with-capability (GOVERNANCE)
      ;  (coin.create-account KDA_BANK_ACCOUNT (kda-bank-guard))
      (coin.create-account (get-SPLITTER-account) (create-SPLITTER-guard))
    )
  )

  )


(if (read-msg "upgrade")
"Upgrade Complete"
[
  (create-table collections)
  (create-table bankInfo)
  (create-table minted-tokens)
  (create-table whitelist-table)
  (create-table tiers)
  (create-table tdata)
  (create-table tier-data)
  (create-table nft-table)
  (create-table auctions)
  ;  (create-table auction-bids)
  ;  (create-table managed-accounts)
  (init)
]
)
