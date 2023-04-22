(namespace "free")

(define-keyset "free.ku-admin" (read-keyset "ku-admin"))
(define-keyset "free.ku-ops" (read-keyset "ku-ops"))


(module ku-market GOVERNANCE
  
    (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.ku-admin" ))
    (compose-capability (OPS_INTERNAL))
    )

    (defcap OPS()
    (enforce-guard (keyset-ref-guard "free.ku-ops" ))
    (compose-capability (OPS_INTERNAL))
    )

  (defcap CREATECOL()
  true
  )

  (defcap EDITCOL (creator-guard:guard) 
  @managed
  (bind (get-col-owner creator-guard) (enforce-guard creator-guard))
)


  (defcap EDIT()
  (compose-capability (OPS))
  (compose-capability (CREATOR))
  )
    
  (defcap CREATOR()
  (enforce-guard (keyset-ref-guard "creator-guard"))
  )
   
  (defcap OPS_INTERNAL ()
  (compose-capability (MINT))
  )
  
  (defcap WHITELIST_UPDATE () 
  true
)


(defcap MINT () 
(compose-capability (WHITELIST_UPDATE))
true
)

(defcap MINT_EVENT 
(
  collection:string 
  tier-id:string 
  account:string 
  amount:integer
)
@event true
)


  
; #################################################################
; #                      Schema Details                           #
; #################################################################

; NFT collections are stored in the nft-collections table.
  ; An NFT collection is uniquely identified by its name across all k:accounts.
  ; The creator is the k:account of the original creator.
  ; The name is the collection name, and also its id. 
  ; num-minted is the current number of NFTs that have been minted in this 
  ; collection.
  ; max-size is the total number of NFTs in this collection.
  ; mint-price is the price to mint each NFT in this collection.
  ; mint-guard defines who can mint NFTs in this collection.
  ; mint-royalties define who gets paid when NFTs in this collection are 
  ; minted.
  ; sale-royalties define who gets paid when NFTs in this collection are sold.
  (defschema collection
    @doc "Stores the name of the collection, the tiers, \
    \ the total supply of the collection. \
    \ The id is the name of the collection."
    name:string
    total-supply:integer
    bank-account:string
    creator:string
    creator-guard:guard
    category:string
    bank-guard:guard
    provenance:string
    root-uri:string
    description:string
    current-index:integer
    fungible:module{fungible-v2}
    tiers:[object:{tier}]
  )

      (defschema minted-token
        @doc "Stores the data for a minted token. \
        \ The id is the collection, tier-id, account, and token-id."
        collection:string
        account:string
        guard:guard
        token-id:integer
        hash:string
        revealed:bool
      )
    
   
      (defschema tier
        @doc "Stores the start time, end time, tier type (WL, PUBLIC), \
        \ tier-id, cost for this tier to mint, \
        \ and the limit for each minter."
        tier-id:string
        tier-type:string
        start-time:time
        end-time:time
        cost:decimal
        limit:decimal
      )
    
  (defschema tier-whitelist-data
  @doc "A data structure for the whitelist data for a tier"
  tier-id:string
  accounts:[string]
  )
  
      (defschema whitelisted
        @doc "Stores the account of the whitelisted user, the tier-id, \
        \ and amount they have minted. The id is 'collection:tier-id:account'."
        account:string
        tier-id:string 
        mint-count:integer 
      )


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
      bank-guard:guard
  )
    @doc "Creates a collection with the provided data."
    (with-capability (CREATECOL)
      ; Validate the collection tiers
      (enforce (> (at "total-supply" collection-data) 0.0) "Total supply must be greater than 0")
      (validate-tiers (at "tiers" collection-data))
      ; Create the bank account
      (insert collections (at "name" collection-data)
        (+ 
          { "fungible": fungible
          , "current-index": 1
          , "bank-account": (create-principal bank-guard)
          , "bank-guard": bank-guard
          , "total-supply": (floor (at "total-supply" collection-data))
          }
          collection-data
        )
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
  )
  

  (defun validate-tiers:bool (tiers:[object:{tier}])
  @doc "Validates the tier start and end time, ensuring they don't overlap \
  \ and that start is before end for each."
  (let*
    (
      (no-overlap
        (lambda (tier:object{tier} other:object{tier})
          ;; If the other is the same as the tier, don't check it
          (if (!= (at "tier-id" tier) (at "tier-id" other))
            (enforce 
              (or
                ;; Start and end of other is before start of tier
                (and? 
                  (<= (at "start-time" other))
                  (<= (at "end-time" other)) 
                  (at "start-time" tier)
                )
                ;; Start and end of other is after end of tier
                (and?
                  (>= (at "end-time" other))
                  (>= (at "start-time" other)) 
                  (at "end-time" tier)
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
            (<= (at "start-time" tier) (at "end-time" tier)) 
            "Start must be before end"
          )
          (enforce
            (or 
              (= (at "tier-type" tier) TIER_TYPE_WL)
              (= (at "tier-type" tier) TIER_TYPE_PUBLIC)
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
)

(defun update-collection-uri 
  (
    collection:string
    uri:string
  )
  (with-capability (OPS)
    (update collections collection
      { "root-uri": uri }
    )
  )
)
    
  ; ============================================
  ; ==               Constants                ==
  ; ============================================

    (defconst Invalid_Collection_Name "Invalid_Collection_Name")
    (defconst Invalid_Collection_Size "Invalid_Collection_Size")
    (defconst Invalid_NFT_Price "Invalid_NFT_Price")
    (defconst Invalid_Mint_Time "Invalid_Mint_Time")
    (defconst TIER_TYPE_WL:string "WL")
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
    (defconst creator-guard "creator-guard")
    (defconst description "description")
    (defconst fungible "fungible")
    (defconst mint-end-time "mint-end-time")
    ;  (defconst mint-price 0.0)
    (defconst mint-starts "mint-starts")
       
    (defconst tokens-list "tokens-list")
    (defconst total-supply "total-supply")
    (defconst status "status")
    (defconst COLLECTION_STATUS_WHITELIST "COLLECTION_STATUS_WHITELIST")
    (defconst collection-name "collection-name")
  

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
          (current-index (at "current-index" collection-data))
          (tier (get-current-tier (at "tiers" collection-data)))
          (tier-id (at "tier-id" tier))
        )
        
        (mint-internal 
          collection 
          account 
          guard
          amount 
          tier-id 
          current-index
        )
      )
    )
  )

  (defun mint:bool 
    (
      collection:string 
      account:string 
      amount:integer
    )
    @doc "Mints the given amount of tokens for the account. \
    \ Gets the current tier and tries to mint from it. \
    \ If the tier is a whitelist, checks that the account is whitelisted \
    \ and that the mint count is wthin the limit. \
    \ If the tier is public, it allows anyone to mint."
    (enforce (> amount 0) "Amount must be greater than 0")

    (with-capability (MINT)
      (with-read collections collection
        { "current-index":= current-index
        , "total-supply":= total-supply
        , "fungible":= fungible:module{fungible-v2}
        , "bank-account":= bank-account:string
        , "bank-guard":= bank-guard
        , "tiers":= tiers
        }
        (enforce 
          (<= (+ (- current-index 1) amount) total-supply) 
          "Can't mint more than total supply"
        )

        (bind (get-current-tier tiers)
          { "cost":= cost
          , "tier-type":= tier-type
          , "tier-id":= tier-id
          , "limit":= mint-limit
          }
          (let 
            (
              (mint-count (get-whitelist-mint-count collection tier-id account))
            )  
            ;; If the tier is public, anyone can mint
            ;; If the mint count is -1, the account is not whitelisted
            (enforce 
              (or 
                (= tier-type TIER_TYPE_PUBLIC)
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

            ;; Transfer funds if the cost is greater than 0
            (if (> cost 0.0)
              (fungible::transfer-create 
                account 
                bank-account 
                bank-guard 
                (* amount cost)
              )
              []
            )

            ;; Handle the mint
            (if (= tier-type TIER_TYPE_WL)
              (update-whitelist-mint-count collection tier-id account (+ mint-count amount))
              []
            )
            (mint-internal 
              collection 
              account 
              (at "guard" (fungible::details account)) 
              amount
              tier-id 
              current-index
            )
          )
        )
      )
    )
  )

  (defun mint-internal:bool
    (
      collection:string 
      account:string 
      guard:guard
      amount:integer
      tier-id:string
      current-index:integer
    )
    (require-capability (MINT))  

    (update collections collection 
      { "current-index": (+ current-index amount) }
    )
    (map 
      (mint-token collection account guard) 
      (map (+ current-index) (enumerate 0 (- amount 1)))
    )
    (emit-event (MINT_EVENT collection tier-id account amount))
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
      , "hash": ""
      , "revealed": false
      }
    )
  )

(defschema token-data
  @doc "The information necessary to mint the token on marmalade"
  precision:integer
  scheme:string 
  data:string
  datum:object 
  policy:module{kip.token-policy-v1}
)

(defschema in-token-data
  @doc "The information necessary to mint the token on marmalade"
  scheme:string 
  data:string
  datum:object
)

(defun create-marmalade-token:string 
  (
    account:string
    guard:guard 
    mint-token-id:string
    t-data:object{token-data}
  )
  @doc "Requires Private OPS. Creates the token on marmalade using the supplied data"
  (require-capability (OPS_INTERNAL))

  (bind t-data
    { "precision":= precision
    , "scheme":= scheme
    , "data":= data
    , "datum":= datum
    , "policy":= policy
    }
    (let*
      (
        (uri (kip.token-manifest.uri scheme data))
        (datum-complete (kip.token-manifest.create-datum uri datum))
        (manifest (kip.token-manifest.create-manifest uri [datum-complete]))
        (token-id (concat ["t:" (at "hash" manifest)]))
      )
      (update minted-tokens mint-token-id
        { "revealed": true
        , "hash": (at "hash" manifest)
        }
      )

      (marmalade.ledger.create-token 
        token-id
        precision
        manifest
        policy
      )
      ;  (install-capability (marmalade.ledger.MINT token-id account 1.0))
      (marmalade.ledger.mint
        token-id
        account
        guard
        1.0
      )
      token-id
    )
  )
)

(defun get-unrevealed-tokens:object{minted-token} ()
  @doc "Returns a list of unrevealed tokens."
  (select minted-tokens (where "revealed" false))
)

(defun reveal-token:string 
  (
    m-token:object{minted-token}
    t-data:object{in-token-data}
    precision:integer
    policy:module{kip.token-policy-v1}
  )
  @doc "Requires OPS. Reveals the token for the given account."
  (with-capability (OPS)
    (bind m-token
      { "collection":= collection
      , "token-id":= token-id
      , "account":= account
      , "guard":= guard
      }

      (create-marmalade-token 
        account 
        guard 
        (get-mint-token-id collection token-id)
        (+ 
          t-data 
          { "precision": precision
          , "policy": policy
          }
        )
      )
    )
  )
)

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
    @doc "Requires OPS. Adds the accounts to the whitelist for the given tier."
    (with-capability (OPS)
      (let
        (
          (handle-tier-data 
            (lambda (tier-data:object{tier-whitelist-data})
              (let
                (
                  (tier-id (at "tier-id" tier-data))
                  (whitelist (at "accounts" tier-data))
                )
                (map (add-to-whitelist collection tier-id) whitelist)
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
@doc "Requires OPS. Adds the accounts to the whitelist for the given tier."
(with-capability (OPS)
  (let
    (
      (tier-id (at "tier-id" tier-data))
      (whitelist (at "whitelist" tier-data))
    )
    (map (add-to-whitelist collection tier-id) whitelist)
  )
)
)

(defun add-to-whitelist:string 
(
  collection:string 
  tier-id:string
  account:string 
)
@doc "Requires private OPS. Adds the account to the whitelist for the given tier."
(require-capability (OPS))

(insert whitelist-table (concat [collection ":" tier-id ":" account])
  { 
    "tier-id": tier-id
  , "account": account
  ,  "mint-count": 0
  }
)
)

(defun is-whitelisted:bool 
(
  collection:string 
  tier-id:string 
  account:string
)
@doc "Returns true if the account is whitelisted for the given tier."
(let
  (
    (whitelist-id (get-whitelist-id collection tier-id account))
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
  tier-id:string 
  account:string
)
(let
  (
    (whitelist-id (get-whitelist-id collection tier-id account))
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
  tier-id:string 
  account:string
)
(concat [collection ":" tier-id ":" account])
)

(defun update-whitelist-mint-count 
(
  collection:string 
  tier-id:string 
  account:string 
  count:integer
)
@doc "Requires Whitelist Update. Updates the mint count for the given account in the whitelist."
(require-capability (WHITELIST_UPDATE))

(let
  (
    (whitelist-id (get-whitelist-id collection tier-id account))
  )
  (update whitelist-table whitelist-id
    { "mint-count": count }
  )
)
)

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

  (defun get-current-tier:object{tier} (tiers:[object:{tier}])
    @doc "Gets the current tier from the list based on block time"
    (let*
      (
        (now (at "block-time" (chain-data)))
        (filter-tier
          (lambda (tier:object{tier})
            (if (= (at "start-time" tier) (at "end-time" tier)) 
              (>= now (at "start-time" tier))  
              (and? 
                (<= (at "start-time" tier))
                (> (at "end-time" tier))
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

  (defun get-all-nft ([object:{minted-token}])
        @doc "Returns a list of all tokens."
    (keys minted-tokens)
     )

     (defun get-col-owner:object{collection} (creator-guard:guard)
     (select collections
      (where "creator-guard" (= creator-guard)
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

(defun get-all-collections:object{collection} ()
  @doc "Returns a list of all collections."    
  (keys collections)
)
(defconst name "name")

(defun get-all-collections1:string (name:[object:{collections}])
  (select collections 
    (where "name" (= name))) 
)

       (defun get-all-collections3 () 
     (select collections (constantly true))
   )

(defun get-collection-uri:string (collection:string)
  (at "root-uri" (read collections collection ["root-uri"]))
)

(defun get-total-supply-for-collection:decimal (collection:string)
(at "total-supply" (read collections collection ["total-supply"]))
)

(defun get-current-index-for-collection:integer (collection:string)
(at "current-index" (read collections collection ["current-index"]))
)

(defun get-bank-for-collection:string (collection:string)
(at "bank-account" (read collections collection ["bank-account"]))
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

  (defun get-category:object{collection} (name:string)
    (with-read collections COLLECTIONS {
      'category:= category
    }
    category
    )
    )

  )


(if (read-msg "upgrade")
"Upgrade Complete"
[
  (create-table free.ku-market.collections)
  (create-table free.ku-market.minted-tokens)
  (create-table free.ku-market.whitelist-table)
  (create-table free.ku-market.tiers)
  (create-table free.ku-market.tdata)
  (create-table free.ku-market.tier-data)
]
)
