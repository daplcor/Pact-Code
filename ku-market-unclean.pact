

  (namespace "free")

  (define-keyset "free.ku-admin" (read-keyset "ku-admin"))
  (define-keyset "free.ku-ops" (read-keyset "ku-ops"))


  (module ku-market GOVERNANCE

      ;  (implements free.ku-tk-policy)
      (implements free.ku-tk-policy)
      (use free.ku-tk-policy [token-info])
      
      (defcap GOVERNANCE ()
      (enforce-guard (keyset-ref-guard "free.ku-admin" ))
      (compose-capability (OPS_INTERNAL))
      )

      (defcap OPS()
      (enforce-guard (keyset-ref-guard "free.ku-ops" ))
      (compose-capability (OPS_INTERNAL))
      )

      (defcap RESERVES ()
      true
    )

    (defcap MINT ()
      true
    )

    (defcap RESERVE ()
      true
    )
        
    (defcap RESERVED (reservation:object{reservation})
      @event
      true
    )

    (defcap OPS_INTERNAL ()
    true
    )
    

    (defschema reservation
      @doc "A reserved NFT in a collection. OPS will use these to mint NFTs. \
      \ ID is collection|id."
      collection:string
      id:string
      account:string
      minted:bool
    )



      (defschema mint-params
        minter:string ; k:account
        name:string 
        description:string
        content-hash:string 
        spec:object
        collection-name:string
        content-uri:object{kip.token-manifest.mf-uri}
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
      name:string
      provenance-hash:string ; combined token hash
      tokens-list:[string] ; list of image hashes
      creator:string ; k:account 
      creator-guard:guard
      total-supply:integer ; token supply
      tiers:[object:{tier}]
      description:string
      category:string 
      current-index:integer
      fungible:module{fungible-v2}
    ) 


    (defschema account-schema
      account:string
      minted:integer
    )

    (defschema account 
      owner:string ; k:string 
      guard:guard 
      active:bool 
    )

    (defschema token-metadata
      name:string
      description:string
      image:string
      image-hash:string
      attributes:[object{traits-schema}]
    )

    

      (defschema nft-manifest-datum 
        name:string 
        description:string 
        content-hash:string 
        spec:object 
        creator:string  
        collection-name:string 
        content-uri:object{kip.token-manifest.mf-uri}
        mint-index:integer 
        mint-time:time 
      )

      (defschema mint-schema
          tokens-list:[integer]
          current-length:integer
          status:string
          public-minted:decimal
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
          limit:integer
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

    (deftable accounts:{account})
    (deftable collections:{collection})
    ;  (deftable reservations:{reservation})
    ;  (deftable manifest-datum:{nft-manifest-datum})
    ;  (deftable account-details:{account-schema})
    ;  (deftable metadata:{token-metadata})
    (deftable whitelist-table:{whitelisted})
    ;  (deftable mint-status:{mint-schema})
    (deftable tiers:{tier})
    (deftable minted-tokens:{minted-token})
    ;  (deftable mintp:{mint-params})

    ; ============================================
    ; ==         Initialize Collection          ==
    ; ============================================
    (defun enforce-init:bool
      ( token:object{token-info} )
      (enforce-ledger)
      true
    )

    (defun create-collection:string (
      name:string
      provenance:string
      tokens-list:[string]
      creator:string
      creator-guard:guard
      total-supply:integer
      description:string
      category:string
      current-index:integer
      fungible:module{fungible-v2})
      (enforce (= (length tokens-list) total-supply) "Total-supply and tokens-list length does not match")
            ;  (enforce (>= mint-end-time mint-starts) Invalid_Mint_Time)
        ;  (enforce (=
        ;    (at 'guard creator-details) creator-guard)
        ;    "Creator guard does not match")
        (enforce (and
          (enforce (>= mint-price 0.0) Invalid_NFT_Price)
          (enforce (>= total-supply 1) Invalid_Collection_Size)
          (enforce (!= "" name) Invalid_Collection_Name)
        )
      (insert collections (at "name" collection-data) 
      (+
        collection-data
        { "fungible": fungible
        , "current-index": 0
      }
            )
    )))

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
            (enforce 
              (or 
                (> (at "start-time" tier) (at "end-time" other))
                (> (at "start-time" other) (at "end-time" tier))
              )
              "Tiers overlap"
            )
          )
        )
        (validate-tier 
          (lambda (tier:object{tier})
            ;; Enforce start time is before end time, 
            ;; and that the tier type is valid
            (enforce 
              (< (at "start-time" tier) (at "end-time" tier)) 
              "Start is before end"
            )
            (enforce
              (or 
                (= (at "tier-type" tier) TIER_TYPE_WL)
                (= (at "tier-type" tier) TIER_TYPE_PUBLIC)
              )
            )
            ;; Loop through all the tiers and ensure they don't overlap
            (map (no-overlap tier) tiers)
          )
        )
      )
      (map (validate-tier) tiers)
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
      (defconst name "name")
      (defconst provenance "provenance")
      (defconst royalty-rate 0.0)
      (defconst royalty-receiver "royalty-receiver")
      (defconst tokens-list "tokens-list")
      (defconst total-supply "total-supply")
      (defconst status "status")
      (defconst COLLECTION_STATUS_WHITELIST "COLLECTION_STATUS_WHITELIST")
      (defconst collection-name "collection-name")
      (defconst COLLECTION_STATUS_WHITELIST_FREE "COLLECTION_STATUS_WHITELIST_FREE")
      (defconst discount "discount")
      (defconst payouts "payouts")

    ; ============================================
    ; ==           Mint Functionality           ==
    ; ============================================


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

  (defschema minted-token
    @doc "Stores the data for a minted token. \
    \ The id is the collection, tier-id, account, and token-id."
    collection:string
    tier-id:string
    account:string
    token-id:integer
    revealed:bool
  )

  (deftable minted-tokens:{minted-token})

  (defun mint:string 
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
    (with-capability (MINT)
      (let*
        (
          (collection-data (read collections collection))
          (current-index (at "current-index" collection-data))
          (tier (get-current-tier (at "tiers" collection)))
          (tier-type (at "tier-type" tier))
          (tier-id (at "tier-id" tier))
          (mint-limit (at "mint-limit" tier))
          (mint-count (get-whitelist-mint-count collection tier-id account))
        )
        (enforce 
          (or 
            (= tier-type TIER_TYPE_PUBLIC)
            (is-whitelisted collection tier-id account)
          )
          "Account is not whitelisted"
        )
        (enforce 
          (or 
            (= mint-limit -1)
            (< (+ mint-count amount) mint-limit)
          )
          "Mint limit reached"
        )

        (update-whitelist-mint-count collection tier-id account (+ mint-count amount))
        (map (mint-token collection tier-id account) (map (+ current-index)) (enumerate 0 amount))
        (emit-event (MINT_EVENT collection tier-id account amount))
      )
    )
  )

  (defun mint-token:string (collection:string tier-id:string account:string token-id:integer)
    @doc "Mints a single token for the account."
    (insert minted-tokens (get-mind-token-id collection tier-id account token-id)
      { "collection": collection
      , "tier-id": tier-id
      , "account": account
      , "token-id": token-id
      , "revealed": false
      }
    )
  )

  

  (defun reveal-token:string 
    (
      m-token:object{minted-token}
      t-data:object{token-data}
    )
    @doc "Requires OPS. Reveals the token for the given account."
    (require-capability (OPS)
      (let
        (
          (collection (at "collection" m-token))
          (tier-id (at "tier-id" m-token))
          (account (at "account" m-token))
          (token-id (at "token-id" m-token))
        )

        (update minted-tokens (get-mint-token-id collection tier-id account token-id)
          { "revealed": true }
        )

        (create-marmalade-token t-data)
      )
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

  (defun create-marmalade-token:string (t-data:object{token-data})
    @doc "Requires Private OPS. Creates the token on marmalade using the supplied data"
    (require-capability (OPS_INTERNAL))

    (let
      (
        (precision (at "precision" t-data))
        (scheme (at "scheme" t-data))
        (data (at "data" t-data))
        (datum (at "datum" t-data))
        (policy (at "policy" t-data))
      )
      (let*
        (
          (uri (kip.token-manifest.uri scheme data))
          (datum-complete (kip.token-manifest.create-datum uri datum))
          (manifest (kip.token-manifest.create-manifest uri [datum-complete]))
        )
        (marmalade.ledger.create-token 
          (concat "t:" (at "hash" manifest)) 
          precision 
          manifest
          policy
        )
      )
    )
  )

  (defun get-unrevealed-tokens[object:{minted-token}] ()
    @doc "Returns a list of unrevealed tokens."
    (select minted-tokens (where "revealed" false))
  )

  (defun get-mint-token-id:string 
    (
      collection:string 
      tier-id:string 
      account:string 
      token-id:integer
    )
    (concat [collection ":" tier-id ":" account ":" (int-to-str 10 token-id)])
  )
)
  


    ; ============================================
    ; ==             Whitelisting               ==
    ; ============================================

    (defcap WHITELIST_UPDATE () 
  true
)

(defschema tier-whitelist-data
  @doc "A data structure for the whitelist data for a tier"
  tier-id:string
  whitelist:[string]
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
      "account": account
      "mint-count": 0.0
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


    (defun get-current-tier-for-collection (collection:string)
    @doc "Gets the current tier for the collection"
    (let*
      (
        (tiers (at "tiers" (read collections collection)))
      )
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
            (and 
              (>= (at "start-time" tier) now)
              (<= (at "end-time" tier) now)
            )
          )
        )
      )
      (at 0 (filter (filter-tier) tiers))
    )
  )


  ; Look up all collection details.  Query with /local
    (defun get-details:object{collection} ()
    (read collections COLLECTIONS)
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

  ; Look up collection royalty receiver.  Query with /local
  (defun get-payee:object{collection} ()
    (with-read collections COLLECTIONS {
      'royalty-receiver:= royalty-receiver
    }
    royalty-receiver
    )
    )

    (defun get-current-price-for-collection:decimal (name:string)
    (with-read collections COLLECTIONS {
      'mint-price:= mint-price
    }
    mint-price
    )
    )

    (defun get-all-reservations:[object{reservation}] ()
      (select reservations (constantly true))
    )

    (defun get-account:object{account}
      ( owner:string ; k:account
      )
      (with-default-read accounts owner
        { 'active: false 
        , 'guard: false 
        }
        { 'active := active 
        , 'guard := guard 
        }
        (enforce active EXC_ACCOUNT_NOT_FOUND)
        { 'owner: owner 
        , 'active: active 
        , 'guard: guard 
        })
    )

    (defun get-reservation:object{reservation} 
      (collection-name:string id:string)
      (read reservations (get-id collection-name id))
    )

    (defun get-unminted-reservations:[object{reservation}] ()
      (select reservations (where "minted" (= false)))
    )

    (defun get-reservations-for-account:[object{reservation}] 
      (account:string)
      (select reservations (where "account" (= account)))  
    )

    

    )


  (if (read-msg "upgrade")
  "Upgrade Complete"
  [
    (create-table free.ku-market.accounts)
    (create-table free.ku-market.collections)
    (create-table free.ku-market.reservations)
    (create-table free.ku-market.account-details)
    (create-table free.ku-market.metadata)
    (create-table free.ku-market.manifest-datum)
    (create-table free.ku-market.mint-status)
    (create-table free.ku-market.mintp)
  ]
  ["no init needed"])
