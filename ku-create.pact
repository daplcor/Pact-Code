(namespace "free")

(define-keyset "free.ku-admin" (read-keyset "ku-admin"))
(define-keyset "free.ku-ops" (read-keyset "ku-ops"))
(define-keyset "free.ku-bank" (read-keyset "ku-bank"))

(module ku-create GOVERNANCE

    (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.ku-admin" ))
    (compose-capability (OPS_INTERNAL))
    )

    (defcap OPS()
    (enforce-guard (keyset-ref-guard "free.ku-ops"))
    (compose-capability (OPS_INTERNAL))
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


(defcap MINT ()
(compose-capability (WHITELIST_UPDATE))
true
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

(defcap WHITELIST:bool (collection:string)
	(enforce-guard (at 'creatorGuard (read collections collection ['creatorGuard])))
	(compose-capability (WLMOD)
))

(defcap WHITELIST:bool (collection:string)
  (enforce
    (or
      (enforce-guard (at 'creatorGuard (read collections collection ['creatorGuard])) false)
      (compose-capabilit (OPS))
    )
    "Must be the collection creator or have OPS capability"
  )
  (compose-capability (WLMOD))
)

(defcap WLMOD ()
	true
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
    totalSupply:integer
    creator:string
    creatorGuard:guard
    category:string
    provenance:string
    rootUri:string
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
        hash:string
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
       (insert collections (at "name" collection-data)
        (+
          { "fungible": fungible
          , "currentIndex": 1
          , "totalSupply": (floor (at "totalSupply" collection-data))
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
)

(defun update-collection-uri
  (
    collection:string
    uri:string
  )
  (with-capability (OPS)
    (update collections collection
      { "rootUri": uri }
    )
  )
)

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
    (defconst KDA_BANK_ACCOUNT:string "ku-bank" )
    (defconst KDA_BANK_GUARD_NAME:string "free.ku-bank")
    (defun kda-bank-guard () (create-module-guard KDA_BANK_GUARD_NAME))

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
    )
    @doc "Mints the given amount of tokens for the account. \
    \ Gets the current tier and tries to mint from it. \
    \ If the tier is a whitelist, checks that the account is whitelisted \
    \ and that the mint count is wthin the limit. \
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
          (let*
            (
              (mint-count (get-whitelist-mint-count collection tierId account))
              (bankAc (get-bank))
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

            ;; Transfer funds if the cost is greater than 0
            ; (if (> cost 0.0)
            ;   (fungible::transfer
            ;     account
            ;     bankAc
            ;     (* amount cost)
            ;   )
            ;
            ;  []
            ; )

            (if (> cost 0.0)
  (let
    (
      (total-cost (* amount cost))
      (creator-amount (* total-cost PERCENT_TO_CREATOR))
      (bank-amount (- total-cost creator-amount))
      (managed-account (get-managed-account))
    )
    (fungible::transfer account managed-account total-cost)
    (fungible::transfer managed-account creator creator-amount)
    (fungible::transfer managed-account bankAc bank-amount)
  )
  []
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
    @doc "Requires creator guard. Adds the accounts to the whitelist for the given tier."
    (with-capability (WHITELIST collection)
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
(with-capability (WHITELIST collection)
  (let
    (
      (tierId (at "tierId" tier-data))
      (whitelist (at "whitelist" tier-data))
    )
    (map (add-to-whitelist collection tierId) whitelist)
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

(defun get-collection-uri:string (collection:string)
  (at "rootUri" (read collections collection ["rootUri"]))
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

  (defun get-category:object{collection} (name:string)
    (with-read collections COLLECTIONS {
      'category:= category
    }
    category
    )
    )

; #############################################
;        Offline Collection Creation
; #############################################

(defun create-offchain-multiple (array:[object])
(map (offchain-token-id-and-manifest) array)
)

(defun offchain-token-id-and-manifest:object (t-data:object)
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
      (token-id (format "t:{}" [(at 'hash manifest)]))
    )
    {"manifest": manifest, "token-id": token-id}
  )
)
)

  (defun init ()
    (with-capability (GOVERNANCE)
      (coin.create-account KDA_BANK_ACCOUNT (kda-bank-guard))
    )
  )

  )


(if (read-msg "upgrade")
"Upgrade Complete"
[
  (create-table free.ku-create.collections)
  (create-table free.ku-create.bankInfo)
  (create-table free.ku-create.minted-tokens)
  (create-table free.ku-create.whitelist-table)
  (create-table free.ku-create.tiers)
  (create-table free.ku-create.tdata)
  (create-table free.ku-create.tier-data)
  (init)
]
)
