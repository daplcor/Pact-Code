(namespace "free")

(define-keyset "free.ku-admin" (read-keyset "ku-admin"))
(define-keyset "free.ku-ops" (read-keyset "ku-ops"))


(module ku-market GOVERNANCE

    (implements kip.token-policy-v1)
    (use kip.token-policy-v1 [token-info])

    (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.ku-admin" )))

    (defcap OPS()
    (enforce-guard (keyset-ref-guard "free.ku-ops" )))

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


  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      royalty-payout:decimal
      creator:string
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
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

  (defschema create-account-params 
    owner:string ; k:string 
    guard:guard 
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
  (defschema collection-schema
    provenance-hash:string ; combined token hash
    tokens-list:[string] ; list of image hashes
    creator:string ; k:account 
    creator-guard:guard
    total-supply:integer ; token supply
    mint-starts:time
    mint-end-time:time 
    whistlist-mint-time:time 
    royalty-receiver:string ; account which receives the royalty
    royalty-rate:decimal
    mint-price:decimal 
    name:string
    description:string
    category:string 
    mint-number:integer
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

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
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
    

  ; ============================================ 
  ; ==                 Tables                 ==
  ; ============================================

  (deftable accounts:{account})
  (deftable collection-info:{collection-schema})
  (deftable reservations:{reservation})
  (deftable manifest-datum:{nft-manifest-datum})
  (deftable account-details:{account-schema})
  (deftable metadata:{token-metadata})
  (deftable quotes:{quote-schema})
  (deftable mint-status:{mint-schema})
  (deftable mintp:{mint-params})
  (deftable account-params:{create-account-params})

 ; ============================================
  ; ==         Initialize Collection          ==
  ; ============================================

  (defun create-collection (
    provenance:string
    tokens-list:[string]
    creator:string
    creator-guard:guard
    total-supply:integer
    mint-starts:time
    mint-end-time:time
    whitelist-mint-time:time
    royalty-receiver:string
    royalty-rate:decimal
    mint-price:decimal
    name:string
    description:string
    category:string
    mint-number:integer
    fungible:module{fungible-v2})
    (enforce (= (length tokens-list) total-supply) "Total-supply and tokens-list length does not match")
    (let ( (creator-details:object (fungible::details creator ))
            )
      (fungible::enforce-unit royalty-rate)
      (enforce (>= mint-end-time mint-starts) Invalid_Mint_Time)
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "Invalid royalty rate")
        (enforce (>= mint-price 0.0) Invalid_NFT_Price)
        (enforce (>= total-supply 1) Invalid_Collection_Size)
        (enforce (!= "" name) Invalid_Collection_Name))

    (insert collection-info COLLECTION_INFO {
      "provenance-hash": provenance,
      "tokens-list": tokens-list,
      "creator": creator,
      "creator-guard": creator-guard,
      "total-supply": total-supply,
      "mint-starts": mint-starts,
      "mint-end-time": mint-end-time,
      "whitelist-mint-time": whitelist-mint-time,
      "royalty-receiver": royalty-receiver,
      "royalty-rate": royalty-rate,
      "mint-price": mint-price,
      "name": name,
      "description": description,
      "category": category,
      "mint-number": 0,
      "fungible": fungible
    })
    (write mint-status MINT_STATUS {
        "current-length": (length tokens-list),
        "tokens-list": (map (str-to-int 64) tokens-list),
        "status": MINT_STARTED,
        "public-minted": 0.0
     }))


  ; ============================================
  ; ==               Constants                ==
  ; ============================================

    (defconst Invalid_Collection_Name "Invalid_Collection_Name")
    (defconst Invalid_Collection_Size "Invalid_Collection_Size")
    (defconst Invalid_NFT_Price "Invalid_NFT_Price")
    (defconst Invalid_Mint_Time "Invalid_Mint_Time")
    (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")
    (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")
    (defconst MINT_STATUS "mint-status")
    (defconst COLLECTION_INFO "collection-info")
    (defconst MINT_COMPLETED "mint-completed")
    (defconst MINT_STARTED "mint-started")  
    (defconst Account_Exists "account-exists")
    (defconst category "category")


  ; ============================================
  ; ==           Mint Functionality           ==
  ; ============================================


  (defun mint-nft:bool
    ( { "minter"; minter,
      "name": nft-name,
      "description": description,
      "content-hash": content-hash,
      "spec": spec,
      "collection-name": collection-name,
      "content-uri": content-uri,
   }
    )
    (let
      (
        (nft (mint-params))
      )

    (bind nft
      { 'name := nft-name
      , 'description := description
      , 'content-hash := content-hash
      , 'spec := spec
      , 'collection-name := collection-name
      , 'owner := minter
      , 'mint-time := mint-time
      , 'content-uri := content-uri
      }

    (bind (get-nft-collection collection-name)
    { 'creator := creator
    , 'provenance-hash := provenance-hash
    }

  (let*
    (
      (datum-object
        { 'name: nft-name
        , 'description: description
        , 'content-hash: content-hash
        , 'spec: spec
        , 'creator: creator
        , 'collection-name: collection-name
        , 'mint-time: mint-time
        , 'content-uri: content-uri
        , 'provenance-hash: provenance-hash
        })
      (minter-guard (at 'guard (ku-market.get-account minter)))
      (datum-uri (kip.token-manifest.uri "pact:schema" "free.ku-market"))
      (manifest-datum (kip.token-manifest.create-datum datum-uri datum-object))
      (manifest-uri content-uri)
      (nft-manifest (kip.token-manifest.create-manifest manifest-uri [manifest-datum]))
      (token-id content-hash)
      (token-precision 0)
    )
    (marmalade.ledger.create-token token-id token-precision nft-manifest ku-tk-policy)
    (marmalade.ledger.create-account token-id minter minter-guard)
    (marmalade.ledger.mint token-id minter minter-guard 1.0)
  ))))
  true
)

(defun create-account:bool
  ( params:object{create-account-params}
  )
  (enforce-guard OPS)
  (bind params 
    { 'owner := owner 
    , 'guard := guard  
    }
  (with-default-read accounts owner
    { 'active: false 
    }
    { 'active := active 
    }
    (enforce (not active) Account_Exists)
    (with-capability (CREATE_ACCOUNT owner)
      (enforce-guard guard)
      (write accounts owner 
        { 'owner: owner 
        , 'active: true 
        , 'guard: guard 
        }))))
  true
)  

  ; ============================================
  ; ==             Policies                   ==
  ; ============================================

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-marmalade-ledger)
    (require-capability (OPS))
  )

  (defun enforce-marmalade-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-marmalade-ledger)
    (enforce false "Burn is not allowed")
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
      , 'creator:= creator:string
      , 'royalty-rate:= royalty-rate:decimal
      }
      (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          { 'price := price:decimal
          , 'recipient := recipient:string
          }
          (let* ((sale-price:decimal (* amount price))
                 (royalty-payout:decimal
                    (floor (* sale-price royalty-rate) (fungible::precision)))
                 (payout:decimal (- sale-price royalty-payout)) )
            (if
              (> royalty-payout 0.0)
              (fungible::transfer buyer creator royalty-payout)
              "No royalty")
            (fungible::transfer buyer recipient payout)))
            true
        ))
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string ; unused
      amount:decimal
      sale-id:string
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-marmalade-ledger)
    (enforce-sale-pact sale-id)
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (fungible:module{fungible-v2} (at 'fungible spec) )
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient))
            (sale-price:decimal (* amount price)) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes-table sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price spec))
    )
    true
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-marmalade-ledger)
    true
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-marmalade-ledger)
    (enforce false "xtransfer prohibited")
  )

  
  

  ; ============================================
  ; ==             Whitelisting               ==
  ; ============================================

  (defun reserve-admin:string (collection-name:string account:string)
   @doc "Reserves an NFT from the collection if possible."

   (with-capability (OPS)
     (reserve-internal collection-name account 1.0)
   )
 )

 (defun reserve:string (collection-name:string account:string)
    @doc "Reserves an NFT from the collection if possible."

    (with-capability (RESERVE)
      (reserve-internal collection-name account 0.0)
    )
  )

 (defun reserve-free:string (collection-name:string account:string)
   @doc "Reserves for free, if the account has available free mints"
   (with-capability (RESERVE)
     (let
       (
         (avail:decimal
           (free.ku-whitelist.get-available-free collection-name account))
       )
       (enforce (> avail 0.0) "No available free mints.")
       (free.ku-whitelist.decrement-available-free-with-owner
         collection-name account 1.0)
       (reserve-internal collection-name account 1.0)
     )
   )
 )

 (defun reserve-internal:string
    (
      COLLECTION_INFO:string
      account:string
    )
    @doc "Private function for reservation"

    (require-capability (RESERVE))

    (with-read collection-info COLLECTION_INFO
      { "provenance-hash": provenance,
      "tokens-list": tokens-list,
      "creator": creator,
      "creator-guard": creator-guard,
      "total-supply": total-supply,
      "mint-starts": mint-starts,
      "mint-end-time": mint-end-time,
      "whitelist-mint-time": whitelist-mint-time,
      "royalty-receiver": royalty-receiver,
      "royalty-rate": royalty-rate,
      "mint-price": mint-price,
      "name": name,
      "description": description,
      "category": category,
      "mint-number": mint-number,
      "fungible": fungible
      }
      (enforce (>= (curr-time) mint-starts) "The mint hasn't started yet")
      (enforce (< mint-number total-supply) "Can't mint more than total supply")
      (enforce (<= (curr-time) mint-end-time) "Cannot reserve from a closed collection")

      (if (= status COLLECTION_STATUS_WHITELIST)
        (free.ku-whitelist.enforce-whitelisted collection-name account)
        []
      )
      (if (= status COLLECTION_STATUS_WHITELIST_FREE)
        [
          (free.ku-whitelist.enforce-whitelisted collection-name account)
          (enforce (= discount 1.0) "Free must be free")
        ]
        []
      )

      (let
        (
          (mint-price:decimal
            (*
              (- 1.0 discount)
              (at "mint-price" (get-current-price-for-collection mint-number payouts))
            )
          )
          (id:string (format "{}" [(floor mint-number)]))
        )

        (if (> mint-price 0.0)
          (fungible::transfer account creator royalty-payout)
          []
        )

        ; Use the current supply minted as an id and insert into reservations
        (let
          (
            (r:object{reservation}
                { "collection": collection-name
              , "id": id
              , "account": account
              , "minted": false
              }
            )
          )
          (insert reservations
            (get-id collection-name id)
            r
          )

          (emit-event (RESERVED r))

          ; Increment the supply minted
          (update collection-info collection-name
            { "mint-number": (+ mintnumber 1.0) }
          )
        )
      )
    )
  )

  ; ============================================
  ; ==         Get Detail Functions           ==
  ; ============================================

; Look up all collection details.  Query with /local
  (defun get-details:object{collection-schema} ()
  (read collection-info COLLECTION_INFO)
 )

 ; Look up NFT collection by name.  Query with /local
 (defun get-nft-collection:object{collection-schema}
     ( name:string
     )
     (read collection-info name)
   )

   (defun get-category:object{collection-schema} (name:string)
    (with-read collection-info COLLECTION_INFO {
      'category:= category
    }
    category
    )
    )

; Look up collection royalty receiver.  Query with /local
(defun get-payee:object{collection-schema} ()
  (with-read collection-info COLLECTION_INFO {
    'royalty-receiver:= royalty-receiver
  }
  royalty-receiver
  )
  )

  (defun get-current-price-for-collection:decimal (name:string)
  (with-read collection-info COLLECTION_INFO {
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
["Upgrade Complete"]
[
  (create-table free.ku-market.accounts)
  (create-table free.ku-market.collection-info)
  (create-table free.ku-market.reservations)
  (create-table free.ku-market.account-details)
  (create-table free.ku-market.metadata)
  (create-table free.ku-market.manifest-datum)
  (create-table free.ku-market.quotes)
  (create-table free.ku-market.mint-status)
  (create-table free.ku-market.mintp)
  (create-table free.ku-market.account-params)

  
]
["no init needed"])
