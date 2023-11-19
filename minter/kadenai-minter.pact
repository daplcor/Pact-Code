(namespace "free")
;This code is meant to work with the Marmalade V2 Minter

(define-keyset "free.ku-admin" (read-keyset "ku-admin"))
(define-keyset "free.ku-ops" (read-keyset "ku-ops"))

(module kadenai-minter GOVERNANCE
    (bless "17Q2ay-ze4sXuVebHg-LCijNpEVQYenz6kMEHN9aekg")
    (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.ku-admin" ))
       )

    (defcap OPS()
    (enforce-guard (keyset-ref-guard "free.ku-ops")))

    (defcap MINT()
    true
    )

(defschema nft
    id:string
    owner:string
  )

  (defschema owner-nfts
    nfts:[string]
  )
  
  (deftable nft-tablev3:{nft})
  (deftable owner-tablev3:{owner-nfts})
  ; Removed these because of inconsistent marm ledgers due to updates
  ;  (deftable nft-table:{nft})
  ;  (deftable owner-table:{owner-nfts})

  (defun mint:bool
    (
      account:string
      uri:string
      precision:integer
      policies:[module{kip.token-policy-v2}]
    )
    @doc "Mints one token and sets owner from account field"
       
    (enforce (= 0 precision) "Precision must be 0")
    ;  (enforce (marmalade.ledger.is-authorized account) "Unauthorized")
   
  (with-capability (MINT) 
    (let*
      (
        (token-id (hash-contents uri precision policies account))
        (guard (at "guard" (coin.details account)))
        (owner account)
      )
      ; Create NFT on marmalade
      (marmalade-v2.ledger.create-token
        token-id
        precision
        uri
        policies
        guard
      )
  
      ; Mint NFT to the account
      (marmalade-v2.ledger.mint
        token-id
        account
        guard
        1.0
      )
  
      ; Add NFT to the NFT table
      (insert nft-tablev3 token-id
        {
          "id": token-id,
          "owner": account
        }
      )
      (with-default-read owner-tablev3 owner
        {"owner": "", "nfts": []} 
        {"nfts":= nft-ids} 
        ;  (enforce (not (= owner-id "")) "Owner id must not be blank.")
        (if (= [] nft-ids)
          (insert owner-tablev3 owner {"nfts": [token-id]})
          (update owner-tablev3 owner {"nfts": (+ [token-id] nft-ids)}))
        )
      
      )
    )
  )
  
(defun hash-contents:string
  ( uri:string
    precision:integer
    policies:[module{kip.token-policy-v2}]
    account:string
    )
    (let 
      (
        (g (at "guard" (coin.details account)))
      )
  (marmalade-v2.ledger.create-token-id {'uri: uri, 'precision:precision, 'policies:policies} g)
  )
)

(defun get-by-account(owner:string)
  "Returns all NFTs owned by the given account"
  (read owner-tablev3 owner)
)

(defun get-uri(owner:string)
  @doc "Returns all NFTs URIs owned by the given account"
  (let ((nfts (at 'nfts (read owner-tablev3 owner))))
    (get-token-uris nfts)
  )
)

(defun get-token-uris(nfts:[string])
  @doc "Returns a list of URIs for the given list of NFT ids"
  (map 
    (lambda (nft) (at 'uri (marmalade-v2.ledger.get-token-info nft))) 
    nfts
  )
)

(defun get-obj(owner:string)
  "Returns all NFTs and URIs owned by the given account as an array of objects"
  (let ((nfts (at 'nfts (read owner-tablev3 owner))))
    (map
      (lambda (nft)
        {"nft-id": nft, "uri": (at 'uri (marmalade-v2.ledger.get-token-info nft))}
      )
      nfts
    )
  )
)

(defun migrate-nfts:bool (account:string nft-ids:[string])
  @doc "Migrates a list of NFT ids to a new owner in owner-table"
  @model [ (property (not (any-empty? nft-ids))) ]
  (with-capability (OPS)
  (let ((owner account))
    (insert owner-tablev3 owner {"nfts": nft-ids})
  )
)))


(if (read-msg "upgrade")
"Upgrade Complete"
[
(create-table nft-tablev3)
(create-table owner-tablev3)
]
)