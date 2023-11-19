(namespace "free")
;This code is meant to work with the Marmalade V2 Minter

(define-keyset "free.ku-admin" (read-keyset "ku-admin"))
(define-keyset "free.ku-ops" (read-keyset "ku-ops"))

(module kadenai-ng-minter GOVERNANCE
    
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

  (deftable nft-tablev2:{nft})
  (deftable owner-tablev2:{owner-nfts})
  ; Removed these because of inconsistent marm ledgers due to updates
  ;  (deftable nft-table:{nft})
  ;  (deftable owner-table:{owner-nfts})

  (defun mint:bool
    (
      account:string
      uri:string
      precision:integer
      guard:guard
      policies:[module{token-policy-ng-v1}]
    )
    @doc "Mints one token and sets owner from account field"
       
    (enforce (= 0 precision) "Precision must be 0")
    ;  (enforce (marmalade.ledger.is-authorized account) "Unauthorized")
    ; removed (token-id (concat ["t:" hash-id]))

  (with-capability (MINT) 
    (let*
      (
        (token-id (hash-contents uri precision policies))
        (guard (at "guard"(coin.details account)))
        (owner account)
      )
      ; Create NFT on marmalade
      (marmalade-ng.ledger.create-token
        token-id
        precision
        uri
        guard
      )
  
      ; Mint NFT to the account
      (marmalade-ng.ledger.mint
        token-id
        account
        guard
        1.0
      )
  
      ; Add NFT to the NFT table
      (insert nft-tablev2 token-id
        {
          "id": token-id,
          "owner": account
        }
      )
      (with-default-read owner-tablev2 owner
        {"owner": "", "nfts": []} 
        {"nfts":= nft-ids} 
        ;  (enforce (not (= owner-id "")) "Owner id must not be blank.")
        (if (= [] nft-ids)
          (insert owner-tablev2 owner {"nfts": [token-id]})
          (update owner-tablev2 owner {"nfts": (+ [token-id] nft-ids)}))
        )
      
      )
    )
  )
  
(defun hash-contents:string
  ( guard
    precision:integer
    policies:[module{marmalade-ng.token-policy-ng-v1}]
    )
  (marmalade-ng.ledger.create-token-id guard uri)
)

(defun get-by-account(owner:string)
  "Returns all NFTs owned by the given account"
  (read owner-tablev2 owner)
)

(defun get-uri(owner:string)
  @doc "Returns all NFTs URIs owned by the given account"
  (let ((nfts (at 'nfts (read owner-tablev2 owner))))
    (get-token-uris nfts)
  )
)

(defun get-token-uris(nfts:[string])
  @doc "Returns a list of URIs for the given list of NFT ids"
  (map 
    (lambda (nft) (at 'uri (marmalade-ng.ledger.get-token-info nft))) 
    nfts
  )
)

(use n_442d3e11cfe0d39859878e5b1520cd8b8c36e5db.ledger)
    (let ((nfts (at 'id (list-balances "k:2c8d728e9f7faa69e432d0753907a4e84719f6a570d56f086c8616ac22f87652"))
    (map
    (lambda (nft)
    { "id": nft, "uri": (get-uri nft)}
    )
    nfts
    ))))

    (defun get-obj(owner:string)
  "Returns all NFTs and URIs owned by the given account as an array of objects"
  (let ((nfts (at 'id (list-balances "k:2c8d728e9f7faa69e432d0753907a4e84719f6a570d56f086c8616ac22f87652"))))
    (map
      (lambda (nft)
      { "id": nft, "uri": (get-uri nft)}

      )
      nfts
    )
  )
)

(defun get-obj(owner:string)
  "Returns all NFTs and URIs owned by the given account as an array of objects"
  (let ((nfts (at 'nfts (read owner-tablev2 owner))))
    (map
      (lambda (nft)
        {"nft-id": nft, "uri": (at 'uri (marmalade-ng.ledger.get-token-info nft))}
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
    (insert owner-tablev2 owner {"nfts": nft-ids})
  )
)))


(if (read-msg "upgrade")
"Upgrade Complete"
[
(create-table nft-tablev2)
(create-table owner-tablev2)
]
)