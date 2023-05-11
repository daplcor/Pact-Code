(namespace "free")
;This code is meant to work with the Marmalade V2 Minter

(define-keyset "free.ku-admin" (read-keyset "ku-admin"))
(define-keyset "free.ku-ops" (read-keyset "ku-ops"))

(module kadenai-minter GOVERNANCE
    
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
  
  (deftable nft-table:{nft})
  
  (defun mint:bool
    (
      account:string
      uri:string
      precision:integer
      policies:object{kip.token-policy-v2.token-policies}
    )
    @doc "Mints one token and sets owner from account field"
       
    (enforce (= 0 precision) "Precision must be 0")
    ;  (enforce (marmalade.ledger.is-authorized account) "Unauthorized")
  (with-capability (MINT)
    (let*
      (
        (hash-id (hash-contents uri precision policies))
        (token-id (concat ["t:" hash-id]))
        (guard (at "guard"(coin.details account)))
      )
      ; Create NFT on marmalade
      (marmalade.ledger.create-token
        token-id
        precision
        uri
        policies
      )
  
      ; Mint NFT to the account
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
    )
  )
  )
    


(defun hash-contents:string
  ( uri:string
    precision:integer
    policies:object{kip.token-policy-v2.token-policies}
  )
  (hash {'uri: uri, 'precision:precision, 'policies:policies})
)

(defun get-nfts()
(keys nft-table)
  )

)

(if (read-msg "upgrade")
"Upgrade Complete"
[
(create-table nft-table)
]
)