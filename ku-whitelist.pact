(namespace "free")

(define-keyset "free.ku-wl-admin" (read-keyset "ku-wl-admin"))
(define-keyset "free.ku-wl-ops" (read-keyset "ku-wl-ops"))

(module ku-whitelist GOVERNANCE
    @doc "Forked from Luzzotica whitelist"
(defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.ku-wl-admin"))
    (compose-capability (OPS_INTERNAL))
)

(defcap OPS ()
    (enforce-guard (keyset-ref-guard "free.ku-wl-ops"))
    (compose-capability (OPS_INTERNAL))
)

(defcap OPS_INTERNAL ()
    (compose-capability (WL_MANAGE))
  )

;; -------------------------------
  ;; Whitelist

  (defcap OWNER (account:string)
    (enforce-guard (at "guard" (coin.details account)))
    (compose-capability (WL_MANAGE))
  )
  (defcap WL_MANAGE () true)
  
  (defschema whitelist
    @doc "Stores all the whitelisters. ID is collection-account"
    collection:string
    account:string
    is-whitelisted:bool
    available-free:decimal
  )
  (deftable whitelisted:{whitelist})

  (defun update-whitelisted:[string] 
    (
      whitelisted-info:[object{whitelist}]
    )
    @doc "Updates the whitelist information with the provided. \
    \ If an account already exists, the available is summed, \
    \ and the discount is overwritten"
    (with-capability (OPS)
      (map (update-whitelist) whitelisted-info)
    )
  )

  (defun update-whitelist:string 
    (info:object{whitelist})
    @doc "Updates an individual whitelist information with the provided. \
    \ If an account already exists, the available is summed, \
    \ and the discount is overwritten"
    (require-capability (WL_MANAGE))

    (let
      (
        (whitelist-id 
          (get-whitelist-id (at "collection" info) (at "account" info)))
      )  

      (with-default-read whitelisted whitelist-id
        { "collection": (at "collection" info)
        , "account": (at "account" info)
        , "is-whitelisted": false
        , "available-free": 0.0
        }
        { "collection":= collection
        , "account":= account
        , "is-whitelisted":= is-whitelisted
        , "available-free":= curr-available-free
        }

        (let
          (
            (avail-free:decimal (+ curr-available-free (at "available-free" info)))
             )
          (enforce (!= "" collection) "Collection name cannot be empty")
          (enforce (>= avail-free 0.0) "Error: Available free must be >= 0")
          (write whitelisted whitelist-id
            { "collection": collection
            , "account": account
            , "is-whitelisted": (at "is-whitelisted" info)
            , "available-free": avail-free
            }
          )
        )
      )
    )
  )

  (defun decrement-available-free-with-ops:string 
    (collection:string account:string amount:decimal)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (with-capability (OPS)
      (decrement-available-free collection account amount)
    )
  )

  (defun decrement-available-free-with-owner:string 
    (collection:string account:string amount:decimal)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (with-capability (OWNER account)
      (decrement-available-free collection account amount)
    )
  )

  (defun decrement-available-free:string 
    (collection:string account:string amount:decimal)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (require-capability (WL_MANAGE))

    (let
      (
        (whitelist-id (get-whitelist-id collection account))
      )  
      (with-read whitelisted whitelist-id
        { "available-free":= curr-available-free
        }
        (let
          (
            (avail (- curr-available-free amount))
          )
          (enforce (>= avail 0.0) "Error: Available free must be >= 0")

          (update whitelisted whitelist-id
            { "available-free": avail
            }
          )
        )
      )
    )
  )

  (defun enforce-whitelisted:bool (collection:string account:string)
    @doc "Ensures that the account is whitelisted"
    (with-default-read whitelisted (get-whitelist-id collection account)
      { "collection": collection
      , "account": account
      , "is-whitelisted": false
      , "available-free": 0.0
      }
      { "is-whitelisted":= wl }
      (enforce wl "Must be whitelisted")
    )
  )

  (defun get-all-whitelisted:[object{whitelist}] ()
    @doc "Gets all the whitelisted"
    (select whitelisted (constantly true))
  )

  (defun get-whitelist:object{whitelist} 
    (collection:string account:string)
    @doc "Get the whitelist info for the account"
    (read whitelisted (get-whitelist-id collection account))
  )

  (defun get-available-free:decimal
    (collection:string account:string)
    @doc "Get the available free mints for the account"
    (at "available-free" 
      (read whitelisted 
        (get-whitelist-id collection account) ["available-free"]))
  )

(defun get-whitelist-id:string (collection:string account:string)
    @doc "Creates the ID for the whitelist based on the collection and account"
    (concat [collection "|" account])
  )
)


