Welcome to our PACT repository.
I will be updating this repository and adding JS examples for how to use some of these contracts.

/NFT marketplace/ku-create.pact is for creating and managing collections, whitelists, and mint information.  
*TODO put together an overview of the commands required to launch your collection quickly.

/payment/kadenai-pay.pact is the starting point for simplified merchant payments, I might eventually build the JS
into an easy to use library.  This contract still needs some guard and enforcements to protect against malicious actors.
  * First you need to load and initialize the tables
  * Then set the (update-string-value "BANK" "yourbankaccount or k: address)
      * You can verify by issuing (get-bank)
  * Then set the (update-image-cost "IMAGECOST" 0.1)
      * You can verify by issuing (get-image-fee)
  *  After you've set your values, you can run (create-collection-with-payment {} coin "k:account")
      * Then validate payment by issuing (get-payment "your collection name")
 Again this contract was made to be more of an example for taking in payments.  Replace collection with whatever you want.
 
 
If you have questions or suggestions, come visit us at kadenai.com for up to date contact information.
 
     
     
