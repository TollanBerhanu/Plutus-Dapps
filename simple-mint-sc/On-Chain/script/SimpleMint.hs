{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleMint where

import           Plutus.V1.Ledger.Value     (flattenValue)
import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy, TxInfo(txInfoMint),
                                       PubKeyHash, ScriptContext(scriptContextTxInfo),
                                       mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)                                       
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), ($), (||), traceIfFalse, Integer, not, Ord ((<)))
import           Prelude              (IO)
import           Utilities            (wrapPolicy, writePolicyToFile)

{-# INLINABLE mkPolicy #-}
-- Any user should be able to burn the tokens.
-- Only the Owner/signatory authorities can Mint the tokens.
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy _pkh () _ctx = traceIfFalse "Only the owner can mint tokens" (ownerSigned || burn)
    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx
        
        ownerSigned :: Bool
        ownerSigned = txSignedBy info _pkh

        mintedAmount :: Integer
        mintedAmount = case flattenValue (txInfoMint info) of   -- flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
            [(_, _ , amt)] -> amt
            _              -> 0

        burn :: Bool
        burn = mintedAmount < 0

-- ====== This is the way to apply the arguments off-chain

{-# INLINABLE mkWrappedPolicy #-}
mkWrappedPolicy :: PubKeyHash -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy pkh = wrapPolicy $ mkPolicy pkh

simpleMintPolicy :: PubKeyHash-> MintingPolicy
simpleMintPolicy pkh = mkMintingPolicyScript $ 
        $$(PlutusTx.compile [|| mkWrappedPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode pkh

savePolicy :: PubKeyHash -> IO ()
savePolicy pkh = writePolicyToFile "serialized/simple_mint.plutus" $ simpleMintPolicy pkh

{- 
-- ====== This is the way to apply the arguments on-chain

    {-# INLINABLE mkWrappedPolicy #-}
    mkWrappedPolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
    mkWrappedPolicy pkh deadline = wrapPolicy $ mkPolicy pkh deadline

    deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
    deadlinePolicy pkh deadline = mkMintingPolicyScript $
        $$(PlutusTx.compile [|| mkWrappedPolicy ||])
            `PlutusTx.applyCode` PlutusTx.liftCode pkh
            `PlutusTx.applyCode` PlutusTx.liftCode deadline

    savePolicy :: PubKeyHash -> POSIXTime -> IO ()
    savePolicy pkh deadline = writePolicyToFile (printf "assets/deadline-%s.plutus" $ show pkh) $ deadlinePolicy pkh deadline
-}