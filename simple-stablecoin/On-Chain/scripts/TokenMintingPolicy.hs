{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}

module TokenMintingPolicy where

import Plutus.V2.Ledger.Api
    ( ScriptContext(scriptContextTxInfo),
      PubKeyHash,
      Datum(Datum),
      TxInfo (txInfoOutputs, TxInfo),
      OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
      TxOut(txOutDatum, txOutValue), BuiltinData, Validator, mkValidatorScript, UnsafeFromData (unsafeFromBuiltinData), ValidatorHash )
import Plutus.V2.Ledger.Contexts
    ( findDatum, txSignedBy, getContinuingOutputs )    
import PlutusTx
    ( unstableMakeIsData,
      FromData(fromBuiltinData),
      makeLift, compile, applyCode, liftCode, CompiledCode )
import PlutusTx.Prelude
    ( Bool,
      Integer,
      Maybe(..), traceIfFalse, ($), (&&), head, Eq ((==)), (.), not
      )
import           Prelude                    (Show (show), undefined, IO)
import Plutus.V1.Ledger.Value
    ( AssetClass(AssetClass), assetClassValueOf )
import Data.Aeson (Value(Bool))
import Utilities (wrapPolicy, writeCodeToFile)

data TokenMintParams = TokenMintParams {
    tDeveloper :: PubKeyHash ,
    tOracle :: ValidatorHash
}
makeLift ''TokenMintParams

data TokenRedeemer = Mint | Burn
unstableMakeIsData ''TokenRedeemer

mkTokenMintingpolicy :: TokenMintParams -> TokenRedeemer -> ScriptContext -> Bool
mkTokenMintingpolicy tParams tRedeemer ctx = traceIfFalse "Wrong amount paid to reserve!" checkPaidAmount &&
                                             traceIfFalse "Minting is not allowed!" checkMintAllowed && 
                                             traceIfFalse "Burning is not allowed!" checkBurnAllowed &&
                                             traceIfFalse "Not enough funds in the reserve!" checkEnoughFunds
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx
        
        rightAmount :: Integer
        rightAmount = undefined

        checkPaidAmount :: Bool
        checkPaidAmount = undefined
            
        checkMintAllowed :: Bool
        checkMintAllowed = undefined

        checkBurnAllowed :: Bool
        checkBurnAllowed = undefined

        checkEnoughFunds :: Bool
        checkEnoughFunds = undefined

wrappedTokenMintingCode :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedTokenMintingCode developer oracle = wrapPolicy $ mkTokenMintingpolicy params
    where
        params = TokenMintParams {
            tDeveloper = unsafeFromBuiltinData developer,
            tOracle = unsafeFromBuiltinData oracle
        }

compiledTokenMintingCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledTokenMintingCode = $$( compile [|| wrappedTokenMintingCode ||] )

saveTokenMintingCode :: IO()
saveTokenMintingCode = writeCodeToFile "./assets/token_minting.plutus" compiledTokenMintingCode