{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module OracleValidator where

import Plutus.V2.Ledger.Api
    ( ScriptContext(scriptContextTxInfo),
      PubKeyHash,
      Datum(Datum),
      TxInfo (txInfoOutputs, TxInfo),
      OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
      TxOut(txOutDatum, txOutValue), BuiltinData, Validator, mkValidatorScript, UnsafeFromData (unsafeFromBuiltinData), TxInInfo, CurrencySymbol )
import Plutus.V2.Ledger.Contexts
    ( findDatum, txSignedBy, getContinuingOutputs, findOwnInput, valueSpent, valueProduced )    
import PlutusTx
    ( unstableMakeIsData,
      FromData(fromBuiltinData),
      makeLift, compile, applyCode, liftCode, CompiledCode)
import PlutusTx.Prelude
    ( Bool (False, True),
      Integer,
      Maybe(..), traceIfFalse, ($), (&&), head, Eq ((==)), (.), traceError
      )
import           Prelude                    (Show (show), undefined, IO)
import Plutus.V1.Ledger.Value
    ( AssetClass(AssetClass), assetClassValueOf )
import Data.Aeson (Value(Bool))
import Utilities (wrapValidator, writeCodeToFile)

data OracleParams = OracleParams { 
    oNFT :: AssetClass,
    oDeveloper :: PubKeyHash
 }
makeLift ''OracleParams

data OracleDatum = OracleDatum { 
    rate :: Integer,
    mintOrBurn :: (Bool, Bool)
} deriving Show
unstableMakeIsData ''OracleDatum

mkOracleValidator :: OracleParams -> OracleDatum -> () -> ScriptContext -> Bool
mkOracleValidator oParams _ _ ctx = traceIfFalse "Developer hasn't signed!" developerSigned &&
                                    traceIfFalse "NFT missing on input!" nftOnInput &&
                                    traceIfFalse "NFT missing on output!" nftOnOutput
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        developerSigned :: Bool
        developerSigned = txSignedBy info $ oDeveloper oParams

        -- input :: TxInInfo
        -- input = case findOwnInput ctx of
        --                 Nothing -> traceError ""
        --                 Just ip -> ip

        nftOnInput :: Bool
        nftOnInput =  1 == assetClassValueOf (valueSpent info) (oNFT oParams)

        nftOnOutput :: Bool
        nftOnOutput = 1 == assetClassValueOf (valueProduced info) (oNFT oParams)

                -- CurrencySymbol    TokenName     Developer PKH   OracleDatum       ()        ScriptContext
wrappedOracleCode :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedOracleCode curSym tn developer = wrapValidator $ mkOracleValidator oParams
    where 
        oParams = OracleParams {
            oNFT = AssetClass (unsafeFromBuiltinData curSym, unsafeFromBuiltinData tn) ,
            oDeveloper = unsafeFromBuiltinData developer
        }

compiledOracleCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledOracleCode = $$( compile [|| wrappedOracleCode ||] )

saveOracleCode :: IO()
saveOracleCode = writeCodeToFile "./assets/oracle.plutus" compiledOracleCode