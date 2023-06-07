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
      TxInfo (txInfoOutputs, TxInfo, txInfoReferenceInputs),
      OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
      TxOut(txOutDatum, txOutValue, txOutAddress), BuiltinData, Validator, mkValidatorScript, UnsafeFromData (unsafeFromBuiltinData), TxInInfo (txInInfoResolved), CurrencySymbol, ValidatorHash )
import Plutus.V2.Ledger.Contexts
    ( findDatum, txSignedBy, getContinuingOutputs, findOwnInput, valueSpent, valueProduced, valuePaidTo )    
import PlutusTx
    ( unstableMakeIsData,
      FromData(fromBuiltinData),
      makeLift, compile, applyCode, liftCode, CompiledCode)
import PlutusTx.Prelude
    ( Bool (False, True),
      Integer,
      Maybe(..), traceIfFalse, ($), (&&), head, Eq ((==)), (.), traceError, filter, not, isJust
      )
import           Prelude                    (Show (show), undefined, IO)
import Plutus.V1.Ledger.Value
    ( AssetClass(AssetClass), assetClassValueOf )
import Data.Aeson (Value(Bool))
import Utilities (wrapValidator, writeCodeToFile)
import Plutus.V1.Ledger.Address (scriptHashAddress)

-- Define the Oracle's parameter type
data OracleParams = OracleParams { 
    oNFT :: AssetClass,
    oDeveloper :: PubKeyHash
 }
makeLift ''OracleParams

-- Define the Oracle's datum type
data OracleDatum = OracleDatum { 
    rate :: Integer,
    mintOrBurn :: (Bool, Bool)
} deriving Show
unstableMakeIsData ''OracleDatum

data OracleRedeemer = Update | Delete
unstableMakeIsData ''OracleRedeemer

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: OracleParams -> OracleDatum -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oParams _ oRedeemer ctx = case oRedeemer of
                                                Update -> traceIfFalse "Developer hasn't signed!" developerSigned &&
                                                          traceIfFalse "NFT missing on input!" nftOnInput &&
                                                          traceIfFalse "NFT missing on output!" nftOnOutput &&
                                                          traceIfFalse "Invalid oracle output datum!" checkOracleOpDatum

                                                Delete -> traceIfFalse "Developer hasn't signed!" developerSigned &&
                                                          traceIfFalse "NFT missing on input!" nftOnInput &&
                                                          traceIfFalse "NFT must be sent back to the developer!" nftSentToDeveloper
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx
 
        developerSigned :: Bool
        developerSigned = txSignedBy info $ oDeveloper oParams

        nftOnInput :: Bool
        nftOnInput =  1 == assetClassValueOf (valueSpent info) (oNFT oParams)

        nftOnOutput :: Bool
        nftOnOutput = 1 == assetClassValueOf (valueProduced info) (oNFT oParams)

        nftSentToDeveloper :: Bool
        nftSentToDeveloper = 1 == assetClassValueOf (valuePaidTo info (oDeveloper oParams)) (oNFT oParams)

        checkOracleOpDatum :: Bool
        checkOracleOpDatum = isJust parsedDatum
            where
                opDatum :: OutputDatum
                opDatum = case getContinuingOutputs ctx of
                        [tOut] -> txOutDatum tOut
                        _      -> traceError "Expected exactly 1 op UTxO to at the OracleValidator!"
                parsedDatum :: Maybe OracleDatum
                parsedDatum = case opDatum of
                                OutputDatum (Datum d) -> fromBuiltinData d
                                _                     -> traceError "Expected inline datum!"


                -- CurrencySymbol    TokenName     Developer PKH   OracleDatum       ()        ScriptContext
{-# INLINABLE wrappedOracleCode #-}
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

-- =============================================================== Helper functions for other scripts =====================================================
    
-- ======== Code to extract the oracle datum from the reference input UTxOs ==========
{-# INLINABLE getOracleDatum #-}
getOracleDatum :: TxInfo -> ValidatorHash -> Maybe OracleDatum
getOracleDatum _info _oracleValHash = case oracleDatum of
                    OutputDatum (Datum d) -> fromBuiltinData d
                    _                     -> traceError "Invalid Oracle Datum!"
    where
        lookupOracleAddress :: TxInInfo -> Bool
        lookupOracleAddress tinfo  = addr == txOutAddress (txInInfoResolved tinfo)   -- Check for the oracle UTxO by its address from reference TxInInfo
            where
                addr = scriptHashAddress _oracleValHash

        oracleDatum :: OutputDatum
        oracleDatum = txOutDatum $ txInInfoResolved oracleTxInInfo
            where
                oracleTxInInfos = filter lookupOracleAddress $ txInfoReferenceInputs _info     -- Filter the oracle UTxO by its address (there might be another reference input UTxO
                oracleTxInInfo = case oracleTxInInfos of                                            -- because we also execute the minting policy in the same txn
                                    [o] -> o
                                    _   -> traceError "Expected exactly one Oracle UTxO!"

