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
import Plutus.V1.Ledger.Value               ( AssetClass(AssetClass), assetClassValueOf )
import Utilities                            (wrapValidator, writeCodeToFile)
import Plutus.V1.Ledger.Address             (scriptHashAddress)

-- Define the Oracle's parameter type
data OracleParams = OracleParams { 
    oNFT :: AssetClass,
    oDeveloper :: PubKeyHash
 }
makeLift ''OracleParams

-- Define the Oracle's datum type
data OracleDatum = OracleDatum { 
    rate :: Integer ,
    canMint :: Bool ,
    mintedAmount :: Bool
} deriving Show
unstableMakeIsData ''OracleDatum

data OracleRedeemer = Update | Toggle | ChangeAmount
unstableMakeIsData ''OracleRedeemer

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: OracleParams -> OracleDatum -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oParams _ oRedeemer ctx = case oRedeemer of
                                                Update      ->  traceIfFalse "Update: Developer hasn't signed!" developerSigned &&
                                                                traceIfFalse "Update: NFT missing on input!" nftOnInput &&
                                                                traceIfFalse "Update: NFT missing on output!" nftOnOutput &&
                                                                traceIfFalse "Update: Invalid oracle output datum!" checkOracleOpDatum &&
                                                                traceIfFalse "Update: You should only change the 'rate' value!" checkValidDeveloperUpdate &&
                                                                traceIfFalse "Update: Incorrect amount in reserve based on rate change!" checkEnoughFundsInReserve

                                                Toggle      ->  traceIfFalse "Toggle: Developer hasn't signed!" developerSigned &&
                                                                traceIfFalse "Toggle: NFT missing on input!" nftOnInput &&
                                                                traceIfFalse "Toggle: NFT missing on output!" nftOnOutput &&
                                                                traceIfFalse "Toggle: Invalid oracle output datum!" checkOracleOpDatum &&
                                                                traceIfFalse "Toggle: You should only change the 'canMint' value!" checkValidDeveloperToggle
                                                
                                                ChangeAmount -> traceIfFalse "ChangeAmount: NFT missing on input!" nftOnInput &&
                                                                traceIfFalse "ChangeAmount: NFT missing on output!" nftOnOutput &&
                                                                traceIfFalse "ChangeAmount: Invalid oracle output datum!" checkOracleOpDatum &&
                                                                traceIfFalse "ChangeAmount: You should only change the 'mintedAmount' value!" checkValidMintUpdate &&
                                                                traceIfFalse "ChangeAmount: Invalid 'mintedAmount' value based on amount minted/burnt!" correctAmountChanged
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- =========== Common checks ============
        developerSigned :: Bool
        developerSigned = txSignedBy info $ oDeveloper oParams

        nftOnInput :: Bool
        nftOnInput =  1 == assetClassValueOf (valueSpent info) (oNFT oParams)

        nftOnOutput :: Bool
        nftOnOutput = 1 == assetClassValueOf (valueProduced info) (oNFT oParams)

        checkOracleOpDatum :: Bool  -- Check the type of the Oracle output datum
        checkOracleOpDatum = isJust parsedDatum
            where
                opDatum :: OutputDatum
                opDatum = case getContinuingOutputs ctx of
                        [tOut] -> txOutDatum tOut
                        _      -> traceError "Expected exactly 1 output UTxO to at the OracleValidator!"
                parsedDatum :: Maybe OracleDatum
                parsedDatum = case opDatum of
                                OutputDatum (Datum d) -> fromBuiltinData d
                                _                     -> traceError "Expected inline datum!"
        
        -- ================ Update checks ============
        -- Check that the developer only changes the rate and nothing else
        checkValidDeveloperUpdate :: Bool
        checkValidDeveloperUpdate = undefined
        
        -- Check that the developer either funds the reserve or receives rewards based on changes in rate
        checkEnoughFundsInReserve :: Bool
        checkEnoughFundsInReserve = undefined

        -- ================ Toggle checks ============
        -- Check that the developer only changes the canMint value and nothing else (The new canMint value must be different from the old canMint value)
        checkValidDeveloperToggle :: Bool
        checkValidDeveloperToggle = undefined
        
        -- ================ ChangeAmount checks ============
        -- Check that the Stablecoin owner only changes the mintedAmount value and nothing else
        checkValidMintUpdate :: Bool
        checkValidMintUpdate = undefined

        -- Check that the Stablecoin owner updates the mintedAmount to the correct value
        correctAmountChanged :: Bool
        correctAmountChanged = undefined

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

