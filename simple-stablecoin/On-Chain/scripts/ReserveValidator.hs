{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module ReserveValidator where

import Plutus.V2.Ledger.Api
    ( ScriptContext(scriptContextTxInfo),
      PubKeyHash,
      Datum(Datum),
      TxInfo (txInfoOutputs, TxInfo, txInfoMint, txInfoReferenceInputs),
      OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
      TxOut(txOutDatum, txOutValue, txOutAddress), BuiltinData, Validator, mkValidatorScript, UnsafeFromData (unsafeFromBuiltinData), ValidatorHash, adaToken, TxInInfo (txInInfoResolved, TxInInfo), Address )
import Plutus.V2.Ledger.Contexts
    ( findDatum, txSignedBy, getContinuingOutputs, valueProduced, scriptOutputsAt )
import PlutusTx
    ( unstableMakeIsData,
      FromData(fromBuiltinData),
      makeLift, compile, applyCode, liftCode, CompiledCode )
import PlutusTx.Prelude
    ( Bool (..),
      Integer,
      Maybe(..), traceIfFalse, ($), (&&), head, Eq ((==)), (.), not, negate, traceError, (*), filter
      )
import           Prelude                    (Show (show), undefined, IO, Ord ((>)), lookup)
import Plutus.V1.Ledger.Value
    ( AssetClass(AssetClass), assetClassValueOf, adaSymbol )
import Data.Aeson (Value(Bool))
import Utilities (wrapValidator, writeCodeToFile)
import OracleValidator (OracleDatum (rate))
import Plutus.V1.Ledger.Address (scriptHashAddress)


data ReserveParams = ReserveParams {
    tokenMintingPolicy :: AssetClass,
    oracleValidator :: ValidatorHash
}
makeLift ''ReserveParams

mkReserveValidator :: ReserveParams -> () -> () -> ScriptContext -> Bool
mkReserveValidator rParams _ _ ctx = traceIfFalse "Insufficient tokens burnt!" checkSufficientTokens
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- ======== Code to extract the oracle datum from the reference inputs ==========
        lookupOracleAddress :: TxInInfo -> Bool
        lookupOracleAddress tinfo  = addr == txOutAddress (txInInfoResolved tinfo)   -- Check for the oracle UTxO by its address from reference TxInInfo
            where
                addr = scriptHashAddress (oracleValidator rParams)

        oracleDatum :: OutputDatum
        oracleDatum = txOutDatum $ txInInfoResolved oracleTxInInfo
            where
                oracleTxInInfos = filter lookupOracleAddress $ txInfoReferenceInputs info     -- Filter the oracle UTxO by its address (there might be another reference input UTxO
                oracleTxInInfo = case oracleTxInInfos of                                            -- because we also execute the minting policy in the same txn
                                    [o] -> o
                                    _   -> traceError "Expected exactly one Oracle UTxO!"

        getOracleDatum :: Maybe OracleDatum
        getOracleDatum = case oracleDatum of
                            OutputDatum (Datum d) -> fromBuiltinData d
                            _             -> traceError "Invalid Oracle Datum!"

        -- ========= Code to check if there are sufficient tokens burnt for the amount of ADA unlocked ===========
        totalAdaProduced :: Integer
        totalAdaProduced = assetClassValueOf (valueProduced info) (AssetClass (adaSymbol, adaToken))

        totalTokensBurnt :: Integer
        totalTokensBurnt = negate $ assetClassValueOf (txInfoMint info) (tokenMintingPolicy rParams)

        checkSufficientTokens :: Bool
        checkSufficientTokens = case getOracleDatum of
                                    Just dtm -> totalAdaProduced > rate dtm * totalTokensBurnt
                                    Nothing  -> False


wrappedReserveCode :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedReserveCode tkn_mint_pol oracle_val = wrapValidator $ mkReserveValidator params
    where
        params = ReserveParams {
            tokenMintingPolicy = unsafeFromBuiltinData tkn_mint_pol,
            oracleValidator = unsafeFromBuiltinData oracle_val
        }

compiledReserveCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledReserveCode = $$( compile [|| wrappedReserveCode ||] )

saveReserveCode :: IO()
saveReserveCode = writeCodeToFile "./assets/reserve.plutus" compiledReserveCode