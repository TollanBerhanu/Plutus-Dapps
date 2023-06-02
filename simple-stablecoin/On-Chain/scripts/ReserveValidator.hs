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

        -- ========= Logic to check if there are sufficient funds (ADA) given as input for the amount of tokens burnt ===========
        totalAdaProduced :: Integer
        totalAdaProduced = assetClassValueOf (valueProduced info) (AssetClass (adaSymbol, adaToken))

        totalTokensBurnt :: Integer
        totalTokensBurnt = negate $ assetClassValueOf (txInfoMint info) (tokenMintingPolicy rParams)

        getOracleDatum :: Maybe OracleDatum
        getOracleDatum = case scriptOutputsAt (oracleValidator rParams) info of
                                [(OutputDatum (Datum d) , _)] -> fromBuiltinData d
                                _                             -> traceError "Expected one Oracle UTxO!"

        lookupAddress :: Address -> TxInInfo -> Bool
        lookupAddress addr tinfo  = addr == txOutAddress (txInInfoResolved tinfo)

        getOracleDatum' :: Maybe OracleDatum
        getOracleDatum' = filter (lookupAddress (scriptHashAddress (oracleValidator rParams)))  txInfoReferenceInputs info


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