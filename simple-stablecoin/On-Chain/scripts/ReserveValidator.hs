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
      TxInfo (txInfoOutputs),
      OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
      TxOut(txOutDatum, txOutValue), BuiltinData, Validator, mkValidatorScript, UnsafeFromData (unsafeFromBuiltinData) )
import Plutus.V2.Ledger.Contexts
    ( findDatum, txSignedBy, getContinuingOutputs )    
import PlutusTx
    ( unstableMakeIsData,
      FromData(fromBuiltinData),
      makeLift, compile, applyCode, liftCode, CompiledCode )
import PlutusTx.Prelude
    ( Bool,
      Integer,
      Maybe(..), traceIfFalse, ($), (&&), head, Eq ((==)), (.)
      )
import           Prelude                    (Show (show), undefined, IO)
import Plutus.V1.Ledger.Value
    ( AssetClass(AssetClass), assetClassValueOf )
import Data.Aeson (Value(Bool))
import Utilities (wrapValidator, writeCodeToFile)

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------

{-# INLINABLE parseOracleDatum #-}
parseOracleDatum :: TxOut -> TxInfo -> Maybe Integer    --  We check and parse the Datum of the output UTxO
parseOracleDatum o info = case txOutDatum o of
    NoOutputDatum -> Nothing
    OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d     -- Inline datum: just parse the datum (check if it is an Integer)
    OutputDatumHash dh -> do
                        Datum d <- findDatum dh info        -- Datum's hash: find the actual datum from the ScriptContext
                        PlutusTx.fromBuiltinData d          --               parse the datum

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data ReserveParams = ReserveParams  
    {
        mintingPolicy :: ValidatorHash,
        oracleValidator :: ValidatorHash
    } 
PlutusTx.makeLift ''ReserveParams

{-# INLINABLE mkValidator #-}
mkValidator :: ReserveParams -> () -> () -> ScriptContext -> Bool
mkValidator col _ _ ctx = traceIfFalse "" not checkBurntAmount
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        checkBurntAmount :: Bool
        checkBurntAmount = undefined
        