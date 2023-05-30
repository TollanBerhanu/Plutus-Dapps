{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Oracle where

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

data OracleParams = OracleParams    -- These parameters never change for the Oracle
    { oNFT        :: AssetClass             -- The oracle can only use one NFT
    , oOperator   :: PubKeyHash             -- The oracle can be operated by only one person
    } 
PlutusTx.makeLift ''OracleParams

data OracleRedeemer = Update | Delete     -- Once the OracleValidator is minted, it can only be updated/deleted
    deriving Prelude.Show
PlutusTx.unstableMakeIsData ''OracleRedeemer

-- Oracle Datum (price of ADA in USD ... in cents)
type Rate = Integer

{-# INLINABLE mkValidator #-}
mkValidator :: OracleParams -> Rate -> OracleRedeemer -> ScriptContext -> Bool
mkValidator oracle _ redeemer ctx =  case redeemer of
                                        Update ->  traceIfFalse "Operator did not sign" checkOperatorSigned &&
                                                   traceIfFalse "NFT must be included in output" checkNFTIncluded
                                        Delete ->  traceIfFalse "Operator did not sign" checkOperatorSigned
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        checkOperatorSigned :: Bool
        checkOperatorSigned = txSignedBy info $ oOperator oracle

        checkNFTIncluded :: Bool
        checkNFTIncluded = assetClassValueOf (txOutValue $ head $ getContinuingOutputs ctx) (oNFT oracle) == 1


---------------------------------------------------------------------------------------------------
------------------------------------ COMPILE VALIDATOR --------------------------------------------

{-# INLINABLE  mkWrappedValidatorLucid #-}
--                            CS              TN           operator        rate          redeemer       context
mkWrappedValidatorLucid :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidatorLucid cs tn pkh = wrapValidator $ mkValidator op
    where
        op = OracleParams   -- Parse the parameters to apply them to the mkValidator function
            { oNFT = AssetClass (unsafeFromBuiltinData cs, unsafeFromBuiltinData tn)    -- Build the AssetClass from the 'CurrencySymbol' and the 'TokenName'
            , oOperator   = unsafeFromBuiltinData pkh       -- Convert pkh from BuiltinData to PubKeyHash
            }

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$( compile [|| mkWrappedValidatorLucid ||])

---------------------------------------------------------------------------------------------------
------------------------------------- SAVE VALIDATOR -------------------------------------------

saveOracleCode :: IO ()
saveOracleCode = writeCodeToFile "assets/oracle.plutus" validatorCode