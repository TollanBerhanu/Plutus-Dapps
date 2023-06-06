{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}

module TokenMintingPolicy where

import Plutus.V2.Ledger.Api
    ( ScriptContext(scriptContextTxInfo),
      PubKeyHash,
      Datum(Datum),
      TxInfo (txInfoOutputs, TxInfo, txInfoReferenceInputs, txInfoMint, txInfoInputs),
      OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
      TxOut(txOutDatum, txOutValue, txOutAddress), BuiltinData, Validator, mkValidatorScript, UnsafeFromData (unsafeFromBuiltinData), ValidatorHash, TxInInfo (txInInfoResolved), adaSymbol, adaToken, TokenName )
import Plutus.V2.Ledger.Contexts
    ( findDatum, txSignedBy, getContinuingOutputs, scriptOutputsAt, ownCurrencySymbol, valueLockedBy, valuePaidTo, valueSpent )
import PlutusTx
    ( unstableMakeIsData,
      FromData(fromBuiltinData),
      makeLift, compile, applyCode, liftCode, CompiledCode )
import PlutusTx.Prelude
    ( Bool (..),
      Integer,
      Maybe(..), traceIfFalse, ($), (&&), head, Eq ((==)), (.), not, filter, traceError, fst, snd, Ord ((>), (<)), (||), (*), divide, (+), foldl, (-)
      )
import           Prelude                    (Show, undefined, IO)
import Plutus.V1.Ledger.Value
    ( AssetClass(AssetClass), assetClassValueOf, valueOf )
import Utilities (wrapPolicy, writeCodeToFile)
import OracleValidator (OracleDatum (mintOrBurn, rate), getOracleDatum)
import Plutus.V1.Ledger.Address (scriptHashAddress)

data TokenMintParams = TokenMintParams {
    tokenName :: TokenName ,
    oracleValidator :: ValidatorHash ,
    reserveValidator :: ValidatorHash ,
    developerPKH :: PubKeyHash
} deriving Show
makeLift ''TokenMintParams

-- data TokenRedeemer = Mint | Burn
-- unstableMakeIsData ''TokenRedeemer

{-# INLINABLE mkTokenMintingpolicy #-}
mkTokenMintingpolicy :: TokenMintParams -> () -> ScriptContext -> Bool
mkTokenMintingpolicy tParams () ctx =   traceIfFalse "Minting is not allowed!" (fst mint_BurnAllowed || not minting) && -- This is false when minting is'nt allowed, but we are minting
                                        traceIfFalse "Burning is not allowed!" (snd mint_BurnAllowed || minting) &&     -- This is false when burning is'nt allowed, but we are burning
                                        traceIfFalse "Insufficient amount paid to reserve while minting!" checkEnoughPaidToReserve  &&     -- This check applies only when we're minting
                                        traceIfFalse "You can't take that much ADA for those amount of tokens!" checkReceivedAmountOnBurn  -- This check applies only when we're burning
                                        -- traceIfFalse "Not enough funds in the reserve!" checkEnoughFunds        -- *** Probably no way to check this ***
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        oracleDatum :: Maybe OracleDatum
        oracleDatum = getOracleDatum info (oracleValidator tParams)

        -- ======= Some Helper functions ========
        mintedAmount :: Integer
        mintedAmount = assetClassValueOf (txInfoMint info) $ AssetClass (ownCurrencySymbol ctx, tokenName tParams)

        minting :: Bool
        minting = mintedAmount > 0
        
        currentAdaRequiredForToken :: Integer  -- This is the number of ADA required for the amount of Tokens we Mint/Burn.
        currentAdaRequiredForToken = case oracleDatum of
                            Just d -> (mintedAmount * 1_000_000) `divide` rate d
                            Nothing -> traceError "TokenMintingPolicy: Invalid 'rate' on Oracle Datum!"
        
        -- ======== Code to extract the (mint, burn) tuple from the oracle's datum to check whether 'Minting' or 'Burning' is allowed
        mint_BurnAllowed :: (Bool, Bool)
        mint_BurnAllowed = case oracleDatum of
                            Just d -> mintOrBurn d
                            Nothing -> traceError "TokenMintingPolicy: Invalid 'mintOrBurn' on Oracle Datum!"


        -- ========= Code to check if sufficient funds are sent to the reserve when minting Tokens =========

        checkEnoughPaidToReserve :: Bool
        checkEnoughPaidToReserve = if minting 
                                    then amountPaidToReserve > currentAdaRequiredForToken                   -- We don't mind if you send more ADA ;)
                                    else True        -- This check is irrelevant while burning
            where
                amountPaidToReserve :: Integer
                amountPaidToReserve = case scriptOutputsAt (reserveValidator tParams) info of
                                        [(_ , v)] -> valueOf v adaSymbol adaToken
                                        _         -> traceError "Expected exactly one UTxO to be sent to the Reserve!"

        -- ========= Code to check if the right amount of funds are consumed from the reserve when burning Tokens =========

        checkReceivedAmountOnBurn :: Bool
        checkReceivedAmountOnBurn = if not minting
                                    then (paidToUser - changeToReserve) < currentAdaRequiredForToken    -- The ADA you get might be lower than expected due to txn fees 
                                    else True       -- This check is irrelevant while minting
            where
                paidToUser = valueOf (valueSpent info) adaSymbol adaToken
                changeToReserve = valueOf (valueLockedBy info (reserveValidator tParams)) adaSymbol adaToken
                -- paidToUserPKH = assetClassValueOf (valuePaidTo info (... userPKH ...)) $ AssetClass (adaSymbol, adaToken)

        -- totalInputAda = foldl foldOnInputs 0 (txInfoInputs info)
        --     where 
        --         foldOnInputs acc x = acc + valueOf (txOutValue $ txInInfoResolved x) adaSymbol adaToken
        
{-# INLINABLE wrappedTokenMintingCode #-}
wrappedTokenMintingCode :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedTokenMintingCode tknName developer oracle reserve = wrapPolicy $ mkTokenMintingpolicy params
    where
        params = TokenMintParams {
            tokenName = unsafeFromBuiltinData tknName,
            developerPKH = unsafeFromBuiltinData developer,
            oracleValidator = unsafeFromBuiltinData oracle,
            reserveValidator = unsafeFromBuiltinData reserve
        }

compiledTokenMintingCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledTokenMintingCode = $$( compile [|| wrappedTokenMintingCode ||] )

saveTokenMintingCode :: IO()
saveTokenMintingCode = writeCodeToFile "./assets/token_minting.plutus" compiledTokenMintingCode