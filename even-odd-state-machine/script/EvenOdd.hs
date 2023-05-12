{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

module EvenOdd where

import           Plutus.V1.Ledger.Interval (contains, to)
import           Plutus.V1.Ledger.Value (AssetClass, flattenValue, assetClassValueOf)
import           Plutus.V2.Ledger.Api      (BuiltinData, POSIXTime, PubKeyHash, Datum (Datum), TxOut (txOutValue),
                                            ScriptContext (scriptContextTxInfo), Value,
                                            TxInfo (txInfoValidRange), BuiltinByteString,
                                            Validator, from, mkValidatorScript, TxInInfo (txInInfoResolved), DatumHash (DatumHash))
import           Plutus.V2.Ledger.Contexts (txSignedBy, findOwnInput, getContinuingOutputs, findDatum)
import           PlutusTx                  (applyCode, compile, liftCode, FromData (fromBuiltinData),
                                            makeLift, unstableMakeIsData)
import           PlutusTx.Prelude          (Bool (True, False), traceIfFalse, ($), (&&), Integer, Eq ((==)), Ord,
                                            Maybe (Just, Nothing), (.), traceError, (>>=), flip)
import           Prelude                   (IO, String, Show(show), (*), (+))
import           Utilities                 (Network, posixTimeFromIso8601,
                                            printDataToJSON,
                                            validatorAddressBech32,
                                            wrapValidator, writeValidatorToFile)
import          GHC.Generics                (Generic)
import          Data.Aeson                  (FromJSON, ToJSON)

-- ====================== ON CHAIN Code ===============================

-- This is given as a parameter to the contract
data Game = Game
    { gFirst          :: PubKeyHash     -- Pubkey of the Player 1
    , gSecond         :: PubKeyHash     -- Pubkey of the Player 2
    , gStake          :: Integer               -- amount of lovelace used as stake by each player
    , gPlayDeadline   :: POSIXTime             -- time the second player can make a move before the first player can claim back its stake
    , gRevealDeadline :: POSIXTime             -- time the first player can claim victory by revealing its nonce, given the second player has made a move
    , gToken          :: AssetClass            -- This is the NFT thats passed around in each turn (to validate whose turn it is and this allows only one valid txn
                                                    -- to be sent by each player on every turn).. because anyone can send UTxOs to the same address with the same datum
                                                    -- but only one can contain the NFT as pat of its Value
    } deriving (Show, Generic, Eq, Ord)

unstableMakeIsData ''Game

data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

unstableMakeIsData ''GameChoice
data GameDatum = GameDatum BuiltinByteString (Maybe GameChoice)
    deriving Show
instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | Reveal BuiltinByteString | ClaimFirst | ClaimSecond
    deriving Show

unstableMakeIsData ''GameRedeemer

-- These are some helper functions
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer   -- extract the amount of lovelaces contained in a Value type
lovelaces val = case flattenValue val of
                    [(_,_,amt)] -> amt
                    _ -> 0

{-# INLINABLE gameDatum #-}
gameDatum :: Maybe DatumHash -> Maybe GameDatum -- Deseralize the Datum into our GameDatum
gameDatum md = do
    DatumHash d <- md
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
        -- Check if the input contains the NFT
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
    -- (GameDatum Hash_1st_Player_Move 2nd_player_Move,  GameRedeemer Current_player_Move)
    case (dat, red) of
            -- First player has moved, but second player is moving ... this is the txn in w/c the second player moves
        (GameDatum bs Nothing, Play c) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))             &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   && -- check staked UTxO in player1's i/p
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            && -- check staked UTxO in player2's o/p
                                        -- we made this 2 * stake because the second player consumes the first player's UTxO and stakes the first players + his own
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&  -- Datum must contain tha same val as 
                                                                                                                            -- before with the 2nd player's move added
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&  -- 2nd player must submit before the play deadline
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)          -- check NFT in player 2's output

        -- Both players have moved and the first player discovers that it has won ... (It has to reveal the nonce to prove that and claim winnings)
        (GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))              &&
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&  -- The nonce must dive a valid hash
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&  -- 1st player must reveal before the reveal deadline
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&  -- The input must contain the stake of both 
                                                                                                                        -- players, and the winner takes all
            traceIfFalse "NFT must go to first player"   nftToFirst                                                             -- The NFT must go back to the 1st player

        -- Second player hasn't moved and the deadline passes (1st player gets its stake back)
        (GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))              &&
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&  -- 1st player can take back its stake only after the deadline has passed
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&  -- 1st player gets its stake
            traceIfFalse "NFT must go to first player"   nftToFirst                                                                 -- and its NFT back

        -- Both players move but first player doesn't reveal its nonce (2nd player can claim its winnings)
        (GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))             &&
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&  -- Both players must have provided the stake
            traceIfFalse "NFT must go to first player"   nftToFirst                                                             -- NFT still goes back to the originator
                                                                                                            --  of the game (1st player) incase they want to play again

        _ -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of -- findOwnInput returns a (Maybe TxInInfo) ... it can be Nothing if this script is used for minting
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i   -- txInInfoResolved :: TxOut ... Actual output: (Address, Value, Maybe DatumHash)

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of -- getContinuingOutputs returns [TxOut]... the output(s) that goes to the script address (normally, we expect one o/p)
        [o] -> o
        _   -> traceError "expected exactly one game output"

    -- the producer of the UTxO is required the actual datum to retrieve the datum
    outputDatum :: GameDatum    -- get the Datum at ownOutput
    outputDatum = case gameDatum $ txOutDatumHash ownOutput >>= flip findDatum info of  -- findDatum :: DatumHash -> TxInfo -> Maybe Datum (we apply the TxInfo to 
                                                                                                -- findDatum using flip (it flips the two arguments of a function))                           
        Nothing -> traceError "game output datum not found"
        Just d  -> d                                                                    -- this will return the datum if its included in the txn

    -- this is used to check if the choice given by the first player is legit after it won this round
               -- Hash it submitted -> the revealed nonce -> move both players made
    checkNonce :: BuiltinByteString -> BuiltinByteString -> GameChoice -> Bool
    checkNonce bs nonce cSecond = sha2_256 (nonce `appendByteString` cFirst) == bs
      where
        cFirst :: BuiltinByteString -- we need the BuiltinByteString version of the move made by the first player
        cFirst = case cSecond of    -- This function should run when the first player wins, so we are checking for what makes Player 1 win (in this case - same choice)
            Zero -> bsZero' 
            One  -> bsOne'

    -- Here we give the NFT back to the first player (the one who initiated the game), after the game is over (no matter who wins)
    nftToFirst :: Bool  -- Return true if the output of the txn implies that the NFT is paid to the first player
    nftToFirst = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ gFirst game) (gToken game) == 1
    
    -- NOTE THAT: we are only checking the payment part here using valuePaidTo. Had this game included any staking rewards, we are not checking whether the staking
    -- rewards go to the first player or the winner ... this is a potential security risk


{-# INLINABLE  mkWrappedEvenOddValidator #-}
mkWrappedEvenOddValidator :: Game -> BuiltinByteString -> BuiltinByteString -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedEvenOddValidator game bsZero' bsOne' = wrapValidator . mkGameValidator

bsZero, bsOne :: BuiltinByteString  -- Define the appropriate Strings to use as moves
bsZero = "0"
bsOne  = "1"

validator :: Game -> Validator
validator game = mkValidatorScript ($$(compile [|| mkWrappedEvenOddValidator ||])
                    `applyCode` liftCode game     -- Apply the game rules (The players, the staking amount, the deadlines, the NFT)
                    `applyCode` liftCode bsZero
                    `applyCode` liftCode bsOne)


-- data Gaming
-- instance Scripts.ValidatorTypes Gaming where
--     type instance DatumType Gaming = GameDatum
--     type instance RedeemerType Gaming = GameRedeemer

-- typedGameValidator :: Game -> Scripts.TypedValidator Gaming
-- typedGameValidator game = Scripts.mkTypedValidator @Gaming
--     ($$(PlutusTx.compile [|| mkGameValidator ||])
--         `PlutusTx.applyCode` PlutusTx.liftCode game     -- Apply the game rules (The players, the staking amount, the deadlines, the NFT)
--         `PlutusTx.applyCode` PlutusTx.liftCode bsZero
--         `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
--     $$(PlutusTx.compile [|| wrap ||])
--   where
--     wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

-- ====================== ON / OFF CHAIN =========================

-- gameValidator :: Game -> Validator
-- gameValidator = Scripts.validatorScript . typedGameValidator

-- gameAddress :: Game -> Ledger.Address
-- gameAddress = scriptAddress . gameValidator

saveVal :: Game -> IO ()
saveVal = writeValidatorToFile "../assets/even-odd.plutus" . validator

-- ======================== OFF CHAIN Code =======================
