{-# LANGUAGE OverloadedStrings #-}
module Escrow where

import Language.Marlowe

main :: IO ()
main = print . pretty $ contract

contract :: Contract
contract =  Let (ValueId 1) (Constant 0) 
                (Let (ValueId 2) (Constant 0) 
                    (Let (ValueId 3) (Constant 0) 
                        (Let (ValueId 4) (Constant 0)
                            (gameLoop 0))))

{- this is the main loop of the game, this is called recursively until everyone wins -}
gameLoop :: Integer -> Contract
gameLoop slots = If (AndObs (AndObs (ValueEQ (UseValue (ValueId 0)) (Constant 1)) (ValueEQ (UseValue (ValueId 1)) (Constant 1))) (ValueEQ (UseValue (ValueId 3)) (Constant 1)))
                    (When [Case kevinDepositAction 
                        (When [Case mikeDepositAction 
                            (When [Case arjunDepositAction (afterDepositing slots)] 
                                (depositTimeOut slots)
                                Refund)] 
                        (depositTimeOut slots)
                        Refund)] 
                    (depositTimeOut slots)
                    Refund)
                    Refund

{- the deposit time is chosen to be 10 slots, all 
players must deposit before this slot at the begining of each game -}
depositTimeOut :: Integer -> Slot
depositTimeOut slots = Slot (slots + 10)

{- deposit made by each player -}
kevinDepositAction, mikeDepositAction, arjunDepositAction :: Action
kevinDepositAction = Deposit "kevin" "kevin" depositAmount
mikeDepositAction = Deposit "mike" "mike" depositAmount
arjunDepositAction = Deposit "arjun" "arjun" depositAmount

{- wait for certain amount of slots before drawing the winners -}
afterDepositing :: Integer -> Contract
afterDepositing slots = When [] (waitingPeriod slots) (claimRewards slots)

waitingPeriod :: Integer -> Slot
waitingPeriod slots = Slot (slots + 90)

{- now time to claim the rewards -}
claimRewards :: Integer -> Contract
claimRewards slots =    If (winObservation 0 0 0)
                            (When [
                                Case kevinChoice (kevinWon slots), 
                                Case mikeChoice (mikeWon slots), 
                                Case arjunChoice (arjunWon slots)] 
                                (choosingPeriod slots)
                                Refund)
                        (If (winObservation 1 0 0)
                             (When [
                                Case mikeChoice (mikeWon slots), 
                                Case arjunChoice (arjunWon slots)] 
                                (choosingPeriod slots)
                                Refund)
                        (If (winObservation 0 1 0)
                             (When [
                                Case kevinChoice (kevinWon slots), 
                                Case arjunChoice (arjunWon slots)] 
                                (choosingPeriod slots) 
                                Refund)
                        (If (winObservation 0 0 1)
                            (When [
                                Case kevinChoice (kevinWon slots), 
                                Case mikeChoice (mikeWon slots)] 
                                (choosingPeriod slots) 
                                Refund)
                        (If (winObservation 1 1 0)
                            (When [
                                Case arjunChoice (arjunWon slots)] 
                                (choosingPeriod slots) 
                                Refund)
                        (If (winObservation 0 1 1)
                            (When [
                                Case mikeChoice (mikeWon slots), 
                                Case arjunChoice (arjunWon slots)] 
                                (choosingPeriod slots)
                                Refund)
                        (If (winObservation 1 0 1)
                            (When [
                                Case mikeChoice (mikeWon slots)] 
                                (choosingPeriod slots) 
                                Refund)
                        (Refund)))))))

winObservation :: Integer -> Integer-> Integer -> Observation
winObservation k m a = (AndObs (AndObs (ValueEQ (UseValue (ValueId k)) (Constant 0)) (ValueEQ (UseValue (ValueId m)) (Constant 0))) (ValueEQ (UseValue (ValueId a)) (Constant 0)))

choosingPeriod :: Integer -> Slot
choosingPeriod slots = Slot (slots + 100)

{- kevin gets paid -}
kevinWon :: Integer -> Contract
kevinWon slots = Pay "mike" (Party "kevin") depositAmount 
                    (Pay "arjun" (Party "kevin") depositAmount Refund)

{- mike gets paid -}
mikeWon :: Integer -> Contract
mikeWon slots = Pay "kevin" (Party "mike") depositAmount 
                    (Pay "arjun" (Party "mike") depositAmount Refund)

{- arjun gets paid -}
arjunWon :: Integer -> Contract
arjunWon slots = Pay "kevin" (Party "arjun") depositAmount 
                    (Pay "mike" (Party "arjun") depositAmount Refund)

{- magic number choices made by participants -}
kevinChoice, mikeChoice, arjunChoice :: Action
kevinChoice = choice "kevin" magicBound
mikeChoice = choice "mike" magicBound
arjunChoice = choice "arjun" magicBound

{- if a player chooses this magic number he/she gets 
   all the money in the pot-}
choice :: Party -> [Bound] -> Action
choice party bounds = Choice (ChoiceId choiceName party) bounds

choiceName :: ChoiceName
choiceName = "choice"

magicBound :: [Bound]
magicBound = [Bound magicNumber magicNumber]

magicNumber :: Integer
magicNumber = 1

{- this is amount players of the kitty party have to deposit
   for being eligible to play -}
depositAmount :: Value
depositAmount = Constant 100

