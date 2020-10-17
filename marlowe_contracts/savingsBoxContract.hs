{-# LANGUAGE OverloadedStrings #-}
module SavingsBox where

import Language.Marlowe

main :: IO ()
main = print . pretty $ contract

contract :: Contract
contract =  Let (ValueId "kevin-won") (Constant 0) 
                (Let (ValueId "mike-won") (Constant 0) 
                    (Let (ValueId "arjun-won") (Constant 0) 
                        (gameLoop 0)))

{- this is the main loop of the game, this is called recursively until everyone wins -}
gameLoop :: Integer -> Contract
gameLoop slots =    When [Case kevinDepositAction 
                        (When [Case mikeDepositAction 
                            (When [Case arjunDepositAction (afterDepositing slots)] 
                                (depositTimeOut slots)
                                Close)] 
                        (depositTimeOut slots)
                        Close)] 
                    (depositTimeOut slots)
                    Close

{- the deposit time is chosen to be 10 slots, all 
players must deposit before this slot at the begining of each game -}
depositTimeOut :: Integer -> Slot
depositTimeOut slots = Slot (slots + 10)

{- deposit made by each player -}
kevinDepositAction, mikeDepositAction, arjunDepositAction :: Action
kevinDepositAction = Deposit "kevin" "kevin" ada depositAmount
mikeDepositAction = Deposit "mike" "mike" ada depositAmount
arjunDepositAction = Deposit "arjun" "arjun" ada depositAmount

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
                                Close)
                        (If (winObservation 1 0 0)
                             (When [
                                Case mikeChoice (mikeWon slots), 
                                Case arjunChoice (arjunWon slots)] 
                                (choosingPeriod slots)
                                Close)
                        (If (winObservation 0 1 0)
                             (When [
                                Case kevinChoice (kevinWon slots), 
                                Case arjunChoice (arjunWon slots)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 0 0 1)
                            (When [
                                Case kevinChoice (kevinWon slots), 
                                Case mikeChoice (mikeWon slots)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 1 1 0)
                            (When [
                                Case arjunChoice (arjunWon slots)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 0 1 1)
                            (When [
                                Case mikeChoice (mikeWon slots), 
                                Case arjunChoice (arjunWon slots)] 
                                (choosingPeriod slots)
                                Close)
                        (If (winObservation 1 0 1)
                            (When [
                                Case mikeChoice (mikeWon slots)] 
                                (choosingPeriod slots) 
                                Close)
                        (Close)))))))

winObservation :: Integer -> Integer-> Integer -> Observation
winObservation k m a = (AndObs 
                            (AndObs 
                                (ValueEQ (UseValue (ValueId "kevin-won")) (Constant k)) 
                                (ValueEQ (UseValue (ValueId "mike-won")) (Constant m))) 
                            (ValueEQ (UseValue (ValueId "arjun-won")) (Constant a)))

choosingPeriod :: Integer -> Slot
choosingPeriod slots = Slot (slots + 100)

{- kevin gets paid -}
kevinWon :: Integer -> Contract
kevinWon slots =Pay "kevin" (Party "kevin") ada depositAmount  
                    (Pay "mike" (Party "kevin") ada depositAmount 
                        (Pay "arjun" (Party "kevin") ada depositAmount 
                            ((Let (ValueId "kevin-won") (Constant 1) Close))))

{- mike gets paid -}
mikeWon :: Integer -> Contract
mikeWon slots = Pay "mike" (Party "mike") ada depositAmount
                    (Pay "kevin" (Party "mike") ada depositAmount 
                        (Pay "arjun" (Party "mike") ada depositAmount
                            ((Let (ValueId "mike-won") (Constant 1) Close))))

{- arjun gets paid -}
arjunWon :: Integer -> Contract
arjunWon slots =Pay "arjun" (Party "arjun") ada depositAmount 
                    (Pay "kevin" (Party "arjun") ada depositAmount 
                        (Pay "mike" (Party "arjun") ada depositAmount 
                            ((Let (ValueId "arjun-won") (Constant 1) Close))))

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
depositAmount :: (Value Observation)
depositAmount = Constant 100
