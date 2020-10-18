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
                                Case kevinChoice (kevinWon slots 1), 
                                Case mikeChoice (mikeWon slots 1), 
                                Case arjunChoice (arjunWon slots 1)] 
                                (choosingPeriod slots)
                                Close)
                        (If (winObservation 1 0 0)
                             (When [
                                Case mikeChoice (mikeWon slots 2), 
                                Case arjunChoice (arjunWon slots 2)] 
                                (choosingPeriod slots)
                                Close)
                        (If (winObservation 0 1 0)
                             (When [
                                Case kevinChoice (kevinWon slots 2), 
                                Case arjunChoice (arjunWon slots 2)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 0 0 1)
                            (When [
                                Case kevinChoice (kevinWon slots 2), 
                                Case mikeChoice (mikeWon slots 2)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 1 1 0)
                            (When [
                                Case arjunChoice (arjunWon slots 3)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 0 1 1)
                            (When [
                                Case kevinChoice (kevinWon slots 3)] 
                                (choosingPeriod slots)
                                Close)
                        (If (winObservation 1 0 1)
                            (When [
                                Case mikeChoice (mikeWon slots 3)] 
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
kevinWon :: Integer -> Integer -> Contract
kevinWon slots round =  Pay "kevin" (Party "kevin") ada depositAmount  
                            (Pay "mike" (Party "kevin") ada depositAmount 
                                (Pay "arjun" (Party "kevin") ada depositAmount 
                                    ((Let (ValueId "kevin-won") (Constant 1) (gameLoop1 100)))))
mikeWon slots round =   Pay "mike" (Party "mike") ada depositAmount
                            (Pay "kevin" (Party "mike") ada depositAmount 
                                (Pay "arjun" (Party "mike") ada depositAmount
                                    ((Let (ValueId "mike-won") (Constant 1) (gameLoop1 100)))))

arjunWon slots round=   Pay "arjun" (Party "arjun") ada depositAmount 
                            (Pay "kevin" (Party "arjun") ada depositAmount 
                                (Pay "mike" (Party "arjun") ada depositAmount 
                                    ((Let (ValueId "arjun-won") (Constant 1) (gameLoop1 100)))))

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

{---------- ROUND 2 ----------}
gameLoop1 :: Integer -> Contract
gameLoop1 slots =    When [Case kevinDepositAction 
                        (When [Case mikeDepositAction 
                            (When [Case arjunDepositAction (afterDepositing1 slots)] 
                                (depositTimeOut slots)
                                Close)] 
                        (depositTimeOut slots)
                        Close)] 
                    (depositTimeOut slots)
                    Close

{- wait for certain amount of slots before drawing the winners -}
afterDepositing1 :: Integer -> Contract
afterDepositing1 slots = When [] (waitingPeriod slots) (claimRewards1 slots)

{- now time to claim the rewards -}
claimRewards1 :: Integer -> Contract
claimRewards1 slots =    If (winObservation 0 0 0)
                            (When [
                                Case kevinChoice (kevinWon1 slots 1), 
                                Case mikeChoice (mikeWon1 slots 1), 
                                Case arjunChoice (arjunWon1 slots 1)] 
                                (choosingPeriod slots)
                                Close)
                        (If (winObservation 1 0 0)
                             (When [
                                Case mikeChoice (mikeWon1 slots 2), 
                                Case arjunChoice (arjunWon1 slots 2)] 
                                (choosingPeriod slots)
                                Close)
                        (If (winObservation 0 1 0)
                             (When [
                                Case kevinChoice (kevinWon1 slots 2), 
                                Case arjunChoice (arjunWon1 slots 2)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 0 0 1)
                            (When [
                                Case kevinChoice (kevinWon1 slots 2), 
                                Case mikeChoice (mikeWon1 slots 2)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 1 1 0)
                            (When [
                                Case arjunChoice (arjunWon1 slots 3)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 0 1 1)
                            (When [
                                Case kevinChoice (kevinWon1 slots 3)] 
                                (choosingPeriod slots)
                                Close)
                        (If (winObservation 1 0 1)
                            (When [
                                Case mikeChoice (mikeWon1 slots 3)] 
                                (choosingPeriod slots) 
                                Close)
                        (Close)))))))

{- kevin gets paid -}
kevinWon1 :: Integer -> Integer -> Contract
kevinWon1 slots round =  Pay "kevin" (Party "kevin") ada depositAmount  
                            (Pay "mike" (Party "kevin") ada depositAmount 
                                (Pay "arjun" (Party "kevin") ada depositAmount 
                                    ((Let (ValueId "kevin-won") (Constant 1) (gameLoop2 200)))))
mikeWon1 slots round =   Pay "mike" (Party "mike") ada depositAmount
                            (Pay "kevin" (Party "mike") ada depositAmount 
                                (Pay "arjun" (Party "mike") ada depositAmount
                                    ((Let (ValueId "mike-won") (Constant 1) (gameLoop2 200)))))

arjunWon1 slots round=   Pay "arjun" (Party "arjun") ada depositAmount 
                            (Pay "kevin" (Party "arjun") ada depositAmount 
                                (Pay "mike" (Party "arjun") ada depositAmount 
                                    ((Let (ValueId "arjun-won") (Constant 1) (gameLoop2 200)))))

{---------- ROUND 3 ----------}
gameLoop2 :: Integer -> Contract
gameLoop2 slots =    When [Case kevinDepositAction 
                        (When [Case mikeDepositAction 
                            (When [Case arjunDepositAction (afterDepositing2 slots)] 
                                (depositTimeOut slots)
                                Close)] 
                        (depositTimeOut slots)
                        Close)] 
                    (depositTimeOut slots)
                    Close

{- wait for certain amount of slots before drawing the winners -}
afterDepositing2 :: Integer -> Contract
afterDepositing2 slots = When [] (waitingPeriod slots) (Close)

{- now time to claim the rewards -}
claimRewards2 :: Integer -> Contract
claimRewards2 slots =    If (winObservation 0 0 0)
                            (When [
                                Case kevinChoice (kevinWon2 slots 1), 
                                Case mikeChoice (mikeWon2 slots 1), 
                                Case arjunChoice (arjunWon2 slots 1)] 
                                (choosingPeriod slots)
                                Close)
                        (If (winObservation 1 0 0)
                             (When [
                                Case mikeChoice (mikeWon2 slots 2), 
                                Case arjunChoice (arjunWon2 slots 2)] 
                                (choosingPeriod slots)
                                Close)
                        (If (winObservation 0 1 0)
                             (When [
                                Case kevinChoice (kevinWon2 slots 2), 
                                Case arjunChoice (arjunWon2 slots 2)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 0 0 1)
                            (When [
                                Case kevinChoice (kevinWon2 slots 2), 
                                Case mikeChoice (mikeWon2 slots 2)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 1 1 0)
                            (When [
                                Case arjunChoice (arjunWon2 slots 3)] 
                                (choosingPeriod slots) 
                                Close)
                        (If (winObservation 0 1 1)
                            (When [
                                Case kevinChoice (kevinWon2 slots 3)] 
                                (choosingPeriod slots)
                                Close)
                        (If (winObservation 1 0 1)
                            (When [
                                Case mikeChoice (mikeWon2 slots 3)] 
                                (choosingPeriod slots) 
                                Close)
                        (Close)))))))

{- kevin gets paid -}
kevinWon2 :: Integer -> Integer -> Contract
kevinWon2 slots round =  Pay "kevin" (Party "kevin") ada depositAmount  
                            (Pay "mike" (Party "kevin") ada depositAmount 
                                (Pay "arjun" (Party "kevin") ada depositAmount 
                                    ((Let (ValueId "kevin-won") (Constant 1) (Close)))))
mikeWon2 slots round =   Pay "mike" (Party "mike") ada depositAmount
                            (Pay "kevin" (Party "mike") ada depositAmount 
                                (Pay "arjun" (Party "mike") ada depositAmount
                                    ((Let (ValueId "mike-won") (Constant 1) (Close)))))

arjunWon2 slots round=   Pay "arjun" (Party "arjun") ada depositAmount 
                            (Pay "kevin" (Party "arjun") ada depositAmount 
                                (Pay "mike" (Party "arjun") ada depositAmount 
                                    ((Let (ValueId "arjun-won") (Constant 1) (Close)))))
