-- Author: David Naughton
-- Login ID: dna@student.unimelb.edu.au
-- Subject: Declarative Programming (COMP30020_2020_SM2) - Project 2 (The Game
-- of Musician)

-- Purpose: As a performer, to find the target chord given by the composer  
-- with the fewest possible guesses.

-- We assume the composer chooses a target chord consisting of exactly 3 
-- pitches that each consist of a note and an octave. The performer guesses the
-- chord and receives feedback for correct pitches, notes and octaves. 

-- The first part of the guessing strategy filters out potential targets 
-- from the game state which are inconsistent with previous guesses. A 
-- potential target is inconsistent with a previous guess if the feedback they 
-- produce differs from the feedback produced by the previous guess and the 
-- target. 

-- The next part of the guessing strategy is to select the best guess from the
-- filtered game state. The best guess is the one which filters out the most 
-- chords from the game state if incorrect. To find this, we compute the 
-- average number of possible targets (to a constant factor) weighted by
-- frequency that will remain after each guess (hint no. 6 from Grok). We then
-- choose the guess which the minimum expected number of remaining possible 
-- targets. 

module Proj2 (Pitch(..), toPitch, feedback, 
                GameState, initialGuess, nextGuess) where

import Data.Char
import Data.List
import Data.Maybe

-- define the pitch data type and show instance implementation
data Pitch = Pitch Char Char deriving (Eq)
instance Show Pitch where show pitch = toString pitch

-- a chord is a list of pitches and a game state is a list of chords
type Chord = [Pitch]
type GameState = [Chord]

-- Generates a list of all possible chords. 'initGuess' is set to the best
-- guess obtained by passing the list of all chords through 'bestGuess', it has
-- been hardcoded for time efficiency. The best guess is removed from the list
-- of all chords to give initial GameState. 
initialGuess :: ([Pitch], GameState)
initialGuess = (initGuess, initGameState)
    where 
        allPitches =  [(Pitch note octave) | note <- ['A'..'G'], 
                                                        octave <- ['1'..'3']]
        chords = [chord | chord <- subsequences allPitches, length chord == 3]
        initGuess = [Pitch 'A' '2', Pitch 'B' '1', Pitch 'C' '1']
        initGameState = delete initGuess chords

-- we map all chords in previous game state through 'feedback' with 
-- previous guess as target and if feedback matches previous feedback then 
-- keep chord in game state otherwise discard. Then we find the best guess of
-- the game state and remove it for use as our next guess. 
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch], GameState)
nextGuess (previousGuess, previousGameState) previousFeedback = 
                                                            (guess, gameState)
    where  
        gameStateWithGuess = filter (\x -> feedback previousGuess x 
                                        == previousFeedback) previousGameState
        guess = bestGuess gameStateWithGuess
        gameState = delete guess gameStateWithGuess

-- takes a target and guess chord, converts the pitches into strings 
-- for processing, finds matching pitches using intersection and removes them.
-- Then isolates head of strings which are notes and counts matches. Similarly
-- for tail of strings which is the octaves.
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback target guess = (correctPitchLength, correctNoteLength, 
                                                        correctOctaveLength)
    where
        targetStrings = map toString target
        guessStrings = map toString guess

        correctPitches = intersect' guessStrings targetStrings
        correctPitchLength = length correctPitches
        unmatchedTargetStrings = targetStrings \\ correctPitches 
        unmatchedGuessStrings = guessStrings \\ correctPitches
        
        targetNotes = map head unmatchedTargetStrings
        guessNotes = map head unmatchedGuessStrings
        correctNotes = intersect' guessNotes targetNotes 
        correctNoteLength = length correctNotes

        targetOctaves = map tail unmatchedTargetStrings
        guessOctaves = map tail unmatchedGuessStrings
        correctOctaves = intersect' guessOctaves targetOctaves 
        correctOctaveLength = length correctOctaves

------------------------------- Helper Functions ------------------------------

-- the 'best guess' filters out the most chords from the game state if 
-- incorrect. To find this we compute the average number of possible targets 
-- (to a constant factor) that will remain after each guess, giving the 
-- expected number of remaining possible targets for each guess, and choose the
-- guess with the minimum expected number of remaining possible targets 
bestGuess :: GameState -> [Pitch]
bestGuess gameState = guess
    where 
        argByConstantFactor = map (\x -> getARGByConstantFactor x 
                                                (gameState \\ [x])) gameState 
        -- 'fromJust' raises error if elemIndex returns 'Nothing', however
        -- should never occur since minimum value must occur in list
        minimumARGIndex = fromJust (elemIndex (minimum argByConstantFactor) 
                                                        argByConstantFactor)
        guess = gameState !! minimumARGIndex 

-- takes a candidate guess and the gameState and returns a value directly
-- proportional to average remaining guesses (ARG) (i.e. hasn't been  divided
-- by length of GameState but doesn't affect minimisation). Get the feedback
-- scores for target with all chords in game state, then sort and group to get
-- distribution. Take the sum of squared frequencies to get numerator of ARG.
getARGByConstantFactor :: [Pitch] -> GameState -> Int
getARGByConstantFactor target gameState = argByConstantFactor 
    where 
        scores = map (feedback target) gameState
        processedScores = group (sort scores)
        argByConstantFactor = foldl (\x y -> x+(length y)^2) 0 processedScores

-- converts strings with two characters which match one of the possible pitch
-- strings to a pitch. Otherwise returns Nothing.
toPitch :: String -> Maybe Pitch
toPitch [] = Nothing
toPitch (_:[]) = Nothing
toPitch (_:_:_:_) = Nothing
toPitch (note:octave:[])  
    | elem [note, octave] [[note, octave] | note <- ['A'..'G'], 
                            octave <- ['1'..'3']] = Just (Pitch note octave)
    | otherwise = Nothing

-- convert a pitch to a string
toString :: Pitch -> String
toString (Pitch note octave) = [note, octave]

-- helper function for intersection without duplicates 
intersect' xs ys = xs \\ (xs \\ ys)