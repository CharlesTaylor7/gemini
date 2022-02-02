module Gemini.UI.Actions
  ( applyMotionToStore, stopRecording, applyRotation, applyToHistory, applyMove, toContinuation
  , dragAngle
  , startDrag, updateDrag, endDrag
  ) where

import           Relude

import           Data.Angle
import           Data.Permutation
import qualified Data.Sequence            as Seq
import           Optics
import           Optics.State.Operators
import           Shpadoinkle
import qualified Shpadoinkle.Continuation as Continuation

import           Gemini.Jsaddle
import           Gemini.Types


type Action m = StateT Store m ()


toContinuation :: MonadJSM m => Action m -> Continuation m Store
toContinuation = Continuation.kleisli . fmap fmap fmap constUpdate . execStateT


applyMove :: MonadJSM m => Move -> Action m
applyMove move = traverse_ applyMotionUnchecked $ move ^. #motions


-- | apply the motion to the history and the gemini state, but don't check if the puzzle is solved
applyMotionUnchecked :: Monad m => Motion -> Action m
applyMotionUnchecked motion = do
    -- apply to puzzle
    (#gemini %= applyToGemini motion)

    -- if recording, apply to recorded move
    recording <- use $ #options % #recording
    when recording $ #recorded %= applyToHistory motion

    -- apply to history to tally move count
    (#history %= applyToHistory motion)


applyMotionToStore :: MonadJSM m => Motion -> Action m
applyMotionToStore motion = do
  unlocked <- use $ #options % #confetti % to (== Off)
  when unlocked $ do
    -- apply to history & gemini
    applyMotionUnchecked motion
    -- check for win
    checkWin


-- check if the puzzle is solved
checkWin :: MonadJSM m => Action m
checkWin = do
  wasScrambled <- get <&> isn't (#stats % _Nothing)
  isSolved <- use $ #gemini % to isSolved
  when (wasScrambled && isSolved) $ do
    solvedAt <- dateNow
    (#stats % _Just % #solvedAt ?= solvedAt)
    (#options % #confetti .= FadeIn)


stopRecording :: Store -> Store
stopRecording state = state
  & #options % #recording .~ False
  & #moves %~ updateMoves
  & #recorded .~ Seq.Empty
  where
    updateMoves =
      case state ^. #recorded of
        Seq.Empty -> identity
        motions   -> (toMove motions :<|)

    toMove :: Seq Motion -> Move
    toMove motions = Move { motions, moveCycles = locationCycles $ toPerm motions }

    locationCycles :: GeminiPermutation -> Cycles Location
    locationCycles = fmap indexToLocation . toCycles


applyRotation :: MonadJSM m => Rotation -> StateT Store m ()
applyRotation r = applyMotionToStore $ toMotion r
  where
    toMotion :: Rotation -> Motion
    toMotion rotation = Motion { amount = 1, rotation }


-- | Apply a new motion to an existing history of motions
-- Collapses motions of the same ring into 1 larger motion.
-- Allows undoing motions.
-- Removes trivial motions that do nothing.
applyToHistory :: Motion -> Seq Motion -> Seq Motion
applyToHistory motion Seq.Empty           = fromList [ motion ]
applyToHistory next all@(ms :|> prev) =
  if next ^. #rotation % #ring /= prev ^. #rotation % #ring
  then all :|> next
  else case normalize (combine prev next) of
    Just m -> ms :|> m
    _      -> ms
  where
    combine :: Motion -> Motion -> Motion
    combine x@(Motion _ r1) (Motion m2 r2)
      | r1 == r2  = x & #amount %~ (+ m2)
      | otherwise = x & #amount %~ (subtract m2)


-- * Drag actions

startDrag :: Location -> Point -> Store -> Store
startDrag location mouse =
  (#drag) ?~ DragState
    { location = dragRing location
    , chosen = Nothing
    , initialPoint = mouse
    , currentPoint = mouse
    }


updateDrag :: Point -> Store -> Store
updateDrag mouse = execState $ do
  -- update current point to where mouse is
  (#drag % _Just % #currentPoint .= mouse)

  initialLocation <- preuse $ #drag % _Just % #location
  dragged <- get <&> dragAngle
  chosen <- preuse $ #drag % _Just % #chosen

  case (initialLocation, dragged) of
    (Just (Ambiguous left _), Just (loc, Turns turns)) ->
      case chosen of
        -- lock choice
        Nothing | abs (turns * 18) > 1 ->
          (#drag % _Just % #chosen ?= if loc == left then L else R)

        -- unlock choice
        Just _  | abs (turns * 18) < 1 ->
          (#drag % _Just % #chosen .= Nothing)

        -- do nothing
        _ -> pure ()

    _ -> pure ()


endDrag :: Point -> StateT Store JSM ()
endDrag mouse = do
  modify $ updateDrag mouse
  drag <- dragAngle <$> get
  case drag of
    Nothing -> pure ()
    Just (location, theta) -> do
      (#drag .= Nothing)
      let ring = location ^. #ring
      let n = angleToPosition theta
      let motion = Motion { amount = abs n, rotation = Rotation { ring, direction = Clockwise } }
      case normalize motion of
        Nothing     -> pure ()
        Just motion -> applyMotionToStore motion


angleToPosition :: forall n. KnownNat n => Angle -> Cyclic n
angleToPosition (Turns turns) = Cyclic $ floor $ (k * turns) + 0.5
  where k = fromIntegral $ knownInt @n


disambiguate :: Store -> DragState -> Location
disambiguate store drag =
  case drag ^. #location of
    Obvious location -> location
    Ambiguous loc1 loc2 ->
      case drag ^. #chosen of
        Just L -> loc1
        Just R -> loc2
        Nothing -> do
          let distanceTo :: Location -> Double
              distanceTo location = do
                let radius = store ^. #dom ^. #ringRadius
                let origin = ringOrigin store (location ^. #ring)
                let mouse = drag ^. #currentPoint
                let p = mouse ~~ origin
                abs (norm p - radius)
          if distanceTo loc1 <= distanceTo loc2 then loc1 else loc2


ringOrigin :: Store -> Ring -> Point
ringOrigin store ring =
  case store ^? #dom % #ringCenters % ix ring of
    Just origin -> origin
    _           -> error "impossible"

-- | angle of current ring being dragged, (via location that disambiguates)
dragAngle :: Store -> Maybe (Location, Angle)
dragAngle store =
  case store ^. #drag of
    Nothing -> Nothing
    Just drag -> do
      let location = disambiguate store drag
      let angleWith :: Point -> Angle
          angleWith point = do
            let origin = ringOrigin store (location ^. #ring)
            let p = point ~~ origin
            angleToOrigin p

      let DragState { initialPoint, currentPoint } = drag
      let currentAngle = angleWith currentPoint ~~ angleWith initialPoint
      Just $ (location, currentAngle)
