module Gemini.UI.Actions where

import           Relude

import           Data.Angle
import           Data.Permutation
import qualified Data.Sequence          as Seq
import           Optics
import           Optics.State.Operators

import           Gemini.Types


-- | UI Operations
applyMotionToStore :: Motion -> Store -> Store
applyMotionToStore motion = execState $ do
  -- apply to puzzle
  (#gemini %= applyToGemini motion)

  -- if recording, apply to recorded move
  recording <- use $ #options % #recording
  when recording $ #recorded %= applyToHistory motion

  -- apply to history to tally move count
  (#history %= applyToHistory motion)

  -- check if the puzzle is solved
  gemini <- use #gemini
  wasScrambled <- use $ #scrambled
  when (isSolved gemini && wasScrambled) $ do
    (#options % #confetti .= True)
    (#scrambled .= False)


stopRecording :: Store -> Store
stopRecording state = state
  & #options % #recording .~ False
  & #moves %~ updateMoves
  & #history .~ Seq.Empty
  where
    updateMoves =
      case state ^. #history of
        Seq.Empty -> identity
        motions   -> (toMove motions :<|)

    toMove :: Seq Motion -> Move
    toMove motions = Move { motions, moveCycles = locationCycles $ toPerm motions }

    locationCycles :: GeminiPermutation -> Cycles Location
    locationCycles = fmap indexToLocation . toCycles


applyRotation :: Rotation -> Store -> Store
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
    combine x@(Motion m1 r1) (Motion m2 r2)
      | r1 == r2  = x & #amount %~ (+ m2)
      | otherwise = x & #amount %~ (subtract m2)


-- * Drag actions
--

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
    (Just (Ambiguous left right), Just (loc, Turns turns)) ->
      case chosen of
        -- lock choice
        Nothing | abs (turns * 18) > 1 ->
          (#drag % _Just % #chosen ?= if loc == left then L else R)

        -- unlock choice
        Just _  | abs (turns * 18) < 1 ->
          (#drag % _Just % #chosen .= Nothing)

    _ -> pure ()



endDrag :: Point -> Store -> Store
endDrag mouse = execState $ do
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
        Just motion -> modify $ applyMotionToStore motion


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
                let Just origin = store ^? #dom % #ringCenters % ix (location ^. #ring)
                let mouse = drag ^. #currentPoint
                let p = mouse ~~ origin
                abs (norm p - radius)
          if distanceTo loc1 <= distanceTo loc2 then loc1 else loc2



-- | angle of current ring being dragged, (via location that disambiguates)
dragAngle :: Store -> Maybe (Location, Angle)
dragAngle store =
  case store ^. #drag of
    Nothing -> Nothing
    Just drag -> do
      let location = disambiguate store drag
      let angleWith :: Point -> Angle
          angleWith point = do
            let Just origin = store ^? #dom % #ringCenters % ix (location ^. #ring)
            let mouse = drag ^. #currentPoint
            let p = point ~~ origin
            angleToOrigin p

      let DragState { initialPoint, currentPoint } = drag
      let currentAngle = angleWith currentPoint ~~ angleWith initialPoint
      Just $ (location, currentAngle)
