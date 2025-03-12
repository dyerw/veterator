{-# LANGUAGE Arrows #-}

-- | This is all brainstorming and thoughts rn
module Display.Component where

-- import Control.Monad.Reader (Reader)
-- import Display.View (View (..))
-- import FRP.Yampa (SF)

-- data Layout prim = Group [prim] | Translate Int Int prim | Leaf prim | Nub

-- data ComponentReturn s e prim = ComponentReturnComponent (Component s e prim) | ComponentReturnLayout (Layout prim)

-- newtype Component s e prim = Component {unComponent :: SF (s, [e]) (ComponentReturn s prim)}

-- component :: SF ()

-- -- In order to render
-- render :: Component s e v -> (SF (s, [e]) -> v)
-- render cs = undefined

-- data DummyCreature = DummyCreature
-- data CreatureType = Whatever

-- getType :: DummyCreature -> CreatureType
-- getType = undefined

-- data DummyEvent = Foo | Bar

-- -- Contravariant? contramap?
-- selecting :: (s -> a) -> Component a e -> Component s e
-- selecting = undefined

-- Layout is map / select is contramap?
-- layout :: (View -> View) -> Component s View -> Component s View

--  --select ::

-- creaturesComponent :: Component DummyCreature DummyEvent View
-- creaturesComponent = do

--     ctType <- select getType
--     return $ ComponentView

-- data NView i s o = undefined

-- NESTED dpSwitch!!!
-- type Px = (Int, Int)

-- data TextAlignment = LeftAligned | RightAligned | Centered deriving (Show)

-- data Side = TopSide | BottomSide | LeftSide | RightSide deriving (Show)

-- data View
--   = Group [View]
--   | Translate Px View
--   | Sprite String
--   | Label TextAlignment String
--   | -- These are all absolute to the screen and reset the translation context
--     CenterX View
--   | CenterY View
--   | From Side Int View
--   deriving (Show)

-- data In i s = In
--   { inbox :: [i],
--     state :: s
--   }

-- data Out o = Out View [o] | Die

-- type Object i o s = SF (In i s) (Out o)

-- type PretendState = Int

-- data CreatureEvent = TookDamage Int | GainedXP Int

-- damageNumber :: Int -> Object () () ()
-- damageNumber = proc () -> do
--   destroyEvent <- after 1 Die -< ()
--   t <- time -< ()
--   let x = round (t * 20)
--   let y = round (t * (-20))
--   let xTranslate = Translate (V2 x y)
--   returnA -< (xTranslate $ damageView s e, destroyEvent)

-- type Component s ie oe = ()

-- creature :: Component
-- creature = (\)