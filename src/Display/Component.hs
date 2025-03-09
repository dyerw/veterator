-- | This is all brainstorming and thoughts rn
module Display.Component where

-- import Control.Monad.Reader (Reader)
-- import Display.View (View (..))
-- import FRP.Yampa (SF)

data Layout prim = Group [prim] | Translate Int Int prim | Leaf prim | Nub

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
