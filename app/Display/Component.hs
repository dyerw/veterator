module Display.Component where

import Control.Monad.Reader (Reader)
import Display.View (View)

newtype Component s = Component {unComponent :: Reader s View}