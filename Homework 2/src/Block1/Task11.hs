module Block1.Task11 where

import Text.Read

-- | Takes as input string with numbers
-- If we have wrong data (for example, float number)
-- We return Nothing - otherwise Just with sum numbers returned
stringSum :: String -> Maybe Int
stringSum input = fmap sum (traverse readMaybe (words input))