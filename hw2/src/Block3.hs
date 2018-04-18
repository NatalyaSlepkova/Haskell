module Block3 where
 
import           Control.Applicative
import           Data.List
import           Data.Maybe
import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.Map            (Map)
import qualified Data.Map            as Map

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }
