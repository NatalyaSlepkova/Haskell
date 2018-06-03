{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module FileSystem
  ( FS (..)
  , getFS
  , name
  , contents
  , cd
  , ls
  , files
  , dirs
  , file
  , changeExt
  , subtreeNames
  , rmdir
  , walker
  ) where

import           Control.Lens
import           Control.Monad.State (MonadIO, MonadState, StateT (..), get,
                                      modify, runStateT)
import           Control.Monad.Trans (liftIO)
import           Data.List           (intercalate)
import           System.Directory    (doesDirectoryExist, listDirectory)
import           System.FilePath     (replaceExtension, splitPath, takeFileName,
                                      (</>))


data FS
  = Dir  { _name     :: FilePath
         , _contents :: [FS] }
  | File { _name     :: FilePath }

instance Show FS where
    show fs = go fs 0 True
      where
        go :: FS -> Int -> Bool -> String
        go (File name)     s islast = enter name s islast
        go (Dir name cont) s islast = enter name s islast
            ++ concat (inners cont (s + 1))
        inners []   _ = []
        inners cont s = map (goeoln s False) (init cont) ++ [goeoln s True $ last cont]
        enter name s islast = dup s "   " ++ (if islast then "└─ " else "├─ ") ++ name
        dup n str = concat $ replicate n str
        goeoln s islast fs' = '\n' : go fs' s islast

getFS ::  FilePath -> IO FS
getFS path = do
    isdir <- doesDirectoryExist path
    if isdir
    then do
        cont <- listDirectory path
        let dirname = last $ splitPath path
        Dir dirname <$> mapM (getFS . (</>) path) cont
    else return $ File $ takeFileName path


makeLenses ''FS
makePrisms ''FS


isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir _         = False

isFile :: FS -> Bool
isFile = not . isDir

isEmpty :: FS -> Bool
isEmpty (Dir _ _cs) = null _cs
isEmpty _           = False


cd :: FilePath -> Traversal' FS FS
cd dirname = contents . traversed . filtered cdpred
  where
    cdpred :: FS -> Bool
    cdpred x = isDir x && x ^. name == dirname

ls :: Traversal' FS FS
ls = contents . traversed

files :: Traversal' FS FS
files = ls . filtered isFile

dirs :: Traversal' FS FS
dirs = ls . filtered isDir

file :: FilePath -> Traversal' FS FS
file filename = ls . filtered filepred
  where
    filepred :: FS -> Bool
    filepred x = isFile x && x ^. name == filename


changeExt :: String -> FS -> FS
changeExt ext = files . name %~ flip replaceExtension ext

subtreeNames :: Traversal' FS FilePath
subtreeNames = go False
  where
    go :: Bool -> Traversal' FS FilePath
    go dep f (Dir cname _cs) =
        let inner = (++) <$> traverse (name f) (filter isFile _cs) <*> traverse (go True f) (filter isDir _cs)
        in if dep
            then Dir <$> f cname <*> inner
            else fmap (Dir cname) inner
    go _   _ fs              = pure fs

rmdir :: FilePath -> FS -> FS
rmdir _   fs@(File _) = fs
rmdir dir fs          = Dir (fs ^. name) newContents
  where
    newContents = filter (\x -> not (isEmpty x) || (x ^. name) /= dir) (fs ^.. ls)



data WalkerInfo = WI
  { _wifs    :: [FS]
  , _wiroot  :: FilePath
  , _wifiles :: [Int]
  , _widirs  :: [Int]
  }

makeLenses ''WalkerInfo

curPath :: WalkerInfo -> FilePath
curPath (WI f _ _ _) = intercalate "/" $ map (^. name) f

instance Show WalkerInfo where
    show wi =
        "In " ++ curPath wi ++
        "\nFiles root " ++ root ++ ": " ++ show (head $ wi ^. wifiles) ++
        "\nDirectories from " ++ root ++ ": " ++ show (head $ wi ^. widirs)
      where
        root = show $ wi ^. wiroot

data Cmd = CD FilePath | UP | ERROR

parseCmd :: String -> Cmd
parseCmd input =
    case length ws of
        1 -> case head ws of
            "up" -> UP
            _    -> ERROR
        2 -> case head ws of
            "cd" -> CD $ unwords $ tail ws
            _    -> ERROR
        _ -> ERROR
  where
    ws = words input

walk :: (MonadState WalkerInfo m, MonadIO m) => m ()
walk = do
    info <- get
    liftIO $ print info
    let isroot = length (info ^. wifiles) == 1
    let fcnt = head $ info ^. wifiles
    let dcnt = head $ info ^. widirs
    let curFs = head $ info ^. wifs

    liftIO $ putStr "> "
    input <- liftIO getLine

    case parseCmd input of
        (CD toName) -> do
            let maybeToFs = curFs ^? cd toName
            case maybeToFs of
                Just toFs -> do
                    modify ((%~) wifs (toFs :))
                    modify ((%~) wifiles (fcnt + length (toFs ^.. files):))
                    modify ((%~) widirs (dcnt + length (toFs ^.. dirs):))
                _         -> liftIO $ putStrLn $ "Cannot find directory: " ++ toName
        UP -> if isroot then
                  liftIO $ putStrLn "Impossible to go up"
              else do
                  modify $ (%~) wifs tail
                  modify $ (%~) wifiles tail
                  modify $ (%~) widirs tail
        _  -> liftIO $ putStrLn $ "Unknown command: " ++ show input

    walk

walker :: FilePath -> IO()
walker path = do
    curFs <- getFS path
    let fcnt = length (curFs ^.. files)
    let dcnt = length (curFs ^.. dirs)
    let info = WI [curFs] path [fcnt] [dcnt]
    _ <- runStateT walk info
    return ()
