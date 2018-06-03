{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Template
  ( chooseByIndices
  , ShowText (..)
  , genShowText
  ) where

import           Control.Monad       (replicateM)
import qualified Data.Text           as T
import           Language.Haskell.TH


chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices lng indices = do
    as <- replicateM lng (newName "a")
    let func i = varE $ as !! i
    lamE [tupP $ map varP as] $ tupE $ map func indices


class ShowText a where
    showText :: a -> T.Text

instance ShowText Int where
  showText = T.pack . show

instance ShowText T.Text where
  showText = id

instance ShowText String where
  showText = T.pack


spaceShow :: ShowText a => a -> T.Text
spaceShow x = T.pack " " `T.append` showText x

commaShow :: ShowText a => a -> T.Text
commaShow x = T.pack ", " `T.append` showText x

strangeShow :: ShowText a => a -> T.Text
strangeShow x = T.pack " {" `T.append` showText x


genShowText :: Name -> Q [Dec]
genShowText ty = do
    (TyConI tyCon) <- reify ty
    constructors <- case tyCon of
        DataD    _ _ _ _ cs _ -> return cs
        NewtypeD _ _ _ _ c  _ -> return [c]
        _ -> fail "genShowText: tycon can't be a type synonym."

    let conc :: Exp -> Exp -> Exp
        conc l r = InfixE (Just l) (VarE 'T.append) (Just r)

    let appPack :: Exp -> Exp
        appPack = AppE (VarE 'showText)

    let makeClause' :: Name -> [Exp] -> [Name] -> String -> Name -> String -> Name -> Q Clause
        makeClause' name aprefs fieldNames pr midSep sf sep = do
            let varEs = map (\(p, fn) -> p `conc` appPack (VarE fn)) $ zip aprefs fieldNames
            let vars = [conP name (map varP fieldNames)]
            let prefix = appPack $ LitE $ StringL pr
            let suffix = appPack $ LitE $ StringL sf
            let f :: Exp -> Exp -> Exp
                f l@(AppE _ _) r = l `conc` AppE (VarE midSep) r
                f l            r = l `conc` AppE (VarE sep) r
            let resFold :: Exp
                resFold = foldl f prefix varEs
            let body = normalB $ pure $ resFold `conc` suffix
            clause vars body []

    let makeClause :: Con -> Q Clause
        makeClause (NormalC name fields) = do
            fieldNames <- replicateM (length fields) (newName "hu")
            let prefs = map (const $ appPack $ LitE $ StringL "") fieldNames
            makeClause' name prefs fieldNames (nameBase name) 'spaceShow "" 'spaceShow
        makeClause (RecC name fields) = do
            fieldNames <- replicateM (length fields) (newName "hu")
            let prefs = map (\(x, _, _) -> appPack $ LitE $ StringL $ nameBase x ++ " = ") fields
            makeClause' name prefs fieldNames (nameBase name) 'strangeShow "}" 'commaShow
        makeClause _ = undefined

    let forShowText :: Q Dec
        forShowText = funD 'showText (map makeClause constructors)

    let instanceType = appT (conT ''ShowText) (conT ty)

    sequence [instanceD (return []) instanceType [forShowText]]
