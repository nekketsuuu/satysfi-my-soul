#!/usr/bin/env stack
-- stack --resolver lts-12.13 --install-ghc runghc

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Shelly
default (T.Text)

-- Dirs

md :: Shelly.FilePath
md = "md"

generated :: Shelly.FilePath
generated = "generated"

mdGenerated :: Shelly.FilePath
mdGenerated = md </> generated

docs :: Shelly.FilePath
docs = "docs"

docsTmp :: Shelly.FilePath
docsTmp = docs </> "tmp"

templates :: Shelly.FilePath
templates = "templates"

templatesDiversen :: Shelly.FilePath
templatesDiversen = templates </> "diversen"

-- Sources

uriRoot :: T.Text
uriRoot = "https://nekketsuuu.github.io/satysfi-my-soul/"

sourceBases :: [T.Text]
sourceBases =
  [ "index"
  , "template-stdjareport"
  , "math-basics"
  , "math-frac"
  , "math-paren"
  , "programming-optional-arguments"
  , "programming-module"
  , "programming-signature"
  , "develop-regexp"
  , "code-hello-world"
  , "code-fizzbuzz"
  , "code-99-bottles-of-beer"
  , "code-day-of-date"
  , "code-kansuji-of-int"
  , "code-pi"
  , "others-errors"]

data Date = Date (T.Text, T.Text, T.Text)

getLastModifiedMap :: Sh (Map.Map T.Text Date)
getLastModifiedMap = do
  kas <- mapM getPair sourceBases
  return $ Map.fromList kas
  where
    getPair base = do
      modified <- getLastModified base
      return (base, modified)

getLastModified :: T.Text -> Sh Date
getLastModified base = do
  dataStr <- 
    run "git" [ "log"
              , "-1"
              , "--date=format:%Y/%m/%d"
              , "--format=%ad"
              , "--"
              , toTextIgnore $ base <.> "md"]
  -- tenuki
  let year : month : date : _ = T.splitOn "/" $ T.strip dataStr
  return $ Date (year, month, date)

-- Main

main :: IO ()
main = shelly $ verbosely $ do
  rm_rf mdGenerated
  mkdir_p mdGenerated
  cd md
  lastModifiedMap <- getLastModifiedMap
  mapM_ (runPandoc lastModifiedMap)sourceBases
  cd ".."
  rm_rf docs
  cp_r mdGenerated docs
  rm_rf docsTmp
  mapM_ (\ file -> cp_r (templatesDiversen </> file) (docs </> ""))
    ["jquery.sticky-kit.js", "menu", "script.js", "template.css"]
  saveSitemap lastModifiedMap

runPandoc :: (Map.Map T.Text Date) -> T.Text -> Sh ()
runPandoc lastModifiedMap basename = do
  let Date (lastYear, lastMonth, lastDate) = lastModifiedMap Map.! basename
      lastModified = T.unwords [lastYear, "年", lastMonth, "月", lastDate, "日"]
  siteModifiedData <-
    if basename == "index" then
      do date <- run "git" [ "log"
                           , "-1"
                           , "--date=format:%Y 年 %m 月 %d 日"
                           , "--format=%ad"]
         return ["--metadata=sitedate:" `T.append` date]
    else
      return []
  run_ "pandoc" $
    -- I use deprecated `markdown_github` instead of `gfm` to use several extensions.
    -- TODO(nekketsuuu): Use `gfm`
    [ "-f", "markdown_github" `T.append`
            "+fenced_code_attributes" `T.append`
            "+markdown_attribute" `T.append`
            "+auto_identifiers" `T.append`
              -- Is this pandoc's bug?
              -- `markdown_github` enables gfm_auto_identifiers though....
            "-ascii_identifiers"
    , "-t", "html5"
    , "--metadata-file", toT $ ".." </> "metadata.yml"
    , "--metadata=date:" `T.append` lastModified]
    ++
    siteModifiedData
    ++
    [ "--template", toT $ ".." </> templatesDiversen </> "standalone.html"
    -- TODO(nekketsuuu): CSS should be written as a URL, not a FilePath
    , "--filter", "PandocPagetitle-exe"
    , "--filter", "SatysfiFilter-exe"
    , "--toc"
    , "--toc-depth=2"
    , "-o", toT $ generated </> fromText basename <.> "html"
    , toT $ basename <.> "md"]
  where toT = toTextIgnore

-- Sitemap

saveSitemap :: (Map.Map T.Text Date) -> Sh ()
saveSitemap lastModifiedMap =
  Shelly.writefile (docs </> "sitemap.xml") (sitemap lastModifiedMap)

sitemap :: (Map.Map T.Text Date) -> T.Text
sitemap lastModifiedMap =
  T.unlines $ header ++ (concat $ map toXml sourceBases) ++ footer
  where
    header = [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
             , "<urlset xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd\" xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"]
    footer = [ "</urlset>" ]
    lastmod base =
      let Date (year, month, date) = lastModifiedMap Map.! base in
        T.concat [year, "-", month, "-", date, "T00:00:00+09:00"]
    toXml base =
      [ "<url>"
      , T.concat ["<loc>", uriRoot, base, ".html</loc>"]
      , T.concat ["<lastmod>", lastmod base, "</lastmod>"]
      , "</url>"]
