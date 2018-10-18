#!/usr/bin/env stack
-- stack --resolver lts-12.13 --install-ghc runghc

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
docsTmp = docs </> generated </> "tmp"

templates :: Shelly.FilePath
templates = "templates"

templatesDiversen :: Shelly.FilePath
templatesDiversen = templates </> "diversen"

-- Sources

sourceBases :: [T.Text]
sourceBases =
  [ "index"
  , "math-basics"
  , "template-stdjareport"]

-- Main

main :: IO ()
main = shelly $ verbosely $ do
  rm_rf mdGenerated
  mkdir_p mdGenerated
  mapM_ runPandoc sourceBases
  rm_rf docs
  cp_r mdGenerated docs
  rm_rf docsTmp
  mapM_ (\ file -> cp_r (templatesDiversen </> file) (docs </> ""))
    ["jquery.sticky-kit.js", "menu", "script.js", "template.css"]

runPandoc :: T.Text -> Sh ()
runPandoc basename = do
  cd md
  lastModified <-
    run "git" [ "log"
              , "-1"
              , "--date=format:%Y 年 %m 月 %d 日"
              , "--format=%ad"
              , "--"
              , toT $ basename <.> "md"]
  siteModified <-
    if basename == "index" then
      do date <- run "git" [ "log"
                           , "-1"
                           , "--date=format:%Y 年 %m 月 %d 日"
                           , "--format=%ad"]
         return ["--metadata=sitedate:" `T.append` date]
    else
      return []
  run_ "pandoc" $
    [ "-f", "gfm"
    , "-t", "html5"
    , "--metadata-file", toT $ ".." </> "metadata.yaml"
    , "--metadata=date:" `T.append` lastModified]
    ++
    siteModified
    ++
    [ "--template", toT $ ".." </> templatesDiversen </> "standalone.html"
    -- TODO(nekketsuuu): CSS is URL, not FilePath
    , "--css", toT $ ".." </> templatesDiversen </> "template.css"
    , "--filter", "PandocPagetitle-exe"
    , "--filter", "SatysfiFilter-exe"
    , "--toc"
    , "--toc-depth=2"
    , "-o", toT $ generated </> fromText basename <.> "html"
    , toT $ basename <.> "md"]
  cd ".."
  where toT = toTextIgnore
