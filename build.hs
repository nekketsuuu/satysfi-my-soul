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
docsTmp = docs </> "tmp"

templates :: Shelly.FilePath
templates = "templates"

templatesDiversen :: Shelly.FilePath
templatesDiversen = templates </> "diversen"

-- Sources

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
    -- TODO(nekketsuuu): CSS is URL, not FilePath
    , "--filter", "PandocPagetitle-exe"
    , "--filter", "SatysfiFilter-exe"
    , "--toc"
    , "--toc-depth=2"
    , "-o", toT $ generated </> fromText basename <.> "html"
    , toT $ basename <.> "md"]
  cd ".."
  where toT = toTextIgnore
