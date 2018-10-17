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
sourceBases = ["index"]

-- Main

main :: IO ()
main = shelly $ verbosely $ do
  mkdir_p mdGenerated
  mapM_ runPandoc sourceBases
  rm_rf docs
  cp_r mdGenerated docs
  rm_rf docsTmp
  mapM_ (\ file -> cp_r (templatesDiversen </> file) (docs </> ""))
    ["jquery.sticky-kit.js", "menu", "script.js", "template.css"]

runPandoc :: T.Text -> Sh ()
runPandoc basename =
  run_ "pandoc" ["-f", "markdown"
                ,"-t", "html5"
                ,"--metadata-file", "metadata.yaml"
                ,"--template", toT $ templatesDiversen </> "standalone.html"
                -- TODO(nekketsuuu): CSS=URL, not FilePath
                ,"--css", toT $ templatesDiversen </> "template.css"
                ,"--filter", "SatysfiFilter-exe"
                ,"--toc"
                ,"--toc-depth=2"
                ,"-o", toT $ mdGenerated </> fromText basename <.> "html"
                ,toT $ md </> basename <.> "md"]
  where toT = toTextIgnore
