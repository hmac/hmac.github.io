{-# LANGUAGE OverloadedStrings  #-}
-- vim: ft=haskell

import Hakyll
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc
import System.Process ( readProcess )
import System.IO.Unsafe ( unsafePerformIO )
import Text.Pandoc.Walk ( walk )
import Control.Monad (filterM)

main =
  hakyllWith config $ do
    match "templates/*" $ compile templateCompiler
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler
    match ("lc/**" .||. "eval-game/**") $ do
        route   idRoute
        compile copyFileCompiler
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*" $ do
      route $ setExtension ".html"
      compile $ pandocGraphvizCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" postContext
        >>= relativizeUrls
    match "index.html" $ do
        route idRoute
        compile $ do
          posts <- loadAll "posts/*" >>= filterM published >>= recentFirst
          let indexContext =
                  listField "posts" postContext (pure posts) <>
                  postContext

          getResourceBody
              >>= applyAsTemplate indexContext
              >>= loadAndApplyTemplate "templates/index.html" indexContext
              >>= relativizeUrls
    create ["atom.xml"] $ do
      route idRoute
      compile $ feedPosts >>= renderAtom feedConfig feedContext
    create ["rss.xml"] $ do
      route idRoute
      compile $ feedPosts >>= renderRss feedConfig feedContext

config :: Configuration
config = defaultConfiguration { destinationDirectory = "docs" }

published :: MonadMetadata m => Item String -> m Bool
published item = (== Just "true") . lookupString "publish" <$> getMetadata (itemIdentifier item)

postContext :: Context String
postContext = dateField "date" "%B %e, %Y" <> defaultContext

feedPosts:: Compiler [Item String]
feedPosts = loadAllSnapshots "posts/*" "content"
    >>= filterM published
    >>= recentFirst

feedContext :: Context String
feedContext = bodyField "description" <> postContext

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle = "hmac.dev"
  , feedDescription = "things I've written"
  , feedAuthorName = "Harry Maclean"
  , feedAuthorEmail = "harry@hmac.dev"
  , feedRoot = "https://hmac.dev"
  }

pandocGraphvizCompiler :: Compiler (Item String)
pandocGraphvizCompiler = pandocCompilerWithTransform
    readerOptions
    defaultHakyllWriterOptions
    graphViz

-- Without implicit figures, ![alt](img) is just an <img> with alt text and
-- no visible caption.
readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions
  { readerExtensions = disableExtension Ext_implicit_figures (readerExtensions defaultHakyllReaderOptions) }

graphViz :: Pandoc -> Pandoc
graphViz = walk codeBlock

codeBlock :: Block -> Block
codeBlock cb@(CodeBlock (id, classes, namevals) contents)
  | "tree" `elem` classes  = RawBlock (Format "html") $ renderTree contents
  | "graph" `elem` classes = RawBlock (Format "html") $ renderGraph contents
  | "diagram" `elem` classes = RawBlock (Format "html") $ renderDiagram contents
  | otherwise              = cb
codeBlock x = x

renderTree :: Text -> Text
renderTree contents = T.pack $ unsafePerformIO $
  readProcess "dot" ["-Nfontname=courier"] (T.unpack contents)
    >>= readProcess "gvpr" ["-c", "-ftree.gv"]
    >>= readProcess "neato" ["-n", "-Tsvg"]

renderGraph :: Text -> Text
renderGraph contents = T.pack $ unsafePerformIO $
  readProcess "dot" ["-Nfontname=courier", "-Tsvg"] (T.unpack contents)

renderDiagram :: Text -> Text
renderDiagram contents = T.pack $ unsafePerformIO $
  readProcess "svgbob" ["--stroke-color", "currentColor", "--fill-color", "currentColor", "--background", "transparent", "--stroke-width", "1", "--font-family", "monospace"] (T.unpack contents)
