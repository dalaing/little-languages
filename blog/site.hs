--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mempty)
import           Hakyll
import           Image.LaTeX.Render
import           Image.LaTeX.Render.Pandoc
import           Hakyll.Contrib.LaTeX

--------------------------------------------------------------------------------

formulaOptionsWithPkgs :: [String] -> FormulaOptions -> FormulaOptions
formulaOptionsWithPkgs pkgs fo =
  let
    p = preamble fo
    f :: String -> String
    f x = "\\usepackage{" ++ x ++ "}"
    pkgString = concatMap f pkgs
  in
    fo { preamble = p ++ pkgString}

pandocFormulaOptionsWithPkgs :: [String] -> PandocFormulaOptions
pandocFormulaOptionsWithPkgs pkgs =
  let 
    opts = defaultPandocFormulaOptions
    fo = formulaOptionsWithPkgs pkgs . formulaOptions opts 
  in
    opts { formulaOptions = fo }

main :: IO ()
main = do
  renderFormulae <- initFormulaCompilerDataURI 1000 defaultEnv
  let pandocMathCompiler =
        pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions .
        renderFormulae $
        pandocFormulaOptionsWithPkgs ["prftree"]
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/**" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= saveSnapshot "post-content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "drafts/**" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
    let rss name render' =
          create [name] $ do
              route idRoute
              compile $ do
                  let feedCtx = postCtx `mappend` bodyField "description"
                  posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "post-content"
                  render' feedConfiguration feedCtx posts

    rss "rss.xml" renderRss
    rss "atom.xml" renderAtom

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- chronological =<< loadAllSnapshots "posts/*" "post-content"
            let
                indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            pandocMathCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Little Languages"
    , feedDescription = ""
    , feedAuthorName  = "Dave Laing"
    , feedAuthorEmail = "dave.laing.80@gmail.com"
    , feedRoot        = "http://dlaing.org/little-languages"
    }
