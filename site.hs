--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "vendor/fonts/OpenSans/OpenSans-Regular.ttf" $ do
        route idRoute
        compile copyFileCompiler

    match "vendor/img/*.svg" $ do
        route idRoute
        compile copyFileCompiler

    match "papers/*.pdf" $ do
        route idRoute
        compile copyFileCompiler

    match "files/*" $ do
        route idRoute
        compile copyFileCompiler

    match "contact.**.markdown" $ do
        route   $ setExtension "html"
        compile $ do
            lang <- getLang . toFilePath <$> getUnderlying
            let ctx = defaultContext
            pandocCompiler
              >>= loadAndApplyTemplate
                (fromFilePath $ "templates/default."++lang++".html") ctx
              >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith def def{
                writerHighlight = True
              }
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.en.html" postCtx
            >>= relativizeUrls

    create (map (\l -> fromFilePath $ "archive."++l++".html") langs) $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            lang <- getLang . toFilePath <$> getUnderlying
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    langContextArchive lang                  `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate (fromFilePath $ "templates/archive."++lang++".html") archiveCtx
                >>= loadAndApplyTemplate (fromFilePath $ "templates/default."++lang++".html") archiveCtx
                >>= relativizeUrls


    match "index.**.html" $ do
        route idRoute
        compile $ do
            lang <- getLang . toFilePath <$> getUnderlying
            let indexCtx =
                    langContextHome lang                         `mappend`
                    defaultContext
            getResourceBody >>= loadAndApplyTemplate
                (fromFilePath $ "templates/default."++lang++".html")
                indexCtx
              >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "teaching/fdv2020/*.pdf" $ do
        route idRoute
        compile copyFileCompiler

    match "teaching/fdv2020/index.markdown" $ do
        route $ setExtension "html"
        compile $ do
            let ctx = defaultContext
            pandocCompiler
              >>= loadAndApplyTemplate
                (fromFilePath $ "templates/default.fr.html") ctx
              >>= relativizeUrls

    -- RSS and Atom feeds

    {-
    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "content"
        renderRss feedConfig feedCtx posts

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "content"
        renderAtom feedConfig feedCtx posts
    -}


--------------------------------------------------------------------------------

langs :: [String]
langs = ["fr","en","eo"]

getLang :: FilePath -> String
getLang path =
  case dropWhile (/= '.') path of
  [] -> "en"
  _ : tl -> takeWhile (/= '.') tl

langContextHome :: String -> Context a
langContextHome "fr" = constField "title" "Accueil"
langContextHome "eo" = constField "title" "Hejmo"
langContextHome _ = constField "title" "Home"

langContextArchive :: String -> Context a
langContextArchive "fr" = constField "title" "Billets"
langContextArchive "eo" = constField "title" "Publikaĵoj"
langContextArchive _ = constField "title" "Posts"

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Otini's weblog"
    , feedDescription = "Blog posts from Olivier Nicole"
    , feedAuthorName  = "Olivier Nicole"
    , feedAuthorEmail = "test@example.com"
    , feedRoot        = "https://otini.chnik.fr"
    }
