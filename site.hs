--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


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

    match "vendor/img/*.png" $ do
        route idRoute
        compile copyFileCompiler

    match "papers/*.pdf" $ do
        route idRoute
        compile copyFileCompiler

    match "talks/*.pdf" $ do
        route idRoute
        compile copyFileCompiler

    match "talks/ressi_2020.mp4" $ do
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
        compile $ pandocCompiler
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
                    --listField "posts" postCtx (return posts) `mappend`
                    langContextHome lang                       `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate
                      (fromFilePath $ "templates/default."++lang++".html")
                      indexCtx
                >>= relativizeUrls

    -- Create `index.html` as a copy of `index.en.html`
    match "index.en.html" $ version "index" $ do
        route $ constRoute "index.html"
        compile $ do
            let lang = "en"
            let indexCtx =
                    langContextHome "en"                       `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate
                      (fromFilePath $ "templates/default.en.html")
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

    match "teaching/fdv2020_s2/*.pdf" $ do
        route idRoute
        compile copyFileCompiler

    match "teaching/fdv2020_s2/index.markdown" $ do
        route $ setExtension "html"
        compile $ do
            let ctx = defaultContext
            pandocCompiler
              >>= loadAndApplyTemplate
                (fromFilePath $ "templates/default.fr.html") ctx
              >>= relativizeUrls


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
