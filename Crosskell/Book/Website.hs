{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/            HomeR       GET
/set-message SetMessageR POST
/crossword-peek PeekCrosswordR GET
/crossword-solve SolveCrosswordR GET
|]

instance Yesod App where
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        mmsg <- getMessage
        mmsg2 <- getMessage
        withUrlRenderer
            [hamlet|
                $doctype 5
                <html>
                    <head>
                        <title>#{pageTitle pc}
                        ^{pageHead pc}
                    <body>
                        $maybe msg <- mmsg
                            <p>My first word is: #{msg}
                        ^{pageBody pc}
            |]


            -- <li class="form_li">
            -- <input name="word[]" class="word" id="word-1"  type="text">
            -- <input name="clue[]" class="clue" id="clue-1"   type="text">
            -- </div></li><li class="form_li">
            -- <input name="word[]" class="word" id="word-2"  type="text">
            -- <input name="clue[]" class="clue" id="clue-2"   type="text">
            -- </div></li><li class="form_li">
            -- <input name="word[]" class="word" id="word-3"  type="text">
            -- <input name="clue[]" class="clue" id="clue-3"   type="text">
            -- </div></li><li class="form_li">
            -- <input name="word[]" class="word" id="word-4"  type="text">
            -- <input name="clue[]" class="clue" id="clue-4"   type="text">
            
            -- </div></li><li class="form_li">
            -- <input name="word[]" class="word" id="word-5"  type="text">
            -- <input name="clue[]" class="clue" id="clue-5"   type="text">
            
            -- </div></li><li class="form_li">
            -- <input name="word[]" class="word" id="word-6"  type="text">
            -- <input name="clue[]" class="clue" id="clue-6"   type="text">
            
            -- </div></li><li class="form_li">
            -- <input name="word[]" class="word" id="word-7"  type="text">
            -- <input name="clue[]" class="clue" id="clue-7"   type="text">
            -- </div></li><li class="form_li">
            -- <input name="word[]" class="word" id="word-8"  type="text">
            -- <input name="clue[]" class="clue" id="clue-8"   type="text">
            -- </div></li><li class="form_li">
            -- <input name="word[]" class="word" id="word-9"  type="text">
            -- <input name="clue[]" class="clue" id="clue-9"   type="text">
            -- </div></li><li class="form_li">
            -- <input name="word[]" class="word" id="word-10"  type="text">
            -- <input name="clue[]" class="clue" id="clue-10"   type="text">
            -- </div></li>                

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <form method=post action=@{SetMessageR}>
            Crosskell
            <ul>
                    <li>
                        Word              Clue
                $forall num <- enumFromTo 1 8
                    <li>
                        <input type=text name=word>
                        <input type=text name=clue>
            <button>Go

    |]

postSetMessageR :: Handler ()
postSetMessageR = do
    --msg <- runInputPost $ concat [ireq textField "word" | i <-[1,2..10]]
    msg <- runInputPost $ ireq textField "word"
    --msg3 <- "JUREK"
    setMessage $ (toHtml msg)
    redirect PeekCrosswordR
    
getPeekCrosswordR :: Handler Html
getPeekCrosswordR = defaultLayout
    [whamlet|
    |]

getSolveCrosswordR :: Handler Html
getSolveCrosswordR = defaultLayout
    [whamlet|
    |]
main :: IO ()
main = warp 3104 App