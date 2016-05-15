{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import           Data.Text (Text, pack, unpack)
import           Text.Read (readMaybe)
import           Data.Maybe (fromMaybe)
import           Control.Applicative ((<$>), (<*>))
import qualified Web.ClientSession   as CS
import           Yesod
import           CalcFunctions

data Calculator = Calculator   -- data type for our app

-- routes
mkYesod "Calculator" [parseRoutes|
/ HomeR GET                         -- it only checks the current logged user
/add/#Integer/#Integer AddR         -- handles the add operation
/sub/#Integer/#Integer SubR         -- handles the sub operation
/div/#Integer/#Integer DivR         -- handles the div operation
/mul/#Integer/#Integer MulR         -- handles the mul operation
/pidigit/#Integer PiR               -- handles the pi operation
/login/#String/#String LoginR GET   -- handles user login
/logout LogoutR GET                 -- handles user logout
/history HistoryR GET               -- handles history petitions
|]

instance Yesod Calculator where
    makeSessionBackend _ = do
        backend <- defaultClientSessionBackend 1 "keyfile.aes"
        return $ Just backend

{- gets the logged user -}
getHomeR = do
      mSessionValue <- lookupSession "user"   -- get user session variable
      let myValue = show mSessionValue        -- convert to string
      return $ object ["result" .= myValue]   -- build the JSON
  
   
{- Adds the given information to the given user. If user is None, does nothing -}
addToHistoryAndStore "None" _ =
  do
   return ()

addToHistoryAndStore user newinfo = 
 do
  let body  = map (\line->splitLines line ':') $ splitLines (loadFile "history.txt") '\n'  -- load history file
  let history = addToHistory user newinfo body -- add new information to history
  liftIO $ putStrLn $ show $ length history    -- print size of the history
  liftIO $ writeFile "history.txt" $ history2String history -- write the history again
  return ()
   
{- Converts a given user session to something readable (it is a Maybe transformed to a string) -}
convertUserSession "Nothing" = "None"
convertUserSession str = takeWhile (\c->c /= '\"') $ tail $ dropWhile (\c->c /= '\"') str

{- Gets the history for the given user -}
getHistory "None" =
  do
   return $ object ["result" .= "nohistory"]
      
getHistory user =
  do
   let body  = map (\line->splitLines line ':') $ splitLines (loadFile "history.txt") '\n' -- load history
   let history =  foldr (\x y -> x++", " ++ y) "" $ tail $ head $ getUserHistory body user -- get the history of interest
   return $ object ["result" .= history] -- build the JSON
   
{- Handles the history request -}
getHistoryR =
  do
    mSessionValue <- lookupSession "user"                    -- get user session variable
    let username = convertUserSession $ show mSessionValue   -- convert it to a string
    getHistory username                                      -- get and return history for the user
  
{- Handles the logout request -}
getLogoutR = 
  do
   deleteSession "user"                  -- simply delete the session variable
   return $ object ["result" .= "Ok"]

{- Handles the login request -}
getLoginR login pass = 
  do
   checkLoginResult (validateUser login pass) login

{- sets the session variable for the user and creates the JSON response -}
checkLoginResult True login = 
  do
   setSession "user" (pack login)   
   return $ object ["result" .= "Ok"]

checkLoginResult False _ = 
  do
   return $ object ["result" .= "Wrong"]
  
{- Handles the add request -}
handleAddR a b = 
 do
  mSessionValue <- lookupSession "user"                     -- get user session variable
  let username = convertUserSession $ show mSessionValue    -- convert it to a string
  addToHistoryAndStore username $ "add "++show(a)++" "++show(b) -- add to user history
  return $ object ["result" .= adds a b]                     -- build JSON and return it

{- Handles the sub request -}
handleSubR a b = 
 do
  mSessionValue <- lookupSession "user"
  let username = convertUserSession $ show mSessionValue
  addToHistoryAndStore username $ "sub "++show(a)++" "++show(b)
  return $ object ["result" .= subs a b]

{- Handles the div request -}
handleDivR a b = 
 do
  mSessionValue <- lookupSession "user"
  let username = convertUserSession $ show mSessionValue
  addToHistoryAndStore username $ "div "++show(a)++" "++show(b)
  return $ object ["result" .= divs a b]

{- Handles the mul request -}
handleMulR a b = 
 do
  mSessionValue <- lookupSession "user"
  let username = convertUserSession $ show mSessionValue
  addToHistoryAndStore username $ "mul "++show(a)++" "++show(b)
  return $ object ["result" .= muls a b]

{- Handles the pi request -}
handlePiR n =
 do
  mSessionValue <- lookupSession "user"
  let username = convertUserSession $ show mSessionValue
  addToHistoryAndStore username $ "pi "++show(n)
  return $ object ["result" .= pis n]

{- Starts the web server -}
main :: IO ()
main = warp 3000 Calculator
