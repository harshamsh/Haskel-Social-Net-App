module Main where

import Control.Concurrent
import System.Random


-- | declaring the data types for user 
data User = User {
                name :: String,
                msg :: String,
                --msgCount :: MVar Int
                messages :: MVar [Message]
            } deriving (Eq)

type Message = String
type MsgCount = Int
type End = Int 


-- | this function is to add elements into the list
addit :: a -> [a] -> [a]
addit a [] = [a]
addit a xs = a : xs


-- |this thread allows us to select random users  
randomUser :: [User] -> IO User
randomUser users = do
    n <- randomIO :: IO Int
    if (n `mod` 10) == 0 then do
        return $ head users
    else if (n `mod` 10) == 1 then do
        return $ users !! 1
    else if (n `mod` 10) == 2 then do
        return $ users !! 2
    else if (n `mod` 10) == 3 then do
        return $ users !! 3
    else if (n `mod` 10) == 4 then do
        return $ users !! 4
    else if (n `mod` 10) == 5 then do
        return $ users !! 5
    else if (n `mod` 10) == 6 then do
        return $ users !! 6
    else if (n `mod` 10) == 7 then do
        return $ users !! 7
    else if (n `mod` 10) == 8 then do
        return $ users !! 8
    else do
        return $ last users


-- | this thread is to select random messages in order to send them to the user
randMes :: [Message] -> IO Message
randMes allRandMesgs = do
    m <- randomIO :: IO Int
    if (m `mod` 10) == 0 then do
        return $ head allRandMesgs
    else if (m `mod` 10) == 1 then do
        return $ allRandMesgs !! 1
    else if (m `mod` 10) == 2 then do
        return $ allRandMesgs !! 2
    else if (m `mod` 10) == 3 then do
        return $ allRandMesgs !! 3
    else if (m `mod` 10) == 4 then do
        return $ allRandMesgs !! 4
    else if (m `mod` 10) == 5 then do
        return $ allRandMesgs !! 5
    else if (m `mod` 10) == 6 then do
        return $ allRandMesgs !! 6
    else if (m `mod` 10) == 7 then do
        return $ allRandMesgs !! 7
    else if (m `mod` 10) == 8 then do
        return $ allRandMesgs !! 8
    else do
        return $ last allRandMesgs

msgAppend :: User -> User-> String ->IO ()
msgAppend user target text = do
    inbox <- takeMVar (messages target)
    let newInbox = addit (text) inbox
    putMVar (messages target) newInbox

    
-- | check if theres message waiting
-- | if the runs inrcrease more than due to concurrency, it will stop after 110
-- | if no message waiting then put one in
-- |checks if there are any blank messages    
-- | try counting with message total and then with msgCount in user
-- | messages sent from the sender to recevier are displayed and appended to the messages list
process :: User -> [User] -> MVar MsgCount -> MVar Message -> [Message]-> MVar End-> IO ()
process user users count message allRandMesgs endit = do
    counRn <- takeMVar count

    if counRn == 110 then 
        putMVar endit counRn
    else if counRn >= 100 then do
        finccc <- readMVar (messages user)
        putStrLn $ name user ++ " received " ++ show (length finccc)
        putMVar count (counRn + 1)
        threadDelay 2000
        process user users count message allRandMesgs endit
    else do
        receiver <- randomUser users
        if user == receiver then do
            putMVar count counRn
            threadDelay 700
            process user users count message allRandMesgs endit
        else do
            text <- randMes allRandMesgs
            if text == "" then do
                putStrLn $ name user ++ " is giving a blank message" 
                msgAppend user receiver text
                putMVar count (counRn + 1)
                threadDelay 700
                process user users count message allRandMesgs endit

            else do
                putStrLn $ name user ++ " sending message to " ++ name receiver 
                putStrLn $"||||| "++ (text) ++ " |||||"
                msgAppend user receiver text
                putMVar count (counRn + 1)
                threadDelay 700
                process user users count message allRandMesgs endit



-- | requried variabes for he process are declared and the process has been forked for each user

main :: IO ()
main = do
    count <- newMVar 0
    message <- newEmptyMVar
    endit <- newEmptyMVar
    
    messages1 <- newMVar []
    messages2 <- newMVar []
    messages3 <- newMVar []
    messages4 <- newMVar []
    messages5 <- newMVar []
    messages6 <- newMVar []
    messages7 <- newMVar []
    messages8 <- newMVar []
    messages9 <- newMVar []
    messages10 <- newMVar []

    let us1 = User "us1" "hello" messages1
    let us2 = User "us2" "hola" messages2
    let us3 = User "us3" "bonjour" messages3
    let us4 = User "us4" "aloha" messages4
    let us5 = User "us5" "ciao" messages5
    let us6 = User "us6" "konnichiwa" messages6
    let us7 = User "us7" "oi" messages7
    let us8 = User "us8" "hey" messages8
    let us9 = User "us9" "guten tag" messages9
    let us10 = User "us10" "namaste" messages10

    let users = [us1, us2, us3, us4, us5, us6, us7, us8, us9, us10]
    let allRandMesgs = ["hey hii","hello how r u doing","doing good?","heyyyyyyy","u alriht?","i dont like ya","i love functional programming","","","","","wyd","just kiddig","iubea","etbuire","etaughuo","","","","","iubea","etbuire","etaughuo"]

    forkIO $ process us1 users count message allRandMesgs endit
    forkIO $ process us2 users count message allRandMesgs endit
    forkIO $ process us3 users count message allRandMesgs endit
    forkIO $ process us4 users count message allRandMesgs endit
    forkIO $ process us5 users count message allRandMesgs endit
    forkIO $ process us6 users count message allRandMesgs endit
    forkIO $ process us7 users count message allRandMesgs endit
    forkIO $ process us8 users count message allRandMesgs endit
    forkIO $ process us9 users count message allRandMesgs endit
    forkIO $ process us10 users count message allRandMesgs endit

    done <- takeMVar endit
    print "messages limit reached"


    

