{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.Map as M
import qualified Data.MultiMap as MM

import Data.Maybe

import Data.Traversable (traverse)

import qualified Data.Configurator as DC
import Data.Configurator.Types

import Control.Applicative ((<$>))

import Data.Aeson (Result(..))

import Data.List (isPrefixOf)

import Data.Char (toLower)

import Database.MySQL.Simple
import qualified Data.ByteString as B

import Control.Monad (forever, forM_, when, unless)

import Text.Email.Validate (isValid)

import qualified Network.ImageTrove.Main as Main
import qualified Network.MyTardis.API as API
import qualified Network.MyTardis.RestTypes as RestTypes

import Data.ByteString.Internal (w2c)

import Control.Monad.Reader

import qualified Data.HashMap.Lazy as HM
import qualified Data.Hashable as H

import qualified Data.HashSet as HS
import Control.Concurrent (threadDelay)

import Text.Printf (printf)

is5Digits :: Integer -> Bool
is5Digits n = Prelude.length (show n) == 5

addUsersAndGroups :: String -> ReaderT API.MyTardisConfig IO ()
addUsersAndGroups configFile = do
    xs <- liftIO $ runSqlQuery configFile

    forM_ xs $ \(projectNumber, address :: B.ByteString) ->
        if is5Digits projectNumber && isValid address
            then do liftIO $ putStrLn $ "Creating group: " ++ show projectNumber
                    project <- API.getOrCreateGroup $ "CAI " ++ show projectNumber

                    liftIO $ putStrLn $ "Creating user: " ++ show address
                    user <- API.getOrCreateUser Nothing  Nothing (map w2c $ B.unpack address) [] False

                    case (user, project) of
                        (Success user', Success project') -> do liftIO $ putStrLn $ "Adding " ++ show address ++ " to group " ++ show projectNumber
                                                                user'' <- API.addUserToGroup user' project'
                                                                case user'' of
                                                                    Success _  -> return ()
                                                                    Error  err -> liftIO $ putStrLn $ "Error: could not add user " ++ show user ++ " to group " ++ show project ++ ": " ++ err
                                                                return ()
                        (Error err, _)                    -> liftIO $ putStrLn $ "Error: could not get user: " ++ show err
                        (_, Error err)                    -> liftIO $ putStrLn $ "Error: could not get group: " ++ show err
                    liftIO $ print user
            else liftIO $ putStrLn $ "Error: invalid project ID or email address: " ++ show (projectNumber, address)

extractRestGroupInfo :: [RestTypes.RestUser] -> [(String, [String])]
extractRestGroupInfo = map (\u -> (map toLower $ RestTypes.ruserUsername u, map RestTypes.groupName $ RestTypes.ruserGroups u))

uniq :: (Eq t, H.Hashable t) => [t] -> [t]
uniq xs = HM.keys $ HM.fromList $ map (\x -> (x, ())) xs

userHasCAIProject :: (String, [String]) -> Bool
userHasCAIProject (_, []) = False
userHasCAIProject (_, groups) = any (isPrefixOf "CAI") groups

deleteUsersGroups :: String -> ReaderT API.MyTardisConfig IO ()
deleteUsersGroups userEmail = do
    liftIO $ putStrLn $ "Removing all access for user: " ++ userEmail

    u <- API.getOrCreateUser Nothing Nothing userEmail [] False

    let groups = RestTypes.ruserGroups <$> u

    case (u, groups) of
        (Success u', Success groups') -> forM_ groups' $ \g -> do liftIO $ putStrLn $ "Removing group access: " ++ show (RestTypes.groupName g)
                                                                  u'' <- API.removeUserFromGroup u' g

                                                                  case u'' of
                                                                    (Success _) -> return ()
                                                                    (Error err) -> liftIO $ do putStrLn $ "Error: could not remove user from group: " ++ err
                                                                                               return ()
        (Error err, _)                -> liftIO $ putStrLn $ "Error: could not get user: " ++ show err
        (_, Error err)                -> liftIO $ putStrLn $ "Error: could not get group: " ++ show err

removeUsers :: String -> ReaderT API.MyTardisConfig IO ()
removeUsers configFile = do
    xs <- liftIO $ runSqlQuery configFile

    let cmrUsers = filter (\(projectNumber, address :: B.ByteString) -> is5Digits projectNumber && isValid address) xs

    let cmrUserHashSet = HS.fromList . map ((map (toLower . w2c) . B.unpack) . snd) $ cmrUsers

    imagetroveUsers <- API.getUsers

    let caiUsersInImageTrove = filter (flip HS.member cmrUserHashSet . fst) . filter userHasCAIProject . extractRestGroupInfo <$> imagetroveUsers

    case caiUsersInImageTrove of
        (Success caiUsersInImageTrove') -> forM_ caiUsersInImageTrove' $ \(u, _) -> unless (u `HS.member` cmrUserHashSet) (deleteUsersGroups u)
        (Error err)                     -> liftIO $ putStrLn $ "Error: could not get list of users in ImageTrove: " ++ show err

addGroupAccessToExperiment :: RestTypes.RestExperiment -> String -> ReaderT API.MyTardisConfig IO ()
addGroupAccessToExperiment e caiProjectID = do
    caiProjectID' <- API.getOrCreateGroup caiProjectID

    case caiProjectID' of
        Success caiProjectID'' -> do g <- API.addGroupReadOnlyAccess e caiProjectID''
                                     case g of Success _ -> liftIO $ putStrLn $ "Added group access for " ++ show caiProjectID
                                               Error err -> liftIO $ putStrLn $ "Error when adding group access: " ++ err
        Error err              -> liftIO $ putStrLn $ "Error when adding retrieving group: " ++ show err

doExperiments = do
    experiments <- API.getExperiments

    case experiments of
        Success experiments' -> forM_ experiments' doExperiment
        Error err            -> liftIO $ putStrLn $ "Error while retrieving experiment list: " ++ err

doExperiment e = do
    m <- map snd <$> mapM API.handyParameterSet (RestTypes.eiParameterSets e)

    case map (MM.lookup "CAI Project") m of
        [[caiProjectID]]    -> addGroupAccessToExperiment e caiProjectID
        err                 -> liftIO $ putStrLn $ "Error: none/too many CAI project IDs found: " ++ show err

setInstrumentOperators e = do
    m <- map snd <$> mapM API.handyParameterSet (RestTypes.eiParameterSets e)

    let operators  = concat $ map (MM.lookup "Operator")   m
        instrument = concat $ map (MM.lookup "Instrument") m

    liftIO $ putStrLn $ "setInstrumentOperators, experiment title: " ++ RestTypes.eiTitle e
    liftIO $ putStrLn $ "setInstrumentOperators, experiment id: "    ++ show (RestTypes.eiID e)

    forM_ instrument $ \instr -> do

        forM operators $ \op -> do
            let groupName = "OPERATOR :: " ++ instr

            liftIO $ putStrLn $ "Creating operator group: " ++ groupName
            group <- API.getOrCreateGroup groupName

            liftIO $ putStrLn $ "Creating user: " ++ op
            user <- API.getOrCreateUser Nothing  Nothing op [] False

            case (user, group) of
                (Success user', Success group') -> do user'' <- API.addUserToGroup user' group'
                                                      case user'' of
                                                        Success _   -> liftIO $ putStrLn $ "Added " ++ op ++ " to group " ++ groupName
                                                        Error   err -> liftIO $ putStrLn $ "Error while adding user to group: " ++ err
                (Error err,     Success _)      -> liftIO $ putStrLn $ "Error while creating user: "  ++ err
                (Success _,     Error err)      -> liftIO $ putStrLn $ "Error while creating group: " ++ err
                (Error userErr, Error groupErr) -> liftIO $ do putStrLn $ "Error while creating user: "  ++ userErr
                                                               putStrLn $ "Error while creating group: " ++ groupErr


            return ()

    return ()

runSqlQuery :: String -> IO [(Integer, B.ByteString)]
runSqlQuery configFile = do
    cfg <- DC.load [Required configFile]
        
    mysql_host  <- DC.lookup cfg "mysql_host" :: IO (Maybe String)
    mysql_user  <- DC.lookup cfg "mysql_user" :: IO (Maybe String)
    mysql_pass  <- DC.lookup cfg "mysql_pass" :: IO (Maybe String)
    mysql_db    <- DC.lookup cfg "mysql_db"   :: IO (Maybe String)

    when (isNothing mysql_host) $ error $ "mysql_host missing in configuration file " ++ configFile
    when (isNothing mysql_user) $ error $ "mysql_user missing in configuration file " ++ configFile
    when (isNothing mysql_pass) $ error $ "mysql_pass missing in configuration file " ++ configFile
    when (isNothing mysql_db)   $ error $ "mysql_db   missing in configuration file " ++ configFile

    case (mysql_host, mysql_user, mysql_pass, mysql_db) of
        (Just mysql_host', Just mysql_user', Just mysql_pass', Just mysql_db') -> do
            let ci = defaultConnectInfo { connectHost       = mysql_host'
                                        , connectUser       = mysql_user'
                                        , connectPassword   = mysql_pass'
                                        , connectDatabase   = mysql_db'
                                        }
            conn <- connect ci
            query_ conn "select CI_list.project_number, email.address from CI_list, email WHERE CI_list.who = email.owner"
        _ -> error "Missing configuration for MySQL database?"

mainAction = do
    let f = "acl.conf"
    mytardisOpts <- Main.getConfig "http://localhost" "http://localhost:8042" f Nothing

    case mytardisOpts of
        Nothing            -> error $ "Could not read config file: " ++ f
        Just mytardisOpts' -> flip runReaderT mytardisOpts' $ do removeUsers       f
                                                                 addUsersAndGroups f
                                                                 doExperiments

                                                                 experiments <- API.getExperiments
                                                                 traverse (mapM setInstrumentOperators) experiments

main = forever $ do
    mainAction
    let sleepMinutes = 1
    liftIO $ printf "Sleeping for %d minutes...\n" sleepMinutes
    liftIO $ threadDelay $ sleepMinutes * (60 * 10^6)
