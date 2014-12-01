{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.Map as M

import Data.Maybe

import qualified Data.Configurator as DC
import Data.Configurator.Types

import Control.Applicative ((<$>))

import Data.Aeson (Result(..))

import Data.List (isPrefixOf)

import Data.Char (toLower)

import Database.MySQL.Simple
import qualified Data.ByteString as B

import Control.Monad (forM_, when)

import Text.Email.Validate (isValid)

import qualified Network.ImageTrove.Main as Main
import qualified Network.MyTardis.API as API
import qualified Network.MyTardis.RestTypes as RestTypes

import Data.ByteString.Internal (w2c)

import Control.Monad.Reader

import qualified Data.HashMap.Lazy as HM
import qualified Data.Hashable as H

import qualified Data.HashSet as HS

is5Digits :: Integer -> Bool
is5Digits n = Prelude.length (show n) == 5

addUsersAndGroups :: String -> ReaderT API.MyTardisConfig IO ()
addUsersAndGroups configFile = do
    xs <- liftIO $ runSqlQuery configFile

    forM_ xs $ \(projectNumber, (address :: B.ByteString)) -> do
        if is5Digits projectNumber && isValid address
            then do liftIO $ putStrLn $ "create group: " ++ show projectNumber
                    project <- API.getOrCreateGroup $ "CAI " ++ show projectNumber
                    liftIO $ print project

                    if B.length address < 29
                        then do liftIO $ putStrLn $ "create user: " ++ show address
                                user <- API.getOrCreateUser Nothing  Nothing (map w2c $ B.unpack address) [] False

                                case (user, project) of
                                    (Success user', Success project') -> do API.addUserToGroup user' project'
                                                                            liftIO $ putStrLn $ "Add user to group..." -- FIXME more descriptive
                                    _                                 -> liftIO $ putStrLn $ "Error: could not add user " ++ show user ++ " to group " ++ show project
                                liftIO $ print user
                        else do liftIO $ putStrLn $ "skipping too-long email address: " ++ show address
            else liftIO $ do putStrLn $ "skip..." ++ show (projectNumber, address)

extractRestGroupInfo :: [RestTypes.RestUser] -> [(String, [String])]
extractRestGroupInfo users = map (\u -> (map toLower $ RestTypes.ruserUsername u, map RestTypes.groupName $ RestTypes.ruserGroups u)) users

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
                                                                  _ <- API.removeUserFromGroup u' g
                                                                  return ()
        _                             -> error "FIXME101"

removeUsers :: String -> ReaderT API.MyTardisConfig IO ()
removeUsers configFile = do
    xs <- liftIO $ runSqlQuery configFile

    let cmrUsers = filter (\(projectNumber, (address :: B.ByteString)) -> is5Digits projectNumber && isValid address) xs

    let cmrUserHashSet = HS.fromList . (map ((map toLower . map w2c . B.unpack) . snd)) $ cmrUsers

    imagetroveUsers <- API.getUsers

    let caiUsersInImageTrove = (filter $ (flip HS.member) cmrUserHashSet . fst) . (filter userHasCAIProject) . extractRestGroupInfo <$> imagetroveUsers

    case caiUsersInImageTrove of
        (Success caiUsersInImageTrove') -> forM_ caiUsersInImageTrove' $ \(u, _) -> when (not $ u `HS.member` cmrUserHashSet) (deleteUsersGroups u)
        _                               -> error "FIXME100"

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
        Error e              -> liftIO $ putStrLn $ "Error while retrieving experiment list: " ++ e

doExperiment e = do
    m <- (map snd) <$> mapM API.handyParameterSet (RestTypes.eiParameterSets e)

    case (map (M.lookup "CAI Project") m) of
        [Just caiProjectID] -> addGroupAccessToExperiment e caiProjectID
        err                 -> liftIO $ putStrLn $ "Warning: none/too many CAI project IDs found: " ++ show err

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

main = do
    let f = "acl.conf"
    mytardisOpts <- Main.getConfig "http://localhost" f Nothing

    case mytardisOpts of
        Nothing            -> error $ "Could not read config file: " ++ f
        Just mytardisOpts' -> (flip runReaderT) mytardisOpts' $ do removeUsers       f
                                                                   addUsersAndGroups f
                                                                   doExperiments
