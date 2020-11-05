
module Main where

import DB
import Database.Beam
import Database.Beam.Sqlite

import Data.Text (Text)

import Database.SQLite.Simple

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

data UserT f =
  User
  { _userEmail :: Columnar f Text
  , _userName :: Columnar f Text
  , _userPassword :: Columnar f Text
  } deriving Generic

instance Beamable UserT

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
  -- hvordan henger PrimaryKey her sammen med `type UserId = ...`?
  -- er dette en instansiering av et slag?
  primaryKey = UserId . _userEmail







data ShoppingCartDb f = ShoppingCartDb
  { _shoppingCartUsers :: f (TableEntity UserT) }
  deriving (Generic, Database be)

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings


main :: IO ()
main = do
  conn <- open "shoppingcart1.db"

  let allUsers = all_ (_shoppingCartUsers shoppingCartDb)
  runBeamSqliteDebug putStrLn conn $ do
    users <- runSelectReturningList $ select allUsers
    mapM_ (liftIO . putStrLn . show) users

  let sortUsersByName = orderBy_ (\u -> asc_ (_userName u)) (all_ (_shoppingCartUsers shoppingCartDb))
  runBeamSqliteDebug putStrLn conn $ do
    users <- runSelectReturningList $ select sortUsersByName
    mapM_ (liftIO . putStrLn . show) users

insertStuff :: IO ()
insertStuff = do
  conn <- open "shoppingcart1.db"
  runBeamSqliteDebug putStrLn conn $ runInsert $
    insert (_shoppingCartUsers shoppingCartDb) $
    insertValues [ User "james@example.com" "James Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
             , User "betty@example.com" "Betty Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
             , User "sam@example.com" "Sam Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
