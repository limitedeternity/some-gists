{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List

-- User privileges for our users
data UserPrivilege = Member | Admin | Guest

-- Our type witness
data WitnessPrivilege up where
  WitnessMember :: WitnessPrivilege Member
  WitnessGuest :: WitnessPrivilege Guest
  WitnessAdmin :: WitnessPrivilege Admin

-- Our user type
data User (up :: UserPrivilege) = User
  { userId :: Integer
  , userName :: String
  , userPrivilege :: WitnessPrivilege up
  }

-- The type that we use to hide the privilege type variable
data SomeUser where
  SomeUser :: User a -> SomeUser

-- A function that accept a user id (Integer), and reads
-- the corresponding user from the database. Note that the return
-- type level privilege is hidden in the return value `SomeUser`.
readUser :: Integer -> IO SomeUser
readUser userId = pure $ case find ((== userId) . (\(a, _, _) -> a)) dbRows of
  Just (id_, name_, type_) ->
    case type_ of
      "member" -> SomeUser (User id_ name_ WitnessMember)
      "guest" -> SomeUser (User id_ name_ WitnessGuest)
      "admin" -> SomeUser (User id_ name_ WitnessAdmin)
  Nothing -> error "User not found"

-- This is a function that does not care 
-- about user privileges
getUserName :: User up -> String
getUserName = userName

-- Admin-only function
deleteStuffAsAdmin :: User 'Admin -> IO ()
deleteStuffAsAdmin _ = pure ()

main :: IO ()
main = do
  (SomeUser user) <- readUser 12

  putStrLn $ getUserName user 
  case userPrivilege user of 
    -- So we bring the type-level user privilege in scope by matching
    -- on `userPrivilege` field and then GHC knows that `user`
    -- is actually `User 'Admin`, and so we can call `deleteStuffAsAdmin`
    -- with `user`.
    WitnessAdmin -> deleteStuffAsAdmin user
    _ -> error "Need admin user"

dbRows :: [(Integer, String, String)]
dbRows =
  [ (10, "John", "member")
  , (11, "alice", "guest")
  , (12, "bob", "admin")
  ]

