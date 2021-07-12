{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-@ LIQUID "--compile-spec" @-}

module Model
  ( migrateAll
  , mkUser
  , User
  , userId'
  , userEmailAddress'
  , userPassword'
  , userFirstName'
  , userLastName'
  , UserId
  )
where


import           Database.Persist               ( Key )
import           Database.Persist.TH            ( share
                                                , mkMigrate
                                                , mkPersist
                                                , sqlSettings
                                                , persistLowerCase
                                                )
import qualified Database.Persist              as Persist

import           Storm.Core

import Data.ByteString (ByteString)
import Data.Text       (Text)

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Persistent
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  emailAddress Text
  password ByteString
  firstName Text
  lastName Text
  UniqueEmailAddress emailAddress
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ measure getJust :: Key record -> Entity record @-}

-- * User
{-@ mkUser ::
        x_0: Text
     -> x_1: ByteString
     -> x_2: Text
     -> x_3: Text
     -> StormRecord <{\row -> userEmailAddress (entityVal row) == x_0 && userPassword (entityVal row) == x_1 && userFirstName (entityVal row) == x_2 && userLastName (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) User
  @-}
mkUser :: Text -> ByteString -> Text -> Text -> StormRecord (Entity User) User
mkUser x_0 x_1 x_2 x_3 = StormRecord (User x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity User | v == getJust (entityKey v)} @-}



{-@ assume userId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) User UserId
  @-}
userId' :: EntityFieldWrapper (Entity User) User UserId
userId' = EntityFieldWrapper UserId

{-@ measure userEmailAddress :: User -> Text @-}

{-@ measure userEmailAddressCap :: Entity User -> Bool @-}

{-@ assume userEmailAddress' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userEmailAddress (entityVal row)},
                          {\field row -> field == userEmailAddress (entityVal row)},
                          {\old -> userEmailAddressCap old},
                          {\old _ _ -> userEmailAddressCap old}>
                          (Entity User) User Text
  @-}
userEmailAddress' :: EntityFieldWrapper (Entity User) User Text
userEmailAddress' = EntityFieldWrapper UserEmailAddress

{-@ measure userPassword :: User -> ByteString @-}

{-@ measure userPasswordCap :: Entity User -> Bool @-}

{-@ assume userPassword' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userPassword (entityVal row)},
                          {\field row -> field == userPassword (entityVal row)},
                          {\old -> userPasswordCap old},
                          {\old _ _ -> userPasswordCap old}>
                          (Entity User) User ByteString
  @-}
userPassword' :: EntityFieldWrapper (Entity User) User ByteString
userPassword' = EntityFieldWrapper UserPassword

{-@ measure userFirstName :: User -> Text @-}

{-@ measure userFirstNameCap :: Entity User -> Bool @-}

{-@ assume userFirstName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userFirstName (entityVal row)},
                          {\field row -> field == userFirstName (entityVal row)},
                          {\old -> userFirstNameCap old},
                          {\old _ _ -> userFirstNameCap old}>
                          (Entity User) User Text
  @-}
userFirstName' :: EntityFieldWrapper (Entity User) User Text
userFirstName' = EntityFieldWrapper UserFirstName

{-@ measure userLastName :: User -> Text @-}

{-@ measure userLastNameCap :: Entity User -> Bool @-}

{-@ assume userLastName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userLastName (entityVal row)},
                          {\field row -> field == userLastName (entityVal row)},
                          {\old -> userLastNameCap old},
                          {\old _ _ -> userLastNameCap old}>
                          (Entity User) User Text
  @-}
userLastName' :: EntityFieldWrapper (Entity User) User Text
userLastName' = EntityFieldWrapper UserLastName
