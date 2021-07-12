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
  , mkItem
  , User
  , Item
  , userId'
  , userEmailAddress'
  , userPassword'
  , userFirstName'
  , userLastName'
  , itemId'
  , itemOwner'
  , itemDescription'
  , itemLevel'
  , UserId
  , ItemId
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

Item
  owner UserId
  description Text
  level String
  
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate IsOwner ITEM VIEWER = itemOwner (entityVal ITEM) == entityKey VIEWER @-}

{-@ predicate IsPublic ITEM = itemLevel (entityVal ITEM) == "public" @-}

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

-- * Item
{-@ mkItem ::
        x_0: UserId
     -> x_1: Text
     -> x_2: String
     -> StormRecord <{\row -> itemOwner (entityVal row) == x_0 && itemDescription (entityVal row) == x_1 && itemLevel (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (IsOwner x_0 x_1 || IsPublic x_0)}>
                     (Entity User) Item
  @-}
mkItem :: UserId -> Text -> String -> StormRecord (Entity User) Item
mkItem x_0 x_1 x_2 = StormRecord (Item x_0 x_1 x_2)

{-@ invariant {v: Entity Item | v == getJust (entityKey v)} @-}



{-@ assume itemId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Item ItemId
  @-}
itemId' :: EntityFieldWrapper (Entity User) Item ItemId
itemId' = EntityFieldWrapper ItemId

{-@ measure itemOwner :: Item -> UserId @-}

{-@ measure itemOwnerCap :: Entity Item -> Bool @-}

{-@ assume itemOwner' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == itemOwner (entityVal row)},
                          {\field row -> field == itemOwner (entityVal row)},
                          {\old -> itemOwnerCap old},
                          {\old _ _ -> itemOwnerCap old}>
                          (Entity User) Item UserId
  @-}
itemOwner' :: EntityFieldWrapper (Entity User) Item UserId
itemOwner' = EntityFieldWrapper ItemOwner

{-@ measure itemDescription :: Item -> Text @-}

{-@ measure itemDescriptionCap :: Entity Item -> Bool @-}

{-@ assume itemDescription' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsOwner x_0 x_1 || IsPublic x_0)},
                          {\row field -> field == itemDescription (entityVal row)},
                          {\field row -> field == itemDescription (entityVal row)},
                          {\old -> itemDescriptionCap old},
                          {\old _ _ -> itemDescriptionCap old}>
                          (Entity User) Item Text
  @-}
itemDescription' :: EntityFieldWrapper (Entity User) Item Text
itemDescription' = EntityFieldWrapper ItemDescription

{-@ measure itemLevel :: Item -> String @-}

{-@ measure itemLevelCap :: Entity Item -> Bool @-}

{-@ assume itemLevel' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == itemLevel (entityVal row)},
                          {\field row -> field == itemLevel (entityVal row)},
                          {\old -> itemLevelCap old},
                          {\old _ _ -> itemLevelCap old}>
                          (Entity User) Item String
  @-}
itemLevel' :: EntityFieldWrapper (Entity User) Item String
itemLevel' = EntityFieldWrapper ItemLevel
