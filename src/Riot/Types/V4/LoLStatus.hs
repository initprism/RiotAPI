{- https://developer.riotgames.com/apis#lol-status-v4 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Riot.Types.V4.LoLStatus where

import Data.Aeson         
import Data.Function      
import Data.Text          
import Data.Typeable      
import Data.Hashable      
import Data.Maybe         
import Data.Foldable
import GHC.Generics    
import Control.DeepSeq 

-- | Represents a platform data
data PlatFormData = PlatFormData { 
                                   -- | platForm id. 
                                   id_          :: !Text
                                   -- | platForm name.
                                 , name         :: !Text
                                   -- | platForm locales.
                                 , locales      :: ![Text]
                                   -- | maintenance status.
                                 , maintenances :: ![Status]
                                   -- | incident status.
                                 , incidents    :: ![Status]
                                 }
  deriving ( Show, Read, Eq, Generic, Typeable
           , NFData, Hashable
           )

instance FromJSON PlatFormData where
  parseJSON = \case  
    (Object o) -> PlatFormData <$> o .: "id"
                               <*> o .: "name"
                               <*> o .: "locales"
                               <*> o .: "maintenances"
                               <*> o .: "incidents"
    _          -> mempty

instance ToJSON PlatFormData where
  toEncoding (PlatFormData _0 _1 _2 _3 _4) = 
      pairs $ fold $ catMaybes 
      [ pure $ "id"           .= _0
      , pure $ "name"         .= _1
      , pure $ "locales"      .= _2
      , pure $ "maintenances" .= _3
      , pure $ "incidents"    .= _4
      ]

instance Ord PlatFormData where
    compare = compare `on` (id_ :: PlatFormData -> Text)  

instance Semigroup PlatFormData where
  PlatFormData _0 _1 _2 _3 _4 <> PlatFormData _ _ __2 __3 __4 = 
      PlatFormData _0 _1 (_2 <> __2) (_3 <> __3) (_4 <> __4)

data MaintenanceStatus = Scheduled | InProgress | Complete
    deriving ( Show, Read, Eq, Generic, Typeable, Ord, Enum
             , Bounded, NFData, Hashable
             )

instance FromJSON MaintenanceStatus where
  parseJSON = \case 
    "scheduled"   -> pure Scheduled
    "in_progress" -> pure InProgress
    "complete"    -> pure Complete
    _             -> error "Failed to parse MaintenanceStatus"

instance ToJSON MaintenanceStatus where
  toJSON Scheduled  = "scheduled"
  toJSON InProgress = "in_progress"
  toJSON Complete   = "complete"

data IncidentSeverity = Info | Warning | Critical
    deriving ( Show, Read, Eq, Generic, Typeable, Ord, Enum
             , Bounded, NFData, Hashable
             )

instance FromJSON IncidentSeverity where
  parseJSON = \case 
    "info"     -> pure Info
    "warning"  -> pure Warning
    "critical" -> pure Critical
    _          -> error "Failed to parse IncidentSeverity"

instance ToJSON IncidentSeverity where
  toJSON Info     = "info"
  toJSON Warning  = "warning"
  toJSON Critical = "critical"
      
-- | Represents a status
data Status = Status { 
                       -- | Status id.
                       id_               :: !Int 
                       -- | maintenance status.  
                       -- legal values: scheduled | in_progress | complete
                     , maintenanceStatus :: !MaintenanceStatus
                       -- | incident severity.  
                       -- legal values: info | warning | critical
                     , incidentSeverity  :: !IncidentSeverity
                       -- | content info.
                     , titles            :: ![Content]
                       -- | update info.
                     , updates           :: ![Update]
                       -- | created time.
                     , createdAt         :: !Text 
                       -- | archived time.
                     , archiveAt         :: !Text
                       -- | updated time.
                     , updatedAt         :: !Text
                       -- | platform info.
                     , platforms         :: ![Text]
                     }
  deriving ( Show, Read, Eq, Generic, Typeable
           , NFData, Hashable
           )

instance FromJSON Status where
  parseJSON = \case  
    (Object o) -> Status <$> o .: "id"
                         <*> o .: "maintenance_status"
                         <*> o .: "incident_severity"
                         <*> o .: "titles"
                         <*> o .: "updates"
                         <*> o .: "created_at"
                         <*> o .: "archive_at"
                         <*> o .: "updated_at"
                         <*> o .: "platforms"
    _          -> mempty

instance ToJSON Status where
  toEncoding (Status _0 _1 _2 _3 _4 _5 _6 _7 _8) = 
      pairs $ fold $ catMaybes 
      [ pure $ "id"                 .= _0
      , pure $ "maintenance_status" .= _1
      , pure $ "incident_severity"  .= _2
      , pure $ "titles"             .= _3
      , pure $ "updates"            .= _4
      , pure $ "created_at"         .= _5
      , pure $ "archive_at"         .= _6
      , pure $ "updated_at"         .= _7
      , pure $ "platforms"          .= _8
      ]

instance Ord Status where
  compare = compare `on` (id_ :: Status -> Int)

instance Semigroup Status where
  Status _0 _1 _2 _3 _4 _5 _6 _7 _8 <> Status _ _ _ __3 __4 _ _ _ __8 =
    Status _0 _1 _2 (_3 <> __3) (_4 <> __4) _5 _6 _7 (_8 <> __8)

-- | Represents a Content.
data Content = Content { 
                         -- locale info.
                         locale  :: !Text
                         -- content.
                       , content :: !Text
                       }
  deriving ( Show, Read, Eq, Ord, Generic, Typeable
           , NFData, Hashable
           )

instance FromJSON Content where
  parseJSON = \case  
    (Object o) -> Content <$> o .: "locale"
                          <*> o .: "content"
    _          -> mempty

instance ToJSON Content where
  toEncoding (Content _0 _1) = 
      pairs $ fold $ catMaybes 
      [ pure $ "locale"  .= _0
      , pure $ "content" .= _1
      ]

instance Semigroup Content where
  Content _0 _1 <> Content {} =
    Content _0 _1

-- | Represents a update.
data Update = Update { 
                       -- | update id.
                       id_              :: !Int 
                       -- | author.
                     , author           :: !Text
                       -- | publish.
                     , publish          :: !Bool
                       -- | publish locations.
                     , publishLocations :: ![Text]
                       -- | translation infos.
                     , translations     :: ![Content]
                       -- | created time.
                     , createdAt        :: !Text
                       -- | updated time.
                     , updatedAt        :: !Text
                     }
  deriving ( Show, Read, Eq, Generic, Typeable
           , NFData, Hashable
           )

instance FromJSON Update where
  parseJSON = \case  
    (Object o) -> Update <$> o .: "id"
                         <*> o .: "author"
                         <*> o .: "publish"
                         <*> o .: "publish_locations"
                         <*> o .: "translations"
                         <*> o .: "created_at"
                         <*> o .: "updated_at"
    _          -> mempty

instance ToJSON Update where
  toEncoding (Update _0 _1 _2 _3 _4 _5 _6) = 
      pairs $ fold $ catMaybes 
      [ pure $ "id"                .= _0
      , pure $ "author"            .= _1
      , pure $ "publish"           .= _2
      , pure $ "publish_locations" .= _3
      , pure $ "translations"      .= _4
      , pure $ "created_at"        .= _5
      , pure $ "updated_at"        .= _6
      ]

instance Ord Update where
  compare = compare `on` (id_ :: Update -> Int)

instance Semigroup Update where
  Update _0 _1 _2 _3 _4 _5 _6 <> Update _ _ _ __3 __4 _ _ =
    Update _0 _1 _2 (_3 <> __3) (_4 <> __4) _5 _6 