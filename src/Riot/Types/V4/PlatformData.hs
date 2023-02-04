{- https://developer.riotgames.com/apis#lol-status-v4 -}

{-# LANGUAGE OverloadedStrings #-}

module Riot.Types.V4.PlatformData where

import Data.Aeson         ( (.:), FromJSON(parseJSON), Value(Object) )
import Data.Function      (on)
import Data.Text          (Text)
import Data.Vector        (Vector)

-- | PlatForm id. 
newtype PlatFormId = PlatFormId Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON PlatFormId where
  parseJSON x = PlatFormId <$> parseJSON x

-- | PlatForm name.
newtype Name = Name Text
  deriving (Show, Read, Eq)

instance FromJSON Name where
  parseJSON x = Name <$> parseJSON x

-- | PlatForm locales.
newtype Locales = Locales (Vector Text)
  deriving (Show, Read, Eq)

instance FromJSON Locales where
  parseJSON x = Locales <$> parseJSON x

-- | Maintenance infos.
newtype Maintenances = Maintenances (Vector Status)
  deriving (Show, Read, Eq)

instance FromJSON Maintenances where
  parseJSON x = Maintenances <$> parseJSON x

-- | Incident infos.
newtype Incidents = Incidents (Vector Status)
  deriving (Show, Read, Eq)

instance FromJSON Incidents where
  parseJSON x = Incidents <$> parseJSON x

-- | Represents a PlatformData
data PlatFormData = PlatFormData { platFormId           :: PlatFormId
                                 , platFormName         :: Name
                                 , platFormLocales      :: Locales
                                 , platFormMaintenances :: Maintenances
                                 , platFormIncidents    :: Incidents
                                 }
  deriving (Show, Read, Eq)

instance FromJSON PlatFormData where
  parseJSON (Object o) = PlatFormData <$> o .: "id"
                                      <*> o .: "name"
                                      <*> o .: "locales"
                                      <*> o .: "maintenances"
                                      <*> o .: "incidents"
  parseJSON _          = mempty

instance Ord PlatFormData where
  compare = compare `on` platFormId

-- | Status id.
newtype StatusId = StatusId Integer 
  deriving (Show, Read, Eq, Ord)

instance FromJSON StatusId where
  parseJSON x = StatusId <$> parseJSON x

-- | Maintenance status.  
-- Legal values: scheduled, in_progress, complete
newtype MaintenanceStatus = MaintenanceStatus Text 
  deriving (Show, Read, Eq)

instance FromJSON MaintenanceStatus where
  parseJSON x = MaintenanceStatus <$> parseJSON x

-- | Incident severity.  
-- Legal values: info, warning, critical
newtype IncidentSeverity = IncidentSeverity Text 
  deriving (Show, Read, Eq)

instance FromJSON IncidentSeverity where
  parseJSON x = IncidentSeverity <$> parseJSON x

-- | Title infos.
newtype Titles = Titles (Vector Content) 
  deriving (Show, Read, Eq)

instance FromJSON Titles where
  parseJSON x = Titles <$> parseJSON x

-- | update infos.
newtype Updates = Updates (Vector Update) 
  deriving (Show, Read, Eq)

instance FromJSON Updates where
  parseJSON x = Updates <$> parseJSON x

-- | Created time info.
newtype CreatedAt = CreatedAt Text
  deriving (Show, Read, Eq)

instance FromJSON CreatedAt where
  parseJSON x = CreatedAt <$> parseJSON x

-- | Archive timing info.
newtype ArchiveAt = ArchiveAt Text
  deriving (Show, Read, Eq)

instance FromJSON ArchiveAt where
  parseJSON x = ArchiveAt <$> parseJSON x

-- | Update timing info.
newtype UpdatedAt = UpdatedAt Text
  deriving (Show, Read, Eq)

instance FromJSON UpdatedAt where
  parseJSON x = UpdatedAt <$> parseJSON x

-- | platform info.  
-- Legal values: windows, macos, android, ios, ps4, xbone, switch
newtype Platforms = Platforms (Vector Text)
  deriving (Show, Read, Eq)

instance FromJSON Platforms where
  parseJSON x = Platforms <$> parseJSON x

-- | Represents a Status.
data Status = Status { statusId                :: StatusId 
                     , statusMaintenanceStatus :: MaintenanceStatus
                     , statusIncidentSeverity  :: IncidentSeverity
                     , statusTitles            :: Titles
                     , statusUpdates           :: Updates
                     , statusCreatedAt         :: CreatedAt 
                     , statusArchiveAt         :: ArchiveAt
                     , statusUpdatedAt         :: UpdatedAt
                     , statusPlatforms         :: Platforms
                     }
  deriving (Show, Read, Eq)

instance FromJSON Status where
  parseJSON (Object o) = Status <$> o .: "id"
                                <*> o .: "maintenance_status"
                                <*> o .: "incident_severity"
                                <*> o .: "titles"
                                <*> o .: "updates"
                                <*> o .: "created_at"
                                <*> o .: "archive_at"
                                <*> o .: "updated_at"
                                <*> o .: "platforms"
  parseJSON _          = mempty

instance Ord Status where
  compare = compare `on` statusId

-- | Content locale.
newtype Locale = Locale Text  
  deriving (Show, Read, Eq)

instance FromJSON Locale where
  parseJSON x = Locale <$> parseJSON x

-- | Represents a Content.
data Content = Content { contentLocale :: Locale
                       , content       :: Text
                       }
  deriving (Show, Read, Eq)

instance FromJSON Content where
  parseJSON (Object o) = Content <$> o .: "locale"
                                 <*> o .: "content"
  parseJSON _          = mempty

-- | Update id.
newtype UpdateId = UpdateId Integer 
  deriving (Show, Read, Eq, Ord)

instance FromJSON UpdateId where
  parseJSON x = UpdateId <$> parseJSON x

-- | Update author.
newtype Author = Author Text
  deriving (Show, Read, Eq)

instance FromJSON Author where
  parseJSON x = Author <$> parseJSON x

-- | Update publish.
newtype Publish = Publish Bool
  deriving (Show, Read, Eq)

instance FromJSON Publish where
  parseJSON x = Publish <$> parseJSON x

-- | Update publish locations.  
-- Legal values: riotclient, riotstatus, game
newtype PublishLocations = PublishLocations (Vector Text)
  deriving (Show, Read, Eq)

instance FromJSON PublishLocations where
  parseJSON x = PublishLocations <$> parseJSON x

-- | Update translations.  
newtype Translations = Translations (Vector Content)
  deriving (Show, Read, Eq)

instance FromJSON Translations where
  parseJSON x = Translations <$> parseJSON x

-- | Represents a Update.
data Update = Update { updateId               :: UpdateId 
                     , updateAuthor           :: Author
                     , updatePublish          :: Publish
                     , updatePublishLocations :: PublishLocations
                     , updateTranslations     :: Translations
                     , updateCreatedAt        :: CreatedAt
                     , updateUpdatedAt        :: UpdatedAt
                     }
  deriving (Show, Read, Eq)

instance FromJSON Update where
  parseJSON (Object o) = Update <$> o .: "id"
                                <*> o .: "author"
                                <*> o .: "publish"
                                <*> o .: "publish_locations"
                                <*> o .: "translations"
                                <*> o .: "created_at"
                                <*> o .: "updated_at"
  parseJSON _          = mempty

instance Ord Update where
  compare = compare `on` updateId