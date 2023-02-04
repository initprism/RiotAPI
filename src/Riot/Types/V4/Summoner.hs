{- https://developer.riotgames.com/apis#summoner-v4 -}

{-# LANGUAGE OverloadedStrings #-}

module Riot.Types.V4.Summoner where

import Data.Aeson         ( (.:), FromJSON(parseJSON), Value(Object) )
import Data.Function      (on)
import Data.Text          (Text)

-- | Encrypted account ID. Max length 56 characters. 
newtype AccountId = AccountId Text
  deriving (Show, Read, Eq)

instance FromJSON AccountId where
  parseJSON x = AccountId <$> parseJSON x

-- | ID of the summoner icon associated with the summoner. 
newtype ProfileIconId = ProfileIconId Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON ProfileIconId where
  parseJSON x = ProfileIconId <$> parseJSON x

-- | Date summoner was last modified specified as epoch milliseconds.   
-- The following events will update this timestamp: profile icon change,  
-- playing the tutorial or advanced tutorial, finishing a game, summoner name change 
newtype RevisionDate = RevisionDate Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON RevisionDate where
  parseJSON x = RevisionDate <$> parseJSON x

-- | Summoner name.
newtype Name = Name Text
  deriving (Show, Read, Eq)

instance FromJSON Name where
  parseJSON x = Name <$> parseJSON x

-- | Encrypted summoner ID. Max length 63 characters. 
newtype Id = Id Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON Id where
  parseJSON x = Id <$> parseJSON x

-- | Encrypted PUUID. Exact length of 78 characters. 
newtype Puuid = Puuid Text
  deriving (Show, Read, Eq)

instance FromJSON Puuid where
  parseJSON x = Puuid <$> parseJSON x

-- | Summoner level associated with the summoner. 
newtype SummonerLevel = SummonerLevel Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON SummonerLevel where
  parseJSON x = SummonerLevel <$> parseJSON x

-- | Represents a summoner. 
data Summoner = Summoner { summonerAccountId     :: AccountId
                         , summonerProfileIconId :: ProfileIconId
                         , summonerRevisionDate  :: RevisionDate
                         , summonerName          :: Name
                         , summonerId            :: Id
                         , summonerPuuId         :: Puuid
                         , summonerLevel         :: SummonerLevel
                         }
  deriving (Show, Read, Eq)

instance FromJSON Summoner where
  parseJSON (Object o) = Summoner <$> o .: "accountId"
                                  <*> o .: "profileIconId"
                                  <*> o .: "revisionDate"
                                  <*> o .: "name"
                                  <*> o .: "id"
                                  <*> o .: "puuid"
                                  <*> o .: "summonerLevel"
  parseJSON _          = mempty

instance Ord Summoner where
  compare = compare `on` summonerId