{- https://developer.riotgames.com/apis#summoner-v4 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Riot.Types.V4.Summoner where

import Data.Aeson         
import Data.Function      
import Data.Text          
import Data.Typeable      
import Data.Hashable      
import Data.Maybe         
import Data.Foldable
import GHC.Generics    
import Control.DeepSeq 

-- | Represents a summoner. 
data Summoner = Summoner { 
                           -- | encrypted account ID. max length 56 characters. 
                           accountId     :: !Text
                           -- | iD of the summoner icon associated with the summoner. 
                         , profileIconId :: !Int
                           -- | date summoner was last modified specified as epoch milliseconds.  
                           -- the following events will update this timestamp: profile icon change,  
                           -- playing the tutorial or advanced tutorial, finishing a game, summoner name change 
                         , revisionDate  :: !Int
                           -- | summoner name. 
                         , name          :: !Text
                           -- | encrypted summoner ID. max length 63 characters. 
                         , id_           :: !Text
                           -- |	encrypted PUUID. exact length of 78 characters. 
                         , puuId         :: !Text
                           -- | summoner level associated with the summoner. 
                         , summonerLevel :: !Int
                         }
  deriving ( Show, Read, Eq, Generic, Typeable
           , NFData, Hashable
           )

instance FromJSON Summoner where
  parseJSON = \case  
    (Object o) -> Summoner <$> o .: "accountId"
                           <*> o .: "profileIconId"
                           <*> o .: "revisionDate"
                           <*> o .: "name"
                           <*> o .: "id"
                           <*> o .: "puuid"
                           <*> o .: "summonerLevel"
    _          -> mempty

instance ToJSON Summoner where
  toEncoding (Summoner _0 _1 _2 _3 _4 _5 _6) = 
      pairs $ fold $ catMaybes 
      [ pure $ "accountId"     .= _0
      , pure $ "profileIconId" .= _1
      , pure $ "revisionDate"  .= _2
      , pure $ "name"          .= _3
      , pure $ "id"            .= _4
      , pure $ "puuid"         .= _5
      , pure $ "summonerLevel" .= _6
      ]

instance Ord Summoner where
  compare = compare `on` id_

instance Semigroup Summoner where
  Summoner _0 _1 _2 _3 _4 _5 _6 <> Summoner {} =
    Summoner _0 _1 _2 _3 _4 _5 _6