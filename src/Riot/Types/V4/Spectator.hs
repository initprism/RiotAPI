{- https://developer.riotgames.com/apis#spectator-v4 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Riot.Types.V4.Spectator where

import Data.Aeson         
import Data.Function      
import Data.Text          
import Data.Typeable      
import Data.Hashable      
import Data.Maybe         
import Data.Foldable
import GHC.Generics    
import Control.DeepSeq 

-- | Represents a current game info. 
data CurrentGameInfo = CurrentGameInfo {
                                         -- | the ID of the game.
                                         gameId            :: !Int
                                         -- | the game type.
                                       , gameType          :: !Text 
                                         -- | the game start time represented in epoch milliseconds.
                                       , gameStartTime     :: !Int 
                                         -- | the id of the map.
                                       , mapId             :: !Int 
                                         -- | the amount of time in seconds that has passed since the game started.
                                       , gameLength        :: !Int
                                         -- | the ID of the platform on which the game is being played.
                                       , platformId        :: !Text 
                                         -- | the game mode.
                                       , gameMode          :: !Text 
                                         -- | banned champion information.
                                       , bannedChampions   :: ![BannedChampion] 
                                         -- | the queue type.
                                       , gameQueueConfigId :: !Int 
                                         -- | the obsever information.
                                       , observers         :: !Observer
                                         -- | the participant informantion.
                                       , participants      :: ![CurrentGameParticipant] 
                                       }
  deriving ( Show, Read, Eq, Generic, Typeable
           , NFData, Hashable
           )

instance FromJSON CurrentGameInfo where
  parseJSON = \case  
    (Object o) -> CurrentGameInfo <$> o .: "gameId"
                                  <*> o .: "gameType"
                                  <*> o .: "gameStartTime"
                                  <*> o .: "mapId"
                                  <*> o .: "gameLength"
                                  <*> o .: "platformId"
                                  <*> o .: "gameMode"
                                  <*> o .: "bannedChampions"
                                  <*> o .: "gameQueueConfigId"
                                  <*> o .: "observers"
                                  <*> o .: "participants"
    _          -> mempty

instance ToJSON CurrentGameInfo where
  toEncoding (CurrentGameInfo _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10) = 
      pairs $ fold $ catMaybes 
      [ pure $ "gameId"            .= _0
      , pure $ "gameType"          .= _1
      , pure $ "gameStartTime"     .= _2
      , pure $ "mapId"             .= _3
      , pure $ "gameLength"        .= _4
      , pure $ "platformId"        .= _5
      , pure $ "gameMode"          .= _6
      , pure $ "bannedChampions"   .= _7
      , pure $ "gameQueueConfigId" .= _8
      , pure $ "observers"         .= _9
      , pure $ "participants"      .= _10
      ]

instance Ord CurrentGameInfo where
  compare = compare `on` (gameId :: CurrentGameInfo -> Int)

instance Semigroup CurrentGameInfo where
  CurrentGameInfo _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 <> CurrentGameInfo _ _ _ _ _ _ _ __7 _ _ __10  =
    CurrentGameInfo _0 _1 _2 _3 _4 _5 _6 (_7 <> __7) _8 _9 (_10 <> __10)

-- | Represents a banned champion.
data BannedChampion = BannedChampion {
                                       -- | the turn during which the champion was banned
                                       pickTurn   :: !Int
                                       -- | the ID of the banned champion
                                     , championId :: !Int
                                       -- | the ID of the team that banned the champion
                                     , teamId     :: !Int
                                     }
  deriving ( Show, Read, Eq, Generic, Typeable
           , NFData, Hashable
           )

instance FromJSON BannedChampion where
  parseJSON = \case  
    (Object o) -> BannedChampion <$> o .: "pickTurn"
                                 <*> o .: "championId"
                                 <*> o .: "teamId"
    _          -> mempty

instance ToJSON BannedChampion where
  toEncoding (BannedChampion _0 _1 _2) = 
      pairs $ fold $ catMaybes 
      [ pure $ "pickTurn" .= _0
      , pure $ "championId" .= _1
      , pure $ "teamId" .= _2
      ]

instance Ord BannedChampion where
  compare = compare `on` pickTurn

instance Semigroup BannedChampion where
  BannedChampion _0 _1 _2 <> BannedChampion {} =
    BannedChampion _0 _1 _2

-- | Represents a observer.
newtype Observer = Observer { 
                              -- | key used to decrypt the spectator grid game data for playback
                              encryptionKey :: Text
                            } 
  deriving ( Show, Read, Eq, Generic, Ord, Typeable
           , NFData, Hashable
           )

instance FromJSON Observer where
  parseJSON = \case  
    (Object o) -> Observer <$> o .: "encryptionKey"
    _          -> mempty

instance ToJSON Observer where
  toEncoding (Observer _0) = 
      pairs $ fold $ catMaybes 
      [ pure $ "encryptionKey" .= _0
      ]

instance Semigroup Observer where
  Observer _0 <> Observer _ =
    Observer _0

-- | Represents a current gmae participant.
data CurrentGameParticipant = CurrentGameParticipant { 
                                                       -- |	the ID of the champion played by this participant 
                                                       championId               :: !Int
                                                       -- | perks/runes reforged information 
                                                     , perks                    :: !Perks
                                                       -- | the ID of the profile icon used by this participant 
                                                     , profileIconId            :: !Int
                                                       -- | flag indicating whether or not this participant is a bot 
                                                     , bot                      :: !Bool
                                                       -- | the team ID of this participant, indicating the participant's team 
                                                     , teamId                   :: !Int
                                                       -- | the summoner name of this participant 
                                                     , summonerName             :: !Text
                                                       -- | the encrypted summoner ID of this participant 
                                                     , summonerId               :: !Text
                                                       -- | the ID of the first summoner spell used by this participant 
                                                     , spell1Id                 :: !Int
                                                       -- | the ID of the second summoner spell used by this participant 
                                                     , spell2Id                 :: !Int
                                                       -- |	list of game customizations 
                                                     , gameCustomizationObjects :: ![GameCustomizationObject]
                                                     }
  deriving ( Show, Read, Eq, Generic, Typeable
           , NFData, Hashable
           )

instance FromJSON CurrentGameParticipant where
  parseJSON = \case  
    (Object o) -> CurrentGameParticipant <$> o .: "championId"
                                         <*> o .: "perks"
                                         <*> o .: "profileIconId"
                                         <*> o .: "bot"
                                         <*> o .: "teamId"
                                         <*> o .: "summonerName"
                                         <*> o .: "summonerId"
                                         <*> o .: "spell1Id"
                                         <*> o .: "spell2Id"
                                         <*> o .: "gameCustomizationObjects"
    _          -> mempty

instance ToJSON CurrentGameParticipant where
  toEncoding (CurrentGameParticipant _0 _1 _2 _3 _4 _5 _6 _7 _8 _9) = 
      pairs $ fold $ catMaybes 
      [ pure $ "championId"               .= _0
      , pure $ "perks"                    .= _1
      , pure $ "profileIconId"            .= _2
      , pure $ "bot"                      .= _3
      , pure $ "teamId"                   .= _4
      , pure $ "summonerName"             .= _5
      , pure $ "summonerId"               .= _6
      , pure $ "spell1Id"                 .= _7
      , pure $ "spell2Id"                 .= _8
      , pure $ "gameCustomizationObjects" .= _9
      ]

instance Ord CurrentGameParticipant where
  compare = compare `on` (teamId :: CurrentGameParticipant -> Int)

instance Semigroup CurrentGameParticipant where
  CurrentGameParticipant _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 <> CurrentGameParticipant _ _ _ _ _ _ _ _ _ __9 =
    CurrentGameParticipant _0 _1 _2 _3 _4 _5 _6 _7 _8 (_9 <> __9) 

-- | Represents a perks.
data Perks = Perks {
                     -- | Ids of the perks/runes assigned.
                     perkIds      :: ![Int]
                     -- | primary runes path
                   , perkStyle    :: !Int
                     -- | secondary runes path
                   , perkSubStyle :: !Int
                   }
  deriving ( Show, Read, Eq, Generic, Ord, Typeable
           , NFData, Hashable
           )

instance FromJSON Perks where
  parseJSON = \case  
    (Object o) -> Perks <$> o .: "perkIds"
                        <*> o .: "perkStyle"
                        <*> o .: "perkSubStyle"
    _          -> mempty

instance ToJSON Perks where
  toEncoding (Perks _0 _1 _2) = 
      pairs $ fold $ catMaybes 
      [ pure $ "perkIds" .= _0
      , pure $ "perkStyle"  .= _1
      , pure $ "perkSubStyle"  .= _2
      ]

instance Semigroup Perks where
  Perks _0 _1 _2 <> Perks __0 _ _  =
    Perks (_0 <> __0) _1 _2

-- | Represents a game customization object.
data GameCustomizationObject = GameCustomizationObject {
                                                         -- | category identifier for game customization 
                                                         category :: !Text
                                                         -- | game customization content
                                                       , content  :: !Text                                                          
                                                       }
  deriving ( Show, Read, Eq, Generic, Ord, Typeable
           , NFData, Hashable
           )

instance FromJSON GameCustomizationObject where
  parseJSON = \case  
    (Object o) -> GameCustomizationObject <$> o .: "category"
                                          <*> o .: "content"
    _          -> mempty

instance ToJSON GameCustomizationObject where
  toEncoding (GameCustomizationObject _0 _1) = 
      pairs $ fold $ catMaybes 
      [ pure $ "category" .= _0
      , pure $ "content"  .= _1
      ]

instance Semigroup GameCustomizationObject where
  GameCustomizationObject _0 _1 <> GameCustomizationObject _ _  =
    GameCustomizationObject _0 _1 

data GameMode = CLASSIC | ODIN | ARAM | TUTORIAL | ONEFORALL | ASCENSION | FIRSTBLOOD | KINGPORO
  deriving ( Show, Read, Eq, Generic, Typeable, Ord, Enum
           , Bounded, NFData, Hashable
           )

instance FromJSON GameMode where
  parseJSON = \case 
    "CLASSIC"    -> pure CLASSIC
    "ODIN"       -> pure ODIN
    "ARAM"       -> pure ARAM
    "TUTORIAL"   -> pure TUTORIAL
    "ONEFORALL"  -> pure ONEFORALL
    "ASCENSION"  -> pure ASCENSION
    "FIRSTBLOOD" -> pure FIRSTBLOOD
    "KINGPORO"   -> pure KINGPORO
    _            -> undefined

instance ToJSON GameMode where
  toJSON CLASSIC    = "CLASSIC"
  toJSON ODIN       = "ODIN"
  toJSON ARAM       = "ARAM"
  toJSON TUTORIAL   = "TUTORIAL"
  toJSON ONEFORALL  = "ONEFORALL"
  toJSON ASCENSION  = "ASCENSION"
  toJSON FIRSTBLOOD = "FIRSTBLOOD"
  toJSON KINGPORO   = "KINGPORO"

data GameType = CUSTOM_GAME | MATCHED_GAME | TUTORIAL_GAME
  deriving ( Show, Read, Eq, Generic, Typeable, Ord, Enum
           , Bounded, NFData, Hashable
           )

instance FromJSON GameType where
  parseJSON = \case 
    "CUSTOM_GAME"   -> pure CUSTOM_GAME
    "MATCHED_GAME"  -> pure MATCHED_GAME
    "TUTORIAL_GAME" -> pure TUTORIAL_GAME
    _               -> undefined

instance ToJSON GameType where
  toJSON CUSTOM_GAME   = "CUSTOM_GAME"
  toJSON MATCHED_GAME  = "MATCHED_GAME"
  toJSON TUTORIAL_GAME = "TUTORIAL_GAME"

-- | Represents a feature games
data FeatureGames = FeatureGames { 
                                   -- | the list of featured games
                                   gameList              :: ![FeaturedGameInfo]
                                   -- | the suggested interval to wait before requesting featured games again
                                 , clientRefreshInterval :: !Int
                                 }
  deriving ( Show, Read, Eq, Generic, Typeable
           , NFData, Hashable
           )

instance FromJSON FeatureGames where
  parseJSON = \case  
    (Object o) -> FeatureGames <$> o .: "gameList"
                               <*> o .: "clientRefreshInterval"
    _          -> mempty

instance ToJSON FeatureGames where
  toEncoding (FeatureGames _0 _1) = 
      pairs $ fold $ catMaybes 
      [ pure $ "gameList"              .= _0
      , pure $ "clientRefreshInterval" .= _1
      ]

instance Ord FeatureGames where
  compare = compare `on` gameList 

instance Semigroup FeatureGames where
  FeatureGames _0 _1 <> FeatureGames __0 _ =
    FeatureGames (_0 <> __0) _1

-- | Represents a feature game info
data FeaturedGameInfo = FeaturedGameInfo {
                                           -- | the game mode  
                                           -- legal values: CLASSIC | ODIN | ARAM | TUTORIAL | ONEFORALL | ASCENSION | FIRSTBLOOD | KINGPORO
                                           gameMode          :: !GameMode
                                           -- | the amount of time in seconds that has passed since the game started
                                         , gameLength        ::  !Int  
                                           -- | the ID of the map
                                         , mapId             :: !Int
                                           -- | the game type
                                           -- legal values: CUSTOM_GAME | MATCHED_GAME | TUTORIAL_GAME
                                         , gameType          :: !GameType
                                           -- | Banned champion information 
                                         , bannedChampions   :: ![BannedChampion]
                                           -- | the ID of the game
                                         , gameId            :: !Int
                                            -- | the ID of the game
                                         , observers         :: !Observer
                                           -- | the queue type
                                         , gameQueueConfigId :: !Int
                                           -- | the game start time represented in epoch milliseconds 
                                         , gameStartTime     :: !Int
                                           -- | the participant information 
                                         , participants      :: ![Participant]
                                           -- | the participant information 
                                         , platformId        :: !Text
                                         }
  deriving ( Show, Read, Eq, Generic, Typeable
           , NFData, Hashable
           )

instance FromJSON FeaturedGameInfo where
  parseJSON = \case  
    (Object o) -> FeaturedGameInfo <$> o .: "gameMode"
                                   <*> o .: "gameLength"
                                   <*> o .: "mapId"
                                   <*> o .: "gameType"
                                   <*> o .: "bannedChampions"
                                   <*> o .: "gameId"
                                   <*> o .: "observers"
                                   <*> o .: "gameQueueConfigId"
                                   <*> o .: "gameStartTime"
                                   <*> o .: "participants"
                                   <*> o .: "platformId"
    _          -> mempty

instance ToJSON FeaturedGameInfo where
  toEncoding (FeaturedGameInfo _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10) = 
      pairs $ fold $ catMaybes 
      [ pure $ "gameMode"          .= _0
      , pure $ "gameLength"        .= _1
      , pure $ "mapId"             .= _2
      , pure $ "gameType"          .= _3
      , pure $ "bannedChampions"   .= _4
      , pure $ "gameId"            .= _5
      , pure $ "observers"         .= _6
      , pure $ "gameQueueConfigId" .= _7
      , pure $ "gameStartTime"     .= _8
      , pure $ "participants"      .= _9
      , pure $ "platformId"        .= _10
      ]

instance Ord FeaturedGameInfo where
  compare = compare `on` (gameId :: FeaturedGameInfo -> Int)

instance Semigroup FeaturedGameInfo where
  FeaturedGameInfo _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 <> FeaturedGameInfo _ _ _ _ __4 _ _ _ _ __9 _ =
    FeaturedGameInfo _0 _1 _2 _3 (_4 <> __4) _5 _6 _7 _8 (_9 <> __9) _10

-- | Represents a gmae participant.
data Participant = Participant { 
                                 -- | flag indicating whether or not this participant is a bot 
                                 bot           :: !Bool
                                 -- | the ID of the second summoner spell used by this participant 
                               , spell2Id      :: !Int
                                 -- | the ID of the profile icon used by this participant 
                               , profileIconId :: !Int
                                 -- | the summoner name of this participant 
                               , summonerName  :: !Text
                                 -- |	the ID of the champion played by this participant 
                               , championId    :: !Int
                                 -- | the team ID of this participant, indicating the participant's team 
                               , teamId        :: !Int
                                 -- | the ID of the first summoner spell used by this participant 
                               , spell1Id      :: !Int
                               }
  deriving ( Show, Read, Eq, Generic, Typeable
           , NFData, Hashable
           )

instance FromJSON Participant where
  parseJSON = \case  
    (Object o) -> Participant <$> o .: "bot"
                              <*> o .: "spell2Id"
                              <*> o .: "profileIconId"
                              <*> o .: "summonerName"
                              <*> o .: "championId"
                              <*> o .: "teamId"
                              <*> o .: "spell1Id"
    _          -> mempty

instance ToJSON Participant where
  toEncoding (Participant _0 _1 _2 _3 _4 _5 _6) = 
      pairs $ fold $ catMaybes 
      [ pure $ "bot"           .= _0
      , pure $ "spell2Id"      .= _1
      , pure $ "profileIconId" .= _2
      , pure $ "summonerName"  .= _3
      , pure $ "championId"    .= _4
      , pure $ "teamId"        .= _5
      , pure $ "spell1Id"      .= _6
      ]

instance Ord Participant where
  compare = compare `on` (teamId :: Participant -> Int)

instance Semigroup Participant where
  Participant _0 _1 _2 _3 _4 _5 _6 <> Participant {} = 
      Participant _0 _1 _2 _3 _4 _5 _6
