{-# LANGUAGE OverloadedStrings #-}

-- TODO: check against https://dev.twitter.com/docs/platform-objects

module Web.Twitter.Types
       ( DateString
       , UserId
       , Friends
       , URIString
       , UserName
       , StatusId
       , LanguageCode
       , StreamingAPI(..)
       , Status(..)
       , SearchResult(..)
       , SearchStatus(..)
       , RetweetedStatus(..)
       , DirectMessage(..)
       , FilterLevel(..)
       , EventTarget(..)
       , Event(..)
       , Delete(..)
       , User(..)
       , List(..)
       , Entities(..)
       , Entity(..)
       , EntityIndices
       , HashTagEntity(..)
       , UserEntity(..)
       , URLEntity(..)
       , SymbolEntity(..)
       , MediaEntity(..)
       , MediaSize(..)
       , MediaSizes(..)
       -- , MediaTag(..)
         
         -- * Coordinate Handling
         --
         -- For now there are separate types for points and polygons, since
         -- they seem to be used for the coordinates/geo and places:bounding_box
         -- fields of a status. It might make sense to have a single Shape type,
         -- but for now keep things separate.
       , Point(..)
       , Polygon(..)
       , Place(..)
       , PlaceId
       , PlaceType(..)
       , checkError
       )
       where

import qualified Data.Map as M

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text, unpack)
import Data.ByteString (ByteString)
import Control.Applicative
import Control.Monad

type DateString   = Text
type UserId       = Integer
type Friends      = [UserId]
type URIString    = ByteString
type UserName     = Text
type StatusId     = Integer

-- | Looks like Twitter are using BCP47 - <http://tools.ietf.org/html/bcp47> - but
--   a present too lazy to wrap this up into a type.
type LanguageCode = Text

data StreamingAPI = SStatus Status
                  | SRetweetedStatus RetweetedStatus
                  | SEvent Event
                  | SDelete Delete
                  -- -- | SScrubGeo ScrubGeo
                  | SFriends Friends
                  | SUnknown Value
                  deriving (Show, Eq)

checkError :: Object -> Parser ()
checkError o = do
  err <- o .:? "error"
  case err of
    Just msg -> fail msg
    Nothing -> return ()

instance FromJSON StreamingAPI where
  parseJSON v@(Object o) =
    SRetweetedStatus <$> js <|>
    SStatus <$> js <|>
    SEvent <$> js <|>
    SDelete <$> js <|>
    SFriends <$> (o .: "friends") <|>
    return (SUnknown v)
    where
      js :: FromJSON a => Parser a
      js = parseJSON v
  parseJSON _ = mzero

data Status =
  Status
  { statusCreatedAt     :: DateString
  , statusId            :: StatusId
  , statusText          :: Text
  , statusSource        :: Text
  , statusLanguage      :: LanguageCode
  , statusFilterLevel   :: Maybe FilterLevel
  , statusTruncated     :: Bool
  , statusEntities      :: Maybe Entities
  , statusInReplyTo     :: Maybe StatusId
  , statusInReplyToUser :: Maybe UserId
  , statusInReplyToScreenName :: Maybe Text -- Easier for DJBs code than the UserId field
  , statusFavorite      :: Maybe Bool
  , statusRetweetCount  :: Maybe Integer
  , statusSensitiveLink :: Maybe Bool
  , statusUser          :: User
  , statusPosition      :: Maybe Point
  , statusPlace         :: Maybe Place
  } deriving (Show, Eq)

instance FromJSON Status where
  parseJSON (Object o) = checkError o >>
    Status <$> o .:  "created_at"
           <*> o .:  "id"
           <*> o .:  "text"
           <*> o .:  "source"
           <*> o .:  "lang"
           <*> o .:? "filter_level"
           <*> o .:  "truncated"
           <*> o .:? "entities"
           <*> o .:? "in_reply_to_status_id"
           <*> o .:? "in_reply_to_user_id"
           <*> o .:? "in_reply_to_screen_name"
           <*> o .:? "favorited"
           <*> o .:? "retweet_count"
           <*> o .:? "possibly_sensitive"
           <*> o .:  "user"
           <*> o .:? "coordinates"
           <*> o .:? "place"
  parseJSON _ = mzero

data SearchResult body =
  SearchResult
  { searchResultCompletedIn :: Float
  , searchResultMaxId :: StatusId
  , searchResultNextPage :: Maybe URIString
  , searchResultQuery :: URIString
  , searchResultRefreshUrl :: URIString
  , searchResultResults :: body
  , searchResultResultsPerPage :: Int
  , searchResultSinceId :: Integer
  } deriving (Show, Eq)

instance FromJSON body =>
         FromJSON (SearchResult body) where
  parseJSON (Object o) = checkError o >>
    SearchResult <$> o .:  "completed_in"
                 <*> o .:  "max_id"
                 <*> o .:? "next_page"
                 <*> o .:  "query"
                 <*> o .:  "refresh_url"
                 <*> o .:  "results"
                 <*> o .:  "results_per_page"
                 <*> o .:  "since_id"
  parseJSON _ = mzero

data SearchStatus =
  SearchStatus
  { searchStatusCreatedAt     :: DateString
  , searchStatusId            :: StatusId
  , searchStatusText          :: Text
  , searchStatusSource        :: Text
  , searchStatusUserId        :: UserId
  , searchStatusUserName      :: UserName
  } deriving (Show, Eq)

instance FromJSON SearchStatus where
  parseJSON (Object o) = checkError o >>
    SearchStatus <$> o .:  "created_at"
                 <*> o .:  "id"
                 <*> o .:  "text"
                 <*> o .:  "source"
                 <*> o .:  "from_user_id"
                 <*> o .:  "from_user"
  parseJSON _ = mzero

data RetweetedStatus =
  RetweetedStatus
  { rsCreatedAt       :: DateString
  , rsId              :: StatusId
  , rsText            :: Text
  , rsSource          :: Text
  , rsLanguage        :: LanguageCode
  , rsFilterLevel     :: Maybe FilterLevel
  , rsTruncated       :: Bool
  , rsEntities        :: Maybe Entities
  , rsUser            :: User
  , rsPosition        :: Maybe Point
  , rsPlace           :: Maybe Place
  , rsRetweetedStatus :: Status
  } deriving (Show, Eq)

instance FromJSON RetweetedStatus where
  parseJSON (Object o) = checkError o >>
    RetweetedStatus <$> o .:  "created_at"
                    <*> o .:  "id"
                    <*> o .:  "text"
                    <*> o .:  "source"
                    <*> o .:  "lang"
                    <*> o .:? "filter_level"
                    <*> o .:  "truncated"
                    <*> o .:? "entities"
                    <*> o .:  "user"
                    <*> o .:? "coordinates"
                    <*> o .:? "place"
                    <*> o .:  "retweeted_status"
  parseJSON _ = mzero

data DirectMessage =
  DirectMessage
  { dmCreatedAt          :: DateString
  , dmSenderScreenName   :: Text
  , dmSender             :: User
  , dmText               :: Text
  , dmRecipientScreeName :: Text
  , dmId                 :: StatusId
  , dmRecipient          :: User
  , dmRecipientId        :: UserId
  , dmSenderId           :: UserId
  } deriving (Show, Eq)

instance FromJSON DirectMessage where
  parseJSON (Object o) = checkError o >>
    DirectMessage <$> o .:  "created_at"
                  <*> o .:  "sender_screen_name"
                  <*> o .:  "sender"
                  <*> o .:  "text"
                  <*> o .:  "recipient_screen_name"
                  <*> o .:  "id"
                  <*> o .:  "recipient"
                  <*> o .:  "recipient_id"
                  <*> o .:  "sender_id"
  parseJSON _ = mzero

-- | From <https://dev.twitter.com/blog/introducing-new-metadata-for-tweets>.
data FilterLevel = FLNone | FLLow | FLMedium | FLHigh
                                       deriving (Show, Eq)

instance FromJSON FilterLevel where
  parseJSON (String t) = case t of
    "none" -> return FLNone
    "low" -> return FLLow
    "medium" -> return FLMedium
    "high" -> return FLHigh
    _ -> fail ("Expected none/low/medium/high, not '" ++ unpack t ++ "'") -- TODO: was mzero
  parseJSON _ = mzero

data EventType = Favorite | Unfavorite
               | ListCreated | ListUpdated | ListMemberAdded
               | UserUpdate | Block | Unblock | Follow
               deriving (Show, Eq)

data EventTarget = ETUser User | ETStatus Status | ETList List | ETUnknown Value
                 deriving (Show, Eq)

instance FromJSON EventTarget where
  parseJSON v@(Object o) = checkError o >>
    ETUser <$> parseJSON v <|>
    ETStatus <$> parseJSON v <|>
    ETList <$> parseJSON v <|>
    return (ETUnknown v)
  parseJSON _ = mzero

data Event =
  Event
  { evCreatedAt       :: DateString
  , evTargetObject    :: Maybe EventTarget
  , evEvent           :: Text
  , evTarget          :: EventTarget
  , evSource          :: EventTarget
  } deriving (Show, Eq)

instance FromJSON Event where
  parseJSON (Object o) = checkError o >>
    Event <$> o .:  "created_at"
          <*> o .:? "target_object"
          <*> o .:  "event"
          <*> o .:  "target"
          <*> o .:  "source"
  parseJSON _ = mzero

data Delete =
  Delete
  { delId  :: StatusId
  , delUserId :: UserId
  } deriving (Show, Eq)

instance FromJSON Delete where
  parseJSON (Object o) = checkError o >> do
    s <- o .: "delete" >>= (.: "status")
    Delete <$> s .: "id"
           <*> s .: "user_id"
  parseJSON _ = mzero

-- | The @User@ structure is also used in the `UserEntity` entry,
--   so most fields have to be optional (probably a bad design
--   choice).
data User =
  User
  { userId              :: UserId
  , userName            :: UserName
  , userScreenName      :: Text
  , userDescription     :: Maybe Text
  , userLocation        :: Maybe Text
  , userProfileImageURL :: Maybe URIString
  , userURL             :: Maybe URIString
  , userProtected       :: Maybe Bool
  , userFollowers       :: Maybe Int
  , userFriends         :: Maybe Int
  , userTweets          :: Maybe Int
  , userLangCode        :: Maybe LanguageCode
  , userCreatedAt       :: Maybe DateString
  , userVerified        :: Maybe Bool
  , userHasContributors :: Maybe Bool
  , userGeoEnabled      :: Maybe Bool
  , userUTCOffset       :: Maybe Int
  , userTimeZone        :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Object o) = checkError o >>
    User <$> o .:  "id"
         <*> o .:  "name"
         <*> o .:  "screen_name"
         <*> o .:? "description"
         <*> o .:? "location"
         <*> o .:? "profile_image_url"  -- TODO: should also support profile_image_url_https
         <*> o .:? "url"
         <*> o .:? "protected"
         <*> o .:? "followers_count"
         <*> o .:? "friends_count"
         <*> o .:? "statuses_count"
         <*> o .:? "lang"
         <*> o .:? "created_at"
         <*> o .:? "verified"
         <*> o .:? "contributors_enabled"
         <*> o .:? "geo_enabled"
         <*> o .:? "utc_offset"
         <*> o .:? "time_zone"
  parseJSON _ = mzero

data List =
  List
  { listId :: Int
  , listName :: Text
  , listFullName :: Text
  , listMemberCount :: Int
  , listSubscriberCount :: Int
  , listMode :: Text
  , listUser :: User
  } deriving (Show, Eq)

instance FromJSON List where
  parseJSON (Object o) = checkError o >>
    List <$> o .:  "id"
         <*> o .:  "name"
         <*> o .:  "full_name"
         <*> o .:  "member_count"
         <*> o .:  "subscriber_count"
         <*> o .:  "mode"
         <*> o .:  "user"
  parseJSON _ = mzero

data HashTagEntity =
  HashTagEntity
  { hashTagText :: Text -- ^ The Hashtag text
  } deriving (Show, Eq)

instance FromJSON HashTagEntity where
  parseJSON (Object o) =
    HashTagEntity <$> o .: "text"
  parseJSON _ = mzero

data SymbolEntity =
  SymbolEntity
  { symbolText :: Text -- ^ The symbol text
  } deriving (Show, Eq)

instance FromJSON SymbolEntity where
  parseJSON (Object o) =
    SymbolEntity <$> o .: "text"
  parseJSON _ = mzero

-- | The 'UserEntity' is just a wrapper around 'User' which is
--   a bit wasteful, and should probably be replaced by just
--   storing the id, name and screen name here.
data UserEntity = UserEntity User
                deriving (Show, Eq)

instance FromJSON UserEntity where
  parseJSON = (UserEntity <$>) . parseJSON

data URLEntity =
  URLEntity
  { ueURL      :: URIString -- ^ The URL that was extracted
  , ueExpanded :: URIString -- ^ The fully resolved URL (only for t.co links)
  , ueDisplay  :: Text    -- ^ Not a URL but a string to display instead of the URL (only for t.co links)
  } deriving (Show, Eq)

instance FromJSON URLEntity where
  parseJSON (Object o) =
    URLEntity <$> o .:  "url"
              <*> o .:  "expanded_url"
              <*> o .:  "display_url"
  parseJSON _ = mzero

data MediaSize =
  MediaSize
  { msWidth :: Int
  , msHeight :: Int
  , msResize :: Text
  } deriving (Show, Eq)

instance FromJSON MediaSize where
  parseJSON (Object o) =
    MediaSize <$> o .: "w"
              <*> o .: "h"
              <*> o .: "resize"
  parseJSON _ = mzero

data MediaTag = MTThumb | MTSmall | MTMedium | MTLarge
                                               deriving (Show, Eq, Ord)

instance FromJSON MediaTag where
  parseJSON (String s) = case s of
    "thumb" -> return MTThumb
    "large" -> return MTLarge
    "medium" -> return MTMedium
    "small" -> return MTSmall
    _ -> fail ("Expected thumb/large/medium/small, not '" ++ unpack s ++ "'") -- TODO: was mzero
    
  parseJSON _ = mzero

data MediaEntity =
  MediaEntity
  { meId      :: Integer   -- ^ ID of the media entity
  , meType    :: Text      -- ^ type of the entity
  , meURL     :: (URIString, URIString)  -- ^ Corresponds to the media_url, media_url_https fields
  , meExpandedURL :: URIString -- ^ Corresponds to expanded_url
  , meDisplayURL :: Text -- ^ shortened form of the expanded URL (corresponds to display_url)
  , meTweetText :: Text -- ^ text used in the actual tweet (corresponds to url)
  , msSizes :: MediaSizes
  } deriving (Show, Eq)

instance FromJSON MediaEntity where
  parseJSON (Object o) =
    MediaEntity <$> o .: "id"
                <*> o .: "type"
                <*> ((,) <$> o .: "media_url" <*> o .: "media_url_https")
                <*> o .: "expanded_url"
                <*> o .: "display_url"
                <*> o .: "url"
                <*> o .: "sizes"
  parseJSON _ = mzero

-- TODO: can any size be optional?
-- TODO: a better name than MediaSizes given that we have MediaSize
data MediaSizes =
  MediaSizes
  { msThumb :: MediaSize
  , msSmall :: MediaSize
  , msMedium :: MediaSize
  , msLarge :: MediaSize
  } deriving (Show, Eq)

             
instance FromJSON MediaSizes where
  parseJSON v@(Object _) =
    let pMap :: Parser (M.Map Text MediaSize)
        pMap = parseJSON v

        conv mp = do
          t <- M.lookup "thumb" mp
          s <- M.lookup "small" mp
          m <- M.lookup "medium" mp
          l <- M.lookup "large" mp
          return $ MediaSizes t s m l
          
    in pMap >>= maybe mzero return . conv

  parseJSON _ = mzero
  
-- | Entity handling.
data Entities =
  Entities
  { enHashTags     :: [Entity HashTagEntity]
  , enUserMentions :: [Entity UserEntity]
  , enURLs         :: [Entity URLEntity]
  , enSymbols      :: [Entity SymbolEntity]
  , enMedia        :: [Entity MediaEntity]
  } deriving (Show, Eq)

instance FromJSON Entities where
  parseJSON (Object o) =
    Entities <$> o .:  "hashtags"
             <*> o .:  "user_mentions"
             <*> o .:  "urls"
             <*> o .:? "symbols" .!= []
             <*> o .:? "media" .!= []
  parseJSON _ = mzero

-- | The character positions the Entity was extracted from
--
--   This is experimental implementation.
--   This may be replaced by more definite types.
type EntityIndices = [Int]

data Entity a =
  Entity
  { entityBody    :: a             -- ^ The detail information of the specific entity types (HashTag, URL, User)
  , entityIndices :: EntityIndices -- ^ The character positions the Entity was extracted from
  } deriving (Show, Eq)

instance FromJSON a => FromJSON (Entity a) where
  parseJSON v@(Object o) =
    Entity <$> parseJSON v
           <*> o .: "indices"
  parseJSON _ = mzero

-- | The coordinates are taken from the @coordinate@ rather than @geo@
--   field.

data Point = Point
             { ptLon :: Double
             , ptLat :: Double
             } deriving (Show, Eq)

instance FromJSON Point where 
  parseJSON (Object o) = do
    checkError o
    ctype <- o .: "type"
    if ctype == "Point"
      then uncurry Point <$> o .: "coordinates"
      else fail $ "Expected type=Point, not '" ++ ctype ++ "'"

  parseJSON _ = mzero

-- | Bounding boxes are assumed to be polygons.
  
data Polygon = Polygon
               { plCoords :: [(Double, Double)] -- ^ (longitude,latitude) pairs
               } deriving (Show, Eq)

instance FromJSON Polygon where 
  parseJSON (Object o) = 
    checkError o >> do
    ctype <- o .: "type"
    if ctype == "Polygon"
      then Polygon . head <$> o .: "coordinates"
      else fail $ "Expected type=Polygon, not '" ++ ctype ++ "'"
    
  parseJSON _ = mzero

-- | Unlike other ids used by Twitter, which are integers,
--   this is string (no attempt is made to decode the value).
type PlaceId      = Text

-- | Represent the \"type\" of a place.
data PlaceType =
  PTAdmin    -- ^ administrative area
  | PTCity     -- ^ city
  | PTCountry      -- ^ country
  | PTNeighborhood -- ^ a neighborhood
  | PTPOI      -- ^ point of interest
  | PTOther Text
    deriving Eq

instance Show PlaceType where
  show PTAdmin        = "admin"
  show PTCity         = "city"
  show PTCountry      = "country"
  show PTNeighborhood = "neighborhood"
  show PTPOI          = "poi"
  show (PTOther t)    = unpack t

instance FromJSON PlaceType where
  parseJSON (String pt) = case pt of
    "admin" -> return PTAdmin
    "city"  -> return PTCity
    "country" ->return PTCountry
    "neighborhood" -> return PTNeighborhood
    "poi"   -> return PTPOI
    _       -> return $ PTOther pt

  parseJSON _ = mzero
  
-- | Some of the information stored in the place field. This is currently
--   geared towards the information returned with a status, rather than
--   an explicit \"place\" query.
-- 
--   TODO: do these fields need to be made optional (i.e. what is the
--   minimal set of fields returned by Twitter)?
data Place =
  Place
  { plId :: PlaceId        -- ^ the id of the place
  , plURI :: URIString     -- ^ Twitter URI for the place
  , plName :: Text         -- ^ the \"name\" field
  , plFullName :: Text     -- ^ the \"full_name\" field
  , plType :: PlaceType    -- ^ the \"place_type\" field
  , plCountry :: Text      -- ^ the \"country\" field
  , plCountryCode :: Text  -- ^ the \"country_code\" field
  , plBBox :: Polygon      -- ^ the bounding box for the location
  , plAttributes :: M.Map Text Text
  } deriving (Eq, Show)

instance FromJSON Place where
  parseJSON (Object o) =
    checkError o >>
    Place <$> o .: "id"
          <*> o .: "url"
          <*> o .: "name"
          <*> o .: "full_name"
          <*> o .: "place_type"
          <*> o .: "country"
          <*> o .: "country_code"
          <*> o .: "bounding_box"
          <*> o .: "attributes"

  parseJSON _ = mzero
  