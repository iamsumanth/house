{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Model.Person where

import Data.Text (Text)
import Import

data Person = Person
  { name :: Text
  , phoneNumber :: Text
  } deriving Show
  
instance ToJSON Person where
  toJSON Person {..} = object
    [ "name" .= name
    , "phone_number"   .= phoneNumber
    ]

instance FromJSON Person where
  parseJSON (Object o) = Person
    <$> (o .: "name")
    <*> (o .: "phone_number")
  
  parseJSON _ = mzero

