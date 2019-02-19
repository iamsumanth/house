{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Model.Person where

import Data.Text (Text)
import Import

data Person = Person
  { email :: Text
  , name :: Text
  , telephone :: Text
  } deriving Show
  
instance ToJSON Model.Person.Person where
  toJSON Model.Person.Person {..} = object
    [ "email" .= email
    , "name" .= name
    , "telephone"   .= telephone
    ]

instance FromJSON Model.Person.Person where
  parseJSON (Object o) = Model.Person.Person
    <$> (o .: "email")
    <*> (o .: "name")
    <*> (o .: "telephone")
  
  parseJSON _ = mzero

