{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Model.House where

import Import
import Model.Address (Address)
import Model.Person (Person)


data House = House
  { address :: Address
  , owner   :: Person
  , rent    :: Int
  } deriving Show

instance ToJSON House where
  toJSON House {..} = object
    [ "address" .= address
    , "owner"   .= owner
    , "rent"    .= rent
    ]

instance FromJSON House where
  parseJSON (Object o) = House
    <$> (o .: "address")
    <*> (o .: "owner")
    <*> (o .: "rent")
    
  parseJSON _ = mzero