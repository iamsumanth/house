{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Model.House where

import Import
import Model.Address (Address)
import qualified Model.Person as DCP

data House = House
  { address :: Address
  , owner   :: DCP.Person
  , rent    :: Int
  } deriving Show

instance ToJSON Model.House.House where
  toJSON Model.House.House {..} = object
    [ "address" .= address
    , "owner"   .= owner
    , "rent"    .= rent
    ]

instance FromJSON Model.House.House where
  parseJSON (Object o) = Model.House.House
    <$> (o .: "address")
    <*> (o .: "owner")
    <*> (o .: "rent")
    
  parseJSON _ = mzero
