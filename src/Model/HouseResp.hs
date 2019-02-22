{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.HouseResp where

import Import

data HouseResp = HouseResp
  { rent :: Int
  , owner :: Person
  , address :: Address
  }

instance ToJSON HouseResp where
  toJSON HouseResp {..} = object
    [ "rent" .= rent
    , "owner" .= owner
    , "address" .= address
    ]
  
instance FromJSON HouseResp where
  parseJSON (Object o) = HouseResp
    <$> (o .: "rent")
    <*> (o .: "owner")
    <*> (o .: "address")
  
  parseJSON _ = mzero

