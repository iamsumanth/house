{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Model.HouseReq where

import Import

data HouseReq = HouseReq
  { address :: Address
  , rent    :: Int
  } deriving Show

instance ToJSON HouseReq where
  toJSON HouseReq {..} = object
    [ "address" .= address
    , "rent"    .= rent
    ]

instance FromJSON HouseReq where
  parseJSON (Object o) = HouseReq
    <$> (o .: "address")
    <*> (o .: "rent")
    
  parseJSON _ = mzero
