{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Model.Address where

import Data.Text (Text)
import Import


data Address = Address
  { number  :: Text
  , street  :: Text
  , pinCode :: Text
  } deriving Show
  
instance ToJSON Address where
  toJSON Address {..} = object
    [ "number" .= number
    , "street"   .= street
    , "pincode" .= pinCode
    ]

instance FromJSON Address where
  parseJSON (Object o) = Address
    <$> (o .: "number")
    <*> (o .: "street")
    <*> (o .: "pincode")
  
  parseJSON _ = mzero
