{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.HouseSpec where

import TestImport


expectedHouses :: String
expectedHouses = "[{\"rent\":1000,\"address\":{\"pincode\":\"500032\",\"street\":\"Gachibowli\",\"number\":\"24/B\"},\"owner\":{\"email\":\"a@email.com\",\"addressId\":1,\"telephone\":\"\",\"name\":\"A\"}}]"

expectedHouse :: String
expectedHouse = "{\"rent\":1000,\"address\":{\"pincode\":\"500032\",\"street\":\"Gachibowli\",\"number\":\"24/B\"},\"owner\":{\"email\":\"a@email.com\",\"addressId\":1,\"telephone\":\"\",\"name\":\"A\"}}"

spec :: Spec
spec =
  withApp $ do
    describe "Houses" $ do
      it "should return empty array when there are no houses posted" $ do
        get ("/houses" :: Text)
        statusIs 200
        bodyEquals "[]"
      it "should return all houses with address and owner information" $ do
        _ <- createDefaultHouse 1000
        get HousesR
        statusIs 200
        bodyEquals expectedHouses
    describe "House" $ do
      it "should return house not found when house does not exist" $ do
        get ("/houses/1" :: Text)
        statusIs 404
      it "should return house details when house is available" $ do
        _ <- createDefaultHouse 1000
        get ("/houses/1" :: Text)
        statusIs 200
        bodyEquals expectedHouse

