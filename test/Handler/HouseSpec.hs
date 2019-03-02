{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.HouseSpec where

import TestImport


expectedHouses :: String
expectedHouses = "[{\"rent\":1000,\"address\":{\"pincode\":\"500032\",\"street\":\"Gachibowli\",\"number\":\"24/B\"},\"owner\":{\"email\":\"a@email.com\",\"addressId\":1,\"telephone\":\"\",\"name\":\"A\"}}]"

expectedHouse :: String
expectedHouse = "{\"rent\":1000,\"address\":{\"pincode\":\"500032\",\"street\":\"Gachibowli\",\"number\":\"24/B\"},\"owner\":{\"email\":\"a@email.com\",\"addressId\":1,\"telephone\":\"\",\"name\":\"A\"}}"

createdHouse :: String
createdHouse = "[{\"rent\":2000,\"addressId\":2,\"ownerId\":1,\"id\":1}]"


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
      
      it "should return unauthenticated when user without logging in tries to create house" $ do
        postBody HousesR "{\"rent\":2000,\"address\":{\"pincode\":\"500032\",\"street\":\"Hyderabad\",\"number\":\"24/B\"}}"
        statusIs 403
      
      it "should create house for logged in user" $ do
        userEntity <- createUser "bar"
        authenticateAs userEntity
        postBody HousesR "{\"rent\":2000,\"address\":{\"pincode\":\"500032\",\"street\":\"Gachibowli\",\"number\":\"24/B\"}}"
        
        houses <- runDB $ selectList ([] :: [Filter House]) []
        assertEq "house table has 1 house" (pack createdHouse :: Text) $ toJsonText houses

    describe "House" $ do
      it "should return house not found when house does not exist" $ do
        get ("/houses/1" :: Text)
        statusIs 404
      
      it "should return house details when house is available" $ do
        _ <- createDefaultHouse 1000
        get ("/houses/1" :: Text)
        statusIs 200
        bodyEquals expectedHouse
