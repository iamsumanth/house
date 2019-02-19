{-# LANGUAGE OverloadedStrings #-}
module Handler.House where

import Import
import Model.HouseReq

getHouseR :: Handler Value
getHouseR = do
 houses <- runDB $ selectList [] [Asc HouseRent]
 return $ toJSON houses

postHouseR :: Handler TypedContent
postHouseR = do
  (HouseReq address' rent') <- requireJsonBody :: Handler HouseReq
  maybeCurrentUserId <- maybeAuthId
  userInfo <- runDB $ getEntity (getUserId maybeCurrentUserId)
  let personId = userPersonId (entityVal (getUser userInfo))
  person <- runDB $ getEntity personId
  addressId <- runDB $ insert address'
  let house' = getHouseModel addressId (entityKey (getPerson person)) rent'
  _ <- runDB $ insertEntity house'
  sendResponseNoContent


getHouseModel :: AddressId -> PersonId -> Int -> House
getHouseModel addressId personId rent' = House rent' personId addressId

getPerson :: Maybe (Entity Person) -> Entity Person
getPerson (Just p) = p

getUser :: Maybe (Entity User) -> Entity User
getUser (Just p) = p

getUserId :: Maybe (AuthId App) -> UserId
getUserId (Just p) = p