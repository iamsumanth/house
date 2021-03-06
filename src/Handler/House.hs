{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.House where

import Import
import Model.HouseReq
import Model.HouseResp

getHousesR :: Handler Value
getHousesR = do
  housesWithReference <- runDB $ selectList [] [Asc HouseRent]
  houses <- mapM (getCompleteHouse . entityVal) housesWithReference
  return $ toJSON houses

postHousesR :: Handler TypedContent
postHousesR = do
  (HouseReq address' rent') <- requireJsonBody :: Handler HouseReq
  currentUserId <- requireAuthId
  loggedInUser <- runDB $ getJust currentUserId
  addressId <- runDB $ insert address'
  let house' = House rent' (userPersonId loggedInUser) addressId
  _ <- runDB $ insertEntity house'
  sendResponseNoContent


getHouseR :: HouseId -> Handler Value
getHouseR houseId = do
  house' <- runDB $ get404 houseId
  fmap toJSON (getCompleteHouse house')

deleteHouseR :: HouseId -> Handler Value
deleteHouseR houseId = do
  house' <- runDB $ get404 houseId
  currentUserId <- requireAuthId
  loggedInUser <- runDB $ getJust currentUserId
  let isUserAuthorizedToDelete = isUserOwnerOfTheHouse loggedInUser house'
  if isUserAuthorizedToDelete
    then do
      _ <- runDB $ delete houseId
      sendResponseNoContent
    else
      permissionDenied "User does not own this house"


isUserOwnerOfTheHouse :: User -> House -> Bool
isUserOwnerOfTheHouse user house = userPersonId user == houseOwnerId house

getCompleteHouse :: House -> Handler HouseResp
getCompleteHouse house = runDB $ do
  let rent' = houseRent house
  person <- getJust (houseOwnerId house)
  address' <- getJust (houseAddressId house)
  return (HouseResp rent' person address')
