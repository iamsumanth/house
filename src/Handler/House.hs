{-# LANGUAGE OverloadedStrings #-}
module Handler.House where

import Import
import Model.HouseReq
import Model.HouseResp

getHousesR :: Handler Value
getHousesR = do
  housesWithReference <- runDB $ selectList [] [Asc HouseRent]
  houses <- sequence (Import.map getCompleteHouse housesWithReference)
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
  house' <- runDB $ getJustEntity houseId
  fmap toJSON (getCompleteHouse house')


deleteHouseR :: HouseId -> Handler Value
deleteHouseR houseId = do
  _ <- runDB $ delete houseId
  sendResponseNoContent


getCompleteHouse :: Entity House -> Handler HouseResp
getCompleteHouse house = runDB $ do
  let rent' = houseRent (entityVal house)
  person <- getJust (houseOwnerId (entityVal house))
  address' <- getJust (houseAddressId (entityVal house))
  return (HouseResp rent' person address')
