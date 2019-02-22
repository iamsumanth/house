{-# LANGUAGE OverloadedStrings #-}
module Handler.House where

import Import
import Model.HouseReq
import Model.HouseResp

getHouseR :: Handler Value
getHouseR = do
  housesWithReference <- runDB $ selectList [] [Asc HouseRent]
  houses <- sequence (Import.map getCompleteHouse housesWithReference)
  return $ toJSON houses


postHouseR :: Handler TypedContent
postHouseR = do
  (HouseReq address' rent') <- requireJsonBody :: Handler HouseReq
  maybeCurrentUserId <- maybeAuthId
  loggedInUser <- runDB $ getJust (getUserId maybeCurrentUserId)
  addressId <- runDB $ insert address'
  let house' = House rent' (userPersonId loggedInUser) addressId
  _ <- runDB $ insertEntity house'
  sendResponseNoContent


getUserId :: Maybe (AuthId App) -> UserId
getUserId (Just p) = p

getCompleteHouse :: Entity House -> Handler HouseResp
getCompleteHouse house = runDB $ do
  let rent' = houseRent (entityVal house)
  person <- getJust (houseOwnerId (entityVal house))
  address' <- getJust (houseAddressId (entityVal house))
  return (HouseResp rent' person address')
