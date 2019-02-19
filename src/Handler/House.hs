{-# LANGUAGE OverloadedStrings #-}
module Handler.House where

import Import
import qualified Model.House as DCH
import qualified Model.Person as DCP
import Model.Address

getHouseR :: Handler Value
getHouseR = return $ toJSON getHouses

postHouseR :: Handler TypedContent
postHouseR = do
  (DCH.House address owner rent) <- requireJsonBody :: Handler DCH.House
  person <- runDB $ getBy (UniquePerson (DCP.email owner))
  let house' = getHouseModel address (entityKey (getPerson person)) rent
  _ <- runDB $ insertEntity house'
  sendResponseNoContent


getHouses :: [DCH.House]
getHouses = [DCH.House (Address "12/B" "Gachibowli" "500032") (DCP.Person "shashi@shashi.com" "Shashi" "12345") 10000]


getHouseModel :: Address -> PersonId -> Int -> House
getHouseModel address personId rent = House number' street' pinCode' rent personId
  where
    number' = number address
    street' = street address
    pinCode' = pinCode address
    

getPerson :: Maybe (Entity Person) -> Entity Person
getPerson (Just p) = p