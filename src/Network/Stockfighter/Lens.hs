{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Stockfighter.Lens (
    stock, venue, direction, originalQuantity,
    price, orderType, orderId, account, timestamp, totalFilled,
    open
    ) where

import Control.Lens (Lens')
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Network.Stockfighter.Types (
    Order, OrderId)

makeLensesWith abbreviatedFields ''Order

orderId :: HasId o OrderId => Lens' o OrderId
orderId = Network.Stockfighter.Lens.id   -- oId abbreviates to id
