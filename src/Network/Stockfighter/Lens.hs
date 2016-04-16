{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Stockfighter.Lens (
    -- Order
    stock, venue, direction, originalQuantity,
    price, orderType, orderId, account, timestamp, totalFilled,
    open,

    -- Fill
    quantity, --price, timestamp,

    -- Quote
    --stock, venue,
    bid, ask, bidSize, askSize, bidDepth, askDepth,
    lastPrice, lastQuantity, lastTimestamp, --timestamp,
    ) where

import Control.Lens (Lens')
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Network.Stockfighter.Types (
    Fill, Order, OrderId, Quote)

makeLensesWith abbreviatedFields ''Fill
makeLensesWith abbreviatedFields ''Order
makeLensesWith abbreviatedFields ''Quote

orderId :: HasId o OrderId => Lens' o OrderId
orderId = Network.Stockfighter.Lens.id   -- oId abbreviates to id
