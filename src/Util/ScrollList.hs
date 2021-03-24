module Util.ScrollList where

up :: Int -> Int -> [a] -> [a]
up index steps items
  | steps <= 0 || index <= 0 || index >= length items = items
  | otherwise = up prev (steps -1) oneStepUpList
  where
    oneStepUpList = take prev items ++ (items !! index) : items !! prev : drop next items
    prev = index - 1
    next = index + 1

down :: Int -> Int -> [a] -> [a]
down index steps items = reverse . up reverseIndex steps $ reverse items
  where
    reverseIndex = length items - index - 1