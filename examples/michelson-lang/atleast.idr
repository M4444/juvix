module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : (Tez, ()) -> (List Operation, Tez)
main (storage, _) = if amount < storage then fail else (nil, amount)

-- Reject txs below a threshold.
-- Ref https://www.michelson-lang.com/contract-a-day.html#sec-1-32.
