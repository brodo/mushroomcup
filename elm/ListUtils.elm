module ListUtils (shuffle, divide, divideInto) where
import Random
import List exposing (..)


-- Shuffle a list
shuffle : List a -> Random.Seed -> List a
shuffle list seed =
  let
    listLength = length list
    randomListGenerator = Random.list listLength (Random.float 0 10)
    randomList = fst (Random.generate randomListGenerator seed)
    listOfPairs = List.map2 (,) randomList list
    listOfSortedPairs = sortBy fst listOfPairs
  in
    List.map snd listOfSortedPairs


{- Divide a list into a list of lists. Each sub-list has length `size`.
  If the list can't be evenly divided to list of the specifies length,
  the last sub-list is smaller and contains reminding elements.
-}
divide : List a -> Int -> List (List a)
divide list size =
  foldl (addToFront size) [] list


{- Divde a list into a number of sub-lists. If the list can't be devided
  evenly, it will be devided as evenly as possible.
-}
divideInto : List a -> Int -> List (List a)
divideInto list numberOfSublists =
  let
    targetSize = (length list) // numberOfSublists
    listOfLists = foldl (addToFront targetSize) [] list
    hasAdditionalElements = (length listOfLists) > numberOfSublists
    listLength = length list
  in
    if hasAdditionalElements
    then
      let
        additionalElements = Maybe.withDefault [] (head listOfLists)
        numberOfAdditionalElements = length additionalElements
        justs = map Just additionalElements
        nothings = repeat (listLength - numberOfAdditionalElements) Nothing
        normalSublists = Maybe.withDefault [] (tail listOfLists)
        additionals =
          List.append justs nothings
        addAdditionals additional list =
          case additional of
            Just x ->
              x :: list
            Nothing ->
              list
      in
        map2 addAdditionals additionals normalSublists
    else
      listOfLists


addToFront : Int -> a  -> List (List a) -> List (List a)
addToFront count current (list) =
  let
    hd = Maybe.withDefault [] (head list)
    tl = Maybe.withDefault [] (tail list)
  in
    if length hd >= count
      then [current] :: list
      else (current :: hd) :: tl
