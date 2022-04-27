module NonEmptyList exposing
    ( NonEmptyList
    , fromList
    , head
    , new
    )


type alias NonEmptyList item =
    ( item, List item )


new : item -> List item -> NonEmptyList item
new first rest =
    ( first, rest )


fromList : List item -> Maybe (NonEmptyList item)
fromList list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            Just ( first, rest )


head : NonEmptyList item -> item
head ( first, _ ) =
    first
