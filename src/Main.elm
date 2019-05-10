module Main exposing (Model, Msg(..), init, main, update, view)

import BookData exposing (Book, allBooks)
import Browser
import Html exposing (Html, button, div, h1, img, li, table, td, text, th, thead, tr, ul)
import Html.Attributes exposing (alt, class, src)
import Html.Events exposing (onClick)
import List exposing (append, filter, map, sum)
import List.Extra exposing (setIf)



---- MODEL ----


type alias Model =
    { books : List Book, cart : Cart }


type alias CartItem =
    { id : BookID, quantity : Int }


type alias Cart =
    List CartItem


type alias BookID =
    Int


addToCart : BookID -> Cart -> Cart
addToCart bookID cart =
    case inCart bookID cart of
        True ->
            let
                incrementQuantity cartItem =
                    if cartItem.id == bookID then
                        { cartItem | quantity = cartItem.quantity + 1 }

                    else
                        cartItem
            in
            List.map (\item -> incrementQuantity item) cart

        -- setIf (\cartItem -> cartItem.id == bookID) { cartItem | quantity = cartItem.quantity + 1 } cart
        False ->
            append cart [ { id = bookID, quantity = 1 } ]


removeFromCart : BookID -> Cart -> Cart
removeFromCart bookID cart =
    List.filter (\cartItem -> cartItem.id /= bookID) cart


increment : Cart -> BookID -> Cart
increment cart bookID =
    let
        incrementQuantity cartItem =
            if cartItem.id == bookID then
                { cartItem | quantity = cartItem.quantity + 1 }

            else
                cartItem
    in
    List.map (\item -> incrementQuantity item) cart


decrement : Cart -> BookID -> Cart
decrement cart bookID =
    let
        decrementQuantity cartItem =
            if cartItem.id == bookID then
                if cartItem.quantity > 1 then
                    { cartItem | quantity = cartItem.quantity - 1 }

                else
                    cartItem
                --to change to remove items with 0 or less quant

            else
                cartItem
    in
    List.map (\item -> decrementQuantity item) cart


init : ( Model, Cmd Msg )
init =
    ( { books = allBooks, cart = [] }
    , Cmd.none
    )


getQuantity : BookID -> Cart -> Int
getQuantity bookID cart =
    case find (\c -> c.id == bookID) cart of
        Nothing ->
            0

        Just r ->
            r.quantity


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    List.filter predicate list |> List.head


inCart : BookID -> Cart -> Bool
inCart bookID cart =
    List.any (\cartItem -> cartItem.id == bookID) cart



---- UPDATE ----


type Msg
    = Add BookID
    | Remove BookID
    | Increment BookID
    | Decrement BookID


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add b ->
            ( { model | cart = addToCart b model.cart }
            , Cmd.none
            )

        Remove b ->
            ( { model | cart = removeFromCart b model.cart }
            , Cmd.none
            )

        Increment b ->
            ( { model | cart = increment model.cart b }
            , Cmd.none
            )

        Decrement b ->
            ( { model | cart = decrement model.cart b }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "book-list" ] (List.map (\b -> bookComponent b model.cart) model.books)
        , div [ class "cart" ] [ cartView model ]
        ]


bookComponent : Book -> Cart -> Html Msg
bookComponent book cart =
    let
        ( buttonClass, clickEvent, buttonText ) =
            if inCart book.bookID cart then
                ( "btn remove", Remove book.bookID, "Remove" )

            else
                ( "btn add", Add book.bookID, "Add to cart" )
    in
    div [ class "book" ]
        [ div [] [ img [ src <| "%PUBLIC_URL%" ++ book.cover, alt book.title ] [] ]
        , div [ class "book-info" ]
            [ div [ class "book-title" ] [ text book.title ]
            , div [ class "book-year" ] [ text <| String.fromInt book.year ]
            , div [ class "book-title-CN" ] [ text book.titleCN ]
            , div [ class "book-title-pinyin" ] [ text <| " (" ++ book.titlePinyin ++ ")" ]
            , div [ class "book-price" ] [ text <| "$" ++ String.fromInt book.price ]
            ]
        , div [ class "buttons" ]
            [ button
                [ class buttonClass
                , onClick clickEvent
                ]
                [ text buttonText
                ]
            , button [ onClick (Decrement book.bookID) ] [ text "-" ]
            , text (String.fromInt (getQuantity book.bookID cart))
            , button [ onClick (Increment book.bookID) ] [ text "+" ]
            ]
        ]


cartView : Model -> Html Msg
cartView model =
    div []
        [ table []
            ([ thead []
                [ th [] [ text "Item" ]
                , th [] [ text "Quantity" ]
                , th [] [ text "Amount" ]
                ]
             ]
                ++ List.map (\c -> cartItemView c model.books) model.cart
                ++ [ tr
                        []
                        [ td [] [ text "Total" ]
                        , td [] []
                        , td [] [ text <| "$" ++ String.fromInt (cartTotalView model) ]
                        ]
                   ]
            )
        ]


cartItemView : CartItem -> List Book -> Html Msg
cartItemView cartItem bookList =
    tr []
        [ td [] [ text <| getName cartItem.id bookList ]
        , td [] [ text (String.fromInt cartItem.quantity) ]
        , td [] [ text <| "$" ++ String.fromInt (getPrice cartItem.id cartItem.quantity bookList) ]
        ]


getName : BookID -> List Book -> String
getName bookID bookList =
    case find (\b -> b.bookID == bookID) bookList of
        Nothing ->
            ""

        Just r ->
            r.title


getPrice : BookID -> Int -> List Book -> Int
getPrice bookID quant bookList =
    case find (\b -> b.bookID == bookID) bookList of
        Nothing ->
            0

        Just r ->
            r.price * quant


cartTotalView : Model -> Int
cartTotalView model =
    List.sum (List.map (\item -> getPrice item.id item.quantity model.books) model.cart)



-- List.foldr (\item -> getPrice item.id item.quantity model.books) 0 model.cart
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
