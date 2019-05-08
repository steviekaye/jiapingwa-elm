module Main exposing (Model, Msg(..), init, main, update, view)

import BookData exposing (Book, allBooks)
import Browser
import Html exposing (Html, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (alt, class, src)
import Html.Events exposing (onClick)
import List exposing (append, filter, map, sum)



---- MODEL ----


type alias Model =
    { books : List Book, cart : Cart }


type alias CartItem =
    { id : BookID, quantity : Int }


type alias Cart =
    List CartItem


type alias BookID =
    Int


add : Cart -> BookID -> Cart
add cart bookID =
    let
        _ =
            Debug.log "Logging b" bookID
    in
    -- append cart [ bookID ]
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

        False ->
            append cart [ { id = bookID, quantity = 1 } ]


remove : Cart -> BookID -> Cart
remove cart bookID =
    -- List.any (\cartItem -> cartItem /= bookID) cart
    cart



-- subtotal : Cart -> Int
-- subtotal cart =
--     -- sum (map bookSubtotal cart)
--     -- sum (map .price cart)
--     sum (map (\item -> item.price) cart)


bookSubtotal : Book -> Int
bookSubtotal b =
    b.price


init : ( Model, Cmd Msg )
init =
    ( { books = allBooks, cart = [] }
    , Cmd.none
    )



---- UPDATE ----


type
    Msg
    -- = NoOp
    = Add BookID
    | Remove BookID


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- ( model, Cmd.none )
    case msg of
        Add b ->
            ( { model | cart = add model.cart b }
            , Cmd.none
            )

        Remove b ->
            ( { model | cart = remove model.cart b }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "book-list" ] (List.map (\b -> bookComponent b model.cart) model.books)
        , div [ class "cart" ] [ cartView model ]
        ]


inCart : BookID -> Cart -> Bool
inCart bookID cart =
    List.any (\cartItem -> cartItem.id == bookID) cart


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
            , button
                [ class buttonClass
                , onClick clickEvent
                ]
                [ text buttonText
                ]
            ]
        ]


cartView : Model -> Html Msg
cartView model =
    div []
        [ div []
            [ if List.isEmpty model.cart then
                div [ class "remove" ]
                    [ text "Cart is empty" ]

              else
                div [] (List.map (\c -> cartItemView c model.books) model.cart)
            ]
        , div [] [ text <| "$" ++ String.fromInt (cartTotalView model) ]
        ]


cartItemView : CartItem -> List Book -> Html Msg
cartItemView cartItem bookList =
    div []
        [ div [] [ text (getName cartItem.id bookList) ]
        , div [] [ text <| "$" ++ String.fromInt (getPrice cartItem.id cartItem.quantity bookList) ]
        ]


getName : BookID -> List Book -> String
getName bookID bookList =
    let
        result =
            List.filter (\b -> b.bookID == bookID) bookList
    in
    case List.head result of
        Nothing ->
            ""

        Just b ->
            b.title


getPrice : BookID -> Int -> List Book -> Int
getPrice bookID quant bookList =
    let
        result =
            List.filter (\b -> b.bookID == bookID) bookList
    in
    case List.head result of
        Nothing ->
            0

        Just b ->
            b.price * quant


cartTotalView : Model -> Int
cartTotalView model =
    List.sum (List.map (\item -> getPrice item.id item.quantity model.books) model.cart)



-- subtotal cart =
--     -- sum (map bookSubtotal cart)
--     -- sum (map .price cart)
--     sum (map (\item -> item.price) cart)
-- let
--     incrementQuantity cartItem =
--         if cartItem.id == bookID then
--             { cartItem | quantity = cartItem.quantity + 1 }
--
--         else
--             cartItem
-- in
-- List.map (\item -> incrementQuantity item) cart
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
