module Main exposing (Model, Msg(..), init, main, update, view)

import BookData exposing (Book, allBooks)
import Browser
import Html exposing (Html, button, div, img, table, td, text, th, thead, tr)
import Html.Attributes exposing (alt, class, src)
import Html.Events exposing (onClick)
import List exposing (filter, map, sum)
import List.Extra as ListX exposing (find, setIf)



---- MODEL ----


type alias Model =
    { books : List Book, cart : Cart }


type alias CartItem =
    { id : BookID, quantity : Int }


type alias Cart =
    List CartItem


type alias BookID =
    Int


removeFromCart : BookID -> Cart -> Cart
removeFromCart bookID cart =
    List.filter (\cartItem -> cartItem.id /= bookID) cart


findItemInCart : BookID -> Cart -> Maybe CartItem
findItemInCart bookID cart =
    ListX.find (\cartItem -> cartItem.id == bookID) cart


addToCart : BookID -> Cart -> Cart
addToCart bookID cart =
    case findItemInCart bookID cart of
        Just cartItem ->
            cart

        -- error message?
        Nothing ->
            { id = bookID, quantity = 1 } :: cart



-- could just be the above line if you are certain this cannot be in cart


increment : BookID -> Cart -> Cart
increment bookID cart =
    case findItemInCart bookID cart of
        Just cartItem ->
            ListX.setIf (\item -> item.id == bookID) { cartItem | quantity = cartItem.quantity + 1 } cart

        Nothing ->
            cart


decrement : BookID -> Cart -> Cart
decrement bookID cart =
    case findItemInCart bookID cart of
        Just cartItem ->
            case cartItem.quantity > 1 of
                True ->
                    ListX.setIf (\item -> item.id == bookID) { cartItem | quantity = cartItem.quantity - 1 } cart

                False ->
                    removeFromCart bookID cart

        Nothing ->
            cart


itemQuantity : BookID -> Cart -> Int
itemQuantity bookID cart =
    case findItemInCart bookID cart of
        Nothing ->
            0

        Just r ->
            r.quantity


inCart : BookID -> Cart -> Bool
inCart bookID cart =
    List.any (\cartItem -> cartItem.id == bookID) cart


init : ( Model, Cmd Msg )
init =
    ( { books = allBooks, cart = [] }
    , Cmd.none
    )



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
            ( { model | cart = increment b model.cart }
            , Cmd.none
            )

        Decrement b ->
            ( { model | cart = decrement b model.cart }
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
            [ bookCartButton book cart ]
        ]


bookCartButton : Book -> Cart -> Html Msg
bookCartButton book cart =
    case inCart book.bookID cart of
        True ->
            div []
                [ button [ onClick (Decrement book.bookID) ] [ text "-" ]
                , text (String.fromInt (itemQuantity book.bookID cart))
                , button [ onClick (Increment book.bookID) ] [ text "+" ]
                , button [ class "btn remove", onClick <| Remove book.bookID ] [ text "Remove from cart" ]
                ]

        False ->
            button [ class "btn add", onClick <| Add book.bookID ] [ text "Add to cart" ]


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
    let
        totals =
            List.map (\cartItem -> bookTotal cartItem model.books) model.cart
    in
    List.sum totals



---- PROGRAM ----


bookTotal : CartItem -> List Book -> Int
bookTotal cartItem books =
    let
        mBook =
            ListX.find (\book -> book.bookID == cartItem.id) books
    in
    case mBook of
        Just book ->
            book.price * cartItem.quantity

        Nothing ->
            0


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
