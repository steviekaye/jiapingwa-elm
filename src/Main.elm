module Main exposing (Model, Msg(..), book, init, main, update, view)

import BookData exposing (Book, allBooks)
import Browser
import Html exposing (Html, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (alt, class, src)
import Html.Events exposing (onClick)
import List exposing (append, map, sum)



---- MODEL ----


type alias Model =
    { books : List Book, cart : Cart }


type alias Cart =
    List Book


add : Cart -> Book -> Cart
add cart b =
    append cart [ b ]


subtotal : Cart -> Int
subtotal cart =
    sum (map bookSubtotal cart)


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
    = Add Book


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- ( model, Cmd.none )
    case msg of
        Add b ->
            ( { model | cart = add model.cart b }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "book-list" ] (List.map (\b -> book b model.cart) model.books)
        ]


isInCart : Book -> Cart -> Bool
isInCart b c =
    List.any (\{ bookID } -> bookID == b.bookID) c


book : Book -> Cart -> Html Msg
book b c =
    let
        inCart =
            isInCart b c
    in
    div [ class "book" ]
        [ div [] [ img [ src <| "%PUBLIC_URL%" ++ b.cover, alt b.title ] [] ]
        , div [ class "book-info" ]
            [ div [ class "book-title" ] [ text b.title ]
            , div [ class "book-year" ] [ text <| String.fromInt b.year ]
            , div [ class "book-title-CN" ] [ text b.titleCN ]
            , div [ class "book-title-pinyin" ] [ text <| " (" ++ b.titlePinyin ++ ")" ]
            , div [ class "book-price" ] [ text <| "$" ++ String.fromInt b.price ]
            , button
                [ class
                    (if inCart then
                        "btn remove"

                     else
                        "btn add"
                    )
                , onClick
                    (if inCart then
                        Add b
                        -- Remove b

                     else
                        Add b
                    )
                ]
                [ text
                    (if inCart then
                        "Remove"

                     else
                        "Add to Cart"
                    )
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
