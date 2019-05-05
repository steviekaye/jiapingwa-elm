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


type alias Cart =
    List Int


add : Cart -> Int -> Cart
add cart bookID =
    let
        _ =
            Debug.log "Logging b" bookID
    in
    append cart [ bookID ]


remove : Cart -> Int -> Cart
remove cart bookID =
    List.filter (\cartItem -> cartItem /= bookID) cart



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
    = Add Int
    | Remove Int


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
        ]


inCart : Int -> Cart -> Bool
inCart bookID cart =
    List.any (\cartItem -> cartItem == bookID) cart


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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
