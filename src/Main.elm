module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, ul, li)
import Html.Attributes exposing (src, class, alt)
import BookData exposing (Book, allbooks)

---- MODEL ----


type alias Model =
    {

  }

init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "book-list"] ( List.map viewBooks allbooks.books )

        ]

viewBooks : Book -> Html Msg
viewBooks book =
   div [ class "book" ]
    [ div [] [ img [src book.cover, alt book.title ] [] ]
    , div [ class "book-info"] [
      div [ class "book-title" ] [ text book.title ]
    , div [ class "book-year" ] [ text (String.fromInt book.year ) ]
    , div [ class "book-title-CN" ] [ text book.titleCN ]
    , div [ class "book-title-pinyin" ] [text " (", text book.titlePinyin, text ")" ]
    , div [ class "book-price" ] [ text "$", text (String.fromInt book.price ) ]
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
