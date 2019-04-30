module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, ul, li)
import Html.Attributes exposing (src, class, alt)
import BookData exposing (Book, allBooks)

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
        [ div [ class "book-list"] ( List.map book allBooks )

        ]

book : Book -> Html Msg
book b =
   div [ class "book" ]
    [ div [] [ img [src b.cover, alt b.title ] [] ]
    , div [ class "book-info"] [
      div [ class "book-title" ] [ text b.title ]
    , div [ class "book-year" ] [ text <| String.fromInt b.year ]
    , div [ class "book-title-CN" ] [ text b.titleCN ]
    , div [ class "book-title-pinyin" ] [text <| " (" ++ b.titlePinyin ++ ")" ]
    , div [ class "book-price" ] [ text <| "$" ++ String.fromInt b.price ]
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
