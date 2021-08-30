module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (decodeString, decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (..)
import Test exposing (..)


decoderTest : Test
decoderTest =
    let
        testCase =
            fuzz2 string int "title defaults to (untitled)"

        jsonObject url size =
            Encode.object
                [ ( "url", Encode.string url )
                , ( "size", Encode.int size )
                ]
    in
    testCase <|
        \url size ->
            jsonObject url size
                |> decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")


sliders : Test
sliders =
    describe "Slider sets the desired field in the Model"
        [ testSlider "SlidHue" SlidHue .hue
        , testSlider "SlidRipple" SlidRipple .ripple
        , testSlider "SlidNoise" SlidNoise .noise
        ]


testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount
