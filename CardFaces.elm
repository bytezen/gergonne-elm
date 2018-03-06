module CardFaces exposing (faces, Msg(..))

import Array exposing (Array)
import Random.Array 
import Random

type alias Url = String

type Msg =
    Shuffle (Array Url)


faces : Cmd Msg
faces =
    let
        generator = Random.Array.shuffle (Array.fromList urls) 
    in
        Random.generate Shuffle generator 

urls : List Url
urls =
    List.map 
        ((++) root)
        files

root : String
root = "assets/images/"

files : List String
files =
    [
     "180118.png"
    ,"180123.png"
    ,"180200.png"
    ,"180735.png"
    ,"180743.png"
    ,"180933.png"
    ,"181002.png"
    ,"181091.png"
    ,"181101.png"
    ,"181149.png"
    ,"181177.png"
    ,"181210.png"
    ,"181216.png"
    ,"181337.png"
    ,"181356.png"
    ,"181363.png"
    ,"181367.png"
    ,"181403.png"
    ,"181527.png"
    ,"181669.png"
    ,"181685.png"
    ,"181693.png"
    ,"181730.png"
    ,"181804.png"
    ,"181843.png"
    ,"181850.png"
    ,"182009.png"
    ,"182112.png"
    ,"182119.png"
    ,"182156.png"
    ,"182158.png"
    ,"182169.png"
    ,"182201.png"
    ,"182205.png"
    ,"182213.png"
    ,"182216.png"
    ,"182222.png"
    ,"182226.png"
    ,"182277.png"
    ,"182279.png"
    ,"182286.png"
    ,"182293.png"
    ,"182301.png"
    ,"182313.png"
    ,"182319.png"
    ,"182321.png"
    ,"182332.png"
    ,"182376.png"
    ,"182514.png"
    ,"182555.png"
    ,"182595.png"
    ,"182789.png"
    ,"182837.png"
    ,"182845.png"
    ,"182910.png"
    ,"182959.png"
    ,"183031.png"
    ,"183039.png"
    ,"183071.png"
    ,"183097.png"
    ,"183315.png"
    ,"183319.png"
    ,"183353.png"
    ,"183527.png"
    ,"183754.png"
    ,"183809.png"
    ,"185419.png"
    ,"185845.png"
    ,"186038.png"
    ,"186263.png"
    ,"186271.png"
    ,"188654.png"
    ,"188975.png"
    ,"188988.png"
    ,"188995.png"
    ,"189000.png"
    ,"189009.png"
    ,"189018.png"
    ,"189020.png"
    ,"189030.png"
    ,"189036.png"
    ,"189041.png"
    ,"189062.png"
    ,"189073.png"
    ,"189085.png"
    ,"189091.png"
    ,"189093.png"
    ,"189101.png"
    ,"189106.png"
    ,"189115.png"
    ,"189279.png"
    ,"189315.png"
    ,"189324.png"
    ,"189334.png"
    ,"189610.png"
    ,"189787.png"
    ,"189796.png"
    ,"189822.png"
    ,"189829.png"
    ,"189830.png"
    ,"189854.png"
    ,"189889.png"
    ,"189901.png"
    ,"189929.png"
    ]
