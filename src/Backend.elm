port module Backend exposing (..)
import Types exposing(Song)

import Utils exposing (..)
import Models exposing (Model)

-- Existe la funcion findSong que recibe
-- una condicion y una lista de canciones
-- findSong : (Song -> Bool) -> List Song -> Song

-- Existe la funcion tailSafe que recibe
-- una lista de canciones y se queda con la cola
-- si la lista no tiene cola (tiene un solo elemento)
-- se queda con una lista vacia
-- tailSafe : List Song -> List Song

-- Existe idFirst que recibe una lista
-- de canciones y devuelve el id de la primera
-- idFirst : List Song -> String

-- Debería darnos la url de la cancion en base al id
urlById : String -> List Song -> String
--urlById id songs = ""
urlById idSearched songs =  (searchById idSearched songs).url

sameId : String -> Song -> Bool
sameId idSearched song = idSearched == song.id

searchById : String -> List Song -> Song
searchById idSearched songs = findSong (sameId idSearched) songs 

-- Debería darnos las canciones que tengan ese texto en nombre o artista

filterByName : String -> List Song -> List Song
--filterByName text songs = songs
filterByName text songs = List.filter (sameNameOrArtist (String.toLower text)) (List.map String.toLower songs)

sameNameOrArtist : String -> Song -> Bool
sameNameOrArtist text song = String.contains text song.artist ||  String.contains text song.artist

-- Recibe un id y tiene que likear/dislikear una cancion
-- switchear song.liked

toggleLike : String -> List Song -> List Song
--toggleLike id songs = songs
toggleLike id songs = List.map (switchById id) songs

switchById : String -> Song -> Song
switchById id song = if (sameId id song) then (switchLike song) else song

switchLike : Song -> Song
switchLike song = if isLiked song then { song | liked = False } else { song | liked = True }

--(searchById id songs).liked => (searchById id songs).liked = False
--|= (searchById id songs).liked = True

--if (searchById id songs).liked == True then (searchById id songs).liked = False else (searchById id songs).liked = True 
    
-- Esta funcion tiene que decir si una cancion tiene
-- nuestro like o no, por ahora funciona mal...
-- hay que arreglarla

isLiked : Song  -> Bool
--isLiked song = False
isLiked song = song.liked

-- Recibe una lista de canciones y nos quedamos solo con las que
-- tienen un like

filterLiked : List Song -> List Song
--filterLiked songs = songs
filterLiked songs = List.filter isLiked songs

-- Agrega una cancion a la cola de reproduccion
-- (NO es necesario preocuparse porque este una sola vez)

addSongToQueue : Song -> List Song -> List Song
--addSongToQueue song queue = queue
addSongToQueue song queue = song::queue

-- Saca una cancion de la cola
-- (NO es necesario que se elimine una sola vez si esta repetida)

removeSongFromQueue : String -> List Song -> List Song
--removeSongFromQueue id queue = queue
removeSongFromQueue id queue = List.filter (diferentId id) queue

diferentId : String -> Song -> Bool
diferentId idSearched song = not (sameId idSearched song)

-- Hace que se reproduzca la canción que sigue y la saca de la cola

playNextFromQueue : Model -> Model
playNextFromQueue model = model
--playNextFromQueue model = nextQueue model 

-------- Funciones Listas --------

-- Esta funcion recibe el modelo y empieza a reproducir la
-- cancion que tenga el id que se pasa...
-- Mirar la función urlById
playSong : Model -> String -> Model
playSong model id = { model | playerUrl = urlById id model.songs, playing = (if id /= "" then Just True else Nothing) }

applyFilters : Model -> List Song
applyFilters model =
  model.songs
    |> filterByName model.filterText
    |> if model.onlyLiked then filterLiked else identity

port togglePlay : Bool -> Cmd msg
port songEnded : (Bool -> msg) -> Sub msg