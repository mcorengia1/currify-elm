port module Backend exposing (..)
import Types exposing(Song)

import Utils exposing (..)
import Models exposing (Model)

import String exposing (..)

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


 {- cover : String
    , url : String
    , artist : String
    , id : String -}

idCompare : String -> Song -> Bool
idCompare id song = song.id == id

buscarPorId : String -> List Song -> Song
buscarPorId id songs = findSong (idCompare id) songs

-- Debería darnos la url de la cancion en base al id
urlById : String -> List Song -> String
urlById id songs = (buscarPorId id songs).url

textoEnNombreArtista : String -> Song -> Bool
textoEnNombreArtista text song = contains text (song.artist) || contains text (song.name)

-- Debería darnos las canciones que tengan ese texto en nombre o artista
filterByName : String -> List Song -> List Song
filterByName text songs = List.filter (textoEnNombreArtista text) songs

switchearLike : Song -> Song
switchearLike song = {song | liked = not song.liked}

switchearSi : String -> Song -> Song
switchearSi id song = if (idCompare id song) then (switchearLike song) else song

-- Recibe un id y tiene que likear/dislikear una cancion
-- switchear song.liked
toggleLike : String -> List Song -> List Song
toggleLike id songs = List.map (switchearSi id) songs

-- Esta funcion tiene que decir si una cancion tiene
-- nuestro like o no, por ahora funciona mal...
-- hay que arreglarla
isLiked : Song  -> Bool
isLiked song = song.liked

-- Recibe una lista de canciones y nos quedamos solo con las que
-- tienen un like
filterLiked : List Song -> List Song
filterLiked songs = List.filter isLiked songs

-- Agrega una cancion a la cola de reproduccion
-- (NO es necesario preocuparse porque este una sola vez)
addSongToQueue : Song -> List Song -> List Song
addSongToQueue song queue = List.append ( List.singleton song ) queue

-- Saca una cancion de la cola
-- (NO es necesario que se elimine una sola vez si esta repetida)
removeSongFromQueue : String -> List Song -> List Song
removeSongFromQueue id queue = List.filter (not << (idCompare id)) queue

lastId : Model -> String
lastId model = case model.queue of
                    [] -> ""
                    a :: b -> a.id

removeLast : Model -> Model 
removeLast model = { model | queue = removeSongFromQueue (lastId model) model.queue }

-- Hace que se reproduzca la canción que sigue y la saca de la cola
playNextFromQueue : Model -> Model
playNextFromQueue model = removeLast ( playSong model (lastId model) )

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