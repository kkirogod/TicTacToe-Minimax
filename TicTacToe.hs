import Data.List (transpose, maximumBy, minimumBy)
import Data.Maybe (isNothing, isJust, fromJust)
import System.Process

type Tablero = [Maybe Char] -- Representa el tablero como una lista de 9 elementos (Maybe Char)

-- Muestra el tablero
mostrarTablero :: Tablero -> IO ()
mostrarTablero tablero = do
    putStrLn $ unlines $ map getFila [0, 3, 6]
  where
    getFila i = concat [getCasilla (tablero !! j) | j <- [i .. i + 2]]
    getCasilla Nothing  = " . "
    getCasilla (Just c) = " " ++ [c] ++ " "

-- Verifica si hay un ganador
hayGanador :: Tablero -> Maybe Char
hayGanador tablero = foldl hayLinea Nothing lineasPosibles
  where
    lineasPosibles = filas ++ columnas ++ diagonales
    filas = [[tablero !! i, tablero !! (i + 1), tablero !! (i + 2)] | i <- [0, 3, 6]]
    columnas = transpose filas
    diagonales = [[tablero !! 0, tablero !! 4, tablero !! 8], [tablero !! 2, tablero !! 4, tablero !! 6]]
    hayLinea acc linea
      | all (== Just 'X') linea = Just 'X'
      | all (== Just 'O') linea = Just 'O'
      | otherwise = acc

-- Verifica si el tablero está lleno
tableroLeno :: Tablero -> Bool
tableroLeno = all isJust

-- Obtiene las casillas vacías
casillasVacias :: Tablero -> [Int]
casillasVacias tablero = [i | (i, Nothing) <- zip [0..] tablero]

-- Realiza un movimiento
mueve :: Tablero -> Int -> Char -> Maybe Tablero
mueve tablero casilla jugador
    | casilla >= 0 && casilla < length tablero && isNothing (tablero !! casilla) = Just (take casilla tablero ++ [Just jugador] ++ drop (casilla + 1) tablero)
    | otherwise = Nothing

-- Obtiene al jugador oponente
oponente :: Char -> Char
oponente 'X' = 'O'
oponente 'O' = 'X'

-- Minimax
minimax :: Tablero -> Char -> Char -> Int
minimax tablero jugador agente
    | isJust ganador = case ganador of
                        Just 'X' -> if agente == 'X' then 1 else -1
                        Just 'O' -> if agente == 'O' then 1 else -1
                        _        -> 0
    | tableroLeno tablero = 0
    | jugador == agente = maximum $ map (\casilla -> minimax (fromJust (mueve tablero casilla jugador)) (oponente jugador) agente) (casillasVacias tablero)
    | otherwise         = minimum $ map (\casilla -> minimax (fromJust (mueve tablero casilla jugador)) (oponente jugador) agente) (casillasVacias tablero)
  where
    ganador = hayGanador tablero

-- Obtiene el mejor movimiento para el agente
mejorMovimiento :: Tablero -> Char -> Int
mejorMovimiento tablero jugador = fst $ maximumBy comparaMovimientos movimientos
  where
    movimientos = [(casilla, minimax (fromJust (mueve tablero casilla jugador)) (oponente jugador) jugador) | casilla <- casillasVacias tablero]
    comparaMovimientos (_, puntuacion1) (_, puntuacion2) = compare puntuacion1 puntuacion2

-- Bucle principal del juego
partida :: Tablero -> Char -> Char -> IO ()
partida tablero jugador agente = do
    clear
    mostrarTablero tablero
    if isJust (hayGanador tablero) then do
        let ganador = fromJust (hayGanador tablero)
        if ganador == agente then
            putStrLn "¡Ooops, el agente te ha ganado!"
        else 
            putStrLn "¡Enhorabuena, has ganado!"
    else if tableroLeno tablero then
        putStrLn "¡Es un empate!"
    else do
        if jugador == agente then do
            putStrLn "Turno del agente..."
            let casilla = mejorMovimiento tablero jugador
            putStrLn $ "El agente elige la casilla: " ++ show casilla
            partida (fromJust (mueve tablero casilla jugador)) (oponente jugador) agente
        else do
            putStrLn "Tu turno (elige una casilla del 0 al 8):"
            casilla <- readLn
            case mueve tablero casilla jugador of
                Just nuevoTablero -> partida nuevoTablero (oponente jugador) agente
                Nothing -> do
                    putStrLn "ERROR: Movimiento inválido."
                    partida tablero jugador agente

-- Inicia el juego, permitiendo al usuario elegir su jugador
main :: IO ()
main = do
    putStrLn "¡Bienvenido al Tic Tac Toe!"
    putStrLn "¿Quieres jugar como 'X' o 'O'?"
    eleccion <- getLine
    let jugador = if eleccion == "X" then 'X' else 'O'
    let agente = if jugador == 'X' then 'O' else 'X'
    let tablero = replicate 9 Nothing  -- 'Nothing' es la casilla vacía
    partida tablero 'X' agente -- Las reglas dicen que siempre empiezan las 'X'
    putStrLn "Pulsa Enter para salir..."
    _ <- getLine
    putStrLn "Adios!"

clear :: IO () 
clear = do
        system "cls"
        return ()