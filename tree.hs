data Node = 
    Empty
    | Node String Node Node Node Integer Integer -- valor, hijo izquierdo, hijo medio, hijo derecho, altura, estado
    deriving (Show)

-- Generar todas las ternas de 0, 1 y 2
ternas :: [[Integer]]
ternas = [[x, y, z] | x <- [0, 1, 2], y <- [0, 1, 2], z <- [0, 1, 2]]

-- Filtrar ternas según el primer dígito
filtrarTernas :: Integer -> [[Integer]] -> [[Integer]]
filtrarTernas n = filter (\(x:_) -> x == n)

-- Construir el árbol
buildTree :: [[Integer]] -> Node
buildTree xs = Node (show [1, 1, 0]) left middle right 1 1
  where
    left = buildSubTree (filtrarTernas 0 xs) 2
    middle = buildSubTree (filtrarTernas 1 (filter (/= [1, 1, 0]) xs)) 2
    right = buildSubTree (filtrarTernas 2 xs) 2

-- Función auxiliar para construir subárboles y calcular alturas
buildSubTree :: [[Integer]] -> Integer -> Node
buildSubTree [] _ = Empty
buildSubTree (x:xs) h = Node (show x) left middle right h 1
  where
    n = length xs `div` 3
    left = buildSubTree (filtrarTernas 0 xs) (h + 1)
    middle = buildSubTree (filtrarTernas 1 xs) (h + 1)
    right = buildSubTree (filtrarTernas 2 xs) (h + 1)

main :: IO ()
main = do
    let tree = buildTree ternas
    print tree