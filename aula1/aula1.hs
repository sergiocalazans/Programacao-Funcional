dobro :: Int -> Int 
dobro x = x * 2 --  λx. x * 2

ehImpar :: Int -> Bool
ehImpar x = mod x 2 /= 0

soma :: Int -> Int -> Int
soma x y = x + y

-- (λx. λy. x + y) 2 3
-- (λy. 2 + y) 3
-- 2 + 3
-- 5

-- Direto no interpretador: (\x -> \y -> x + y) 2 3

main :: IO()
main = do
  putStrLn "Olá, mundo!"
  putStrLn "Este é o meu primeiro programa em Haskell."
  print (2 + 2) -- SOMA: 4
  print (10 - 7) -- SUBTRAÇÃO: 3
  print (5 * 3) -- MULTIPLICAÇÃO: 15
  print (div 10 2) -- DIVISÃO: 5
  print (10 `div` 3) -- DIVISÃO INTEIRA: 3
  print (mod 10 3) -- MÓDULO: 1
  print (2 ^ 3) -- POTÊNCIA: 8
  print (sqrt 16) -- RAIZ QUADRADA: 4.0
  print (pi) -- PI: 3.141592653589793
  print (not True) -- NEGANDO: False
  print (True && False) -- E: False
  print (True || False) -- OU: True
  print (5 > 3) -- MAIOR: True
  print (5 < 3) -- MENOR: False
  print (5 == 5) -- IGUAL: True
  print (5 /= 3) -- DIFERENTE: True
  print (if 5 > 3 then "Maior" else "Menor") -- CONDICIONAL: "Maior"