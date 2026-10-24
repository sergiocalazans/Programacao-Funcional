-- (λf. f 10) (λn. n - 1)
-- M = (λf. f 10)
-- N = (λn. n - 1)

fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

-- Fibonacci
-- (λ0. λ1. λn. n - 1 + n - 2) 0 1 10
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

somalista :: [Int] -> Int
somalista [] = 0
somalista (h:t) = h + somalista t

dobroLista :: [Int] -> [Int]
dobroLista [] = []
dobroLista (h:t) = 2 * h : dobroLista t

positivos :: [Int] -> Int
positivos [] = 0
positivos (h:t)
  | h > 0     = 1 + positivos t
  | otherwise = positivos t


-- Exercicio 1
depositar :: Int -> Int -> Int
depositar saldo valor = ((div saldo 100) + (div valor 100)) * 100

-- Exercicio 2
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (_:t) = 1 + tamanho t

-- Exercicio 3
pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence x (h:t)
  | x == h    = True
  | otherwise = pertence x t

-- Exercicio 4
negativos :: [Int] -> Int
negativos [] = 0
negativos (h:t)
  | h < 0     = 1 + negativos t
  | otherwise = negativos t

-- Exercicio 5
maior :: [Int] -> Int
maior [] = 0
maior [x] = x
maior (h:t) -- alternativa: max h (maior t)
  | h > maior t = h
  | otherwise   = maior t
  -- alternativa:if h > maior t then h else maior t


main :: IO ()
main = do
  print ((\f -> f 10) (\n -> n - 1)) -- Saída: 9
  print ((\x -> \y -> x y + 1) (\z -> z * 2) 3) -- Saída: 7
  print (fatorial 5) -- Saída: 120
  print (fibonacci 10) -- Saída: 55