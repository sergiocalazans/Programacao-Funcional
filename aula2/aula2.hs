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


main :: IO ()
main = do
  print ((\f -> f 10) (\n -> n - 1)) -- Saída: 9
  print ((\x -> \y -> x y + 1) (\z -> z * 2) 3) -- Saída: 7
  print (fatorial 5) -- Saída: 120
  print (fibonacci 10) -- Saída: 55