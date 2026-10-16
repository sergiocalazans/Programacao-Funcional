-- Exercício 1
triplo :: Int -> Int
triplo x = x * 3

-- Exercício 2
ehPar :: Int -> Bool
ehPar x = mod x 2 == 0

-- Exercício 3
areaCirculo :: Double -> Double
areaCirculo x = pi * (x ^ 2)

-- Exercício 4
-- Execução:
-- (\f -> \x -> f (f x)) (\y -> y * 2) 3
-- (\x -> (\y -> y * 2) ((\y -> y * 2) x)) 3
-- (\y -> y * 2) ((\y -> y * 2) 3)
-- (\y -> y * 2) (3 * 2)
-- (3 * 2) * 2
-- 6 * 2
-- 12

main :: IO ()
main = do
  print(triplo 4) -- Saída: 12
  print(triplo 3) -- Saída: 9
  print(ehPar 4)  -- Saída: True
  print(areaCirculo 1) -- Saída: 3.141592653589793
  print((\f -> \x -> f (f x)) (\y -> y * 2) 3) -- Saída: 12
