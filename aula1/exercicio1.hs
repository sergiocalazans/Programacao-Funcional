triplo :: Int -> Int
triplo x = x * 3

ehPar :: Int -> Bool
ehPar x = mod x 2 == 0

areaCirculo :: Double -> Double
areaCirculo x = 3.141592653589793 * (x ** 2)

main :: IO ()
main = do
  print(triplo 4) -- Saída: 12
  print(triplo 3) -- Saída: 9
  print(ehPar 4)  -- Saída: True
  print(areaCirculo 1) -- Saída: 3.141592653589793
