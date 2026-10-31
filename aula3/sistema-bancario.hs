depositar :: Int -> Int -> Int        -- valor, saldo -> novo saldo
depositar valor saldo = valor + saldo

sacar :: Int -> Int -> Int            -- sem saldo suficiente: saldo inalterado
sacar valor saldo
  | valor <= saldo = saldo - valor
  | otherwise      = saldo

aplicar :: Int -> Int -> Int
aplicar valor saldo
  | valor >= 0 = depositar valor saldo
  | otherwise  = sacar (abs valor) saldo

rendimento :: Int -> Int
rendimento saldo = saldo + div saldo 100

criarTarifa :: Int -> (Int -> Int)
criarTarifa valor = \saldo -> saldo - valor

tarifaSimples :: Int -> Int
tarifaSimples = criarTarifa 5

tarifaPlus :: Int -> Int
tarifaPlus = criarTarifa 30

saldo :: [Int]
saldo = [1000, 2000, 3000, 0]

aplicaDuasVezes :: (Int -> Int) -> Int -> Int
aplicaDuasVezes funcao saldo = funcao (funcao saldo)

aplicaNVezes :: (Int -> Int) -> Int -> Int -> Int
aplicaNVezes funcao saldo 0 = saldo
aplicaNVezes funcao saldo n = aplicaNVezes funcao (funcao saldo) (n - 1)

nossoMap :: (Int -> Int) -> [Int] -> [Int]
nossoMap _ [] = []
nossoMap funcao (h:t) = funcao h : nossoMap funcao t

contas :: [(String, Int)]
contas = [("Alice", 1000), ("Bob", 2000), ("Charlie", 3000), ("David", 0)]


extrato :: [Int]
extrato = [500, -50, 400, -40, 300, -30]

ehDeposito :: Int -> Bool
ehDeposito valor = valor > 0

ehSaque :: Int -> Bool
ehSaque valor = valor < 0

podePagarTarifa :: Int -> Bool
podePagarTarifa saldo = saldo >= 5

clienteVip :: (String, Int) -> Bool
clienteVip (_, saldo) = saldo >= 2500

-- 1. Aplicar juros (percentual inteiro) a todos os saldos
aplicarJuros :: Int -> [Int] -> [Int]
aplicarJuros percentual = map (\saldo -> saldo + div (saldo * percentual) 100)

-- 2. Saques cujo valor ultrapassa um limite
saquesAcimaDe :: Int -> [Int] -> [Int]
saquesAcimaDe limite = filter (\valor -> valor > limite)

main :: IO ()
main = do
  print (depositar 50 1000)                     
  print (sacar 50 1000)                          
  print (aplicar 50 1000)                          
  print (aplicar (-50) 1000)     
  print (rendimento 1000)
  print (tarifaSimples 1000)
  print (tarifaPlus 1000)
  print (aplicaDuasVezes (criarTarifa 5) 1000)
  print (aplicaNVezes (criarTarifa 5) 1000 3)
  print (nossoMap (criarTarifa 5) saldo)
  print (map fst contas)
  print (map snd contas)
  print (map rendimento (map snd contas))
  print (saldo)
  print (filter ehDeposito extrato)
  print (filter ehSaque extrato)
  print (map tarifaSimples saldo)
  print (map tarifaSimples (filter podePagarTarifa saldo))
  print (map fst (filter clienteVip contas))
  print (aplicarJuros 10 saldo)
  print (saquesAcimaDe 100 extrato)