-- Funções para um sistema bancário simples

-- Função para depositar um valor em um saldo
depositar :: Int -> Int -> Int        -- valor, saldo -> novo saldo
depositar valor saldo = valor + saldo

-- Função para sacar um valor de um saldo
sacar :: Int -> Int -> Int            -- sem saldo suficiente: saldo inalterado
sacar valor saldo
  | valor <= saldo = saldo - valor
  | otherwise      = saldo

-- Função para aplicar um valor (positivo ou negativo) a um saldo
aplicar :: Int -> Int -> Int
aplicar valor saldo
  | valor >= 0 = depositar valor saldo
  | otherwise  = sacar (abs valor) saldo

-- Função para calcular o rendimento de um saldo (1% de juros)
rendimento :: Int -> Int
rendimento saldo = saldo + div saldo 100

-- Função para criar uma tarifa que subtrai um valor fixo de um saldo
criarTarifa :: Int -> (Int -> Int)
criarTarifa valor = \saldo -> saldo - valor

-- Funções para aplicar tarifas simples e plus
tarifaSimples :: Int -> Int
tarifaSimples = criarTarifa 5

-- Função para aplicar tarifa plus
tarifaPlus :: Int -> Int
tarifaPlus = criarTarifa 30

-- Lista de saldos para testes
saldos :: [Int]
saldos = [1000, 2000, 3000, 0]

-- Função para aplicar uma função duas vezes a um saldo
aplicaDuasVezes :: (Int -> Int) -> Int -> Int
aplicaDuasVezes funcao saldo = funcao (funcao saldo)

-- Função para aplicar uma função n vezes a um saldo
aplicaNVezes :: (Int -> Int) -> Int -> Int -> Int
aplicaNVezes funcao saldo 0 = saldo
aplicaNVezes funcao saldo n = aplicaNVezes funcao (funcao saldo) (n - 1)

-- Implementação de map e filter
nossoMap :: (a -> b) -> [a] -> [b]
nossoMap _ [] = []
nossoMap funcao (h:t) = funcao h : nossoMap funcao t

nossoFilter :: (a -> Bool) -> [a] -> [a]
nossoFilter _ [] = []
nossoFilter funcao (h:t)
  | funcao h  = h : nossoFilter funcao t
  | otherwise = nossoFilter funcao t

-- Lista de contas com nomes e saldos
contas :: [(String, Int)]
contas = [("Alice", 1000), ("Bob", 2000), ("Charlie", 3000), ("David", 0)]

-- Lista de extrato com depósitos e saques
extrato :: [Int]
extrato = [500, -50, 400, -40, 300, -30]

-- Funções para verificar se um valor é depósito ou saque
ehDeposito :: Int -> Bool
ehDeposito valor = valor > 0

ehSaque :: Int -> Bool
ehSaque valor = valor < 0

-- Função para verificar se um saldo pode pagar a tarifa
podePagarTarifa :: Int -> Bool
podePagarTarifa saldo = saldo >= 5

-- Função para verificar se um cliente é VIP (saldo >= 2500)
clienteVip :: (String, Int) -> Bool
clienteVip (_, saldo) = saldo >= 2500

-- Exercício 1. Aplicar juros (percentual inteiro) a todos os saldos
aplicarJuros :: Int -> [Int] -> [Int]
aplicarJuros percentual = map (\saldo -> saldo + div (saldo * percentual) 100)

-- Exercício 2. Saques cujo valor ultrapassa um limite
saquesAcimaDe :: Int -> [Int] -> [Int]
saquesAcimaDe limite = filter (\valor -> valor > limite)

totalDepositado :: [Int] -> Int
totalDepositado lista = foldr (+) 0 (filter ehDeposito lista)

saldoFinal :: Int -> [Int] -> Int
saldoFinal inicial lista = foldr aplicar inicial lista

main :: IO ()
main = do
  print (depositar 50 1000)  -- Depositar 50 em um saldo de 1000

  print (sacar 50 1000)  -- Sacar 50 de um saldo de 1000

  print (aplicar 50 1000)   -- Aplicar um depósito de 50 em um saldo de 1000  

  print (aplicar (-50) 1000)  -- Aplicar um saque de 50 em um saldo de 1000

  print (rendimento 1000) -- Calcular o rendimento de um saldo de 1000

  print (tarifaSimples 1000) -- Aplicar a tarifa simples de 5 em um saldo de 1000

  print (tarifaPlus 1000) -- Aplicar a tarifa plus de 30 em um saldo de 1000

  print (aplicaDuasVezes (criarTarifa 5) 1000) -- Aplicar a tarifa de 5 duas vezes em um saldo de 1000

  print (aplicaNVezes (criarTarifa 5) 1000 3) -- Aplicar a tarifa de 5 três vezes em um saldo de 1000

  print (nossoMap (criarTarifa 5) saldos) -- Aplicar a tarifa de 5 em todos os saldos

  print (map fst contas) -- Obter os nomes dos clientes das contas

  print (map snd contas) -- Obter os saldos das contas

  print (map rendimento (map snd contas)) -- Calcular o rendimento de todos os saldos das contas

  print (saldos) -- Imprimir a lista de saldos

  print (filter ehDeposito extrato) -- Filtrar apenas os depósitos do extrato

  print (filter ehSaque extrato) -- Filtrar apenas os saques do extrato

  print (map tarifaSimples saldos) -- Aplicar a tarifa simples de 5 em todos os saldos

  print (map tarifaSimples (filter podePagarTarifa saldos)) -- Aplicar a tarifa simples de 5 apenas nos saldos que podem pagar a tarifa
  
  print (map fst (filter clienteVip contas)) -- Obter os nomes dos clientes VIP

  print (aplicarJuros 10 saldos) -- Aplicar juros de 10% a todos os saldos

  print (saquesAcimaDe 100 extrato) -- Filtrar saques acima de 100

  print (map (depositar 100) saldos) -- Depositar 100 em todos os saldos

  print (map (sacar 100) saldos) -- Sacar 100 de todos os saldos

  print (map (+ 100) saldos) -- Adicionar 100 a todos os saldos

  print (map (\saldo -> saldo + saldo `div` 100) saldos) -- Calcular o rendimento de todos os saldos

  print (map rendimento saldos) -- Calcular o rendimento de todos os saldos usando a função rendimento

  print (foldr (+) 0 saldos) -- Somar todos os saldos