module Root.Exercicios.UnBCare where

import Root.Modelo.ModeloDados

{-
 

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med qtd estoque =
   if existe med estoque
   then atualizar med qtd estoque
   else (med, qtd) : estoque

   where
      -- verifica se o medicamento existe
      existe :: Medicamento -> EstoqueMedicamentos -> Bool
      existe _ [] = False
      existe m  ((nome, _):resto)
         | m == nome = True
         | otherwise = existe m resto
      
      -- se existe, atualiza percorrendo a lista
      atualizar :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
      atualizar _ _ [] = []
      atualizar m q ((nome, quant):resto)
         | m == nome = (nome, quant + q) : resto
         | otherwise = (nome, quant) : atualizar m q resto




{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento med estoque =
   if existe med estoque
   then
      if consultarMedicamento med estoque > 0 
      then Just (diminuir med estoque)
      else Nothing
   else Nothing

   where
      -- verifica se o medicamento existe
      existe :: Medicamento -> EstoqueMedicamentos -> Bool
      existe _ [] = False
      existe m  ((nome, _):resto)
         | m == nome = True
         | otherwise = existe m resto
      -- se existe, percorre diminuindo 1 da quant
      diminuir :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
      diminuir _ [] = []
      diminuir m ((nome, quant):resto)
         | m == nome = (nome, quant - 1) : resto
         | otherwise = (nome, quant) : diminuir m resto



{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento m ((nome, quant):resto)
   | m == nome = quant
   | otherwise = consultarMedicamento m resto



{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos ((nome,horarios):resto) =
   insere (nome, length horarios) (demandaMedicamentos resto)

   where
      insere :: (Medicamento, Quantidade) -> EstoqueMedicamentos -> EstoqueMedicamentos
      insere novo [] = [novo]
      insere (nomeNovo, quantNova) ((nomeAtual,quantAtual):restoEstoque)
         | nomeNovo <= nomeAtual = (nomeNovo, quantNova) : (nomeAtual, quantAtual) : restoEstoque
         | otherwise = (nomeAtual, quantAtual) : insere (nomeNovo, quantNova) restoEstoque


{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

receituarioValido :: Receituario -> Bool
receituarioValido receituario = validarRec receituario
  where
    validarRec :: Receituario -> Bool
    validarRec [] = True
    validarRec [(med, horas)] = hValido horas
    validarRec ((med1, horas1):(med2, horas2):resto) =
       if med1 < med2 && hValido horas1
       then validarRec ((med2, horas2):resto)
       else False

    hValido :: [Horario] -> Bool
    hValido [] = True
    hValido [_] = True
    hValido (h1:h2:restoH) =
       if h1 < h2
       then hValido (h2:restoH)
       else False


planoValido :: PlanoMedicamento -> Bool
planoValido plano = validarPlano plano
  where
    validarPlano :: PlanoMedicamento -> Bool
    validarPlano [] = True
    validarPlano [(hora, meds)] = mValido meds
    validarPlano ((hora1, meds1):(hora2, meds2):resto) =
       if hora1 < hora2 && mValido meds1
       then validarPlano ((hora2, meds2):resto)
       else False

    mValido :: [Medicamento] -> Bool
    mValido [] = True
    mValido [_] = True
    mValido (m1:m2:restoM) =
       if m1 < m2 
       then mValido (m2:restoM)
       else False


{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

plantaoValido :: Plantao -> Bool
plantaoValido plantao = validarPlantao plantao
   where
      -- Cond 1: verifica se horários estão crescentes

      validarPlantao :: Plantao -> Bool
      validarPlantao [] = True
      validarPlantao [(h, cuidados)] = cValido cuidados
      validarPlantao ((h1, c1):(h2, c2):resto) =
         if h1 < h2 && cValido c1
         then validarPlantao ((h2, c2):resto)
         else False

      -- Cond 2: compra e medicagem do mesmo medicamento

      sConflito :: [Medicamento] -> [Medicamento] -> Bool
      sConflito [] _ = True
      sConflito (m:restoM) compras =
         if existe m compras
         then False
         else sConflito restoM compras
      
      existe :: Medicamento -> [Medicamento] -> Bool
      existe _ [] = False
      existe m (x:xs)
         | m == x = True
         | otherwise = existe m xs 

      -- Cond 3: medicar ordenado lexicograficamente

      mOrdenados :: [Medicamento] -> Bool
      mOrdenados [] = True
      mOrdenados [_] = True
      mOrdenados (m1:m2:resto) =
         if m1 <= m2
         then mOrdenados (m2:resto)
         else False
      
      -- auxiliares
      cValido :: [Cuidado] -> Bool
      cValido cuidados =
         mOrdenados (medicar cuidados) && sConflito (medicar cuidados) (comprar cuidados)  

      medicar :: [Cuidado] -> [Medicamento]
      medicar [] = []
      medicar (Medicar m : resto) = m : medicar resto
      medicar (Comprar _ _ : resto) = medicar resto

      comprar :: [Cuidado] -> [Medicamento]
      comprar [] = []
      comprar (Comprar m _ : resto) = m : comprar resto
      comprar (Medicar _ : resto) = comprar resto     
      


{- QUESTÃO 7 VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario ((med, horarios):resto) = addHorarios med horarios (geraPlanoReceituario resto)
   where
      addHorarios :: Medicamento -> [Horario] -> PlanoMedicamento -> PlanoMedicamento
      addHorarios _ [] plano = plano
      addHorarios m (h:hs) plano = addHorario h m (addHorarios m hs plano)

      addHorario :: Horario -> Medicamento -> PlanoMedicamento -> PlanoMedicamento
      addHorario h m [] = [(h, [m])]
      addHorario h m ((h2, meds):restoP)
         | h == h2 = (h, inserirMed m meds) : restoP
         | h < h2 = (h, [m]) : (h2, meds) : restoP
         | otherwise = (h2, meds) : addHorario h m restoP

      -- auxiliar
      inserirMed :: Medicamento -> [Medicamento] -> [Medicamento]
      inserirMed m [] = [m]
      inserirMed m (x:xs)
         | m <= x = m : x : xs
         | otherwise = x : inserirMed m xs



{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano [] = []
geraReceituarioPlano ((horario, meds):resto) = addMeds horario meds (geraReceituarioPlano resto)
   where
      addMeds :: Horario -> [Medicamento] -> Receituario -> Receituario
      addMeds _ [] rec = rec
      addMeds h (m:ms) rec = addMed m h (addMeds h ms rec)

      addMed :: Medicamento -> Horario -> Receituario -> Receituario
      addMed m h [] = [(m, [h])]
      addMed m h ((m2, horas):restoR)
         | m == m2 = (m, inserirHorario h horas) : restoR 
         | m < m2 = (m, [h]) : (m2, horas) : restoR
         | otherwise = (m2, horas) : addMed m h restoR
      
      -- auxiliar
      inserirHorario :: Horario -> [Horario] -> [Horario]
      inserirHorario h [] = [h]
      inserirHorario h (x:xs) 
         | h <= x = h : x : xs
         | otherwise = x : inserirHorario h xs

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] estoque = Just estoque
executaPlantao ((hora, cuidados):resto) estoque =
    case executaCuidados cuidados estoque of
        Nothing -> Nothing
        Just novoEstoque ->
            executaPlantao resto novoEstoque

  where
    executaCuidados [] estoque = Just estoque
    
    executaCuidados (Medicar med:resto) estoque =
        case tomarMedicamento med estoque of
            Nothing -> Nothing
            Just novoEstoque ->
                executaCuidados resto novoEstoque

    executaCuidados (Comprar med qtd:resto) estoque =
        executaCuidados resto (comprar med qtd estoque)

    comprar med qtd [] = [(med, qtd)]
    comprar med qtd ((nome, quant):resto)
        | med == nome = (nome, quant + qtd) : resto
        | med < nome = (med, qtd) : (nome, quant) : resto 
        | otherwise = (nome, quant) : comprar med qtd resto

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plantao plano estoque =
   case executaPlantao plantao estoque of
      Nothing -> False
      Just _ -> planoDoPlantao plantao == plano

   where
      planoDoPlantao [] = []

      planoDoPlantao ((hora, cuidados):resto) =
         case medicações cuidados of
            [] -> planoDoPlantao resto
            meds -> (hora, meds) : planoDoPlantao resto

      medicações [] = []

      medicações (Medicar med:resto) =
         med : medicações resto

      medicações (Comprar _ _:resto) =
         medicações resto

{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto [] estoque = []

plantaoCorreto plano estoque =
   if null compras
   then medicamentos plano
   else (primeiroHorario plano - 1, compras) : medicamentos plano

   where
      receitas = geraReceituarioPlano plano
      demanda = demandaMedicamentos receitas
      compras = comprarNecessario demanda estoque
      comprarNecessario [] estoque = []
      comprarNecessario ((med, qtd):resto) estoque =
         if qtd > consultarMedicamento med estoque
         then Comprar med (qtd - consultarMedicamento med estoque) : comprarNecessario resto estoque
         else comprarNecessario resto estoque

      medicamentos [] = []
      medicamentos ((hora, meds):resto) = (hora, transforma meds) : medicamentos resto

      transforma [] = []
      transforma (med:resto) = Medicar med : transforma resto
      
      primeiroHorario ((hora, _):_) = hora
