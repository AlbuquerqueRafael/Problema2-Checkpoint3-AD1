library("dplyr")
library("ggplot2")
library("dplyr")
library("pander")
library("reshape2")

ler_gastos <- function(arquivo = "dados/ano-atual.csv"){
  #' Lê um csv criado a partir dos dados de gastos dos deputados da 
  #' Câmara e seta os tipos de colunas mais convenientemente. 
  #' Versão sem readr, para máquinas onde não é possível instalar esse pacote. 
  #' É um pouco mais lenta que a com readr.
  require("dplyr", warn.conflicts = FALSE)
  
  gastos = read.csv(arquivo, stringsAsFactors = FALSE)
  gastos = gastos %>% 
    mutate_each(funs(as.factor), sgPartido, sgUF, txNomeParlamentar, indTipoDocumento)
  return(gastos)
} 

gastos_ck <- ler_gastos()

gastos_ck <- gastos_ck %>% select (1, 5, 6, 15, 18, 23, 27)

names(gastos_ck) <- c("Partido", "Nome", "UF", "Descricao","Fornecedor", "valorLiquido", "Passageiro")
gastosTotais.ce_ck <- gastos_ck %>% filter( UF == "CE")

gastosPorParlamentar_ck <- select(gastosTotais.ce_ck , valorLiquido, Nome, Partido)
agruparParlamentar_ck <-  group_by(gastosPorParlamentar_ck, Nome, Partido)
gastosPorParlamentar_ck <- summarise(agruparParlamentar_ck, sum(valorLiquido))
colnames(gastosPorParlamentar_ck) <- c("Deputado", "Partido", "Gastos")


lucroFornecedor <- select(gastosTotais.ce_ck, valorLiquido, Fornecedor)
agruparFornecedor <-  group_by(lucroFornecedor, Fornecedor)
lucroPorFornecedor <- summarise(agruparFornecedor, sum(valorLiquido))
colnames(lucroPorFornecedor) <- c("Fornecedor", "Valor.Total")
lucroPorFornecedor <- filter(lucroPorFornecedor, Valor.Total > 50000)

lucroFornecedor_ck <- select(gastosTotais.ce_ck, valorLiquido, Fornecedor, Nome, Partido)
agruparFornecedor_ck <-  group_by(lucroFornecedor_ck , Fornecedor, Nome, Partido)
lucroPorFornecedor_ck <- summarise(agruparFornecedor_ck, sum(valorLiquido))
colnames(lucroPorFornecedor_ck ) <- c("Fornecedor", "Deputado","Partido", "ValorRecebido")

topFornecedores_ck <- subset(lucroPorFornecedor_ck, Fornecedor %in% lucroPorFornecedor$Fornecedor)
topFornecedores_ck <- filter(topFornecedores_ck, ValorRecebido > 0)


topFornecedores_ck <- subset(agruparFornecedor_ck , Fornecedor %in% lucroPorFornecedor_ck$Fornecedor)
topFornecedores_ck <- filter(topFornecedores_ck, ValorRecebido > 0)


gastosTotais <- ler_gastos()
gastosTotais <- gastosTotais %>% select (1, 5, 6, 15, 23, 27)

names(gastosTotais) <- c("Partido", "Nome", "UF", "Descricao","valorLiquido", "Passageiro")

gastosTotais.ce <- gastosTotais %>% filter( UF == "CE")
gastosTotais1.ce <- gastosTotais.ce %>% filter( Nome != Passageiro)
gastosTotais2.ce <- gastosTotais.ce %>% filter( Descricao == "Emissão Bilhete Aéreo")

gastosPassagens1 <- select(gastosTotais1.ce, valorLiquido, Nome, Partido)
agruparGastosPassagens1 <-  group_by(gastosPassagens1, Nome, Partido)
gastosPorPassagens1 <- summarise(agruparGastosPassagens1, sum(valorLiquido))
colnames(gastosPorPassagens1) <- c("Deputado", "Partido", "Valor Gasto com Terceiros")


gastosPassagens2 <- select(gastosTotais2.ce, valorLiquido, Nome, Partido)
agruparGastosPassagens2 <-  group_by(gastosPassagens2, Nome, Partido)
gastosPorPassagens2 <- summarise(agruparGastosPassagens2, sum(valorLiquido))
colnames(gastosPorPassagens2) <- c("Deputado", "Partido", "Valor Total Gasto")

total <- merge(gastosPorPassagens1 ,gastosPorPassagens2, all.x = TRUE)
total.long<-melt(total)

colnames(total.long) <- c("Deputado", "Partido", "Legenda", "Gastos")
total.long$Deputado <- reorder(total.long$Deputado, total.long$Gastos, FUN=identity)


escolhas <<- list("Gastos com Passagens Aereas" = "passagens",
               "Gastos Gerais" = "gerais",
               "Inicio" = "inicio")
