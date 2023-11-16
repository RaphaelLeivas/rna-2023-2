rm(list = ls())
# dev.off()

set.seed(203)

trainReduzidoPath <- "C:\\dev\\rna-2023-2\\trabalhoFinal\\datasets\\trainReduzido.csv"
validacaoPath <- "C:\\dev\\rna-2023-2\\trabalhoFinal\\datasets\\validacao.csv"

trainReduzido <- read.csv(file = trainReduzidoPath, header = T)
validacao <- read.csv(file = validacaoPath, header = T)