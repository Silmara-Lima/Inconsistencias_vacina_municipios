install.packages("tidyverse")
install.packages("dplyr")
install.packages("plyr")
install.packages("openxlsx")
install.packages("writexl")
install.packages("readxl")
install.packages("stringr")
library("writexl")
library(readxl)
library("dplyr")
library("plyr")
library("openxlsx")
library("stringr")

RemoveAcentos <- function(textoComAcentos) {
  if(!is.character(textoComAcentos)){
    on.exit()
  }
  letrasComAcentos <- "??????????????????????????????????????????????????Ǵ`^~?"
  letrasSemAcentos <- "aeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC     "
  textoSemAcentos <- chartr(
    old = letrasComAcentos,
    new = letrasSemAcentos,
    x = textoComAcentos
  ) 
  return(textoSemAcentos)
}

#abrir arquivos
data_astrazeneca0 <- readxl::read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados astrazeneca.xlsx", sheet= "0 a 4 anos")
data_astrazeneca1 <- readxl::read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados astrazeneca.xlsx", sheet= "5 a 11 anos")
data_astrazeneca2 <- readxl::read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados astrazeneca.xlsx", sheet= "12 a 17 anos")
data_janssen0 <- read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados janssen.xlsx", sheet= "0 a 4 anos")
data_janssen1 <- read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados janssen.xlsx", sheet= "5 a 11 anos")
data_janssen2 <- read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados janssen.xlsx", sheet= "12 a 17 anos")
data_pfizer0 <- read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados pfizer.xlsx", sheet= "0 a 4 anos")
data_pfizer1 <- read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados pfizer.xlsx", sheet= "5 a 11 anos")
data_pfizer2 <- read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados pfizer.xlsx", sheet= "12 a 17 anos")
data_butantan0 <- read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados butantan.xlsx", sheet= "0 a 4 anos")
data_butantan1 <- read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados butantan.xlsx", sheet= "5 a 11 anos")
data_butantan2 <- read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/dados butantan.xlsx", sheet= "12 a 17 anos")

#inserir nome do laboratório
lab_ast <- data.frame("astrazeneca")
lab_jan <- data.frame("janssen")
lab_pfi <- data.frame("pfizer")
lab_but <- data.frame("butantan")


#inserir coluna de laboratório
data_astrazeneca0 <- cbind(data_astrazeneca0, lab_ast)
data_astrazeneca1 <- cbind(data_astrazeneca1, lab_ast)
data_astrazeneca2 <- cbind(data_astrazeneca2, lab_ast)
data_astrazeneca <- bind_rows(data_astrazeneca0, data_astrazeneca1, data_astrazeneca2 )

data_janssen0 <- cbind(data_janssen0, lab_jan)
data_janssen1 <- cbind(data_janssen1, lab_jan)
data_janssen2 <- cbind(data_janssen2, lab_jan)
data_janssen <- bind_rows(data_janssen0, data_janssen1, data_janssen2 )

data_butantan0 <- cbind(data_butantan0, lab_jan)
data_butantan1 <- cbind(data_butantan1, lab_jan)
data_butantan2 <- cbind(data_butantan2, lab_jan)
data_butantan <- ifelse(data_butantan0)



(data_butantatan0 = Not found & 
                     data_butantatan1 >= 1 & 
                     data_butantatan2 >= 1) {
    return(bind_rows(data_butantan0, data_butantan1, data_butantan2 ))
  
} else {(data_butantatan0 < 1 & 
    data_butantatan1 < 1 & 
    data_butantatan2 < 1)
  return(bind_rows(data_butantan0, data_butantan1, data_butantan2 ))
  
}
    
  

data_pfizer0 <- cbind(data_pfizer0, lab_jan)
data_pfizer1 <- cbind(data_pfizer1, lab_jan)
data_pfizer2 <- cbind(data_pfizer2, lab_jan)
data_pfizer <- bind_rows(data_pfizer0, data_pfizer1, data_pfizer2 )






#renomear dataframe
MAEDAGUA <- data.frame(`MAE D'AGUA`)
OLHODAGUA <- data.frame(`OLHO D'AGUA`)

#remover acentos
RemoveAcentos(data_excel$estabelecimento_municipio_nome)
remove

#remover caracteres especiais
str_replace_all(data_excel$estabelecimento_municipio_nome, "[^[:alnum:]]", " ")

#separar por lista
split_list <- split(data_excel, data_excel$estabelecimento_municipio_nome)

#remover apostrofo
municipios_vac <- gsub(" '' ", "",names(split_list), fixed = TRUE)

#executar as listas por municipios
for(i in seq_along(municipios_vac)){
  assign(municipios_vac[i], split_list[[i]])
}


list_of_datasets <- list ("AGUA BRANCA" = AGUABRANCA,
                          "AGUIAR" = AGUIAR,
                          "Alagoa Grande" = ALAGOAGRANDE, 
                          "Alagoa Nova" = ALAGOANOVA, 
                          "Alagoinha" = ALAGOINHA, 
                          "Alcantil" = ALCANTIL, 
                          "Algodao De Jandaira" = ALGODAODEJANDAIRA, 
                          "Alhandra" = ALHANDRA, 
                          "Amparo" = AMPARO, 
                          "Aparecida" = APARECIDA, 
                          "Aracagi" = ARACAGI, 
                          "Arara" = ARARA, 
                          "Araruna" = ARARUNA, 
                          "Areia" = AREIA, 
                          "Areia De Baraunas" = AREIADEBARAUNAS, 
                          "Areial" = AREIAL, 
                          "Aroeiras" = AROEIRAS, 
                          "Assuncao" = ASSUNCAO, 
                          "Baia Da Traicao" = BAIADATRAICAO, 
                          "Bananeiras" = BANANEIRAS, 
                          "Barauna" = BARAUNA, 
                          "Barra De Santa Rosa" = BARRADESANTAROSA, 
                          "Barra De Santana" = BARRADESANTANA, 
                          "Barra De Sao Miguel" = BARRADESAOMIGUEL, 
                          "Bayeux" = BAYEUX, 
                          "Belem" = BELEM, 
                          "Belem Do Brejo Do Cruz" = BELEMDOBREJODOCRUZ, 
                          "Bernardino Batista" = BERNARDINOBATISTA, 
                          "Boa Ventura" = BOAVENTURA, 
                          "Boa Vista" = BOAVISTA, 
                          "Bom Jesus" = BOMJESUS, 
                          "Bom Sucesso" = BOMSUCESSO, 
                          "Bonito De Santa Fe" = BONITODESANTAFE, 
                          "Boqueirao" = BOQUEIRAO, 
                          "Borborema" = BORBOREMA, 
                          "Brejo Do Cruz" = BREJODOCRUZ, 
                          "Brejo Dos Santos" = BREJODOSSANTOS, 
                          "Caapora" = CAAPORA, 
                          "Cabaceiras" = CABACEIRAS, 
                          "Cabedelo" = CABEDELO, 
                          "Cachoeira Dos Indios" = CACHOEIRADOSINDIOS, 
                          "Cacimba De Areia" = CACIMBADEAREIA, 
                          "Cacimba De Dentro" = CACIMBADEDENTRO, 
                          "Cacimbas" = CACIMBAS, 
                          "Caicara" = CAICARA, 
                          "Cajazeiras" = CAJAZEIRAS, 
                          "Cajazeirinhas" = CAJAZEIRINHAS, 
                          "Caldas Brandao" = CALDASBRANDAO, 
                          "Camalau" = CAMALAU, 
                          "Campina Grande" = CAMPINAGRANDE, 
                          "Capim" = CAPIM, 
                          "Caraubas" = CARAUBAS, 
                          "Carrapateira" = CARRAPATEIRA, 
                          "Casserengue" = CASSERENGUE, 
                          "Catingueira" = CATINGUEIRA, 
                          "Catole Do Rocha" = CATOLEDOROCHA, 
                          "Caturite" = CATURITE, 
                          "Conceicao" = CONCEICAO, 
                          "Condado" = CONDADO, 
                          "Conde" = CONDE, 
                          "Congo" = CONGO, 
                          "Coremas" = COREMAS, 
                          "Coxixola" = COXIXOLA, 
                          "Cruz Do Espirito Santo" = CRUZDOESPIRITOSANTO, 
                          "Cubati" = CUBATI, 
                          "Cuite" = CUITE, 
                          "Cuite De Mamanguape" = CUITEDEMAMANGUAPE, 
                          "Cuitegi" = CUITEGI, 
                          "Curral De Cima" = CURRALDECIMA, 
                          "Curral Velho" = CURRALVELHO, 
                          "Damiao" = DAMIAO, 
                          "Desterro" = DESTERRO, 
                          "Diamante" = DIAMANTE, 
                          "Dona Ines" = DONAINES, 
                          "Duas Estradas" = DUASESTRADAS, 
                          "Emas" = EMAS, 
                          "Esperanca" = ESPERANCA, 
                          "Fagundes" = FAGUNDES, 
                          "Frei Martinho" = FREIMARTINHO,
                          "Gado Bravo" = GADOBRAVO, 
                          "Guarabira" = GUARABIRA, 
                          "Gurinhem" = GURINHEM, 
                          "Gurjao" = GURJAO, 
                          "Ibiara" = IBIARA, 
                          "Igaracy" = IGARACY, 
                          "Imaculada" = IMACULADA, 
                          "Inga" = INGA, 
                          "Itabaiana" = ITABAIANA, 
                          "Itaporanga" = ITAPORANGA, 
                          "Itapororoca" = ITAPOROROCA, 
                          "Itatuba" = ITATUBA, 
                          "Jacarau" = JACARAU, 
                          "Jerico" = JERICO, 
                          "Joao Pessoa" = JOAOPESSOA, 
                          "Joca Claudino" = JOCACLAUDINO, 
                          "Juarez Tavora" = JUAREZTAVORA, 
                          "Juazeirinho" = JUAZEIRINHO, 
                          "Junco Do Serido" = JUNCODOSERIDO, 
                          "Juripiranga" = JURIPIRANGA, 
                          "Juru" = JURU, 
                          "Lagoa" = LAGOA, 
                          "Lagoa De Dentro" = LAGOADEDENTRO, 
                          "Lagoa Seca" = LAGOASECA, 
                          "Lastro" = LASTRO, 
                          "Livramento" = LIVRAMENTO, 
                          "Logradouro" = LOGRADOURO, 
                          "Lucena" = LUCENA, 
                          "Mae D'Agua" = MAEDAGUA, 
                          "Malta" = MALTA, 
                          "Mamanguape" = MAMANGUAPE, 
                          "Manaira" = MANAIRA, 
                          "Marcacao" = MARCACAO, 
                          "Mari" = MARI, 
                          "Marizopolis" = MARIZOPOLIS, 
                          "Massaranduba" = MASSARANDUBA, 
                          "Mataraca" = MATARACA, 
                          "Matinhas" = MATINHAS, 
                          "Mato Grosso" = MATOGROSSO, 
                          "Matureia" = MATUREIA, 
                          "Mogeiro" = MOGEIRO, 
                          "Montadas" = MONTADAS, 
                          "Monte Horebe" = MONTEHOREBE, 
                          "Monteiro" = MONTEIRO, 
                          "Mulungu" = MULUNGU, 
                          "Natuba" = NATUBA, 
                          "Nazarezinho" = NAZAREZINHO, 
                          "Nova Floresta" = NOVAFLORESTA, 
                          "Nova Olinda" = NOVAOLINDA, 
                          "Nova Palmeira" = NOVAPALMEIRA, 
                          "Olho D'Agua" = OLHODAGUA, 
                          "Olivedos" = OLIVEDOS, 
                          "Ouro Velho" = OUROVELHO, 
                          "Parari" = PARARI, 
                          "Passagem" = PASSAGEM, 
                          "Patos" = PATOS, 
                          "Paulista" = PAULISTA, 
                          "Pedra Branca" = PEDRABRANCA, 
                          "Pedra Lavrada" = PEDRALAVRADA, 
                          "Pedras De Fogo" = PEDRASDEFOGO, 
                          "Pedro Regis" = PEDROREGIS, 
                          "Pianco" = PIANCO, 
                          "Picui" = PICUI, 
                          "Piloes" = PILOES, 
                          "Piloezinhos" = PILOEZINHOS, 
                          "Pirpirituba" = PIRPIRITUBA, 
                          "Pitimbu" = PITIMBU, 
                          "Pocinhos" = POCINHOS, 
                          "Poco Dantas" = POCODANTAS, 
                          "Poco De Jose De Moura" = POCODEJOSEDEMOURA, 
                          "Pombal" = POMBAL, 
                          "Prata" = PRATA, 
                          "Princesa Isabel" = PRINCESAISABEL, 
                          "Puxinana" = PUXINANA, 
                          "Queimadas" = QUEIMADAS, 
                          "Quixaba" = QUIXABA, 
                          "Remigio" = REMIGIO, 
                          "Riachao" = RIACHAO, 
                          "Riachao Do Bacamarte" = RIACHAODOBACAMARTE, 
                          "Riachao Do Poco" = RIACHAODOPOCO, 
                          "Riacho De Santo Antonio" = RIACHODESANTOANTONIO, 
                          "Riacho Dos Cavalos" = RIACHODOSCAVALOS, 
                          "Rio Tinto" = RIOTINTO, 
                          "Salgadinho" = SALGADINHO, 
                          "Salgado De Sao Felix" = SALGADODESAOFELIX, 
                          "Santa Cecilia" = SANTACECILIA, 
                          "Santa Cruz" = SANTACRUZ, 
                          "Santa Helena" = SANTAHELENA, 
                          "Santa Ines" = SANTAINES, 
                          "Santa Luzia" = SANTALUZIA, 
                          "Santa Rita" = SANTARITA, 
                          "Santa Teresinha" = SANTATERESINHA, 
                          "Santana De Mangueira" = SANTANADEMANGUEIRA, 
                          "Santana Dos Garrotes" = SANTANADOSGARROTES, 
                          "Santo Andre" = SANTOANDRE, 
                          "Sao Bentinho" = SAOBENTINHO, 
                          "Sao Bento" = SAOBENTO, 
                          "Sao Domingos" = SAODOMINGOS, 
                          "Sao Domingos Do Cariri" = SAODOMINGOSDOCARIRI, 
                          "Sao Francisco" = SAOFRANCISCO, 
                          "Sao Joao Do Cariri" = SAOJOAODOCARIRI, 
                          "Sao Joao Do Rio Do Peixe" = SAOJOAODORIODOPEIXE, 
                          "Sao Joao Do Tigre" = SAOJOAODOTIGRE, 
                          "Sao Jose Da Lagoa Tapada" = SAOJOSEDALAGOATAPADA, 
                          "Sao Jose De Caiana" = SAOJOSEDECAIANA, 
                          "Sao Jose De Espinharas" = SAOJOSEDEESPINHARAS, 
                          "Sao Jose De Piranhas" = SAOJOSEDEPIRANHAS, 
                          "Sao Jose De Princesa" = SAOJOSEDEPRINCESA, 
                          "Sao Jose Do Bonfim" = SAOJOSEDOBONFIM, 
                          "Sao Jose Do Brejo Do Cruz" = SAOJOSEDOBREJODOCRUZ, 
                          "Sao Jose Do Sabugi" = SAOJOSEDOSABUGI, 
                          "Sao Jose Dos Cordeiros" = SAOJOSEDOSCORDEIROS, 
                          "Sao Jose Dos Ramos" = SAOJOSEDOSRAMOS, 
                          "Sao Mamede" = SAOMAMEDE, 
                          "Sao Miguel De Taipu" = SAOMIGUELDETAIPU, 
                          "Sao Sebastiao De Lagoa De Roca" = SAOSEBASTIAODELAGOADEROCA, 
                          "Sao Sebastiao Do Umbuzeiro" = SAOSEBASTIAODOUMBUZEIRO, 
                          "Sao Vicente do Serido" = SAOVICENTEDOSERIDO, 
                          "Sape" = SAPE, 
                          "Serra Branca" = SERRABRANCA, 
                          "Serra Da Raiz" = SERRADARAIZ, 
                          "Serra Grande" = SERRAGRANDE, 
                          "Serra Redonda" = SERRAREDONDA, 
                          "Serraria" = SERRARIA, 
                          "Sertaozinho" = SERTAOZINHO, 
                          "Sobrado" = SOBRADO, 
                          "Solanea" = SOLANEA, 
                          "Soledade" = SOLEDADE, 
                          "Sossego" = SOSSEGO, 
                          "Sousa" = SOUSA, 
                          "Sume" = SUME, 
                          "Tacima" = TACIMA, 
                          "Taperoa" = TAPEROA, 
                          "Tavares" = TAVARES, 
                          "Teixeira" = TEIXEIRA, 
                          "Tenorio" = TENORIO, 
                          "Triunfo" = TRIUNFO, 
                          "Uirauna" = UIRAUNA, 
                          "Umbuzeiro" = UMBUZEIRO, 
                          "Varzea" = VARZEA, 
                          "Vieiropolis" = VIEIROPOLIS, 
                          "Vista Serrana" = VISTASERRANA, 
                          "Zabele" = ZABELE, 
)

write.xlsx(list_of_datasets, file = "C:/Users/ADM/Dropbox/PC/Desktop/Influenza/Projeto teste_srag_gal/saidas_imunizacao/mun_vac.xlsx")

