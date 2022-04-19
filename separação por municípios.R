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
data_excel <- readxl::read_excel("C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/entradas/data_19_04_2022.xlsx")

#ordenar
#data_excel <- arrange(data_excel, data_excel$Idade)
data_excel <- data_excel %>% 
              filter (data_excel$idade >= 5 & data_excel$idade <= 11)


#remover acentos
RemoveAcentos(data_excel$estabelecimento_municipio_nome)

#remover caracteres especiais
str_replace_all(data_excel$estabelecimento_municipio_nome, "[^[:alnum:]]", " ")

#separar por lista
split_list <- split(data_excel, data_excel$Municipio)

#remover apostrofo
municipios_vac <- gsub(" ", "",names(split_list), fixed = TRUE)

#executar as listas por municipios
for(i in seq_along(municipios_vac)){
  assign(municipios_vac[i], split_list[[i]])
}

#renomear dataframe
MAEDAGUA <- data.frame("MAE D'AGUA")
OLHODAGUA <- data.frame("OLHO D'AGUA")


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
                        

write.xlsx(split_list, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/municipios1.xlsx")
write.xlsx(AGUABRANCA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/AGUABRANCA.xlsx")
write.xlsx(AGUIAR, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/AGUIAR.xlsx")
write.xlsx(ALAGOAGRANDE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ALAGOAGRANDE.xlsx")
write.xlsx(ALAGOANOVA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ALAGOANOVA.xlsx")
write.xlsx(ALAGOINHA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ALAGOINHA.xlsx")
write.xlsx(ALCANTIL, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ALCANTIL.xlsx")
write.xlsx(ALGODAODEJANDAIRA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ALGODAODEJANDAIRA.xlsx")
write.xlsx(ALHANDRA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ALHANDRA.xlsx")
write.xlsx(AMPARO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/AMPARO.xlsx")
write.xlsx(APARECIDA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/APARECIDA.xlsx")
write.xlsx(ARACAGI, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ARACAGI.xlsx")
write.xlsx(ARARA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ARARA.xlsx")
write.xlsx(ARARUNA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ARARUNA.xlsx")
write.xlsx(AREIA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/AREIA.xlsx")
write.xlsx(AREIADEBARAUNAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/AREIADEBARAUNAS.xlsx")
write.xlsx(AREIAL, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/AREIAL.xlsx")
write.xlsx(AROEIRAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/AROEIRAS.xlsx")
write.xlsx(ASSUNCAO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ASSUNCAO.xlsx")
write.xlsx(BAIADATRAICAO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BAIADATRAICAO.xlsx")
write.xlsx(BANANEIRAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BANANEIRAS.xlsx")
write.xlsx(BARAUNA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BARAUNA.xlsx")
write.xlsx(BARRADESANTAROSA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BARRADESANTAROSA.xlsx")
write.xlsx(BARRADESANTANA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BARRADESANTANA.xlsx")
write.xlsx(BARRADESAOMIGUEL, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BARRADESAOMIGUEL.xlsx")
write.xlsx(BAYEUX, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BAYEUX.xlsx")
write.xlsx(BELEM, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BELEM.xlsx")
write.xlsx(BELEMDOBREJODOCRUZ, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BELEMDOBREJODOCRUZ.xlsx")
write.xlsx(BERNARDINOBATISTA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BERNARDINOBATISTA.xlsx")
write.xlsx(BOAVENTURA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BOAVENTURA.xlsx")
write.xlsx(BOAVISTA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BOAVISTA.xlsx")
write.xlsx(BOMJESUS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BOMJESUS.xlsx")
write.xlsx(BOMSUCESSO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BOMSUCESSO.xlsx")
write.xlsx(BONITODESANTAFE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BONITODESANTAFE.xlsx")
write.xlsx(BOQUEIRAO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BOQUEIRAO.xlsx")
write.xlsx(BORBOREMA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BORBOREMA.xlsx")
write.xlsx(BREJODOCRUZ, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BREJODOCRUZ.xlsx")
write.xlsx(BREJODOSSANTOS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/BREJODOSSANTOS.xlsx")
write.xlsx(CAAPORA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CAAPORA.xlsx")
write.xlsx(CABACEIRAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CABACEIRAS.xlsx")
write.xlsx(CABEDELO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CABEDELO.xlsx")
write.xlsx(CACHOEIRADOSINDIOS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CACHOEIRADOSINDIOS.xlsx")
write.xlsx(CACIMBADEAREIA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CACIMBADEAREIA.xlsx")
write.xlsx(CACIMBADEDENTRO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CACIMBADEDENTRO.xlsx")
write.xlsx(CACIMBAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CACIMBAS.xlsx")
write.xlsx(CAICARA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CAICARA.xlsx")
write.xlsx(CAJAZEIRAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CAJAZEIRAS.xlsx")
write.xlsx(CAJAZEIRINHAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CAJAZEIRINHAS.xlsx")
write.xlsx(CALDASBRANDAO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CALDASBRANDAO.xlsx")
write.xlsx(CAMALAU, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CAMALAU.xlsx")
write.xlsx(CAMPINAGRANDE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CAMPINAGRANDE.xlsx")
write.xlsx(CAPIM, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CAPIM.xlsx")
write.xlsx(CARAUBAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CARAUBAS.xlsx")
write.xlsx(CARRAPATEIRA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CARRAPATEIRA.xlsx")
write.xlsx(CASSERENGUE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CASSERENGUE.xlsx")
write.xlsx(CATINGUEIRA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CATINGUEIRA.xlsx")
write.xlsx(CATOLEDOROCHA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CATOLEDOROCHA.xlsx")
write.xlsx(CATURITE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CATURITE.xlsx")
write.xlsx(CONCEICAO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CONCEICAO.xlsx")
write.xlsx(CONDADO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CONDADO.xlsx")
write.xlsx(CONDE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CONDE.xlsx")
write.xlsx(CONGO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CONGO.xlsx")
write.xlsx(COREMAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/COREMAS.xlsx")
write.xlsx(COXIXOLA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/COXIXOLA.xlsx")
write.xlsx(CRUZDOESPIRITOSANTO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CRUZDOESPIRITOSANTO.xlsx")
write.xlsx(CUBATI, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CUBATI.xlsx")
write.xlsx(CUITE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CUITE.xlsx")
write.xlsx(CUITEDEMAMANGUAPE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CUITEDEMAMANGUAPE.xlsx")
write.xlsx(CUITEGI, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CUITEGI.xlsx")
write.xlsx(CURRALDECIMA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CURRALDECIMA.xlsx")
write.xlsx(CURRALVELHO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/CURRALVELHO.xlsx")
write.xlsx(DAMIAO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/DAMIAO.xlsx")
write.xlsx(DESTERRO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/DESTERRO.xlsx")
write.xlsx(DIAMANTE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/DIAMANTE.xlsx")
write.xlsx(DONAINES, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/DONAINES.xlsx")
write.xlsx(DUASESTRADAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/DUASESTRADAS.xlsx")
write.xlsx(EMAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/EMAS.xlsx")
write.xlsx(ESPERANCA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ESPERANCA.xlsx")
write.xlsx(FAGUNDES, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/FAGUNDES.xlsx")
write.xlsx(FREIMARTINHO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/FREIMARTINHO.xlsx")
write.xlsx(GADOBRAVO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/GADOBRAVO.xlsx")
write.xlsx(GUARABIRA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/GUARABIRA.xlsx")
write.xlsx(GURINHEM, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/GURINHEM.xlsx")
write.xlsx(GURJAO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/GURJAO.xlsx")
write.xlsx(IBIARA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/IBIARA.xlsx")
write.xlsx(IGARACY, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/IGARACY.xlsx")
write.xlsx(IMACULADA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/IMACULADA.xlsx")
write.xlsx(INGA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/INGA.xlsx")
write.xlsx(ITABAIANA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ITABAIANA.xlsx")
write.xlsx(ITAPORANGA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ITAPORANGA.xlsx")
write.xlsx(ITAPOROROCA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ITAPOROROCA.xlsx")
write.xlsx(ITATUBA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ITATUBA.xlsx")
write.xlsx(JACARAU, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/JACARAU.xlsx")
write.xlsx(JERICO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/JERICO.xlsx")
write.xlsx(JOAOPESSOA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/JOAOPESSOA.xlsx")
write.xlsx(JOCACLAUDINO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/JOCACLAUDINO.xlsx")
write.xlsx(JUAREZTAVORA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/JUAREZTAVORA.xlsx")
write.xlsx(JUAZEIRINHO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/JUAZEIRINHO.xlsx")
write.xlsx(JUNCODOSERIDO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/JUNCODOSERIDO.xlsx")
write.xlsx(JURIPIRANGA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/JURIPIRANGA.xlsx")
write.xlsx(JURU, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/JURU.xlsx")
write.xlsx(LAGOA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/LAGOA.xlsx")
write.xlsx(LAGOADEDENTRO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/LAGOADEDENTRO.xlsx")
write.xlsx(LAGOASECA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/LAGOASECA.xlsx")
write.xlsx(LASTRO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/LASTRO.xlsx")
write.xlsx(LIVRAMENTO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/LIVRAMENTO.xlsx")
write.xlsx(LOGRADOURO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/LOGRADOURO.xlsx")
write.xlsx(LUCENA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/LUCENA.xlsx")
write.xlsx(MAEDAGUA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MAED'AGUA.xlsx")
write.xlsx(MALTA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MALTA.xlsx")
write.xlsx(MAMANGUAPE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MAMANGUAPE.xlsx")
write.xlsx(MANAIRA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MANAIRA.xlsx")
write.xlsx(MARCACAO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MARCACAO.xlsx")
write.xlsx(MARI, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MARI.xlsx")
write.xlsx(MARIZOPOLIS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MARIZOPOLIS.xlsx")
write.xlsx(MASSARANDUBA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MASSARANDUBA.xlsx")
write.xlsx(MATARACA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MATARACA.xlsx")
write.xlsx(MATINHAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MATINHAS.xlsx")
write.xlsx(MATOGROSSO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MATOGROSSO.xlsx")
write.xlsx(MATUREIA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MATUREIA.xlsx")
write.xlsx(MOGEIRO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MOGEIRO.xlsx")
write.xlsx(MONTADAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MONTADAS.xlsx")
write.xlsx(MONTEHOREBE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MONTEHOREBE.xlsx")
write.xlsx(MONTEIRO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MONTEIRO.xlsx")
write.xlsx(MULUNGU, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/MULUNGU.xlsx")
write.xlsx(NATUBA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/NATUBA.xlsx")
write.xlsx(NAZAREZINHO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/NAZAREZINHO.xlsx")
write.xlsx(NOVAFLORESTA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/NOVAFLORESTA.xlsx")
write.xlsx(NOVAOLINDA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/NOVAOLINDA.xlsx")
write.xlsx(NOVAPALMEIRA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/NOVAPALMEIRA.xlsx")
write.xlsx(OLHODAGUA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/OLHOD'AGUA.xlsx")
write.xlsx(OLIVEDOS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/OLIVEDOS.xlsx")
write.xlsx(OUROVELHO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/OUROVELHO.xlsx")
write.xlsx(PARARI, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PARARI.xlsx")
write.xlsx(PASSAGEM, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PASSAGEM.xlsx")
write.xlsx(PATOS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PATOS.xlsx")
write.xlsx(PAULISTA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PAULISTA.xlsx")
write.xlsx(PEDRABRANCA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PEDRABRANCA.xlsx")
write.xlsx(PEDRALAVRADA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PEDRALAVRADA.xlsx")
write.xlsx(PEDRASDEFOGO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PEDRASDEFOGO.xlsx")
write.xlsx(PEDROREGIS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PEDROREGIS.xlsx")
write.xlsx(PIANCO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PIANCO.xlsx")
write.xlsx(PICUI, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PICUI.xlsx")
write.xlsx(PILAR, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PILAR.xlsx")
write.xlsx(PILOES, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PILOES.xlsx")
write.xlsx(PILOEZINHOS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PILOEZINHOS.xlsx")
write.xlsx(PIRPIRITUBA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PIRPIRITUBA.xlsx")
write.xlsx(PITIMBU, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PITIMBU.xlsx")
write.xlsx(POCINHOS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/POCINHOS.xlsx")
write.xlsx(POCODANTAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/POCODANTAS.xlsx")
write.xlsx(POCODEJOSEDEMOURA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/POCODEJOSEDEMOURA.xlsx")
write.xlsx(POMBAL, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/POMBAL.xlsx")
write.xlsx(PRATA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PRATA.xlsx")
write.xlsx(PRINCESAISABEL, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PRINCESAISABEL.xlsx")
write.xlsx(PUXINANA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/PUXINANA.xlsx")
write.xlsx(QUEIMADAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/QUEIMADAS.xlsx")
write.xlsx(QUIXABA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/QUIXABA.xlsx")
write.xlsx(REMIGIO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/REMIGIO.xlsx")
write.xlsx(RIACHAO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/RIACHAO.xlsx")
write.xlsx(RIACHAODOBACAMARTE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/RIACHAODOBACAMARTE.xlsx")
write.xlsx(RIACHAODOPOCO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/RIACHAODOPOCO.xlsx")
write.xlsx(RIACHODESANTOANTONIO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/RIACHODESANTOANTONIO.xlsx")
write.xlsx(RIACHODOSCAVALOS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/RIACHODOSCAVALOS.xlsx")
write.xlsx(RIOTINTO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/RIOTINTO.xlsx")
write.xlsx(SALGADINHO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SALGADINHO.xlsx")
write.xlsx(SALGADODESAOFELIX, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SALGADODESAOFELIX.xlsx")
write.xlsx(SANTACECILIA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SANTACECILIA.xlsx")
write.xlsx(SANTACRUZ, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SANTACRUZ.xlsx")
write.xlsx(SANTAHELENA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SANTAHELENA.xlsx")
write.xlsx(SANTAINES, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SANTAINES.xlsx")
write.xlsx(SANTALUZIA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SANTALUZIA.xlsx")
write.xlsx(SANTARITA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SANTARITA.xlsx")
write.xlsx(SANTATERESINHA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SANTATERESINHA.xlsx")
write.xlsx(SANTANADEMANGUEIRA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SANTANADEMANGUEIRA.xlsx")
write.xlsx(SANTANADOSGARROTES, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SANTANADOSGARROTES.xlsx")
write.xlsx(SANTOANDRE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SANTOANDRE.xlsx")
write.xlsx(SAOBENTINHO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOBENTINHO.xlsx")
write.xlsx(SAOBENTO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOBENTO.xlsx")
write.xlsx(SAODOMINGOS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAODOMINGOS.xlsx")
write.xlsx(SAODOMINGOSDOCARIRI, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAODOMINGOSDOCARIRI.xlsx")
write.xlsx(SAOFRANCISCO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOFRANCISCO.xlsx")
write.xlsx(SAOJOAODOCARIRI, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOAODOCARIRI.xlsx")
write.xlsx(SAOJOAODORIODOPEIXE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOAODORIODOPEIXE.xlsx")
write.xlsx(SAOJOAODOTIGRE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOAODOTIGRE.xlsx")
write.xlsx(SAOJOSEDALAGOATAPADA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOSEDALAGOATAPADA.xlsx")
write.xlsx(SAOJOSEDECAIANA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOSEDECAIANA.xlsx")
write.xlsx(SAOJOSEDEESPINHARAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOSEDEESPINHARAS.xlsx")
write.xlsx(SAOJOSEDEPIRANHAS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOSEDEPIRANHAS.xlsx")
write.xlsx(SAOJOSEDEPRINCESA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOSEDEPRINCESA.xlsx")
write.xlsx(SAOJOSEDOBONFIM, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOSEDOBONFIM.xlsx")
write.xlsx(SAOJOSEDOBREJODOCRUZ, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOSEDOBREJODOCRUZ.xlsx")
write.xlsx(SAOJOSEDOSABUGI, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOSEDOSABUGI.xlsx")
write.xlsx(SAOJOSEDOSCORDEIROS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOSEDOSCORDEIROS.xlsx")
write.xlsx(SAOJOSEDOSRAMOS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOJOSEDOSRAMOS.xlsx")
write.xlsx(SAOMAMEDE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOMAMEDE.xlsx")
write.xlsx(SAOMIGUELDETAIPU, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOMIGUELDETAIPU.xlsx")
write.xlsx(SAOSEBASTIAODELAGOADEROCA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOSEBASTIAODELAGOADEROCA.xlsx")
write.xlsx(SAOSEBASTIAODOUMBUZEIRO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOSEBASTIAODOUMBUZEIRO.xlsx")
write.xlsx(SAOVICENTEDOSERIDO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAOVICENTEDOSERIDO.xlsx")
write.xlsx(SAPE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SAPE.xlsx")
write.xlsx(SERRABRANCA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SERRABRANCA.xlsx")
write.xlsx(SERRADARAIZ, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SERRADARAIZ.xlsx")
write.xlsx(SERRAGRANDE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SERRAGRANDE.xlsx")
write.xlsx(SERRAREDONDA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SERRAREDONDA.xlsx")
write.xlsx(SERRARIA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SERRARIA.xlsx")
write.xlsx(SERTAOZINHO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SERTAOZINHO.xlsx")
write.xlsx(SOBRADO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SOBRADO.xlsx")
write.xlsx(SOLANEA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SOLANEA.xlsx")
write.xlsx(SOLEDADE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SOLEDADE.xlsx")
write.xlsx(SOSSEGO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SOSSEGO.xlsx")
write.xlsx(SOUSA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SOUSA.xlsx")
write.xlsx(SUME, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/SUME.xlsx")
write.xlsx(TACIMA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/TACIMA.xlsx")
write.xlsx(TAPEROA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/TAPEROA.xlsx")
write.xlsx(TAVARES, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/TAVARES.xlsx")
write.xlsx(TEIXEIRA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/TEIXEIRA.xlsx")
write.xlsx(TENORIO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/TENORIO.xlsx")
write.xlsx(TRIUNFO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/TRIUNFO.xlsx")
write.xlsx(UIRAUNA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/UIRAUNA.xlsx")
write.xlsx(UMBUZEIRO, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/UMBUZEIRO.xlsx")
write.xlsx(VARZEA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/VARZEA.xlsx")
write.xlsx(VIEIROPOLIS, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/VIEIROPOLIS.xlsx")
write.xlsx(VISTASERRANA, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/VISTASERRANA.xlsx")
write.xlsx(ZABELE, file = "C:/Users/ADM/Dropbox/PC/Desktop/Scipts by_Silmara/Imunização/saidas/ZABELE.xlsx")
#######
