require(RMySQL)
require(dplyr)
require(xts)
require(lubridate)
require(BETS)

# Conecta com a base
# @return O objeto serve de input para todas as outras fun??es 
connect <- function(){
  return(dbConnect(MySQL(), db = "gtrends2", host = "179.210.169.125", 
                   user = "root", password = "root", port = 3306))
}

# Desconecta da base
disconnect <- function(conn){
  dbDisconnect(conn)
}

# @return Tabela com os tipos de termos usados para buscar s?ries do GTrends e seus identificadores ?nicos (numericos)
get.term.types <- function(conn){
  return(dbGetQuery(conn,"select * from term_type"))
}

# @return Tabela com as siglas das localidades (estados, regi?es, DF e BR) seus identificadores ?nicos (num?ricos)
get.locations <- function(conn){
  return(dbGetQuery(conn,"select * from location"))
}

# @return Vetor com todos os termos usados para buscar s?ries do GTrends
# @param type - Identificador do tipo de termo (ver fun??o get.types)
get.terms <- function(conn, type){
  return(dbGetQuery(conn,paste0("select text from term where term_type_id = ", type)))
}

# @return Lista de s?ries do GTrends para determinado tipo de termo e estado da federa??o
# @param type.id - Identificador do tipo de termo (ver fun??oo get.types)
# @param location.id - Identificador da localidade (ver fun??o get.locations)
# @param data.frame - A sa?da deve ser um data frame ou um objeto ts? 
get.series <- function(conn, type.id, location.id, data.frame = F){
  series <- dbGetQuery(conn,paste0("select t.text, s.date, s.value from series as s, term as t where s.term_id = t.id and t.term_type_id = ", type.id, " and s.location_id = ", location.id))
  terms <- unique(series[,1])
  names(series) <- c("text","date","value")
  res <- list()
  
  for(i in 1:length(terms)){
    res[[i]] <- filter(series, text == terms[i])[,-1]
    
    if(!data.frame){
      stt = strsplit(format(as.Date(res[[i]]$date[1]), "%Y %m")," ")[[1]]
      res[[i]] = window(ts(res[[i]]$value, frequency = 12, start = as.integer(stt)))
    }
  }
  
  names(res) <- terms
  return(res)
}

# @return Identificadores unicos das s?ries de emprego do IBGE
# Exemplos: Taxa de Desocupa??o, PEA, PIA, PO
get.ibge.types <- function(conn){
  results <- dbGetQuery(conn, "select * from ibge_type")  
  Encoding(results$name) <- "latin1"
  return(results)
}

# @return S?rie da PNAD
# @param location.id - Identificador da localidade (ver fun??o get.locations)
# @param type - "m" = mensal, "t" = trimestral
# @param ibge.id - Identificador da serie
get.pnad <- function(conn, location.id = 0, type = "m", ibge.id = 3){
  
  if(type == "t"){
    series <- dbGetQuery(conn,paste0("select value from pnad_series where location_id = ",location.id, " and type = '",type,"' and ibge_type_id = ", ibge.id))
    if(location.id == 0){
      return(ts(as.vector(series), frequency = 4, start=c(1992,3)))
    } else {
      return(ts(as.vector(series), frequency = 4, start=c(2012,1)))
    }
  } else {
    series <- dbGetQuery(conn,paste0("select value from pnad_series where type = 'm' and ibge_type_id = ", ibge.id))
    return(ts(as.vector(series), frequency = 12, start=c(1992,9)))
  }
}

# @return S?rie da PME
# @param location.id - Identificador da localidade (ver fun??o get.locations)
# @param pnad.id - Identificador da serie
# @note Por enquanto, s? h? series para o Brasil (location.id = 0)
get.pme <- function(conn, location.id = 0, ibge.id = 3){
  series <- dbGetQuery(conn,paste0("select value from pme_series where location_id = ",location.id, " and ibge_type_id = ", ibge.id))
  return(ts(as.vector(series), frequency = 12, start=c(2002,3)))
}

# @return S?rie trimestral constru?da a partir da s?rie mensal, tomando a m?dia de cada trimestre.
# @param series - S?rie mensal a ser transformada
quarterly <- function(series){
  stt = start(series)
  new = apply.quarterly(as.xts(series), mean)
  return(ts(new, start = stt, frequency = 4))
}

# @return S?rie em log retorno
# @param series - S?rie a ser transformada
log.ret <- function(series){
  
  l.series <- diff(log(series), lag=1)
  
  inxs <- c(which(is.nan(l.series) == T), which(is.infinite(l.series) == T))
  if(length(inxs) > 0){
    l.series[inxs] <- 0
    l.series[inxs] <- mean(l.series)
  } 
  
  return(l.series)
}

# @return S?rie em m?dia movel trimestral
# @param series - S?rie a ser transformada
ma3 <- function(series){
  series <- rollmean(series, k=3)
  freq <- frequency(series)
  dt <- start(series)
  dt <- as.Date(paste0(dt[1],"-",dt[2],"-1"))
  month(dt) <- month(dt) + 1
  series <- ts(as.vector(series), start=c(year(dt),month(dt)), frequency=freq)
  return(series)
}

# @return Correla??o entre duas s?ries
# @param series1 - Primeira s?rie 
# @param series2 - Segunda s?rie
get.corr <- function(series1, series2){
  return(round(as.numeric(cor.test(series1, series2)$estimate),2))
}

# Faz um gr?fico padr?o mostrando duas s?ries: uma do Google Trends e outra do IBGE
# @param google - S?rie do Google Trends
# @param ibge - S?rie do IBGE
# @param title - Titulo do gr?fico
# @param lbls - Nomes da s?ries 
# @param corr - Mostrar correla??o? 
# @param ylim - Limites para o eixo y da s?rie do Google
# @param y2 - Fazer em dois eixos?
# @param sec.ylab - O segundo eixo deve ter identifica??o?
plot.gt <- function(google, ibge, lbls = c("Google","IBGE"), title = "", corr = T, ylim = NULL, y2 = T, sec.ylab = T){
  
  if(corr == T){
    title <- paste0(title, " (Correlation: ", get.corr(google,ibge), ")")
  }
  
  if(!y2){
    g.lab <- ""
  }
  
  plot(google, type="l", col="red3", ylab=lbls[1], main = title, cex.main=0.75, ylim = ylim)
  
  if(y2){
    par(new = T)
    plot(ibge, type="l", axes=F, xlab=NA, ylab=NA)
    axis(side = 4)
    
    if(sec.ylab){
      mtext(side = 4, line = 3, lbls[2]) 
    }
  }
  else {
    lines(ibge, col = "black")
  }
  
  legend("topleft", lbls, lty=c(1,1), col=c("red3", "black"), cex = 0.4)
}

# @return S?rie sem outliers e vetor com as posi??es dos outliers
# @param series - S?rie da qual se deseja remover outliers
# @param FUNC - Fun??o que calcula o valor que entra no lugar de cada outlier. 
#               A ?nica entrada dessa fun??o deve ser a s?rie.
rem.outliers <- function(series, FUNC = mean){
  inxs = NULL 
  norm.series <- normalize(series, mode="scale")
  for(i in 1:length(series)){
    if(norm.series[i] > 3){
        series[i] <- FUNC(series)
        inxs <- c(inxs, i)
    }
  }
  if(length(inxs) > 0){
    print(paste0("Found outliers in the following positions: ",paste0(inxs, collapse = ", ")))
  }
  return(list(series = series, positions = inxs))
}

