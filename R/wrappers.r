# wrappers in Portuguese

# interface
salvar.resultados <- function(arquivo)
{
save.output(arquivo)
}

carregar.resultados <- function(arquivo)
{
load.output(arquivo)
}

alterar.diretorio <- function(diretorio)
{
# altera o diretório de trabalho
change.dir(diretorio)
}

importar.dados <- function(arquivo,objeto=NULO,sepvar=NULO,sepdec=NULO,...)
# importa bancos de dados
{
if (!is.null(sepvar))
	{
	sepvar <- switch(sepvar,
		tab="\t",
		espaco=" ",
		sepvar)
	}
import.data(arquivo,objeto,sepvar,sepdec,...)
}

salvar.dados <- function(...,arquivo)
# salva varios objetos do r em arquivo
{
save.data(...,file=arquivo)
}

carregar.dados <- function(arquivo)
# carrega arquivo do r
{
load.data(arquivo)
}

remover.todos <- function()
# limpa área de trabalho
{
clear.all()
}

remover.dados <- function(...)
# remove dados da área de trabalho
{
remove.data(...)
}

esvaziar.lixeira <- function(confirmar=FALSO)
# esvaziar a lixeira
{
erase.trash(confirmar)
}

restaurar.excluidos <- function(...)
# restaurar excluídos
{
restore.deleted(...)
}

listar.dados <- function()
# listar objetos da área de trabalho
{
list.data()
}

descarregar <- function()
# shut EpiR down
{
unload()
}

excluir.colunas <- function(dados,...,objeto=dados,exceto=FALSO)
# exclui as colunas listadas em ...
{
drop.columns(dataset=dados,...,object=objeto,all.but=exceto)
}

selecionar.linhas <- function(dados,consulta,objeto=dados)
# seleciona linhas de acordo com os filtros em consulta
{
select.rows(dataset=dados,query=consulta,object=objeto)
}

agregar.linhas <- function(dados,variavel=NULO,variaveis.agregacao,funcao="media",objeto=NULO,filtro=NULO)
{
funcao <- switch(funcao,
	media="mean",
	soma="sum",
	frequencia="count",
	"mean")
if(!is.null(filtro))
	if(filtro == "")
		filtro <- NULL
row.aggregate(dataset=dados,x=variavel,groups=variaveis.agregacao,FUN=funcao,object=objeto,filter=filtro)
}

categorizar.variavel <- function(banco,variavel,nova.categoria=NULO,numerico=VERDADEIRO,como.numerico=FALSO,categorizar.por=NULO,valores=NULO,probabilidades=NULO,ordenar.fator=FALSO,categoria.referencia=NULO,incluir.esquerda=VERDADEIRO,objeto=NULO,NA.como.fator=FALSO)
{
if(!is.null(categorizar.por))
	categorizar.por <- switch(categorizar.por,
			valores="values",
			quintis="centiles",
			quartis="quantile",
			mediana="median",
			percentis="percentiles",
		"values")
recodeVar(dataset=banco,x=variavel,numeric=numerico,options=categorizar.por,breaks=valores,probs=probabilidades,rigth=incluir.esquerda,ordered_result=ordenar.fator,ref=categoria.referencia,labels=nova.categoria,object=objeto,as.numeric=como.numerico,na.level=NA.como.fator)
}

combinar.bancos <- function(banco1,banco2=NULO,variavel.ligacao1=NULO,variavel.ligacao2=NULO,selecao.variavel.1,selecao.variavel.2=NULO,exceto.selecionadas.1=FALSO,exceto.selecionadas.2=FALSO,mesmas.variaveis=FALSO,objeto=NULO)
{
 merge.dataset(dataset.x=banco1,dataset.y=banco2,by.row=mesmas.variaveis,by.x=variavel.ligacao1,by.y=variavel.ligacao2,all=T,selection.x=selecao.variavel.1,selection.y=selecao.variavel.2,exeption.x=exceto.selecionadas.1,exeption.y=exceto.selecionadas.2,object=objeto)
}

transformar.dados <- function(dados,...,objeto=dados)
# opera transformações de variáveis
{
transform.data(dados,...,object=objeto)
}

iniciar.odbc <- function(fonte,usuario,senha,letra="naomuda",conexao="conODBC")
# conecta a um fonte de dados odbc
{
letracon <- switch(tolower(letra),
	nao_muda="nochange",
	maiuscula="toupper",
	minuscula="tolower",
	tolower(letra)
)
connect.odbc(dsn=fonte,uid=usuario,pwd=senha,case=letracon,conn=conexao)
}

encerrar.odbc <- function(conexao="conODBC")
{
end.odbc(conn=conexao)
}

consultar.odbc <- function(conexao="conODBC",consulta,objeto)
# submete consulta para um servidor sobre uma conexão odbc
{
query.odbc(conn=conexao,sql=consulta,object=objeto)
}

salvar.grafico <- function(arquivo,disp=NULO)
# salva uma janela de gráfico ativa
{
if(is.null(disp))
	disp <- dev.cur()
save.plot(filename=arquivo,dev=disp)
}

# stats
resumo.breve <- function(dados,...,estratos=NULO)
# imprime um resumo breve de variáveis
{
brief.summary(dataset=dados,...,by=estratos)
}

estatisticas.descritivas <- function(dados,...,estratos=NULO,estatisticas=c("n","na","media","desv.padrao","mediana","min","max","quantil","percentil","coef.variacao","var","assimetria","curtose","width"),probabilidades=NULL,valores.extremos=0,decimais=2)
{
#Gera algumas estatísticas descritivas
estatisticas <- tolower(estatisticas)
stats <- NULL
if(any(estatisticas == "n"))
	stats <- "n"
if(any(estatisticas == "nd"))
	stats <- c(stats, "na")
if(any(estatisticas == "media"))
	stats <- c(stats, "mean")
if(any(estatisticas == "desv.padrao"))
	stats <- c(stats, "sd")
if(any(estatisticas == "mediana"))
	stats <- c(stats, "median")
if(any(estatisticas == "min"))
	stats <- c(stats, "min")
if(any(estatisticas == "max"))
	stats <- c(stats, "max")
if(any(estatisticas == "quartil"))
	stats <- c(stats, "quantile")
if(any(estatisticas == "quintil"))
	stats <- c(stats, "centiles")
if(any(estatisticas == "coef.variacao"))
	stats <- c(stats, "variation coefficient")
if(any(estatisticas == "var"))
	stats <- c(stats, "var")
if(any(estatisticas == "assimetria"))
	stats <- c(stats, "skewness")
if(any(estatisticas == "curtose"))
	stats <- c(stats, "kurtosis")
if(any(estatisticas == "amplitude"))
	stats <- c(stats, "range")

desc.vars(dataset=dados,...,by=estratos,stats=stats,probs=probabilidades,trim=valores.extremos,dec=decimais)
}

tabela.frequencia <- function(dados,...,estratos=NULO,remove.na=FALSO)
# Gera uma tabela de frequencia
{
frequency.tables(dataset=dados,...,by=estratos,na.rm=remove.na)
}

tabela.contingencia <- function(dados,...,estratos=NULO,frequencia=VERDADEIRO,calculo_frequencia_relativa=NULO, totais_marginais=FALSO,margem=NULO)
{
#imprime uma tabela cruzada
if(!is.null(margem))	
margem <- switch(margem,
	linha="row",
	coluna="col",
	ambos="total",
	nenhum="none",
	"total")
else
margem <- "total"
if(!is.null(calculo_frequencia_relativa))	
calculo_frequencia_relativa <- switch(calculo_frequencia_relativa,
	linha="row",
	coluna="col",
	ambos="total",
	"total")
else
calculo_frequencia_relativa <- "total"
cross.frequency(dataset = dados,...,by=estratos,frequency=frequencia,calc_margin=calculo_frequencia_relativa,paste_margin=totais_marginais,print_margin=margem)
}

correlacao <- function(banco,...,estratos=NULO,covariancia=FALSO,metodo="Pearson",valor.p=FALSO,elementos.validos=FALSO,excluir.2.a.2=FALSO)
{
if(excluir.2.a.2)
	pairwise <- "pairwise.complete.obs"
else
	pairwise <- "everything"

if(!is.null(estratos))
	if(estratos == "")
		estratos <- NULL

correl(dataset=banco,...,by=estratos,method=metodo,correlation=!covariancia,use=pairwise)
}

# graficos
grafico.simples <- function(dados,x,...,estratos=NULO,tipo="p",cor="auto",linha="auto",ponto="auto",tamanho=1,sobrepor="nenhum",linha.horizontal=NULO,cor.linha.horizontal="auto",titulo="",subtitulo="",rotulo.x="",rotulo.y="",legenda=VERDADEIRO,posicao.legenda="altodireito",legenda.horizontal=FALSO)
# gera um gráfico simples
{
tipo <- switch(tipo,
			ponto="p",
			linha="l",
			ponto_linha="b",
			histograma="h",
			degraus="s",
			"p"
			)
linha <- switch(linha,
			solida="solid",
			traco="dashed",
			ponto="dotted",
			ponto_traco="dotdash",
			traco_longo="longdash",
			dois_tracos="twodash",
			"auto"
			)
ponto <- switch(ponto,
			losango=18,
			disco=19,
			disco_menor=20,
			circulo=21,
			quadrado=22,
			diamante=23,
			triangulo=24,
			triangulo_invertido=25,
			ponto
			)
simple.plot(dataset=dados,x=x,...,by=estratos,type=tipo,col=cor,lty=linha,pch=ponto,size=tamanho,overlay=convOverlayOption(sobrepor),hline=linha.horizontal,col.hline=cor.linha.horizontal,title=titulo,subtitle=subtitulo,xlab=rotulo.x,ylab=rotulo.y,legend=legenda,legend.pos=convLegendPosition(posicao.legenda),legend.horiz=legenda.horizontal)
}

grafico.dispersao <- function(dados,...,estratos=NULO,cor="auto",ponto="auto",tamanho=1,sobrepor="nenhum",linha.horizontal=NULO,cor.linha.horizontal="auto",titulo="",subtitulo="",rotulo.x="",rotulo.y="",legenda=VERDADEIRO,posicao.legenda="altodireito",legenda.horizontal=FALSO)
# gera um gráfico simples
{
ponto <- switch(ponto,
			losango=18,
			disco=19,
			disco_menor=20,
			circulo=21,
			quadrado=22,
			diamante=23,
			triangulo=24,
			triangulo_invertido=25,
			ponto
			)

dispersion.plot(dataset=dados,...,by=estratos,col=cor,pch=ponto,size=tamanho,overlay=convOverlayOption(sobrepor),hline=linha.horizontal,col.hline=cor.linha.horizontal,title=titulo,subtitle=subtitulo,xlab=rotulo.x,ylab=rotulo.y,legend=legenda,legend.pos=convLegendPosition(posicao.legenda),legend.horiz=legenda.horizontal)
}

grafico.histograma <- function(dados,...,estratos=NULO,barras=0,frequencia=VERDADEIRO,cor="auto",titulo="",subtitulo="",rotulo.x="",rotulo.y="")
# executa um histograma
{
histogram.plot(dataset=dados,...,by=estratos,nbreaks=barras,frequency=frequencia,col=cor,title=titulo,subtitle=subtitulo,xlab=rotulo.x,ylab=rotulo.y)
}

grafico.normal <- function(dados,...,estratos=NULO,cor="auto",cor.linha="auto",titulo="",subtitulo="",rotulo.x="",rotulo.y="")
# executa um qqplot
{
normal.plot(dataset=dados,...,by=estratos,col=cor,col.line=cor.linha,title=titulo,subtitle=subtitulo,xlab=rotulo.x,ylab=rotulo.y)
}


grafico.pontos <- function(dados, ..., estratos = NULO, cor = "auto", ponto = "auto", titulo = "", subtitulo = "", rotulo.x = "", rotulo.y = "")
{
ponto <- switch(ponto,
			losango=18,
			disco=19,
			disco_menor=20,
			circulo=21,
			quadrado=22,
			diamante=23,
			triangulo=24,
			triangulo_invertido=25,
			ponto
			)
dot.plot(dataset = dados, ..., by = estratos, col = cor, pch = ponto, title = titulo, subtitle = subtitulo, xlab = rotulo.x, ylab = rotulo.y)
}

grafico.caixa <- function(dados,...,estratos=NULO,cor="auto",titulo="",subtitulo="",rotulo.x="",rotulo.y="")
# executa um histograma
{
box.plot(dataset=dados,...,by=estratos,col=cor,title=titulo,subtitle=subtitulo,xlab=rotulo.x,ylab=rotulo.y)
}


grafico.barras <- function(dados, ..., estratos = NULO, barras.horizontais = FALSO, linha.horizontal = NULO, cor = "auto", linha = "auto", espessura.linha = 1, subtitulo = "", titulo = "", legenda = VERDADEIRO, rotulo.x = "", rotulo.y = "",cor.linha = "auto")
{
# gera um gráfico de barras
linha <- switch(linha,
			solida="solid",
			traco="dashed",
			ponto="dotted",
			ponto_traco="dotdash",
			traco_longo="longdash",
			dois_tracos="twodash",
			"auto"
			)
bar.plot(dataset = dados, ..., by = estratos, horiz = barras.horizontais, hline = linha.horizontal, col = cor, lty = linha, lwd = espessura.linha, subtitle = subtitulo, title = titulo, legend = legenda, xlab = rotulo.x, ylab = rotulo.y, col.hline = cor.linha)
}

grafico.setores <- function(dados,...,estratos=NULO,cor="auto",titulo="",subtitulo="",rotulo.x="",rotulo.y="",rotulos=NULO)
# executa um gráfico de setores
{
cor <- switch(cor,
			arco_iris = "rainbow",
			cinza = "gray",
			terrena = "terrain",
			quente = "heat",
			cor
			)
variaveis <- paste(match.call(expand.dots = FALSE)$...)
if(length(variaveis) != 1)
	{
	categorias.originais <- NULO
	nova.categoria <- NULO
	}
sector.plot(dataset=dados,...,by=estratos,palette=cor,title=titulo,subtitle=subtitulo,xlab=rotulo.x,ylab=rotulo.y,labels=rotulos)
}


grafico.mosaico <- function(dados,...,estratos=NULL,cor ="auto",titulo="",subtitulo="",rotulo.x="",rotulo.y="")
{
mosaic.plot(dataset=dados,...,by=estratos,col=cor,title=titulo,subtitle=subtitulo,xlab=rotulo.x,ylab=rotulo.y)
}

ramo.folha <- function(dados,...,estratos=NULO,escala=1,largura=80)
# implementa o diagrama ramo e folha
{
stem.leaf(dataset=dados,...,by=estratos,scale=escala,width=largura)
}


# models
suavizar.curva <- function(dados,x,y,metodo="spline",ps=4,objeto=NULO,grafico=VERDADEIRO,linha="auto",tamanho=1,titulo="",subtitulo="",rotulo.x="",rotulo.y="",cor.linha = "auto")
# suaviza um gráfico de dispersão
{
linha <- switch(linha,
			solida="solid",
			traco="dashed",
			ponto="dotted",
			ponto_traco="dotdash",
			traco_longo="longdash",
			dois_tracos="twodash",
			"auto"
			)
metodo <- switch(metodo,
			polinomial="polynomial",
			medias_moveis="moving_averages",
			regressao_local="lowess",
			"spline"
			)
curve.smoothing(dataset=dados,x=x,y=y,method=metodo,sp=ps,object=objeto,lwd=tamanho,lty=linha,linecol=cor.linha,plot=grafico,title=titulo,subtitle=subtitulo,xlab=rotulo.x,ylab=rotulo.y)
}


regressao.linear <- function(dados,formula,durbin.watson=FALSO,goldfeld.quandt=FALSO,objeto=NULO,anova=FALSO,pesos=NULL,graficos=VERDADEIRO,retirar.intercepto=FALSO)
# faz uma regressão linear
{
linear.regression(dataset=dados,formula=formula,durbin.watson=durbin.watson,goldfeld.quandt=goldfeld.quandt,object=objeto,anova=anova,weights=pesos,plot=graficos,intercept=!retirar.intercepto)
}

regressao.logistica <- function(dados,formula,objeto=NULL,pesos=NULL,teste.HLemeshow=FALSE,razao.chances=FALSO,offset=NULO,curva.roc=FALSO,graficos.basicos=VERDADEIRO,X.por.Y.estimado=FALSO,D.por.Y.estimado=FALSO,beta.por.Y.estimado=FALSO,X.por.h=FALSO,D.por.h=FALSO,beta.por.h=FALSO)
{
if(any(c(X.por.Y.estimado,D.por.Y.estimado,beta.por.Y.estimado,X.por.h,D.por.h,beta.por.h)))
	graficos.basicos <- FALSO
logistic.regression(dataset=dados,formula=formula,offset=offset,subset=NULL,object=objeto,weights=pesos,odds.ratio=razao.chances,roc.curve=curva.roc,HL.test=teste.HLemeshow,basic.plot=graficos.basicos,X.yhat=X.por.Y.estimado,D.yhat=D.por.Y.estimado,beta.yhat=beta.por.Y.estimado,X.h=X.por.h,D.h=D.por.h,beta.h=beta.por.h)
}

# parametric test

teste.media <- function(dados,variavel.1,variavel.2=NULO,referencia=NULO,duas.amostras=FALSO,parametro=NULO,teste="teste.t",nivel.confianca=0.9,hip.alternativa="diferente_de",pareado=FALSO,variancias.iguais=FALSE,filtro=NULO,agrupamento=FALSO,remove.na=VERDADEIRO)
{
teste <- switch(teste,
		 teste.t="t.test",
		 proporcao="prop.test",
		 "t.test")
hip.alternativa <- switch(hip.alternativa,
				   diferente_de="two.sided",
				   maior_que="greater",
				   menor_que="less",
				   "two.sided")	
mean.test(dataset=dados,x=variavel.1,y=variavel.2,one.sample=!duas.amostras,group=referencia,par=parametro,test=teste,conf.level=nivel.confianca,alternative=hip.alternativa,paired=pareado,filter=filtro,by=agrupamento,var.equal=variancias.iguais,na.rm=remove.na)
}

teste.var <- function(dados,amostra.1,amostra.2=NULO,referencia=NULO,parametro=NULO,teste="teste.t",nivel.confianca=0.9,hip.alternativa="igualdade",remove.na=VERDADEIRO,filtro=NULO,agrupamento=FALSO)
{
if(!is.null(filtro))
	if(filtro == "NULO" | filtro == "")
		filtro <- NULL
teste <- switch(teste,
		 teste.F="var.test",
		 teste.bartlett="bartlett.test",
		 teste.Fligner="fligner.test",
		 "var.test")
hip.alternativa <- switch(hip.alternativa,
				   igualdade="two.sided",
				   maior_que="greater",
				   menor_que="less",
				   "two.sided")
variance.test(dataset=dados,x=amostra.1,y=amostra.2,par=parametro,test=teste,conf.level=nivel.confianca,alternative=hip.alternativa,na.rm=remove.na,filter=filtro,group=agrupamento)
}

teste.associacao <- function(dados,variavel.1,variavel.2,estrato=NULO,teste="q_quadrado",filtro=NULL,mostrar.tabela=FALSO,valor_p=FALSO,nivel.confianca=0.9,hip.alternativa="igualdade",remove.na=VERDADEIRO)
{
teste <- switch(teste,
		 q_quadrado="chisq.test",
		 mantelhaen="mantelhaen.test",
		 "chisq.test")
hip.alternativa <- switch(hip.alternativa,
				   igualdade="two.sided",
				   maior_que="greater",
				   menor_que="less",
				   "two.sided")
if(!is.null(filtro))	 
	if(filtro == "NULO" | filtro == "")
		filtro <- NULL
association.test(dataset=dados,x=variavel.1,y=variavel.2,by=estrato,test=teste,show.table=mostrar.tabela,simulate.p.value=valor_p,conf.level=nivel.confianca,alternative=hip.alternativa,na.rm=remove.na,filter=filtro)
}

#non parametric test
teste.friedman <- function(dados, variavel.1,variavel.2,variavel.3=NULO,variaveis.categoricas=FALSO,filtro=NULO)
{
friedman.htest(dataset=dados,x=variavel.1,y=variavel.2,z=variavel.3,na.action="na.omit",filter=filtro,as.factor=variaveis.categoricas)
}

teste.sinais <- function(dados, variavel.1, variavel.2, filtro=NULO,hip.alternativa="diferente_de")
{
hip.alternativa <- switch(hip.alternativa,
				   diferente_de="two.sided",
				   maior_que="greater",
				   menor_que="less",
				   "two.sided")
rank.htest(dataset=dados,x=variavel.1,y=variavel.2,alternative=hip.alternativa,filter=filtro)
}

teste.wcoxon <- function(dados, variavel.1, variavel.2, parametro=0,nivel.confianca=0.95,hip.alternativa="diferente_de",pareado=FALSO,filtro=NULO)
{
hip.alternativa <- switch(hip.alternativa,
				   diferente_de="two.sided",
				   maior_que="greater",
				   menor_que="less",
				   "two.sided")
if(is.null(pareado) || is.na(pareado) || !is.numeric(pareado))
	pareado <- 0
if(nivel.confianca > 1)
	nivel.confianca <- nivel.confianca/100
wilcoxon.htest(dataset=dados,x=variavel.1,y=variavel.2,alternative=hip.alternativa,mu=parametro,paired=pareado,conf.int=TRUE,conf.level=nivel.confianca,filter=filtro)
}

# epi

razao.chance <- function(dados = NULO,desfecho=NULO,casos=NULO,exposicao=NULO,expostos=NULO,tabela=NULO,teste.q.quadrado=FALSO,teste.fisher=FALSO,nivel.confianca=0.95,calculadora=FALSO)
{
if(!is.null(tabela))
	tabela <- as.numeric(tabela)
case.control(dataset=dados,x=desfecho,x.non_dis=casos,y=exposicao,y.non_exp=expostos,cross.table=tabela,calc=calculadora,fisher.test=teste.fisher,chisq.test=teste.q.quadrado,decimal=EPIR_OPTION_DIGITS,conf.level=nivel.confianca)
}

risco.relativo <- function(dados=NULO,desfecho=NULO,casos=NULO,exposicao=NULO,expostos=NULO,tabela=NULO,risco.relativo=FALSO,teste.q.quadrado=FALSO,teste.fisher=FALSO,nivel.confianca=0.95,calculadora=FALSO)
{
if(!is.null(tabela))
	tabela <- as.numeric(tabela)
cohort(dataset=dados,x=desfecho,x.non_dis=casos,y=exposicao,y.non_exp=expostos,cross.table=tabela,calc=calculadora,risk.print=risco.relativo,fisher.test=teste.fisher,chisq.test=teste.q.quadrado,decimal=EPIR_OPTION_DIGITS,conf.level=nivel.confianca)
}

grafico.controle <- function(dados,variavel,data=NULO,periodo.analise,periodicidade="diario",variavel.mes=NULO,variavel.ano=NULO,metodo="media",periodos.futuros=FALSO,desvios.padrao=1.96,linha.central=FALSO,titulo="",subtitulo="",rotulo.x="",rotulo.y="",linha="solida",espessura=1,cor.linha="red") 
{
if(metodo == "media")
	{
	simple.method <- TRUE
	median.method <- FALSE
	}
else
	{
	simple.method <- FALSE
	median.method <- TRUE
	}

linha <- switch(linha,
			solida="solid",
			traco="dashed",
			ponto="dotted",
			ponto_traco="dotdash",
			traco_longo="longdash",
			dois_tracos="twodash",
			"solid"
			)
periodicidade <- switch(periodicidade,
				diario="daily",
				semanal="weekly",
				mensal="monthly",
				"daily"
				)
control.charts(dataset=dados,x=variavel,date=data,type=periodicidade,date.month=variavel.mes,date.year=variavel.ano,year=periodo.analise,until.year=!periodos.futuros,simple.method=simple.method,median.method=median.method,limit=desvios.padrao,col=cor.linha,lty=linha,size=espessura,title=titulo,subtitle=subtitulo,xlab=rotulo.x,ylab=rotulo.y,year.plot=linha.central)
}

formatar.data <- function(dados,variavel.data,nome.variavel,formato,seculo="1900")
{
format.date(dataset=dados,datevar=variavel.data,varname=nome.variavel,format=formato,origin=seculo)
}
# operadores
"%!pertence%" <- function(x,y) !(x %in% y)

"%pertence%" <- function(x,y) x %in% y

quadrado <- function(x) (x^2)

raiz2 <- function(x) sqrt(x)

inverte <- function(x) 1/x

media <- function(x) mean(x,na.rm=TRUE)

mediana <- function(x) median(x, na.rm=TRUE)

centrar.media <- function(x) (x - mean(x,na.rm=TRUE))

variancia <- function(x) var(x,na.rm=TRUE)

# conversoes de macros
convLegendPosition <- function(position)
# convert positions from portuguese to english
{
if(is.character(position))
	retval <-	switch(position,
				alto="top",
				alto_direito="topright",
				alto_esquerdo="topleft",
				esquerdo="left",
				direito="right",
				baixo_direito="bottomright",
				baixo_esquerdo="bottomleft",
				baixo="bottom",
				centro="center",
				NULL
				)
else if(length(position)==2)
	retval <- position
return(retval)
}

convOverlayOption <- function(option)
# convert positions from portuguese to english
{
retval <- switch(option,
				niveis="levels",
				variaveis="variables",
				"none"
				)
return(retval)
}

