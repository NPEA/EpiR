on_btn_MTST_help_clicked <- function(widget,user.data)
{
showHelp("MeanTest")
}

on_btn_MTST_cancel_clicked <- function(widget,user.data)
{
closeWindow("MeanTest")
}

on_btn_MTST_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("MeanTest","cbx_MTST_dataset"))
variable1 <- getActiveData(getWidget("MeanTest","cbx_MTST_variable1"))
variable2 <- getActiveData(getWidget("MeanTest","cbx_MTST_variable2"))
two_samples <- getWidget("MeanTest","rb_MTST_2sample")$getActive()
by <- getWidget("MeanTest","chk_MTST_agrup")$getActive()
if(by) 
	by <- "VERDADEIRO"
else
	by <- "FALSO"
group <- getActiveData(getWidget("MeanTest","cbx_MTST_ref"))
if(group =="")
	group <- "NULO"
else
	group <- paste("\"",group ,"\"")
if(variable2 == "")
	variable2 <- "NULO"
else 
	variable2 <- paste("\"",variable2 ,"\"")
if(getWidget("MeanTest","rb_MTST_2sample")$getActive())
	two_samples <- "VERDADEIRO"
else
	two_samples <- "FALSO"
if(getWidget("MeanTest","rb_MTST_proptest")$getActive())
	test <- "proporcao"
else
	test <- "teste.t"
par <- getWidget("MeanTest","txt_MTST_statistic")$getText()
if(par == "")
	if(test == "proporcao")
		par <- 0.5
	else
		par <- 0
conf.level <- getWidget("MeanTest","spb_MTST_conf")$getValue()
if(conf.level > 1)
	conf.level <- conf.level/100
alternative <- getActiveData(getWidget("MeanTest","cbx_MTST_test_type"))
if(alternative=="")
	alternative <- "diferente_de"
if(getWidget("MeanTest","chk_MTST_paired")$getActive())
	paired <- "VERDADEIRO"
else
	paired <- "FALSO"
if(getWidget("MeanTest","chk_MTST_equalvar")$getActive())
	equal.var <- "VERDADEIRO"
else
	equal.var <- "FALSO"
na.remove <- getWidget("MeanTest","chk_MTST_ND")$getActive()
if(na.remove)
	na.remove <- "VERDADEIRO"
else
	na.remove <- "FALSO"
filter <- setLocale(getWidget("MeanTest","txt_MTST_filter")$getText(),fixspc=FALSE)
dataset.sample <- eval(parse(text = dataset))
filter_test <- try(eval(parse(text = filter),dataset.sample),silent=TRUE)
if(class(filter_test) == "try-error")
	{
	msgDialog("MeanTest","error","Há um erro na expressão do filtro. Corrija-a e tente novamente.")
	return()
	}
else if(!any(filter_test) & trim(filter) != "")
	{
	msgDialog("MeanTest","error","Verifique a expressão do filtro. Nenhuma linha do banco de dados será selecionada.")
	return()
	}
else
	{
	if(trim(filter) == "")
		filter <- NULL
	#translate
	if(test == "proporcao" & !is.null(filter))
		cmd <- paste("teste.media(\"",dataset,"\",variavel.1 =\"",variable1,"\",variavel.2=",variable2,",referencia=",group,",duas.amostras=",two_samples,",parametro=",par,",teste=\"",test,"\",nivel.confianca=",conf.level,",hip.alternativa=\"",alternative,"\",filtro=\"",filter,"\",agrupamento=",by,",remove.na=",na.remove,")",sep="")
	else if(test == "proporcao")
		cmd <- paste("teste.media(\"",dataset,"\",variavel.1 =\"",variable1,"\",variavel.2=",variable2,",referencia=",group,",duas.amostras=",two_samples,",parametro=",par,",teste=\"",test,"\",nivel.confianca=",conf.level,",hip.alternativa=\"",alternative,"\",agrupamento=",by,",remove.na=",na.remove,")",sep="")
	else if(!is.null(filter))
		cmd <- paste("teste.media(\"",dataset,"\",variavel.1 =\"",variable1,"\",variavel.2=",variable2,",referencia=",group,",duas.amostras=",two_samples,",parametro=",par,",teste=\"",test,"\",nivel.confianca=",conf.level,",hip.alternativa=\"",alternative,"\",pareado=",paired,",filtro=\"",filter,"\",variancias.iguais=",equal.var,",agrupamento=",by,",remove.na=",na.remove,")",sep="")
	else
		cmd <- paste("teste.media(\"",dataset,"\",variavel.1 =\"",variable1,"\",variavel.2=",variable2,",referencia=",group,",duas.amostras=",two_samples,",parametro=",par,",teste=\"",test,"\",nivel.confianca=",conf.level,",hip.alternativa=\"",alternative,"\",pareado=",paired,",variancias.iguais=",equal.var,",agrupamento=",by,",remove.na=",na.remove,")",sep="")

	runCommand(cmd)
	closeWindow("MeanTest", "MTST")
	}
}

on_cbx_MTST_dataset_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("MeanTest","cbx_MTST_dataset"))
group <- getWidget("MeanTest","chk_MTST_agrup")$getActive()
ttest.rb <- getWidget("MeanTest","rb_MTST_ttest")$getActive()
sample2.button <- getWidget("MeanTest","rb_MTST_2sample")$getActive()
fillComboBox(getWidget("MeanTest","cbx_MTST_variable1"),c("",getNames(dataset)))
fillComboBox(getWidget("MeanTest","cbx_MTST_variable2"),c("",getNames(dataset)))
if(ttest.rb)
	{
	getWidget("MeanTest","txt_MTST_statistic")$setText(0)
	if(group)
		{
		names.dataset <- names(eval(parse(text=dataset)))
		ret.names.dataset <- NULL
		for(i in names.dataset)
			if(nlevels(as.factor(eval(parse(text = paste(dataset,"$",i,sep=""))))) == 2)
				ret.names.dataset <- c(ret.names.dataset,i)
		fillComboBox(getWidget("MeanTest", "cbx_MTST_variable2"), c("",ret.names.dataset))
		}
	}
else
	{
	if(!sample2.button)
		getWidget("MeanTest","txt_MTST_statistic")$setText(0.5)
	else
		getWidget("MeanTest","txt_MTST_statistic")$setText(0)
	}
if(dataset == "")
	getWidget("MeanTest","btn_MTST_expconstructor")$setSensitive(FALSE)
else
	getWidget("MeanTest","btn_MTST_expconstructor")$setSensitive(TRUE)
}

on_cbx_MTST_variable1_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("MeanTest","cbx_MTST_dataset"),fix=FALSE)
variable1 <- trim(getActiveData(getWidget("MeanTest","cbx_MTST_variable1"),fix=FALSE))
variable2 <- trim(getActiveData(getWidget("MeanTest","cbx_MTST_variable2"),fix=FALSE))
sample2.button <- getWidget("MeanTest","rb_MTST_2sample")$getActive()
ref <- trim(getActiveData(getWidget("MeanTest","cbx_MTST_ref"),fix=FALSE))
ttest.rb <- getWidget("MeanTest","rb_MTST_ttest")$getActive()

if(ttest.rb)
	{
	if(!sample2.button)
		{
		if(variable1 != "")
			{
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
			getWidget("MeanTest","cbx_MTST_variable2")$setSensitive(FALSE)
			getWidget("MeanTest","cbx_MTST_ref")$setSensitive(FALSE)
			}			
		else
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		}
	else
		{
		if(variable1 != "" && variable2 != "")			
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
		else
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		}
	}
else
	{
	if(!sample2.button)
		{
		if(variable1 != "")
			{
			factor <- as.factor(eval(parse(text=paste(dataset,"$",variable1,sep=""))))
			factor <- levels(factor)
			fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c("",factor))
			}
		getWidget("MeanTest","cbx_MTST_variable2")$setSensitive(FALSE)
		if(variable1 == "")
			fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c(""))
		}
	else
		{
		if(variable1 != "" && variable2 != "")
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
		}
	}
}

on_cbx_MTST_variable2_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("MeanTest","cbx_MTST_dataset"))
variable1 <- getActiveData(getWidget("MeanTest","cbx_MTST_variable1"))
variable2 <- getActiveData(getWidget("MeanTest","cbx_MTST_variable2"))
sample2.button <- getWidget("MeanTest","rb_MTST_2sample")$getActive()
ttest.rb <- getWidget("MeanTest","rb_MTST_ttest")$getActive()
if(ttest.rb)
	{
	if(!sample2.button)
		{
		if(variable2 != "")
			{
			factor <- as.factor(eval(parse(text=paste(dataset,"$",variable2,sep=""))))
			factor <- levels(factor)
			fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c("",factor))
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
			}
		else
			{
			if(variable1 != "")
				getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
			fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c(""))
			}
		}
	else
		{
		if(variable1 != "" && variable2 != "")
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
		else
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		}
	}
else
	{
	if(variable2 == "")
		{
		fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c(""))
		getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		}
	else
		{
		factor <- as.factor(eval(parse(text=paste(dataset,"$",variable2,sep=""))))
		factor <- levels(factor)
		fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c("",factor))
		getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		}
	}
}

on_rb_MTST_ttest_toggled <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("MeanTest","cbx_MTST_dataset"))
variable1 <- getActiveData(getWidget("MeanTest","cbx_MTST_variable1"))
variable2 <- getActiveData(getWidget("MeanTest","cbx_MTST_variable2"))
sample2.button <- getWidget("MeanTest","rb_MTST_2sample")$getActive()
ttest.rb <- getWidget("MeanTest","rb_MTST_ttest")$getActive()
ref <-  getActiveData(getWidget("MeanTest","cbx_MTST_ref"))
if(ttest.rb)
	{
	getWidget("MeanTest","txt_MTST_statistic")$setSensitive(TRUE)
	getWidget("MeanTest","txt_MTST_statistic")$setText(0)
	getWidget("MeanTest","cbx_MTST_ref")$setSensitive(FALSE)
	if(!sample2.button)
		{		
		getWidget("MeanTest","cbx_MTST_variable2")$setSensitive(FALSE)
		getWidget("MeanTest","chk_MTST_paired")$setSensitive(FALSE)
		getWidget("MeanTest","chk_MTST_equalvar")$setSensitive(FALSE)
		if(variable1 == "")
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		else
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
		}
	else
		{
		getWidget("MeanTest","chk_MTST_agrup")$setSensitive(TRUE)
		getWidget("MeanTest","cbx_MTST_variable2")$setSensitive(TRUE)
		getWidget("MeanTest","chk_MTST_paired")$setSensitive(TRUE)
		getWidget("MeanTest","chk_MTST_equalvar")$setSensitive(TRUE)
		if(variable1 != "" && variable2 != "")
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
		else
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		}
	}
else
	{
	getWidget("MeanTest","chk_MTST_agrup")$setSensitive(FALSE)
	if(!sample2.button)
		{
		getWidget("MeanTest","txt_MTST_statistic")$setSensitive(TRUE)
		getWidget("MeanTest","txt_MTST_statistic")$setText(0.5)
		getWidget("MeanTest","cbx_MTST_ref")$setSensitive(TRUE)
		getWidget("MeanTest","chk_MTST_paired")$setSensitive(FALSE)
		getWidget("MeanTest","chk_MTST_equalvar")$setSensitive(FALSE)
		fillComboBox(getWidget("MeanTest","cbx_MTST_variable2"),c("",getNames(dataset)))
		getWidget("MeanTest","cbx_MTST_variable2")$setSensitive(FALSE)
		if(variable1 != "")
			{
			factor <- as.factor(eval(parse(text=paste(dataset,"$",variable1,sep=""))))
			factor <- levels(factor)
			fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c("",factor))
			}
		getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		}
	else
		{
		getWidget("MeanTest","txt_MTST_statistic")$setText(0)
		if(variable2 != "")
			{
			factor <- as.factor(eval(parse(text=paste(dataset,"$",variable2,sep=""))))
			factor <- levels(factor)
			fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c("",factor))
			}
		getWidget("MeanTest","cbx_MTST_ref")$setSensitive(TRUE)
		getWidget("MeanTest","txt_MTST_statistic")$setText("")
		getWidget("MeanTest","txt_MTST_statistic")$setSensitive(FALSE)
		getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		}
	}
}


on_rb_MTST_1sample_toggled <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("MeanTest","cbx_MTST_dataset"))
variable1 <- getActiveData(getWidget("MeanTest","cbx_MTST_variable1"))
variable2 <- getActiveData(getWidget("MeanTest","cbx_MTST_variable2"))
sample2.button <- getWidget("MeanTest","rb_MTST_2sample")$getActive()
ttest.rb <- getWidget("MeanTest","rb_MTST_ttest")$getActive()
ref <-  getActiveData(getWidget("MeanTest","cbx_MTST_ref"))
sample1 <- getWidget("MeanTest","rb_MTST_1sample")$getActive()
by <- getWidget("MeanTest","chk_MTST_agrup")$getActive()
if(sample1)
	{	
	getWidget("MeanTest","chk_MTST_agrup")$setSensitive(FALSE)
	if(ttest.rb) #using t.test and 1 sample
		{
		fillComboBox(getWidget("MeanTest","cbx_MTST_variable2"),c("",getNames(dataset)))
		getWidget("MeanTest","chk_MTST_paired")$setSensitive(FALSE)
		getWidget("MeanTest","chk_MTST_equalvar")$setSensitive(FALSE)
		getWidget("MeanTest","cbx_MTST_ref")$setSensitive(TRUE)
		getWidget("MeanTest","cbx_MTST_variable2")$setSensitive(FALSE)
		getWidget("MeanTest","cbx_MTST_ref")$setSensitive(FALSE)
		if(variable1 != "")
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
		else
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		}
	else #using prop.test and 1 sample
		{
		getWidget("MeanTest","txt_MTST_statistic")$setText(0.5)
		getWidget("MeanTest","cbx_MTST_variable2")$setSensitive(FALSE)
		fillComboBox(getWidget("MeanTest","cbx_MTST_variable2"),c("",getNames(dataset)))
		getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		getWidget("MeanTest","cbx_MTST_ref")$setSensitive(TRUE)
		if(variable1 != "")
			{
			factor <- as.factor(eval(parse(text=paste(dataset,"$",variable1,sep=""))))
			factor <- levels(factor)
			fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c("",factor))			
			}
		else
			fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c(""))
		}
	}
else # 2 samples
	{
	if(ttest.rb) #using t.test and 2 samples
		{
		getWidget("MeanTest","chk_MTST_agrup")$setSensitive(TRUE)
		getWidget("MeanTest","chk_MTST_paired")$setSensitive(TRUE)
		getWidget("MeanTest","chk_MTST_equalvar")$setSensitive(TRUE)
		fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c(""))
		getWidget("MeanTest","cbx_MTST_variable2")$setSensitive(TRUE)
		if(by)
			{
			getWidget("MeanTest","cbx_MTST_ref")$setSensitive(FALSE)
			if(variable2 != "")
				{
				factor <- as.factor(eval(parse(text=paste(dataset,"$",variable2,sep=""))))
				factor <- levels(factor)
				fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c("",factor))
				}
			}
		if(variable1 == "" || variable2 == "")
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		else
			{
			if(by)
				{
				if(ref == "")
					getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
				}
			else
				getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
			}
		}
	else #using prop.test and 2 samples
		{
		getWidget("MeanTest","chk_MTST_agrup")$setSensitive(FALSE)
		getWidget("MeanTest","txt_MTST_statistic")$setText(0)
		getWidget("MeanTest","cbx_MTST_variable2")$setSensitive(TRUE)
		fillComboBox(getWidget("MeanTest","cbx_MTST_ref"),c(""))
		getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		}
	}
}


on_cbx_MTST_ref_changed <- function(widget, user.data)
{
variable1 <- getActiveData(getWidget("MeanTest","cbx_MTST_variable1")) 
variable2 <- getActiveData(getWidget("MeanTest","cbx_MTST_variable2"))
sample2.button <- getWidget("MeanTest","rb_MTST_2sample")$getActive()
ttest.rb <- getWidget("MeanTest","rb_MTST_ttest")$getActive()
ref <-  getActiveData(getWidget("MeanTest","cbx_MTST_ref"))
if(ref != "")
	{
	if(ttest.rb)
		{
		if(!sample2.button && variable1 != "")			
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
		else if(sample2.button && variable1 != "" && variable2 != "")
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
		else
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
		}
	else #prop.test
		{
		if(!sample2.button)
			getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
		else
			{
			if(variable1 != "" && variable2 != "")
				getWidget("MeanTest","btn_MTST_execute")$setSensitive(TRUE)
			else
				getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
			}
		}
	}
else
	getWidget("MeanTest","btn_MTST_execute")$setSensitive(FALSE)
}

on_chk_MTST_agrup_toggled <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("MeanTest","cbx_MTST_dataset"))
names.dataset <- getNames(dataset)
paired <- getWidget("MeanTest","chk_MTST_paired")$getActive()
group <- getWidget("MeanTest","chk_MTST_agrup")$getActive()
if(group)
	{
	getWidget("MeanTest","chk_MTST_paired")$setActive(FALSE)	
	if(dataset != "")
		{
		ret.names.dataset <- NULL
		for(i in names.dataset)
			if(nlevels(as.factor(eval(parse(text = paste(dataset,"$",i,sep=""))))) == 2)
				ret.names.dataset <- c(ret.names.dataset,i)
		fillComboBox(getWidget("MeanTest", "cbx_MTST_variable2"), c("",ret.names.dataset))
		}
	}
else
	{
	if(dataset != "")
		fillComboBox(getWidget("MeanTest", "cbx_MTST_variable2"), c("",names.dataset))
	}

}
on_chk_MTST_paired_toggled <- function(widget,user.data)
{
paired <- getWidget("MeanTest","chk_MTST_paired")$getActive()
group <- getWidget("MeanTest","chk_MTST_agrup")$getActive()
if(paired)
	getWidget("MeanTest","chk_MTST_agrup")$setActive(FALSE)
}

on_btn_MTST_expconstructor_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("MeanTest","cbx_MTST_dataset"))
variables <- colnames(eval(parse(text=dataset),envir=.GlobalEnv))
relations <- c("!=","<","<=",">",">=","==","pertence","não-pertence")
setWindow("SelectRows", parent="MeanTest")
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
fillListView(getWidget("SelectRows","tvw_SR_variables"),variables,FALSE,"Variável",sel.mode="browse")
fillListView(getWidget("SelectRows","tvw_SR_relations"),relations,FALSE,"Operador relacional",sel.mode="browse")
fillComboBox(getWidget("SelectRows","cbx_SR_dataset"),dataset,FALSE,TRUE)
fillListView(getWidget("SelectRows","tvw_SR_values"),NULL,FALSE,"Valores",sel.mode="multiple")
gtkWidgetHide(getWidget("SelectRows","hbx_SR_selectDataset"))
}