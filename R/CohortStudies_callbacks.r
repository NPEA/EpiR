on_btn_CCT_help_clicked <- function(widget,user.data)
{
showHelp("CohortStudies")
}

on_btn_CCT_cancel_clicked <- function(widget,user.data)
{
closeWindow("CohortStudies")
}

on_btn_CCT_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("CohortStudies","cbx_CCT_dataset"))
desf.variable <- getActiveData(getWidget("CohortStudies","cbx_CCT_desf_var"))
exp.variable <- getActiveData(getWidget("CohortStudies","cbx_CCT_exp_var"))
desf.cases <- getActiveData(getWidget("CohortStudies","cbx_CCT_cases"))
exp.exp <- getActiveData(getWidget("CohortStudies","cbx_CCT_exp"))
nexp.ncases <- setLocale(getWidget("CohortStudies","spb_nexp_ncase")$getText())
nexp.cases <- setLocale(getWidget("CohortStudies","spb_nexp_case")$getText())
exp.ncases <- setLocale(getWidget("CohortStudies","spb_exp_ncase")$getText())
exp.cases <- setLocale(getWidget("CohortStudies","spb_exp_case")$getText())
relative.risk <- getWidget("CohortStudies","chk_CCT_r.risk")$getActive()
if(relative.risk)
	relative.risk <- "VERDADEIRO"
else
	relative.risk <- "FALSO"
chisq.test <- getWidget("CohortStudies","chk_CCT_chisq")$getActive()
if(chisq.test)
	chisq.test <- "VERDADEIRO"
else
	chisq.test <- "FALSO"
fisher.test <- getWidget("CohortStudies","chk_CCT_fisher")$getActive()
if(fisher.test)
	fisher.test <- "VERDADEIRO"
else
	fisher.test <- "FALSO"
conf.level <- getWidget("CohortStudies","spb_CCT_IC")$getValue()
conf.level <- conf.level/100
calc <- getWidget("CohortStudies","chk_CCT_calc")$getActive()
if(calc)
	calc <- "VERDADEIRO"
else
	calc <- "FALSO"
table <- c(exp.cases,exp.ncases,nexp.cases,nexp.ncases)
table <- as.character(table)
table <- paste("c(\"",vectorToString(table),"\")", sep = "")
#translate
if(calc=="VERDADEIRO")
	cmd <- paste("risco.relativo(tabela=",table,",risco.relativo=",relative.risk,",teste.q.quadrado=",chisq.test ,",teste.fisher=",fisher.test,",nivel.confianca=",conf.level,",calculadora=",calc,")",sep="")
else
	cmd <- paste("risco.relativo(\"",dataset,"\",desfecho=\"",desf.variable,"\",casos= \"",desf.cases,"\",exposicao=\"",exp.variable,"\",expostos=\"",exp.exp,"\",risco.relativo=",relative.risk,",teste.q.quadrado=",chisq.test ,",teste.fisher=",fisher.test,",nivel.confianca=",conf.level,",calculadora=",calc,")",sep="")
runCommand(cmd)
closeWindow("CohortStudies", "CCT")
}

on_cbx_CCT_dataset_changed <- function(widget, user.data)
{
dataset <- getActiveData(getWidget("CohortStudies","cbx_CCT_dataset"))
names.dataset <- names(eval(parse(text=dataset)))
ret.names.dataset <- NULL
for(i in names.dataset)
	if(nlevels(as.factor(eval(parse(text = paste(dataset,"$",i,sep=""))))) == 2)
		ret.names.dataset <- c(ret.names.dataset,i)
fillComboBox(getWidget("CohortStudies","cbx_CCT_desf_var"),c("",ret.names.dataset))
fillComboBox(getWidget("CohortStudies","cbx_CCT_exp_var"),c("",ret.names.dataset))
}

on_cbx_CCT_desf_var_changed <- function(widget, user.data)
{
dataset <- getActiveData(getWidget("CohortStudies","cbx_CCT_dataset"))
desf.variable <- getActiveData(getWidget("CohortStudies","cbx_CCT_desf_var"))
exp.variable <- getActiveData(getWidget("CohortStudies","cbx_CCT_exp_var"))
desf.ncases <- getActiveData(getWidget("CohortStudies","cbx_CCT_cases"))
exp.nexp <- getActiveData(getWidget("CohortStudies","cbx_CCT_exp"))
if(desf.variable != "")
	{
	factor <- as.factor(eval(parse(text=paste(dataset,"$",desf.variable,sep=""))))
	factor <- levels(factor)
	fillComboBox(getWidget("CohortStudies","cbx_CCT_cases"),c("",factor))
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(FALSE)
	getWidget("CohortStudies","spb_nexp_ncase")$setText(0)
	getWidget("CohortStudies","spb_nexp_case")$setText(0)
	getWidget("CohortStudies","spb_exp_ncase")$setText(0)
	getWidget("CohortStudies","spb_exp_case")$setText(0)
	}
else
	fillComboBox(getWidget("CohortStudies","cbx_CCT_cases"),c(""))
}


on_cbx_CCT_exp_var_changed <- function(widget, user.data)
{
dataset <- getActiveData(getWidget("CohortStudies","cbx_CCT_dataset"))
desf.variable <- getActiveData(getWidget("CohortStudies","cbx_CCT_desf_var"))
exp.variable <- getActiveData(getWidget("CohortStudies","cbx_CCT_exp_var"))
desf.ncases <- getActiveData(getWidget("CohortStudies","cbx_CCT_cases"))
exp.nexp <- getActiveData(getWidget("CohortStudies","cbx_CCT_exp"))

if(exp.variable != "")
	{
	factor <- as.factor(eval(parse(text=paste(dataset,"$",exp.variable,sep=""))))
	factor <- levels(factor)
	fillComboBox(getWidget("CohortStudies","cbx_CCT_exp"),c("",factor))
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(FALSE)
	getWidget("CohortStudies","spb_nexp_ncase")$setText(0)
	getWidget("CohortStudies","spb_nexp_case")$setText(0)
	getWidget("CohortStudies","spb_exp_ncase")$setText(0)
	getWidget("CohortStudies","spb_exp_case")$setText(0)
	}
else
	fillComboBox(getWidget("CohortStudies","cbx_CCT_exp"),c(""))
}

on_cbx_CCT_cases_changed <- function(widget, user.data)
{
dataset <- getActiveData(getWidget("CohortStudies","cbx_CCT_dataset"))
desf.variable <- getActiveData(getWidget("CohortStudies","cbx_CCT_desf_var"))
exp.variable <- getActiveData(getWidget("CohortStudies","cbx_CCT_exp_var"))
desf.ncases <- getActiveData(getWidget("CohortStudies","cbx_CCT_cases"))
exp.nexp <- getActiveData(getWidget("CohortStudies","cbx_CCT_exp"))

if(desf.variable != "" && exp.variable != "" && desf.ncases != "" && exp.nexp != "")
	{
	desf <- eval(parse(text=paste(dataset,"$",desf.variable,sep="")))
	desf <- as.factor(desf)
	expo <-  eval(parse(text=paste(dataset,"$",exp.variable,sep="")))
	expo <- as.factor(expo)
	desf <- relevel(desf,levels(desf)[levels(desf) != desf.ncases])
	expo <- relevel(expo,levels(expo)[levels(expo) != exp.nexp])
	table <- table(expo, desf)
	getWidget("CohortStudies","spb_exp_case")$setText(table[2,2])	
	getWidget("CohortStudies","spb_nexp_case")$setText(table[1,2])
	getWidget("CohortStudies","spb_exp_ncase")$setText(table[2,1])
	getWidget("CohortStudies","spb_nexp_ncase")$setText(table[1,1])
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(TRUE)
	}
else
	{
	getWidget("CohortStudies","spb_exp_case")$setText(0)
	getWidget("CohortStudies","spb_exp_ncase")$setText(0)
	getWidget("CohortStudies","spb_nexp_case")$setText(0)
	getWidget("CohortStudies","spb_nexp_ncase")$setText(0)	
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(FALSE)
	}
}

on_cbx_CCT_exp_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("CohortStudies","cbx_CCT_dataset"))
desf.variable <- getActiveData(getWidget("CohortStudies","cbx_CCT_desf_var"))
exp.variable <- getActiveData(getWidget("CohortStudies","cbx_CCT_exp_var"))
desf.ncases <- getActiveData(getWidget("CohortStudies","cbx_CCT_cases"))
exp.nexp <- getActiveData(getWidget("CohortStudies","cbx_CCT_exp"))
if(desf.variable != "" && exp.variable != "" && desf.ncases != "" && exp.nexp != "")
	{
	desf <- eval(parse(text=paste(dataset,"$",desf.variable,sep="")))
	desf <- as.factor(desf)
	expo <-  eval(parse(text=paste(dataset,"$",exp.variable,sep="")))
	expo <- as.factor(expo)
	desf <- relevel(desf,levels(desf)[levels(desf) != desf.ncases])
	expo <- relevel(expo,levels(expo)[levels(expo) != exp.nexp])
	table <- table(expo, desf)
	getWidget("CohortStudies","spb_exp_case")$setText(table[2,2])	
	getWidget("CohortStudies","spb_nexp_case")$setText(table[1,2])
	getWidget("CohortStudies","spb_exp_ncase")$setText(table[2,1])
	getWidget("CohortStudies","spb_nexp_ncase")$setText(table[1,1])
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(TRUE)
	}
else
	{
	getWidget("CohortStudies","spb_exp_case")$setText(0)
	getWidget("CohortStudies","spb_exp_ncase")$setText(0)
	getWidget("CohortStudies","spb_nexp_case")$setText(0)
	getWidget("CohortStudies","spb_nexp_ncase")$setText(0)	
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(FALSE)
	}
}

on_chk_CCT_calc_toggled <- function(widget, user.data)
{
dataset <- getActiveData(getWidget("CohortStudies","cbx_CCT_dataset"))
calc <- getWidget("CohortStudies","chk_CCT_calc")$getActive()
if(calc)
	{
	if(dataset != "")
		{
		names.dataset <- names(eval(parse(text=dataset)))
		ret.names.dataset <- NULL
		for(i in names.dataset)
			if(nlevels(as.factor(eval(parse(text = paste(dataset,"$",i,sep=""))))) == 2)
				ret.names.dataset <- c(ret.names.dataset,i)
		#restart table
		fillComboBox(getWidget("CohortStudies","cbx_CCT_desf_var"),c("",ret.names.dataset))
		fillComboBox(getWidget("CohortStudies","cbx_CCT_exp_var"),c("",ret.names.dataset))
		fillComboBox(getWidget("CohortStudies","cbx_CCT_cases"),c(""))
		fillComboBox(getWidget("CohortStudies","cbx_CCT_exp"),c(""))
		}
	getWidget("CohortStudies","hbx_CCT_box_calc")$setSensitive(TRUE)
	getWidget("CohortStudies","vbx_CCT_table1")$setSensitive(FALSE)
	getWidget("CohortStudies","vbx_CCT_table2")$setSensitive(FALSE)
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(FALSE)
	#restart calc
	getWidget("CohortStudies","spb_exp_case")$setText(0)
	getWidget("CohortStudies","spb_exp_ncase")$setText(0)
	getWidget("CohortStudies","spb_nexp_case")$setText(0)
	getWidget("CohortStudies","spb_nexp_ncase")$setText(0)	
	}
else
	{
	getWidget("CohortStudies","hbx_CCT_box_calc")$setSensitive(FALSE)
	getWidget("CohortStudies","vbx_CCT_table1")$setSensitive(TRUE)
	getWidget("CohortStudies","vbx_CCT_table2")$setSensitive(TRUE)
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(FALSE)
	#restart calc
	getWidget("CohortStudies","spb_exp_case")$setText(0)
	getWidget("CohortStudies","spb_exp_ncase")$setText(0)
	getWidget("CohortStudies","spb_nexp_case")$setText(0)
	getWidget("CohortStudies","spb_nexp_ncase")$setText(0)	
	}
}

on_chk_CCT_fisher_toggled <- function(widget,user.data)
{
fisher <- getWidget("CohortStudies","chk_CCT_fisher")$getActive()
if(fisher)
	getWidget("CohortStudies","hbx_CCT_IC")$setSensitive(TRUE)
else
	getWidget("CohortStudies","hbx_CCT_IC")$setSensitive(FALSE)
}

on_spb_nexp_ncase_value_changed <- function(widget,user.data)
{
nexp_ncases <- getWidget("CohortStudies","spb_nexp_ncase")$getValue()
nexp_cases <- getWidget("CohortStudies","spb_nexp_case")$getValue()
exp_ncases <- getWidget("CohortStudies","spb_exp_ncase")$getValue()
exp_cases <- getWidget("CohortStudies","spb_exp_case")$getValue()
calc <- c(nexp_ncases,nexp_cases,exp_ncases,exp_cases)
if(any(calc == 0))
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(FALSE)
else
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(TRUE)
}

on_spb_nexp_case_value_changed <- function(widget,user.data)
{
nexp_ncases <- getWidget("CohortStudies","spb_nexp_ncase")$getValue()
nexp_cases <- getWidget("CohortStudies","spb_nexp_case")$getValue()
exp_ncases <- getWidget("CohortStudies","spb_exp_ncase")$getValue()
exp_cases <- getWidget("CohortStudies","spb_exp_case")$getValue()
calc <- c(nexp_ncases,nexp_cases,exp_ncases,exp_cases)
if(any(calc == 0))
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(FALSE)
else
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(TRUE)
}

on_spb_exp_ncase_value_changed <- function(widget,user.data)
{
nexp_ncases <- getWidget("CohortStudies","spb_nexp_ncase")$getValue()
nexp_cases <- getWidget("CohortStudies","spb_nexp_case")$getValue()
exp_ncases <- getWidget("CohortStudies","spb_exp_ncase")$getValue()
exp_cases <- getWidget("CohortStudies","spb_exp_case")$getValue()
calc <- c(nexp_ncases,nexp_cases,exp_ncases,exp_cases)
if(any(calc == 0))
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(FALSE)
else
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(TRUE)
}

on_spb_exp_case_value_changed <- function(widget,user.data)
{
nexp_ncases <- getWidget("CohortStudies","spb_nexp_ncase")$getValue()
nexp_cases <- getWidget("CohortStudies","spb_nexp_case")$getValue()
exp_ncases <- getWidget("CohortStudies","spb_exp_ncase")$getValue()
exp_cases <- getWidget("CohortStudies","spb_exp_case")$getValue()
calc <- c(nexp_ncases,nexp_cases,exp_ncases,exp_cases)
if(any(calc == 0))
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(FALSE)
else
	getWidget("CohortStudies","btn_CCT_execute")$setSensitive(TRUE)

}