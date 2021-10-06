on_btn_CCS_help_clicked <- function(widget,user.data)
{
showHelp("CaseControlStudies")
}

on_btn_CCS_cancel_clicked <- function(widget,user.data)
{
closeWindow("CaseControlStudies")
}

on_btn_CCS_execute_clicked <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_dataset"))
desf.variable <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_desf_var"))
exp.variable <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_exp_var"))
desf.case <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_desf_case"))
exposure <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_exp"))
controls.nexp <- setLocale(getWidget("CaseControlStudies","spb_nexp_cont")$getText())
controls.exp <- setLocale(getWidget("CaseControlStudies","spb_exp_cont")$getText())
cases.nexp <- setLocale(getWidget("CaseControlStudies","spb_nexp_case")$getText())
cases.exp <- setLocale(getWidget("CaseControlStudies","spb_exp_case")$getText())
chisq.test <- getWidget("CaseControlStudies","chk_CCS_chisq")$getActive()
if(chisq.test)
	chisq.test <- "VERDADEIRO"
else
	chisq.test <- "FALSO"
fisher.test <- getWidget("CaseControlStudies","chk_CCS_fisher")$getActive()
if(fisher.test)
	fisher.test <- "VERDADEIRO"
else
	fisher.test <- "FALSO"
conf.level <- getWidget("CaseControlStudies","spb_CCS_IC")$getValue()
conf.level <- conf.level/100
calc <- getWidget("CaseControlStudies","chk_CCS_calc")$getActive()
if(calc)
	calc <- "VERDADEIRO"
else
	calc <- "FALSO"
table <- c(cases.exp,controls.exp,cases.nexp,controls.nexp)
table <- as.character(table)
table <- paste("c(\"",vectorToString(table),"\")", sep = "")
#translate
if(calc=="VERDADEIRO")
	cmd <- paste("razao.chance(tabela=",table,",teste.q.quadrado=",chisq.test ,",teste.fisher=",fisher.test,",nivel.confianca=",conf.level,",calculadora=",calc,")",sep="")
else
	cmd <- paste("razao.chance(\"",dataset,"\",desfecho =\"",desf.variable,"\",casos=\"",desf.case,"\",exposicao=\"",exp.variable,"\",expostos=\"",exposure,"\",teste.q.quadrado=",chisq.test ,",teste.fisher=",fisher.test,",nivel.confianca=",conf.level,",calculadora=",calc,")",sep="")
runCommand(cmd)
closeWindow("CaseControlStudies", "CCS")
}

on_cbx_CCS_dataset_changed <- function(widget, user.data)
{
dataset <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_dataset"))
names.dataset <- names(eval(parse(text=dataset)))
ret.names.dataset <- NULL
for(i in names.dataset)
      if(nlevels(as.factor(eval(parse(text = paste(dataset,"$",i,sep=""))))) == 2)
	      ret.names.dataset <- c(ret.names.dataset,i)
fillComboBox(getWidget("CaseControlStudies","cbx_CCS_desf_var"),c("",ret.names.dataset))
fillComboBox(getWidget("CaseControlStudies","cbx_CCS_exp_var"),c("",ret.names.dataset))
}

on_cbx_CCS_desf_var_changed <- function(widget, user.data)
{
dataset <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_dataset"))
desf.variable <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_desf_var"))
exp.variable <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_exp_var"))
desf.cont <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_desf_case"))
n.exp <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_exp"))
if(desf.variable != "")
	{
	factor <- as.factor(eval(parse(text=paste(dataset,"$",desf.variable,sep=""))))
	factor <- levels(factor)
	fillComboBox(getWidget("CaseControlStudies","cbx_CCS_desf_case"),c("",factor))
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(FALSE)
	getWidget("CaseControlStudies","spb_nexp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_nexp_case")$setText(0)
	getWidget("CaseControlStudies","spb_exp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_exp_case")$setText(0)
	}
else
	fillComboBox(getWidget("CaseControlStudies","cbx_CCS_desf_case"),c(""))
}

on_cbx_CCS_exp_var_changed <- function(widget, user.data)
{
dataset <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_dataset"))
desf.variable <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_desf_var"))
exp.variable <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_exp_var"))
desf.cont <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_desf_case"))
n.exp <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_exp"))

if(exp.variable != "")
	{
	factor <- as.factor(eval(parse(text=paste(dataset,"$",exp.variable,sep=""))))
	factor <- levels(factor)
	fillComboBox(getWidget("CaseControlStudies","cbx_CCS_exp"),c("",factor))
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(FALSE)
	getWidget("CaseControlStudies","spb_nexp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_nexp_case")$setText(0)
	getWidget("CaseControlStudies","spb_exp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_exp_case")$setText(0)
	}
else
	fillComboBox(getWidget("CaseControlStudies","cbx_CCS_exp"),c(""))
}

on_cbx_CCS_desf_case_changed <- function(widget, user.data)
{
dataset <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_dataset"))
desf.variable <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_desf_var"))
exp.variable <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_exp_var"))
desf.cont <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_desf_case"))
n.exp <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_exp"))
if(desf.variable != "" && exp.variable != "" && desf.cont != "" && n.exp != "")
	{
	desf <- eval(parse(text=paste(dataset,"$",desf.variable,sep="")))
	desf <- as.factor(desf)
	expo <-  eval(parse(text=paste(dataset,"$",exp.variable,sep="")))
	expo <- as.factor(expo)
	desf <- relevel(desf,levels(desf)[levels(desf) != desf.cont])
	expo <- relevel(expo,levels(expo)[levels(expo) != n.exp])
	table <- table(expo, desf)
	getWidget("CaseControlStudies","spb_exp_case")$setText(table[2,2])	
	getWidget("CaseControlStudies","spb_nexp_case")$setText(table[1,2])
	getWidget("CaseControlStudies","spb_exp_cont")$setText(table[2,1])
	getWidget("CaseControlStudies","spb_nexp_cont")$setText(table[1,1])
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(TRUE)
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(TRUE)
	}
else
	{
	getWidget("CaseControlStudies","spb_nexp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_nexp_case")$setText(0)
	getWidget("CaseControlStudies","spb_exp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_exp_case")$setText(0)	
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(FALSE)
	}
}

on_cbx_CCS_exp_changed <- function(widget,user.data)
{
dataset <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_dataset"))
desf.variable <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_desf_var"))
exp.variable <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_exp_var"))
desf.cont <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_desf_case"))
n.exp <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_exp"))
if(desf.variable != "" && exp.variable != "" && desf.cont != "" && n.exp != "")
	{
	desf <- eval(parse(text=paste(dataset,"$",desf.variable,sep="")))
	desf <- as.factor(desf)
	expo <-  eval(parse(text=paste(dataset,"$",exp.variable,sep="")))
	expo <- as.factor(expo)
	desf <- relevel(desf,levels(desf)[levels(desf) != desf.cont])
	expo <- relevel(expo,levels(expo)[levels(expo) != n.exp])
	table <- table(expo, desf)
	getWidget("CaseControlStudies","spb_exp_case")$setText(table[2,2])	
	getWidget("CaseControlStudies","spb_nexp_case")$setText(table[1,2])
	getWidget("CaseControlStudies","spb_exp_cont")$setText(table[2,1])
	getWidget("CaseControlStudies","spb_nexp_cont")$setText(table[1,1])
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(TRUE)
	}
else
	{
	getWidget("CaseControlStudies","spb_nexp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_nexp_case")$setText(0)
	getWidget("CaseControlStudies","spb_exp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_exp_case")$setText(0)
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(FALSE)
	}
}

on_chk_CCS_calc_toggled <- function(widget, user.data)
{
dataset <- getActiveData(getWidget("CaseControlStudies","cbx_CCS_dataset"))
calc <- getWidget("CaseControlStudies","chk_CCS_calc")$getActive()
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
	fillComboBox(getWidget("CaseControlStudies","cbx_CCS_desf_var"),c("",ret.names.dataset))
	fillComboBox(getWidget("CaseControlStudies","cbx_CCS_exp_var"),c("",ret.names.dataset))
	fillComboBox(getWidget("CaseControlStudies","cbx_CCS_desf_case"),c(""))
	fillComboBox(getWidget("CaseControlStudies","cbx_CCS_exp"),c(""))
	}
	getWidget("CaseControlStudies","vbx_CCS_box_calc1")$setSensitive(TRUE)
	getWidget("CaseControlStudies","vbx_CCS_box_calc2")$setSensitive(TRUE)
	getWidget("CaseControlStudies","vbx_CCS_table1")$setSensitive(FALSE)
	getWidget("CaseControlStudies","vbx_CCS_table2")$setSensitive(FALSE)
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(FALSE)
	#restart calc
	getWidget("CaseControlStudies","spb_nexp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_nexp_case")$setText(0)
	getWidget("CaseControlStudies","spb_exp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_exp_case")$setText(0)	
	}
else
	{
	getWidget("CaseControlStudies","vbx_CCS_box_calc1")$setSensitive(FALSE)
	getWidget("CaseControlStudies","vbx_CCS_box_calc2")$setSensitive(FALSE)
	getWidget("CaseControlStudies","vbx_CCS_table1")$setSensitive(TRUE)
	getWidget("CaseControlStudies","vbx_CCS_table2")$setSensitive(TRUE)
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(FALSE)
	#restart calc
	getWidget("CaseControlStudies","spb_nexp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_nexp_case")$setText(0)
	getWidget("CaseControlStudies","spb_exp_cont")$setText(0)
	getWidget("CaseControlStudies","spb_exp_case")$setText(0)	
	}
}

on_chk_CCS_fisher_toggled <- function(widget,user.data)
{
fisher <- getWidget("CaseControlStudies","chk_CCS_fisher")$getActive()
if(fisher)
	getWidget("CaseControlStudies","hbx_CCS_IC")$setSensitive(TRUE)
else
	getWidget("CaseControlStudies","hbx_CCS_IC")$setSensitive(FALSE)
}

on_spb_nexp_cont_value_cc_changed <- function(widget,user.data)
{
nexp_cont <- getWidget("CaseControlStudies","spb_nexp_cont")$getValue()
nexp_cases <- getWidget("CaseControlStudies","spb_nexp_case")$getValue()
exp_cont <- getWidget("CaseControlStudies","spb_exp_cont")$getValue()
exp_case <- getWidget("CaseControlStudies","spb_exp_case")$getValue()
if(any(c(nexp_cont, nexp_cases,exp_cont,exp_case)==0))
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(FALSE)
else
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(TRUE)
}

on_spb_nexp_case_value_cc_changed <- function(widget,user.data)
{
nexp_cont <- getWidget("CaseControlStudies","spb_nexp_cont")$getValue()
nexp_cases <- getWidget("CaseControlStudies","spb_nexp_case")$getValue()
exp_cont <- getWidget("CaseControlStudies","spb_exp_cont")$getValue()
exp_case <- getWidget("CaseControlStudies","spb_exp_case")$getValue()
if(any(c(nexp_cont, nexp_cases,exp_cont,exp_case)==0))
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(FALSE)
else
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(TRUE)
}

on_spb_exp_cont_value_cc_changed <- function(widget,user.data)
{
nexp_cont <- getWidget("CaseControlStudies","spb_nexp_cont")$getValue()
nexp_cases <- getWidget("CaseControlStudies","spb_nexp_case")$getValue()
exp_cont <- getWidget("CaseControlStudies","spb_exp_cont")$getValue()
exp_case <- getWidget("CaseControlStudies","spb_exp_case")$getValue()
if(any(c(nexp_cont, nexp_cases,exp_cont,exp_case)==0))
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(FALSE)
else
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(TRUE)
}

on_spb_exp_case_value_cc_changed <- function(widget,user.data)
{
nexp_cont <- getWidget("CaseControlStudies","spb_nexp_cont")$getValue()
nexp_cases <- getWidget("CaseControlStudies","spb_nexp_case")$getValue()
exp_cont <- getWidget("CaseControlStudies","spb_exp_cont")$getValue()
exp_case <- getWidget("CaseControlStudies","spb_exp_case")$getValue()
if(any(c(nexp_cont, nexp_cases,exp_cont,exp_case)==0))
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(FALSE)
else
	getWidget("CaseControlStudies","btn_CCS_execute")$setSensitive(TRUE)
}
