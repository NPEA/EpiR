setup_env <- function(gui)
{
    # setting the environments
    .EpiREnv <<- new.env() # internal 
    .EpiRTrash <<- new.env() # trash can
    
    # library directory
    assign("EPIR_DIR",file.path(path.package("EpiR",quiet=TRUE)[1]),envir=.EpiREnv)
    
    # using gui or not
    if(gui)
    {
        assign("EPIR_GUI",TRUE,envir=.EpiREnv)
    }
    else
        assign("EPIR_GUI",FALSE,envir=.EpiREnv)
    
    # description
    assign("EPIR_VER",packageDescription("EpiR",fields="Version"),envir=.EpiREnv)
    assign("EPIR_HOMEPAGE",packageDescription("EpiR",fields="URL"),envir=.EpiREnv)
    
    # load the splash screen
    assign("EPIR_CRAN","http://cran.fiocruz.br/",envir=.EpiREnv)
    
    # paths
    # location of the gui glade files
    assign("EPIR_GUI_DIR",file.path(path.package("EpiR",quiet=TRUE)[1],"etc","gui"),envir=.EpiREnv)
    assign("EPIR_PATH_ETC",file.path(path.package("EpiR",quiet=TRUE)[1],"etc"),envir=.EpiREnv)
    assign("EPIR_PATH_DOC",file.path(path.package("EpiR",quiet=TRUE)[1],"etc","doc"),envir=.EpiREnv)
    assign("EPIR_PATH_PLUGINS",file.path(path.package("EpiR",quiet=TRUE)[1],"etc","plugins"),envir=.EpiREnv)
    
    # fonts
    assign("EPIR_TEXT_FONT","monospace 10",envir=.EpiREnv)
    
    # colors
    assign("EPIR_DEFAULT_COLOR","#000000",envir=.EpiREnv)
    assign("EPIR_CURRENT_COLOR","#000000",envir=.EpiREnv)
    
    # printing
    assign("EPIR_HEADER_HEIGHT",(10*72/25.4),envir=.EpiREnv)
    assign("EPIR_HEADER_GAP",(3*72/25.4),envir=.EpiREnv)
    
    # sytem
    assign("EPIR_OS",Sys.info()["sysname"],envir=.EpiREnv)
    assign("EPIR_USER",Sys.info()["user"],envir=.EpiREnv)
    assign("EPIR_USER_HOME",Sys.getenv("HOME"),envir=.EpiREnv)
    assign("EPIR_USER_CONF",paste(Sys.getenv("HOME"),"/.EpiR.conf",sep=""),envir=.EpiREnv)
    assign("EPIR_LOCALE","UTF-8",envir=.EpiREnv)
    
    # avoid warnings
    assign("EPIR_WARN_STATE",getOption("warn"),envir=.EpiREnv)
    options(warn=-1)
    
    #logical values and constants in Portuguese
    assign("V",TRUE,envir=.EpiREnv)
    assign("F",FALSE,envir=.EpiREnv)
    assign("VERDADEIRO",TRUE,envir=.EpiREnv)
    assign("FALSO",FALSE,envir=.EpiREnv)
    assign("NULO",NULL,envir=.EpiREnv)
    
    # options
    assign("EPIR_OPTION_DIGITS",4,envir=.EpiREnv)
    assign("EPIR_OPTION_DIGITS_PVALUE",4,envir=.EpiREnv)
    assign("EPIR_OPTION_SAVE_WORKSPACE",FALSE,envir=.EpiREnv)
    assign("EPIR_BROWSE_NROWS",100,envir=.EpiREnv) #used to EditData.glade
    
    # operational
    assign("EPIR_TEXT_HISTORY",NULL,envir=.EpiREnv)
    assign("EPIR_EXECUTE_STATUS",FALSE,envir=.EpiREnv)
    assign("EPIR_SELECTED_GRAPHS",NULL,envir=.EpiREnv)
    assign("EPIR_ACTIVE_DATASET",NULL,envir=.EpiREnv)
    assign("EPIR_DATE_VARIABLE",NULL,envir=.EpiREnv)
    assign("EPIR_WORKING_DIR",NULL,envir=.EpiREnv)
    
    # not wrapped  !!! MUST BE TRANSLATED LATER!!!
    assign("EPIR_PLOT_TYPES",c("ponto","linha","ponto_linha","histograma","degraus"),envir=.EpiREnv) # need translation
    assign("EPIR_PLOT_LINE_TYPES",c("auto","solida","traco","ponto","ponto_traco","traco_longo","dois_tracos"),envir=.EpiREnv) # need translation
    assign("EPIR_PLOT_POINT_TYPES",c("auto","losango","disco","disco_menor","circulo","quadrado","diamante","triangulo","triangulo_invertido"),envir=.EpiREnv) # need translation
    assign("EPIR_PLOT_OVERLAY_OPTIONS",c("nenhum","niveis","variaveis"),envir=.EpiREnv) # need translation
    assign("EPIR_PLOT_LEGEND_POSITIONS",c("alto","alto_direito","alto_esquerdo","esquerdo","direito","baixo_direito","baixo_esquerdo","baixo","centro"),envir=.EpiREnv) # need translation
    assign("EPIR_PLOT_PALETTE",c("arco_iris","cinza","quente","terrena","ciano_magenta","sem_magenta"),envir=.EpiREnv) # need translation
    assign("EPIR_TABLE_MARGINALS",c("linha","coluna","ambos","nenhum"),envir=.EpiREnv)
    assign("EPIR_REGRESSION_FUNCTIONS",c(" ","raiz","quadratica","inversa","log"),envir=.EpiREnv)
    assign("EPIR_CALC_FUNCTIONS",c("log","log10","exp","logit","alogit","quadrado","raiz2","identidade","inverso","media","mediana","centrar.media","variancia"), envir=.EpiREnv)
    assign("EPIR_HTESTS_ALTERNATIVES",c("diferente_de","maior_que","menor_que"),envir=.EpiREnv)
    assign("EPIR_SMOOTHER_METHODS",c("spline","polinomial","medias_moveis","regressao_local"),envir=.EpiREnv)
    assign("EPIR_RODBC_CASE",c("","nao_muda","maiuscula","minuscula","mysql","postgresql","oracle","msacess"),envir=.EpiREnv)
    #date
    assign("EPIR_DATE_FORMAT", c("DD","MM","AA","AAAA"),envir=.EpiREnv)
    # end not wrapped
    
    # packages
    assign("required",c("boot","bitops","caTools","grid","RGtk2","cairoDevice","stats","utils","foreign","lattice","DBI","RODBC","RSQLite","zoo","lmtest","splines","akima","gam","gtools","gdata","gplots"),envir=.EpiREnv)
    
}