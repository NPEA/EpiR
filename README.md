# Epi-R
Epi-R é uma bilioteca de funções e interface gráfica para análise de dados epidemiologicos em R.

O Epi-R também é uma ferramenta que pode ser usada em cursos introdutórios de Bioestatística.

Este aplicativo é desenvolvido pelo Instituto de Medicina Social da Universidade do Estado do Rio de Janeiro com finaciamento do Ministério da Saúde do Brasil.

O Epi-R é desenvolvido totalmente em R e a interface gráfica em GTk2.


## Instalação


### Windows

Para instalar o R no Windows acesse este [link](https://cloud.r-project.org/bin/windows/).

Instale [Rtools 4.0](https://cran.r-project.org/bin/windows/Rtools/).


### Linux

Estas instruções são para o Ubuntu 20.04 e outras distribuições derivadas.
Para instalar o R no Linux acesse este [link](https://cloud.r-project.org/bin/linux/).

Antes de instalar o Epi-R no Linux é necessário instalar os arquivos de desenvolvimento das bibliotecas utilizadas pelos pacotes do R. Certifique-se que o pacote r-base-dev esteja instalado.

No terminal do Linux, execute os seguiinte comando
```sh
sudo apt install r-base-dev libmysqlclient-dev libcurl4-openssl-dev libxml2-dev unixodbc-dev libcairo2-dev libxt-dev libgtk2.0-dev

```

### MacOS
Para instalar o R no MacOS acesse este [link](https://cloud.r-project.org/bin/macosx/).

Este programa não foi testado no sistema operacional MacOS.


### Etapas comuns

Instale as dependências

```r
install.packages(c("boot","bitops","caTools","RGtk2","cairoDevice","foreign","lattice","DBI","RODBC","RSQLite","zoo","lmtest","akima","gam","gtools","gdata","gplots","remotes"), dep=TRUE)
```
No Windows, antes de instalar o Epi-R, digite
```r
library(RGtk2)
```
e clique em "Instalar GTk+" para instalar a bilbioteca GTk2.

Instale o Epi-R

```r
remotes::install_github("wjunger/EpiR")
library(EpiR)
```

ou

```r
remotes::install_github("NPEA/EpiR")
library(EpiR)
```

Acesse a [página do pacote remotes](https://github.com/r-lib/remotes) para mais informações sobre como instalar pacotes do GitHub no R.

## Notas

O repositório [NPEA/EpiR](https://github.com/NPEA/EpiR) é uma bifurcação (fork) atualizada pronta para uso. Entretanto, se você pretende bifurcar o projeto use o repositório original [wjunger/EpiR](https://github.com/wjunger/EpiR).

No Windows, alguns caracteres poderão ser exibidos com erro. Este bug será corrigido em breve.


## Licença
Este software é distribuído sob a licensa GPL 2 ou superior.

Epi-R é um software livre e vem sem GARANTIA ALGUMA.
Você pode redistribuí-lo sob certas circunstâncias.
