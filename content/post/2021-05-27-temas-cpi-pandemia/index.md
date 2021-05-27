---
title: Os temas da CPI da Pandemia
subtitle: Quem está falando sobre o que?

# Summary for listings and search engines
summary: A CPI da pandemia tem sido amplamente repercutida nas últimas semanas. Por isso, orientados à análise de dados, eu e o Thales Carvalho buscaremos cobrir a Comissão nos próximos dias. Começaremos, nessa primeira postagem, colocando a “bola no chão” e reunindo os fatos que permitem que o leitor entenda o caminho percorrido pela CPI até aqui.


# Date published
date: "2020-05-26T00:00:00Z"

# Date updated
lastmod: "2020-05-26T00:00:00Z"

# Is this an unpublished draft?
draft: false

# Show this page in the Featured widget?
featured: false

# Featured image
# Place an image named `featured.jpg/png` in this page's folder and customize its options here.
image:
  caption: 'Image credit: [**Senado Federal**](https://www.canva.com/)'
  focal_point: ""
  placement: 2
  preview_only: false

authors:
- admin

tags:
- CPI Pandemia
- Text as data
- Structural Topic Model
- Data Analysis

---

## Overview

A Comissão Parlamentar de Inquérito (CPI) sobre a atuação do governo ao longo da pandemia tem recebido grande atenção da opinião pública. Os depoimentos têm batido recordes de visualizações entre os vídeos contidos no canal da TV Senado no YouTube, chegando ao pico de 1.1 milhão de visualizações, no caso do depoimento do ex-ministro Eduardo Pazuello.

A CPI é o instrumento que assegura aos parlamentares o direito de exercer sua função fiscalizadora sem que haja a obstrução de outros grupos parlamentares. Assim, para que possa ser estabelecida três requisitos precisam ser preenchidos: 1) assinatura de um terço dos integrantes da Casa; 2) indicação de fato determinado a ser apurado; e 3) definição de prazo certo para duração. Por isso, uma vez cumprida essas determinações, não cabe à Presidência da Casa, nesse caso, ao Senador Rodrigo Pacheco (DEM - MG), análise política ou omissão na determinação do estabelecimento do instrumento, mas sim sua instauração. 

No entanto, a CPI da pandemia, ainda que tenha seguido todos os ritos institucionais foi instaurada a partir de uma decisão do STF, especificamente do Ministro Barroso atendendo a um pedido feito pelos senadores Alessandro Vieira (Cidadania-SE) e Jorge Kajuru (Cidadania-GO). A instauração da Comissão se veria atravessada pela necessidade de “apurar as ações e omissões do governo federal no enfrentamento da pandemia da covid-19 no Brasil e, em especial, no agravamento da crise sanitária no Amazonas com a ausência de oxigênio para os pacientes internados”. Com a iminência da sua instauração, a resposta governista veio na forma de ampliação do escopo da investigação. Inicialmente centrada no governo federal, um requerimento feito pelo senador Eduardo Girão (Pode-CE) propôs também discutir os repasses federais feitos aos Estados e municípios. Nesse sentido, a CPI tem servido como uma arena de embate entre governo e oposição amplamente acompanhada e repercutida pela sociedade.


{{< figure src="https://s2.glbimg.com/8h9DjboljD9WJbClLT0VcFByrW4=/0x0:1200x630/984x0/smart/filters:strip_icc()/i.s3.glbimg.com/v1/AUTH_59edd422c0c84a879bd37670ae4f538a/internal_photos/bs/2021/K/U/MjTsgFRfKio5AczSvRgw/cpipandemiasenadores.png" title="Composição da CPI" >}}

## Coleta de Dados

No site do Senado Federal coletamos todas as notas taquigráficas referentes às sessões da CPI em que estiveram para depor Luiz Henrique Mandetta (04/05), Nelson Teich (05/05), Marcelo Queiroga (06/05), Antonio Barra Torres (11/05), Fabio Wajngarten (12/05), Carlos Murillo (13/05),  Ernesto Araújo (18/05), Eduardo Pazuello (19 e 20/05). O trabalho continuará ao longo dos próximos depoimentos.


## Análise dos Dados

Analisamos os dados utilizando um algoritmo de machine learning, por meio da modelagem de tópicos estruturais (STM).


## Relatório de pesquisa completo:     
   
<iframe src="https://docs.google.com/presentation/d/e/2PACX-1vRG6IOBZbDAtG8Tx3PIJFsrHrK1w4qvYSA8foZ1WL9DuNyGd3mudAUalZNYfCXfOw/embed?start=false&loop=false&delayms=3000" frameborder="0" width="700" height="569" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe>

{{% callout note %}}
Clique Ctrl + Shift + F para apresentação em tela cheia.
{{% /callout %}}


## Resumo  

[__Projeto__] Cobertura da CPI da Pandemia

[__Sobre__] Sobre o projeto:  

O projeto propõe cobrir a CPI da pandemia no Senado Federal orientado à análise de dados. Neste primeiro relatório, nos debruçamos sobre os debates ocorrido ao longo das sessões, por meio das notas taquigráficas das oportunidades em que o Senado Federal inquiriu um conjunto de depoentes entre os dias 04 e 20 de maio de 2020 para mobilizar estratégias de Mineração de Textos. Empregamos, para isso, ferramentas de análise de texto como dados (*text as data*).  


[__Tecnologia__] Ferramentas utilizadas:

-*linguagem de programação*: R   
-*IDE de desenvolvimento*: RStudio       
-*Pacotes e Frameworks*: tm, stringr, dplyr, tidyverse, stm

