
shinyServer(function(input, output) {
  
  
  output$trendPlot <- renderPlotly({
    # size of the bins depend on the input 'bins'
    if(input$var == "Passagens" ){
      gg <- ggplot(total.long,aes(x = Deputado, y =Gastos, z=Partido, fill=Legenda))+
        geom_bar(stat="identity",position="dodge") +
        labs(x='Deputado', y='Total Gasto com Bilhetes Aereos') + 
        theme(panel.background=element_blank()) +
        coord_fixed(ratio = 2.0) +
        coord_flip()
        
      # Convert the ggplot to a plotly
      p <- ggplotly(gg)
      p
    }else if(input$var == "Gastos Gerais"){
      gg <- ggplot(gastosPorParlamentar_ck, mapping = aes(x = 1, y = Gastos, z = Deputado)) + 
      geom_boxplot() + geom_point( aes(color=factor(Partido)), 
      position=position_dodge(width=0.75) ) + 
      labs(x='--', y='Gastos dos Deputados', color = "Partido")
      p <- ggplotly(gg)
      p
    }else if(input$var == "Fornecedores"){
      gg <-ggplot(topFornecedores_ck, aes(x = Fornecedor, y = ValorRecebido, z=Partido)) +
        geom_point(aes(colour = Deputado)) + 
        coord_flip()  + ylab("Valor gasto") + 
        xlab("Fornecedor")
       
      p <- ggplotly(gg)
      p
    }else{
      p <- ggplot()
    }
  })
  
  output$text1 <- renderText({ 
    if(input$var == "Passagens" ){
      paste("O gráfico abaixo tem como objetivo mostrar um pouco de como se comporta o gráfico
        dos deputados cearenses em relação ao gasto de passagens àereas para terceiros.
        Podemos ver a partir deste gráfico várias informações, tais como: 
        Nome do Deputado, Total Gasto com exatidão, Partido e além de tudo, 
        pode comparar o valor gasto com passagens aéreas para terceiras com o total 
        gasto pelo deputado em questão. Condensando várias informações em somente um 
        gráfico sem ocorrer um grande overhead de informações.
        Fazendo uma analise simples, podemos ver que os deputados Vicente Arruda do PDT 
        e o deputado Ronaldo Martins do PRB, foram os deputados que mais gastaram com 
        passagens para terceiro. Além disso, a partir deste gráfico, é póssivel verificar 
        que o deputado Martins gasta proporcionalmente bastante com terceiros, já que a 
        diferença entre os dois gŕafico é bem pequena. Por outro lado, podemos ver 
        que o deputado Anibal Gomes do PMDB gasta bastante com bilhetes aereos, porém, 
        bem pouco com bilhetes aereos para terceiros. Para filtrar os dados, basta
        clicar na legenda")
    }else if(input$var == "Gastos Gerais"){
      paste('O gráfico abaixo tem como objetivo fazer um apanhado geral dos gastos dos deputados
          cearenses. A partir do boxPlot abaixo podemos ver que o maior gasto foi do deputado 
         Ronaldo Martins com aproximadamente 245.300 reais e o menor gasto foi do deputado 
         Mauro Benevides com aproximadamente 18.200 reais. 
         Além disso, podemos analisar pelo gráfico que o ponto
         central desses dados foi de aproximadamente 175.400 reais A diferença entre
         o primeiro e o terceiro quartil indicam que cerca de 50% dos deputados ficaram 
         entre 98.100 reais e 215.000 reais em gastos. Caso queira-se, também é possível visualizar
         apenas os partidos que se deseja clicando nos botões da legenda. Também é possível ver o valor
         exato e o nome do deputado passando o mouse em cima dos pontos.
         A linha central que aparece neste gráfico é a mediana(valor central), o primeiro quartil é 
         o inicio do retângulo e o terceiro quartil o fechamento deste retângulo')
    }else if(input$var == "Fornecedores"){
      paste("Observação: Talvez seja necessário deixar a página em fullscreen para 
            visualizar este gráfico. Este gráfico tem o objetivo de analisar a dispersão
            dos gastos dos deputados cearenses em relação aos fornecedores.
            Pelo gráfico abaixo podemos notar que apesar das empresas de aviação 
            são as que possuem uma menor dispersão ou valores estranhos. 
            O ponto extremo na empresa Eder Holanda de carvalho ME na verdade equivale a 3 e 
            equivalem a todo o dinheiro recebido por esta empresa em relação aos deputados cerenses. 
            Todos os gastos foram feitos pelo mesmo deputado cearense de nome Macedo . 
            Que coincidentemente é um dos deputados que mais gasta. 
            Outra empresa que segue um pouco do modelo da Éder Holanda de carvalho ME, 
            é a empresa Xerez Araripe Advocacia e Consultoria ME que possui quatro 
            recebimentos de 15.000 feitas pelo deputado Leônidas Cristino. O gráfico
            já está filtrado para as 13 empresas que mais gastam em fortaleza. Isto foi feito
            para não gerar um overhead de informação. Contudo, segundo sugestão, isto pode
            ser alterado futuramente. Assim como os outros, é póssivel filtrar(clicando na
            legenda) e 
            dar zoom(selecionado com seu mouse)")
    }else{
      paste("Este site tem como objetivo mostrar várias analises dos deputados cearense.
            Basta escolher a analise que você deseja no lado esquerdo e pronto. A base de todos os
            gráficos construidos fora shiny + plotly. Isto é uma atividade referente a disciplina Analise de dados 1
            da Universidade Federal de Campina Grande e não tem qualquer carater político. Enjoy")
    }
  })
 

})