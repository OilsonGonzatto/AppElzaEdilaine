source('00_packages.R', encoding='UTF8', local=TRUE)

novaFichaMACC_UI <- function(id) {
  ns = NS(id)
  h = 100
  
  load('www/listaMunicipios.RData')
# ------------------------------------------------------------------------------ Informacoes gerais -----
  fluidRow(
    column(
      width = 12,
      tags$span(
        style = 'color:#3C8DBC;', 
        h3(tags$strong('Informações Gerais'))
      )
    ),
    column(
      width = 12,
      style = 'padding-left:10px; padding-right:0px; margin-left:-10px; margin-top:10px; margin-bottom:-10px;',
      column(
        width = 6,
        textInput(
          inputId=ns('nomeCompleto'),
          label='Nome completo',
          value='',
          width='100%'
        )
      ),
      column(
        width = 6,
        style = 'padding-left:0px; padding-right:0px;',
        column(
          width = 4,
          selectInput(
            inputId=ns('sexo'),
            label='Sexo',
            choices=c('','Masculino'='M','Feminino'='F','Intersexo'='I'),
            selected = '',
            width='100%'
          )
        ),
        column(
          width = 4,
          dateInput(
            inputId=ns('dataNascimento'),
            label='Data de nascimento',
            format='dd-mm-yyyy',
            value = NA,
            width='100%'
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId=ns('racaCor'),
            label='Raça/Cor',
            choices = c('Não declarada'='','Amarela'='A','Indígena'='I','Branca'='B','Parda'='P','Negra'='N'),
            selected = '',
            width='100%'
          )
        )
      )
    ),
    column(
      width = 12,
      style = 'padding-left:10px; padding-right:0px; padding-bottom:-10px; margin-left:-10px;',
      column(
        width = 6,
        textInput(
          inputId=ns('nomeMae'),
          label='Nome da mãe',
          value=NULL,
          width='100%'
        )
      ),
      column(
        width = 6,
        style = 'padding-left:0px; padding-right:0px;',
        column(
          width = 4,
          selectInput(
            inputId=ns('municipio'),
            label='Município',
            choices=c('',municipios),
            width='100%'
          )
        ),
        column(
          width = 4,
          textInput(
            inputId=ns('ubs'),
            label='UBS',
            value=NULL,
            width='100%'
          )
        ),
        column(
          width = 4,
          textInput(
            inputId=ns('cns'),
            label='nº do CNS',
            value='',
            width='100%'
          )
        )
      )
    ),
    column(
      width = 12,
      style = 'padding-bottom:25px;',
      textInput(
        inputId = ns('profissionalResponsavel'),
        label = 'Profissional Responsável pela Estratificação:',
        width = '100%'
      )
    ),
    column(
      width = 12,
      tabsetPanel(
        type = 'pills',
        tabPanel(
          title = 'Questionário MACC',
          tagList(
            column(
              width = 12,
              style = 'padding-top:25px; padding-left:15px; padding-right:15px;',
              fluidRow(
                useShinyjs(),
                tags$style("label{color:#3C8DBC;}"),
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------ Fatores de risco cardiovascular -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;', 
                    h3(tags$strong('Fatores de risco cardiovascular'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66',
                    fluidRow(
                      column(
                        width = 12,
                        style = 'padding-left:0px; padding-right:0px',
                        column(
                          width = 2,
                          uiOutput(ns('ui-macc-01-sexo')),
                          uiOutput(ns('out-macc-01-sexo'))
                        ),
                        column(
                          width = 2,
                          uiOutput(ns('ui-macc-02-idade')),
                          uiOutput(ns('out-macc-02-idade'))
                        ),
                        column(
                          width = 2,
                          checkboxGroupButtons(
                            inputId = ns('macc-03-tabagismo'),
                            label = '(3) Tabagismo',
                            choices = c('Fuma ou deixou de fumar <br> a menos de 1 ano.'='S'), # list('S') %>% set_names( HTML(paste0(strwrap('Fuma ou deixou de fumar <br> a menos de 1 ano.',width=30), collapse='</br>')) ),
                            selected = NA,
                            width = '100%',
                            direction = 'horizontal',
                            status = 'primary',
                            justified = TRUE
                          ),
                          tags$script(paste0("$(\"input:checkbox[name='",ns('macc-03-tabagismo'),"']\").parent().css('height', '",h,"px')")),
                          uiOutput(ns('out-macc-03-tabagismo'))
                        ),
                        column(
                          width = 3,
                          checkboxGroupButtons(
                            inputId = ns('macc-04-historiaDCV'),
                            label = '(4) História de DCV',
                            choices = list('S') %>% set_names( 'Pai e irmãos antes dos 55 anos; ou<br>mãe e irmãs antes dos 65 anos.' ),
                            selected = NA,
                            width = '100%',
                            direction = 'horizontal',
                            status = 'primary',
                            justified = TRUE
                          ),
                          tags$script(paste0("$(\"input:checkbox[name='",ns('macc-04-historiaDCV'),"']\").parent().css('height', '",h,"px')")),
                          uiOutput(ns('out-macc-04-historiaDcv'))
                        ),
                        column(
                          width = 3,
                          uiOutput(ns('ui-macc-05-obesidade')),
                          uiOutput(ns('out-macc-05-obesidade'))
                        ),
                        #
                        column(
                          width = 12,
                          style = 'padding-bottom:30px;'
                        ),
                        column(
                          width = 12,
                          tags$span(
                            style = 'color:#3C8DBC;', 
                            h5(tags$strong('(6) Dislipidemias (e datas de coleta)'))
                          )
                        ),
                        column(
                          width = 3,
                          style = 'padding-left:15px; padding-right:5px',
                          numericInput(
                            inputId = ns('macc-06a-dislipidemiaCt'),
                            label = 'CT',
                            value = 0,
                            min = 0,
                            max = 500,
                            step = 1,
                            width = '100%'
                          ),
                          dateInput(
                            inputId = ns('macc-06a-dataDislipidemiaCt'),
                            label = NULL,
                            value = Sys.Date()
                          )
                        ),
                        column(
                          width = 3,
                          style = 'padding-left:5px; padding-right:5px',
                          numericInput(
                            inputId = ns('macc-06b-dislipidemiaTg'),
                            label = 'TG',
                            value = 0,
                            min = 0,
                            max = 500,
                            step = 1,
                            width = '100%'
                          ),
                          dateInput(
                            inputId = ns('macc-06b-dataDislipidemiaTg'),
                            label = NULL,
                            value = Sys.Date()
                          )
                        ),
                        column(
                          width = 3,
                          style = 'padding-left:5px; padding-right:5px',
                          numericInput(
                            inputId = ns('macc-06c-dislipidemiaLdl'),
                            label = 'LDL',
                            value = 0,
                            min = 0,
                            max = 500,
                            step = 1,
                            width = '100%'
                          ),
                          dateInput(
                            inputId = ns('macc-06c-dataDislipidemiaLdl'),
                            label = NULL,
                            value = Sys.Date()
                          )
                        ),
                        column(
                          width = 3,
                          style = 'padding-left:5px; padding-right:15px',
                          numericInput(
                            inputId = ns('macc-06d-dislipidemiaHdl'),
                            label = 'HDL',
                            value = 0,
                            min = 0,
                            max = 500,
                            step = 1,
                            width = '100%'
                          ),
                          dateInput(
                            inputId = ns('macc-06d-dataDislipidemiaHdl'),
                            label = NULL,
                            value = Sys.Date()
                          )
                        ),
                        #
                        column(
                          width = 12,
                          uiOutput(ns('out-macc-06-dislipidemia'))
                        ),
                        #
                        column(
                          width = 12,
                          style = 'padding-bottom:30px;'
                        ),
                        column(
                          width = 12,
                          tags$span(
                            style = 'color:#3C8DBC;', 
                            h5(tags$strong('(7) Pré-diabetes (e datas de coleta)'))
                          )
                        ),
                        column(
                          width = 4,
                          style = 'padding-left:15px; padding-right:5px',
                          numericInput(
                            inputId = ns('macc-07a-preDiabetesGlicemiaJejum'),
                            label = 'Glicemia em Jejum',
                            value = 0,
                            min = 0,
                            max = 500,
                            step = 1,
                            width = '100%'
                          ),
                          dateInput(
                            inputId = ns('macc-07a-dataPreDiabetesGlicemiaJejum'),
                            label = NULL,
                            value = Sys.Date()
                          )
                        ),
                        column(
                          width = 4,
                          style = 'padding-left:5px; padding-right:5px',
                          numericInput(
                            inputId = ns('macc-07b-preDiabetesHb1ac'),
                            label = 'Hb1AC',
                            value = 0,
                            min = 0,
                            max = 100,
                            step = 0.1,
                            width = '100%'
                          ),
                          dateInput(
                            inputId = ns('macc-07b-dataPreDiabetesHb1ac'),
                            label = NULL,
                            value = Sys.Date()
                          )
                        ),
                        column(
                          width = 4,
                          style = 'padding-left:5px; padding-right:15px',
                          numericInput(
                            inputId = ns('macc-07c-preDiabetesTesteOralToleranciaGlicose'),
                            label = 'Teste oral de tolerância',
                            value = 0,
                            min = 0,
                            max = 500,
                            step = 1,
                            width = '100%'
                          ),
                          dateInput(
                            inputId = ns('macc-07c-dataPreDiabetesTesteOralToleranciaGlicose'),
                            label = NULL,
                            value = Sys.Date()
                          )
                        ),
                        #
                        column(
                          width = 12,
                          uiOutput(ns('out-macc-07-preDiabetes'))
                        )
                      )
                    )
                  )
                ),
          # ------------------------------------------------------------------------------ 
          # ------------------------------------------------------------------------------ Informações complementares -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;',
                    h3(tags$strong('Informações complementares'))
                  ),
                  column(
                    width = 12,
                    style = 'padding-left:0px; padding-right:10px',
                    wellPanel(
                      style = 'background: #d1f0ff66',
                      fluidRow(
                        column(
                          width = 12,
                          tags$span(
                            style = 'color:#3C8DBC;',
                            h5(tags$strong('(8) Taxa de filtração glomerular e função renal'))
                          ),
                          column(
                            width = 12,
                            style = 'padding-left:0px; padding-right:0px',
                            column(
                              width = 4,
                              style = 'padding-left:0px; padding-right:5px',
                              numericInput(
                                inputId = ns('macc-08a-tfg'),
                                label = 'Creatinina Plasmática (mg/dL)', # 'TFG (mL/min)',
                                value = 0,
                                min = 0,
                                max = 500,
                                step = 1,
                                width = '100%'
                              )
                            ),
                            column(
                              width = 4,
                              dateInput(
                                inputId = ns('macc-08a-dataTfg'),
                                label = 'Data de registro',
                                value = Sys.Date()
                              )
                            ),
                            column(
                              width = 4,
                              uiOutput(ns('out-macc-08-tfg'))
                            )
                          ),
                          tags$span(
                            style = 'color:#3C8DBC;',
                            h5(tags$strong('(9) Controle pressórico'))
                          ),
                          column(
                            width = 6,
                            style = 'padding-left:0px; padding-right:10px;',
                            numericInput(
                              inputId = ns('macc-09a-pressaoSistolica'),
                              label = 'Pressão sistólica (mmHg)',
                              value = 0,
                              min = 0,
                              max = 0,
                              step = 1,
                              width = '100%'
                            )
                          ),
                          column(
                            width = 6,
                            style = 'padding-left:0px; padding-right:10px;',
                            numericInput(
                              inputId = ns('macc-09b-pressaoDiastolica'),
                              label = 'Pressão diastólica (mmHg)',
                              value = 0,
                              min = 0,
                              max = 0,
                              step = 1,
                              width = '100%'
                            )
                          )
                        )
                      )
                    )
                  )
                ),
          # ------------------------------------------------------------------------------
          # ------------------------------------------------------------------------------
                uiOutput(ns('apenasHipertensos')),
          # ------------------------------------------------------------------------------
          # ------------------------------------------------------------------------------
                uiOutput(ns('apenasDiabeticos')),
          # ------------------------------------------------------------------------------
          # ------------------------------------------------------------------------------ Estratificação do risco cardiovascular global -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;',
                    h3(tags$strong('Estratificação do risco cardiovascular global'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66;',
                    fluidRow(
                      column(
                        width = 12,
                        style = 'margin-bottom:-15px;',
                        tableOutput(ns('estratificacaoDoRiscoCardiovascularGlobal'))
                      )
                    )
                  )
                ),
          # ------------------------------------------------------------------------------
          # ------------------------------------------------------------------------------ Estratificação do risco metabolico -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;',
                    h3(tags$strong('Estratificação do risco metabólico'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66;',
                    fluidRow(
                      column(
                        width = 12,
                        style = 'margin-bottom:-15px;',
                        tableOutput(ns('estratificacaoDoRiscoMetabolico'))
                      )
                    )
                  )
                ),
          # ------------------------------------------------------------------------------
          # ------------------------------------------------------------------------------ Historico de cuidado na aps (referencia) -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;',
                    h3(tags$strong('Histórico de cuidado na APS (referência)'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66;',
                    fluidRow(
                      column(
                        width = 12,
                        style = 'margin-bottom:-15px;',
                        textAreaInput(
                          inputId = ns('macc-25-motivacaoParaAAE'),
                          label = 'Qual a principal motivação para o encaminhamento para a AAE?',
                          width = '100%'
                        ),
                        textAreaInput(
                          inputId = ns('macc-26-tratamentoAtual'),
                          label = 'Qual o tratamento atualmente instituído (farmacológico e não farmacológico)?',
                          width = '100%'
                        ),
                        textAreaInput(
                          inputId = ns('macc-27-dificuldadesPelaAPS'),
                          label = 'Quais as dificuldades enfrentadas pela APS no cuidado do usuário?',
                          width = '100%'
                        )
                      )
                    )
                  )
                )
          # ------------------------------------------------------------------------------
              )
            )
          )
        ),
        tabPanel(
          title = 'Questionário ICVF-20',
          tagList(
            tags$style("label{color:#3C8DBC;}"),
            fluidRow(
              useShinyjs(),
              column(
                width = 12,
                column(
                  width = 12,
                  style = 'padding-top:15px',
                  # tags$span(
                  #   style = 'color:#3C8DBC;', 
                  #   h3(tags$strong('ÍNDICE DE VULNERABILIDADE CLÍNICO-FUNCIONAL-20'))
                  # ),
                  wellPanel(
                    fluidRow(
                      column(
                        width = 12,
                        tags$span(
                          style = 'color:#3C8DBC;', 
                          h4(tags$strong('Responda às perguntas abaixo com a ajuda de familiares ou acompanhantes. Marque a opção mais apropriada para a sua condição de saúde atual. Todas as respostas devem ser confirmadas por alguém que conviva com você.'))
                        )
                      ),
                      column(
                        width = 12,
                        tags$span(
                          style = 'color:#3C8DBC;', 
                          h4(tags$strong('Nos idosos incapazes de responder, utilizar as respostas do cuidador.'))
                        )
                      )
                    )
                  )
                ),
                # ------------------------------------------------------------------------------
                # ------------------------------------------------------------------------------ Idade -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;', 
                    h4(tags$strong('IDADE'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66',
                    fluidRow(
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-01'),
                          label = '01. Qual é a sua idade',
                          choices = c('60 a 74 anos'='0',
                                      '75 a 84 anos'='1',
                                      '>= 85 anos'='3'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                        # tags$script(paste0("$(\"input:checkbox[name='",ns('macc03Tabagismo'),"']\").parent().css('height', '",h,"px')")),
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-01'))
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                # ------------------------------------------------------------------------------
                # ------------------------------------------------------------------------------ Auto-percepcao da saude -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;', 
                    h4(tags$strong('AUTO-PERCEPÇÃO DA SAÚDE'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66',
                    fluidRow(
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-02'),
                          label = '02. Em geral, comparando com outras pessoas de sua idade, você diria que sua saúde é:',
                          choices = c('Excelente, muito boa ou boa'='0',
                                      'Regular ou ruim'='1'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-02'))
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                # ------------------------------------------------------------------------------
                # ------------------------------------------------------------------------------ Auto-percepcao da saude -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;', 
                    h4(tags$strong('ATIVIDADES DE VIDA DIÁRIA'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66',
                    fluidRow(
                      column(
                        width = 12,
                        tags$span(
                          style = 'color:#3C8DBC;', 
                          h4(tags$strong('AVD Instrumental'))
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-03'),
                          label = '03. Por causa de sua saúde ou condição física, você deixou de fazer compras?',
                          choices = c('Sim'='4',
                                      'Não ou não faz compras por outros motivos que não a saúde'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        ),
                        radioGroupButtons(
                          inputId = ns('ivcf-04'),
                          label = '04. Por causa de sua saúde ou condição física, você deixou de controlar seu dinheiro, gastos ou pagar as contas de sua casa?',
                          choices = c('Sim'='4',
                                      'Não ou não controla o dinheiro por outros motivos que não a saúde'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        ),
                        radioGroupButtons(
                          inputId = ns('ivcf-05'),
                          label = '05. Por causa de sua saúde ou condição física, você deixou de realizar pequenos trabalhos domésticos, como lavar louça, arrumar a casa ou fazer limpeza leve?',
                          choices = c('Sim'='4',
                                      'Não ou não faz mais pequenos trabalhos domésticos por outros motivos que não a saúde'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:87px; padding-bottom:87px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-03-04-05'))
                            )
                          )
                        )
                      ),
                      column(
                        width = 12,
                        tags$span(
                          style = 'color:#3C8DBC;', 
                          h4(tags$strong('AVD Básica'))
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-06'),
                          label = '06. Por causa de sua saúde ou condição física, você deixou de tomar banho sozinho?',
                          choices = c('Sim'='6',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-06'))
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                # ------------------------------------------------------------------------------
                # ------------------------------------------------------------------------------ Cognicao -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;', 
                    h4(tags$strong('COGNIÇÃO'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66',
                    fluidRow(
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-07'),
                          label = '07. Algum familiar ou amigo falou que você está ficando esquecido?',
                          choices = c('Sim'='1',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-07'))
                            )
                          )
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-08'),
                          label = '08. Este esquecimento está piorando nos últimos meses?',
                          choices = c('Sim'='1',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-08'))
                            )
                          )
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-09'),
                          label = '09. Este esquecimento está impedindo a realização de alguma atividade do cotidiano?',
                          choices = c('Sim'='2',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-09'))
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                # ------------------------------------------------------------------------------
                # ------------------------------------------------------------------------------ Humor -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;', 
                    h4(tags$strong('HUMOR'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66',
                    fluidRow(
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-10'),
                          label = '10. No último mês, você ficou com desânimo, tristeza ou desesperança?',
                          choices = c('Sim'='2',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-10'))
                            )
                          )
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-11'),
                          label = '11. No último mês, você perdeu o interesse ou prazer em atividades anteriormente prazerosas?',
                          choices = c('Sim'='2',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-11'))
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                # ------------------------------------------------------------------------------
                # ------------------------------------------------------------------------------ Mobilidade -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;', 
                    h4(tags$strong('MOBILIDADE'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66',
                    fluidRow(
                      column(
                        width = 12,
                        tags$span(
                          style = 'color:#3C8DBC;', 
                          h4(tags$strong('Alcance, preensão e pinça'))
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-12'),
                          label = '12. Você é incapaz de elevar os braços acima do nível do ombro?',
                          choices = c('Sim'='1',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-12'))
                            )
                          )
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-13'),
                          label = '13. Você é incapaz de manusear ou segurar pequenos objetos?',
                          choices = c('Sim'='1',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-13'))
                            )
                          )
                        )
                      ),
                      #
                      column(
                        width = 12,
                        tags$span(
                          style = 'color:#3C8DBC;', 
                          h4(tags$strong('Capacidade aeróbica e/ou muscular'))
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-14'),
                          label = HTML('14. Você tem alguma das quatro condições abaixo relacionadas?</br></br>
                        •	Perda de peso não intencional de 4,5 kg ou 5% do peso corporal no último ano ou 6 kg nos últimos 6 meses ou 3 kg no último mês;</br>
                        •	Índice de Massa Corporal (IMC) menor que 22 kg/m2;</br>
                        •	Circunferência da panturrilha a < 31 cm;</br>
                        •	Tempo gasto no teste de velocidade da marcha (4m) > 5 segundos.'),
                          choices = c('Sim'='2',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:125px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-14'))
                            )
                          )
                        )
                      ),
                      #
                      column(
                        width = 12,
                        tags$span(
                          style = 'color:#3C8DBC;', 
                          h4(tags$strong('Marcha'))
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-15'),
                          label = '15. Você tem dificuldade para caminhar capaz de impedir a realização de alguma atividade do cotidiano?',
                          choices = c('Sim'='2',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-15'))
                            )
                          )
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-16'),
                          label = '16. Você teve duas ou mais quedas no último ano?',
                          choices = c('Sim'='2',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-16'))
                            )
                          )
                        )
                      ),
                      #
                      column(
                        width = 12,
                        tags$span(
                          style = 'color:#3C8DBC;', 
                          h4(tags$strong('Continência esfincteriana'))
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-17'),
                          label = '17. Você perde urina ou fezes, sem querer, em algum momento?',
                          choices = c('Sim'='2',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-17'))
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                # ------------------------------------------------------------------------------
                # ------------------------------------------------------------------------------ Comunicacao -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;', 
                    h4(tags$strong('COMUNICAÇÃO'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66',
                    fluidRow(
                      # column(
                      #   width = 12,
                      #   tags$span(
                      #     style = 'color:#3C8DBC;', 
                      #     h4(tags$strong('Visão'))
                      #   )
                      # ),
                      # column(
                      #   width = 11,
                      #   style = 'padding-right:10px;',
                      #   radioGroupButtons(
                      #     inputId = ns('ivcf-18'),
                      #     label = '18. Você tem problemas de visão capazes de impedir a realização de alguma atividade do cotidiano? É permitido o uso de óculos ou lentes de contato.',
                      #     choices = c('Sim'='2',
                      #                 'Não'='0'),
                      #     selected = NA,
                      #     width = '100%',
                      #     direction = 'horizontal',
                      #     # size = 'sm',
                      #     status = 'primary',
                      #     justified = TRUE
                      #   )
                      # ),
                      # column(
                      #   width = 1,
                      #   style = 'margin-top:25px; padding-left:10px;',
                      #   wellPanel(
                      #     style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                      #     fluidRow(
                      #       column(
                      #         width = 12,
                      #         style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                      #         htmlOutput(ns('pts-ivcf-18'))
                      #       )
                      #     )
                      #   )
                      # ),
                      # #
                      column(
                        width = 12,
                        tags$span(
                          style = 'color:#3C8DBC;', 
                          h4(tags$strong('Visão'))
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-18'),
                          label = '18. Você tem problemas de visão capazes de impedir a realização de alguma atividade do cotidiano? É permitido o uso de óculos ou lentes de contato.',
                          choices = c('Sim'='2',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-18'))
                            )
                          )
                        )
                      ),
                      #
                      column(
                        width = 12,
                        tags$span(
                          style = 'color:#3C8DBC;', 
                          h4(tags$strong('Audição'))
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-19'),
                          label = '19. Você tem problemas de audição capazes de impedir a realização de alguma atividade do cotidiano? É permitido o uso de aparelhos de audição.',
                          choices = c('Sim'='2',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:25px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-19'))
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                # ------------------------------------------------------------------------------
                # ------------------------------------------------------------------------------ Comorbidades -----
                column(
                  width = 12,
                  tags$span(
                    style = 'color:#3C8DBC;', 
                    h4(tags$strong('COMORBIDADES MÚLTIPLAS'))
                  ),
                  wellPanel(
                    style = 'background: #d1f0ff66',
                    fluidRow(
                      column(
                        width = 12,
                        tags$span(
                          style = 'color:#3C8DBC;', 
                          h4(tags$strong('Polipatologia | Polifarmácia | Internação recente (<6 meses)'))
                        )
                      ),
                      column(
                        width = 11,
                        style = 'padding-right:10px;',
                        radioGroupButtons(
                          inputId = ns('ivcf-20'),
                          label = HTML('20. Você tem alguma das três condições abaixo relacionadas?</br></br>
                        •	Cinco ou mais doenças crônicas;</br>
                        •	Uso regular de cinco ou mais medicamentos diferentes, todo dia;</br>
                        •	Internação recente, nos últimos 6 meses'),
                          choices = c('Sim'='4',
                                      'Não'='0'),
                          selected = NA,
                          width = '100%',
                          direction = 'horizontal',
                          # size = 'sm',
                          status = 'primary',
                          justified = TRUE
                        )
                      ),
                      column(
                        width = 1,
                        style = 'margin-top:105px; padding-left:10px;',
                        wellPanel(
                          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
                          fluidRow(
                            column(
                              width = 12,
                              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
                              htmlOutput(ns('pts-ivcf-20'))
                            )
                          )
                        )
                      )
                    )
                  )
                )
# ------------------------------------------------------------------------------
              )
            )
          )
        ),
        column(
          width = 12,
          downloadButton(
            outputId = ns('imprimeFicha'),
            label = 'IMPRIMIR RESULTADO',
            icon = icon('print'),
            class = 'btn-lg',
            style = 'padding:10px; font-size:140%; color: #fff; background-color: #337ab7; border-color: #2e6da4; width:100%'
          )
        )
      )
    )
  )
}

novaFichaMACC_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns = session$ns
    
# ==============================================================================    
# = MACC =======================================================================    
# ==============================================================================
    
    h = 100
    riscoNAO =
      tagList(
        wellPanel(
          style = 'background: #B8FFBD; padding-top:5px; margin-bottom:-5px;',
          fluidRow(
            column(
              width = 12,
              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
              HTML('<p style="text-align:center; color:#00B70E; font-weight:bold;">NÃO</p>') 
            )
          )
        )
      )
    riscoSIM =
      tagList(
        wellPanel(
          style = 'background: #FFC5B8; padding-top:5px; margin-bottom:-5px;',
          fluidRow(
            column(
              width = 12,
              style = 'padding-left:0px; padding-right:0px; padding-bottom:-15px; margin-bottom:-25px;',
              HTML('<p style="text-align:center; vertical-align:bottom; color:#DA0000; font-weight:bold;">SIM</p>') 
            )
          )
        )
      )
    
    # ---
    
    # ---
    
    observeEvent({ input[['cns']] },{
      req( input[['cns']] )
      
      auxCns = input[['cns']]
      if ( nchar(auxCns)>15 ) {
        updateTextInput(
          session = session,
          inputId = 'cns',
          value = str_trunc(auxCns,width=15,ellipsis='')
        )
      }
    })
    
    aux_idade <- reactiveVal()
    observeEvent({ input[['dataNascimento']] },{
      req( input[['dataNascimento']] )
      
      auxDataNascimento = input[['dataNascimento']]
      auxIdade = as.numeric(floor(difftime(time1=Sys.Date(), time2=auxDataNascimento, units='days')/365))
      aux_idade( auxIdade )
    }, priority=1000 )
    
    aux_macc01Sexo = reactiveVal(0)
    aux_choiceIdade = reactiveVal()
    aux_choiceObesidade = reactiveVal()
    aux_choiceEcocardiograma = reactiveVal()
    observeEvent({ input[['macc-01-sexo']] },{
      if ( is.null(input[['macc-01-sexo']]) ) { 
        aux_macc01Sexo(0) 
        aux_choiceIdade( c('Idade > 65 anos?'='S') )
        aux_choiceObesidade( c('IMC >= 30 kg/m2<br>Circunferência abdominal >= 88 cm'='S') )
        aux_choiceEcocardiograma( c('Índice de massa<br>ventricular esquerda<br> > 95 g/m2'='S') )
      } else { 
        aux_macc01Sexo(1) 
        aux_choiceIdade( c('Idade > 55 anos?'='S') )
        aux_choiceObesidade( c('IMC >= 30 kg/m2<br>Circunferência abdominal > 102 cm'='S') )
        aux_choiceEcocardiograma( c('Índice de massa<br>ventricular esquerda<br> > 115 g/m2'='S') )
      }
    }, ignoreNULL=FALSE, priority=1000 )
    
    output[['ui-macc-01-sexo']] <- renderUI({
    
      if ( input[['sexo']]=='M' ) { selected='S' } else { selected=NA }
      tagList(
        checkboxGroupButtons(
          inputId = ns('macc-01-sexo'),
          label = '(1) Sexo', 
          choices = c('Sexo masculino?'='S'),
          selected = selected,
          width = '100%',
          direction = 'horizontal',
          status = 'primary',
          justified = TRUE
        ),
        tags$script(paste0("$(\"input:checkbox[name='",ns('macc-01-sexo'),"']\").parent().css('height', '",h,"px')"))
      )
    })
    output[['out-macc-01-sexo']] <- renderUI({
      if ( is.null(input[['macc-01-sexo']]) ) { return( riscoNAO ) } else { return( riscoSIM ) }
    })
    
    
    aux_macc02Idade = reactiveVal(0)
    observeEvent({ input[['macc-02-idade']] },{
      if ( is.null(input[['macc-02-idade']]) ) { 
        aux_macc02Idade(0) 
      } else { 
        aux_macc02Idade(1) 
      }
    }, ignoreNULL=FALSE, priority=1000 )
    output[['ui-macc-02-idade']] <- renderUI({
      
      if ( is.null(aux_idade()) ) { selected=NA } else { if ( aux_idade()>65 ) { selected='S' } else { selected=NA } }
      tagList(
        checkboxGroupButtons(
          inputId = ns('macc-02-idade'),
          label = '(2) Idade',
          choices = aux_choiceIdade(),
          selected = selected,
          width = '100%',
          direction = 'horizontal',
          status = 'primary',
          justified = TRUE
        ),
        tags$script(paste0("$(\"input:checkbox[name='",ns('macc-02-idade'),"']\").parent().css('height', '",h,"px')"))
      )
    })
    output[['out-macc-02-idade']] <- renderUI({
      if ( is.null(input[['macc-02-idade']]) ) { return( riscoNAO ) } else { return( riscoSIM ) }
    })
    
    
    aux_macc03Tabagismo = reactiveVal(0)
    observeEvent({ input[['macc-03-tabagismo']] },{
      if ( is.null(input[['macc-03-tabagismo']]) ) { aux_macc03Tabagismo(0) } else { aux_macc03Tabagismo(1) }
    }, ignoreNULL=FALSE, priority=1000 )
    output[['out-macc-03-tabagismo']] <- renderUI({
      if ( is.null(input[['macc-03-tabagismo']]) ) { return( riscoNAO ) } else { return( riscoSIM ) }
    })
    
    
    aux_macc04HistoriaDcv = reactiveVal()
    observeEvent({ input[['macc-04-historiaDcv']] },{
      if ( is.null(input[['macc-04-historiaDcv']]) ) { aux_macc04HistoriaDcv(0) } else { aux_macc04HistoriaDcv(1) }
    }, ignoreNULL=FALSE, priority=1000 )
    output[['out-macc-04-historiaDcv']] <- renderUI({
      if ( is.null(input[['macc-04-historiaDCV']]) ) { return( riscoNAO ) } else { return( riscoSIM ) }
    })
    
    
    aux_macc05Obesidade = reactiveVal(0)
    observeEvent({ input[['macc-05-obesidade']] },{
      if ( is.null(input[['macc-05-obesidade']]) ) { aux_macc05Obesidade(0) } else { aux_macc05Obesidade(1) }
    }, ignoreNULL=FALSE, priority=1000 )
    output[['ui-macc-05-obesidade']] <- renderUI({
      tagList(
        checkboxGroupButtons(
          inputId = ns('macc-05-obesidade'),
          label = '(5) Obesidade',
          choices = aux_choiceObesidade(),
          selected = NA,
          width = '100%',
          direction = 'horizontal',
          status = 'primary',
          justified = TRUE
        ),
        tags$script(paste0("$(\"input:checkbox[name='",ns('macc-05-obesidade'),"']\").parent().css('height', '",h,"px')"))
      )
    })
    output[['out-macc-05-obesidade']] <- renderUI({
      if ( is.null(input[['macc-05-obesidade']]) ) { return( riscoNAO ) } else { return( riscoSIM ) }
    })
    
    
    aux_macc06Dislipidemia = reactiveVal()
    observeEvent({ c(
      input[['macc-06a-dislipidemiaCt']], input[['macc-06b-dislipidemiaTg']], 
      input[['macc-06c-dislipidemiaLdl']], input[['macc-06d-dislipidemiaHdl']]
    ) },{
      req( 
        input[['macc-06a-dislipidemiaCt']], input[['macc-06b-dislipidemiaTg']], 
        input[['macc-06c-dislipidemiaLdl']], input[['macc-06d-dislipidemiaHdl']] 
      )
      
      if ( input[['macc-06a-dislipidemiaCt']]>190 | 
           input[['macc-06b-dislipidemiaTg']]>150 | 
           input[['macc-06c-dislipidemiaLdl']]>115 | 
           ( input[['macc-06d-dislipidemiaHdl']]<40 & !is.null(input[['macc-01-sexo']]) ) | 
           ( input[['macc-06d-dislipidemiaHdl']]<46 & is.null(input[['macc-01-sexo']]) ) ) { 
        aux_macc06Dislipidemia(1) 
      } else { 
        aux_macc06Dislipidemia(0) 
      }
      
    })
    output[['out-macc-06-dislipidemia']] <- renderUI({ 
      req( aux_macc06Dislipidemia() )
      
      if ( aux_macc06Dislipidemia()==0 ) { riscoNAO } else { riscoSIM } 
    })


    aux_macc07PreDiabetes = reactiveVal()
    observeEvent({ c( 
      input[['macc-07a-preDiabetesGlicemiaJejum']],
      input[['macc-07b-preDiabetesHb1ac']],
      input[['macc-07c-preDiabetesTesteOralToleranciaGlicose']]
    ) },{
      req( 
        input[['macc-07a-preDiabetesGlicemiaJejum']],
        input[['macc-07b-preDiabetesHb1ac']],
        input[['macc-07c-preDiabetesTesteOralToleranciaGlicose']]
      )
      
      if ( ( input[['macc-07a-preDiabetesGlicemiaJejum']]>110 & input[['macc-07a-preDiabetesGlicemiaJejum']]<125 ) |
           ( input[['macc-07b-preDiabetesHb1ac']]>5.7 & input[['macc-07b-preDiabetesHb1ac']]<6.4 ) |
           ( input[['macc-07c-preDiabetesTesteOralToleranciaGlicose']]>140 & input[['macc-07c-preDiabetesTesteOralToleranciaGlicose']]<199 ) ) { 
        aux_macc07PreDiabetes(1)
      } else { 
        aux_macc07PreDiabetes(0)
      }
      
    })
    output[['out-macc-07-preDiabetes']] <- renderUI({
      req( aux_macc07PreDiabetes() )
      
      if ( aux_macc07PreDiabetes()==1 ) { riscoSIM } else { riscoNAO }
    })
    
    aux_classificacaoTfg = reactiveVal()
    output[['out-macc-08-tfg']] <- renderUI({
      req( input[['macc-08a-tfg']], input[['dataNascimento']] )
      
      auxCreatinina = input[['macc-08a-tfg']]
      auxDataNascimento = input[['dataNascimento']]
      
      req( auxCreatinina>0 )
      
      auxIdade = aux_idade()
      RFG = 186*auxCreatinina**(-1.154)*auxIdade**(-0.203) # (ml/min/1.73m2)
      
      auxSexo = is.null(input[['macc-01-sexo']])
      if ( auxSexo ) { RFG = RFG*0.742 }
      auxRaca = input[['racaCor']]=='N'
      if ( auxSexo ) { RFG = RFG*1.210 }
      auxAlbuminuria = is.null(input[['macc-15-aaa']])
      # browser()
      if ( RFG>=90 ) {
        if ( auxAlbuminuria ) { auxClassificacao = 'Sem lesão' } else { auxClassificacao = '1' }
        cor = '#00B70E'; corT = '#B8FFBD'
      } else 
      if ( RFG>=60 ) {
        auxClassificacao = '2'
        cor = '#00B70E'; corT = '#B8FFBD'
      } else 
      if ( RFG>=45 ) {
        auxClassificacao = '3A'
        cor = '#FFC300'; corT = '#FFF3CC'
      } else 
      if ( RFG>=30 ) {
        auxClassificacao = '3B'
        cor = '#FF6A00'; corT = '#FCCDAB'
      } else 
      if ( RFG>=15 ) {
        auxClassificacao = '4'
        cor = '#DA0000'; corT = '#FFC5B8'
      } else {
        auxClassificacao = '5'
        cor = '#DA0000'; corT = '#FFC5B8'
      }
      
      tfgNomes = c(
        'IRC estando ou não em TRS'='5', 
        'TFG severamente diminuída'='4', 
        'TFG moderadamente diminuída'='3B', 
        'TFG moderadamente diminuída'='3A', 
        'TFG levemente diminuída + Lesão renal'='2', 
        'TFG normal ou aumentada + Lesão renal'='1', 
        'TFG normal ou aumentada'='Sem lesão'
      )
      
      auxClassificacaoTexto = paste0(auxClassificacao,' (',names(tfgNomes)[tfgNomes==auxClassificacao],')')  
      aux_classificacaoTfg( auxClassificacaoTexto )
      
      tagList(
        wellPanel(
          style = paste0('background:',corT,'; padding-top:20px; margin-bottom:-10px;'),
          fluidRow(
            column(
              width = 12,
              style = 'padding-left:0px; padding-right:0px; padding-bottom:0px; margin-bottom:-10px;',
              HTML('<p style="text-align:center; vertical-align:bottom; color:',cor,'; font-weight:bold;">',auxClassificacaoTexto,'</p>') 
            )
          )
        )
      )
      
    })
    
    output[['apenasHipertensos']] <- renderUI({
     
    aux = 
      tagList(
# ------------------------------------------------------------------------------ Apenas para hipertensos -----
# ------------------------------------------------------------------------------ Lesão de órgão alvo e doenças existentes -----
          column(
            width = 12,
            tags$span(
              style = 'color:#3C8DBC;',
              h3(tags$strong('Lesão de órgão alvo e doenças existentes (apenas para hipertensos)'))
            ),
            wellPanel(
              style = 'background: #d1f0ff66',
              fluidRow(
# ------------------------------------------------------------------------------ linha 1 -----
                column(
                  width = 12,
                  column(
                    width = 6,
                    style = 'padding-left:0px; padding-right:0px',
                    tags$span(
                      style = 'color:#3C8DBC;',
                      h5(tags$strong('(10) Hipertrofia ventricular esquerda (LOA)'))
                    ),
                    column(
                      width = 6,
                      style = 'padding-left:5px; padding-right:5px',
                      checkboxGroupButtons(
                        inputId = ns('macc-10a-eletrocardiograma'),
                        label = 'Eletrocardiograma',
                        choices = c('Sokolol-Lyon (SV1+RVS ou RV 6) <br> >= 35 mm <br> RaVL > 11 mm <br> Cornell voltagem > 2440 mm*ms'='S'),
                        selected = NA,
                        width = '100%',
                        direction = 'horizontal',
                        # size = 'sm',
                        status = 'primary',
                        justified = TRUE
                      ),
                      tags$script(paste0("$(\"input:checkbox[name='",ns('macc-10a-eletrocardiograma'),"']\").parent().css('height', '",1.35*h,"px')"))
                    ),
                    column(
                      width = 6,
                      style = 'padding-left:5px; padding-right:5px',
                      checkboxGroupButtons(
                        inputId = ns('macc-10b-ecocardioagrama'),
                        label = 'Ecocardiograma',
                        choices = aux_choiceEcocardiograma(),
                        selected = NA,
                        width = '100%',
                        direction = 'horizontal',
                        status = 'primary',
                        justified = TRUE
                      ),
                      tags$script(paste0("$(\"input:checkbox[name='",ns('macc-10b-ecocardioagrama'),"']\").parent().css('height', '",1.35*h,"px')"))
                    )
                  ),
                  column(
                    width = 3,
                    style = 'padding-left:0px; padding-right:0px;',
                    h5(tags$strong(' ')),
                    column(
                      width = 12,
                      style = 'padding-left:5px; padding-right:5px; padding-top:25px;',
                      checkboxGroupButtons(
                        inputId = ns('macc-11-aaa'),
                        label = '(11)',
                        choices = c('<b>Espessura mediointimal <br> da carótida (LOA)</b> <br> > 0,9 mm ou placa carotídea'='S'),
                        selected = NA,
                        width = '100%',
                        direction = 'horizontal',
                        # size = 'sm',
                        status = 'primary',
                        justified = TRUE
                      ),
                      tags$script(paste0("$(\"input:checkbox[name='",ns('macc-11-aaa'),"']\").parent().css('height', '",1.35*h,"px')"))
                    )
                  ),
                  column(
                    width = 3,
                    style = 'padding-left:0px; padding-right:0px',
                    h5(tags$strong(' ')),
                    column(
                      width = 12,
                      style = 'padding-left:5px; padding-right:5px; padding-top:25px;',
                      checkboxGroupButtons(
                        inputId = ns('macc-12-aaa'),
                        label = '(12)',
                        choices = c('<b>Velocidade da onda de <br> pulso carótido-femoral (LOA)</b> <br> > 10 m/s'='S'),
                        selected = NA,
                        width = '100%',
                        direction = 'horizontal',
                        # size = 'sm',
                        status = 'primary',
                        justified = TRUE
                      ),
                      tags$script(paste0("$(\"input:checkbox[name='",ns('macc-12-aaa'),"']\").parent().css('height', '",1.35*h,"px')"))
                    )
                  )
                ),
# ------------------------------------------------------------------------------ linha 2 -----
                column(
                  width = 12,
                  column(
                    width = 3,
                    style = 'padding-left:0px; padding-right:0px',
                    # h5(tags$strong('(13)')),
                    column(
                      width = 12,
                      style = 'padding-left:5px; padding-right:5px',
                      checkboxGroupButtons(
                        inputId = ns('macc-13-aaa'),
                        label = '(13)',
                        choices = c('<b>Índice tornozelo-braquial (LOA)</b> <br> < 0,9'='S'),
                        selected = NA,
                        width = '100%',
                        direction = 'horizontal',
                        # size = 'sm',
                        status = 'primary',
                        justified = TRUE
                      ),
                      tags$script(paste0("$(\"input:checkbox[name='",ns('macc-13-aaa'),"']\").parent().css('height', '",1.35*h,"px')"))
                    )
                  ),
                  column(
                    width = 3,
                    style = 'padding-left:0px; padding-right:0px',
                    # h5(tags$strong('(14)')),
                    column(
                      width = 12,
                      style = 'padding-left:5px; padding-right:5px',
                      checkboxGroupButtons(
                        inputId = ns('macc-14-aaa'),
                        label = '(14)',
                        choices = c('<b>Doença renal crônica (LOA)</b> <br> Estágio 3 <br> (ritmo de filtração estimado entre <br> 30 e 60 mL/min/1,73 m3)'='S'),
                        selected = NA,
                        width = '100%',
                        direction = 'horizontal',
                        # size = 'sm',
                        status = 'primary',
                        justified = TRUE
                      ),
                      tags$script(paste0("$(\"input:checkbox[name='",ns('macc-14-aaa'),"']\").parent().css('height', '",1.35*h,"px')"))
                    )
                  ),
                  column(
                    width = 3,
                    style = 'padding-left:0px; padding-right:0px',
                    # h5(tags$strong('(15)')),
                    column(
                      width = 12,
                      style = 'padding-left:5px; padding-right:5px;',
                      checkboxGroupButtons(
                        inputId = ns('macc-15-aaa'),
                        label = '(15)',
                        choices = c('<b>Albuminúria (LOA)</b> <br> Entre 30 e 300 mg/24h ou <br> Relação albumina-creatinina <br> urinária de 30 a 300 mg.'='S'),
                        selected = NA,
                        width = '100%',
                        direction = 'horizontal',
                        # size = 'sm',
                        status = 'primary',
                        justified = TRUE
                      ),
                      tags$script(paste0("$(\"input:checkbox[name='",ns('macc-15-aaa'),"']\").parent().css('height', '",1.35*h,"px')"))
                    )
                  ),
                  column(
                    width = 3,
                    style = 'padding-left:0px; padding-right:0px',
                    # h5(tags$strong('(16)')),
                    column(
                      width = 12,
                      style = 'padding-left:5px; padding-right:5px;',
                      checkboxGroupButtons(
                        inputId = ns('macc-16-aaa'),
                        label = '(16)',
                        choices = c('<b>Doença cérebro-vascular (DCV)</b> <br> Acidente vascular encefálico isquêmico; <br> Hemorragia cerebral ou <br> ataque isquêmico trnasitório.'='S'),
                        selected = NA,
                        width = '100%',
                        direction = 'horizontal',
                        # size = 'sm',
                        status = 'primary',
                        justified = TRUE
                      ),
                      tags$script(paste0("$(\"input:checkbox[name='",ns('macc-16-aaa'),"']\").parent().css('height', '",1.35*h,"px')"))
                    )
                  )
                ),
# ------------------------------------------------------------------------------ linha 3 -----
                column(
                  width = 12,
                  column(
                    width = 5,
                    style = 'padding-left:0px; padding-right:0px',
                    # h5(tags$strong('(17)')),
                    column(
                      width = 12,
                      style = 'padding-left:5px; padding-right:5px',
                      checkboxGroupButtons(
                        inputId = ns('macc-17-aaa'),
                        label = '(17)',
                        choices = c('<b>Doença da artéria coronária (DAC)</b> <br><font size="-1"> Anginina estável o instável; Infarto do miocárdio; <br> Revascularização do miocárdio percutânea (angioplastia) ou cirúrgica; <br> Insuficiência caríaca com fração de ejeção reduzida ou preservada; <br> Doença arterial perférica sintomática dos membros inferiores; <br> Retinopatia avançada (hemorragias, exsudatos e papiledema)</font>.'='S'),
                        selected = NA,
                        width = '100%',
                        direction = 'horizontal',
                        # size = 'sm',
                        status = 'primary',
                        justified = TRUE
                      ),
                      tags$script(paste0("$(\"input:checkbox[name='",ns('macc-17-aaa'),"']\").parent().css('height', '",1.75*h,"px')"))
                    )
                  ),
                  column(
                    width = 4,
                    style = 'padding-left:0px; padding-right:0px',
                    # h5(tags$strong('(18)')),
                    column(
                      width = 12,
                      style = 'padding-left:5px; padding-right:5px;',
                      checkboxGroupButtons(
                        inputId = ns('macc-18-aaa'),
                        label = '(18)',
                        choices = c('<b>Doença renal crônica (DRC)</b> <br> Estágio >= 4 <br> (ritmo de filtração estimado <br> >= 15 mL/min/1,73 m3).'='S'),
                        selected = NA,
                        width = '100%',
                        direction = 'horizontal',
                        # size = 'sm',
                        status = 'primary',
                        justified = TRUE
                      ),
                      tags$script(paste0("$(\"input:checkbox[name='",ns('macc-18-aaa'),"']\").parent().css('height', '",1.75*h,"px')"))
                    )
                  ),
                  column(
                    width = 3,
                    style = 'padding-left:0px; padding-right:0px',
                    # h5(tags$strong('(19)')),
                    column(
                      width = 12,
                      style = 'padding-left:5px; padding-right:5px;',
                      checkboxGroupButtons(
                        inputId = ns('macc-19-aaa'),
                        label = '(19)',
                        choices = c('<b>Diabetes mellitus (DM)</b>.'='S'),
                        selected = NA,
                        width = '100%',
                        direction = 'horizontal',
                        # size = 'sm',
                        status = 'primary',
                        justified = TRUE
                      ),
                      tags$script(paste0("$(\"input:checkbox[name='",ns('macc-19-aaa'),"']\").parent().css('height', '",1.75*h,"px')"))
                    )
                  )
                )
# ------------------------------------------------------------------------------
              )
            )
          )
        )
# ------------------------------------------------------------------------------
      return( aux )
    })
    
    output[['apenasDiabeticos']] <- renderUI({
# ------------------------------------------------------------------------------ Apenas para diabeticos -----
# ------------------------------------------------------------------------------ Diagnostico, controle e complicacoes do diabetes -----
    aux = 
      tagList(
        column(
          width = 12,
          tags$span(
            style = 'color:#3C8DBC;',
            h3(tags$strong('Diagnóstico, controle e complicações do diabetes (apenas para diabéticos)'))
          ),
          wellPanel(
            style = 'background: #d1f0ff66',
            fluidRow(
              column(
                width = 6,
                checkboxGroupButtons(
                  inputId = ns('macc-20-DiabetesMelittus'),
                  label = '(20) Diabetes melittus',
                  choices = c('<b>Tipo I</b>'='I', '<b>Tipo II</b>'='II'),
                  selected = NA,
                  width = '100%',
                  direction = 'vertical',
                  # size = 'sm',
                  status = 'primary',
                  justified = TRUE
                ),
                tags$script(paste0("$(\"input:checkbox[name='",ns('macc-20-DiabetesMelittus'),"']\").parent().css('height', '",0.75*h,"px')")),
                checkboxGroupButtons(
                  inputId = ns('macc-21-ControleMetabolico'),
                  label = '(21) Controle metabólico',
                  choices = c('<b>Hb1Ac <= 7%</b>'='S'),
                  selected = NA,
                  width = '100%',
                  direction = 'vertical',
                  # size = 'sm',
                  status = 'primary',
                  justified = TRUE
                ),
                tags$script(paste0("$(\"input:checkbox[name='",ns('macc-21-ControleMetabolico'),"']\").parent().css('height', '",0.56*h,"px')")),
                checkboxGroupButtons(
                  inputId = ns('macc-22-InternacaoComplicacaoAguda'),
                  label = '(22) Internação por complicação aguda nos últimos 12 meses',
                  choices = c('<b>Hipoglicemia</b>'='H', '<b>Cetoacidose</b>'='C', '<b>Síndrome hiperosmolar não cetótica</b>'='S'),
                  selected = NA,
                  width = '100%',
                  direction = 'vertical',
                  # size = 'sm',
                  status = 'primary',
                  justified = TRUE
                ),
                tags$script(paste0("$(\"input:checkbox[name='",ns('macc-22-InternacaoComplicacaoAguda'),"']\").parent().css('height', '",0.56*h,"px')"))
              ),
              column(
                width = 6,
                checkboxGroupButtons(
                  inputId = ns('macc-23-Macroangiopatias'),
                  label = '(23) Complicações crônicas (Macroangiopatias)',
                  choices = c('<b>Doença arterial coronariana (DAC)</b>'='DAC',
                              '<b>Doença cerebrovascular (DCV)</b>'='DCV',
                              '<b>Doença vascular periférica (DVP)</b>'='DVP'),
                  selected = NA,
                  width = '100%',
                  direction = 'vertical',
                  # size = 'sm',
                  status = 'primary',
                  justified = TRUE
                ),
                tags$script(paste0("$(\"input:checkbox[name='",ns('macc-23-Macroangiopatias'),"']\").parent().css('height', '",0.52*h,"px')")),
                checkboxGroupButtons(
                  inputId = ns('macc-24-Microangiopatias'),
                  label = '(24) Complicações crônicas (Microangiopatias)',
                  choices = c('<b>Retinopatia diabética</b>'='R',
                              '<b>Doença renal diabética</b>'='D',
                              '<b>Insuficiência renal crônica</b>'='I',
                              '<b>Pé diabético</b>'='P',
                              '<b>Neuropatia sensitivo-motora</b>'='N'),
                  selected = NA,
                  width = '100%',
                  direction = 'vertical',
                  # size = 'sm',
                  status = 'primary',
                  justified = TRUE
                ),
                tags$script(paste0("$(\"input:checkbox[name='",ns('macc-24-Microangiopatias'),"']\").parent().css('height', '",0.52*h,"px')"))
              )
# ------------------------------------------------------------------------------
            )
          )
        )
      )
      
    return( aux )
  })
    
    aux_riscoCardiovascular <- reactiveVal()
    output[['estratificacaoDoRiscoCardiovascularGlobal']] <- function() { # renderTable({
      req( 
        input[['macc-09a-pressaoSistolica']],
        input[['macc-09b-pressaoDiastolica']]
      )
      
      macc09PressaoSistolica = input[['macc-09a-pressaoSistolica']]
      macc09PressaoDiastolica = input[['macc-09b-pressaoDiastolica']]

      macc01sexo = input[['macc-01-sexo']]
      macc02idade = input[['macc-02-idade']]
      macc03tabagismo = input[['macc-03-tabagismo']]
      macc04historiaDcv = input[['macc-04-historiaDCV']]
      macc05obesidade = input[['macc-05-obesidade']]

      soma =
        aux_macc01Sexo() + aux_macc02Idade() + aux_macc03Tabagismo() +
        aux_macc04HistoriaDcv() + aux_macc05Obesidade() + aux_macc06Dislipidemia() +
        aux_macc07PreDiabetes()

      linha = 0
      if ( soma==0 ) {
        linha = 1
      } else if ( soma==1 | soma==2 ) {
        linha = 2
      } else if ( soma>=3 ) {
        linha = 3
      }

      coluna = 0
      if ( macc09PressaoSistolica >= 180 & macc09PressaoDiastolica >= 110 ) {
        coluna = 6
      } else if ( macc09PressaoSistolica >= 160 & macc09PressaoDiastolica >= 100 ) {
        coluna = 5
      } else if ( macc09PressaoSistolica >= 140 & macc09PressaoDiastolica >= 90 ) {
        coluna = 4
      } else if ( macc09PressaoSistolica >= 130 & macc09PressaoDiastolica >= 85 ) {
        coluna = 3
      }

      aux =
        tibble(
          C0 = c(' ','nº "SIM"<br>(perguntas 1-7)','nº "SIM"<br>(perguntas 1-7)','nº "SIM"<br>(perguntas 1-7)',' '),
          C1 = c('Fatores<br>de Risco','0','1 a 2','>=3','LOA, DCV, DAC, DM ou<br>DRC ESTÁGIO >= 4'),
          C2 = c('Limítrofe<br>PAS: 130-139<br>PAD: 85-89','Sem risco adicional','Baixo risco','Moderado risco','Alto risco'),
          C3 = c('Estágio 1<br>PAS: 140-159<br>PAD: 90-99','Baixo risco','Moderado risco','Alto risco','Alto risco'),
          C4 = c('Estágio 2<br>PAS: 160-179<br>PAD: 100-109','Moderado risco','Alto risco','Alto risco','Alto risco'),
          C5 = c('Estágio 3<br>PAS: >= 180<br>PAD: >= 110','Alto risco','Alto risco','Alto risco','Alto risco')
        )

      # aux =
      #   tibble(
      #     C0 = c(' ',
      #            '\\parbox{.25\\linewidth}{nº "SIM"\\\\(perguntas 1-7)}',
      #            '\\parbox{.25\\linewidth}{nº "SIM"\\\\(perguntas 1-7)}',
      #            '\\parbox{.25\\linewidth}{nº "SIM"\\\\(perguntas 1-7)}',
      #            ' '),
      #     C1 = c('\\parbox{.25\\linewidth}{Fatores\\\\de Risco',
      #            '0','1 a 2','>=3',
      #            '\\parbox{.25\\linewidth}{LOA, DCV, DAC, DM ou\\\\DRC ESTÁGIO >= 4}'),
      #     C2 = c('\\parbox{.25\\linewidth}{Limítrofe\\\\PAS: 130-139\\\\PAD: 85-89}',
      #            'Sem risco adicional', 'Baixo risco', 'Moderado risco', 'Alto risco'),
      #     C3 = c('\\parbox{.25\\linewidth}{Estágio 1\\\\PAS: 140-159\\\\PAD: 90-99}',
      #            'Baixo risco', 'Moderado risco', 'Alto risco', 'Alto risco'),
      #     C4 = c('\\parbox{.25\\linewidth}{Estágio 2\\\\PAS: 160-179\\\\PAD: 100-109}',
      #            'Moderado risco','Alto risco','Alto risco','Alto risco'),
      #     C5 = c('\\parbox{.25\\linewidth}{Estágio 3\\\\PAS: >= 180\\\\PAD: >= 110}',
      #            'Alto risco','Alto risco','Alto risco','Alto risco')
      #   )

      names(aux) <- aux %>% slice(1) %>% unlist()
      aux <- aux %>% slice(-1)

      if ( coluna>2 & linha>0 ) {
        if ( aux[linha,coluna,drop=TRUE]=='Sem risco adicional' ) cor='green'
        else if ( aux[linha,coluna,drop=TRUE]=='Baixo risco' ) cor='gold'
        else if ( aux[linha,coluna,drop=TRUE]=='Moderado risco' ) cor='orange'
        else if ( aux[linha,coluna,drop=TRUE]=='Alto risco' ) cor='red'
        
        aux[,coluna] = cell_spec(
          aux[,coluna,drop=T],
          color = ifelse(1:nrow(aux) == linha, 'white', 'black'),
          bold = ifelse(1:nrow(aux) == linha, T, F)
        )

        AUX =
          aux %>%
            knitr::kable('html', escape=FALSE, align=rep('c',times=6), booktabs = T, full_width = T) %>%
            column_spec(column=1:6, background = 'white') %>%
            column_spec(column=c(1,2), bold=T) %>%
            row_spec(linha, background = cor, color = cor) %>%
            row_spec(c(1:4)[c(1:4)!=linha], background = 'white', color = 'black') %>% # #FFC5B8
            column_spec(c(1:6)[c(1:6)!=coluna], background = 'white', bold=F) %>%
            column_spec(c(1:6)[c(1:6)!=coluna], color = 'black', bold=F) %>%
            collapse_rows(1, valign='middle') %>%
            kable_styling('bordered', full_width=T) %>%
            # kable_classic(full_width=T) %>%
            add_header_above(c(' ', ' ', 'PRESSÃO (pergunta 9)'=4), background = 'white', bold = T) %>%
            row_spec(0, background = 'white', bold = T)
      } else {
        AUX =
          aux %>%
            knitr::kable('html', escape=FALSE, align=rep('c',times=6), booktabs = T, full_width = T) %>%
            column_spec(column=1:6, background = 'white') %>%
            column_spec(column=c(1,2), bold=T) %>%
            kable_styling('bordered', full_width=T) %>%
            # kable_classic(full_width=T) %>%
            add_header_above(c(' ', ' ', 'PRESSÃO (pergunta 9)'=4), background = 'white', bold = T) %>%
            row_spec(0, background = 'white', bold = T) %>%
            collapse_rows(1, valign='middle')
      }
      aux_riscoCardiovascular( AUX )
      aux_riscoCardiovascular()
    }

    
    
    aux_riscoMetabolico <- reactiveVal()
    output[['estratificacaoDoRiscoMetabolico']] <- function() { # renderTable({

      macc09PressaoDiastolica = input[['macc-09b-pressaoDiastolica']]
      macc09PressaoSistolica = input[['macc-09a-pressaoSistolica']]

      macc20DiabetesMelittus = input[['macc-20-DiabetesMelittus']]
      macc20DiabetesMelittus = ifelse(is.null(macc20DiabetesMelittus),'-',macc20DiabetesMelittus)

      macc21ControleMetabolico = input[['macc-21-ControleMetabolico']]
      macc21ControleMetabolico = ifelse(is.null(macc21ControleMetabolico),'-',macc21ControleMetabolico)

      macc22InternacaoComplicacaoAguda = input[['macc-22-InternacaoComplicacaoAguda']]
      macc22InternacaoComplicacaoAguda = ifelse(is.null(macc22InternacaoComplicacaoAguda),'-',macc22InternacaoComplicacaoAguda)

      macc23Macroangiopatias = input[['macc-23-Macroangiopatias']]
      macc23Macroangiopatias = ifelse(is.null(macc23Macroangiopatias),'-',macc23Macroangiopatias)

      macc24Microangiopatias = input[['macc-24-Microangiopatias']]
      macc24Microangiopatias = ifelse(is.null(macc24Microangiopatias),'-',macc24Microangiopatias)

      coluna = 0
      if (
        (
          ( macc20DiabetesMelittus=='I' | macc20DiabetesMelittus=='II' ) &
          ( macc21ControleMetabolico!='S' | ( macc09PressaoSistolica >= 130 | macc09PressaoDiastolica >= 85 ) )
        ) | (
          macc20DiabetesMelittus=='II' & ( macc21ControleMetabolico=='S' & ( macc09PressaoSistolica < 130 & macc09PressaoDiastolica < 85 ) )
        ) & (
          macc22InternacaoComplicacaoAguda!='-' | macc23Macroangiopatias!='-' | macc24Microangiopatias!='-'
        )
      ) {
        coluna = 3
      } else if ( macc20DiabetesMelittus=='II' & macc21ControleMetabolico=='S' & macc09PressaoSistolica < 130 & macc09PressaoDiastolica < 85 & macc22InternacaoComplicacaoAguda=='-' & macc23Macroangiopatias=='-' & macc24Microangiopatias=='-' ) {
        coluna = 2
      } else if ( aux_macc07PreDiabetes()==1 ) {
        coluna = 1
      }

      aux =
        tibble(
          C0 = c('RISCO BAIXO','Pessoa com <b>pré-diabetes</b>'),
          C1 = c('RISCO MODERADO','<b>Pessoa com DM2</b>, Com controle metabólico e pressórico adequado, sem internações por complicações agudas nos últimos 12 meses e sem complicações crônicas'),
          C2 = c('RISCO ALTO','<b>Pessoa com DM1</b> ou <b>Pessoa com DM2</b>, com controle metabólico e/ou pressórico inadequado, ou <b>Pessoa com DM2 e</b> com controle metabólico e pressórico adequado e Internações por complicações agudas nos últimos 12 meses e/ou complicações crônicas.')
        )
      
      names(aux) <- aux %>% slice(1) %>% unlist()
      aux <- aux %>% slice(-1)
      # browser()
      if ( coluna>0 ) {
        if ( coluna==1 ) cor='gold'
        else if ( coluna==2 ) cor='orange'
        else if ( coluna==3 ) cor='red'
        AUX =
          aux %>%
            knitr::kable('html', escape=FALSE, align=rep('c',times=3), booktabs = T, full_width = T) %>%
            row_spec(1, background = 'white') %>%
            kable_styling('bordered', full_width=T) %>%
            column_spec(coluna, background = cor, color = 'white', bold = T) %>%
            row_spec(0, background = 'white', bold = T)
      } else {
        AUX =
          aux %>%
            knitr::kable('html', escape=FALSE, align=rep('c',times=3), booktabs = T, full_width = T) %>%
            row_spec(1, background = 'white') %>%
            kable_styling('bordered', full_width=T) %>%
            row_spec(0, background = 'white', bold = T)
      }
      aux_riscoMetabolico( AUX )
      aux_riscoMetabolico()
    }
    
# ==============================================================================    
# = IVCF20 =====================================================================    
# ==============================================================================
    
    output[['pts-ivcf-01']] <- renderText({
      if ( !is.null(input[['ivcf-01']]) ) {
        HTML(paste0('<p style="text-align:center; color:#00B70E; font-weight:bold;">',input[['ivcf-01']],'</p>')) 
      } else {
        HTML('<p style="text-align:center; color:#00B70E; font-weight:bold;">-</p>')
      }
    })
    output[['pts-ivcf-02']] <- renderText({
      if ( !is.null(input[['ivcf-02']]) ) {
        HTML(paste0('<p style="text-align:center; color:#00B70E; font-weight:bold;">',input[['ivcf-02']],'</p>')) 
      } else {
        HTML('<p style="text-align:center; color:#00B70E; font-weight:bold;">-</p>')
      }
    })
    output[['pts-ivcf-03-04-05']] <- renderText({
      if ( !is.null(input[['ivcf-03']]) ) { pts01 = as.numeric(input[['ivcf-03']]) } else { pts01 = 0 }
      if ( !is.null(input[['ivcf-04']]) ) { pts02 = as.numeric(input[['ivcf-04']]) } else { pts02 = 0 }
      if ( !is.null(input[['ivcf-05']]) ) { pts03 = as.numeric(input[['ivcf-05']]) } else { pts03 = 0 }
      
      if ( pts01 > 0 | pts02 > 0 | pts03 > 0 ) {
        HTML('<p style="text-align:center; color:#00B70E; font-weight:bold;">4</p>')
      } else {
        if ( pts01 == 0 & pts02 == 0 & pts03 == 0 ) {
          HTML('<p style="text-align:center; color:#00B70E; font-weight:bold;">-</p>')
        }
      }
    })
    output[['pts-ivcf-06']] <- renderText({
      if ( !is.null(input[['ivcf-06']]) ) { pts = as.numeric(input[['ivcf-06']]) } else { pts = 0 }
      
      if ( pts > 0 ) {
        HTML('<p style="text-align:center; color:#00B70E; font-weight:bold;">6</p>')
      } else {
        if ( pts == 0 ) {
          HTML('<p style="text-align:center; color:#00B70E; font-weight:bold;">-</p>')
        }
      }
    })
    
    nomes = paste0('ivcf-',c(paste0('0',7:9),10:13,14,15:19,20))
    lapply(X=nomes, FUN=function(nome){
      
      output[[paste0('pts-',nome)]] <- renderText({
        if ( !is.null(input[[nome]]) ) {
          HTML(paste0('<p style="text-align:center; color:#00B70E; font-weight:bold;">',input[[nome]],'</p>'))
        } else {
          HTML('<p style="text-align:center; color:#00B70E; font-weight:bold;">-</p>')
        }
      })
      
    })
    
# ==============================================================================
    
    output[['imprimeFicha']] <- downloadHandler(
      filename = paste0('Ficha_',Sys.Date(),'.html'),
      content = function(fname) {
        
        params = list(dados=list())
        
        auxRacaCor = c('Não declarada'='','Amarela'='A','Indígena'='I','Branca'='B','Parda'='P','Negra'='N')
        
        params$dados$nomeCompleto = input[['nomeCompleto']]
        params$dados$sexo = input[['sexo']]
        params$dados$dataNascimento = format(input[['dataNascimento']],'%d/%m/%Y')
        params$dados$idade = paste0(aux_idade(),' anos')
        params$dados$racaCor = names(auxRacaCor)[auxRacaCor==input[['racaCor']]]
        params$dados$nomeMae = input[['nomeMae']]
        params$dados$municipio = input[['municipio']]
        params$dados$ubs = input[['ubs']]
        params$dados$cns = input[['cns']]
        params$dados$profissional = input[['profissionalResponsavel']]
        
        params$dados$riscoCardiovascular = aux_riscoCardiovascular()
        params$dados$riscoMetabolico = aux_riscoMetabolico()
        params$dados$motivacaoParaAAE = input[['macc-25-motivacaoParaAAE']]
        params$dados$tratamentoAtual = input[['macc-26-tratamentoAtual']]
        params$dados$dificuldadesPelaAPS = input[['macc-27-dificuldadesPelaAPS']]
        
        params$dados$dislipidemiaCt = input[['macc-06a-dislipidemiaCt']]
        params$dados$dislipidemiaTg = input[['macc-06b-dislipidemiaTg']]
        params$dados$dislipidemiaLdl = input[["macc-06c-dislipidemiaLdl"]]
        params$dados$dislipidemiaHdl = input[["macc-06d-dislipidemiaHdl"]]
        
        params$dados$dataDislipidemiaCt = format(input[['macc-06a-dataDislipidemiaCt']],'%d/%m/%Y')
        params$dados$dataDislipidemiaTg = format(input[['macc-06b-dataDislipidemiaTg']],'%d/%m/%Y')
        params$dados$dataDislipidemiaLdl = format(input[["macc-06c-dataDislipidemiaLdl"]],'%d/%m/%Y')
        params$dados$dataDislipidemiaHdl = format(input[["macc-06d-dataDislipidemiaHdl"]],'%d/%m/%Y')
        

        params$dados$preDiabetesGlicemiaJejum = input[["macc-07a-preDiabetesGlicemiaJejum"]]
        params$dados$preDiabetesHb1ac = input[["macc-07b-preDiabetesHb1ac"]]
        params$dados$preDiabetesTesteOralToleranciaGlicose = input[["macc-07c-preDiabetesTesteOralToleranciaGlicose"]]
        
        params$dados$dataPreDiabetesGlicemiaJejum = format(input[["macc-07a-dataPreDiabetesGlicemiaJejum"]],'%d/%m/%Y')
        params$dados$dataPreDiabetesHb1ac = format(input[["macc-07b-dataPreDiabetesHb1ac"]],'%d/%m/%Y')
        params$dados$dataPreDiabetesTesteOralToleranciaGlicose = format(input[["macc-07c-dataPreDiabetesTesteOralToleranciaGlicose"]],'%d/%m/%Y')
        
        params$dados$tfgNomes = c(
          'IRC estando ou não em TRS'='5', 
          'TFG severamente diminuída'='4', 
          'TFG moderadamente diminuída'='3B', 
          'TFG moderadamente diminuída'='3A', 
          'TFG levemente diminuída + Lesão renal'='2', 
          'TFG normal ou aumentada + Lesão renal'='1', 
          'TFG normal ou aumentada'='0'
        )
        
        params$dados$tfg = input[["macc-08a-tfg"]]
        params$dados$dataTfg = format(input[["macc-08a-dataTfg"]],'%d/%m/%Y')
        params$dados$tfgClassificacao = aux_classificacaoTfg() # input[["macc-08b-tfgClassificacao"]]
        params$dados$pressaoSistolica = input[["macc-09a-pressaoSistolica"]]
        params$dados$pressaoDiastolica = input[["macc-09b-pressaoDiastolica"]]
        
        nomes = paste0('ivcf-',c(paste0('0',1:9),10:13,14,15:19,20))
        soma = 0
        soma345 = 0
        na = 0
        for ( nome in nomes ) {
          if ( !is.null(input[[nome]]) ) {
            if ( input[[nome]]!='-' ) {
              if ( nome=='ivcf-03' | nome=='ivcf-04' | nome=='ivcf-05' ) { 
                soma345 = soma345 + as.numeric(input[[nome]])
              } else {
                soma = soma + as.numeric(input[[nome]])
              }
            } else {
              na = na + 1
            }
          } else {
            na = na + 1
          }
        }
        soma = soma + min(c(4,soma345))
        params$dados$soma = soma
        params$dados$na = na
        
        
        dir.ori = 'www/ficha'
        dir.create(paste0(tempdir(),'/ficha'))
        file.copy(from=dir.ori, to=tempdir(), recursive=TRUE, copy.mode=TRUE, overwrite=TRUE)

        rmarkdown::render(
          paste0(tempdir(),'/ficha/ficha.Rmd'),
          output_file = fname,
          params = params,
          envir = new.env(parent=globalenv())
        )

        unlink(x=paste0(tempdir(),'/ficha'), recursive=TRUE, force=TRUE)
      },
      contentType = 'application/html'
    )
    
  })
}


shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(disable=TRUE),
    dashboardBody(
      novaFichaMACC_UI('OILSON')
    ),
    title='userBox'
  ),
  server = function(input, output) {
    novaFichaMACC_server('OILSON')
  }
)

