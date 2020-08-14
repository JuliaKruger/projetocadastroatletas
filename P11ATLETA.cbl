      $set sourceformat"free"
      *>Divisão de identificação do programa
       identification division.
       program-id. "P11ATLETA".
       author. "Julia Krüger".
       installation. "PC".
       date-written. 13/08/2020.
       date-compiled. 13/08/2020.

      *>Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.

       i-o-control.

      *>Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.

      *>--- -Variaveis de trabalho
       working-storage section.
      *> variáveis que vem da tela
       01 f-tela_atletas is external-form.
           05 f-atleta.
               10 f-id-atleta                      pic 9(03)    identified by "f-id-atleta".
               10 f-nome                           pic x(50)    identified by "f-nome".
               10 f-peso                           pic 9(03)V99 identified by "f-peso".
               10 f-altura                         pic 9(03)V99 identified by "f-altura".
               10 f-imc                            pic 9(02)V99 identified by "f-imc".
               10 f-peso-ideal                     pic 9(03)V99 identified by "f-peso-ideal".
           05 f-op-salvar                          pic x(02)    identified by "f-op-salvar".
           05 f-op-deletar                         pic x(02)    identified by "f-op-deletar".
           05 f-op-buscar-um                       pic x(02)    identified by "f-op-buscar-um".
           05 f-op-buscar-proximo                  pic x(02)    identified by "f-op-buscar-proximo".
           05 f-op-buscar-anterior                 pic x(02)    identified by "f-op-buscar-anterior".
           05 f-confirmar                          pic x(06)    identified by "f-hd-confirma".
           05 f-msn                                pic x(50)    identified by "f-hd-msn".
           05 f-cf-operacao                        pic x(02)    identified by "f-hd-operacao".
           05 f-msn-erro                           pic x(50)    identified by "f-hd-msn-erro".

       01 f-tela_atletas2 is external-form identified by "tela_atletas2.html".
           05 f-atleta2.
               10 f-id-atleta2                     pic 9(03)   identified by "f-id-atleta".
               10 f-nome2                          pic x(50)   identified by "f-nome".
               10 f-peso2                          pic -999,99 identified by "f-peso".
               10 f-altura2                        pic -99,99  identified by "f-altura".
               10 f-imc2                           pic -99,99  identified by "f-imc".
               10 f-peso-ideal2                    pic -99,99  identified by "f-peso-ideal".
           05 f-op-salvar2                         pic x(02)   identified by "f-op-salvar".
           05 f-op-deletar2                        pic x(02)   identified by "f-op-deletar".
           05 f-op-buscar-um2                      pic x(02)   identified by "f-op-buscar-um".
           05 f-op-buscar-proximo2                 pic x(02)   identified by "f-op-buscar-proximo".
           05 f-op-buscar-aterior2                 pic x(02)   identified by "f-op-buscar-anterior".
           05 f-confirmar2                         pic x(06)   identified by "f-hd-confirma".
           05 f-msn2                               pic x(50)   identified by "f-hd-msn".
           05 f-cf-operacao2                       pic x(02)   identified by "f-hd-operacao".
           05 f-msn-erro2                          pic x(50)   identified by "f-hd-msn-erro".

      *> variáveis de trabalho
       01 ws-controle.
           05 ws-operacao                          pic x(02).
           05 ws-confirmacao                       pic x(01).
               88 ws-confirmar                     value "?".
               88 ws-confirmado                    value "S".
               88 ws-nao-confirmado                value "N".
           05 ws-msn                               pic x(50).
           05 ws-retorno.
               10 ws-msn-erro-pmg                  pic x(09). *> id do pmg
               10 ws-msn-erro-offset               pic 9(03). *> local do erro
               10 ws-return-code                   pic 9(02). *> status do pmg
               10 ws-msn-erro-cod                  pic x(02). *> file status
               10 ws-msn-erro-text                 pic x(50). *> mensagem de erro

       01 ws-atleta.
           05 ws-chave.
               10 ws-id-atleta                     pic 9(03).
               10 ws-nome                          pic x(50).
           05 ws-peso                              pic -99,99.
           05 ws-altura                            pic -99,99.
           05 ws-imc                               pic -99,99.
           05 ws-peso-ideal                        pic -99,99.

      *>----Variaveis para comunicação entre programas
       linkage section.

      *>----Declaração de tela
       screen section.

      *>Declaração do corpo do programa
       procedure division.

      *>------------------------------------------------------------------------
      *>  Controle das seções
      *>------------------------------------------------------------------------
       0000-controle section.
           perform 1000-inicializa
           perform 2000-processamento
           perform 3000-finaliza
           .
       0000-controle-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Procedimentos de inicialização
      *>------------------------------------------------------------------------
       1000-inicializa section.
           next sentence
           .
       1000-inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento principal
      *>------------------------------------------------------------------------
       2000-processamento section.
      *> aceitando a tela com os dados do atleta
           accept f-tela_atletas

      *> se o usuário confirmou a ação
           if   f-confirmar = "true" then
                move "S"             to ws-confirmacao
                move f-cf-operacao   to ws-operacao
           else
                move "N"     to ws-confirmacao
           end-if

      *> calculando o imc e o peso ideal com os dados recebidos da tela
           compute f-imc = f-peso/(f-altura * f-altura)
           compute f-peso-ideal = 21,5 * (f-altura * f-altura)
      *> movendo os dados calculados para as variáveis da working storage
           move f-imc to ws-imc
           move f-peso-ideal to ws-peso-ideal

      *> movendo dados da tela para as variáveis da working storage
           if   f-op-salvar = "SA" then            *> operação salvar
                move "SA"                          to ws-operacao
           end-if
           if   f-op-deletar = "DE" then           *> operação deletar
                move "DE"                          to ws-operacao
           end-if
           if   f-op-buscar-um = "BU" then         *> operação consultar/buscar um
                move "C1"                          to ws-operacao
           end-if
           if   f-op-buscar-proximo = "BP" then    *> operação consultar/buscar próximo
                move "BP"                          to ws-operacao
           end-if
           if   f-op-buscar-anterior = "BA" then   *> operação consultar/buscar anterior
                move "BA"                          to ws-operacao
           end-if

      *> movendo os dados da tela para as variáveis da working storage
           move f-id-atleta                        to ws-id-atleta
           move f-nome                             to ws-nome
           move f-peso                             to ws-peso
           move f-altura                           to ws-altura

      *> chamando o programa "P01ATLETA"
           call "P01ATLETA" using ws-controle, ws-atleta

      *> movendo a confirmação (S/N/?) para a variável de tela
           move ws-confirmacao to f-confirmar2
      *> movendo a operação a ser feita (SA/DE/CT/C1) para a variável de tela
           move ws-msn(1:2)    to f-cf-operacao2
      *> movendo a mensagem de pergunta para a variável de tela
           move ws-msn(4:46)   to f-msn2
      *> movendo a mensagem de erro/sucesso para a variável de tela
           move ws-msn-erro-text to f-msn-erro2
      *> movendo o item de grupo resultado carregado com dados do arquivo para o item de grupo da tela
           move ws-atleta   to f-atleta2
      *> mostrando a tela 2 com a mensagem/os dados do arquivo
           display f-tela_atletas2

           .
       2000-processamento-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Finalização Normal
      *>------------------------------------------------------------------------
       3000-finaliza section.
           stop run
           .
       3000-finaliza-exit.
           exit.


