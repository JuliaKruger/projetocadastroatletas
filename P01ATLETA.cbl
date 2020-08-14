      $set sourceformat"free"
      *>----Divis�o de identifica��o do programa
       identification division.
       program-id. "P01ATLETA".
       author. "Julia Kr�ger".
       installation. "PC".
       date-written. 13/08/2020.
       date-compiled. 13/08/2020.

      *>----Divis�o para configura��o do ambiente
       environment division.
       configuration section.
       special-names. decimal-point is comma.

      *>----Declara��o dos recursos externos
       input-output section.
       file-control.

           select arq-atletas assign to "arq-atletas.dat"
           organization is indexed
           access mode is dynamic
           lock mode is manual with lock on multiple records
           record key is fl-chave
           file status is ws-fs-arq-atletas.

       i-o-control.

      *>----Declara��o de vari�veis
       data division.

      *>----Vari�veis de arquivos
       file section.
       fd arq-atletas.
       01 fl-atleta.
           05 fl-chave.
               10 fl-id-atleta                     pic 9(03).
               10 fl-nome                          pic x(50).
           05 fl-peso                              pic -99,99.
           05 fl-altura                            pic -99,99.
           05 fl-imc                               pic -99,99.
           05 fl-peso-ideal                        pic -99,99.

      *>----Vari�veis de trabalho
       working-storage section.
       77 ws-fs-arq-atletas                        pic x(02).

       77 ws-operacao                              pic x(02).
           88 ws-salvar                            value "SA".
           88 ws-consultar-um                      value "C1".
           88 ws-consultar-varios                  value "CN".
           88 ws-consultar-todos                   value "CT".
           88 ws-excluir                           value "DE".

       77 ws-confirmacao                           pic x(01).
           88 ws-confirmar                         value "?".
           88 ws-confirmado                        value "S".
           88 ws-nao-confirmado                    value "N".


      *>----Vari�veis para comunica��o entre programas
       linkage section.

       01 lnk-controle.
           05 lnk-operacao                         pic x(02).
           05 lnk-confirmacao                      pic x(01).
           05 lnk-msn                              pic x(50).
           05 lnk-retorno.
               10 lnk-msn-erro-pmg                 pic x(09). *> id do pmg
               10 lnk-msn-erro-offset              pic 9(03). *> local do erro
               10 lnk-return-code                  pic 9(02). *> status do pmg
               10 lnk-msn-erro-cod                 pic x(02). *> file status
               10 lnk-msn-erro-text                pic x(50). *> mensagem de erro

       01 lnk-atleta.
           05 lnk-chave.
               10 lnk-id-atleta                    pic 9(03).
               10 lnk-nome                         pic x(50).
           05 lnk-peso                             pic -99,99.
           05 lnk-altura                           pic -99,99.
           05 lnk-imc                              pic -99,99.
           05 lnk-peso-ideal                       pic -99,99.

      *>----Declara��o de tela
       screen section.

      *>Declara��o do corpo do programa
       procedure division using lnk-controle, lnk-atleta.

      *>------------------------------------------------------------------------
      *>  Controle das se��es
      *>------------------------------------------------------------------------
       0000-controle section.
           perform 1000-inicializa
           perform 2000-processamento
           perform 3000-finaliza
           .
       0000-controle-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Procedimentos de inicializa��o
      *>------------------------------------------------------------------------
       1000-inicializa section.
           open i-o arq-atletas                    *> open i-o abre o arquivo para leitura e escrita
           if   ws-fs-arq-atletas  <> "00"         *> file status 00: comando executado com sucesso
           and  ws-fs-arq-atletas <> "05" then     *> file status 05: open opcional com sucesso, mas n�o existe aquivo anterior
                move "P06SISC20"                       to lnk-msn-erro-pmg
                move 1                                 to lnk-msn-erro-offset
                move 12                                to lnk-return-code
                move ws-fs-arq-atletas                 to lnk-msn-erro-cod
                move "Erro ao abrir arq. arq-atletas"  to lnk-msn-erro-text
                perform 9000-finaliza-anormal
           end-if
           move lnk-confirmacao to ws-confirmacao  *> movendo a confirma��o do usu�rio da linkage storage para a working storage
           .
       1000-inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento principal
      *>------------------------------------------------------------------------
       2000-processamento section.
           evaluate lnk-operacao
               when "SA"
                   perform 2100-salvar-dados       *> se��o para salvar dados
               when "C1"
                   perform 2200-b-um-registro      *> se��o para buscar um registro
               when "BP"
                   perform 2300-b-proximo          *> se��o para buscar o pr�ximo registro
               when "BA"
                   perform 2400-b-anterior         *> se��o para buscar o registro anterior
               when "DE"
                   perform 2500-deletar-dados      *> se��o para deletar dados
           end-evaluate
           .
       2000-processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Se��o para salvar dados
      *>------------------------------------------------------------------------
       2100-salvar-dados section.
           move lnk-atleta                         to fl-atleta
           write fl-atleta                         *> escrevendo os dados no arquivo
           if   ws-fs-arq-atletas  = "00" or ws-fs-arq-atletas = "02" then  *> file status 02: sucesso, mas existe chave alternada
                move "P01ATLETA"                   to lnk-msn-erro-pmg
                move 2                             to lnk-msn-erro-offset
                move 00                            to lnk-return-code
                move "Registro salvo com sucesso"  to lnk-msn-erro-text
                move ws-fs-arq-atletas             to lnk-msn-erro-cod
           else
                if   ws-fs-arq-atletas = 22 then   *> file status 22: na grava��o, registro j� existe
                     if   ws-confirmado then
                          *> movendo "N" para ws-confirmacao (usu�rio ainda precisa confirmar a exclus�o de registro)
                          set ws-nao-confirmado    to true
                          rewrite fl-atleta        *> reescrevendo o registro caso o usu�rio queira
                          if   ws-fs-arq-atletas = "00" then
                               move "P01ATLETA"                          to lnk-msn-erro-pmg
                               move 3                                    to lnk-msn-erro-offset
                               move 00                                   to lnk-return-code
                               move "Registro alterado com sucesso"      to lnk-msn-erro-text
                               move ws-fs-arq-atletas                    to lnk-msn-erro-cod
                          else
                               move "P01ATLETA"                          to lnk-msn-erro-pmg
                               move 4                                    to lnk-msn-erro-offset
                               move 12                                   to lnk-return-code
                               move "Erro ao alterar registro"           to lnk-msn-erro-text
                               move ws-fs-arq-atletas                    to lnk-msn-erro-cod
                               perform 9000-finaliza-anormal
                          end-if
                     else
                          *> movendo "?" para ws-confirmacao
                          set ws-confirmar         to true
                          *> saber se o usu�rio quer reescrever o registro
                          move "SA-Confirmar a alteracao de registro?"   to lnk-msn
                     end-if
                else
                     move "P01ATLETA"                                    to lnk-msn-erro-pmg
                     move 5                                              to lnk-msn-erro-offset
                     move 12                                             to lnk-return-code
                     move "Erro ao escrever registro"                    to lnk-msn-erro-text
                     move ws-fs-arq-atletas                              to lnk-msn-erro-cod
                     perform 9000-finaliza-anormal
                end-if
           end-if
           .
       2100-salvar-dados-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Se��o para consultar/buscar um registro
      *>------------------------------------------------------------------------
       2200-b-um-registro section.
      *> carregando as chaves do arquivo
           move lnk-id-atleta                      to fl-id-atleta
           move lnk-nome                           to fl-nome
           read arq-atletas key fl-id-atleta       *> lendo o arquivo usando a chave
           if   ws-fs-arq-atletas = "00" then
                move fl-atleta to lnk-atleta
                move "P01ATLETA"                   to lnk-msn-erro-pmg
                move 6                             to lnk-msn-erro-offset
                move 00                            to lnk-return-code
                move "Registro lido com sucesso"   to lnk-msn-erro-text
                move ws-fs-arq-atletas             to lnk-msn-erro-cod
           else
                if   ws-fs-arq-atletas = "23" then *> file status 23: na leitura, registro n�o existe
                     move "P01ATLETA"              to lnk-msn-erro-pmg
                     move 7                        to lnk-msn-erro-offset
                     move 04                       to lnk-return-code
                     move "Codigo inexistente"     to lnk-msn-erro-text
                     move ws-fs-arq-atletas        to lnk-msn-erro-cod
                else
                     move "P01ATLETA"              to lnk-msn-erro-pmg
                     move 8                        to lnk-msn-erro-offset
                     move 12                       to lnk-return-code
                     move "Erro ao ler registro"   to lnk-msn-erro-text
                     move ws-fs-arq-atletas        to lnk-msn-erro-cod
                     perform 9000-finaliza-anormal
                end-if
           end-if
           .
       2200-b-um-registro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Se��o para consultar/buscar o pr�ximo registro
      *>------------------------------------------------------------------------
       2300-b-proximo section.
      *> carregando as chaves do arquivo
           move lnk-id-atleta                      to fl-id-atleta
           move lnk-nome                           to fl-nome
           start arq-atletas key = fl-chave        *> come�ando o arquivo a partir da chave que o usu�rio inseriu
           if   ws-fs-arq-atletas = "00" then
                     read arq-atletas next         *> lendo o arquivo sequencialmente
                     if   ws-fs-arq-atletas = "00" or ws-fs-arq-atletas = "02" then
                          read arq-atletas next    *> necess�rio colocar mais um read next para ele realemte ler o pr�ximo, sem mostrar o mesmo registro
                                                   *> (caso contr�rio ele apenas l� o mesmo registro)
                          if   ws-fs-arq-atletas = "00" or ws-fs-arq-atletas = "02" then
                               *> movendo o registro do arquivo para as vari�veis da linkage section
                               move fl-atleta        to lnk-atleta
                          else
                               if   ws-fs-arq-atletas = "10" then   *> file status 10: fim do arquivo
                                       move "P01ATLETA"             to lnk-msn-erro-pmg
                                       move 9                       to lnk-msn-erro-offset
                                       move 04                      to lnk-return-code
                                       move "Fim do arquivo"        to lnk-msn-erro-text
                                       move ws-fs-arq-atletas       to lnk-msn-erro-cod
                               else
                                       move "P01ATLETA"             to lnk-msn-erro-pmg
                                       move 10                      to lnk-msn-erro-offset
                                       move 12                      to lnk-return-code
                                       move "Erro ao ler registro"  to lnk-msn-erro-text
                                       move ws-fs-arq-atletas       to lnk-msn-erro-cod
                                       perform 9000-finaliza-anormal
                               end-if
                          end-if
                     else
                          if   ws-fs-arq-atletas = "10"
                               move "P01ATLETA"             to lnk-msn-erro-pmg
                               move 11                      to lnk-msn-erro-offset
                               move 04                      to lnk-return-code
                               move "Fim do arquivo"        to lnk-msn-erro-text
                               move ws-fs-arq-atletas       to lnk-msn-erro-cod
                          else
                               move "P01ATLETA"             to lnk-msn-erro-pmg
                               move 12                      to lnk-msn-erro-offset
                               move 12                      to lnk-return-code
                               move "Erro ao ler registro"  to lnk-msn-erro-text
                               move ws-fs-arq-atletas       to lnk-msn-erro-cod
                               perform 9000-finaliza-anormal
                          end-if
                     end-if
           else
                if   ws-fs-arq-atletas = "23" then
                     move "P01ATLETA"              to lnk-msn-erro-pmg
                     move 13                       to lnk-msn-erro-offset
                     move 04                       to lnk-return-code
                     move "Codigo inexistente"     to lnk-msn-erro-text
                     move ws-fs-arq-atletas        to lnk-msn-erro-cod
                else
                     move "P01ATLETA"              to lnk-msn-erro-pmg
                     move 14                       to lnk-msn-erro-offset
                     move 12                       to lnk-return-code
                     move "Erro ao ler registro"   to lnk-msn-erro-text
                     move ws-fs-arq-atletas        to lnk-msn-erro-cod
                     perform 9000-finaliza-anormal
                end-if
           end-if
           .
       2300-b-proximo-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Se��o para consultar/buscar o registro anterior
      *>------------------------------------------------------------------------
       2400-b-anterior section.
      *> carregando as chaves do arquivo
           move lnk-id-atleta                      to fl-id-atleta
           move lnk-nome                           to fl-nome
           start arq-atletas key = fl-id-atleta    *> come�ando o arquivo a partir da chave que o usu�rio inseriu
           if   ws-fs-arq-atletas = "00" then
                     read arq-atletas previous     *> lendo o arquivo sequencialmente de tr�s para frente
                     if   ws-fs-arq-atletas = "00" or ws-fs-arq-atletas = "02" then
                          read arq-atletas previous*> necess�rio colocar mais um read previous para ele realemte ler o anterior, sem mostrar o mesmo registro
                                                   *> (caso contr�rio ele apenas l� o mesmo registro)
                          if   ws-fs-arq-atletas = "00" or ws-fs-arq-atletas = "02" then
                               *> movendo o registro do arquivo para as vari�veis da linkage section
                               move fl-atleta      to lnk-atleta
                          else
                               if   ws-fs-arq-atletas = "10"
                                       move "P01ATLETA"             to lnk-msn-erro-pmg
                                       move 15                      to lnk-msn-erro-offset
                                       move 04                      to lnk-return-code
                                       move "Fim do arquivo"        to lnk-msn-erro-text
                                       move ws-fs-arq-atletas       to lnk-msn-erro-cod
                               else
                                       move "P01ATLETA"             to lnk-msn-erro-pmg
                                       move 16                      to lnk-msn-erro-offset
                                       move 12                      to lnk-return-code
                                       move "Erro ao ler registro"  to lnk-msn-erro-text
                                       move ws-fs-arq-atletas       to lnk-msn-erro-cod
                                       perform 9000-finaliza-anormal
                               end-if
                          end-if
                     else
                          if   ws-fs-arq-atletas = "10"
                               move "P01ATLETA"             to lnk-msn-erro-pmg
                               move 17                      to lnk-msn-erro-offset
                               move 04                      to lnk-return-code
                               move "Fim do arquivo"        to lnk-msn-erro-text
                               move ws-fs-arq-atletas       to lnk-msn-erro-cod
                          else
                               move "P01ATLETA"             to lnk-msn-erro-pmg
                               move 18                      to lnk-msn-erro-offset
                               move 12                      to lnk-return-code
                               move "Erro ao ler registro"  to lnk-msn-erro-text
                               move ws-fs-arq-atletas       to lnk-msn-erro-cod
                               perform 9000-finaliza-anormal
                          end-if
                     end-if
           else
                if   ws-fs-arq-atletas = "23" then
                     move "P01ATLETA"              to lnk-msn-erro-pmg
                     move 19                       to lnk-msn-erro-offset
                     move 04                       to lnk-return-code
                     move "Codigo inexistente"     to lnk-msn-erro-text
                     move ws-fs-arq-atletas        to lnk-msn-erro-cod
                else
                     move "P01ATLETA"              to lnk-msn-erro-pmg
                     move 20                       to lnk-msn-erro-offset
                     move 12                       to lnk-return-code
                     move "Erro ao ler registro"   to lnk-msn-erro-text
                     move ws-fs-arq-atletas     to lnk-msn-erro-cod
                     perform 9000-finaliza-anormal
                end-if
           end-if
           .
       2400-b-anterior-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Se��o para deletar dados
      *>------------------------------------------------------------------------
       2500-deletar-dados section.
      *> movendo os dados da linkage section para as vari�veis da file section (chaves)
           move lnk-id-atleta                      to fl-id-atleta
           move lnk-nome                           to fl-nome
           read arq-atletas                        *> lendo o arquivo
           if   ws-fs-arq-atletas = "00" then
                if   ws-confirmado then
                     *> movendo "N" para ws-confirmacao (usu�rio ainda precisa confirmar a exclus�o de registro)
                     set ws-nao-confirmado         to true
                     delete arq-atletas            *> deletando o registro
                     if   ws-fs-arq-atletas = "00" then
                          move "P01ATLETA"                      to lnk-msn-erro-pmg
                          move 21                               to lnk-msn-erro-offset
                          move 00                               to lnk-return-code
                          move "Registro excluido com sucesso"  to lnk-msn-erro-text
                          move ws-fs-arq-atletas                to lnk-msn-erro-cod
                     else
                          move "P01ATLETA"                      to lnk-msn-erro-pmg
                          move 22                               to lnk-msn-erro-offset
                          move 12                               to lnk-return-code
                          move "Erro ao excluir registro"       to lnk-msn-erro-text
                          move ws-fs-arq-atletas                to lnk-msn-erro-cod
                          perform 9000-finaliza-anormal
                     end-if
                else
                     *> movendo "?" para ws-confirmacao
                     set ws-confirmar              to true
                     *> saber se o usu�rio quer excluir/deletar o registro
                     move "DE-Confirma a exclusao de registro?" to lnk-msn
                end-if
           else
                if   ws-fs-arq-atletas = "23" then
                     move "P01ATLETA"              to lnk-msn-erro-pmg
                     move 23                       to lnk-msn-erro-offset
                     move 04                       to lnk-return-code
                     move "Codigo inexistente"     to lnk-msn-erro-text
                     move ws-fs-arq-atletas        to lnk-msn-erro-cod
                else
                     move "P01ATLETA"              to lnk-msn-erro-pmg
                     move 24                       to lnk-msn-erro-offset
                     move 12                       to lnk-return-code
                     move "Erro ao ler registro"   to lnk-msn-erro-text
                     move ws-fs-arq-atletas        to lnk-msn-erro-cod
                     perform 9000-finaliza-anormal
                end-if
           end-if
           .
       2500-deletar-dados-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Finaliza��o  Anormal
      *>------------------------------------------------------------------------
       9000-finaliza-anormal section.
      *> movendo 12 (seguindo a especifica��o) para o return code da linkage section
           move 12                                 to lnk-return-code
      *> parando a execu��o o programa
           stop run
           .
       9000-finaliza-anormal-exit.
           exit.

      *>------------------------------------------------------------------------
      *> Finaliza��o Normal
      *>------------------------------------------------------------------------
       3000-finaliza section.
      *> movendo a vari�vel de confirma��o da working storage para a linkage section
           move ws-confirmacao                     to lnk-confirmacao
           close arq-atletas                       *> fechando o arquivo
           if   ws-fs-arq-atletas  <> "00" then
                move "P01ATLETA"                           to lnk-msn-erro-pmg
                move 25                                    to lnk-msn-erro-offset
                move 12                                    to lnk-return-code
                move "Erro ao fechar arq. arq-atletas"     to lnk-msn-erro-text
                move ws-fs-arq-atletas                     to lnk-msn-erro-cod
                perform 9000-finaliza-anormal
           end-if
      *> saindo do programa chamado
           exit program
           .
       3000-finaliza-exit.
           exit.

