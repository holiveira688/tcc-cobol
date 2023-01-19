       IDENTIFICATION DIVISION.
       PROGRAM-ID.PROJCOBOL.

      *=================================================================
      * OBJETIVO: CRIACAO DE UM SISTEMA DE GERENCIAMENTO DE ALUNOS E NO
      * TAS DE UMA ESCOLA.
      * AUTHOR  :  HANIF CRUZ DE OLIVEIRA RIBEIRO
      * PROFESSOR/ORIENTADOR: IVAN PETRUCCI
      *=================================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

      *---------------APONTAMENTOS DE LOCAIS DE ARQUIVOS----------------

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ALUNOS ASSIGN TO 'C:\COBOL\tcc-cobol\ALUNOS.DAT'
       ORGANIZATION IS INDEXED
       ACCESS MODE IS RANDOM
       FILE STATUS IS ALUNOS-STATUS
       RECORD KEY IS ALUNOS-CHAVE.

      *-----------------VARIAVEIS DA ESTRUTURA--------------------------

       DATA DIVISION.
       FILE SECTION.
       FD ALUNOS.
       01 ALUNOS-REG.
          05 ALUNOS-CHAVE.
          10 RM-ALUNOS                             PIC 9(05).
          05 NOME-ALUNOS                           PIC X(20).
          05 SERIE-ALUNOS                          PIC X(03).
          05 NOTA-1BIMESTRE                        PIC 9(02).
          05 NOTA-2BIMESTRE                        PIC 9(02).

      *----------------VARIAVEIS DE APOIO-------------------------------

       WORKING-STORAGE SECTION.
       77 WRK-OPCAO                                PIC X(01).
       77 WRK-MODULO                               PIC X(25).
       77 WRK-TECLA                                PIC X(01).
       77 ALUNOS-STATUS                            PIC 9(02).
       77 WRK-MSGERRO                              PIC X(40).
       77 WRK-MEDIA                                PIC 9(02) COMP-3.
       77 WRK-MEDIA-EDITADA                        PIC Z9.
       77 WRK-LINHA                                PIC 9(02) VALUE 5.
       77 WRK-MEDIA-STATUS                         PIC X(10).

      *--------------TELAS DO SISTEMA-----------------------------------

       SCREEN SECTION.

      *-------------TELA PRINCIPAL--------------------------------------

       01 TELA.
            05 LIMPA-TELA.
                10 BLANK SCREEN.
                10 LINE 01 COLUMN 01 PIC X(26) ERASE EOL
                   BACKGROUND-COLOR 3.
                10 LINE 01 COLUMN 45 PIC X(26)
                BACKGROUND-COLOR 3 FOREGROUND-COLOR 8
                   FROM 'SISTEMA DE GESTAO ESCOLAR'.
                10 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                   BACKGROUND-COLOR 1 FROM WRK-MODULO.
                10 LINE 04 COLUMN 45 PIC X(23)
                BACKGROUND-COLOR 2 FOREGROUND-COLOR 8
                FROM "--SELECIONE UMA OPCAO--".

      *--------------TELA RELATORIO-------------------------------------

       01 TELA-2.
                05 LIMPA-TELA.
                10 BLANK SCREEN.
                10 LINE 01 COLUMN 01 PIC X(26) ERASE EOL
                   BACKGROUND-COLOR 3.
                10 LINE 01 COLUMN 45 PIC X(26)
                BACKGROUND-COLOR 3 FOREGROUND-COLOR 8
                   FROM 'SISTEMA DE GESTAO ESCOLAR'.
                10 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                   BACKGROUND-COLOR 1 FROM WRK-MODULO.

      *-------------TELA MENU PRINCIPAL---------------------------------

       01 MENU.
            05 LINE 07 COLUMN 15 VALUE '1 - INCLUIR ALUNO'.
            05 LINE 08 COLUMN 15 VALUE '2 - CONSULTAR ALUNO'.
            05 LINE 09 COLUMN 15 VALUE '3 - ALTERAR ALUNO'.
            05 LINE 10 COLUMN 15 VALUE '4 - EXCLUIR ALUNO'.
            05 LINE 11 COLUMN 15 VALUE '5 - RELACAO DE ALUNOS'.
            05 LINE 12 COLUMN 15 VALUE '6 - SAIR'.
            05 LINE 13 COLUMN 15 VALUE 'OPCAO......: ' .
            05 LINE 13 COLUMN 28 USING WRK-OPCAO.

      *-------------TELA REGISTRO DE ALUNOS-----------------------------

       01 TELA-REGISTRO.
           05 CHAVE FOREGROUND-COLOR 2.
              10 LINE 10 COLUMN 10 VALUE 'RM...  '.
              10 COLUMN PLUS 2 PIC 9(05) USING RM-ALUNOS
                 BLANK WHEN ZEROS.
           05  SS-DADOS.
              10 LINE 11 COLUMN 10 VALUE 'NOME.... ' .
              10 COLUMN PLUS 2 PIC X(20) USING NOME-ALUNOS.
              10 LINE 12 COLUMN 10 VALUE 'SERIE... ' .
              10 COLUMN PLUS 2 PIC X(03) USING SERIE-ALUNOS.
              10 LINE 13 COLUMN 10 VALUE 'NOTA PRIMEIRO BIMESTRE' .
              10 COLUMN PLUS 2 PIC 9(02) USING NOTA-1BIMESTRE.
              10 LINE 14 COLUMN 10 VALUE 'NOTA SEGUNDO BIMESTRE' .
              10 COLUMN PLUS 2 PIC 9(02) USING NOTA-2BIMESTRE.

      *---------------TELA PARA MOSTRAR OS ERROS------------------------

       01 MOSTRA-ERRO.
           02 MSG-ERRO.
               10 LINE 16 COLUMN 10 PIC X(30) FROM WRK-MSGERRO
               BACKGROUND-COLOR 2 FOREGROUND-COLOR 8.

               10 LINE 16 COLUMN 45 PIC X(01) USING WRK-TECLA.

      *--------------TELA REGISTRO DO CABEÇALHO-------------------------

       01 RELAT-CABEC.
          05 LINE 4 COLUMN 01 VALUE "RM".
          05 LINE 4 COLUMN 06 VALUE "NOME".
          05 LINE 4 COLUMN 30 VALUE "SERIE".
          05 LINE 4 COLUMN 36 VALUE "NOTA 1".
          05 LINE 4 COLUMN 46 VALUE "NOTA 2".
          05 LINE 4 COLUMN 56 VALUE "MEDIA".
          05 LINE 4 COLUMN 66 VALUE "STATUS".


      *---------------TELA REGISTRO RELATORIO---------------------------

       01 RELAT-REG.
          05 LINE WRK-LINHA COLUMN 01 PIC 9(05) FROM RM-ALUNOS.
          05 LINE WRK-LINHA COLUMN 06 PIC X(20) FROM NOME-ALUNOS.
          05 LINE WRK-LINHA COLUMN 30 PIC X(03) FROM SERIE-ALUNOS.
          05 LINE WRK-LINHA COLUMN 36 PIC 9(02) FROM NOTA-1BIMESTRE.
          05 LINE WRK-LINHA COLUMN 46 PIC 9(02) FROM NOTA-2BIMESTRE.
          05 LINE WRK-LINHA COLUMN 56 PIC 9(02) FROM WRK-MEDIA-EDITADA.
          05 LINE WRK-LINHA COLUMN 66 PIC X(10) FROM WRK-MEDIA-STATUS.

      *-----------------------------------------------------------------


       PROCEDURE DIVISION.
       0001-PRINCIPAL  SECTION.

       PERFORM 1000-INICIAR        THRU 1100-MONTATELA.
       PERFORM 2000-PROCESSAR      UNTIL WRK-OPCAO = '6'.
       PERFORM 3000-FINALIZAR.
           STOP RUN.

      *---------------INICIALIZACAO-------------------------------------

       1000-INICIAR.
            OPEN I-O ALUNOS
              IF ALUNOS-STATUS = 35 THEN
                  OPEN OUTPUT ALUNOS
                  CLOSE ALUNOS
                  OPEN I-O ALUNOS
               END-IF.

       1100-MONTATELA.
            DISPLAY TELA.
            ACCEPT MENU.

      *----------------PROCESSAMENTO------------------------------------

       2000-PROCESSAR.
            EVALUATE WRK-OPCAO
             WHEN 1
                PERFORM 5000-INCLUIR-ALUNO
             WHEN 2
                PERFORM 6000-CONSULTAR-ALUNO
             CONTINUE
              WHEN 3
                PERFORM 7000-ALTERAR-ALUNO
             CONTINUE
             WHEN 4
                PERFORM 8000-EXCLUIR-ALUNO
             CONTINUE
             WHEN 5
             PERFORM 9000-RELAT-TELA
            WHEN OTHER
              IF  WRK-OPCAO NOT EQUAL 'X'
                  DISPLAY 'ENTRE COM A OPCAO CERTA'
                  END-IF
                  END-EVALUATE.
                  PERFORM 1100-MONTATELA.

      *--------------FINALIZACAO----------------------------------------

       3000-FINALIZAR.
            CLOSE ALUNOS.

      *-------------INCLUSÃO DE ALUNO-----------------------------------

       5000-INCLUIR-ALUNO.
             INITIALIZE ALUNOS-REG.
             MOVE 'MODULO - INCLUIR ALUNO' TO WRK-MODULO.
            DISPLAY TELA.
             ACCEPT TELA-REGISTRO.
              WRITE ALUNOS-REG
              INVALID KEY
                 MOVE 'ALUNO EXISTENTE' TO WRK-MSGERRO
                  ACCEPT MOSTRA-ERRO
                  NOT INVALID KEY
                  MOVE 'DESEJA INCLUIR ALUNO(S/N) ? '  TO WRK-MSGERRO
                  ACCEPT MOSTRA-ERRO
                  IF WRK-TECLA = 'S'
                  DISPLAY 'ALUNO REGISTRADO'
                  END-WRITE.
                  MOVE ALUNOS-STATUS TO WRK-MSGERRO.
                  ACCEPT MOSTRA-ERRO.

                  DISPLAY TELA.
                 ACCEPT MENU.

      *---------------CONSULTA DE ALUNO---------------------------------

       6000-CONSULTAR-ALUNO.
             INITIALIZE ALUNOS-REG.
             MOVE 'MODULO - CONSULTAR ALUNO' TO WRK-MODULO.
             DISPLAY TELA.
               DISPLAY TELA-REGISTRO.
               ACCEPT CHAVE.
                READ ALUNOS
                 INVALID KEY
                 MOVE 'ALUNO NAO ENCONTRADO' TO WRK-MSGERRO
                NOT INVALID KEY
                 MOVE ' ALUNO ENCONTRADO' TO WRK-MSGERRO
                 DISPLAY SS-DADOS
                END-READ.
                 ACCEPT MOSTRA-ERRO.

      *--------------ALTERAÇÃO DE ALUNO---------------------------------

       7000-ALTERAR-ALUNO.
             INITIALIZE ALUNOS-REG.
             MOVE 'MODULO - ALTERAR ALUNO ' TO WRK-MODULO.
             DISPLAY TELA.
             DISPLAY TELA-REGISTRO.
              ACCEPT CHAVE.
                READ ALUNOS
                IF ALUNOS-STATUS = 0
                    ACCEPT SS-DADOS
                    REWRITE ALUNOS-REG
                      IF ALUNOS-STATUS = 0
                        MOVE 'REGISTRO DE ALUNO ALTERADO'TO WRK-MSGERRO
                        ACCEPT MOSTRA-ERRO
                      ELSE
                        MOVE 'REGISTRO DE ALUNO NAO ALTERADO'
                        TO WRK-MSGERRO

                        ACCEPT MOSTRA-ERRO
                      END-IF
                ELSE
                    MOVE 'REGISTRO DE ALUNO NAO ENCONTRADO  '
                    TO WRK-MSGERRO

                    ACCEPT MOSTRA-ERRO
                END-IF.

      *------------------EXCLUSÃO DE ALUNO------------------------------

       8000-EXCLUIR-ALUNO.
            INITIALIZE ALUNOS-REG
            MOVE 'MODULO - EXCLUIR ALUNO' TO WRK-MODULO.
             DISPLAY TELA.
                DISPLAY TELA-REGISTRO
                ACCEPT CHAVE.
               READ ALUNOS
                 INVALID KEY
                  MOVE 'ALUNO NAO ENCONTRADO'  TO WRK-MSGERRO
                 NOT INVALID KEY
                  MOVE 'DESEJA EXCLUIR ALUNO(S/N) ? '  TO WRK-MSGERRO
                   DISPLAY SS-DADOS
                    ACCEPT MOSTRA-ERRO
                   IF WRK-TECLA = 'S'
                       DELETE ALUNOS
                       INVALID KEY
                       MOVE 'ALUNO NAO EXCLUIDO' TO WRK-MSGERRO
                       NOT INVALID KEY
                       MOVE 'ALUNO EXCLUIDO' TO WRK-MSGERRO
                       END-DELETE
                       ACCEPT MOSTRA-ERRO
                   END-IF
               END-READ.

      *-----------------RELAÇÃO DE ALUNOS-------------------------------

       9000-RELAT-TELA      SECTION.
           MOVE 'MODULO - RELACAO DE ALUNOS' TO WRK-MODULO.
            DISPLAY TELA-2.
             DISPLAY RELAT-CABEC.
                MOVE 0001 TO RM-ALUNOS
            START ALUNOS KEY EQUAL RM-ALUNOS
              READ ALUNOS
           INVALID KEY
             DISPLAY "NAO ACHOU"
           NOT INVALID KEY
             PERFORM UNTIL ALUNOS-STATUS EQUAL 10
              COMPUTE WRK-MEDIA = (NOTA-1BIMESTRE + NOTA-2BIMESTRE) / 2
                MOVE WRK-MEDIA TO WRK-MEDIA-EDITADA
               IF WRK-MEDIA >= 7
                MOVE 'APROVADO' TO WRK-MEDIA-STATUS
               ELSE
                  MOVE 'REPROVADO' TO WRK-MEDIA-STATUS
               END-IF
           DISPLAY RELAT-REG
              READ ALUNOS NEXT
             ADD 1 TO WRK-LINHA
               END-PERFORM
                 END-READ
                MOVE "APERTE ALGUMA TECLA" TO WRK-MSGERRO.
           ACCEPT MOSTRA-ERRO.

      *---------------------FIM DE PROGRAMA-----------------------------
