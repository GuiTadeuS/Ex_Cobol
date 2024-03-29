       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX06.
       AUTHOR. ANDERSON & GUILHERME & KARINNA.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 25/04/2022.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCLI  ASSIGN TO DISK 
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADOK   ASSIGN TO DISK 
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELOCOR ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       FD CADCLI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADCLI.DAT".

       01 REGCLI.
           02 CODIGOIN PIC 9(03).
           02 CPFIN.
              03 DIG-1 PIC 9(01).
              03 DIG-2 PIC 9(01).
              03 DIG-3 PIC 9(01).
              03 DIG-4 PIC 9(01).
              03 DIG-5 PIC 9(01).
              03 DIG-6 PIC 9(01).
              03 DIG-7 PIC 9(01).
              03 DIG-8 PIC 9(01).
              03 DIG-9 PIC 9(01).
              03 DIG-V1 PIC 9(01).
              03 DIG-V2 PIC 9(01).

           02 NOMEIN PIC X(30).

           02 ESTADOIN PIC X(02).
             88 ESTADO-VALIDO VALUE 
                "AC" "AL" "AP" "AM" "BA" "CE" "DF" "ES" "GO"
                "MA" "MT" "MS" "MG" "PA" "PB" "PR" "PE" "PI"
                "RJ" "RN" "RS" "RO" "RR" "SC" "SP" "SE" "TO".

           02 CIDADEIN PIC X(30).
           02 EMAILIN PIC X(30).
           02 TELEFONEIN PIC 9(10).

           02 DATA-ENT.
             03 DD  PIC 9(02).
                88 DIA-VALIDO VALUE 1 THRU 31.

             03 MM  PIC 9(02).
                88 MES-VALIDO VALUE 1 THRU 12.
                88 MES-COM-30-DIAS VALUE 4 6 9 11.
                88 MES-COM-31-DIAS VALUE 1 3 5 7 8 10 12.

             03 AA  PIC 9(04).

       FD CADOK
          LABEL RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADOK.DAT".

       01 REGOK.
           02 CPF-OK PIC 9(11).
           02 NOME-OK PIC X(30).
           02 ESTADO-OK PIC X(02).
           02 CIDADE-OK PIC X(30).
           02 EMAIL-OK PIC X(30).
           02 TELEFONE-OK PIC 9(10).
           02 DATA-OK.
             03 DD-OK PIC 9(02).
             03 MM-OK PIC 9(02).
             03 AA-OK PIC 9(04).

       FD  RELOCOR
           LABEL RECORD IS OMITTED.

       01  REG-ATR PIC X(80).

       WORKING-STORAGE SECTION.

       77 FIM-ARQ PIC X(03) VALUE "NAO".
       77 CT-LIN  PIC 9(02) VALUE 41.
       77 ERRO PIC 9(01) VALUE 0.
       77 ERRO-NOME PIC 9(01) VALUE 0.
       77 ERRO-CPF PIC 9(01) VALUE 0.
       77 ERRO-ESTADO  PIC 9(01) VALUE 0.
       77 ERRO-CIDADE PIC 9(01) VALUE 0.
       77 ERRO-EMAIL PIC 9(01) VALUE 0.
       77 ERRO-DATA PIC 9(01) VALUE 0.
       77 RESTO PIC 9(03).
       77 DIG-V1-CORRET PIC 9(03).
       77 DIG-V2-CORRET PIC 9(03).
       77 DIG-1-MULT PIC 9(03).
       77 DIG-2-MULT PIC 9(03).
       77 DIG-3-MULT PIC 9(03).
       77 DIG-4-MULT PIC 9(03).
       77 DIG-5-MULT  PIC 9(03).
       77 DIG-6-MULT PIC 9(03).
       77 DIG-7-MULT  PIC 9(03).
       77 DIG-8-MULT PIC 9(03).
       77 DIG-9-MULT PIC 9(03).
       77 DIG-V1-CORRET-MULT PIC 9(03).

       01 CAB01.
           02 FILLER      PIC X(30) VALUE SPACES.
           02 FILLER      PIC X(17) VALUE "TURISMAR TURISMOS".
           02 FILLER      PIC X(33) VALUE SPACES.

       01 CAB02.
           02 FILLER    PIC X(15) VALUE SPACES.
           02 FILLER    PIC X(45) VALUE 
                        "RELATORIO DE DADOS DE CLIENTES INCONSISTENTES".
           02 FILLER    PIC X(20)  VALUE SPACES.
 
       01 CAB03.
           02 FILLER PIC X(17) VALUE "CODIGO DO CLIENTE".
           02 FILLER PIC X(15) VALUE SPACES.
           02 FILLER PIC X(15) VALUE "DADOS INVALIDOS".
           02 FILLER PIC X(33) VALUE SPACES.

       01 CAB04.
           02 FILLER PIC X(80) VALUE ALL "-".

       01 CAB05.
           02 FILLER PIC X(23)  VALUE SPACES.

       01 CAB06.
           02 FILLER PIC X(12) VALUE "CPF INVALIDO".

       01 CAB07.
           02 FILLER PIC X(18) VALUE "NOME NAO INFORMADO".

       01 CAB08.
           02 FILLER PIC X(15) VALUE "ESTADO INVALIDO".

       01 CAB09.
           02 FILLER PIC X(20) VALUE "CIDADE NAO INFORMADA".

       01 CAB10.
           02 FILLER PIC X(19)  VALUE "EMAIL SEM DESCRICAO".

       01 CAB11.
           02 FILLER PIC X(13)  VALUE "DATA INVALIDA".

       01 DETALHE.
           02 FILLER PIC X(05)  VALUE SPACES.
           02 CODIGO-CLI     PIC ZZZ.
           02 FILLER         PIC X(25)  VALUE SPACES.
           02 MENSAGEM-ERRO  PIC X(47).

       PROCEDURE DIVISION.

       PROGRAMA05.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
           OPEN INPUT CADCLI OUTPUT CADOK RELOCOR.
           PERFORM LEITURA.
       LEITURA.
           READ CADCLI
              AT END MOVE "SIM" TO FIM-ARQ.
       
       PRINCIPAL.
           PERFORM CONSIST.
           PERFORM IMPRESSAO.
           PERFORM LEITURA.
       GRAVA-REGOK.
           MOVE CPFIN      TO CPF-OK.
           MOVE NOMEIN     TO NOME-OK.
           MOVE ESTADOIN   TO ESTADO-OK.
           MOVE CIDADEIN   TO CIDADE-OK.
           MOVE EMAILIN    TO EMAIL-OK.
           MOVE TELEFONEIN TO TELEFONE-OK.
           MOVE DATA-ENT TO DATA-OK.
           WRITE REGOK.
       CONSIST.
           MOVE 0 TO ERRO.
           MOVE 0 TO ERRO-CPF.
           MOVE 0 TO ERRO-NOME.
           MOVE 0 TO ERRO-ESTADO.
           MOVE 0 TO ERRO-CIDADE.
           MOVE 0 TO ERRO-EMAIL.
           MOVE 0 TO ERRO-DATA.
           PERFORM VAL-ESTADO.
           PERFORM VAL-NOME.
           PERFORM VAL-CPF.
           PERFORM VAL-CIDADE.
           PERFORM VAL-EMAIL.
           PERFORM VAL-DATA.
       IMPRESSAO.
           IF ERRO EQUAL 0
              PERFORM GRAVA-REGOK
           ELSE
              IF CT-LIN GREATER THAN 40
                 PERFORM CABECALHO
              END-IF
              PERFORM IMPDETALHE
           END-IF.

       IMPDETALHE.
           MOVE CODIGOIN TO CODIGO-CLI.
           IF ERRO-NOME EQUAL 1
              MOVE CAB07 TO MENSAGEM-ERRO
              MOVE 0 TO ERRO-NOME
           ELSE 
              IF ERRO-ESTADO EQUAL 1
                 MOVE CAB08 TO MENSAGEM-ERRO
                 MOVE 0 TO ERRO-ESTADO
              ELSE
                 IF ERRO-CIDADE EQUAL 1
                    MOVE CAB09 TO MENSAGEM-ERRO
                    MOVE 0 TO ERRO-CIDADE
                 ELSE
                    IF ERRO-EMAIL EQUAL 1
                       MOVE CAB10 TO MENSAGEM-ERRO
                       MOVE 0 TO ERRO-EMAIL
                    ELSE
                       IF ERRO-DATA EQUAL 1
                          MOVE CAB11 TO MENSAGEM-ERRO
                          MOVE 0 TO ERRO-DATA
                       ELSE
                          MOVE CAB06 TO MENSAGEM-ERRO
                          MOVE 0 TO ERRO-CPF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF

           WRITE REG-ATR FROM DETALHE AFTER ADVANCING 2 LINES.
           ADD 1 TO CT-LIN.
           IF CT-LIN GREATER THAN 40  GO TO PRINCIPAL.
           MOVE ZEROES TO CODIGO-CLI.

           IF ERRO-NOME EQUAL 1
              MOVE CAB07 TO MENSAGEM-ERRO
              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE
              ADD 1 TO CT-LIN
              IF CT-LIN GREATER THAN 40  GO TO PRINCIPAL
           END-IF.

           IF ERRO-ESTADO EQUAL 1
              MOVE CAB08 TO MENSAGEM-ERRO
              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE
              ADD 1 TO CT-LIN
              IF CT-LIN GREATER THAN 40  GO TO PRINCIPAL
           END-IF.

           IF ERRO-CIDADE EQUAL 1
              MOVE CAB09 TO MENSAGEM-ERRO
              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE
              ADD 1 TO CT-LIN
              IF CT-LIN GREATER THAN 40  GO TO PRINCIPAL
           END-IF.

           IF ERRO-EMAIL EQUAL 1
              MOVE CAB10 TO MENSAGEM-ERRO
              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE
              ADD 1 TO CT-LIN
              IF CT-LIN GREATER THAN 40  GO TO PRINCIPAL
           END-IF.

           IF ERRO-DATA EQUAL 1
              MOVE CAB11 TO MENSAGEM-ERRO
              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE
              ADD 1 TO CT-LIN
              IF CT-LIN GREATER THAN 40  GO TO PRINCIPAL
           END-IF.

           IF ERRO-CPF EQUAL 1
              MOVE CAB06 TO MENSAGEM-ERRO
              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE
              ADD 1 TO CT-LIN
           END-IF.
       CABECALHO.
           WRITE REG-ATR FROM CAB01 AFTER ADVANCING PAGE.
           WRITE REG-ATR FROM CAB02 AFTER ADVANCING 2 LINES.
           WRITE REG-ATR FROM CAB03 AFTER ADVANCING 2 LINES.
           WRITE REG-ATR FROM CAB04 AFTER ADVANCING 1 LINE.
           MOVE 1 TO CT-LIN.
       VAL-NOME.
           IF NOMEIN EQUAL SPACES
              MOVE 1 TO ERRO
              MOVE 1 TO ERRO-NOME
           END-IF.
       VAL-ESTADO.
           IF NOT ESTADO-VALIDO
              MOVE 1 TO ERRO
              MOVE 1 TO ERRO-ESTADO
           END-IF.
       VAL-CIDADE.
           IF CIDADEIN EQUAL SPACES
              MOVE 1 TO ERRO
              MOVE 1 TO ERRO-CIDADE
           END-IF.
       VAL-EMAIL.
           IF EMAILIN EQUAL SPACES
              MOVE 1 TO ERRO
              MOVE 1 TO ERRO-EMAIL
           END-IF.
       VAL-DATA.
           IF NOT DIA-VALIDO OR NOT MES-VALIDO
              MOVE 1 TO ERRO
              MOVE 1 TO ERRO-DATA
           ELSE
              IF MES-COM-30-DIAS AND DD GREATER THAN 30 OR
                 MES-COM-31-DIAS AND DD GREATER THAN 31
                 MOVE 1 TO ERRO
                 MOVE 1 TO ERRO-DATA
              ELSE
                 IF MM EQUAL 2 AND DD GREATER THAN 29
                    MOVE 1 TO ERRO
                    MOVE 1 TO ERRO-DATA
                 ELSE
                    IF DD EQUAL 29 AND MM EQUAL 2
                       DIVIDE AA BY 4 GIVING RESTO REMAINDER RESTO
                       IF RESTO NOT EQUAL 0
                          MOVE 1 TO ERRO
                          MOVE 1 TO ERRO-DATA
                       ELSE
                          DIVIDE AA BY 100 GIVING RESTO REMAINDER RESTO
                          IF RESTO EQUAL 0
                             MOVE 1 TO ERRO
                             MOVE 1 TO ERRO-DATA
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
       VAL-CPF.

           MULTIPLY 10 BY DIG-1 GIVING DIG-1-MULT.
           MULTIPLY 09 BY DIG-2 GIVING DIG-2-MULT.
           MULTIPLY 08 BY DIG-3 GIVING DIG-3-MULT.
           MULTIPLY 07 BY DIG-4 GIVING DIG-4-MULT.
           MULTIPLY 06 BY DIG-5 GIVING DIG-5-MULT.
           MULTIPLY 05 BY DIG-6 GIVING DIG-6-MULT.
           MULTIPLY 04 BY DIG-7 GIVING DIG-7-MULT.
           MULTIPLY 03 BY DIG-8 GIVING DIG-8-MULT.
           MULTIPLY 02 BY DIG-9 GIVING DIG-9-MULT.

           ADD DIG-1-MULT DIG-2-MULT DIG-3-MULT
               DIG-4-MULT DIG-5-MULT DIG-6-MULT
               DIG-7-MULT DIG-8-MULT DIG-9-MULT GIVING DIG-1-MULT.

           DIVIDE DIG-1-MULT BY 11 GIVING RESTO REMAINDER RESTO.

           IF RESTO LESS THAN 2
              MOVE 0 TO DIG-V1-CORRET
           ELSE
              SUBTRACT RESTO FROM 11 GIVING DIG-V1-CORRET
           END-IF

           MULTIPLY 11 BY DIG-1 GIVING DIG-1-MULT.
           MULTIPLY 10 BY DIG-2 GIVING DIG-2-MULT.
           MULTIPLY 09 BY DIG-3 GIVING DIG-3-MULT.
           MULTIPLY 08 BY DIG-4 GIVING DIG-4-MULT.
           MULTIPLY 07 BY DIG-5 GIVING DIG-5-MULT.
           MULTIPLY 06 BY DIG-6 GIVING DIG-6-MULT.
           MULTIPLY 05 BY DIG-7 GIVING DIG-7-MULT.
           MULTIPLY 04 BY DIG-8 GIVING DIG-8-MULT.
           MULTIPLY 03 BY DIG-9 GIVING DIG-9-MULT.
           MULTIPLY 02 BY DIG-V1-CORRET GIVING DIG-V1-CORRET-MULT.

           ADD DIG-1-MULT DIG-2-MULT DIG-3-MULT
               DIG-4-MULT DIG-5-MULT DIG-6-MULT
               DIG-7-MULT DIG-8-MULT DIG-9-MULT TO DIG-V1-CORRET-MULT.

           DIVIDE DIG-V1-CORRET-MULT BY 11 GIVING RESTO REMAINDER RESTO.

           IF RESTO LESS THAN 2
              MOVE 0 TO DIG-V2-CORRET
           ELSE
              SUBTRACT RESTO FROM 11 GIVING DIG-V2-CORRET
           END-IF

           IF (DIG-V1 NOT EQUAL DIG-V1-CORRET) OR
              (DIG-V2 NOT EQUAL DIG-V2-CORRET)
              MOVE 1 TO ERRO
              MOVE 1 TO ERRO-CPF
           END-IF.
       TERMINO.
           CLOSE CADCLI CADOK RELOCOR.