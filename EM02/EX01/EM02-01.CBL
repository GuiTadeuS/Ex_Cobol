       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EM02-01.
       AUTHOR.        g.
       INSTALLATION.  FATEC-SP.
       DATE-WRITTEN.  30/03/2022.
       DATE-COMPILED.
       SECURITY.      APENAS A AUTORA PODE MODIFICA-LO.
      *REMARKS.       LE UM REGISTRO E IMPRIME UM RELATORIO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. HP-PC.
       OBJECT-COMPUTER. HP-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADSOC1 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSOC2 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELSOCIO ASSIGN TO DISK.
        
       DATA DIVISION.
       FILE SECTION.

       FD CADSOC1
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADSOC1.DAT".

       01 REG-ENT.
           02 COD-PAG       PIC 9(02).
           02 NUM-SOCIO1    PIC 9(06).
           02 NOME-SOCIO1   PIC X(30).
           02 VALOR-PAG1    PIC 9(09)V9(02).

       FD CADSOC2

           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADSOC2.DAT".

       01 REG-SAI.
           02 NUM-SOCIO2     PIC 9(06).
           02 NOME-SOCIO2    PIC X(30).
           02 VALOR-PAG2     PIC 9(09)V9(02).

       FD RELSOCIO
          LABEL RECORD IS OMITTED.

       01 REG-SOCIO PIC X(80).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ    PIC X(03) VALUE "NAO".
       77 CT-LIN     PIC 9(02) VALUE 31.
       77 CT-PAG     PIC 9(02) VALUE ZEROES.
       77 VALOR-TOT  PIC 9(12)V9(02) VALUE ZEROES.

       01 CAB-01.
           02 FILLER  PIC X(70) VALUE SPACES.
           02 FILLER  PIC X(05) VALUE "PAG. ".
           02 VAR-PAG PIC 99.
           02 FILLER  PIC X(03) VALUE SPACES.

       01 CAB-02.
           02 FILLER  PIC X(25) VALUE SPACES.
           02 FILLER  PIC X(27) VALUE "RELACAO DE SOCIOS ATRASADOS".
           02 FILLER  PIC X(25) VALUE SPACES.
           
       01 CAB-03.
           02 FILLER  PIC X(05) VALUE SPACES.
           02 FILLER  PIC X(15) VALUE "NUMERO DE SOCIO".
           02 FILLER  PIC X(10) VALUE SPACES.
           02 FILLER  PIC X(13) VALUE "NOME DO SOCIO".
           02 FILLER  PIC X(15) VALUE SPACES.
           02 FILLER  PIC X(22) VALUE "VALOR DO PAGAMENTO".
           
       01 CAB-04.

          02 FILLER    PIC X(05) VALUE SPACES.
          02 FILLER    PIC X(26) VALUE "Total de Socios atrasados:".
          02 FILLER    PIC X(02) VALUE SPACES.
          02 VAR-TOTAL PIC 9(05) VALUE ZEROES.
          02 FILLER    PIC X(42) VALUE SPACES.

       01 CAB-05.
          02 FILLER    PIC X(05) VALUE SPACES.
          02 FILLER    PIC X(26) VALUE "Valor Total atrasado     :".
          02 VAR-VALT  PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
          02 FILLER    PIC X(34) VALUE SPACES.

       01 DETALHE.
           02 FILLER    PIC X(09) VALUE SPACES.
           02 NUM       PIC 999.999.
           02 FILLER    PIC X(09) VALUE SPACES.
           02 NOME      PIC X(30).
           02 FILLER    PIC X(05) VALUE SPACES.
           02 VALOR     PIC ZZZ.ZZZ.ZZ9,99.
           02 FILLER    PIC X(06) VALUE SPACES.

       PROCEDURE DIVISION.

       PGM-EM02-01.
       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM CABECALHO-FIM.
       PERFORM FIM.
       STOP RUN.

       INICIO.
            OPEN INPUT  CADSOC1
                 OUTPUT CADSOC2
                        RELSOCIO.
            PERFORM LEITURA.

       LEITURA.
           READ CADSOC1 AT END MOVE "SIM" TO FIM-ARQ.

       COPIA.
           MOVE NUM-SOCIO1   TO NUM-SOCIO2.
           MOVE NOME-SOCIO1  TO NOME-SOCIO2.
           MOVE VALOR-PAG1   TO VALOR-PAG2.
           WRITE REG-SAI.

       IMPRESSAO.
           IF COD-PAG EQUAL 01
               PERFORM COPIA
           ELSE
               IF CT-LIN GREATER THAN 30
                   PERFORM CABECALHO
               END-IF
               PERFORM IMPDET
           END-IF.
       
       PRINCIPAL.
           PERFORM IMPRESSAO.
           PERFORM LEITURA.


       IMPDET.
           MOVE NUM-SOCIO1  TO NUM.
           MOVE NOME-SOCIO1  TO NOME.
           MOVE VALOR-PAG1   TO VALOR.
           WRITE REG-SOCIO FROM DETALHE AFTER ADVANCING 1 LINE.
           ADD 1 TO CT-LIN.
           ADD 1 TO VAR-TOTAL.
           ADD VALOR-PAG1 TO VALOR-TOT.

       CABECALHO.

           ADD 1       TO CT-PAG.
           MOVE CT-PAG TO VAR-PAG.
           MOVE SPACES TO REG-SOCIO.
           WRITE REG-SOCIO AFTER ADVANCING PAGE.
           WRITE REG-SOCIO FROM CAB-01 AFTER ADVANCING 2 LINES.
           WRITE REG-SOCIO FROM CAB-02 AFTER ADVANCING 2 LINES.
           WRITE REG-SOCIO FROM CAB-03 AFTER ADVANCING 2 LINES.
           MOVE SPACES TO REG-SOCIO.
           WRITE REG-SOCIO AFTER ADVANCING 1 LINE.
           MOVE 1 TO CT-LIN.

       CABECALHO-FIM.

           MOVE SPACES TO REG-SOCIO.
           MOVE VALOR-TOT TO VAR-VALT.
           WRITE REG-SOCIO FROM CAB-04 AFTER ADVANCING 2 LINES.
           WRITE REG-SOCIO FROM CAB-05 AFTER ADVANCING 2 LINES.


       FIM.
           CLOSE CADSOC1
                 CADSOC2
                 RELSOCIO.













