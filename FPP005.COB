       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGGER.
       AUTHOR. AUTMAN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PR ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS RK
           .

           SELECT CADCARGO ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS CRK.

           SELECT FILEFUNC ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS RKT
           .

       DATA DIVISION.
       FILE SECTION.
       FD PR
       LABEL RECORD IS STANDARD
       VALUE OF FILE-ID IS "PRT1.DATA".
       01 PRM.
           03 RK.
               05 RKF  PIC 9(06).
               05 RKM  PIC 9(02).
               05 RKY  PIC 9(04).
           03 PSALVALB     PIC 9(10)V99.
           03 PINSS     PIC 9(10)V99.
           03 PIMPR     PIC 9(10)V99.
           03 PSALF     PIC 9(10)V99.
           03 PSLIQUID  PIC 9(10)V99.

      *-----------------------------------------------------------------
      *CARGO VARIAVEIS
      *-----------------------------------------------------------------
       FD CADCARGO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCARGO1.DAT".
       01 CARGO.
           03 CRK       PIC 9(04).
           03 CDESC     PIC X(25).
           03 TS       PIC X(01).
           03 TSTXT    PIC X(12).
           03 BPS      PIC 9(01).
           03 BPSTXT   PIC X(20).
           03 BCOM     PIC 9(01).
           03 BCOMTXT  PIC X(20).
           03 BVA      PIC 9(01).
           03 BVATXT   PIC X(20).
           03 BCB      PIC 9(01).
           03 BCBTXT   PIC X(20).
           03 SALVAL   PIC 9(06)V99.
      *-----------------------------------------------------------------
       FD FILEFUNC
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "FUNCDATA.DAT".
       01 FILES.
           03 RKT          PIC 9(06).
           03 NOME        PIC X(40).
           03 CPF         PIC 9(11).
           03 RG          PIC 9(09).
           03 SEXO        PIC X(01).
           03 SEXOTXT     PIC X(10).
           03 GENERO      PIC 9(01).
           03 GENEROTXT   PIC X(15).
           03 DNDIA       PIC 9(02).
           03 DNMES       PIC 9(02).
           03 DNANO       PIC 9(04).
           03 NUMDEPEND   PIC 9(02).
           03 TELEFONE    PIC 9(11).
           03 EMAIL       PIC X(40).
      *-----------------------------------------------------------------
           03 ARK2        PIC 9(03).
           03 CRK2        PIC 9(04).
           03 CEP2        PIC 9(08).
           03 NUM          PIC 9(05).
           03 COMPLEMENTO  PIC X(12).
       WORKING-STORAGE SECTION.
           01 FS   PIC X(02).
      *-----------------------------------------------------------------
           01 SALVALB     PIC 9(10).
           01 INSS     PIC 9(10)V99.
           01 IMPR     PIC 9(10)V99.
           01 SALF     PIC 9(10)V99.
           01 SLIQUID  PIC 9(10)V99.
      *-----------------------------------------------------------------
           01 MENSAGEM  PIC X(40).
           01 OPT       PIC X(03).
           01 RKVAL     PIC 9(06).
           01 CONT      PIC 9(04).
           01 SBCONT    PIC 9(09).
           01 NUMD      PIC 9(02).
      *-----------------------------------------------------------------

       SCREEN SECTION.
       01  TELAPAG.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "ÚÒÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 01  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÒ¿".
           05  LINE 02  COLUMN 01
               VALUE  "³ºÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄ".
           05  LINE 02  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿º³".
           05  LINE 03  COLUMN 01
               VALUE  "³º³ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍ¾FOLHA DE".
           05  LINE 03  COLUMN 41
               VALUE  "PAGAMENTOÔÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»³º³".
           05  LINE 04  COLUMN 01
               VALUE  "³º³ºÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ÀÄÄ¿ÀÄÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 04  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÄÙÚÄÄÙÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿º³º³".
           05  LINE 05  COLUMN 01
               VALUE  "³º³º³                  ÀÄ¿ ÀÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 05  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÙ ÚÄÙ                  ³º³º³".
           05  LINE 06  COLUMN 01
               VALUE  "³º³º³                    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 06  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ                    ³º³º³".
           05  LINE 07  COLUMN 01
               VALUE  "³º³º³   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ ÚÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           05  LINE 07  COLUMN 41
               VALUE  " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ ÚÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³º³º³".
           05  LINE 08  COLUMN 01
               VALUE  "³º³º³  Ú´CODIGO       ÃÂ´PERIODO      ÃÂ".
           05  LINE 08  COLUMN 41
               VALUE  "Â´SALARIO      ÃÂ´INSS         Ã¿  ³º³º³".
           05  LINE 09  COLUMN 01
               VALUE  "³º³º³  ³ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³".
           05  LINE 09  COLUMN 41
               VALUE  "³ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³  ³º³º³".
           05  LINE 10  COLUMN 01
               VALUE  "³º³º³  ³               ³    |          ³".
           05  LINE 10  COLUMN 41
               VALUE  "³               ³               ³  ³º³º³".
           05  LINE 11  COLUMN 01
               VALUE  "³º³º³  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁ".
           05  LINE 11  COLUMN 41
               VALUE  "ÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ  ³º³º³".
           05  LINE 12  COLUMN 01
               VALUE  "³º³º³ ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ ÚÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 12  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄ¿ ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ ³º³º³".
           05  LINE 13  COLUMN 01
               VALUE  "³º³º³Ú´IMPOSTO - RENDA    ÃÂ´SALARIO - L".
           05  LINE 13  COLUMN 41
               VALUE  "IQUIDO     ÃÂ´SALARIO - FAMILIA  Ã¿³º³º³".
           05  LINE 14  COLUMN 01
               VALUE  "³º³º³³ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³ÀÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 14  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÙ³ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³º³º³".
           05  LINE 15  COLUMN 01
               VALUE  "³º³º³³                     ³".
           05  LINE 15  COLUMN 41
               VALUE  "            ³                     ³³º³º³".
           05  LINE 16  COLUMN 01
               VALUE  "³º³º³ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
           05  LINE 16  COLUMN 41
               VALUE  "            ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³º³º³".
           05  LINE 17  COLUMN 01
               VALUE  "³º³ºÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 17  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙº³º³".
           05  LINE 18  COLUMN 01
               VALUE  "³º³ÈÍÍÍÍÍÍÍÍÍÍ» ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 18  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ º³º³".
           05  LINE 19  COLUMN 01
               VALUE  "³º³ ÚÄÄÄÄÄÄÄÄ¿ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 19  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼³º³".
           05  LINE 20  COLUMN 01
               VALUE  "³º³Ú´MENSAGEMÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 20  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿³º³".
           05  LINE 21  COLUMN 01
               VALUE  "³º³³ÀÄÄÄÄÄÄÄÄÙ".
           05  LINE 21  COLUMN 41
               VALUE  "               ³op:   ³rk:          ³³º³".
           05  LINE 22  COLUMN 01
               VALUE  "³º³ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 22  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³º³".
           05  LINE 23  COLUMN 01
               VALUE  "³ÈÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 23  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏ¼³".
           05  LINE 24  COLUMN 01
               VALUE  "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 24  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
           05  TRKF
               LINE 10  COLUMN 10  PIC 9(06)
               USING  RKF.
           05  TRKM
               LINE 10  COLUMN 26  PIC 9(02)
               USING  RKM.
           05  TRKY
               LINE 10  COLUMN 31  PIC 9(04)
               USING  RKY.
           05  TSALVAL
               LINE 10  COLUMN 43  PIC 9(06)V99
               USING  PSALVALB.
           05  TINSS
               LINE 10  COLUMN 59  PIC 9(10)V99
               USING  PINSS.
           05  TIMPR
               LINE 15  COLUMN 08  PIC 9(10)V99
               USING  PIMPR.
           05  TSLIQUID
               LINE 15  COLUMN 30  PIC 9(10)V99
               USING  PSLIQUID.
           05  TSALF
               LINE 15  COLUMN 55  PIC 9(10)V99
               USING  PSALF.
           05  TMENSAGEM
               LINE 21  COLUMN 15  PIC X(40)
               USING  MENSAGEM.
           05  TOPT
               LINE 21  COLUMN 60  PIC X(03)
               USING  OPT.
           05  TRKVAL
               LINE 21  COLUMN 67  PIC 9(06)
               USING  RKVAL.
       PROCEDURE DIVISION.
       R01.
           OPEN I-O PR
           IF FS NOT = "00"
               OPEN OUTPUT PR
               CLOSE PR
               GO TO R01
       .
       R01A.
           OPEN I-O CADCARGO
           IF FS NOT = "00"
               OPEN INPUT CADCARGO
               CLOSE CADCARGO
               GO TO R01A
       .
       R01AA.
           OPEN I-O FILEFUNC
           IF FS NOT = "00"
               OPEN INPUT FILEFUNC
               CLOSE FILEFUNC
               GO TO R01AA
       .
       R02A.

           MOVE ZEROS TO PSALVALB PINSS PIMPR PSALF PSLIQUID
      *-----------------------------------------------------------------
           MOVE ZEROS TO INSS IMPR SALF SLIQUID RKVAL CONT
           MOVE SPACES TO OPT MENSAGEM
      *-----------------------------------------------------------------
           MOVE ZEROS TO RKF RKM RKY
      *-----------------------------------------------------------------
           MOVE ZEROS TO CRK BPS BCOM BVA BCB SALVAL SALVALB SBCONT NUMD
           MOVE SPACES TO CDESC TS TSTXT BPSTXT BCOMTXT BVATXT BCBTXT
           DISPLAY TELAPAG.
      *-----------------------------------------------------------------
           MOVE ZEROS TO RKT CPF RG GENERO DNDIA DNMES DNANO NUMDEPEND
           MOVE ZERO TO TELEFONE
           MOVE SPACES TO NOME SEXO SEXOTXT GENEROTXT EMAIL
      *-----------------------------------------------------------------
           .
       R02.
           ACCEPT TRKF
           IF RKF = ZEROS
               MOVE "CODIFO NAO PODE SER NULO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R02
       .
       R03.
           ACCEPT TRKM
           IF RKM = ZEROS
               MOVE "CODIFO NAO PODE SER NULO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R03
           ELSE
               IF RKM > 12
                   MOVE "MES NAO PODE SER MAIOR QUE 12" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
                   GO TO R03
       .
       R04.
           ACCEPT TRKY
           IF RKY = ZEROS
               MOVE "ANO NAO PO SER NULO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R04
       .
       R05.
           READ PR
           IF FS = "00"
               DISPLAY TRKF
               DISPLAY TRKM
               DISPLAY TRKY
               DISPLAY TSALVAL
               DISPLAY TINSS
               DISPLAY TIMPR
               DISPLAY TSALF
               DISPLAY TSLIQUID
               GO TO R07
           ELSE
               MOVE "NAO CADASTRADO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
      *         GO TO FF1
           .
       R05B.
           MOVE RKF TO RKT
           READ FILEFUNC
           IF FS = "00"
               MOVE "EXISTE" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               MOVE NUMDEPEND TO NUMD
           ELSE
               MOVE "ERRO AO LER" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               MOVE RKT TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO FF1
           .
       R06.
           MOVE CRK2 TO CRK
           READ CADCARGO
      *     IF FS = "00"
      *         MOVE "EXECUTANDO" TO MENSAGEM
      *         PERFORM MSG1 THRU MSG2
           MOVE SALVAL TO SALVALB
      *         MOVE TS TO MENSAGEM
      *         PERFORM MSG1 THRU MSG2
           PERFORM SALBCALC THRU LIQUIDCALC
           PERFORM RS

      *         PERFORM INSSCALC
      *         PERFORM IMPSRCALC

      *     ELSE
      *     MOVE "ERRO AO LER CARGO" TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
      *     MOVE CRK2 TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
           .
       R07.
           MOVE "DIGITE: R-REPETIR|X-EXCLUIR" TO
           MENSAGEM
           PERFORM MSG1 THRU MSG2
           ACCEPT TOPT
           IF OPT = "R"
               GO TO R02A
           ELSE
               IF OPT = "X"
                   PERFORM R02A THRU R04
                   PERFORM RX
                   GO TO R07
           .
       FF1.
           STOP RUN
           .
       FF2.
           EXIT PROGRAM
           .
       MSG1.
           MOVE ZEROS TO CONT
           DISPLAY TMENSAGEM
           .
       MSG2.
           ADD 1 TO CONT
           IF CONT < 5555
               GO TO MSG2
           MOVE SPACES TO MENSAGEM
           DISPLAY TMENSAGEM
           .
       SALBCALC.
           MOVE ZEROS TO SBCONT
           IF TS = "H"
              COMPUTE SBCONT = 220
              COMPUTE SALVALB = SALVALB * SBCONT
      *        MOVE SALVALB TO MENSAGEM
      *        PERFORM MSG1 THRU MSG2
              MOVE SALVALB TO PSALVALB
              DISPLAY TSALVAL
           ELSE
               IF TS = "D"
                   COMPUTE SBCONT = 30
                   COMPUTE SALVALB = SALVALB * SBCONT
      *             MOVE SALVALB TO MENSAGEM
      *             PERFORM MSG1 THRU MSG2
                   MOVE SALVALB TO PSALVALB
                   DISPLAY TSALVAL
               ELSE
                   IF TS = "M"
                       COMPUTE SBCONT = 1
                       COMPUTE SALVALB = SALVALB * SBCONT
      *                 MOVE SALVALB TO MENSAGEM
      *                 PERFORM MSG1 THRU MSG2
                       MOVE SALVALB TO PSALVALB
                       DISPLAY TSALVAL
       .
       INSSCALC.
           IF SALVALB > 0,01 AND SALVALB < 1350,00
               COMPUTE INSS = SALVALB *8,0
               IF INSS > 400
                   MOVE 650,00 TO INSS
                   MOVE INSS TO PINSS
                   DISPLAY TINSS
               ELSE
                   MOVE INSS TO PINSS
                   DISPLAY TINSS
           ELSE
               IF SALVALB > 1350,00 AND SALVALB < 3400,00
                   COMPUTE INSS = SALVALB *9,0
                   IF INSS > 400
                       MOVE 650,00 TO INSS
                       MOVE INSS TO PINSS
                       DISPLAY TINSS
                   ELSE
                       MOVE INSS TO PINSS
                       DISPLAY TINSS
               ELSE
                   IF SALVALB > 3400,00
                       COMPUTE INSS = SALVALB *10,0
                       IF INSS > 400
                           MOVE 650,00 TO INSS
                           MOVE INSS TO PINSS
                           DISPLAY TINSS
                       ELSE
                           MOVE INSS TO PINSS
                           DISPLAY TINSS

           .
       IMPSRCALC.
           COMPUTE IMPR = SALVALB - INSS
           IF IMPR < 3500
               MOVE IMPR TO PIMPR
               DISPLAY TIMPR
           ELSE
               IF IMPR > 3500,01 AND IMPR < 7650,00
                   COMPUTE IMPR = IMPR * 0,10
                   MOVE IMPR TO PIMPR
                   DISPLAY TIMPR
               ELSE
                   IF IMPR > 7650,01 AND IMPR < 17000,00
                       COMPUTE IMPR = IMPR * 0,15
                       MOVE IMPR TO PIMPR
                       DISPLAY TIMPR
                   ELSE
                       IF IMPR > 17000,00
                          COMPUTE IMPR = IMPR * 0,25
                          MOVE IMPR TO PIMPR
                          DISPLAY TIMPR

           .
       SALFCALC.
           COMPUTE SALF = NUMD * 34,00
           MOVE SALF TO PSALF
           DISPLAY TSALF
           .
       LIQUIDCALC.
           COMPUTE SLIQUID = SALVALB - INSS - IMPR + SALF
           MOVE SLIQUID TO PSLIQUID
           DISPLAY TSLIQUID
           .

       RS.
           WRITE PRM
           IF FS = "00"
               MOVE "GRAVADO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
           ELSE
               MOVE "ERRO AO GRAVAR" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
           .
       RX.
           DELETE PR RECORD
           IF FS = "00"
               MOVE "EXCLUIDO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
           ELSE
               MOVE "ERRO AO EXCLUIR" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
           .
