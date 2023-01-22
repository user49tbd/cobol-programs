       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP003.
       AUTHOR. AUT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCARGO ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS RK.
       DATA DIVISION.
       FILE SECTION.
       FD CADCARGO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCARGO1.DAT".
       01 CARGO.
           03 RK       PIC 9(04).
           03 DESC     PIC X(25).
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
       WORKING-STORAGE SECTION.
           01 FS   PIC X(02).
           01 CONT PIC 9(04).
           01 CONT2 PIC 9(01).
      *     01 CONT3 PIC 9(01).
           01 SERVICO  PIC X(01).
           01 SERVICO2 PIC X(01).
           01 TPSERV   PIC X(01).
           01 VALI PIC X(01).
           01 OP1  PIC 9(06)V99.
      *     01
      *-----------------------------------------------------------------
           01 TPBEN    PIC 9(01).
           01 BENTXT   PIC X(17).
           01 MENSAGEM PIC X(40).
           01 SNB      PIC X(01).
           01 SRK      PIC 9(04).
           01 LVAL     PIC X(21).
           01 OPO       PIC X(01).
           01 BENTP    PIC 9(01).
           01 STA1.
               03 STA1A     PIC X(21) OCCURS 4 TIMES.
           01 STA2.
               03 STA2A     PIC X(21) OCCURS 3 TIMES.
      *----BENEFICIO----------------------------------------------------
           01 TABB.
               03 FILLER   PIC X(21) VALUE "1PLANO DE SAUDE".
               03 FILLER   PIC X(21) VALUE "2COMBUSTIVEL".
               03 FILLER   PIC X(21) VALUE "3VALE ALIMENTACAO".
               03 FILLER   PIC X(21) VALUE "4CESTA BASICA".
           01 AB REDEFINES TABB.
               03 ARRB     PIC X(21) OCCURS 4 TIMES.
      *----TIPO DE SERVICO----------------------------------------------
           01 TABTS.
               03 FILLER   PIC X(21) VALUE "HHORISTA".
               03 FILLER   PIC X(21) VALUE "DDIARISTA".
               03 FILLER   PIC X(21) VALUE "MMENSALISTA".
           01 ATS REDEFINES TABTS.
               03 ARRBTS   PIC X(21) OCCURS 3 TIMES.
      *----PLANO DE SAUDE-----------------------------------------------
           01 TABBPS.
               03 FILLER   PIC X(21) VALUE "1PLANO GLOBAL".
               03 FILLER   PIC X(21) VALUE "2PLANO NACIONAL".
               03 FILLER   PIC X(21) VALUE "3PLANO REGIONAL".
               03 FILLER   PIC X(21) VALUE "4PLANO BASIC".
           01 ABPS REDEFINES TABBPS.
               03 ARRBBPS   PIC X(21) OCCURS 4 TIMES.
      *----COMBUSTIVEL--------------------------------------------------
           01 TABBCOM.
               03 FILLER   PIC X(21) VALUE "1PLANO MASTER".
               03 FILLER   PIC X(21) VALUE "2PLANO DIAMOND".
               03 FILLER   PIC X(21) VALUE "3PLANO GOLD".
               03 FILLER   PIC X(21) VALUE "4PLANO BASIC".
           01 ABCOM REDEFINES TABBCOM.
               03 ARRBBCOM   PIC X(21) OCCURS 4 TIMES.
      *----VALE ALIMENTAгцO---------------------------------------------
           01 TABBVA.
               03 FILLER   PIC X(21) VALUE "1PLANO RED".
               03 FILLER   PIC X(21) VALUE "2PLANO GREEN".
               03 FILLER   PIC X(21) VALUE "3PLANO BLUE".
               03 FILLER   PIC X(21) VALUE "4PLANO BASIC".
           01 ABVA REDEFINES TABBVA.
               03 ARRBBVA   PIC X(21) OCCURS 4 TIMES.
      *----CESTA BASICA-------------------------------------------------
           01 TABBCB.
               03 FILLER   PIC X(21) VALUE "1PLANO LARGE".
               03 FILLER   PIC X(21) VALUE "2PLANO BIG".
               03 FILLER   PIC X(21) VALUE "3PLANO SMALL".
               03 FILLER   PIC X(21) VALUE "4PLANO BASIC".
           01 ABCB REDEFINES TABBCB.
               03 ARRBBCB   PIC X(21) OCCURS 4 TIMES.
           01 CHECKB.
               03 CHECKB1    PIC X(01).
               03 CHECKB2    PIC X(20).

       SCREEN SECTION.
       01  TELACARGO.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "зддддддддддддддддддддддддддддддддддддддд".
           05  LINE 01  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддддддддддддддд©".
           05  LINE 02  COLUMN 01
               VALUE  "Ёимммммммммммммммммммммммммммммммммммммм".
           05  LINE 02  COLUMN 41
               VALUE  "мммммммммммммммммммммммммммммммммммммм╩Ё".
           05  LINE 03  COLUMN 01
               VALUE  "Ё╨ з©   здддддддд©    здддддддддд©".
           05  LINE 03  COLUMN 41
               VALUE  "                      здддддддддддддд©╨Ё".
           05  LINE 04  COLUMN 01
               VALUE  "Ё╨ Ёюд©з╢PROGRAM3цдд© ЁDESCRICAO:цдддддд".
           05  LINE 04  COLUMN 41
               VALUE  "дддддддддддддддддддд© ЁSALARIO:      Ё╨Ё".
           05  LINE 05  COLUMN 01
               VALUE  "Ё╨ Ё  ЁЁюдддддддды  ю©юбддддддддды".
           05  LINE 05  COLUMN 41
               VALUE  "                    Ё Ё              Ё╨Ё".
           05  LINE 06  COLUMN 01
               VALUE  "Ё╨ ю©зыЁзддддддддддд©Ё юдддддддддддддддд".
           05  LINE 06  COLUMN 41
               VALUE  "дддддддддддддддддддды юдддддддддддддды╨Ё".
           05  LINE 07  COLUMN 01
               VALUE  "Ё╨  ЁЁзыЁCARGO:     ЁЁзддддддддддддддддб".
           05  LINE 07  COLUMN 41
               VALUE  "дддбддддддддддддддддддддддддддддддддд©╨Ё".
           05  LINE 08  COLUMN 01
               VALUE  "Ё╨  ЁЁЁ юдддддддддддыЁЁTIPO DE SERVICO:Ё".
           05  LINE 08  COLUMN 41
               VALUE  "   Ё                                 Ё╨Ё".
           05  LINE 09  COLUMN 01
               VALUE  "Ё╨ зыЁЁ зддддддддддддыюдддддддддддддддда".
           05  LINE 09  COLUMN 41
               VALUE  "дддаддддддддддддддддддддддддддддддддды╨Ё".
           05  LINE 10  COLUMN 01
               VALUE  "Ё╨ Ё ЁЁ Ёздддддддддддбдбдддбдддддддддддд".
           05  LINE 10  COLUMN 41
               VALUE  "дддддддддддддд©зддддддддддддддддддддд©╨Ё".
           05  LINE 11  COLUMN 01
               VALUE  "Ё╨ Ё ЁЁ ЁЁBENEFICIO :Ё Ё   Ё".
           05  LINE 11  COLUMN 41
               VALUE  "              ЁЁ                     Ё╨Ё".
           05  LINE 12  COLUMN 01
               VALUE  "Ё╨ ю©ЁЁ Ёюбддддддддддададдды".
           05  LINE 12  COLUMN 41
               VALUE  "              Ёцддддддддддддддддддддд╢╨Ё".
           05  LINE 13  COLUMN 01
               VALUE  "Ё╨  ЁЁЁ Ё Ё1-PLANO:".
           05  LINE 13  COLUMN 41
               VALUE  "              ЁЁ                     Ё╨Ё".
           05  LINE 14  COLUMN 01
               VALUE  "Ё╨  ЁЁю©Ё Ё2-PLANO:".
           05  LINE 14  COLUMN 41
               VALUE  "              ЁЁ                     Ё╨Ё".
           05  LINE 15  COLUMN 01
               VALUE  "Ё╨ зыю©ЁЁ Ё3-PLANO:".
           05  LINE 15  COLUMN 41
               VALUE  "              ЁЁ                     Ё╨Ё".
           05  LINE 16  COLUMN 01
               VALUE  "Ё╨ Ё  ЁЁЁ Ё4-PLANO:".
           05  LINE 16  COLUMN 41
               VALUE  "              ЁЁ                     Ё╨Ё".
           05  LINE 17  COLUMN 01
               VALUE  "Ё╨ Ёздыюы Ё".
           05  LINE 17  COLUMN 41
               VALUE  "              ЁЁ                     Ё╨Ё".
           05  LINE 18  COLUMN 01
               VALUE  "Ё╨ юы     юдддд©".
           05  LINE 18  COLUMN 41
               VALUE  "              ЁЁ                     Ё╨Ё".
           05  LINE 19  COLUMN 01
               VALUE  "Ё╨  зддддддддд©юдддддддддддддддддддддддд".
           05  LINE 19  COLUMN 41
               VALUE  "ддддддддддддддыюддддддддддддддддддддды╨Ё".
           05  LINE 20  COLUMN 01
               VALUE  "Ё╨ з╢MENSAGEM:цддддддддддддддддддддддддд".
           05  LINE 20  COLUMN 41
               VALUE  "ддддддддддддддддбддддддбддддддддддддд©╨Ё".
           05  LINE 21  COLUMN 01
               VALUE  "Ё╨ Ёюддддддддды".
           05  LINE 21  COLUMN 41
               VALUE  "                ЁOP:   ЁRK:          Ё╨Ё".
           05  LINE 22  COLUMN 01
               VALUE  "Ё╨ юдддддддддддддддддддддддддддддддддддд".
           05  LINE 22  COLUMN 41
               VALUE  "ддддддддддддддддаддддддаддддддддддддды╨Ё".
           05  LINE 23  COLUMN 01
               VALUE  "Ёхмммммммммммммммммммммммммммммммммммммм".
           05  LINE 23  COLUMN 41
               VALUE  "мммммммммммммммммммммммммммммммммммммм╪Ё".
           05  LINE 24  COLUMN 01
               VALUE  "юддддддддддддддддддддддддддддддддддддддд".
           05  LINE 24  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддддддддддддддды".
           05  TDESC
               LINE 05  COLUMN 35  PIC X(25)
               USING  DESC.
           05  TSALVAL
               LINE 05  COLUMN 64  PIC 9(06)V99
               USING  SALVAL.
           05  TRK
               LINE 07  COLUMN 17  PIC 9(04)
               USING  RK.
           05  TTS
               LINE 08  COLUMN 42  PIC X(01)
               USING  TS.
           05  TTSTXT
               LINE 08  COLUMN 45  PIC X(12)
               USING  TSTXT.
           05  TTPBEN
               LINE 11  COLUMN 23  PIC 9(01)
               USING  TPBEN.
           05  TSNB
               LINE 11  COLUMN 26  PIC X(01)
               USING  SNB.
           05  TBENTXT
               LINE 11  COLUMN 30  PIC X(17)
               USING  BENTXT.
           05  TBPS
               LINE 13  COLUMN 21  PIC X(01)
               USING  BPS.
           05  TBPSTXT
               LINE 13  COLUMN 23  PIC X(21)
               USING  BPSTXT.
           05  TBCOM
               LINE 14  COLUMN 21  PIC X(01)
               USING  BCOM.
           05  TBCOMTXT
               LINE 14  COLUMN 23  PIC X(21)
               USING  BCOMTXT.
           05  TBVA
               LINE 15  COLUMN 21  PIC X(01)
               USING  BVA.
           05  TBVATXT
               LINE 15  COLUMN 23  PIC X(21)
               USING  BVATXT.
           05  TBCB
               LINE 16  COLUMN 21  PIC X(01)
               USING  BCB.
           05  TBCBTXT
               LINE 16  COLUMN 23  PIC X(21)
               USING  BCBTXT.
           05  TMENSAGEM
               LINE 21  COLUMN 16  PIC X(40)
               USING  MENSAGEM.
           05  TOPO
               LINE 21  COLUMN 62  PIC X(01)
               USING  OPO.
           05  TSRK
               LINE 21  COLUMN 69  PIC 9(04)
               USING  SRK.

       01  B1.
           05  LINE 11  COLUMN 57
               VALUE  "1 - PLANO DE SAUDE  ".
           05  LINE 13  COLUMN 57
               VALUE  "1 - PLANO GLOBAL    ".
           05  LINE 14  COLUMN 57
               VALUE  "2 - PLANO NACIONAL  ".
           05  LINE 15  COLUMN 57
               VALUE  "3 - PLANO REGIONAL  ".
           05  LINE 16  COLUMN 57
               VALUE  "4 - PLANO BASIC     ".
       01  B2.
           05  LINE 11  COLUMN 57
               VALUE  "2 - COMBUSTIVEL     ".
           05  LINE 13  COLUMN 57
               VALUE  "1 - PLANO MASTER    ".
           05  LINE 14  COLUMN 57
               VALUE  "2 - PLANO DIAMOND   ".
           05  LINE 15  COLUMN 57
               VALUE  "3 - PLANO GOLD      ".
           05  LINE 16  COLUMN 57
               VALUE  "4 - PLANO BASIC     ".
       01  B3.
           05  LINE 11  COLUMN 57
               VALUE  "3 - VALE ALIMENTACAO".
           05  LINE 13  COLUMN 57
               VALUE  "1 - PLANO RED       ".
           05  LINE 14  COLUMN 57
               VALUE  "2 - PLANO GREEN     ".
           05  LINE 15  COLUMN 57
               VALUE  "3 - PLANO BLUE      ".
           05  LINE 16  COLUMN 57
               VALUE  "4 - PLANO BASIC     ".
       01  B4.
           05  LINE 11  COLUMN 57
               VALUE  "4 - CESTA BASICA    ".
           05  LINE 13  COLUMN 57
               VALUE  "1 - PLANO LARGE     ".
           05  LINE 14  COLUMN 57
               VALUE  "2 - PLANO BIG       ".
           05  LINE 15  COLUMN 57
               VALUE  "3 - PLANO SMALL     ".
           05  LINE 16  COLUMN 57
               VALUE  "4 - PLANO BASIC     ".
       PROCEDURE DIVISION.
       R01.
           OPEN I-O CADCARGO
           IF FS NOT = "00"
               OPEN OUTPUT CADCARGO
               CLOSE CADCARGO
               GO TO R01
           .
       R02.
           MOVE SPACES TO DESC TS TSTXT BPSTXT BCOMTXT BVATXT BCBTXT
           MOVE SPACES TO BENTXT MENSAGEM SNB OPO VALI SERVICO
           MOVE ZEROS TO RK BPS BCOM BVA BCB SALVAL CONT TPBEN SRK
           MOVE ZEROS TO CONT CONT2

           .
       R02A.
           DISPLAY TELACARGO
           .
       R03.
           ACCEPT TRK
           IF RK = ZEROS
               MOVE "CARGO NAO PODE SER NULO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
           .
       R04.
           READ CADCARGO
           IF FS = "00"
               DISPLAY TRK
               DISPLAY TDESC
               DISPLAY TSALVAL
               DISPLAY TTS
               DISPLAY TTSTXT

               DISPLAY TBPS
               DISPLAY TBPSTXT

               DISPLAY TBCOM
               DISPLAY TBCOMTXT

               DISPLAY TBVA
               DISPLAY TBVATXT

               DISPLAY TBCB
               DISPLAY TBCBTXT
               MOVE "CARGO JA CADASTRADO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R013
           .
       R05.
           ACCEPT TDESC
           IF DESC = SPACES
               MOVE "DESCRICAO NAO PODE SER NULA" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R05
           MOVE ZEROS TO CONT2

           .
       R06.
           ADD 1 TO CONT2
      *     MOVE CONT2 TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2

           MOVE ARRBTS(CONT2) TO CHECKB

           MOVE CHECKB TO STA2A(CONT2)

      *     MOVE STA2A(CONT2) TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
           IF CONT2 < 3
               GO TO R06
           ELSE
      *         MOVE "DDDDDD" TO MENSAGEM
      *         PERFORM MSG1 THRU MSG2
               ACCEPT TTS
               MOVE "L" TO TPSERV
               MOVE TS TO SERVICO
               IF TS = SPACES
                   MOVE "TIPO DE SERVICO NAO PODE SER NULA" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
                   GO TO R06
               ELSE
                   PERFORM RARR1I THRU RARR2I
                   IF VALI = "F"
                       MOVE "NAO FOI ENCONTRADO O servico" TO MENSAGEM
                       PERFORM MSG1 THRU MSG2
                       MOVE ZEROS TO CONT2
                       ADD 1 TO CONT2
                       GO TO R06
                   ELSE
                       MOVE LVAL TO TSTXT
                       DISPLAY TTSTXT
               MOVE ZEROS TO CONT2
               .
       R07V.
           IF TS = "H"
               MOVE 12987,34 TO SALVAL
               DISPLAY TSALVAL
           ELSE
               IF TS = "D"
                   MOVE 42000,34 TO SALVAL
                   DISPLAY TSALVAL
               ELSE
                   IF TS = "M"
                       MOVE 952000,34 TO SALVAL
                       DISPLAY TSALVAL
               .
      *PLANO SAUDE------------------------------------------------------
       R08A.
           ADD 1 TO CONT2
      *     MOVE "BENEFICIOKKKKKK" TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
           ADD 1 TO TPBEN
           PERFORM R07
           MOVE ZEROS TO CONT2
           DISPLAY B1
           .
       R08B.
           MOVE SPACES TO SNB
           DISPLAY TSNB
           ACCEPT TSNB
           IF SNB = "N"
               MOVE "SEM PLANO" TO LVAL
               MOVE ZEROS TO BPS
               DISPLAY TBPS
               MOVE LVAL TO BPSTXT
               DISPLAY TBPSTXT
               GO TO R09A
           ELSE
               IF SNB NOT = "S"
                   MOVE "VALOR INCORRETO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
                   GO TO R08B
           .
       R08.
           ADD 1 TO CONT2
      *     MOVE CONT2 TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2

           MOVE ARRBBPS(CONT2) TO CHECKB

           MOVE CHECKB TO STA1A(CONT2)

      *     MOVE STA1A(CONT2) TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
           IF CONT2 < 4
               GO TO R08
           ELSE
      *         MOVE "DDDDDD" TO MENSAGEM
      *         PERFORM MSG1 THRU MSG2
               ACCEPT TBPS
      *         MOVE TPBEN TO BPS
      *         DISPLAY TBPS
               MOVE "N" TO TPSERV
               MOVE BPS TO SERVICO2
               IF BPS = SPACES
                   MOVE "TIPO DE SERVICO NAO PODE SER NULO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
                   MOVE ZEROS TO CONT2
                   GO TO R08
               ELSE
                   PERFORM RARR1I THRU RARR2I
                   IF VALI = "F"
                       MOVE "NAO FOI ENCONTRADO O SERVICO" TO MENSAGEM
                       PERFORM MSG1 THRU MSG2
                       MOVE ZEROS TO CONT2
                       GO TO R08
                   ELSE
                       PERFORM RSAL
                       MOVE LVAL TO BPSTXT
                       DISPLAY TBPSTXT
               MOVE ZEROS TO CONT2
               ADD 1 TO CONT2
               MOVE SPACES TO SNB
               DISPLAY TSNB
               .
      *-----------------------------------------------------------------
      *PLANO COMBUSTIVEL------------------------------------------------
       R09A.
           PERFORM R08A
           DISPLAY B2
           .
       R09B.
           MOVE SPACES TO SNB
           DISPLAY TSNB
           ACCEPT TSNB
           IF SNB = "N"
               MOVE "SEM PLANO" TO LVAL
               MOVE ZEROS TO BCOM
               DISPLAY TBCOM
               MOVE LVAL TO BCOMTXT
               DISPLAY TBCOMTXT
               GO TO R010A
           ELSE
               IF SNB NOT = "S"
                   MOVE "VALOR INCORRETO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
                   GO TO R09B
           .
       R09.
           ADD 1 TO CONT2
      *     MOVE CONT2 TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
      *-------------
           MOVE ARRBBCOM(CONT2) TO CHECKB
      *-------------
           MOVE CHECKB TO STA1A(CONT2)

      *     MOVE STA1A(CONT2) TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
           IF CONT2 < 4
      *-----------
               GO TO R09
      *-----------
           ELSE
      *             MOVE "DDDDDD" TO MENSAGEM
      *             PERFORM MSG1 THRU MSG2
      *    -------------
               ACCEPT TBCOM
      *    -------------
      *         MOVE TPBEN TO BPS
      *         DISPLAY TBPS
               MOVE "N" TO TPSERV
      *    -------------
               MOVE BCOM TO SERVICO2
      *    -------------
      *    -------------
               IF BCOM = SPACES
      *    -------------
                   MOVE "TIPO DE SERVICO NAO PODE SER NULO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
                   MOVE ZEROS TO CONT2
      *    -------------
                   GO TO R09
      *    -------------
               ELSE
                   PERFORM RARR1I THRU RARR2I
                   IF VALI = "F"
                       MOVE "NAO FOI ENCONTRADO O SERVICO" TO MENSAGEM
                       PERFORM MSG1 THRU MSG2
                       MOVE ZEROS TO CONT2
      *    -------------
                       GO TO R09
      *    -------------
                   ELSE
      *    -------------
                       PERFORM RSAL
                       MOVE LVAL TO BCOMTXT
                       DISPLAY TBCOMTXT
      *    -------------
               MOVE ZEROS TO CONT2
               ADD 1 TO CONT2
               MOVE SPACES TO SNB
               DISPLAY TSNB
               .
      *-----------------------------------------------------------------
      *VALE ALIMENTACAO-------------------------------------------------
       R010A.
           PERFORM R08A
           DISPLAY B3
           .
       R010B.
           MOVE SPACES TO SNB
           DISPLAY TSNB
           ACCEPT TSNB
           IF SNB = "N"
               MOVE "SEM PLANO" TO LVAL
               MOVE ZEROS TO BVA
               DISPLAY TBVA
               MOVE LVAL TO BVATXT
               DISPLAY TBVATXT
               GO TO R011A
           ELSE
               IF SNB NOT = "S"
                   MOVE "VALOR INCORRETO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
                   GO TO R010B
           .
       R010.
           ADD 1 TO CONT2
      *     MOVE CONT2 TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
      *-------------
           MOVE ARRBBVA(CONT2) TO CHECKB
      *-------------
           MOVE CHECKB TO STA1A(CONT2)

      *     MOVE STA1A(CONT2) TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
           IF CONT2 < 4
      *-----------
               GO TO R010
      *-----------
           ELSE
      *         MOVE "DDDDDD" TO MENSAGEM
      *         PERFORM MSG1 THRU MSG2
      *-------------
               ACCEPT TBVA
      *    -------------
      *         MOVE TPBEN TO BPS
      *         DISPLAY TBPS
               MOVE "N" TO TPSERV
      *    -------------
               MOVE BVA TO SERVICO2
      *    -------------
      *    -------------
               IF BVA = SPACES
      *    -------------
                   MOVE "TIPO DE SERVICO NAO PODE SER NULO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
                   MOVE ZEROS TO CONT2
      *    -------------
                   GO TO R010
      *    -------------
               ELSE
                   PERFORM RARR1I THRU RARR2I
                   IF VALI = "F"
                       MOVE "NAO FOI ENCONTRADO O SERVICO" TO MENSAGEM
                       PERFORM MSG1 THRU MSG2
                       MOVE ZEROS TO CONT2
      *    -------------
                       GO TO R010
      *    -------------
                   ELSE
      *    -------------
                       PERFORM RSAL
                       MOVE LVAL TO BVATXT
                       DISPLAY TBVATXT
      *    -------------
               MOVE ZEROS TO CONT2
               ADD 1 TO CONT2
               MOVE SPACES TO SNB
               DISPLAY TSNB
               .
      *-----------------------------------------------------------------
      *CESTA BASICA------------------------------------------------
       R011A.
           PERFORM R08A
           DISPLAY B4
           .
       R011B.
           MOVE SPACES TO SNB
           DISPLAY TSNB
           ACCEPT TSNB
           IF SNB = "N"
               MOVE "SEM PLANO" TO LVAL
               MOVE ZEROS TO BCB
               DISPLAY TBCB
               MOVE LVAL TO BCBTXT
               DISPLAY TBCBTXT
               GO TO R011Z
           ELSE
               IF SNB NOT = "S"
                   MOVE "VALOR INCORRETO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
                   GO TO R011A

           .
       R011.
           ADD 1 TO CONT2
      *     MOVE CONT2 TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
      *-------------
           MOVE ARRBBCB(CONT2) TO CHECKB
      *-------------
           MOVE CHECKB TO STA1A(CONT2)

      *     MOVE STA1A(CONT2) TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
           IF CONT2 < 4
      *-----------
               GO TO R011
      *-----------
           ELSE
      *         MOVE "DDDDDD" TO MENSAGEM
      *         PERFORM MSG1 THRU MSG2
      *-------------
               ACCEPT TBCB
      *    -------------
      *         MOVE TPBEN TO BPS
      *         DISPLAY TBPS
               MOVE "N" TO TPSERV
      *    -------------
               MOVE BCB TO SERVICO2
      *    -------------
      *    -------------
               IF BCB = SPACES
      *    -------------
                   MOVE "TIPO DE SERVICO NAO PODE SER NULO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
                   MOVE ZEROS TO CONT2
      *    -------------
                   GO TO R011
      *    -------------
               ELSE
                   PERFORM RARR1I THRU RARR2I
                   IF VALI = "F"
                       MOVE "NAO FOI ENCONTRADO O SERVICO" TO MENSAGEM
                       PERFORM MSG1 THRU MSG2
                       MOVE ZEROS TO CONT2
      *    -------------
                       GO TO R011
      *    -------------
                   ELSE
      *    -------------
                       PERFORM RSAL
                       MOVE LVAL TO BCBTXT
                       DISPLAY TBCBTXT
      *    -------------
               MOVE ZEROS TO CONT2
               ADD 1 TO CONT2
               DISPLAY TELACARGO
               MOVE SPACES TO SNB
               DISPLAY TSNB
               .
      *-----------------------------------------------------------------
       R011Z.

           .
       R013.
           MOVE "R REPETIR X REMOVER A ATUALIZAR S SALVAR" TO MENSAGEM
           PERFORM MSG1 THRU MSG2
           ACCEPT TOPO
           IF OPO = "R"  OR "r"
               MOVE "REPETIR" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R02
           ELSE
               IF OPO = "X"  OR "x"
                   PERFORM RRX
                   GO TO R013
               ELSE
                   IF OPO = "A" OR "a"
                       PERFORM RRA
                       GO TO R013
                   ELSE
                       IF OPO = "S" OR "s"
                           PERFORM R012
                           GO TO R013
           .
       RF1.
           CLOSE CADCARGO
           STOP RUN.
       RF2.
           EXIT PROGRAM.
       MSG1.
           MOVE ZEROS TO CONT
           DISPLAY  TMENSAGEM
           .
       MSG2.
           IF CONT < 4444
               ADD 1 TO CONT
               GO TO MSG2
           ELSE
               MOVE SPACES TO MENSAGEM
               DISPLAY TMENSAGEM
           .
       RARR1I.
           MOVE ZEROS TO CONT2
           ADD 1 TO CONT2
           .
       RARR2I.

           IF TPSERV = "L"
               MOVE STA2A(CONT2) TO CHECKB
      *         MOVE CHECKB2 TO MENSAGEM
      *         PERFORM MSG1 THRU MSG2
      *         MOVE CHECKB2 TO MENSAGEM
      *         PERFORM MSG1 THRU MSG2
               IF CHECKB1 NOT= SERVICO
                   IF CONT2 < 3
                       ADD 1 TO CONT2
                       GO TO RARR2I
                   ELSE
                       MOVE "SERVICO NAO ENCONTRADO" TO MENSAGEM
                       MOVE "F" TO VALI
               ELSE
                   MOVE CHECKB2 TO LVAL
                   MOVE "V" TO VALI
           ELSE
               MOVE STA1A(CONT2) TO CHECKB
      *         MOVE CHECKB2 TO MENSAGEM
      *         PERFORM MSG1 THRU MSG2
               IF CHECKB1 NOT= SERVICO2
                   IF CONT2 < 4
                       ADD 1 TO CONT2
                       GO TO RARR2I
                   ELSE
                       MOVE "SERVICO NAO ENCONTRADO" TO MENSAGEM
                       MOVE "F" TO VALI
               ELSE
                   MOVE CHECKB2 TO LVAL
      *----
      *             PERFORM RSAL
                   DISPLAY TSALVAL
                   MOVE "V" TO VALI
           .

      *-----------------------------------------------------------------
       R07.
           MOVE ARRB(CONT2) TO STA1A(CONT2)
           IF CONT2 < 4
               ADD 1 TO CONT2
               GO TO R07
           ELSE
               DISPLAY TTPBEN
               MOVE "N" TO TPSERV
               MOVE TPBEN TO SERVICO2
               PERFORM RARR1I THRU RARR2I
               MOVE LVAL TO BENTXT
               DISPLAY TBENTXT
           .
       RRX.
           ACCEPT TSRK
           IF SRK = SPACES
               MOVE "CODIGO NAO PODE SER NULO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO RRX
           ELSE
               MOVE SRK TO RK
               DELETE CADCARGO RECORD
               IF FS ="00"
                   MOVE "REMOVIDO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
               ELSE
                   MOVE "ERRO AO REMOVER" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
           .
       RRA.
           ACCEPT TSRK
           IF SRK = ZEROS
               MOVE "CODIGO NAO PODE SER NULO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO RRA
           ELSE
               MOVE SRK TO RK
      *         PERFORM R02
               READ CADCARGO
               IF FS = "00"
                   DISPLAY TRK
                   DISPLAY TDESC
                   DISPLAY TSALVAL
                   DISPLAY TTS
                   DISPLAY TTSTXT

                   DISPLAY TBPS
                   DISPLAY TBPSTXT

                   DISPLAY TBCOM
                   DISPLAY TBCOMTXT

                   DISPLAY TBVA
                   DISPLAY TBVATXT

                   DISPLAY TBCB
                   DISPLAY TBCBTXT
                   MOVE ZEROS TO CONT CONT2
                   MOVE ZEROS TO TPBEN
                   PERFORM R05 THRU R011Z
                   REWRITE CARGO
                   IF FS = "00"
                       MOVE "ATUALIZADO" TO MENSAGEM
                       PERFORM MSG1 THRU MSG2
                   ELSE
                       MOVE "ERRO AO ATUALIZAR" TO MENSAGEM
                       PERFORM MSG1 THRU MSG2
               ELSE
                   MOVE "ERRO ARQUIVO NAO EXISTE" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
           .
       RSAL.
           IF CHECKB1 = 1
               COMPUTE OP1 = 400,00 - SALVAL
               MOVE OP1 TO SALVAL
           ELSE
               IF CHECKB1 = 2
                   COMPUTE OP1 = 230,00 - SALVAL
                   MOVE OP1 TO SALVAL
               ELSE
                   IF CHECKB1 = 3
                       COMPUTE OP1 = 120,00 - SALVAL
                       MOVE OP1 TO SALVAL
                   ELSE
                       IF CHECKB1 = 4
                       COMPUTE OP1 = 13,00 - SALVAL
                       MOVE OP1 TO SALVAL
           .
       R012.
           WRITE CARGO
           IF FS ="00"
               MOVE "GRAVADO" TO MENSAGEM
           ELSE
               MOVE "ERRO AO GRAVAR" TO MENSAGEM
           .
