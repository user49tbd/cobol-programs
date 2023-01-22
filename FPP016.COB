       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG01.
       AUTHOR. AUT.
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
      *-----------------------------------------------------------------
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
       WORKING-STORAGE SECTION.
           01 FS          PIC X(02).
           01 OPT         PIC X(01).
           01 MENSAGEM    PIC X(40).
           01 CONT        PIC 9(04).
           01 CONTLN      PIC 9(04).
           01 ARK         PIC 9(06).
       SCREEN SECTION.
       01  SCREENCONVAL.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "зддддддддммммммммммммммммкмдиммммммммммм".
           05  LINE 01  COLUMN 41
               VALUE  "ммммммммм╩дмкмммммммммммммммммддддддддд©".
           05  LINE 02  COLUMN 01
               VALUE  "юддддддды жрррррррррррр╥ сдд©  CONSULTAR".
           05  LINE 02  COLUMN 41
               VALUE  "_VALORES здд╫ жррррррррррррр╥ юдддддддды".
           05  LINE 03  COLUMN 01
               VALUE  " дддддддддппппппппппппп╫    юддддддддддд".
           05  LINE 03  COLUMN 41
               VALUE  "ддддддддды    сппппппппппппппдддддддддд".
           05  LINE 04  COLUMN 01
               VALUE  "  RK     MES|ANO       INSS        IMPOS".
           05  LINE 04  COLUMN 41
               VALUE  "TO_RENDA     SALARIO_F       SALARIO_L".
           05  LINE 20  COLUMN 01
               VALUE  "дддддддддддддддддддддддддддддддддддддддд".
           05  LINE 20  COLUMN 41
               VALUE  "дддддддддддддддддддддддддддддддддддддддд".
           05  LINE 21  COLUMN 01
               VALUE  " здддддддд©ммммммммммммммммммммммммммммм".
           05  LINE 21  COLUMN 41
               VALUE  "мммммммммммммммммммммммммммммммммммммммм".
           05  LINE 22  COLUMN 01
               VALUE  "з╢MENSAGEMцддддддддддддддддддддддддддддд".
           05  LINE 22  COLUMN 41
               VALUE  "дддддддддддбдддддбддддддддддддддддддддд©".
           05  LINE 23  COLUMN 01
               VALUE  "Ёюдддддддды".
           05  LINE 23  COLUMN 41
               VALUE  "           ЁOP:  ЁRK:                  Ё".
           05  LINE 24  COLUMN 01
               VALUE  "юддддддддддддддддддддддддддддддддддддддд".
           05  LINE 24  COLUMN 41
               VALUE  "дддддддддддадддддаддддддддддддддддддддды".
           05  TMENSAGEM
               LINE 23  COLUMN 12  PIC X(40)
               USING  MENSAGEM.
           05  TOPT
               LINE 23  COLUMN 56  PIC X(01)
               USING  OPT.
           05  TRK
               LINE 23  COLUMN 62  PIC 9(06)
               USING  ARK.
       PROCEDURE DIVISION.
       R00B.
           OPEN I-O PR
           IF FS NOT = "00"
               OPEN INPUT PR
               CLOSE PR
               GO TO R00B
           .

       R00.
      *-----------------------------------------------------------------
           MOVE ZEROS TO RKF RKM RKY PSALVALB PINSS PIMPR PSALF PSLIQUID

           MOVE ZEROS TO CONTLN ARK

           MOVE SPACES TO OPT
           .
       R01.
           DISPLAY SCREENCONVAL
           .
       R02.
           MOVE "DIGITE O LIMITE" TO MENSAGEM
           PERFORM MSG1 THRU MSG2
           ACCEPT TRK
           IF ARK  = ZEROS
               MOVE "RK NAO PODE SER NULA" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R02
           ELSE
               COMPUTE ARK = ARK - 1
               MOVE ARK TO RKF
               PERFORM RLIMIT THRU PL

           .
       R03.
           MOVE "DIGITE R PRA REPETIR" TO MENSAGEM
           PERFORM MSG1 THRU MSG2
           ACCEPT TOPT
           IF OPT = "R" OR "r"
               GO TO R00
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
           IF CONT < 2300
               GO TO MSG2
           MOVE SPACES TO MENSAGEM
           DISPLAY TMENSAGEM
           .
       RLIMIT.
           START PR KEY IS > RKF
           MOVE ZEROS TO LIN
           COMPUTE LIN = LIN + 4
           .
       PL.
           READ PR NEXT
               IF FS = "00"
                   IF CONTLN < 15
      *                 MOVE "NAO EXISTE" TO MENSAGEM
                       PERFORM MSG1 THRU MSG2
                       COMPUTE LIN = LIN + 1
                       PERFORM RLINE
                       COMPUTE CONTLN = CONTLN + 1
                       GO TO PL
                   ELSE
                       PERFORM R01
                       MOVE ZEROS TO LIN
                       MOVE ZEROS TO CONTLN
                       COMPUTE LIN =  LIN + 4
                       COMPUTE LIN = LIN + 1
                       PERFORM RLINE
                       COMPUTE CONTLN = CONTLN + 1
                       GO TO PL
               ELSE
                   MOVE "END" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
           .
       RLINE.
           DISPLAY(LIN, 1) RKF
           DISPLAY(LIN, 10) RKM
           DISPLAY(LIN, 13) "|"
           DISPLAY(LIN, 14) RKY
           DISPLAY(LIN, 20) PINSS
           DISPLAY(LIN, 36) PIMPR
           DISPLAY(LIN, 52) PSALF
           DISPLAY(LIN, 68) PSLIQUID
           .
