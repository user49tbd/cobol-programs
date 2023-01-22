       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP0.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *-----------------------------------------------------------------
           SELECT CADAREAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS RK.
      *-----------------------------------------------------------------
       DATA DIVISION.
        FILE SECTION.
      *-----------------------------------------------------------------
       FD CADAREAS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCEP1".
       01 AR.
           03 RK   PIC  9(03).
           03 DESC PIC  X(20).
           03 N    PIC  9(01).
           03 NTXT PIC  X(19).
           03 SRK  PIC  9(03).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
           01 FS   PIC X(02).
           01 MENSAGEM PIC X(30).
           01 OPT   PIC X(01).
           01 ARK PIC 9(08).
           01 ARKFS PIC 9(08).
           01 CONT PIC 9(04).
           01 CONT2 PIC 9(04).
           01 CONTERV  PIC 9(09).


      *-----------------------------------------------------------------
           01 SCDATA.
               03 LN1A PIC X(80) VALUE
               "зддддддддддддддддддддддддддддддддддддддд".
               03 LN1B PIC X(80) VALUE
               "ддддддддддддддддддддддддддддддддддддддд©".
      *-----------------------------------------------------------------
               03 LN2A PIC X(80) VALUE
               "ЁRK:     DESCRICAO:".
               03 LN2B PIC X(80) VALUE
               " NUMERO_AREA:    -                     Ё".
      *-----------------------------------------------------------------
               03 LN3A PIC X(80) VALUE
               "Ё".
               03 LN3B PIC X(80) VALUE
               "                                       Ё".
      *//---------------------------------------------------------------
               03 LN4A PIC X(80) VALUE
               "Ё".
               03 LN4B PIC X(80) VALUE
               "                                       Ё".
      *-----------------------------------------------------------------
               03 LN5A PIC X(80) VALUE
               "юддддддддддддддддддддддддддддддддддддддд".
               03 LN5B PIC X(80) VALUE
               "ддддддддддддддддддддддддддддддддддддддды".
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  CONSULTAREA.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "зббббмбмяяяя╩бяяяяяяяяяяяяякякяямддддддд".
           05  LINE 01  COLUMN 41
               VALUE  "ддддддмяякякяяяяяяяяяяяяяябияяяямбмбббб©".
           05  LINE 02  COLUMN 01
               VALUE  "юмамаааааааахоооооооооооооойойоа╢CONSULT".
           05  LINE 02  COLUMN 41
               VALUE  "A_AREAцаойойооооооооооооооо╪аааааааамамы".
           05  LINE 03  COLUMN 01
               VALUE  "                                юддддддд".
           05  LINE 03  COLUMN 41
               VALUE  "дддддды".
           05  LINE 20  COLUMN 01
               VALUE  "мдмдмдмдмдмя╩дрббимммммммммммммммммммммм".
           05  LINE 20  COLUMN 41
               VALUE  "мммммммммммммммммммммм╩ббрдиямдмдмдмдмдм".
           05  LINE 21  COLUMN 01
               VALUE  " здддддддд©юпдпаапмюдддддддддддддддддддд".
           05  LINE 21  COLUMN 41
               VALUE  "ддддддддддддддддддддым╪аапдпы".
           05  LINE 22  COLUMN 01
               VALUE  "з╢MENSAGEMцддддддддддддддддддддддддддддд".
           05  LINE 22  COLUMN 41
               VALUE  "ддддддддддддбддддбддддддддддддддддддддд©".
           05  LINE 23  COLUMN 01
               VALUE  "Ёюдддддддды".
           05  LINE 23  COLUMN 41
               VALUE  "            ЁOP: ЁRK:                  Ё".
           05  LINE 24  COLUMN 01
               VALUE  "юддддддддддддддддддддддддддддддддддддддд".
           05  LINE 24  COLUMN 41
               VALUE  "ддддддддддддаддддаддддддддддддддддддддды".
           05  TMENSAGEM
               LINE 23  COLUMN 12  PIC X(40)
               USING  MENSAGEM.
           05  TOPT
               LINE 23  COLUMN 57  PIC X(01)
               USING  OPT.
           05  TARK
               LINE 23  COLUMN 63  PIC 9(08)
               USING  ARK.
       PROCEDURE DIVISION.
       R00.
      *-----------------------------------------------------------------
           MOVE ZEROS TO ARK CONT CONT2 CONTERV LIN COL
           MOVE SPACES TO MENSAGEM OPT

           MOVE ZEROS TO RK N SRK ARKFS
           MOVE SPACES TO NTXT DESC
           .
       R01.
           OPEN I-O CADAREAS
           IF FS NOT = "00"
               OPEN INPUT CADAREAS
               CLOSE CADAREAS
               GO TO R01
           .
       R02.
           DISPLAY CONSULTAREA
           .
       R03A.
           MOVE ZEROS TO CONT2
           MOVE ZEROS TO LIN
           COMPUTE LIN = LIN + 4
      *     PERFORM EXB
      *     GO TO FF1
           .
       R03AB.
           MOVE "DIGITE O LIMITE" TO MENSAGEM
           PERFORM MSG1 THRU MSG2
           ACCEPT TARK
           IF ARK =  ZEROS
               MOVE "RK NAO PODE SER NULO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R03AB
           ELSE
               COMPUTE ARK = ARK - 1
               MOVE ARK TO RK
               PERFORM R04 THRU R05
               MOVE CONTERV TO MENSAGEM
               PERFORM MSG1 THRU MSG2
      *         GO TO FF1
               IF CONTERV = ZEROS
                   MOVE "NENHUM VALOR ENCONTRADO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
                   GO TO R03A
               ELSE
                   MOVE ZEROS TO CONTERV

           .
       R06.
           MOVE "DIGIE A OPERACAO R PARA REPETIR" TO MENSAGEM
           PERFORM MSG1 THRU MSG2
           ACCEPT TOPT
           IF OPT = "R" OR "r"
               PERFORM R00
               GO TO R02
           PERFORM MSG1 THRU MSG2
           .
       FF1.
           CLOSE CADAREAS
           STOP RUN
           .
       FF2.
           EXIT PROGRAM
           .
       EXB.
           DISPLAY(LIN, 01) LN1A
           DISPLAY(LIN, 41) LN1B


           COMPUTE LIN = LIN + 1

           DISPLAY(LIN, 01) LN2A
           DISPLAY(LIN, 41) LN2B

           DISPLAY(LIN, 06) RK
           DISPLAY(LIN, 21) DESC
           DISPLAY(LIN, 56) N
           DISPLAY(LIN, 60) NTXT
      *-----------------------------------------------------------------
           COMPUTE LIN = LIN + 1

           DISPLAY(LIN, 01) LN3A
           DISPLAY(LIN, 41) LN3B

      *-----------------------------------------------------------------
           COMPUTE LIN = LIN + 1

           DISPLAY(LIN, 01) LN4A
           DISPLAY(LIN, 41) LN4B
           PERFORM R0P1 THRU R0P2

      *-----------------------------------------------------------------
           COMPUTE LIN = LIN + 1
           DISPLAY(LIN, 01) LN5A
           DISPLAY(LIN, 41) LN5B
           COMPUTE LIN = LIN + 1
           .
       MSG1.
           MOVE ZEROS TO CONT
           DISPLAY TMENSAGEM
           .
       MSG2.
           IF CONT < 2500
               ADD 1 TO CONT
               GO TO MSG2
           ELSE
               MOVE SPACES TO MENSAGEM
               DISPLAY TMENSAGEM
               MOVE ZEROS TO CONT
           .
       R03B.
           PERFORM MSG2
           IF CONT2 < 3
               ADD 1 TO CONT2
               PERFORM EXB
               GO TO R03B
           .
      *-----------------------------------------------------------------
       R04.
           START CADAREAS KEY IS > RK
      *                     INVALID KEY GO TO FF1
           .
       R05.
           READ CADAREAS NEXT
           IF FS = "00"
               ADD 1 TO CONTERV
               IF CONT2 < 3
                   ADD 1 TO CONT2
                   PERFORM EXB
                   PERFORM MSG2
                   GO TO R05
               ELSE
                   PERFORM R0R
                   PERFORM R02
                   MOVE ZEROS TO CONT2
                   MOVE ZEROS TO LIN
                   COMPUTE LIN = LIN + 4
                   ADD 1 TO CONT2
                   PERFORM EXB
                   PERFORM MSG2
                   GO TO R05

      *         GO TO FF1
           .
       R0R.
           MOVE "DIGITE PARA CONTINUAR" TO MENSAGEM
           PERFORM MSG1 THRU MSG2
           ACCEPT TOPT
           .
       R0P1.
           MOVE RK TO ARKFS
           MOVE ZEROS TO COL
           COMPUTE COL = COL + 2
           .
       R0P2.
           READ CADAREAS
           IF FS = "00"
               DISPLAY(LIN, COL) SRK
               COMPUTE COL = COL + 4
               MOVE SRK TO RK
               GO TO R0P2
           ELSE
               MOVE ARKFS TO RK
               PERFORM R04
           .
