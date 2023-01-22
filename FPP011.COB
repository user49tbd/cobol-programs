       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP0.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRO ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS CEP.
       DATA DIVISION.
        FILE SECTION.
       FD PRO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "PRO1.DAT".
       01 PROT.
           03 UF   PIC X(02).
           03 UFN  PIC X(23).
      *-----------------------------------------------------------------
           03 CEP  PIC 9(08).
           03 LOGRADOURO PIC X(40).
           03 BAIRRO PIC X(20).
           03 CIDADE PIC X(20).
           03 VALIT PIC 9(02).
       WORKING-STORAGE SECTION.
           01 FS   PIC X(02).
           01 MENSAGEM PIC X(30).
           01 OPT   PIC X(01).
           01 ACEP PIC 9(08).
           01 CONT PIC 9(04).
           01 CONT2 PIC 9(04).
           01 CONTERV  PIC 9(09).
           01 SCDATA.
               03 LN1A PIC X(80) VALUE
               "зддддддддддддддддддддддддддддддддддддддд".
               03 LN1B PIC X(80) VALUE
               "ддддддддддддддддддддддддддддддддддддддд©".
      *-----------------------------------------------------------------
               03 LN2A PIC X(80) VALUE
               "ЁCEP:         LOGRADOURO:".
               03 LN2B PIC X(80) VALUE
               "                                       Ё".
      *-----------------------------------------------------------------
               03 LN3A PIC X(80) VALUE
               "ЁBAIRRO:                     CIDADE:".
               03 LN3B PIC X(80) VALUE
               "                                       Ё".
      *-----------------------------------------------------------------
               03 LN4A PIC X(80) VALUE
               "ЁUF:   -".
               03 LN4B PIC X(80) VALUE
               "                                       Ё".
      *-----------------------------------------------------------------
               03 LN5A PIC X(80) VALUE
               "юддддддддддддддддддддддддддддддддддддддд".
               03 LN5B PIC X(80) VALUE
               "ддддддддддддддддддддддддддддддддддддддды".
       SCREEN SECTION.
       01  CONSULTCEP.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "жрррррррррррррррррр╥имммммдрдддддддддддд".
           05  LINE 01  COLUMN 41
               VALUE  "дддддддддддрдммммм╩жррррррррррррррррррр╥".
           05  LINE 02  COLUMN 01
               VALUE  "хййййййййййййййййййййммммм хмддд©CONSULT".
           05  LINE 02  COLUMN 41
               VALUE  "A  CEPздддм╪ мммммййййййййййййййййййййй╪".
           05  LINE 03  COLUMN 01
               VALUE  "                                тммммммм".
           05  LINE 03  COLUMN 41
               VALUE  "мммммм╬".
           05  LINE 20  COLUMN 01
               VALUE  "сппппппппппппппппппппппппппппппппппппппп".
           05  LINE 20  COLUMN 41
               VALUE  "ппппппппппппппппппппппппппппппппппппппп╫".
           05  LINE 21  COLUMN 01
               VALUE  " здддддддд©".
           05  LINE 22  COLUMN 01
               VALUE  "з╢MENSAGEMцддддддддддддддддддддддддддддд".
           05  LINE 22  COLUMN 41
               VALUE  "дддддддддддбддддбдддддддддддддддддддддд©".
           05  LINE 23  COLUMN 01
               VALUE  "Ёюдддддддды".
           05  LINE 23  COLUMN 41
               VALUE  "           ЁOP: ЁCEP:                  Ё".
           05  LINE 24  COLUMN 01
               VALUE  "юддддддддддддддддддддддддддддддддддддддд".
           05  LINE 24  COLUMN 41
               VALUE  "дддддддддддаддддадддддддддддддддддддддды".
           05  TMENSAGEM
               LINE 23  COLUMN 12  PIC X(40)
               USING  MENSAGEM.
           05  TOPT
               LINE 23  COLUMN 56  PIC X(01)
               USING  OPT.
           05  TACEP
               LINE 23  COLUMN 62  PIC 9(08)
               USING  ACEP.
       PROCEDURE DIVISION.
       R00.
           MOVE ZEROS TO CEP VALIT ACEP CONT CONT2 CONTERV
           MOVE SPACES TO UF UFN LOGRADOURO BAIRRO CIDADE MENSAGEM OPT
           .
       R01.
           OPEN I-O PRO
           IF FS NOT = "00"
               OPEN INPUT PRO
               CLOSE PRO
               GO TO R01
           .
       R02.
           DISPLAY CONSULTCEP
           .
       R03A.
           MOVE ZEROS TO CONT2
           MOVE ZEROS TO LIN
           COMPUTE LIN = LIN + 4
           .
       R03AB.
           MOVE "DIGITE O LIMITE" TO MENSAGEM
           PERFORM MSG1 THRU MSG2
           ACCEPT TACEP
           IF ACEP =  ZEROS
               MOVE "CEP NAO PODE SER NULO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R03AB
           ELSE
               COMPUTE ACEP = ACEP - 1
               MOVE ACEP TO CEP
               PERFORM R04 THRU R05
               MOVE CONTERV TO MENSAGEM
               PERFORM MSG1 THRU MSG2
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
           CLOSE PRO
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

           DISPLAY(LIN, 06) CEP
           DISPLAY(LIN, 26) LOGRADOURO
      *-----------------------------------------------------------------
           COMPUTE LIN = LIN + 1

           DISPLAY(LIN, 01) LN3A
           DISPLAY(LIN, 41) LN3B

           DISPLAY(LIN, 09) BAIRRO
           DISPLAY(LIN, 37) CIDADE
      *-----------------------------------------------------------------
           COMPUTE LIN = LIN + 1

           DISPLAY(LIN, 01) LN4A
           DISPLAY(LIN, 41) LN4B

           DISPLAY(LIN, 06) UF
           DISPLAY(LIN, 11) UFN

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
           START PRO KEY IS > CEP
      *                     INVALID KEY GO TO FF1
           .
       R05.
           READ PRO NEXT
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
