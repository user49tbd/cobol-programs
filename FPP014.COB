       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP0.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEFUNC ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS RK.
       DATA DIVISION.
       FILE SECTION.
       FD FILEFUNC
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "FUNCDATA.DAT".
       01 FILES.
           03 RK          PIC 9(06).
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
      *-----------------------------------------------------------------

       WORKING-STORAGE SECTION.
           01 FS           PIC X(02).
           01 MENSAGEM     PIC X(40).
           01 OPT          PIC X(01).
           01 ARK          PIC 9(06).
           01 CONT         PIC 9(04).
           01 CONT2        PIC 9(04).
           01 CONTERV      PIC 9(09).
           01 SCDATA.
               03 LN1A PIC X(80) VALUE
               "зддддддддддддддддддддддддддддддддддддддд".
               03 LN1B PIC X(80) VALUE
               "ддддддддддддддддддддддддддддддддддддддд©".
      *-----------------------------------------------------------------
               03 LN2A PIC X(80) VALUE
               "ЁRK:       NOME:".
               03 LN2B PIC X(80) VALUE
               "                 SEXO:                 Ё".
      *-----------------------------------------------------------------
               03 LN3A PIC X(80) VALUE
               "ЁCPF:            RG:          DATA_NASCI".
               03 LN3B PIC X(80) VALUE
               ":  |  |          GENERO:               Ё".
      *-----------------------------------------------------------------
               03 LN4A PIC X(80) VALUE
               "ЁEMAIL:".
               03 LN4B PIC X(80) VALUE
               "        TELEFONE:            NUM_D:    Ё".
      *-----------------------------------------------------------------
               03 LN5A PIC X(80) VALUE
               "юддддддддддддддддддддддддддддддддддддддд".
               03 LN5B PIC X(80) VALUE
               "ддддддддддддддддддддддддддддддддддддддды".
       SCREEN SECTION.
       01  SCREENCONFUNC.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "имдддддддддддддддддддддддддддддддддддддд".
           05  LINE 01  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддддддддддддддм╩".
           05  LINE 02  COLUMN 01
               VALUE  "хмммммммммммммммммммммммммм╦ CONSULTA_FU".
           05  LINE 02  COLUMN 41
               VALUE  "NCIONARIOS умммммммммммммммммммммммммммй".
           05  LINE 03  COLUMN 01
               VALUE  "                           тмммммммммммм".
           05  LINE 03  COLUMN 41
               VALUE  "ммммммммммм╬".
           05  LINE 20  COLUMN 01
               VALUE  "ммммммммммд© иккрррррррррррррррррррррррр".
           05  LINE 20  COLUMN 41
               VALUE  "рррррррррррррррррррррррркк╩ здмммммммммм".
           05  LINE 21  COLUMN 01
               VALUE  " здддддддд©юдппппппппппппппппппппппппппп".
           05  LINE 21  COLUMN 41
               VALUE  "пппппппппппппппппппппппппппды".
           05  LINE 22  COLUMN 01
               VALUE  "з╢MENSAGEMцддддддддддддддддддддддддддддд".
           05  LINE 22  COLUMN 41
               VALUE  "дддддддддддбддддбдддддддддддддддддддддд©".
           05  LINE 23  COLUMN 01
               VALUE  "Ёюдддддддды".
           05  LINE 23  COLUMN 41
               VALUE  "           ЁOP: ЁRK:                   Ё".
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
           05  TARK
               LINE 23  COLUMN 61  PIC 9(06)
               USING  ARK.
       PROCEDURE DIVISION.
       R00.
           MOVE ZEROS TO ARK CONT CONT2 CONTERV
           MOVE SPACES TO MENSAGEM OPT

           MOVE ZEROS TO RK CPF RG GENERO DNDIA DNMES DNANO NUMDEPEND
           MOVE ZEROS TO TELEFONE ARK2 CRK2 CEP2 NUM

           MOVE SPACES TO NOME SEXO SEXOTXT GENEROTXT EMAIL COMPLEMENTO
           .
       R01.
           OPEN I-O FILEFUNC
           IF FS NOT = "00"
               OPEN INPUT FILEFUNC
               CLOSE FILEFUNC
               GO TO R01
           .
       R02.
           DISPLAY SCREENCONFUNC
           .
       R03A.
           MOVE ZEROS TO CONT2
           MOVE ZEROS TO LIN
           COMPUTE LIN = LIN + 4
           .
       R03AB.
      *     PERFORM EXB
      *     GO TO FF1
           MOVE "DIGITE O LIMITE" TO MENSAGEM
           PERFORM MSG1 THRU MSG2
           ACCEPT TARK
           IF ARK =  ZEROS
               MOVE "VALOR DE RK NAO PODE SER NULO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R03AB
           ELSE
               COMPUTE ARK = ARK - 1
               MOVE ARK TO RK
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
           CLOSE FILEFUNC
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

           DISPLAY(LIN, 05) RK
           DISPLAY(LIN, 17) NOME
           DISPLAY(LIN, 63) SEXOTXT
      *-----------------------------------------------------------------
           COMPUTE LIN = LIN + 1

           DISPLAY(LIN, 01) LN3A
           DISPLAY(LIN, 41) LN3B

           DISPLAY(LIN, 06) CPF
           DISPLAY(LIN, 21) RG
           DISPLAY(LIN, 42) DNDIA
           DISPLAY(LIN, 45) DNMES
           DISPLAY(LIN, 48) DNANO
           DISPLAY(LIN, 65) GENEROTXT
      *-----------------------------------------------------------------
           COMPUTE LIN = LIN + 1

           DISPLAY(LIN, 01) LN4A
           DISPLAY(LIN, 41) LN4B

           DISPLAY(LIN, 08) EMAIL
           DISPLAY(LIN, 58) TELEFONE
           DISPLAY(LIN, 76) NUMDEPEND
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
           START FILEFUNC KEY IS > RK
      *                     INVALID KEY GO TO FF1
           .
       R05.
           READ FILEFUNC NEXT
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
