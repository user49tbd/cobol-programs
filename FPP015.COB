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
      *FUNCIONARIO VARIAVEIS
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
           01 FS          PIC X(02).
           01 OPT         PIC X(01).
           01 MENSAGEM    PIC X(40).
           01 CONT        PIC 9(04).
           01 CONTLN      PIC 9(04).
           01 ARK         PIC 9(06).
       SCREEN SECTION.
       01  SCREENSALB.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "ﾚﾄﾒﾒﾄﾄﾄﾒﾒﾒﾄﾄﾄﾒﾉﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄ".
           05  LINE 01  COLUMN 41
               VALUE  "ﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍｻﾒﾄﾄﾄﾒﾒﾒﾄﾄﾄﾒﾒﾄｿ".
           05  LINE 02  COLUMN 01
               VALUE  "ｳﾍﾊﾊﾍﾍﾍﾊﾊﾊﾍﾍﾍﾊﾊﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍ CONSULTAR_SA".
           05  LINE 02  COLUMN 41
               VALUE  "LARIO_BRUTO ﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾊﾊﾍﾍﾍﾊﾊﾊﾍﾍﾍﾊﾊﾍｳ".
           05  LINE 03  COLUMN 01
               VALUE  "ﾀﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄ".
           05  LINE 03  COLUMN 41
               VALUE  "ﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾙ".
           05  LINE 04  COLUMN 01
               VALUE  "  RK       MES | ANO".
           05  LINE 04  COLUMN 41
               VALUE  "  NOME                        SALARIO_B".
           05  LINE 20  COLUMN 01
               VALUE  "ﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄ".
           05  LINE 20  COLUMN 41
               VALUE  "ﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄ".
           05  LINE 21  COLUMN 01
               VALUE  " ﾚﾄﾄﾄﾄﾄﾄﾄﾄｿﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍ".
           05  LINE 21  COLUMN 41
               VALUE  "ﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍﾍ".
           05  LINE 22  COLUMN 01
               VALUE  "ﾚｴMENSAGEMﾃﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄ".
           05  LINE 22  COLUMN 41
               VALUE  "ﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾂﾄﾄﾄﾄﾂﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄｿ".
           05  LINE 23  COLUMN 01
               VALUE  "ｳﾀﾄﾄﾄﾄﾄﾄﾄﾄﾙ".
           05  LINE 23  COLUMN 41
               VALUE  "           ｳOP: ｳRK:                   ｳ".
           05  LINE 24  COLUMN 01
               VALUE  "ﾀﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄ".
           05  LINE 24  COLUMN 41
               VALUE  "ﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾁﾄﾄﾄﾄﾁﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾄﾙ".
           05  TMENSAGEM
               LINE 23  COLUMN 12  PIC X(40)
               USING  MENSAGEM.
           05  TOPT
               LINE 23  COLUMN 56  PIC X(01)
               USING  OPT.
           05  TRK
               LINE 23  COLUMN 61  PIC X(06)
               USING  ARK.
       PROCEDURE DIVISION.
       R00A.
           OPEN I-O FILEFUNC
           IF FS NOT = "00"
               OPEN INPUT FILEFUNC
               CLOSE FILEFUNC
               GO TO R00A
           .
       R00B.
           OPEN I-O PR
           IF FS NOT = "00"
               OPEN INPUT PR
               CLOSE PR
               GO TO R00B
           .

       R00.
           MOVE ZEROS TO RK CPF RG GENERO DNDIA DNMES DNANO NUMDEPEND
           MOVE ZEROS TO TELEFONE ARK2 CRK2 CEP2 NUM
           MOVE SPACES TO NOME SEXO SEXOTXT GENEROTXT EMAIL COMPLEMENTO
      *-----------------------------------------------------------------
           MOVE ZEROS TO RKF RKM RKY PSALVALB PINSS PIMPR PSALF PSLIQUID

           MOVE ZEROS TO CONTLN ARK RKT

           MOVE SPACES TO OPT
           .
       R01.
           DISPLAY SCREENSALB
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
                       MOVE RKF TO RKT
                       READ FILEFUNC
                           IF FS NOT = "00"
                               MOVE "NAO EXISTE" TO MENSAGEM
                               PERFORM MSG1 THRU MSG2
                               MOVE "****************************"
                               TO NOME
                               COMPUTE LIN = LIN + 1
                               PERFORM RLINE
                               COMPUTE CONTLN = CONTLN + 1
                               GO TO PL
                           ELSE
                               MOVE "EXISTE" TO MENSAGEM
                               PERFORM MSG1 THRU MSG2
                               MOVE NOME TO MENSAGEM
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
                       MOVE RKF TO RKT
                       READ FILEFUNC NEXT
                       IF FS NOT = "00"
                           MOVE "****************************" TO NOME
                           COMPUTE LIN = LIN + 1
                           PERFORM RLINE
                           COMPUTE CONTLN = CONTLN + 1
                           GO TO PL
                       ELSE
                           COMPUTE LIN = LIN + 1
                           PERFORM RLINE
                           COMPUTE CONTLN = CONTLN + 1
                           GO TO PL
               ELSE
                   MOVE ZEROS TO RKT
                   START FILEFUNC KEY IS > RKT
                   MOVE "END" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
           .
       RLINE.
           DISPLAY(LIN, 1) RKF
           DISPLAY(LIN, 13) RKM
           DISPLAY(LIN, 15) "|"
           DISPLAY(LIN, 16) RKY
           DISPLAY(LIN, 26) NOME
           DISPLAY(LIN, 68) PSALVALB
           .
