       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP0.
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
           01 FS           PIC X(02).
           01 MENSAGEM     PIC X(40).
           01 OPT          PIC X(01).
           01 ARK          PIC 9(06).
           01 CONT         PIC 9(04).
           01 CONT2        PIC 9(04).
           01 CONTERV      PIC 9(09).
           01 SCDATA.
               03 LN1A PIC X(80) VALUE
               "旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴".
               03 LN1B PIC X(80) VALUE
               "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커".
      *-----------------------------------------------------------------
               03 LN2A PIC X(80) VALUE
               "쿝K:     DESCRICAO:".
               03 LN2B PIC X(80) VALUE
               "     TIPO_SERVICO:            쿞ALARIO:?".
      *-----------------------------------------------------------------
               03 LN3A PIC X(80) VALUE
               "쿛LANO_DE_SAUDE:                     COM".
               03 LN3B PIC X(80) VALUE
               "BUSTIVEL:                     ?        ?".
      *-----------------------------------------------------------------
               03 LN4A PIC X(80) VALUE
               "쿣ALE_ALIMENTACAO:                     C".
               03 LN4B PIC X(80) VALUE
               "ESTA_B:                       ?        ?".
      *-----------------------------------------------------------------
               03 LN5A PIC X(80) VALUE
               "읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴".
               03 LN5B PIC X(80) VALUE
               "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸".
       SCREEN SECTION.
       01  SCREENCARGO.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "?箇藁袴袴袴袴袴袴袴袴袴藁袴講쩡컴컴컴컴?".
           05  LINE 01  COLUMN 41
               VALUE  "컴컴컴컴컫?箇藁袴袴袴袴袴袴袴袴袴袴箇藁?".
           05  LINE 02  COLUMN 01
               VALUE  "唐켐姦袴袴袴袴袴袴袴袴袴姦컹솎敲CONSULTA".
           05  LINE 02  COLUMN 41
               VALUE  "_CARGOS?槁횝켐姦袴袴袴袴袴袴袴袴袴菰羔戟".
           05  LINE 03  COLUMN 01
               VALUE  "                               聃컴컴컴?".
           05  LINE 03  COLUMN 41
               VALUE  "컴컴컴캭".
           05  LINE 20  COLUMN 01
               VALUE  "袴袴袴袴袴袴袴袴袴藁袴컫컴컴컴컴컴컴옵袴".
           05  LINE 20  COLUMN 41
               VALUE  "敲旼컴컴컴컴컴컫켐藁袴袴袴袴袴袴袴袴袴袴".
           05  LINE 21  COLUMN 01
               VALUE  " 旼컴컴컴커        聃컴鳩袴袴袴袴袴賈姦?".
           05  LINE 21  COLUMN 41
               VALUE  "菰鳩袴袴袴袴袴賈컴캭".
           05  LINE 22  COLUMN 01
               VALUE  "渼MENSAGEM쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴".
           05  LINE 22  COLUMN 41
               VALUE  "컴컴컴컴컴컫컴컴쩡컴컴컴컴컴컴컴컴컴컴커".
           05  LINE 23  COLUMN 01
               VALUE  "냅컴컴컴컴?".
           05  LINE 23  COLUMN 41
               VALUE  "           쿚P: 쿝K:                   ?".
           05  LINE 24  COLUMN 01
               VALUE  "읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴".
           05  LINE 24  COLUMN 41
               VALUE  "컴컴컴컴컴컨컴컴좔컴컴컴컴컴컴컴컴컴컴켸".
           05  TMENSAGEM
               LINE 23  COLUMN 12  PIC X(40)
               USING  MENSAGEM.
           05  TOPT
               LINE 23  COLUMN 56  PIC X(01)
               USING  OPT.
           05  TARK
               LINE 23  COLUMN 61  PIC 9(04)
               USING  ARK.
       PROCEDURE DIVISION.
       R00.
           MOVE ZEROS TO ARK CONT CONT2 CONTERV
           MOVE SPACES TO MENSAGEM OPT

           MOVE ZEROS TO RK BPS BCOM BVA BCB SALVAL

           MOVE SPACES TO DESC TS TSTXT BPSTXT BCOMTXT BVATXT BCBTXT
           .
       R01.
           OPEN I-O CADCARGO
           IF FS NOT = "00"
               OPEN INPUT CADCARGO
               CLOSE CADCARGO
               GO TO R01
           .
       R02.
           DISPLAY SCREENCARGO
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
           CLOSE CADCARGO
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
           DISPLAY(LIN, 20) DESC
           DISPLAY(LIN, 59) TSTXT
      *-----------------------------------------------------------------
           COMPUTE LIN = LIN + 1

           DISPLAY(LIN, 01) LN3A
           DISPLAY(LIN, 41) LN3B

           DISPLAY(LIN, 17) BPSTXT
           DISPLAY(LIN, 50) BCOMTXT
           DISPLAY(LIN, 72) SALVAL
      *-----------------------------------------------------------------
           COMPUTE LIN = LIN + 1

           DISPLAY(LIN, 01) LN4A
           DISPLAY(LIN, 41) LN4B

           DISPLAY(LIN, 19) BVATXT
           DISPLAY(LIN, 48) BCBTXT
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
           START CADCARGO KEY IS > RK
      *                     INVALID KEY GO TO FF1
           .
       R05.
           READ CADCARGO NEXT
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
