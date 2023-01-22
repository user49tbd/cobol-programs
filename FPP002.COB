       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP002.
       AUTHOR. AUT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADAREAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS RK.

       DATA DIVISION.
       FILE SECTION.
       FD CADAREAS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCEP1".
       01 AR.
           03 RK   PIC  9(03).
           03 DESC PIC  X(20).
           03 N    PIC  9(01).
           03 NTXT PIC  X(19).
           03 SRK  PIC  9(03).
       WORKING-STORAGE SECTION.
           01 UTKEY PIC 9(03).
           01 RKX   PIC 9(03).
           01 FS   PIC  X(02).
           01 ARRN1.
               03 ARRN2 PIC  9(03) OCCURS 9 TIMES.
           01 ARRN4.
               03 ARRN3 PIC  9(03) OCCURS 9 TIMES.
           01 MENSAGEM PIC X(43).
           01 OP   PIC  X(01).
           01 RKOP PIC  9(03).
           01 CONT PIC  9(04).
           01 CONT2 PIC  9(04).
           01 VALI PIC  X(01) VALUE "F".
           01 ANTN PIC  9(01).
           01 RES  PIC  9(02).
           01 CT   PIC  9(02).
           01 ARREYNIVEL.
               03  FILLER PIC X(20) VALUE "1 PRESIDENCIA".
               03  FILLER PIC X(20) VALUE "2 DIRETORIA".
               03  FILLER PIC X(20) VALUE "3 GERENCIA".
               03  FILLER PIC X(20) VALUE "4 SUPERVISAO".
               03  FILLER PIC X(20) VALUE "5 COORDENACAO".
               03  FILLER PIC X(20) VALUE "6 OPERACIONAL ADM".
               03  FILLER PIC X(20) VALUE "7 OPERACIONAL CONL".
               03  FILLER PIC X(20) VALUE "8 OPERACIONAL INDL".
               03  FILLER PIC X(20) VALUE "9 OUTROS".
           01 ARREYNIVEL2 REDEFINES ARREYNIVEL.
               03  ARR PIC X(20) OCCURS 9 TIMES.

           01 GETARREY.
               03 GETARREY1 PIC X(01).
      *-----------
               03 GETARREYR REDEFINES GETARREY1.
                   05 TFVAL PIC 9(01).

               03 GETARREY2 PIC X(19).
       SCREEN SECTION.
       01  TELAREAS1.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "зддддддддддддддддддддддддддддддддддддддд".
           05  LINE 01  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддддддддддддддд©".
           05  LINE 02  COLUMN 01
               VALUE  "Ёимммммммммммкмм╩ имммммммммммммм╩ ддддд".
           05  LINE 02  COLUMN 41
               VALUE  "ддддд имммммммммммммм╩ иммкммммммммммм╩Ё".
           05  LINE 03  COLUMN 01
               VALUE  "Ё╨иммммм╩    ╨  ╨ ╨              хмммммм".
           05  LINE 03  COLUMN 41
               VALUE  "мммммм╪              ╨ ╨  ╨    иммммм╩╨Ё".
           05  LINE 04  COLUMN 01
               VALUE  "Ё╨хмммм лмммм╪  ╨ х╩            зд PROGR".
           05  LINE 04  COLUMN 41
               VALUE  "AM001 д©            и╪ ╨  хмммм╧ мммм╪╨Ё".
           05  LINE 05  COLUMN 01
               VALUE  "Ёлмммм╩ хммммм╩ х╩ хммммммямммм юд".
           05  LINE 05  COLUMN 41
               VALUE  "       ы ммммямммммм╪ и╪ иммммм╪ имммм╧Ё".
           05  LINE 06  COLUMN 01
               VALUE  "Ё╨    хмммммммй╩ хммммммм юддддддддддддд".
           05  LINE 06  COLUMN 41
               VALUE  "ддддддддддддды ммммммм╪ ийммммммм╪    ╨Ё".
           05  LINE 07  COLUMN 01
               VALUE  "Ё╨             хмммммммммммммддддддддддд".
           05  LINE 07  COLUMN 41
               VALUE  "ддддддддддмммммммммммммм╪             ╨Ё".
           05  LINE 08  COLUMN 01
               VALUE  "Ё╨ здддддддддддддддддддддддддддддддддддд".
           05  LINE 08  COLUMN 41
               VALUE  "дддддддддддддддддддддддддддддддддддд© ╨Ё".
           05  LINE 09  COLUMN 01
               VALUE  "Ё╨ Ё здддддддбддд© зддддддддддбддддддддд".
           05  LINE 09  COLUMN 41
               VALUE  "ддддддддддд©                        Ё ╨Ё".
           05  LINE 10  COLUMN 01
               VALUE  "Ё╨ Ё ЁCODIGO:Ё   Ё ЁDESCRICAO:Ё".
           05  LINE 10  COLUMN 41
               VALUE  "           Ё                        Ё ╨Ё".
           05  LINE 11  COLUMN 01
               VALUE  "Ё╨ Ё юдддддддаддды юддддддддддаддддддддд".
           05  LINE 11  COLUMN 41
               VALUE  "ддддддддддды                        Ё ╨Ё".
           05  LINE 12  COLUMN 01
               VALUE  "Ё╨ Ё зддддддбдбдддддддддддддддддддд©зддд".
           05  LINE 12  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддддддддддд©Ё ╨Ё".
           05  LINE 13  COLUMN 01
               VALUE  "Ё╨ Ё ЁNIVEL:Ё Ё                    ЁЁ".
           05  LINE 13  COLUMN 41
               VALUE  "                                   ЁЁ ╨Ё".
           05  LINE 14  COLUMN 01
               VALUE  "Ё╨ Ё юддддддададдддддддддддддддддддыЁ".
           05  LINE 14  COLUMN 41
               VALUE  "                                   ЁЁ ╨Ё".
           05  LINE 15  COLUMN 01
               VALUE  "Ё╨ Ё здддддддддддддддддддддддддбддд©Ё".
           05  LINE 15  COLUMN 41
               VALUE  "                                   ЁЁ ╨Ё".
           05  LINE 16  COLUMN 01
               VALUE  "Ё╨ Ё ЁCODIGO AREA SUBORDINADA: Ё   ЁЁ".
           05  LINE 16  COLUMN 41
               VALUE  "                                   ЁЁ ╨Ё".
           05  LINE 17  COLUMN 01
               VALUE  "Ё╨ Ё юдддддддддддддддддддддддддадддыЁ".
           05  LINE 17  COLUMN 41
               VALUE  "                                   ЁЁ ╨Ё".
           05  LINE 18  COLUMN 01
               VALUE  "Ё╨ юддддддддддд©                    юддд".
           05  LINE 18  COLUMN 41
               VALUE  "дддддддддддддддддддддддддддддддддддыЁ ╨Ё".
           05  LINE 19  COLUMN 01
               VALUE  "Ё╨  зддддддддд©юдддддддддддддддддддддддд".
           05  LINE 19  COLUMN 41
               VALUE  "дддддддддддддддддддддддддддддддддддды ╨Ё".
           05  LINE 20  COLUMN 01
               VALUE  "Ё╨ з╢MENSAGEM цддддддддддддддддддддддддд".
           05  LINE 20  COLUMN 41
               VALUE  "ддддддддддддддддддбддддддддддддддддд© ╨Ё".
           05  LINE 21  COLUMN 01
               VALUE  "Ё╨ Ёюддддддддды".
           05  LINE 21  COLUMN 41
               VALUE  "                  ЁOP:      RK:     Ё ╨Ё".
           05  LINE 22  COLUMN 01
               VALUE  "Ё╨ юдддддддддддддддддддддддддддддддддддд".
           05  LINE 22  COLUMN 41
               VALUE  "ддддддддддддддддддаддддддддддддддддды ╨Ё".
           05  LINE 23  COLUMN 01
               VALUE  "Ёхмммммммммммммммммммммммммммммммммммммм".
           05  LINE 23  COLUMN 41
               VALUE  "мммммммммммммммммммммммммммммммммммммм╪Ё".
           05  LINE 24  COLUMN 01
               VALUE  "юддддддддддддддддддддддддддддддддддддддд".
           05  LINE 24  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддддддддддддддды".
           05  TRK
               LINE 10  COLUMN 15  PIC 9(03)
               USING  RK.
           05  TDESC
               LINE 10  COLUMN 32  PIC X(20)
               USING  DESC.
           05  TN
               LINE 13  COLUMN 14  PIC 9(01)
               USING  N.
           05  TNTXT
               LINE 13  COLUMN 16  PIC X(20)
               USING  NTXT.
           05  TSRK
               LINE 16  COLUMN 33  PIC 9(03)
               USING  SRK.
           05  TMENSAGEM
               LINE 21  COLUMN 16  PIC X(43)
               USING  MENSAGEM.
           05  TTOP
               LINE 21  COLUMN 64  PIC X(01)
               USING  OP.
           05  TRKOP
               LINE 21  COLUMN 72  PIC 9(03)
               USING  RKOP.
       01  CARGOTAB.
           05  LINE 13  COLUMN 39
               VALUE  "1".
           05  LINE 13  COLUMN 41
               VALUE  "PRESIDENCIA     6 OPERACIONAL ADM".
           05  LINE 14  COLUMN 39
               VALUE  "2".
           05  LINE 14  COLUMN 41
               VALUE  "DIRETORIA       7 OPERACIONAL CONL".
           05  LINE 15  COLUMN 39
               VALUE  "3".
           05  LINE 15  COLUMN 41
               VALUE  "GERENCIA        8 OPERACIONAL INDL".
           05  LINE 16  COLUMN 39
               VALUE  "4".
           05  LINE 16  COLUMN 41
               VALUE  "SUPERVISAO      9 OUTROS".
           05  LINE 17  COLUMN 39
               VALUE  "5".
           05  LINE 17  COLUMN 41
               VALUE  "COORDENACAO".

       PROCEDURE DIVISION.
       R00.
           OPEN I-O CADAREAS
           IF FS NOT = "00"
               OPEN OUTPUT CADAREAS
               CLOSE CADAREAS
               GO TO R00
       .
       R01.
           MOVE ZEROS TO RK SRK N RKOP CONT RES
           MOVE SPACES TO FS NTXT DESC OP MENSAGEM
           DISPLAY TELAREAS1
           .
       R02.

           ACCEPT TRK
           IF RK = ZEROS
               MOVE "CODIGO NцO  PODE SER ZERO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R02
           .
       R03.
           READ CADAREAS
           IF FS = "00"
               DISPLAY TRK
               DISPLAY TDESC
               DISPLAY TN
               DISPLAY TNTXT
               DISPLAY TSRK
               GO TO ROP
           .
       R04.
           ACCEPT TDESC
           IF DESC = ZEROS
               MOVE "DESCRICAO NцO  PODE SER NULO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R04
           ELSE
               DISPLAY CARGOTAB
           .
       R05.
           ACCEPT TN
           IF N = SPACES
               MOVE "NUMERO NцO  PODE SER ZERO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R05
           ELSE
               PERFORM RTAB1 THRU RTAB2
               IF VALI = "F"
                   GO TO R05
           .
       R06.
           ACCEPT TSRK
           IF SRK = ZEROS
               MOVE "CODIGO NцO  PODE SER ZERO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R06
           .

       R0S.
           WRITE AR
           IF FS = "00"
               MOVE "GRAVADO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
           ELSE
               MOVE "NAO GRAVADO" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
           .
       ROP.
           MOVE "X EXCLUR A ATUALIZAR R REPETIR L LISTAR" TO MENSAGEM
           PERFORM MSG1 THRU MSG2
           ACCEPT TTOP
           IF OP = "R"
               MOVE "REPETIR" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO R01
           ELSE
               IF OP = "A"
                   PERFORM R04 THRU R06
                   PERFORM RAT
                   GO TO ROP
               ELSE
                   IF OP = "X"
                       PERFORM REX
                       GO TO ROP
                   ELSE
                       IF OP = "L"
      *-/-/-/-/-/-/
                           PERFORM R01LOOP THRU R03LOOP
                           PERFORM R03EXB1 THRU R03EXB
                           GO TO ROP


           .
       RF1.
           STOP RUN
           .
       RF2.
           EXIT PROGRAM
           .
       MSG1.
           DISPLAY TMENSAGEM
           MOVE ZEROS TO CONT
           .
       MSG2.
           IF CONT < 4444
               ADD 1 TO CONT
               GO TO MSG2
           ELSE
               MOVE SPACES TO MENSAGEM
               DISPLAY TMENSAGEM
           .
      *-------------------------------------
       R01LOOP.
           MOVE ZEROS TO CT
           PERFORM VETINI THRU VETINI2
           MOVE ZEROS TO CONT2
           ADD 1 TO CONT2
           .
       R03LOOP.
           READ CADAREAS
           IF FS = "00"
               IF CONT2 < 10
                   PERFORM RCONF
      *            IF W-SEL
      *             MOVE N TO ANTN
                   MOVE RK TO ARRN2(CONT2)
                   MOVE N TO ARRN3(CONT2)
                   ADD 1 TO CONT2
                   ADD 1 TO CT
                   MOVE SRK TO RK

                   GO TO R03LOOP
               ELSE
                   MOVE "FINALIZADO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
           .
       RCONF.
           READ CADAREAS
           IF FS = "00"
               MOVE "CHAVE CADASTRADA" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               COMPUTE RES = CONT2 - 1
               MOVE RES TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               IF RES NOT = ZEROS
      *             MOVE "HORAAAAAA" TO MENSAGEM
      *             PERFORM MSG1 THRU MSG2

      *             MOVE RK TO UTKEY
      *             MOVE UTKEY TO MENSAGEM
      *             PERFORM MSG1 THRU MSG2

      *             MOVE ARRN2(RES) TO RK
                   MOVE ARRN3(RES) TO ANTN

      *             MOVE RK TO MENSAGEM
      *             PERFORM MSG1 THRU MSG2

                   MOVE "HORAAAAAA-------" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2

      *             MOVE N TO ANTN

                   MOVE N TO MENSAGEM
                   PERFORM MSG1 THRU MSG2

      *             MOVE UTKEY TO RK

                   MOVE N TO MENSAGEM
                   PERFORM MSG1 THRU MSG2


                   IF N < ANTN
                       MOVE "NIVEL MENOR NAO PODE SER SUREIOR A MAIOR"
                       TO MENSAGEM
                       PERFORM MSG1 THRU MSG2
                       MOVE RK TO UTKEY
      *             MOVE UTKEY TO MENSAGEM
      *             PERFORM MSG1 THRU MSG2

                       MOVE ARRN2(RES) TO RK
                       READ CADAREAS
                       IF FS = "00"
                           DISPLAY TRK
                           DISPLAY TDESC
                           DISPLAY TN
                           DISPLAY TNTXT
                           DISPLAY TSRK
                       PERFORM R06
                       MOVE "----------------ok-------------------"
                       TO MENSAGEM
                       PERFORM MSG1 THRU MSG2

      *                 PERFORM RAT
                       REWRITE AR
                       IF FS = "00"
                           MOVE "ATUALIZADO" TO MENSAGEM
                           PERFORM MSG1 THRU MSG2
                       ELSE
                           MOVE "NAO FOI POSSIVEL ATUALIZAR" TO MENSAGEM
                           PERFORM MSG1 THRU MSG2
                       MOVE UTKEY TO RK
      *                 MOVE SRK TO RK
                       GO TO RCONF
      *             ELSE
      *                 MOVE RK TO ARRN2(CONT2)
      *                 ADD 1 TO CONT2
      *                 MOVE SRK TO RK
      *                 READ CADAREAS


           ELSE
               MOVE "CHAVE NAO CADASTRADA" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               EXIT
           .
       R03EXB1.
           MOVE ZEROS TO CONT2
           ADD 1 TO CONT2
           ADD 1 TO CT
           .
       R03EXB.

           IF CONT2 < 9
               IF ARRN2(CONT2) NOT= ZEROS
                  MOVE ARRN2(CONT2) TO MENSAGEM
                  PERFORM MSG1 THRU MSG2
                  MOVE ARRN2(CONT2) TO RK
                  READ CADAREAS
                       IF FS = "00"
                           DISPLAY TRK
                           DISPLAY TDESC
                           DISPLAY TN
                           DISPLAY TNTXT
                           DISPLAY TSRK

                  ADD 1 TO CONT2
                  GO TO R03EXB
                ELSE
                    EXIT
           .
       VETINI.
           MOVE ZEROS TO CONT
           ADD 1 TO CONT
           .
       VETINI2.
           IF CONT < 9
               MOVE ZEROS TO ARRN2(CONT)
               ADD 1 TO CONT
               GO TO VETINI2
           .
       RTAB1.
           MOVE ZEROS TO CONT
           ADD 1 TO CONT
           .
       RTAB2.
           MOVE ARR(CONT) TO GETARREY
           IF GETARREYR NOT = N
               IF CONT < 9
                   ADD 1 TO CONT
                   GO TO RTAB2
               ELSE
                   MOVE "F" TO VALI
                   MOVE "VALOR NAO ENCONTRADO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
           ELSE
               MOVE GETARREY2 TO NTXT
               DISPLAY TNTXT
               MOVE "V" TO VALI
           .
       RAT.
      *     MOVE "-----REWRITE-----" TO MENSAGEM
      *     PERFORM MSG1 THRU MSG2
           REWRITE AR
           IF FS = "00"
               MOVE "ATUALIZADO" TO MENSAGEM
           ELSE
               MOVE "NAO FOI POSSIVEL ATUALIZAR" TO MENSAGEM
           PERFORM MSG1 THRU MSG2
           .
       REX.
           ACCEPT TRKOP
           IF RKOP = ZEROS
               MOVE "RK NAO PODE SER NULA" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO REX
           ELSE
               MOVE RKOP TO RK
               DELETE CADAREAS RECORD
               IF FS = "00"
                   MOVE "DELETADO" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
               ELSE
                   MOVE "NAO FOI POSSIVEL EXCLUIR" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
           .
       RSK.
           ACCEPT TRKOP
           MOVE SPACES TO UTKEY
           IF RKOP = ZEROS
               MOVE "RK NAO PODE SER NULA" TO MENSAGEM
               PERFORM MSG1 THRU MSG2
               GO TO RSK
           ELSE
               MOVE RK TO UTKEY
               MOVE RKOP TO RK
               READ CADAREAS
               IF FS = "00"
                   DISPLAY TRK
                   DISPLAY TDESC
                   DISPLAY TN
                   DISPLAY TNTXT
                   DISPLAY TSRK
               ELSE
                   MOVE UTKEY TO RK
                   MOVE "RK NAO EXISTE" TO MENSAGEM
                   PERFORM MSG1 THRU MSG2
           .
