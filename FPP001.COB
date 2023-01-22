       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRO.
       AUTHOR. JONATHAN MOURA ANDRADE.
       ENVIRONMENT DIVISION.
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
       01 IND  PIC 9(02).
       01 VAL  PIC X(01).
       01 MENSAGEM PIC X(30).
       01 TM   PIC 9(04).
       01 FG   PIC X(4) VALUE "tan".
       01 VL   PIC X(01).
       01 WFUNC.
           05 UPCA PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 LOWCA PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
       01 TBUF.
           03 FILLER   PIC X(23) VALUE "SP SAO PAULO".
           03 FILLER   PIC X(23) VALUE "RO RONDONIA".
           03 FILLER   PIC X(23) VALUE "AC ACRE".
           03 FILLER   PIC X(23) VALUE "AM AMAZONAS".
           03 FILLER   PIC X(23) VALUE "RR RORAIMA".
           03 FILLER   PIC X(23) VALUE "PA PARA".
           03 FILLER   PIC X(23) VALUE "AM AMAPA".
           03 FILLER   PIC X(23) VALUE "TO TOCANTINS".
           03 FILLER   PIC X(23) VALUE "MA MARANHAO".
           03 FILLER   PIC X(23) VALUE "PI PIAUI".
           03 FILLER   PIC X(23) VALUE "CE CEARA".
           03 FILLER   PIC X(23) VALUE "RN RIO GRANDE DO NORTE".
           03 FILLER   PIC X(23) VALUE "PB PARAIBA".
           03 FILLER   PIC X(23) VALUE "PE PERNAMBUCO".
           03 FILLER   PIC X(23) VALUE "AL ALAGOAS".
           03 FILLER   PIC X(23) VALUE "SE SERGIPE".
           03 FILLER   PIC X(23) VALUE "BA BAHIA".
           03 FILLER   PIC X(23) VALUE "MG MINASGERAIS".
           03 FILLER   PIC X(23) VALUE "ES ESPIRITO SANTO".
           03 FILLER   PIC X(23) VALUE "RJ RIO DE JANEIRO".
           03 FILLER   PIC X(23) VALUE "PR PARANA".
           03 FILLER   PIC X(23) VALUE "SC SANTA CATARINA".
           03 FILLER   PIC X(23) VALUE "RS RIO GRANDE DO SUL".
           03 FILLER   PIC X(23) VALUE "MS MATO GROSSO DO SUL".
           03 FILLER   PIC X(23) VALUE "GO GOIAS".
           03 FILLER   PIC X(23) VALUE "DF DISTRITO FEDERAL".
       01 TBUFV REDEFINES TBUF.
           03 TBUFV1   PIC X(23) OCCURS 27 TIMES.
       01 TBUFR.
           03 TBUFR1  PIC X(02).
           03 TBUFR2   PIC X(23).
       SCREEN SECTION.
       01  CEPTELA.
           05  LINE 01  COLUMN 01
               VALUE  "здддддддддддддддддддддддддддддддддд©ENDE".
           05  LINE 01  COLUMN 41
               VALUE  "RECOздддддддддддддддддддддддддддддддддд©".
           05  LINE 02  COLUMN 01
               VALUE  "Ёиммммкммммммммммммммммммммммммммммюдддд".
           05  LINE 02  COLUMN 41
               VALUE  "ддддыммммммммммммммммммммммммммммкмммм╩Ё".
           05  LINE 03  COLUMN 01
               VALUE  "Ёлмм╩ ╨збдддббдддддддд©здддддддддддддддд".
           05  LINE 03  COLUMN 41
               VALUE  "дддд© здддддддддддддддддддд©     ╨ имм╧Ё".
           05  LINE 04  COLUMN 01
               VALUE  "Ё╨  ╨ ╨ЁЁCEPЁЁ        ЁЁ".
           05  LINE 04  COLUMN 41
               VALUE  "    Ё Ё                    Ё     ╨ ╨  ╨Ё".
           05  LINE 05  COLUMN 01
               VALUE  "Ё╨  ╨ ╨юадддааддддддддыЁздддддд©зддддддд".
           05  LINE 05  COLUMN 41
               VALUE  "дддды Ёздддддд©зддддддддддды     ╨ ╨  ╨Ё".
           05  LINE 06  COLUMN 01
               VALUE  "Ё╨  ╨ ╨  здддддддддд©  ю╢BAIRROцы".
           05  LINE 06  COLUMN 41
               VALUE  "      ю╢CIDADEцы                 ╨ ╨  ╨Ё".
           05  LINE 07  COLUMN 01
               VALUE  "Ё╨ и╪ ╨ з╢LOGRADOUROц©  юдддддды".
           05  LINE 07  COLUMN 41
               VALUE  "       юдддддды                  ╨ х╩ ╨Ё".
           05  LINE 08  COLUMN 01
               VALUE  "Ё╨ ╨ и╪ Ёюддддддддддыюдддддддддддддддддд".
           05  LINE 08  COLUMN 41
               VALUE  "ддддддддд©                       х╩ ╨ ╨Ё".
           05  LINE 09  COLUMN 01
               VALUE  "Ё╨ ╨ ╨  Ё".
           05  LINE 09  COLUMN 41
               VALUE  "         Ё                        ╨ ╨ ╨Ё".
           05  LINE 10  COLUMN 01
               VALUE  "Ё╨ лм╪  юддддддддддддддддддддддддддддддд".
           05  LINE 10  COLUMN 41
               VALUE  "ддддддддды                        хм╧ ╨Ё".
           05  LINE 11  COLUMN 01
               VALUE  "Ё╨ ╨ збдддддддббдддддддддддддддддддддддд".
           05  LINE 11  COLUMN 41
               VALUE  "дддддддддддддддддддддддддддддддддд© ╨ ╨Ё".
           05  LINE 12  COLUMN 01
               VALUE  "Ё╨ ╨ ЁЁ  U.F  Ёцддддбддддбддддбддддбдддд".
           05  LINE 12  COLUMN 41
               VALUE  "дддддддддддддддддддддддддддддддддд╢ ╨ ╨Ё".
           05  LINE 13  COLUMN 01
               VALUE  "Ё╨ ╨ цадддбддда╢ MA Ё PE Ё ES Ё MS Ё здд".
           05  LINE 13  COLUMN 41
               VALUE  "дддддддддддддддддддддддддддддддд© Ё ╨ ╨Ё".
           05  LINE 14  COLUMN 01
               VALUE  "Ё╨ ╨ Ё SP Ё RR Ё PI Ё AL Ё RJ Ё GO Ё ЁUF".
           05  LINE 14  COLUMN 41
               VALUE  ":                               Ё Ё ╨ ╨Ё".
           05  LINE 15  COLUMN 01
               VALUE  "Ё╨ ╨ Ё RO Ё PA Ё CE Ё SE Ё PR Ё DF Ё Ё".
           05  LINE 15  COLUMN 41
               VALUE  "                                Ё Ё ╨ ╨Ё".
           05  LINE 16  COLUMN 01
               VALUE  "Ё╨ ╨ Ё AC Ё AM Ё RN Ё BA Ё SC цдддды Ё".
           05  LINE 16  COLUMN 41
               VALUE  "                                Ё Ё ╨ ╨Ё".
           05  LINE 17  COLUMN 01
               VALUE  "Ё╨ ╨ Ё AM Ё TO Ё PB Ё MG Ё RS Ё      юдд".
           05  LINE 17  COLUMN 41
               VALUE  "дддддддддддддддддддддддддддддддды Ё ╨ ╨Ё".
           05  LINE 18  COLUMN 01
               VALUE  "Ё╨ х╩юддддаддддаддддаддддаддддаддддддддд".
           05  LINE 18  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддддддддддыи╪ ╨Ё".
           05  LINE 19  COLUMN 01
               VALUE  "Ё╨  ╨     MENSAGEM:".
           05  LINE 19  COLUMN 41
               VALUE  "                                   ╨  ╨Ё".
           05  LINE 20  COLUMN 01
               VALUE  "Ё╨  лммм╩ юдддддды".
           05  LINE 20  COLUMN 41
               VALUE  "                               иммм╧  ╨Ё".
           05  LINE 21  COLUMN 01
               VALUE  "Ё╨  ╨   хммммм╩ддддддддддддддддддддддддд".
           05  LINE 21  COLUMN 41
               VALUE  "дддддддддддддддддддддддддиммммм╪   ╨  ╨Ё".
           05  LINE 22  COLUMN 01
               VALUE  "Ёлмм╪    иммммйммммммммммммммммммммммммм".
           05  LINE 22  COLUMN 41
               VALUE  "мммммммммммммммммммммммммймммм╩    хмм╧Ё".
           05  LINE 23  COLUMN 01
               VALUE  "Ёхммммммм╪зддддддддддддддддддддддддддддд".
           05  LINE 23  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддддд©хммммммм╪Ё".
           05  LINE 24  COLUMN 01
               VALUE  "юддддддддды".
           05  LINE 24  COLUMN 41
               VALUE  "                             юддддддддд".
           05  CEPT
               LINE 04  COLUMN 15  PIC 9(08)
               USING  CEP.
           05  BAIRROT
               LINE 04  COLUMN 25  PIC X(20)
               USING  BAIRRO.
           05  CIDADET
               LINE 04  COLUMN 48  PIC X(20)
               USING  CIDADE.
           05  LOGRADOUROT
               LINE 09  COLUMN 10  PIC X(40)
               USING  LOGRADOURO.
           05  UFST
               LINE 14  COLUMN 42  PIC X(02)
               USING  UF.
           05  UFNT
               LINE 15  COLUMN 44  PIC X(23)
               USING  UFN.
           05  MENSAGEMT
               LINE 19  COLUMN 20  PIC X(30)
               USING  MENSAGEM.
       PROCEDURE DIVISION.
       R01.
           OPEN I-O PRO
           IF FS NOT = "00"
               OPEN OUTPUT PRO
               CLOSE PRO
               GO TO R01.
       R02.
           MOVE SPACES TO LOGRADOURO CIDADE BAIRRO UF UFN MENSAGEM
           MOVE ZEROS TO CEP IND
           DISPLAY CEPTELA
       .
       R03.
           ACCEPT CEPT
           IF CEP = ZEROS
               MOVE " CEP INVALIDA " TO MENSAGEM
               PERFORM MS1 THRU MS2
               GO TO R03
       .
       R04.
           READ PRO
           IF FS = "00"
               DISPLAY CEPT
               DISPLAY LOGRADOUROT
               DISPLAY BAIRROT
               DISPLAY CIDADET
               DISPLAY UFST
               DISPLAY UFNT
               GO TO R12
           ELSE
               NEXT SENTENCE
       .
       R05.
           ACCEPT LOGRADOUROT
           IF LOGRADOURO = SPACES
               MOVE " LOGRADOURO INVALIDO " TO MENSAGEM
               PERFORM MS1 THRU MS2
               GO TO R05
       .
       R06.
           ACCEPT BAIRROT
           IF BAIRRO = SPACES
               MOVE " BAIRRO INVALIDO " TO MENSAGEM
               PERFORM MS1 THRU MS2
               GO TO R06
       .
       R07.
           ACCEPT CIDADET
           IF CIDADE = SPACES
               MOVE " CIDADE INVALIDA " TO MENSAGEM
               PERFORM MS1 THRU MS2
               GO TO R07
       .
       R09.
           ACCEPT UFST
           IF UF = SPACES
               MOVE "UF INVALIDO" TO MENSAGEM
               PERFORM MS1 THRU MS2
               GO TO R09
           ELSE
               PERFORM RFF1 THRU RFF2
               IF VAL = "F"
                   GO TO R09
       .

       R11.
           WRITE PROT
           IF FS = "00"
               MOVE " GRAVADO " TO MENSAGEM
               PERFORM MS1 THRU MS2

       .
       R12.
           MOVE "E EXCLUIR R REPETIR A ALTERAR" TO MENSAGEM
           PERFORM MS1 THRU MS2
           ACCEPT (19, 50) VL
           IF VL = "E"
               DISPLAY (19, 50) "OP:"
               ACCEPT (19, 51) CEP
               PERFORM EXC
           ELSE
               IF VL = "R"
                   GO TO R02
               ELSE
                   IF VL = "A"
                       PERFORM RW
       .
       RF1.
           CLOSE PRO
           STOP RUN
           .
       RF2.
           EXIT PROGRAM
           .
       RFF1.
           MOVE ZEROS TO IND
       .
       RFF2.
           ADD 1 TO IND
           MOVE TBUFV1(IND) TO TBUFR
           IF TBUFR1 NOT = UF
               IF IND < 27
                   GO TO RFF2
               ELSE
                   MOVE "ERRO" TO MENSAGEM
                   MOVE "F" TO VAL
           ELSE

               MOVE TBUFR2 TO UFN
               DISPLAY UFNT
               MOVE IND TO VALIT
               MOVE "V" TO VAL
       .
       MS1.
           MOVE ZEROS TO TM
           DISPLAY MENSAGEMT
       .
       MS2.
           IF TM < 999
               ADD 1 TO TM
               GO TO MS2
           ELSE
               MOVE SPACES TO MENSAGEM
       .

       EXC.
           DELETE PRO RECORD
           IF FS = "00"
               MOVE "EXCLUIDO " TO MENSAGEM
               PERFORM MS1 THRU MS2
           ELSE
               MOVE "ERRO" TO MENSAGEM
               PERFORM MS1 THRU MS2
       .

       RW.
           DISPLAY (19, 50) "OP:"
           ACCEPT (19, 51) CEP
           READ PRO
           IF FS = "00"
               DISPLAY CEPT
               DISPLAY LOGRADOUROT
               DISPLAY BAIRROT
               DISPLAY CIDADET
               DISPLAY UFST
               DISPLAY UFNT
               PERFORM R05 THRU R09
               REWRITE PROT
               IF FS = "00" OR "02"
                   MOVE "ALTERADO" TO MENSAGEM
                   PERFORM MS1 THRU MS2
               ELSE
                   MOVE "ERRO AO ALTERAR" TO MENSAGEM
                   PERFORM MS1 THRU MS2

           ELSE
               MOVE "CHAVE NAO CADASTRADA" TO MENSAGEM
               PERFORM MS1 THRU MS2
           .
