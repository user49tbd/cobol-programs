       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGGER.
       AUTHOR. AUT.
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
           RECORD KEY IS RK
           .

           SELECT CADAREAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS ARK
           .

           SELECT CADCARGO ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS CRK.

           SELECT PRO ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS CEP.

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
      *AREAS VARIAVEIS
      *-----------------------------------------------------------------
       FD CADAREAS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCEP1".
       01 AR.
           03 ARK   PIC  9(03).
           03 DESC PIC  X(20).
           03 N    PIC  9(01).
           03 NTXT PIC  X(19).
           03 ASRK  PIC  9(03).
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
      *CEP VARIAVEIS
      *-----------------------------------------------------------------
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
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
           01 FS       PIC X(02).
           01 MENSAGEM PIC X(40).
           01 OPT      PIC X(01).
           01 SRK      PIC 9(06).
           01 CONT     PIC 9(04).
           01 CONT2    PIC 9(04).
           01 VL       PIC X(01).
      *-----------------------------------------------------------------
           01 GENTIP   PIC X(20) VALUE "MASCULINOFEMENINO ".
           01 TABGENTIP REDEFINES GENTIP.
               03 ARRGENTIP    PIC X(09) OCCURS 2 TIMES.
      *-----------------------------------------------------------------
           01 TABGEN.
               03 FILLER  PIC X(16) VALUE "1HETEROSEXUAL".
               03 FILLER  PIC X(16) VALUE "2HOMOSEXUAL".
               03 FILLER  PIC X(16) VALUE "3BISEXUAL".
               03 FILLER  PIC X(16) VALUE "4PANSEXUAL".
               03 FILLER  PIC X(16) VALUE "5NAO DECLARADO".
           01 TABGENARR REDEFINES TABGEN.
               03 ARRGEN  PIC X(16) OCCURS 5 TIMES.
           01 ARRGENREAD.
               03 ARRGENREAD1  PIC X(01).
               03 ARRGENREAD2  PIC X(15).
      *-----------------------------------------------------------------
      *AREAS VARIAVEIS
      *-----------------------------------------------------------------
           01 ASRK2       PIC 9(03).
           01 TABAREAS.
               03 ARRAREAS  PIC 9(03) OCCURS 9 TIMES.
           01 ACONT       PIC 9(12).
           01 ACONT2       PIC 9(12).
           01 ACONT3       PIC 9(12).
      *-----------------------------------------------------------------
           01 SUBA        PIC 9(03).
           01 SUBATXT     PIC X(19).

      *-----------------------------------------------------------------
      *CARGO VARIAVEIS
      *-----------------------------------------------------------------
           01 DISPTPBENTXT     PIC X(20).
           01 TABB.
               03 FILLER   PIC X(21) VALUE "1PLANO DE SAUDE".
               03 FILLER   PIC X(21) VALUE "2COMBUSTIVEL".
               03 FILLER   PIC X(21) VALUE "3VALE ALIMENTACAO".
               03 FILLER   PIC X(21) VALUE "4CESTA BASICA".
           01 AB REDEFINES TABB.
               03 ARRB     PIC X(21) OCCURS 4 TIMES.
           01 BENLEVEL     PIC 9(01).
           01 BENLEVELTXT  PIC X(20).
           01 ARRBENCHECK.
               03 ARRBENCHECK1    PIC 9(01).
               03 ARRBENCHECK2    PIC X(20).
           01 ARRBTXT.
               03 ARRBTXT2    PIC X(20) OCCURS 4 TIMES.
           01 ARRLEVEL.
               03 ARRLEVEL2    PIC 9(01) OCCURS 4 TIMES.
      *-----------------------------------------------------------------
      *CEP VARIAVEIS
      *-----------------------------------------------------------------
      *     01 NUM          PIC 9(05).
      *     01 COMPLEMENTO  PIC X(12).
       SCREEN SECTION.
       01  CDFTELA.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "здддддддддддбддддддддддддддддддддддддддд".
           05  LINE 01  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддбдддддддддддд©".
           05  LINE 02  COLUMN 01
               VALUE  "Ёздддммммм╩ юддддмммммммммм╩CADASTRO DE".
           05  LINE 02  COLUMN 41
               VALUE  "FUNCIONARIOиммммммммммдддды иммммммддд©Ё".
           05  LINE 03  COLUMN 01
               VALUE  "ЁЁздддддд©хмммм╩ ддддддддд©сдддддддддддд".
           05  LINE 03  COLUMN 41
               VALUE  "ддддддддддд╫зддддддддд имммм╪зддддддд©ЁЁ".
           05  LINE 04  COLUMN 01
               VALUE  "Ёюдддммм╩юдддд хмммммм╩   тммямкмммммммм".
           05  LINE 04  COLUMN 41
               VALUE  "мммммммкмямм╬   имммммм╪ ддддыиммммдддыЁ".
           05  LINE 05  COLUMN 01
               VALUE  "Ёи╩дддд©хмммммммммммммйммммдды хмммммммм".
           05  LINE 05  COLUMN 41
               VALUE  "ммммммм╪ юддммммйммммммммммммм╪зддддди╩Ё".
           05  LINE 06  COLUMN 01
               VALUE  "Ёл╧здд юдддддддддддддддддддддддддддддддд".
           05  LINE 06  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддддддды  дд©л╧Ё".
           05  LINE 07  COLUMN 01
               VALUE  "Ёл╧ЁNUM MATRICULA:       NOME:".
           05  LINE 07  COLUMN 41
               VALUE  "                                    Ёл╧Ё".
           05  LINE 08  COLUMN 01
               VALUE  "Ёл╧ЁCPF:            RG:          SEXO:".
           05  LINE 08  COLUMN 41
               VALUE  "           GENERO:                  Ёл╧Ё".
           05  LINE 09  COLUMN 01
               VALUE  "Ёл╧ЁDATA-NASCIMENTO:  |  |     NUMERO DE".
           05  LINE 09  COLUMN 41
               VALUE  "PENDENTES:   TELEFONE:              Ёл╧Ё".
           05  LINE 10  COLUMN 01
               VALUE  "Ёл╧ЁEMAIL:".
           05  LINE 10  COLUMN 41
               VALUE  "                                    Ёл╧Ё".
           05  LINE 11  COLUMN 01
               VALUE  "Ёхйомддддддддддддддддддддддддддддддддддд".
           05  LINE 11  COLUMN 41
               VALUE  "дддддддддддддддддддддддддддддддддддмой╪Ё".
           05  LINE 12  COLUMN 01
               VALUE  "Ё".
           05  LINE 12  COLUMN 41
               VALUE  "                          зддддддддд©  Ё".
           05  LINE 13  COLUMN 01
               VALUE  "Ёздддддддддддддддддддддддддддддддддддддд".
           05  LINE 13  COLUMN 41
               VALUE  "дддддддддддддддддддддддддд╢         цд©Ё".
           05  LINE 14  COLUMN 01
               VALUE  "ЁЁ".
           05  LINE 14  COLUMN 41
               VALUE  "                          юддддддддды ЁЁ".
           05  LINE 15  COLUMN 01
               VALUE  "ЁЁ".
           05  LINE 15  COLUMN 41
               VALUE  "                                      ЁЁ".
           05  LINE 16  COLUMN 01
               VALUE  "ЁЁ".
           05  LINE 16  COLUMN 41
               VALUE  "                                      ЁЁ".
           05  LINE 17  COLUMN 01
               VALUE  "ЁЁ".
           05  LINE 17  COLUMN 41
               VALUE  "                                      ЁЁ".
           05  LINE 18  COLUMN 01
               VALUE  "ЁЁ".
           05  LINE 18  COLUMN 41
               VALUE  "                                      ЁЁ".
           05  LINE 19  COLUMN 01
               VALUE  "Ёюддддддддддд©".
           05  LINE 19  COLUMN 41
               VALUE  "                                      ЁЁ".
           05  LINE 20  COLUMN 01
               VALUE  "Ё зддддддддд©юдддддддддддддддддддддддддд".
           05  LINE 20  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддддддддддддддыЁ".
           05  LINE 21  COLUMN 01
               VALUE  "Ёз╢MENSAGEM:цддддддддддддддддддддддддддд".
           05  LINE 21  COLUMN 41
               VALUE  "дддддддддддддбддддддбддддддддддддддддд©Ё".
           05  LINE 22  COLUMN 01
               VALUE  "ЁЁюддддддддды".
           05  LINE 22  COLUMN 41
               VALUE  "             ЁOP:   ЁRK:              ЁЁ".
           05  LINE 23  COLUMN 01
               VALUE  "Ёюдддддддддддддддддддддддддддддддддддддд".
           05  LINE 23  COLUMN 41
               VALUE  "дддддддддддддаддддддадддддддддддддддддыЁ".
           05  LINE 24  COLUMN 01
               VALUE  "юддддддддддддддддддддддддддддддддддддддд".
           05  LINE 24  COLUMN 41
               VALUE  "ддддддддддддддддддддддддддддддддддддддды".
           05  TRK
               LINE 07  COLUMN 19  PIC 9(06)
               USING  RK.
           05  TNOME
               LINE 07  COLUMN 31  PIC X(40)
               USING  NOME.
           05  TCPF
               LINE 08  COLUMN 09  PIC 9(11)
               USING  CPF.
           05  TRG
               LINE 08  COLUMN 24  PIC 9(09)
               USING  RG.
           05  TSEXO
               LINE 08  COLUMN 39  PIC X(01)
               USING  SEXO.
           05  TSEXOTXT
               LINE 08  COLUMN 41  PIC X(10)
               USING  SEXOTXT.
           05  TGENERO
               LINE 08  COLUMN 59  PIC 9(01)
               USING  GENERO.
           05  TGENEROTXT
               LINE 08  COLUMN 61  PIC X(15)
               USING  GENEROTXT.
           05  TDNDIA
               LINE 09  COLUMN 21  PIC 9(02)
               USING  DNDIA.
           05  TDNMES
               LINE 09  COLUMN 24  PIC 9(02)
               USING  DNMES.
           05  TDNANO
               LINE 09  COLUMN 27  PIC 9(04)
               USING  DNANO.
           05  TNUMDEPEND
               LINE 09  COLUMN 51  PIC 9(02)
               USING  NUMDEPEND.
           05  TTELEFONE
               LINE 09  COLUMN 63  PIC 9(11)
               USING  TELEFONE.
           05  TEMAIL
               LINE 10  COLUMN 11  PIC X(40)
               USING  EMAIL.
           05  TMENSAGEM
               LINE 22  COLUMN 14  PIC X(40)
               USING  MENSAGEM.
           05  TOPT
               LINE 22  COLUMN 59  PIC X(01)
               USING  OPT.
           05  TSRK
               LINE 22  COLUMN 66  PIC 9(06)
               USING  SRK.
      *-----------------------------------------------------------------
      *TELA AREAS
      *-----------------------------------------------------------------
       01  CDFAREA.
           05  LINE 13  COLUMN 68
               VALUE  "AREAS".
           05  LINE 14  COLUMN 03
               VALUE  "здддддд© DESCRICAO:".
           05  LINE 15  COLUMN 03
               VALUE  "ЁRK:   Ё NIVEL:".
           05  LINE 16  COLUMN 03
               VALUE  "юдддддды AREA:".
           05  LINE 16  COLUMN 41
               VALUE  " SUB AREA:".
           05  LINE 17  COLUMN 04
               VALUE  "дддддд".
           05  LINE 19  COLUMN 15
               VALUE  "AREAS SUBORDINADAS:".
           05  TDESC
               LINE 14  COLUMN 22  PIC X(20)
               USING  DESC.
           05  TARK
               LINE 15  COLUMN 07  PIC 9(03)
               USING  ARK2.
           05  TN
               LINE 15  COLUMN 19  PIC 9(01)
               USING  N.
           05  TASRK
               LINE 16  COLUMN 18  PIC 9(03)
               USING  ASRK.
           05  TNTXT
               LINE 16  COLUMN 22  PIC X(19)
               USING  NTXT.
           05  TSUBA
               LINE 16  COLUMN 51  PIC 9(03)
               USING  SUBA.
           05  TSUBATXT
               LINE 16  COLUMN 55  PIC X(19)
               USING  SUBATXT.
      *-----------------------------------------------------------------
      *TELA CARGOS
      *-----------------------------------------------------------------
       01  CDFBENTELA.
           05  LINE 13  COLUMN 68
               VALUE  "  CARGO".
           05  LINE 14  COLUMN 03
               VALUE  "зддддддд© DESCRCAO:".
           05  LINE 15  COLUMN 03
               VALUE  "ЁRK:    Ё TIPO DE SERVICO:".
           05  LINE 15  COLUMN 41
               VALUE  "               зддддддддддддддддддддд©".
           05  LINE 16  COLUMN 03
               VALUE  "юддддддды TIPO DE BENEFICIO:".
           05  LINE 16  COLUMN 41
               VALUE  "               ЁSALARIO:             Ё".
           05  LINE 17  COLUMN 03
               VALUE  " ддддддд  NIVEL DO BENEFICO:".
           05  LINE 17  COLUMN 41
               VALUE  "               юддддддддддддддддддддды".
           05  TCDESC
               LINE 14  COLUMN 23  PIC X(25)
               USING  CDESC.
           05  TCRK
               LINE 15  COLUMN 07  PIC 9(04)
               USING  CRK2.
           05  TTS
               LINE 15  COLUMN 30  PIC X(01)
               USING  TS.
           05  TTSTXT
               LINE 15  COLUMN 32  PIC X(12)
               USING  TSTXT.
           05  TDISPTPBENTXT
               LINE 16  COLUMN 32  PIC X(20)
               USING  DISPTPBENTXT.
           05  TSALVAL
               LINE 16  COLUMN 65  PIC 9(06)V99
               USING  SALVAL.
           05  TBENLEVEL
               LINE 17  COLUMN 32  PIC 9(01)
               USING  BENLEVEL.
           05  TBENLEVELTXT
               LINE 17  COLUMN 34  PIC X(20)
               USING  BENLEVELTXT.
      *-----------------------------------------------------------------
      *TELA CEP
      *-----------------------------------------------------------------
       01  CDFCEPTELA.
           05  LINE 13  COLUMN 68
               VALUE  "   CEP".
           05  LINE 14  COLUMN 03
               VALUE  "здддддддддддд©здддддддддддд©зддддддддд".
           05  LINE 14  COLUMN 41
               VALUE  "ддддддддддддддд©".
           05  LINE 15  COLUMN 03
               VALUE  "ЁCEP:        ЁЁNUMERO:     ЁЁCOMPLEMEN".
           05  LINE 15  COLUMN 41
               VALUE  "TO:            Ё".
           05  LINE 16  COLUMN 03
               VALUE  "юддддддддддддыюддддддддддддыюддддддддд".
           05  LINE 16  COLUMN 41
               VALUE  "ддддддддддддддды".
           05  LINE 17  COLUMN 03
               VALUE  "LOGRADOURO:".
           05  LINE 18  COLUMN 03
               VALUE  "BAIRRO:                     CIDADE:".
           05  LINE 19  COLUMN 15
               VALUE  "UF:".
           05  TCEP
               LINE 15  COLUMN 08  PIC 9(08)
               USING  CEP2.
           05  TNUM
               LINE 15  COLUMN 25  PIC 9(05)
               USING  NUM.
           05  TCOMPLEMENTO
               LINE 15  COLUMN 44  PIC X(12)
               USING  COMPLEMENTO.
           05  TLOGRADOURO
               LINE 17  COLUMN 14  PIC X(40)
               USING  LOGRADOURO.
           05  TBAIRRO
               LINE 18  COLUMN 10  PIC X(20)
               USING  BAIRRO.
           05  TCIDADE
               LINE 18  COLUMN 38  PIC X(20)
               USING  CIDADE.
           05  TUF
               LINE 19  COLUMN 18  PIC X(02)
               USING  UF.
           05  TUFN
               LINE 19  COLUMN 21  PIC X(23)
               USING  UFN.


       PROCEDURE DIVISION.
       OP0.
           OPEN I-O FILEFUNC
           IF FS NOT = "00"
               OPEN OUTPUT FILEFUNC
               CLOSE FILEFUNC
               GO TO OP0
           .
       OP0AREA.
           OPEN I-O CADAREAS
           IF FS NOT = "00"
               OPEN OUTPUT CADAREAS
               CLOSE CADAREAS
               GO TO OP0AREA
           .
       OP0CARGO.
           OPEN I-O CADCARGO
           IF FS NOT = "00"
               OPEN OUTPUT CADCARGO
               CLOSE CADCARGO
               GO TO OP0CARGO
           .

       OP0CEP.
           OPEN I-O PRO
           IF FS NOT = "00"
               OPEN OUTPUT PRO
               CLOSE PRO
               GO TO OP0CEP
           .

       OP01.
           MOVE ZEROS TO RK CPF RG GENERO DNDIA DNMES DNANO NUMDEPEND
           MOVE ZEROS TO TELEFONE CONT SRK CONT2 ARK ASRK N CRK2
           MOVE SPACES TO NOME SEXO SEXOTXT GENEROTXT EMAIL OPT MENSAGEM
           MOVE SPACES TO VL ARRGENREAD DESC NTXT SUBATXT CDESC TS TSTXT
           MOVE SPACES TO BPSTXT BCOMTXT BVATXT BCBTXT DISPTPBENTXT
           MOVE ZEROS TO ACONT ACONT2 ACONT3 ASRK2 SUBA CRK BPS BCOM BVA
           MOVE ZEROS TO BCB SALVAL BENLEVEL CEP VALIT NUM ARK2 CEP2
           MOVE SPACES TO BENLEVELTXT UF UFN LOGRADOURO BAIRRO CIDADE
           MOVE SPACES TO COMPLEMENTO
           .
       OP02.
           DISPLAY CDFTELA
           .
       OP03.
           ACCEPT TRK
           IF RK = ZEROS
               MOVE "NUM MATRICULA NAO NPODE SER NULO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP03
           .
       OP04.
           READ FILEFUNC
           IF FS = "00"
               DISPLAY TRK
               DISPLAY TNOME
               DISPLAY TCPF
               DISPLAY TRG
               DISPLAY TSEXO
               DISPLAY TSEXOTXT
               DISPLAY TGENERO
               DISPLAY TGENEROTXT
               DISPLAY TDNDIA
               DISPLAY TDNMES
               DISPLAY TDNANO
               DISPLAY TNUMDEPEND
               DISPLAY TTELEFONE
               DISPLAY TEMAIL
      *-----------------------------------------------------------------
               DISPLAY CDFAREA
               DISPLAY TARK
               MOVE ARK2 TO ARK
               PERFORM READAREA
               PERFORM OPMSG1 THRU OPMSG2
      *-----------------------------------------------------------------
               DISPLAY CDFTELA
               DISPLAY CDFBENTELA
               DISPLAY TCRK
               MOVE CRK2 TO CRK
               PERFORM READCARGO
               PERFORM OPMSG1 THRU OPMSG2
      *-----------------------------------------------------------------
               DISPLAY CDFTELA
               DISPLAY CDFCEPTELA
               MOVE CEP2 TO CEP
               DISPLAY TCEP
               DISPLAY TNUM
               DISPLAY TCOMPLEMENTO
               PERFORM READCEP
               GO TO OP014
           ELSE
               MOVE "NAO CADASTRADO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
           .
       OP05.
           ACCEPT TNOME
           IF NOME = SPACES
               MOVE "NOME NAO PODE SER NULO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP05
           .
       OP06.
           ACCEPT TCPF
           IF CPF = ZEROS
               MOVE "CPF NAO PODE SER NULO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP06
           .
       OP07.
           ACCEPT TRG
           IF RG = ZEROS
               MOVE "RG NAO PODE SER NULO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP07
           .
       OP08.
           ACCEPT TSEXO
           IF SEXO = SPACES
               MOVE "SEXO NAO PODE SER NULO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP08
           ELSE
               IF SEXO = "M" OR "m"
                   MOVE ARRGENTIP(1) TO SEXOTXT
                   DISPLAY TSEXOTXT
               ELSE
                   IF SEXO = "F" OR "f"
                       MOVE ARRGENTIP(2) TO SEXOTXT
                       DISPLAY TSEXOTXT
                   ELSE
                       MOVE "INPUT INCORRETO" TO MENSAGEM
                       PERFORM OPMSG1 THRU OPMSG2
                       GO TO OP08
           .
       OP09.
           ACCEPT TGENERO
           IF GENERO = SPACES
               MOVE "GENERO NAO PODE SER NULO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP09
           ELSE
               PERFORM LOOPCONF1 THRU LOOPCONF2
               IF VL = "F"
                   GO TO OP09
           .
       OP010.
           ACCEPT TDNDIA
           ACCEPT TDNMES
           ACCEPT TDNANO
           IF DNDIA = ZEROS OR DNMES = ZEROS OR DNANO = ZEROS
               MOVE "VALORES DA DATA NAO PODEM SER NULOS " TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP010
           .
       OP011.
           ACCEPT TNUMDEPEND
           .
       OP012.
           ACCEPT TTELEFONE
           IF TELEFONE = ZEROS
               MOVE "TELEFONE NAO PODE SER NULO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP012
           .
       OP013.
           ACCEPT TEMAIL
           IF EMAIL = SPACES
               MOVE "EMAIL NAO PODE SER NULO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP013
           .
      *-----------------------------------------------------------------
       OP014A.
           DISPLAY CDFAREA
           ACCEPT TARK
           MOVE ARK2 TO ARK
           PERFORM READAREA

           .
       OP014A2.
           MOVE "R OU r PARA REPETIR" TO MENSAGEM
           PERFORM OPMSG1 THRU OPMSG2
           ACCEPT TOPT
           IF OPT = "R" OR "r"
               MOVE "REPETIR" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP014A
           .

       OP014B.
           DISPLAY CDFTELA
           DISPLAY CDFBENTELA
           ACCEPT TCRK
           MOVE CRK2 TO CRK
           PERFORM READCARGO

           .
       OP014B2.
           MOVE "R OU r PARA REPETIR" TO MENSAGEM
           PERFORM OPMSG1 THRU OPMSG2
           ACCEPT TOPT
           IF OPT = "R" OR "r"
               MOVE "REPETIR" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP014B
           .

       OP014C.
           DISPLAY CDFTELA
           DISPLAY CDFCEPTELA
           PERFORM OPCEP THRU OPCOMPLEMENTO
           PERFORM READCEP

      *     ACCEPT TCEP
      *     PERFORM READCARGO

           .
       OP014C2.
           MOVE "R OU r PARA REPETIR" TO MENSAGEM
           PERFORM OPMSG1 THRU OPMSG2
           ACCEPT TOPT
           IF OPT = "R" OR "r"
               MOVE "REPETIR" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OP014C
           .

       OP014.
           DISPLAY CDFTELA
           PERFORM OPW
           .
       OP015.
           MOVE "A ATUALIZAR R REPETIR D DELETAR" TO MENSAGEM
           PERFORM OPMSG1 THRU OPMSG2
           ACCEPT TOPT
           IF OPT = "R"
               GO TO OP01
               ELSE
                   IF OPT = "D"
                       PERFORM OPSRK
                       PERFORM OPR
                   ELSE
                       IF OPT = "A"
                           PERFORM OPSRK
      *                     PERFORM OP05 THRU OP013
                           PERFORM OP05 THRU OP014C2
                           PERFORM OPRW
           .
       OPF01.
           STOP RUN
           .
       OPF02.
           EXIT PROGRAM
           .
       OPMSG1.
           MOVE ZEROS TO CONT
           ADD 1 TO CONT
           DISPLAY TMENSAGEM
           .
       OPMSG2.
           IF CONT < 3333
               ADD 1 TO CONT
               GO TO OPMSG2
           ELSE
               MOVE SPACES TO MENSAGEM
               DISPLAY TMENSAGEM
           .
       LOOPCONF1.
           MOVE ZEROS TO CONT2
           ADD 1 TO CONT2
           .
       LOOPCONF2.
           MOVE ARRGEN(CONT2) TO ARRGENREAD
           IF ARRGENREAD1 NOT = GENERO
               IF CONT2 < 6
                   ADD 1 TO CONT2
                   GO TO LOOPCONF2
               ELSE
                   MOVE "VALOR NAO ENCOTRADO" TO MENSAGEM
                   PERFORM OPMSG1 THRU OPMSG2
                   MOVE "F" TO VL
           ELSE
               MOVE ARRGENREAD2 TO GENEROTXT
               DISPLAY TGENEROTXT
               MOVE "V" TO VL
           .
       OPW.
           WRITE FILES
           IF FS = "00"
               MOVE "GRAVADO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
           ELSE
               MOVE "ERRO AO GRAVAR" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
           .
       OPRW.
           REWRITE FILES
           IF FS = "00"
               MOVE "ATUALIZADO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
           ELSE
               MOVE "ERRO AO ATUALIZAR" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
           .
       OPR.
           DELETE FILEFUNC RECORD
           IF FS = "00"
               MOVE "REMOVIDO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
           ELSE
               MOVE "ERRO AO REMOVER" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
           .
       OPERATION.
           MOVE "A ATUALIZAR R REPETIR D DELETAR" TO MENSAGEM
           PERFORM OPMSG1 THRU OPMSG2
           ACCEPT TOPT
           IF OPT = "R"
               GO TO OP01
               ELSE
                   IF OPT = "D"
                       PERFORM OPSRK
                       PERFORM OPR
                   ELSE
                       IF OPT = "A"
                           PERFORM OPSRK
                           PERFORM OP05 THRU OP013
                           PERFORM OPRW
           .
       OPSRK.
           ACCEPT TSRK
           MOVE SRK TO RK
           READ FILEFUNC
           IF FS NOT = "00"
               MOVE "CHAVE NAO CADASTRADA DIGITE OUTRA" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OPSRK
           ELSE
               DISPLAY TRK
               DISPLAY TNOME
               DISPLAY TCPF
               DISPLAY TRG
               DISPLAY TSEXO
               DISPLAY TSEXOTXT
               DISPLAY TGENERO
               DISPLAY TGENEROTXT
               DISPLAY TDNDIA
               DISPLAY TDNMES
               DISPLAY TDNANO
               DISPLAY TNUMDEPEND
               DISPLAY TTELEFONE
               DISPLAY TEMAIL
           .
       READAREA.
           READ CADAREAS
           IF FS= "00"
               DISPLAY TDESC
               DISPLAY TN
               DISPLAY TNTXT
               DISPLAY TASRK
               PERFORM ARRCLEAR1 THRU ARRCLEAR2
               PERFORM READCADA1 THRU READCADA2
               DISPLAY (19, 35) ARRAREAS(1) "-" ARRAREAS(2) "-"
               ARRAREAS(3) "-"
               ARRAREAS(4) "-" ARRAREAS(5) "-" ARRAREAS(6) "-"
               ARRAREAS(7) "-" ARRAREAS(8) "-" ARRAREAS(9)
               PERFORM IDAR1 THRU IDAR2
           ELSE
               MOVE "NAO EXISTE" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
           .
       ARRCLEAR1.
           MOVE ZEROS TO ACONT2
           ADD 1 TO ACONT2
           .
       ARRCLEAR2.
           IF ACONT2 < 10
               MOVE ZEROS TO ARRAREAS(ACONT2)
               ADD 1 TO ACONT2
               GO TO ARRCLEAR2.
           .
       READCADA1.
           MOVE ZEROS TO ACONT
           ADD 1 TO ACONT
           .
       READCADA2.
           MOVE ASRK TO ARRAREAS(ACONT)
           MOVE ASRK TO ARK
           READ CADAREAS
           IF FS = "00"
               ADD 1 TO ACONT
               GO TO READCADA2
           ELSE
               MOVE "CHAVE NAO CADASTRADA" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
           .
       IDAR1.
           MOVE ZEROS TO ACONT2
           ADD 1 TO ACONT2
           .
       IDAR2.
           MOVE ARRAREAS(ACONT2) TO ARK
           MOVE ARK TO MENSAGEM
           PERFORM OPMSG1 THRU OPMSG2
           IF ACONT2 < ACONT
               READ CADAREAS
               MOVE ASRK TO SUBA
               DISPLAY TSUBA
               MOVE NTXT TO SUBATXT
               DISPLAY TSUBATXT
               ADD 1 TO ACONT2
               PERFORM OPMSG1 THRU OPMSG2
               GO TO IDAR2

           .
       READCARGO.
           READ CADCARGO
           IF FS = "00"
               DISPLAY TCDESC
               DISPLAY TTS
               DISPLAY TTSTXT
               PERFORM OPDISPBEN1 THRU OPDISPBEN2
               DISPLAY TSALVAL

           .
       OPDISPBEN1.
           MOVE ZEROS TO ACONT
           ADD 1 TO ACONT
           MOVE BPSTXT TO ARRBTXT2(1).
           MOVE BCOMTXT TO ARRBTXT2(2).
           MOVE BVATXT TO ARRBTXT2(3).
           MOVE BCBTXT TO ARRBTXT2(4).

           MOVE BPS TO ARRLEVEL2(1).
           MOVE BCOM TO ARRLEVEL2(2).
           MOVE BVA TO ARRLEVEL2(3).
           MOVE BCB TO ARRLEVEL2(4).
           .
       OPDISPBEN2.
           IF ACONT < 5
               MOVE ARRB(ACONT) TO ARRBENCHECK
               MOVE ARRBENCHECK2 TO DISPTPBENTXT
               DISPLAY TDISPTPBENTXT

               MOVE ARRLEVEL2(ACONT) TO BENLEVEL
               DISPLAY TBENLEVEL

               MOVE ARRBTXT2(ACONT) TO BENLEVELTXT
               DISPLAY TBENLEVELTXT
               PERFORM OPMSG1 THRU OPMSG2
               ADD 1 TO ACONT
               GO TO OPDISPBEN2
           .
       OPCEP.
           ACCEPT TCEP
           IF CEP2 = ZEROS
               MOVE "CEP NAO PODE SER NULO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OPCEP
           ELSE
               MOVE CEP2 TO CEP
           .
       OPNUM.
           ACCEPT TNUM
           IF NUM = ZEROS
               MOVE "NUMERO NAO PODE SER NULO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OPNUM
           .
       OPCOMPLEMENTO.
           ACCEPT TCOMPLEMENTO
           IF COMPLEMENTO = SPACES
               MOVE "COMPLEMENTO NAO PODE SER NULO" TO MENSAGEM
               PERFORM OPMSG1 THRU OPMSG2
               GO TO OPCOMPLEMENTO
           .
       READCEP.
           READ PRO
           IF FS = "00"
               DISPLAY TLOGRADOURO
               DISPLAY TBAIRRO
               DISPLAY TCIDADE
               DISPLAY TUF
               DISPLAY TUFN
               PERFORM OPMSG1 THRU OPMSG2
           ELSE
               MOVE "****************************************"
               TO LOGRADOURO
               DISPLAY TLOGRADOURO
               MOVE "********************" TO BAIRRO
               DISPLAY TBAIRRO
               MOVE "********************" TO CIDADE
               DISPLAY TCIDADE
               MOVE "**" TO UF
               DISPLAY TUF
               MOVE "***********************" TO UFN
               DISPLAY TUFN
               PERFORM OPMSG1 THRU OPMSG2
               PERFORM OPMSG1 THRU OPMSG2
           .
