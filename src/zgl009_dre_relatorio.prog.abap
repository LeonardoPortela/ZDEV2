************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 25.03.2009                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Relatório de aprensantação da DRE Gerencial         *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 25.03.2009    Marcus.Barbara       Criação              DEVK905716   *
************************************************************************

REPORT  ZGL009_DRE_RELATORIO.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPE-POOLS: STREE, KLFAZ.

*&---------------------------------------------------------------------*
*& Declarações de Tabelas.
*&---------------------------------------------------------------------*
CONSTANTS: C_MARK TYPE C VALUE 'X'.

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
DATA: BEGIN OF WA_CONTA,
        SAKNR            LIKE SKAT-SAKNR,
        TXT50            LIKE SKAT-TXT50,
      END   OF WA_CONTA,

      BEGIN OF WA_CUSTO,
        KOSTL            LIKE CSKT-KOSTL,
        LTEXT            LIKE CSKT-LTEXT,
      END   OF WA_CUSTO,

      BEGIN OF WA_DRE_008_NIVEL,
        NIVEL_TOTAL      LIKE ZGL008_DRE_TOTAL-NIVEL,
        NIVEL            LIKE ZGL008_DRE_TOTAL-NIVEL,
        SINAL(1),
      END OF WA_DRE_008_NIVEL,

      BEGIN OF WA_ORDEM,
        AUFNR            LIKE AUFK-AUFNR,
        KTEXT            LIKE AUFK-KTEXT,
      END   OF WA_ORDEM,

      BEGIN OF WA_LUCRO,
        PRCTR            LIKE CEPCT-PRCTR,
        LTEXT            LIKE CEPCT-LTEXT,
      END   OF WA_LUCRO,

      WA_DADOS           LIKE ZST_GL009_DADOS,
      WA_DADOS2          LIKE ZST_GL009_DADOS,
      WA_DRE_001         LIKE ZGL001_DRE_EST,
      WA_DRE_002         LIKE ZGL002_DRE_EST,
      WA_DRE_003         LIKE ZGL003_DRE_EST,
      WA_DRE_004         LIKE ZGL004_DRE_EST,
      WA_DRE_005         LIKE ZGL005_DRE_DADOS,
      WA_DRE_006         LIKE ZGL006_DRE_DADOS,
      WA_DRE_007         LIKE ZGL007_DRE_DADOS,
      WA_DRE_008         LIKE ZGL008_DRE_TOTAL,
      WA_MES             LIKE T247.

DATA: WA_BDCDATA         LIKE BDCDATA,
      WA_MESSAGE         LIKE BDCMSGCOLL.

DATA: IT_CONTA           LIKE STANDARD TABLE OF WA_CONTA,
      IT_CUSTO           LIKE STANDARD TABLE OF WA_CUSTO,
      IT_ORDEM           LIKE STANDARD TABLE OF WA_ORDEM,
      IT_LUCRO           LIKE STANDARD TABLE OF WA_LUCRO,
      IT_DADOS           LIKE STANDARD TABLE OF WA_DADOS,
      IT_DADOS2          LIKE STANDARD TABLE OF WA_DADOS,
      IT_DRE_001         LIKE STANDARD TABLE OF WA_DRE_001,
      IT_DRE_002         LIKE STANDARD TABLE OF WA_DRE_002,
      IT_DRE_003         LIKE STANDARD TABLE OF WA_DRE_003,
      IT_DRE_004         LIKE STANDARD TABLE OF WA_DRE_004,
      IT_DRE_005         LIKE STANDARD TABLE OF WA_DRE_005,
      IT_DRE_006         LIKE STANDARD TABLE OF WA_DRE_006,
      IT_DRE_007         LIKE STANDARD TABLE OF WA_DRE_007,
      IT_DRE_008         LIKE STANDARD TABLE OF WA_DRE_008,
      IT_DRE_008_NIVEL   LIKE STANDARD TABLE OF WA_DRE_008_NIVEL,
      IT_MES             LIKE STANDARD TABLE OF WA_MES,
      IT_BDCDATA         LIKE STANDARD TABLE OF WA_BDCDATA,
      IT_MESSAGE         LIKE STANDARD TABLE OF WA_MESSAGE.

*----------------------------------------------------------------------*
*  Tabelas Internas (ALV Tree) / Includes
*----------------------------------------------------------------------*

DATA TREE1               TYPE REF TO CL_GUI_ALV_TREE.
DATA MR_TOOLBAR          TYPE REF TO CL_GUI_TOOLBAR.

INCLUDE <ICON>.
INCLUDE BCALV_TOOLBAR_EVENT_RECEIVER.

DATA TOOLBAR_EVENT_RECEIVER TYPE REF TO LCL_TOOLBAR_EVENT_RECEIVER.

DATA: IT_FIELDCATALOG   TYPE LVC_T_FCAT, "Fieldcatalog.
      IT_ALVTREE        LIKE STANDARD TABLE OF WA_DADOS,
      GO_TBM            TYPE REF TO CL_ALV_TREE_TOOLBAR_MANAGER,
      OK_CODE           LIKE SY-UCOMM,
      VG_TITULO         LIKE ZGL001_DRE_EST-VSTXT,
      DATAINI          LIKE SY-DATUM,
      DATAFIM          LIKE SY-DATUM,
      TXDATAINI(8),
      TXDATAFIM(8),
      VLR_ACM      LIKE ZST_GL009_DADOS-VLR_ACM,
      VLR_REA      LIKE ZST_GL009_DADOS-VLR_REA,
      VLR_MOV      LIKE ZST_GL009_DADOS-VLR_MOV,
      VLR_CALC     LIKE ZST_GL009_DADOS-VLR_CALC,
      VLR_MOV_ACM  LIKE ZST_GL009_DADOS-VLR_MOV_ACM,
      VLR_CALC_ACM LIKE ZST_GL009_DADOS-VLR_CALC_ACM,
      VLR_HIST  LIKE ZST_GL009_DADOS-VLR_CALC.

INCLUDE ZGL009_INCLUDE.

*--------------------

DATA: BEGIN OF WA_LIST OCCURS 50.       " Internal table hierarchy
        INCLUDE STRUCTURE SNODETEXT.
DATA: END OF WA_LIST.

DATA: IT_LIST            LIKE STANDARD TABLE OF WA_LIST,
      F15                TYPE C.

*----------------------------------------------------------------------*
* Variávei globais
*----------------------------------------------------------------------*
DATA: VG_CHAVE           TYPE C LENGTH 14,
      VG_FM_NAME         TYPE RS38L_FNAM, "Nome da função smart form
      VG_MES             LIKE T247-LTX,
      VG_EMPRESA         LIKE T001-BUTXT.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
PARAMETER:
          R_IMP       RADIOBUTTON GROUP TP DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-S04.
PARAMETERS:
      B_NVL    AS CHECKBOX DEFAULT 'X',
      B_CNT    AS CHECKBOX DEFAULT 'X',
      B_OBJ    AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK B4.
PARAMETERS:
          R_ANA       RADIOBUTTON GROUP TP.
SELECTION-SCREEN END   OF BLOCK B2.


SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-S01.
PARAMETERS:
           P_BUKRS       LIKE ZGL005_DRE_DADOS-BUKRS OBLIGATORY,
           P_VERSN       LIKE ZGL005_DRE_DADOS-VERSN MATCHCODE OBJECT ZVERSN_DRE OBLIGATORY,
           P_MONAT       LIKE ZGL005_DRE_DADOS-MONAT OBLIGATORY,
           P_GJAHR       LIKE ZGL005_DRE_DADOS-GJAHR OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK B0.

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  SET TITLEBAR 'TITULO'.

*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.


  DATA: VG_FUNCAO LIKE ZGL001_DRE_EST-FUNCAO.

  SELECT SINGLE FUNCAO INTO VG_FUNCAO
    FROM ZGL001_DRE_EST
   WHERE BUKRS EQ P_BUKRS
     AND VERSN EQ P_VERSN.

  IF VG_FUNCAO NE 'G'.
    IF VG_FUNCAO EQ 'F'.
      MESSAGE 'A Estrutura informada é para DRE Fiscal!' TYPE 'E'.
    ELSEIF VG_FUNCAO IS INITIAL.
      MESSAGE 'Estrutura informada não possue uma função definida! (ZGL002)' TYPE 'E'.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Event Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CLEAR VG_TITULO.

  SELECT SINGLE VSTXT INTO VG_TITULO
    FROM ZGL001_DRE_EST
   WHERE BUKRS EQ P_BUKRS
     AND VERSN EQ P_VERSN.

  CONCATENATE P_GJAHR P_MONAT '01' INTO DATAINI.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = DATAINI
    IMPORTING
      LAST_DAY_OF_MONTH = DATAFIM.

  CONCATENATE DATAINI+6(2) DATAINI+4(2) DATAINI(4) INTO TXDATAINI.
  CONCATENATE DATAFIM+6(2) DATAFIM+4(2) DATAFIM(4) INTO TXDATAFIM.

* Gero chave conforme parâmetro
  CONCATENATE P_BUKRS
              P_VERSN
              P_MONAT
              P_GJAHR
              INTO VG_CHAVE.
* Busco dados para o relatorio.S
  PERFORM F_BUSCA_DADOS.
* Monto estrutura de dados para chamada do relatorio
  PERFORM F_MONTA_DADOS.
* Se selecionado tipo de apresentação IMPRESSÃO
  IF R_IMP IS NOT INITIAL.
* Totaliza niveis e contas sem OBJ de custo
    PERFORM F_TOTALIZA.
* Chamo relatorio Smart Form.
    PERFORM F_CHAMA_RELATORIO.
  ELSE.
* Chamo ALV tri list
*    perform f_dados_alv.
    CALL SCREEN 100.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       Busca dados para montar relatorio Smart Form
*----------------------------------------------------------------------*
FORM F_BUSCA_DADOS.
  SELECT *
    INTO TABLE IT_DRE_001
    FROM ZGL001_DRE_EST
   WHERE BUKRS EQ P_BUKRS
     AND VERSN EQ P_VERSN.

  SELECT *
    INTO TABLE IT_DRE_002
    FROM ZGL002_DRE_EST
   WHERE BUKRS EQ P_BUKRS
     AND VERSN EQ P_VERSN
   ORDER BY ORDNV.

  SELECT *
    FROM ZGL003_DRE_EST
    INTO TABLE IT_DRE_003
     FOR ALL ENTRIES IN IT_DRE_002
   WHERE BUKRS EQ IT_DRE_002-BUKRS
     AND VERSN EQ IT_DRE_002-VERSN
     AND NIVEL EQ IT_DRE_002-NIVEL.
*   order by saknr.

  SELECT SAKNR TXT50
    FROM SKAT
    INTO TABLE IT_CONTA
     FOR ALL ENTRIES IN IT_DRE_003
   WHERE SAKNR EQ IT_DRE_003-SAKNR
     AND KTOPL EQ '0050'
     AND SPRAS EQ 'PT'.

  SELECT *
    FROM ZGL004_DRE_EST
    INTO TABLE IT_DRE_004
     FOR ALL ENTRIES IN IT_DRE_003
   WHERE BUKRS EQ IT_DRE_003-BUKRS
     AND VERSN EQ IT_DRE_003-VERSN
     AND NIVEL EQ IT_DRE_003-NIVEL
     AND SAKNR EQ IT_DRE_003-SAKNR.

  SELECT KOSTL LTEXT
    FROM CSKT
    INTO TABLE IT_CUSTO
     FOR ALL ENTRIES IN IT_DRE_004
   WHERE KOSTL EQ IT_DRE_004-KOSTL.

  SELECT AUFNR KTEXT
    FROM AUFK
    INTO TABLE IT_ORDEM
     FOR ALL ENTRIES IN IT_DRE_004
   WHERE AUFNR EQ IT_DRE_004-AUFNR.

  SELECT PRCTR LTEXT
    FROM CEPCT
    INTO TABLE IT_LUCRO
     FOR ALL ENTRIES IN IT_DRE_004
   WHERE PRCTR EQ IT_DRE_004-PRCTR.

  SELECT SINGLE *
    FROM ZGL005_DRE_DADOS
    INTO WA_DRE_005
   WHERE CHAVE EQ VG_CHAVE.

  SELECT *
    FROM ZGL006_DRE_DADOS
    INTO TABLE IT_DRE_006
   WHERE CHAVE EQ VG_CHAVE.

  SELECT *
    FROM ZGL007_DRE_DADOS
    INTO TABLE IT_DRE_007
   WHERE CHAVE EQ VG_CHAVE.
ENDFORM.                    " F_BUSCA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS
*&---------------------------------------------------------------------*
*       Monto estrutura de dados para chamada do relatorio
*----------------------------------------------------------------------*
FORM F_MONTA_DADOS.
  DATA : TEXTO(80),
        WL_FIRST(1).

  SORT: IT_DRE_002 BY NIVEL,
        IT_DRE_003 BY NIVEL SAKNR,
        IT_DRE_004 BY NIVEL SAKNR KOSTL AUFNR PRCTR,
        IT_CONTA   BY SAKNR,
        IT_ORDEM   BY AUFNR,
        IT_LUCRO   BY PRCTR,
        IT_CUSTO   BY KOSTL,
        IT_DRE_006 BY NIVEL SAKNR KOSTL AUFNR PRCTR,
        IT_DRE_007 BY NIVEL SAKNR KOSTL AUFNR PRCTR.

  SELECT *
    INTO TABLE IT_DRE_008
    FROM ZGL008_DRE_TOTAL
   WHERE BUKRS EQ P_BUKRS
     AND VERSN EQ P_VERSN.

  LOOP AT IT_DRE_008 INTO WA_DRE_008.
    PERFORM GERA_DRE_NIVEL_TOTAL USING WA_DRE_008-NIVEL WA_DRE_008-EQUAC .
  ENDLOOP.
*---> 05/07/2023 - Migração S4 - DL
SORT IT_DRE_001 BY BUKRS BUKRS.
*<--- 05/07/2023 - Migração S4 - DL
  READ TABLE IT_DRE_001 INTO WA_DRE_001 WITH KEY BUKRS = P_BUKRS
                                                 VERSN = P_VERSN
                                                 BINARY SEARCH.

  LOOP AT IT_DRE_002 INTO WA_DRE_002.
    CLEAR: WA_DADOS.
    WA_DADOS-NIVEL  = WA_DRE_002-NIVEL.
    WA_DADOS-DESNVL = WA_DRE_002-DESNVL.
    WA_DADOS-ORDNV  = WA_DRE_002-ORDNV.
    WA_DADOS-TLEVEL = WA_DRE_002-TLEVEL.
    WA_DADOS-NVL1   = WA_DRE_002-ORDNV(2).
    WA_DADOS-NVL2   = WA_DRE_002-ORDNV+2(2).
    WA_DADOS-NVL3   = WA_DRE_002-ORDNV+4(2).
    WA_DADOS-NVL4   = WA_DRE_002-ORDNV+6(2).
    WA_DADOS-NVL5   = WA_DRE_002-ORDNV+8(2).
    WA_DADOS-NVL6   = WA_DRE_002-ORDNV+10(2).
    WA_DADOS-NVL7   = WA_DRE_002-ORDNV+12(2).
    WA_DADOS-NVL8   = WA_DRE_002-ORDNV+14(2).
    WA_DADOS-NVL9   = WA_DRE_002-ORDNV+16(2).
    WA_DADOS-NVL10  = WA_DRE_002-ORDNV+18(2).


    READ TABLE IT_DRE_003 INTO WA_DRE_003 WITH KEY NIVEL = WA_DRE_002-NIVEL
                                                   BINARY SEARCH.
    IF SY-SUBRC EQ 0. "Caso o tenha uma conta ligada a este nivel
      APPEND WA_DADOS TO IT_DADOS.
      CLEAR: WA_DADOS.
      WA_DADOS-NIVEL  = WA_DRE_002-NIVEL.
      WA_DADOS-DESNVL = WA_DRE_002-DESNVL.
      WA_DADOS-ORDNV  = WA_DRE_002-ORDNV.
      WA_DADOS-TLEVEL = WA_DRE_002-TLEVEL.
      WA_DADOS-NVL1   = WA_DRE_002-ORDNV(2).
      WA_DADOS-NVL2   = WA_DRE_002-ORDNV+2(2).
      WA_DADOS-NVL3   = WA_DRE_002-ORDNV+4(2).
      WA_DADOS-NVL4   = WA_DRE_002-ORDNV+6(2).
      WA_DADOS-NVL5   = WA_DRE_002-ORDNV+8(2).
      WA_DADOS-NVL6   = WA_DRE_002-ORDNV+10(2).
      WA_DADOS-NVL7   = WA_DRE_002-ORDNV+12(2).
      WA_DADOS-NVL8   = WA_DRE_002-ORDNV+14(2).
      WA_DADOS-NVL9   = WA_DRE_002-ORDNV+16(2).
      WA_DADOS-NVL10  = WA_DRE_002-ORDNV+18(2).
      WL_FIRST = 'X'.
      LOOP AT IT_DRE_003 INTO WA_DRE_003 WHERE NIVEL = WA_DRE_002-NIVEL.
        WA_DADOS-SAKNR = WA_DRE_003-SAKNR.
        READ TABLE IT_CONTA INTO WA_CONTA WITH KEY SAKNR = WA_DRE_003-SAKNR
                                                   BINARY SEARCH.
        WA_DADOS-TXT50 = WA_CONTA-TXT50.
        READ TABLE IT_DRE_004 INTO WA_DRE_004 WITH KEY NIVEL = WA_DRE_003-NIVEL
                                                       SAKNR = WA_DRE_003-SAKNR
                                                       BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          CLEAR: WA_DADOS-VLR_REA, WA_DADOS-VLR_ACM, WA_DADOS-QTD_TON,
                 WA_DADOS-ANA_ACM, WA_DADOS-QTD_ACM, WA_DADOS-ANA_VLR,
                 WA_DADOS-VLR_MOV, WA_DADOS-VLR_CALC,
                 WA_DADOS-VLR_MOV_ACM, WA_DADOS-VLR_CALC_ACM.
          APPEND WA_DADOS TO IT_DADOS.
          CLEAR WA_DADOS.
          WA_DADOS-NIVEL  = WA_DRE_002-NIVEL.
          WA_DADOS-DESNVL = WA_DRE_002-DESNVL.
          WA_DADOS-ORDNV  = WA_DRE_002-ORDNV.
          WA_DADOS-TLEVEL  = WA_DRE_002-TLEVEL + 2.
          WA_DADOS-NVL1   = WA_DRE_002-ORDNV(2).
          WA_DADOS-NVL2   = WA_DRE_002-ORDNV+2(2).
          WA_DADOS-NVL3   = WA_DRE_002-ORDNV+4(2).
          WA_DADOS-NVL4   = WA_DRE_002-ORDNV+6(2).
          WA_DADOS-NVL5   = WA_DRE_002-ORDNV+8(2).
          WA_DADOS-NVL6   = WA_DRE_002-ORDNV+10(2).
          WA_DADOS-NVL7   = WA_DRE_002-ORDNV+12(2).
          WA_DADOS-NVL8   = WA_DRE_002-ORDNV+14(2).
          WA_DADOS-NVL9   = WA_DRE_002-ORDNV+16(2).
          WA_DADOS-NVL10  = WA_DRE_002-ORDNV+18(2).

          READ TABLE IT_CONTA INTO WA_CONTA WITH KEY SAKNR = WA_DRE_003-SAKNR
                                                     BINARY SEARCH.
          WA_DADOS-TXT50 = WA_CONTA-TXT50.
          WA_DADOS-SAKNR = WA_DRE_003-SAKNR.
          LOOP AT IT_DRE_004 INTO WA_DRE_004 WHERE NIVEL = WA_DRE_003-NIVEL
                                               AND SAKNR = WA_DRE_003-SAKNR.

            CLEAR: WA_DADOS-VLR_REA, WA_DADOS-VLR_ACM, WA_DADOS-QTD_TON,
                   WA_DADOS-ANA_ACM, WA_DADOS-QTD_ACM, WA_DADOS-ANA_VLR,
                   WA_DADOS-VLR_MOV, WA_DADOS-VLR_CALC,
                   WA_DADOS-VLR_MOV_ACM, WA_DADOS-VLR_CALC_ACM.

            IF WA_DRE_004-KOSTL IS NOT INITIAL. "Centro de Custo
              WA_DADOS-KOSTL = WA_DRE_004-KOSTL.
              READ TABLE IT_CUSTO INTO WA_CUSTO WITH KEY KOSTL = WA_DRE_004-KOSTL
                                                         BINARY SEARCH.

              CONCATENATE WA_CUSTO-KOSTL '-' WA_CUSTO-LTEXT INTO TEXTO.
              WA_DADOS-LTEXT = TEXTO(40).

              CLEAR WA_DRE_006.
              READ TABLE IT_DRE_006 INTO WA_DRE_006 WITH KEY NIVEL = WA_DRE_004-NIVEL
                                                             SAKNR = WA_DRE_004-SAKNR
                                                             KOSTL = WA_DRE_004-KOSTL
                                                             BINARY SEARCH.
              IF SY-SUBRC = 0.
                WA_DADOS-QTD_TON = WA_DRE_006-QTD_TON.
*---> 10/06/2023 - Migração S4 - JS
*                WA_DADOS-VLR_REA = WA_DRE_006-VLR_REA.
*                WA_DADOS-VLR_MOV = WA_DRE_006-VLR_MOV_GER.
*                WA_DADOS-VLR_CALC = WA_DRE_006-VLR_CALC_AJ.
                 WA_DADOS-VLR_REA  = CONV #( WA_DRE_006-VLR_REA ).
                 WA_DADOS-VLR_MOV  = CONV #( WA_DRE_006-VLR_MOV_GER ).
                 WA_DADOS-VLR_CALC = CONV #( WA_DRE_006-VLR_CALC_AJ ).
*<--- 10/06/2023 - Migração S4 - JS
                IF WA_DRE_006-QTD_TON GT 0.
                  WA_DADOS-ANA_VLR = WA_DRE_006-VLR_CALC_AJ / WA_DRE_006-QTD_TON.
                ENDIF.
              ENDIF.
              CLEAR WA_DRE_007.
              READ TABLE IT_DRE_007 INTO WA_DRE_007 WITH KEY NIVEL = WA_DRE_004-NIVEL
                                                             SAKNR = WA_DRE_004-SAKNR
                                                             KOSTL = WA_DRE_004-KOSTL
                                                             BINARY SEARCH.
              IF SY-SUBRC = 0.
                WA_DADOS-QTD_ACM = WA_DRE_007-QTD_TON.

*---> 10/06/2023 - Migração S4 - JS
*                WA_DADOS-VLR_ACM =      WA_DRE_007-VLR_REA.
*                WA_DADOS-VLR_MOV_ACM  = WA_DRE_007-VLR_MOV_GER.
*                WA_DADOS-VLR_CALC_ACM = WA_DRE_007-VLR_CALC_AJ.
                WA_DADOS-VLR_ACM      = CONV #( WA_DRE_007-VLR_REA ).
                WA_DADOS-VLR_MOV_ACM  = CONV #( WA_DRE_007-VLR_MOV_GER ).
                WA_DADOS-VLR_CALC_ACM = CONV #( WA_DRE_007-VLR_CALC_AJ ).
*<--- 10/06/2023 - Migração S4 - JS
                IF WA_DRE_007-QTD_TON GT 0.
                  WA_DADOS-ANA_ACM = WA_DRE_007-VLR_CALC_AJ / WA_DRE_007-QTD_TON.
                ENDIF.
              ENDIF.
              IF ( WA_DADOS-VLR_REA NE 0 ) OR ( WA_DADOS-VLR_ACM NE 0 )
              OR ( WA_DADOS-VLR_MOV NE 0 ) OR ( WA_DADOS-VLR_MOV_ACM NE 0 )
              OR ( WA_DADOS-VLR_CALC NE 0 ) OR ( WA_DADOS-VLR_CALC_ACM NE 0 ).
                APPEND WA_DADOS TO IT_DADOS.
              ENDIF.
            ELSEIF WA_DRE_004-AUFNR IS NOT INITIAL. "Ordem interna
              WA_DADOS-AUFNR = WA_DRE_004-AUFNR.
              READ TABLE IT_ORDEM INTO WA_ORDEM WITH KEY AUFNR = WA_DRE_004-AUFNR
                                                         BINARY SEARCH.
              CONCATENATE WA_ORDEM-AUFNR '-' WA_ORDEM-KTEXT INTO TEXTO.
              WA_DADOS-LTEXT = TEXTO(40).
              CLEAR WA_DRE_006.
              READ TABLE IT_DRE_006 INTO WA_DRE_006 WITH KEY NIVEL = WA_DRE_004-NIVEL
                                                             SAKNR = WA_DRE_004-SAKNR
                                                             AUFNR = WA_DRE_004-AUFNR
                                                             BINARY SEARCH.
              IF SY-SUBRC = 0.
                WA_DADOS-QTD_TON = WA_DRE_006-QTD_TON.
*---> 10/06/2023 - Migração S4 - JS
*                WA_DADOS-VLR_REA  = WA_DRE_006-VLR_REA.
*                WA_DADOS-VLR_MOV  = WA_DRE_006-VLR_MOV_GER.
*                WA_DADOS-VLR_CALC = WA_DRE_006-VLR_CALC_AJ.
                WA_DADOS-VLR_REA   = CONV #( WA_DRE_006-VLR_REA ).
                WA_DADOS-VLR_MOV   = CONV #( WA_DRE_006-VLR_MOV_GER ).
                WA_DADOS-VLR_CALC  = CONV #( WA_DRE_006-VLR_CALC_AJ ).
*<--- 10/06/2023 - Migração S4 - JS

                IF WA_DRE_006-QTD_TON GT 0.
                  WA_DADOS-ANA_VLR = WA_DRE_006-VLR_CALC_AJ / WA_DRE_006-QTD_TON.
                ENDIF.
              ENDIF.
              CLEAR WA_DRE_007.
              READ TABLE IT_DRE_007 INTO WA_DRE_007 WITH KEY NIVEL = WA_DRE_004-NIVEL
                                                             SAKNR = WA_DRE_004-SAKNR
                                                             AUFNR = WA_DRE_004-AUFNR
                                                             BINARY SEARCH.
              IF SY-SUBRC = 0.
                WA_DADOS-QTD_ACM = WA_DRE_007-QTD_TON.
*---> 10/06/2023 - Migração S4 - JS
*                WA_DADOS-VLR_ACM      = WA_DRE_007-VLR_REA.
*                WA_DADOS-VLR_MOV_ACM  = WA_DRE_007-VLR_MOV_GER.
*                WA_DADOS-VLR_CALC_ACM = WA_DRE_007-VLR_CALC_AJ.
               WA_DADOS-VLR_ACM       = CONV #( WA_DRE_007-VLR_REA ).
               WA_DADOS-VLR_MOV_ACM   = CONV #( WA_DRE_007-VLR_MOV_GER ).
               WA_DADOS-VLR_CALC_ACM  = CONV #( WA_DRE_007-VLR_CALC_AJ ).
*<--- 10/06/2023 - Migração S4 - JS

                IF WA_DRE_007-QTD_TON GT 0.
                  WA_DADOS-ANA_ACM = WA_DRE_007-VLR_CALC_AJ / WA_DRE_007-QTD_TON.
                ENDIF.
              ENDIF.
              IF ( WA_DADOS-VLR_REA NE 0 ) OR ( WA_DADOS-VLR_ACM NE 0 )
              OR ( WA_DADOS-VLR_MOV NE 0 ) OR ( WA_DADOS-VLR_MOV_ACM NE 0 )
              OR ( WA_DADOS-VLR_CALC NE 0 ) OR ( WA_DADOS-VLR_CALC_ACM NE 0 ).
                APPEND WA_DADOS TO IT_DADOS.
              ENDIF.
            ELSEIF WA_DRE_004-PRCTR IS NOT INITIAL. "Centro de Lucro
              WA_DADOS-PRCTR = WA_DRE_004-PRCTR.
              READ TABLE IT_LUCRO INTO WA_LUCRO WITH KEY PRCTR = WA_DRE_004-PRCTR
                                                         BINARY SEARCH.
              CONCATENATE WA_LUCRO-PRCTR '-' WA_LUCRO-LTEXT INTO TEXTO.
              WA_DADOS-LTEXT = TEXTO(40).
              CLEAR WA_DRE_006.
              READ TABLE IT_DRE_006 INTO WA_DRE_006 WITH KEY NIVEL = WA_DRE_004-NIVEL
                                                             SAKNR = WA_DRE_004-SAKNR
                                                             PRCTR = WA_DRE_004-PRCTR
                                                             BINARY SEARCH.
              IF SY-SUBRC = 0.
                WA_DADOS-QTD_TON = WA_DRE_006-QTD_TON.
*---> 10/06/2023 - Migração S4 - JS
*                WA_DADOS-VLR_REA  = WA_DRE_006-VLR_REA.
*                WA_DADOS-VLR_MOV  = WA_DRE_006-VLR_MOV_GER.
*                WA_DADOS-VLR_CALC = WA_DRE_006-VLR_CALC_AJ.
                WA_DADOS-VLR_REA  = CONV #( WA_DRE_006-VLR_REA ).
                WA_DADOS-VLR_MOV  = CONV #( WA_DRE_006-VLR_MOV_GER ).
                WA_DADOS-VLR_CALC = CONV #( WA_DRE_006-VLR_CALC_AJ ).
*<--- 10/06/2023 - Migração S4 - JS

                IF WA_DRE_006-QTD_TON GT 0.
                  WA_DADOS-ANA_VLR = WA_DRE_006-VLR_CALC_AJ / WA_DRE_006-QTD_TON.
                ENDIF.
              ENDIF.
              CLEAR WA_DRE_007.
              READ TABLE IT_DRE_007 INTO WA_DRE_007 WITH KEY NIVEL = WA_DRE_004-NIVEL
                                                             SAKNR = WA_DRE_004-SAKNR
                                                             PRCTR = WA_DRE_004-PRCTR
                                                             BINARY SEARCH.
              IF SY-SUBRC = 0.
                WA_DADOS-QTD_ACM = WA_DRE_007-QTD_TON.
*---> 10/06/2023 - Migração S4 - JS
*                WA_DADOS-VLR_REA  = WA_DRE_006-VLR_REA.
*                WA_DADOS-VLR_MOV  = WA_DRE_006-VLR_MOV_GER.
*                WA_DADOS-VLR_CALC = WA_DRE_006-VLR_CALC_AJ.
                WA_DADOS-VLR_REA  = CONV #( WA_DRE_006-VLR_REA ).
                WA_DADOS-VLR_MOV  = CONV #( WA_DRE_006-VLR_MOV_GER ).
                WA_DADOS-VLR_CALC = CONV #( WA_DRE_006-VLR_CALC_AJ ).
*<--- 10/06/2023 - Migração S4 - JS
                IF WA_DRE_007-QTD_TON GT 0.
                  WA_DADOS-ANA_ACM = WA_DRE_007-VLR_CALC_AJ / WA_DRE_007-QTD_TON.
                ENDIF.
              ENDIF.
              IF ( WA_DADOS-VLR_REA NE 0 ) OR ( WA_DADOS-VLR_ACM NE 0 )
              OR ( WA_DADOS-VLR_MOV NE 0 ) OR ( WA_DADOS-VLR_MOV_ACM NE 0 )
              OR ( WA_DADOS-VLR_CALC NE 0 ) OR ( WA_DADOS-VLR_CALC_ACM NE 0 ).
                APPEND WA_DADOS TO IT_DADOS.
              ENDIF.
            ELSE.
              READ TABLE IT_DRE_006 INTO WA_DRE_006 WITH KEY NIVEL = WA_DRE_004-NIVEL
                                                             SAKNR = WA_DRE_004-SAKNR
                                                             BINARY SEARCH.
              IF SY-SUBRC = 0.
                WA_DADOS-QTD_TON = WA_DRE_006-QTD_TON.
*---> 10/06/2023 - Migração S4 - JS
*                WA_DADOS-VLR_REA  = WA_DRE_006-VLR_REA.
*                WA_DADOS-VLR_MOV  = WA_DRE_006-VLR_MOV_GER.
*                WA_DADOS-VLR_CALC = WA_DRE_006-VLR_CALC_AJ.
                WA_DADOS-VLR_REA  = CONV #( WA_DRE_006-VLR_REA ).
                WA_DADOS-VLR_MOV  = CONV #( WA_DRE_006-VLR_MOV_GER ).
                WA_DADOS-VLR_CALC = CONV #( WA_DRE_006-VLR_CALC_AJ ).
*<--- 10/06/2023 - Migração S4 - JS
                IF WA_DRE_006-QTD_TON GT 0.
                  WA_DADOS-ANA_VLR = WA_DRE_006-VLR_CALC_AJ / WA_DRE_006-QTD_TON.
                ENDIF.
              ENDIF.
              CLEAR WA_DRE_007.
              READ TABLE IT_DRE_007 INTO WA_DRE_007 WITH KEY NIVEL = WA_DRE_004-NIVEL
                                                             SAKNR = WA_DRE_004-SAKNR
                                                             BINARY SEARCH.
              IF SY-SUBRC = 0.
                WA_DADOS-QTD_ACM = WA_DRE_007-QTD_TON.
*---> 10/06/2023 - Migração S4 - JS
*                WA_DADOS-VLR_REA  = WA_DRE_006-VLR_REA.
*                WA_DADOS-VLR_MOV  = WA_DRE_006-VLR_MOV_GER.
*                WA_DADOS-VLR_CALC = WA_DRE_006-VLR_CALC_AJ.
                WA_DADOS-VLR_REA  = CONV #( WA_DRE_006-VLR_REA ).
                WA_DADOS-VLR_MOV  = CONV #( WA_DRE_006-VLR_MOV_GER ).
                WA_DADOS-VLR_CALC = CONV #( WA_DRE_006-VLR_CALC_AJ ).
*<--- 10/06/2023 - Migração S4 - JS
                IF WA_DRE_007-QTD_TON GT 0.
                  WA_DADOS-ANA_ACM = WA_DRE_007-VLR_CALC_AJ / WA_DRE_007-QTD_TON.
                ENDIF.
              ENDIF.
              IF ( WA_DADOS-VLR_REA NE 0 ) OR ( WA_DADOS-VLR_ACM NE 0 )
              OR ( WA_DADOS-VLR_MOV NE 0 ) OR ( WA_DADOS-VLR_MOV_ACM NE 0 )
              OR ( WA_DADOS-VLR_CALC NE 0 ) OR ( WA_DADOS-VLR_CALC_ACM NE 0 ).
                MOVE WA_CONTA-TXT50 TO WA_DADOS-LTEXT.
                "concatenate 'c. contabil:' wa_dados-saknr into wa_dados-ltext SEPARATED BY space.
                APPEND WA_DADOS TO IT_DADOS.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          CLEAR: WA_DADOS-VLR_REA, WA_DADOS-VLR_ACM, WA_DADOS-QTD_TON,
                 WA_DADOS-ANA_ACM, WA_DADOS-QTD_ACM, WA_DADOS-ANA_VLR,
                 WA_DADOS-VLR_MOV, WA_DADOS-VLR_CALC,
                 WA_DADOS-VLR_MOV_ACM, WA_DADOS-VLR_CALC_ACM.

          CLEAR WA_DRE_006.
          READ TABLE IT_DRE_006 INTO WA_DRE_006 WITH KEY NIVEL = WA_DRE_003-NIVEL
                                                         SAKNR = WA_DRE_003-SAKNR
                                                         BINARY SEARCH.
          IF SY-SUBRC = 0.

            WA_DADOS-QTD_TON = WA_DRE_006-QTD_TON.
*---> 10/06/2023 - Migração S4 - JS
*                WA_DADOS-VLR_REA  = WA_DRE_006-VLR_REA.
*                WA_DADOS-VLR_MOV  = WA_DRE_006-VLR_MOV_GER.
*                WA_DADOS-VLR_CALC = WA_DRE_006-VLR_CALC_AJ.
                WA_DADOS-VLR_REA  = CONV #( WA_DRE_006-VLR_REA ).
                WA_DADOS-VLR_MOV  = CONV #( WA_DRE_006-VLR_MOV_GER ).
                WA_DADOS-VLR_CALC = CONV #( WA_DRE_006-VLR_CALC_AJ ).
*<--- 10/06/2023 - Migração S4 - JS
            IF WA_DRE_006-QTD_TON GT 0.
              WA_DADOS-ANA_VLR = WA_DRE_006-VLR_CALC_AJ / WA_DRE_006-QTD_TON.
            ENDIF.

          ENDIF.
          CLEAR WA_DRE_007.
          READ TABLE IT_DRE_007 INTO WA_DRE_007 WITH KEY NIVEL = WA_DRE_003-NIVEL
                                                         SAKNR = WA_DRE_003-SAKNR
                                                         BINARY SEARCH.
          IF SY-SUBRC = 0.
            WA_DADOS-QTD_ACM = WA_DRE_007-QTD_TON.
*---> 10/06/2023 - Migração S4 - JS
*                WA_DADOS-VLR_REA  = WA_DRE_006-VLR_REA.
*                WA_DADOS-VLR_MOV  = WA_DRE_006-VLR_MOV_GER.
*                WA_DADOS-VLR_CALC = WA_DRE_006-VLR_CALC_AJ.
                WA_DADOS-VLR_REA  = CONV #( WA_DRE_006-VLR_REA ).
                WA_DADOS-VLR_MOV  = CONV #( WA_DRE_006-VLR_MOV_GER ).
                WA_DADOS-VLR_CALC = CONV #( WA_DRE_006-VLR_CALC_AJ ).
*<--- 10/06/2023 - Migração S4 - JS
            IF WA_DRE_007-QTD_TON GT 0.
              WA_DADOS-ANA_ACM = WA_DRE_007-VLR_CALC_AJ / WA_DRE_007-QTD_TON.
            ENDIF.
          ENDIF.
          IF ( WA_DADOS-VLR_REA NE 0 ) OR ( WA_DADOS-VLR_ACM NE 0 )
          OR ( WA_DADOS-VLR_MOV NE 0 ) OR ( WA_DADOS-VLR_MOV_ACM NE 0 )
          OR ( WA_DADOS-VLR_CALC NE 0 ) OR ( WA_DADOS-VLR_CALC_ACM NE 0 ).
            "concatenate 'c. contabil:' wa_dados-saknr into wa_dados-ltext SEPARATED BY space.
            MOVE WA_CONTA-TXT50 TO WA_DADOS-LTEXT.
            APPEND WA_DADOS TO IT_DADOS.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
*     Caso o tenha uma conta ligada a este nivel
      APPEND WA_DADOS TO IT_DADOS.
    ENDIF.

  ENDLOOP.

  DATA: VG_NIVEL(1).


  LOOP AT IT_DRE_002 INTO WA_DRE_002.

    CLEAR: VLR_ACM,
           VLR_REA,
           IT_DADOS2,
           VG_NIVEL,
           VLR_MOV,
           VLR_CALC,
           VLR_MOV_ACM,
           VLR_CALC_ACM,
           VLR_HIST.

    LOOP AT IT_DRE_008_NIVEL INTO WA_DRE_008_NIVEL WHERE NIVEL_TOTAL EQ WA_DRE_002-NIVEL.
      LOOP AT IT_DADOS INTO WA_DADOS2.
        PERFORM VERIFICA_NIVEL USING WA_DRE_008_NIVEL-NIVEL WA_DADOS2-NIVEL VG_NIVEL.
        IF VG_NIVEL EQ 'X'.
          IF WA_DRE_008_NIVEL-SINAL EQ '-'.
            VLR_ACM  = VLR_ACM - WA_DADOS2-VLR_ACM.
            VLR_REA  = VLR_REA - WA_DADOS2-VLR_REA.
            VLR_MOV  = VLR_MOV  - WA_DADOS2-VLR_MOV.
            VLR_CALC = VLR_CALC - WA_DADOS2-VLR_CALC.
            VLR_MOV_ACM = VLR_MOV_ACM - WA_DADOS2-VLR_MOV_ACM.
            VLR_CALC_ACM = VLR_CALC_ACM - WA_DADOS2-VLR_CALC_ACM.
          ELSEIF WA_DRE_008_NIVEL-SINAL EQ '+'.
            VLR_ACM = VLR_ACM + WA_DADOS2-VLR_ACM.
            VLR_REA = VLR_REA + WA_DADOS2-VLR_REA.
            VLR_MOV  = VLR_MOV  + WA_DADOS2-VLR_MOV.
            VLR_CALC = VLR_CALC + WA_DADOS2-VLR_CALC.
            VLR_MOV_ACM = VLR_MOV_ACM + WA_DADOS2-VLR_MOV_ACM.
            VLR_CALC_ACM = VLR_CALC_ACM + WA_DADOS2-VLR_CALC_ACM.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF ( VLR_ACM IS NOT INITIAL ) OR ( VLR_REA IS NOT INITIAL )
    OR ( VLR_MOV IS NOT INITIAL ) OR ( VLR_CALC IS NOT INITIAL )
    OR ( VLR_HIST IS NOT INITIAL ) OR ( VLR_MOV_ACM IS NOT INITIAL )
    OR ( VLR_CALC_ACM IS NOT INITIAL ) .
      READ TABLE IT_DADOS INTO WA_DADOS WITH KEY NIVEL = WA_DRE_002-NIVEL.
      IF SY-SUBRC EQ 0.
        WA_DADOS-VLR_ACM = VLR_ACM.
        WA_DADOS-VLR_REA = VLR_REA.
        WA_DADOS-VLR_MOV = VLR_MOV.
        WA_DADOS-VLR_CALC = VLR_CALC.
        WA_DADOS-VLR_MOV_ACM = VLR_MOV_ACM.
        WA_DADOS-VLR_CALC_ACM = VLR_CALC_ACM.
        MODIFY IT_DADOS INDEX SY-TABIX
          FROM WA_DADOS TRANSPORTING VLR_REA VLR_ACM VLR_MOV VLR_CALC VLR_MOV_ACM VLR_CALC_ACM.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_MONTA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_CHAMA_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CHAMA_RELATORIO .
  DATA: VL_MES           LIKE T247-LTX,
        VL_EMPRESA       LIKE T001-BUTXT.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME = 'ZGL009_DRE_FORM'
    IMPORTING
      FM_NAME  = VG_FM_NAME
    EXCEPTIONS
      OTHERS   = 3.

  CHECK SY-SUBRC IS INITIAL.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      LANGUAGE    = SY-LANGU
    TABLES
      MONTH_NAMES = IT_MES.

  SORT: IT_MES   BY MNR,
        IT_DADOS BY ORDNV TXT50 LTEXT.

  READ TABLE IT_MES INTO WA_MES WITH KEY MNR = P_MONAT.

  SELECT SINGLE BUTXT
    FROM T001
    INTO VL_EMPRESA
   WHERE BUKRS EQ P_BUKRS.

  CALL FUNCTION VG_FM_NAME
    EXPORTING
      MES     = WA_MES-LTX
      ANO     = P_GJAHR
      EMPRESA = VL_EMPRESA
      MOEDA   = WA_DRE_001-WAERS
      USUARIO = WA_DRE_005-UNAME
      DATA    = WA_DRE_005-DATUM
      HORA    = WA_DRE_005-UZEIT
      TITULO  = VG_TITULO
      NIVEL   = B_NVL
      CONTA   = B_CNT
      OBJETO  = B_OBJ
    TABLES
      DADOS   = IT_DADOS.


ENDFORM.                    " F_CHAMA_RELATORIO
*&---------------------------------------------------------------------*
*&      Form  F_TOTALIZA
*&---------------------------------------------------------------------*
*       Totalizar niveis e contas com dependentes
*----------------------------------------------------------------------*
FORM F_TOTALIZA .
  DATA: VL_ORD             LIKE ZGL002_DRE_EST-ORDNV,
        VL_IDX             LIKE SY-TABIX.
  SORT: IT_DADOS BY ORDNV TXT50 LTEXT.
  CLEAR VL_IDX.

  LOOP AT IT_DADOS INTO WA_DADOS.
    IF WA_DADOS-SAKNR IS INITIAL.
      VL_IDX = SY-TABIX.
      CONCATENATE WA_DADOS-ORDNV
                  '%' INTO VL_ORD.
      SELECT *
        FROM ZGL002_DRE_EST
        INTO TABLE IT_DRE_002
       WHERE BUKRS EQ P_BUKRS
         AND VERSN EQ P_VERSN
         AND ORDNV LIKE VL_ORD.

      SELECT *
        FROM ZGL006_DRE_DADOS
        INTO TABLE IT_DRE_006
         FOR ALL ENTRIES IN IT_DRE_002
       WHERE CHAVE EQ VG_CHAVE
         AND NIVEL EQ IT_DRE_002-NIVEL.

      SELECT *
        FROM ZGL007_DRE_DADOS
        INTO TABLE IT_DRE_007
         FOR ALL ENTRIES IN IT_DRE_002
       WHERE CHAVE EQ VG_CHAVE
         AND NIVEL EQ IT_DRE_002-NIVEL.

      LOOP AT IT_DRE_006 INTO WA_DRE_006.
        WA_DADOS-VLR_REA = WA_DADOS-VLR_REA + WA_DRE_006-VLR_REA.
        WA_DADOS-VLR_MOV = WA_DADOS-VLR_MOV + WA_DRE_006-VLR_MOV_GER.
        WA_DADOS-VLR_CALC = WA_DADOS-VLR_CALC + WA_DRE_006-VLR_CALC_AJ.
        WA_DADOS-QTD_TON = WA_DADOS-QTD_TON + WA_DRE_006-QTD_TON.
      ENDLOOP.
      IF WA_DADOS-QTD_TON GT 0.
        WA_DADOS-ANA_VLR = WA_DADOS-VLR_CALC / WA_DADOS-QTD_TON.
      ENDIF.

      LOOP AT IT_DRE_007 INTO WA_DRE_007.
        WA_DADOS-VLR_ACM = WA_DADOS-VLR_ACM + WA_DRE_007-VLR_REA.
        WA_DADOS-VLR_MOV_ACM = WA_DADOS-VLR_MOV_ACM + WA_DRE_007-VLR_MOV_GER.
        WA_DADOS-VLR_CALC_ACM = WA_DADOS-VLR_CALC_ACM + WA_DRE_007-VLR_CALC_AJ.
        WA_DADOS-QTD_ACM = WA_DADOS-QTD_TON + WA_DRE_007-QTD_TON.
      ENDLOOP.
      IF WA_DADOS-QTD_ACM GT 0.
        WA_DADOS-ANA_ACM = WA_DADOS-VLR_CALC_ACM / WA_DADOS-QTD_ACM.
      ENDIF.

      MODIFY IT_DADOS INDEX VL_IDX
        FROM WA_DADOS
      TRANSPORTING VLR_REA VLR_MOV VLR_CALC QTD_TON VLR_ACM VLR_MOV_ACM VLR_CALC_ACM QTD_ACM.

    ENDIF.
  ENDLOOP.

  CLEAR: IT_DADOS2.
  LOOP AT IT_DADOS INTO WA_DADOS.

    IF ( NOT WA_DADOS-SAKNR IS INITIAL ).
      IF ( WA_DADOS-VLR_REA NE 0 ) OR ( WA_DADOS-VLR_MOV NE 0 ) OR ( WA_DADOS-VLR_CALC NE 0 )
      OR  ( WA_DADOS-QTD_TON NE 0 )
      OR ( WA_DADOS-VLR_ACM NE 0 ) OR ( WA_DADOS-VLR_MOV_ACM NE 0 ) OR ( WA_DADOS-VLR_CALC_ACM NE 0 )
      OR ( WA_DADOS-QTD_ACM NE 0 ).
        APPEND WA_DADOS TO IT_DADOS2.
      ENDIF.
    ELSE.
      APPEND WA_DADOS TO IT_DADOS2.
    ENDIF.
  ENDLOOP.

  CLEAR: IT_DADOS.

  IT_DADOS = IT_DADOS2.

ENDFORM.                    " F_TOTALIZA

*&---------------------------------------------------------------------*
*&      Form  GERA_DRE_NIVEL_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GERA_DRE_NIVEL_TOTAL  USING  P_WA_DRE_008_NIVEL P_WA_DRE_008_EQUAC.

  DATA: PC TYPE I,
        PX TYPE I,
        PM TYPE I,
        TC TYPE C,
        SN TYPE C,
        NV(30),
        EQ LIKE ZGL008_DRE_TOTAL-EQUAC.

  PC = 1.
  SN = 'N'.
  EQ = P_WA_DRE_008_EQUAC.
  CLEAR: NV.

  CALL FUNCTION 'STRING_LENGTH'
    EXPORTING
      STRING = EQ
    IMPORTING
      LENGTH = PM.

  WHILE PC LE PM.

    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        STRING = EQ
      IMPORTING
        LENGTH = PX.

    IF PX NE 1.
      CALL FUNCTION 'STRING_SPLIT_AT_POSITION'
        EXPORTING
          STRING  = EQ
          POS     = 1
        IMPORTING
          STRING1 = TC
          STRING2 = EQ.
    ELSE.
      TC = EQ.
    ENDIF.

    IF TC IS NOT INITIAL.

      WA_DRE_008_NIVEL-NIVEL_TOTAL = WA_DRE_008-NIVEL.

      IF ( TC EQ '-' ) OR ( TC EQ '+' ).
        IF SN = 'S'.
          WA_DRE_008_NIVEL-NIVEL = NV.
          CLEAR: NV.
          APPEND WA_DRE_008_NIVEL TO IT_DRE_008_NIVEL.
        ENDIF.

        CLEAR: WA_DRE_008_NIVEL.
        WA_DRE_008_NIVEL-SINAL = TC.
        SN = 'S'.
      ELSE.
        IF NV IS NOT INITIAL.
          CALL FUNCTION 'STRING_CONCATENATE'
            EXPORTING
              STRING1 = NV
              STRING2 = TC
            IMPORTING
              STRING  = NV.
        ELSE.
          NV = TC.
        ENDIF.
        IF PC EQ PM.
          WA_DRE_008_NIVEL-NIVEL = NV.
          CLEAR: NV.
          APPEND WA_DRE_008_NIVEL TO IT_DRE_008_NIVEL.
        ENDIF.
      ENDIF.
    ENDIF.
    PC = PC + 1.
  ENDWHILE.

ENDFORM.                    " GERA_DRE_NIVEL_TOTAL

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_NIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VERIFICA_NIVEL  USING    P_WA_DRE_008_NIVEL_NIVEL
                              P_WA_DADOS2_NIVEL
                              P_VG_NIVEL.
  DATA: PC  TYPE I,
        PX  TYPE I,
        TC  TYPE C,
        VG1 TYPE C,
        VG2 TYPE C,
        NV1(30),
        NV1_PX TYPE I,
        NV2(30),
        NV2_PX TYPE I,
        NV_TEXT(30),
        W_WA_DRE_008_NIVEL_NIVEL(30),
        W_WA_DADOS2_NIVEL(30).

  CLEAR: P_VG_NIVEL,
         NV1,
         NV2.

  W_WA_DRE_008_NIVEL_NIVEL = P_WA_DRE_008_NIVEL_NIVEL.
  PC = 1.
  WHILE ( PC LE 30 ) AND ( W_WA_DRE_008_NIVEL_NIVEL IS NOT INITIAL ).

    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        STRING = W_WA_DRE_008_NIVEL_NIVEL
      IMPORTING
        LENGTH = PX.

    IF PX NE 1.
      CALL FUNCTION 'STRING_SPLIT_AT_POSITION'
        EXPORTING
          STRING  = W_WA_DRE_008_NIVEL_NIVEL
          POS     = 1
        IMPORTING
          STRING1 = TC
          STRING2 = W_WA_DRE_008_NIVEL_NIVEL.
    ELSE.
      TC = W_WA_DRE_008_NIVEL_NIVEL.
      CLEAR: W_WA_DRE_008_NIVEL_NIVEL.
    ENDIF.

    IF ( TC NE '0' ) OR ( NV1 IS NOT INITIAL ).
      IF NV1 IS NOT INITIAL.
        CALL FUNCTION 'STRING_CONCATENATE'
          EXPORTING
            STRING1 = NV1
            STRING2 = TC
          IMPORTING
            STRING  = NV1.
      ELSE.
        NV1 = TC.
      ENDIF.
    ENDIF.
    PC = PC + 1.

  ENDWHILE.

  CALL FUNCTION 'STRING_LENGTH'
    EXPORTING
      STRING = NV1
    IMPORTING
      LENGTH = NV1_PX.

  W_WA_DADOS2_NIVEL = P_WA_DADOS2_NIVEL.
  PC = 1.

  WHILE ( PC LE 30 ) AND ( P_VG_NIVEL IS INITIAL ) AND ( W_WA_DADOS2_NIVEL IS NOT INITIAL ).

    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        STRING = W_WA_DADOS2_NIVEL
      IMPORTING
        LENGTH = PX.

    IF PX NE 1.
      CALL FUNCTION 'STRING_SPLIT_AT_POSITION'
        EXPORTING
          STRING  = W_WA_DADOS2_NIVEL
          POS     = 1
        IMPORTING
          STRING1 = TC
          STRING2 = W_WA_DADOS2_NIVEL.
    ELSE.
      TC = W_WA_DADOS2_NIVEL.
      CLEAR: W_WA_DADOS2_NIVEL.
    ENDIF.

    IF ( TC NE '0' ) OR ( NV2 IS NOT INITIAL ).
      IF NV2 IS NOT INITIAL.
        CALL FUNCTION 'STRING_CONCATENATE'
          EXPORTING
            STRING1 = NV2
            STRING2 = TC
          IMPORTING
            STRING  = NV2.
      ELSE.
        NV2 = TC.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        STRING = NV2
      IMPORTING
        LENGTH = NV2_PX.

    CONCATENATE NV2 W_WA_DADOS2_NIVEL INTO NV_TEXT.

    IF ( NV2_PX EQ NV1_PX ).
      PERFORM PAR_UNIT USING NV1 NV_TEXT VG1 VG2.
      IF ( VG1 EQ VG2 ).
        IF ( NV2 EQ NV1 ).
          P_VG_NIVEL = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

    PC = PC + 1.

  ENDWHILE.

ENDFORM.                    " VERIFICA_NIVEL

*&---------------------------------------------------------------------*
*&      Form  PAR_UNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NV1      --> Nivel Primário
*      -->P_NV_TEXT  --> Nivel Secundário concatenado com o restante não lido
*      -->P_VG1      --> Paridade Primária
*      -->P_VG2      --> Paridade Secundária
*----------------------------------------------------------------------*
FORM PAR_UNIT  USING    P_NV1
                        P_NV_TEXT
                        P_VG1
                        P_VG2.

  DATA: Q1 TYPE I,
        Q2 TYPE I,
        R1 TYPE I,
        R2 TYPE F.

  Q1 = 0.
  Q2 = 0.

  CLEAR: P_VG1,
         P_VG2.

  IF P_NV1 IS NOT INITIAL.
    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        STRING = P_NV1
      IMPORTING
        LENGTH = Q1.
  ELSE.
    P_VG1 = 'X'.
  ENDIF.

  IF P_NV1 IS NOT INITIAL.
    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        STRING = P_NV_TEXT
      IMPORTING
        LENGTH = Q2.
  ELSE.
    P_VG2 = 'X'.
  ENDIF.

  IF ( P_VG1 IS INITIAL ) AND ( P_VG2 IS INITIAL ).

    R1 = Q1 / 2.
    R2 = Q1 / 2.

    IF R1 EQ R2.
      P_VG1 = 'X'.
    ELSE.
      CLEAR P_VG1.
    ENDIF.

    R1 = Q2 / 2.
    R2 = Q2 / 2.

    IF R1 EQ R2.
      P_VG2 = 'X'.
    ELSE.
      CLEAR P_VG2.
    ENDIF.

  ENDIF.

ENDFORM.                    " PAR_UNIT
