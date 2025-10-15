*&---------------------------------------------------------------------*
*& Report  ZMMR032
*&
*&---------------------------------------------------------------------*
*&TITULO: Vincular Pedido de compras X Navio
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 17.05.2013
*&---------------------------------------------------------------------*
REPORT  ZMMR032.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: EKKO,
        SSCRFIELDS.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
      DATA: END OF IT_MSG.
DATA: WL_MODE(1).



TYPES: BEGIN OF TY_ZMMT0033.
         INCLUDE STRUCTURE ZMMT0033.
         TYPES:  MARK TYPE C,
       END OF TY_ZMMT0033.

TYPES:


  BEGIN OF TY_PEDI,
    AEDAT TYPE EKKO-AEDAT,
    EBELN TYPE EKKO-EBELN,
    LIFNR TYPE EBAN-LIFNR,
    EKGRP TYPE EKKO-EKGRP,
    TXZ01 TYPE EKPO-TXZ01,
    ERNAM TYPE EKKO-ERNAM,
    EBELP TYPE EKPO-EBELP,
    WERKS TYPE EKPO-WERKS,
  END OF TY_PEDI,

  BEGIN OF TY_EKKN,
    EBELN TYPE EKKN-EBELN,
    EBELP TYPE EKKN-EBELP,
    SAKTO TYPE EKKN-SAKTO,
    KOSTL TYPE EKKN-KOSTL,
  END OF TY_EKKN,


  BEGIN OF TY_T001K,
    BWKEY TYPE T001K-BWKEY,
    BUKRS TYPE T001K-BUKRS,
  END OF TY_T001K,

  BEGIN OF TY_LFA1,
    LIFNR TYPE LFA1-LIFNR,
    NAME1 TYPE LFA1-NAME1,
  END OF TY_LFA1,

  BEGIN OF TY_ZNOM_TRANSPORTE,
    ID_TRANSPORTE    TYPE ZNOM_TRANSPORTE-ID_TRANSPORTE,
    DS_NOME_TRANSPOR TYPE ZNOM_TRANSPORTE-DS_NOME_TRANSPOR,
    ID_NOMEACAO_TRAN TYPE ZNOM_TRANSPORTE-ID_NOMEACAO_TRAN,
  END OF TY_ZNOM_TRANSPORTE,


  BEGIN OF TY_SAIDA,
    EBELN            TYPE EKPO-EBELN,
    EBELP            TYPE EKPO-EBELP,
    AEDAT            TYPE EKKO-AEDAT,
    ERNAM            TYPE EKKO-ERNAM,
    TXZ01            TYPE EKPO-TXZ01,
    LIFNR            TYPE EKKO-LIFNR,
    NAME1            TYPE LFA1-NAME1,
    ID_TRANSPORTE    TYPE ZNOM_TRANSPORTE-ID_TRANSPORTE,
    DS_NOME_TRANSPOR TYPE ZNOM_TRANSPORTE-DS_NOME_TRANSPOR,
    BUKRS            TYPE T001K-BUKRS,
    ID_NOMEACAO_TRAN TYPE ZMMT0033-ID_NOMEACAO_TRAN,
    BOOKING          TYPE ZSDT0045-BOOKING,
    INSTRUCAO        TYPE ZSDT0045-INSTRUCAO,
    QTD_CTNERS       TYPE ZSDT0045-QTD_CTNERS,
    TEXTO_PEDIDO(4)  TYPE C,
  END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
         INCLUDE TYPE SLIS_FIELDCAT_MAIN.
         INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
       TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: TI_BDCDATA         TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      T_MESSTAB          TYPE TABLE OF BDCMSGCOLL,
      IT_PEDI            TYPE TABLE OF TY_PEDI,
      IT_EKKN            TYPE TABLE OF TY_EKKN,
      IT_T001K           TYPE TABLE OF TY_T001K,
      IT_LFA1            TYPE TABLE OF TY_LFA1,
      IT_ZNOM_TRANSPORTE TYPE TABLE OF TY_ZNOM_TRANSPORTE,
      IT_ZMMT0033        TYPE TABLE OF TY_ZMMT0033,
      IT_SAIDA           TYPE TABLE OF TY_SAIDA,
      IT_COLOR           TYPE TABLE OF LVC_S_SCOL,
      TG_MSG_RET         TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE,
      IT_ZSDT0045        TYPE TABLE OF ZSDT0045.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:

  WA_CONT            TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_ALV             TYPE REF TO CL_GUI_ALV_GRID,
  WA_BDCDATA         LIKE LINE OF TI_BDCDATA,
  WA_PEDI            TYPE TY_PEDI,
  WA_EKKN            TYPE TY_EKKN,
  WA_T001K           TYPE TY_T001K,
  WA_LFA1            TYPE TY_LFA1,
  WA_ZNOM_TRANSPORTE TYPE TY_ZNOM_TRANSPORTE,
  WA_ZMMT0033        TYPE TY_ZMMT0033,
  WA_SAIDA           TYPE TY_SAIDA,
  WA_ZSDT0045        TYPE ZSDT0045.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  IT_FCAT         TYPE TABLE OF TY_ESTRUTURA,
  S_VARIANT       TYPE DISVARIANT           , " Tabela Estrutura co
  T_TOP           TYPE SLIS_T_LISTHEADER,
  XS_EVENTS       TYPE SLIS_ALV_EVENT,
  EVENTS          TYPE SLIS_T_EVENT,
  GD_LAYOUT       TYPE SLIS_LAYOUT_ALV,
  T_PRINT         TYPE SLIS_PRINT_ALV,
  V_REPORT        LIKE SY-REPID,
  T_SORT          TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
  IT_SETLEAF      LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
  ESTRUTURA       TYPE TABLE OF TY_ESTRUTURA,
  VG_I            TYPE I,
  WG_MENSAGEM(30).

DATA: OK-CODE          TYPE SY-UCOMM,
      VNUM(10)         TYPE C,
      VSEQ(10)         TYPE P,
      WL_ERRO(1),
      WG_DOCUMENTO(10),
      VSTATUS(1),
      INDROW           TYPE LVC_T_ROW,
      W_IND            TYPE LVC_T_ROW  WITH HEADER LINE,
      W_CONT           TYPE I,
      W_CONTC(5),
      W_MENSAGEM(50),
      W_FLAG(1)        VALUE ''.

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
DATA: EDITCONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      EDITOR          TYPE REF TO CL_GUI_TEXTEDIT,
*      CL_CONTAINER_95 TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      CL_CONTAINER_95 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_DYNDOC_ID   TYPE REF TO CL_DD_DOCUMENT,
      CL_GRID         TYPE REF TO CL_GUI_ALV_GRID,
      CL_GRID1        TYPE REF TO CL_GUI_ALV_GRID,
      GRID1           TYPE REF TO CL_GUI_ALV_GRID,
      WA_STABLE       TYPE LVC_S_STBL,
      WA_AFIELD       TYPE LVC_S_FCAT,
      IT_FIELDCAT     TYPE LVC_T_FCAT,
      W_FIELDCAT      TYPE LVC_S_FCAT,
      I_SORT          TYPE LVC_T_SORT,
      WA_LAYOUT       TYPE LVC_S_LAYO,
      IS_STABLE       TYPE LVC_S_STBL VALUE 'XX',
      WG_REPNAME      LIKE SY-REPID,
      WG_X_VARIANT    LIKE DISVARIANT,
      WG_EXIT(1)      TYPE C,
      WG_SAVE(1)      TYPE C,
      WG_VARIANT      LIKE DISVARIANT.


DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
      TL_DSELC      TYPE TABLE OF DSELC WITH HEADER LINE,
      H_DSELC       LIKE DSELC OCCURS 0 WITH HEADER LINE,
      WL_DSELC      TYPE DSELC,
      LT_F4         TYPE LVC_T_F4 WITH HEADER LINE,
      ST_HEADER     TYPE KKBLO_LISTHEADER,
      ST_FIELDCAT   TYPE KKBLO_FIELDCAT,
      IT_HEADER     TYPE KKBLO_T_LISTHEADER,
      TL_DFIES      TYPE TABLE OF DFIES WITH HEADER LINE.

DATA: H_FIELD_WA  LIKE DFIES,
      H_FIELD_TAB LIKE DFIES OCCURS 0 WITH HEADER LINE..

*** Campo da tela 0200
*------------------------------------------------------------------------*
DATA: WG_0200-BANFN TYPE ZMMT0033-BANFN,
      WG_0200-BNFPO TYPE ZMMT0033-BNFPO,
      WG_0200-BUKRS TYPE ZMMT0033-BUKRS.


*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_0               TYPE C VALUE '0',
           C_1               TYPE C VALUE '1',
           C_2               TYPE C VALUE '2',
           C_B               TYPE C VALUE 'B',
           C_S               TYPE C VALUE 'S',
           C_L               TYPE C VALUE 'L',
           C_X               TYPE C VALUE 'X',
           C_D               TYPE C VALUE 'D',
           C_K               TYPE C VALUE 'K',
           C_W               TYPE C VALUE 'W',
           C_F               TYPE C VALUE 'F',
           C_T               TYPE C VALUE 'T',
           C_I               TYPE C VALUE 'I',
           C_N               TYPE C VALUE 'N',
           C_H               TYPE C VALUE 'H',
           C_AG(2)           TYPE C VALUE 'AG',
           C_NE(2)           TYPE C VALUE 'NE',
           C_01(2)           TYPE C VALUE '01',
           C_30(2)           TYPE C VALUE '30',
           C_40(2)           TYPE C VALUE '40',
           C_50(4)           TYPE C VALUE '0050',
           C_76(2)           TYPE C VALUE '76',
           C_71(2)           TYPE C VALUE '71',
           C_72(2)           TYPE C VALUE '72',
           C_BR(2)           TYPE C VALUE 'BR',
           C_LF(2)           TYPE C VALUE 'LF',
           C_LR(2)           TYPE C VALUE 'LR',
           C_Z1(2)           TYPE C VALUE 'Z1',
           C_ADD(3)          TYPE C VALUE 'ADD',
           C_DEL(3)          TYPE C VALUE 'DEL',
           C_DG1(3)          TYPE C VALUE 'DG1',
           C_DG2(3)          TYPE C VALUE 'DG2',
           C_DUMMY_HEADER(3) TYPE C VALUE '099',
           C_DUMMY_ITENS(3)  TYPE C VALUE '098',
           C_EXIT(4)         TYPE C VALUE 'EXIT',
           C_ROOT(4)         TYPE C VALUE 'ROOT',
           C_MINIMIZAR(4)    TYPE C VALUE '@K2@',
           C_MAXIMIZAR(4)    TYPE C VALUE '@K1@',
           C_BACK(4)         TYPE C VALUE 'BACK',
           C_SAVE(4)         TYPE C VALUE 'SAVE',
           C_DESAT(5)        TYPE C VALUE 'DESAT',
           C_DMBTR(5)        TYPE C VALUE 'DMBTR',
           C_MODIF(5)        TYPE C VALUE 'MODIF',
           C_CANCEL(6)       TYPE C VALUE 'CANCEL',
           C_DELDOC(6)       TYPE C VALUE 'DELDOC',
           C_DCLICK(6)       TYPE C VALUE 'DCLICK',
           C_SEARCH(6)       TYPE C VALUE 'SEARCH',
           C_ATUALI(6)       TYPE C VALUE 'ATUALI',
           C_ADD_MSG(7)      TYPE C VALUE 'ADD_MSG',
           C_DEL_MSG(7)      TYPE C VALUE 'DEL_MSG',
           C_CLOS_MSG(8)     TYPE C VALUE 'CLOS_MSG',
           C_SAVE_MSG(8)     TYPE C VALUE 'SAVE_MSG',
           C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.


************************************************************************
* D E F I N I T I O N
************************************************************************
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS: CATCH_HOTSPOT
                FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID
                E_COLUMN_ID
                ES_ROW_NO,

      ON_DATA_CHANGED
                  FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED
                  E_ONF4
                  E_ONF4_BEFORE
                  E_ONF4_AFTER
                  E_UCOMM,

      ON_DATA_CHANGED_FINISHED
                  FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED
                  ET_GOOD_CELLS,

      ON_F4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME
                  E_FIELDVALUE
                  ES_ROW_NO
                  ER_EVENT_DATA
                  ET_BAD_CELLS
                  E_DISPLAY.

  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION

************************************************************************
* I M P L E M E N T A T I O N
************************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD ON_DATA_CHANGED.

    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          VL_VALUE TYPE LVC_VALUE.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'ID_TRANSPORTE'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX LS_GOOD-ROW_ID.
      IF WA_SAIDA-INSTRUCAO IS INITIAL.
        SELECT SINGLE ID_TRANSPORTE DS_NOME_TRANSPOR
          FROM ZNOM_TRANSPORTE
          INTO WA_ZNOM_TRANSPORTE
            WHERE ID_TRANSPORTE EQ LV_VALUE.

        IF SY-SUBRC IS INITIAL AND LV_VALUE IS NOT INITIAL.
          MOVE WA_ZNOM_TRANSPORTE-DS_NOME_TRANSPOR TO LV_VALUE.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'DS_NOME_TRANSPOR'
              I_VALUE     = LV_VALUE.
        ELSE.
          LV_VALUE = ''.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'ID_TRANSPORTE'
              I_VALUE     = LV_VALUE.
        ENDIF.
      ENDIF.

    ENDLOOP.
    "matchcode INSTRUÇÃO
***************************************
    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                          INTO LS_GOOD
                          WHERE FIELDNAME = 'INSTRUCAO'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE.

      IF LV_VALUE IS INITIAL.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'INSTRUCAO'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'DS_NOME_TRANSPOR'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'BOOKING'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'QTD_CTNERS'
            I_VALUE     = LV_VALUE.

      ELSE.
        SELECT SINGLE *
          FROM ZSDT0045
          INTO WA_ZSDT0045
            WHERE INSTRUCAO EQ LV_VALUE.

        IF SY-SUBRC IS INITIAL.
          MOVE WA_ZSDT0045-INSTRUCAO TO LV_VALUE.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'INSTRUCAO'
              I_VALUE     = LV_VALUE.

          MOVE WA_ZSDT0045-NAVIO TO LV_VALUE.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'DS_NOME_TRANSPOR'
              I_VALUE     = LV_VALUE.

          MOVE WA_ZSDT0045-BOOKING TO LV_VALUE.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'BOOKING'
              I_VALUE     = LV_VALUE.

          MOVE WA_ZSDT0045-QTD_CTNERS TO LV_VALUE.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'QTD_CTNERS'
              I_VALUE     = LV_VALUE.

        ELSE.
          MESSAGE 'Instrução não cadastrada' TYPE 'S'.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
  METHOD ON_DATA_CHANGED_FINISHED.


  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED
  "on_data_changed_finisheD
*----------------------------------------------------------------------*
  METHOD CATCH_HOTSPOT.

    TYPES: BEGIN OF TY_TDLINE,
             TDLINE TYPE TDLINE,
           END OF TY_TDLINE.

    DATA: LT_LINES TYPE TABLE OF TLINE,
          LS_LINE  LIKE LINE OF LT_LINES.

    DATA: LT_LINES_SHOW TYPE TABLE OF TY_TDLINE,
          LS_LINE_SHOW  LIKE LINE OF LT_LINES_SHOW.

    DATA: LT_TXW_NOTE TYPE TABLE OF TXW_NOTE,
          LS_TXW_NOTE TYPE TXW_NOTE.

    DATA: LV_ID     TYPE THEAD-TDID,
          LV_NAME   TYPE  THEAD-TDNAME,
          LV_OBJECT TYPE  THEAD-TDOBJECT.

    IF E_COLUMN_ID-FIELDNAME = 'TEXTO_PEDIDO'.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX ES_ROW_NO-ROW_ID.
      IF SY-SUBRC = 0.

        LV_ID      = 'F01'.
        LV_NAME    = WA_SAIDA-EBELN.
        LV_OBJECT  = 'EKKO'.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            ID                      = LV_ID
            LANGUAGE                = SY-LANGU
            NAME                    = LV_NAME
            OBJECT                  = LV_OBJECT
          TABLES
            LINES                   = LT_LINES
          EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.
        IF LT_LINES[] IS NOT INITIAL.
          LOOP AT LT_LINES INTO LS_LINE.
            LS_LINE_SHOW-TDLINE = LS_LINE-TDLINE.
            APPEND LS_LINE_SHOW TO LT_LINES_SHOW.
          ENDLOOP.

          CALL FUNCTION 'ZMM_POPUP_WITH_TABLE_DISPLAY'
            EXPORTING
              ENDPOS_COL   = 90
              ENDPOS_ROW   = 15
              STARTPOS_COL = 10
              STARTPOS_ROW = 7
              TITLETEXT    = 'Texto Pedido de Compra'
            TABLES
              VALUETAB     = LT_LINES_SHOW
            EXCEPTIONS
              BREAK_OFF    = 1
              OTHERS       = 2.
          REFRESH LT_LINES_SHOW.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "CATCH_HOTSPOT

  METHOD ON_F4.
    TYPES: BEGIN OF TY_TRANSPORTE,
             ID_TRANSPORTE    TYPE ZNOM_TRANSPORTE-ID_TRANSPORTE,
             DS_NOME_TRANSPOR TYPE ZNOM_TRANSPORTE-DS_NOME_TRANSPOR,
             DS_PORTO         TYPE ZNOM_TRANSPORTE-DS_PORTO,
             NR_ANO           TYPE ZNOM_TRANSPORTE-NR_ANO,
             NR_MES           TYPE ZNOM_TRANSPORTE-NR_MES,
             ID_NOMEACAO_TRAN TYPE ZNOM_TRANSPORTE-ID_NOMEACAO_TRAN,
           END OF TY_TRANSPORTE.

    "matchcode INSTRUÇÃO
    TYPES: BEGIN OF TY_INSTRUCAO,
             INSTRUCAO TYPE ZSDT0045-INSTRUCAO,
             NAVIO     TYPE ZSDT0045-NAVIO,
             BOOKING   TYPE ZSDT0045-BOOKING,
             QTD_CTNERS type ZSDT0045-QTD_CTNERS,
           END OF TY_INSTRUCAO.
*************************************
    DATA: TL_TRANSPORTE      TYPE TABLE OF TY_TRANSPORTE,
          WL_TRANSPORTE      TYPE TY_TRANSPORTE,
          TL_INSTRUCAO       TYPE TABLE OF TY_INSTRUCAO,
          WL_INSTRUCAO       TYPE TY_INSTRUCAO,
          VL_ID_TRANSP       TYPE C,
          TL_ZNOM_TRANSPORTE TYPE TABLE OF ZNOM_TRANSPORTE,
          WL_ZNOM_TRANSPORTE TYPE ZNOM_TRANSPORTE.

    CASE E_FIELDNAME.
      WHEN 'ID_TRANSPORTE'.
        REFRESH: TL_DSELC.

        SELECT *
          FROM ZNOM_TRANSPORTE
          INTO CORRESPONDING FIELDS OF TABLE TL_TRANSPORTE.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            RETFIELD    = 'ID_TRANSPORTE'
            DYNPPROG    = SY-REPID
            DYNPNR      = SY-DYNNR
            DYNPROFIELD = 'ID_TRANSPORTE'
            VALUE_ORG   = 'S'
          TABLES
            VALUE_TAB   = TL_TRANSPORTE
            RETURN_TAB  = TL_RETURN_TAB
          EXCEPTIONS
            OTHERS      = 3.

        READ TABLE TL_TRANSPORTE INTO WL_TRANSPORTE WITH KEY ID_TRANSPORTE = TL_RETURN_TAB-FIELDVAL.
        MOVE: WL_TRANSPORTE-ID_TRANSPORTE    TO WA_SAIDA-ID_TRANSPORTE,
              WL_TRANSPORTE-DS_NOME_TRANSPOR TO WA_SAIDA-DS_NOME_TRANSPOR,
              WL_TRANSPORTE-ID_NOMEACAO_TRAN TO WA_SAIDA-ID_NOMEACAO_TRAN.
        MODIFY IT_SAIDA FROM WA_SAIDA INDEX ES_ROW_NO-ROW_ID TRANSPORTING ID_TRANSPORTE DS_NOME_TRANSPOR ID_NOMEACAO_TRAN.

*******************************
        "matchcode INSTRUÇÃO
      WHEN 'INSTRUCAO'.
        REFRESH: TL_DSELC.

        SELECT *
          FROM ZSDT0045
          INTO CORRESPONDING FIELDS OF TABLE TL_INSTRUCAO.

*---> 04/07/2023 - Migração S4 - WS
  SORT TL_INSTRUCAO.
*<--- 04/07/2023 - Migração S4 - WS
        DELETE ADJACENT DUPLICATES FROM TL_INSTRUCAO COMPARING ALL FIELDS.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            RETFIELD    = 'INSTRUCAO'
            DYNPPROG    = SY-REPID
            DYNPNR      = SY-DYNNR
            DYNPROFIELD = 'INSTRUCAO'
            VALUE_ORG   = 'S'
          TABLES
            VALUE_TAB   = TL_INSTRUCAO
            RETURN_TAB  = TL_RETURN_TAB
          EXCEPTIONS
            OTHERS      = 3.

        READ TABLE TL_INSTRUCAO INTO WL_INSTRUCAO WITH KEY INSTRUCAO = TL_RETURN_TAB-FIELDVAL.
        MOVE: WL_INSTRUCAO-INSTRUCAO TO WA_SAIDA-INSTRUCAO.
        MOVE: WL_INSTRUCAO-NAVIO TO WA_SAIDA-DS_NOME_TRANSPOR.
        MOVE: WL_INSTRUCAO-BOOKING TO WA_SAIDA-BOOKING.
        MOVE: WL_INSTRUCAO-QTD_CTNERS TO WA_SAIDA-QTD_CTNERS.
        MODIFY IT_SAIDA FROM WA_SAIDA INDEX ES_ROW_NO-ROW_ID TRANSPORTING INSTRUCAO DS_NOME_TRANSPOR BOOKING QTD_CTNERS.

    ENDCASE.
*************************************************************
    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDMETHOD.                    "ON_F4
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

*----------------------------------------------------------------------*
*** Adiciona botões ao menu superior
*----------------------------------------------------------------------*
INITIALIZATION.
  SELECTION-SCREEN : FUNCTION KEY 1.
  SSCRFIELDS-FUNCTXT_01 = 'Estornar Vinculação'.

  DATA:       EVENT_RECEIVER   TYPE REF TO LCL_EVENT_RECEIVER.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

  SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: P_EBELN  FOR EKKO-EBELN,
                  P_AEDAT  FOR EKKO-AEDAT OBLIGATORY,
                  P_LIFNR  FOR EKKO-LIFNR.
  PARAMETERS: P_VINC AS CHECKBOX DEFAULT ''.
  SELECTION-SCREEN: END OF BLOCK B1.

AT SELECTION-SCREEN.
  IF SY-UCOMM = 'FC01'.
    CALL SCREEN 0200 STARTING AT 10 10.
  ENDIF.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM: F_SELECIONA_DADOS, " Form seleciona dados
           F_SAIDA, " Form de saida
           F_IMPRIME_DADOS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .
  IF P_VINC IS INITIAL.
    SELECT EKKO~AEDAT EKPO~EBELN LIFNR EKGRP EKPO~TXZ01 ERNAM EKPO~EBELP WERKS
      FROM EKKO
      INNER JOIN EKPO ON EKPO~EBELN = EKKO~EBELN
      INTO TABLE IT_PEDI
     WHERE EKKO~AEDAT  IN P_AEDAT
       AND EKPO~EBELN  IN P_EBELN
       AND LIFNR       IN P_LIFNR
       AND EKGRP       EQ  'D14'
*       AND     EXISTS ( SELECT *
*                        FROM ZMMT0024
*                        WHERE EBELN = EKKO~EBELN )
       AND NOT EXISTS ( SELECT *
                          FROM ZMMT0033
                         WHERE BANFN = EKPO~EBELN
                           AND BNFPO = EKPO~EBELP
                           AND ( ID_TRANSPORTE <> ''
                           OR INSTRUCAO <> '' ) ).

  ELSE.
    SELECT EKKO~AEDAT EKPO~EBELN LIFNR EKGRP EKPO~TXZ01 ERNAM EKPO~EBELP WERKS
          FROM EKKO
          INNER JOIN EKPO ON EKPO~EBELN = EKKO~EBELN
          INTO TABLE IT_PEDI
         WHERE EKKO~AEDAT  IN P_AEDAT
           AND EKPO~EBELN  IN P_EBELN
           AND LIFNR       IN P_LIFNR
           AND  EXISTS ( SELECT *
                              FROM ZMMT0033
                             WHERE BANFN = EKPO~EBELN
                               AND BNFPO = EKPO~EBELP
                               AND ( ID_TRANSPORTE <> ''
                               OR INSTRUCAO <> '' ) ).

    CHECK IT_PEDI[] IS NOT INITIAL.


    SELECT *
      FROM ZMMT0033
      INTO TABLE IT_ZMMT0033
      FOR ALL ENTRIES IN IT_PEDI
   WHERE BANFN  EQ IT_PEDI-EBELN
     AND BNFPO  EQ IT_PEDI-EBELP.

    SELECT ID_TRANSPORTE DS_NOME_TRANSPOR ID_NOMEACAO_TRAN
       FROM ZNOM_TRANSPORTE
        INTO TABLE IT_ZNOM_TRANSPORTE
        FOR ALL ENTRIES IN IT_ZMMT0033
        WHERE ID_TRANSPORTE    EQ IT_ZMMT0033-ID_TRANSPORTE.

  ENDIF.

  CHECK IT_PEDI[] IS NOT INITIAL.

  SELECT EBELN EBELP SAKTO KOSTL
    FROM EKKN
    INTO TABLE IT_EKKN
     FOR ALL ENTRIES IN IT_PEDI
   WHERE EBELN      EQ IT_PEDI-EBELN
     AND EBELP      EQ IT_PEDI-EBELP.

  SELECT LIFNR NAME1
    FROM LFA1
    INTO TABLE IT_LFA1
    FOR ALL ENTRIES IN IT_PEDI
    WHERE LIFNR EQ IT_PEDI-LIFNR.

  SELECT BWKEY BUKRS
    FROM T001K
    INTO TABLE IT_T001K
    FOR ALL ENTRIES IN IT_PEDI
    WHERE BWKEY	=	IT_PEDI-WERKS.

  SELECT INSTRUCAO
    FROM ZSDT0045
    INTO TABLE IT_ZSDT0045.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .
  SORT: IT_EKKN    BY EBELN EBELP,
        IT_LFA1    BY LIFNR,
        IT_T001K   BY BWKEY.

  LOOP AT IT_PEDI INTO WA_PEDI.
    WA_SAIDA-EBELN              = WA_PEDI-EBELN.
    WA_SAIDA-EBELP              = WA_PEDI-EBELP.
    WA_SAIDA-AEDAT              = WA_PEDI-AEDAT.
    WA_SAIDA-ERNAM              = WA_PEDI-ERNAM.
    WA_SAIDA-TXZ01              = WA_PEDI-TXZ01.
    WA_SAIDA-LIFNR              = WA_PEDI-LIFNR.

    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_PEDI-LIFNR BINARY SEARCH.
    WA_SAIDA-NAME1              = WA_LFA1-NAME1.

    WA_SAIDA-ID_TRANSPORTE      = ''.
    WA_SAIDA-DS_NOME_TRANSPOR   = ''.
    WA_SAIDA-TEXTO_PEDIDO       = '@HJ@'.

    READ TABLE IT_T001K INTO WA_T001K WITH KEY BWKEY  = WA_PEDI-WERKS BINARY SEARCH.
    WA_SAIDA-BUKRS              = WA_T001K-BUKRS.

    READ TABLE IT_ZMMT0033 INTO WA_ZMMT0033 WITH KEY  BANFN = WA_PEDI-EBELN
                                                      BNFPO = WA_PEDI-EBELP.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_ZNOM_TRANSPORTE INTO WA_ZNOM_TRANSPORTE WITH KEY ID_TRANSPORTE = WA_ZMMT0033-ID_TRANSPORTE.
      IF WA_ZMMT0033-ID_TRANSPORTE IS NOT INITIAL.
        WA_SAIDA-ID_TRANSPORTE    = WA_ZMMT0033-ID_TRANSPORTE.
        WA_SAIDA-DS_NOME_TRANSPOR = WA_ZMMT0033-DS_NOME_TRANSPOR.
        WA_SAIDA-ID_NOMEACAO_TRAN = WA_ZMMT0033-ID_NOMEACAO_TRAN.
      ELSE.
        WA_SAIDA-ID_TRANSPORTE      = ''.
        WA_SAIDA-DS_NOME_TRANSPOR   = ''.
        WA_SAIDA-ID_NOMEACAO_TRAN   = WA_ZMMT0033-ID_NOMEACAO_TRAN.

      ENDIF.
**************************************************************************************************
      READ TABLE IT_ZSDT0045 INTO WA_ZSDT0045 WITH KEY INSTRUCAO = WA_ZMMT0033-INSTRUCAO.
      IF WA_ZMMT0033-INSTRUCAO IS NOT INITIAL.
        WA_SAIDA-INSTRUCAO = WA_ZMMT0033-INSTRUCAO.
      ELSE.
        WA_SAIDA-INSTRUCAO = ''.
        WA_SAIDA-BOOKING = ''.
        WA_SAIDA-DS_NOME_TRANSPOR = ''.
        WA_SAIDA-QTD_CTNERS = ''.
      ENDIF.
      IF  WA_ZMMT0033-BOOKING IS NOT INITIAL.
        WA_SAIDA-BOOKING = WA_ZMMT0033-BOOKING.
      ELSE.
        WA_SAIDA-BOOKING = WA_ZSDT0045-BOOKING.
      ENDIF.
      IF  WA_ZMMT0033-NAVIO IS NOT INITIAL.
        WA_SAIDA-DS_NOME_TRANSPOR = WA_ZMMT0033-NAVIO.
      ELSE.
        WA_SAIDA-DS_NOME_TRANSPOR = WA_ZSDT0045-NAVIO.
      ENDIF.
      IF  WA_ZMMT0033-QTD_CTNERS IS NOT INITIAL.
        WA_SAIDA-QTD_CTNERS = WA_ZMMT0033-QTD_CTNERS.
      ELSE.
        WA_SAIDA-QTD_CTNERS = WA_ZSDT0045-QTD_CTNERS.
      ENDIF.
************************************************************************************************
    ENDIF.
    APPEND WA_SAIDA TO IT_SAIDA.

  ENDLOOP.

ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .

  PERFORM F_ALV_FIELDCAT.

  WA_LAYOUT-ZEBRA      = 'X'.
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.
  WA_LAYOUT-GRID_TITLE = 'Vincular Pedido X Navio'.
  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT   = 'X'.

  CALL SCREEN 0100.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .

  DATA I TYPE I.

  WA_AFIELD-TABNAME     = 'IT_SAIDA'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'EBELN'.
  WA_AFIELD-SCRTEXT_S = 'Pedido'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'AEDAT'.
  WA_AFIELD-SCRTEXT_S = 'Dt. Pedido'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ERNAM'.
  WA_AFIELD-SCRTEXT_S = 'Usuário'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TXZ01'.
  WA_AFIELD-SCRTEXT_S = 'Descrição'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'LIFNR'.
  WA_AFIELD-SCRTEXT_S = 'Fornecedor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NAME1'.
  WA_AFIELD-SCRTEXT_S = 'Nome do Fornecedor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BUKRS'.
  WA_AFIELD-SCRTEXT_S = 'Empresa'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*  IF P_VINC IS NOT INITIAL.
*    I = I + 1.
*    CLEAR WA_AFIELD.
*    WA_AFIELD-COL_POS       = I.
*    WA_AFIELD-FIELDNAME     = 'ID_TRANSPORTE'.
*    WA_AFIELD-SCRTEXT_S = 'Navio'.
*    WA_AFIELD-REF_TABLE = SPACE."'ZNOM_TRANSPORTE'.
*    WA_AFIELD-REF_FIELD = SPACE."'ID_TRANSPORTE'.
*    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
*    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
**    WA_AFIELD-EDIT          = 'X'.
*    WA_AFIELD-KEY           = ''.
**    WA_AFIELD-F4AVAILABL    = C_X.
*    APPEND WA_AFIELD TO IT_FIELDCAT.
*  ELSE.
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ID_TRANSPORTE'.
  WA_AFIELD-SCRTEXT_S = 'Navio'.
  WA_AFIELD-REF_TABLE = SPACE."'ZNOM_TRANSPORTE'.
  WA_AFIELD-REF_FIELD = SPACE."'ID_TRANSPORTE'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = 'X'.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-F4AVAILABL    = C_X.
  APPEND WA_AFIELD TO IT_FIELDCAT.
*  ENDIF.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DS_NOME_TRANSPOR'.
  WA_AFIELD-SCRTEXT_S = 'Nome do Navio'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = 'X'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BOOKING'.
  WA_AFIELD-SCRTEXT_S = 'Booking'.
  WA_AFIELD-REF_TABLE = SPACE.                              "ZSDT0045
  WA_AFIELD-REF_FIELD = SPACE. "instrução
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = 'X'.
  WA_AFIELD-DD_OUTLEN    = '35'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'INSTRUCAO'.
  WA_AFIELD-SCRTEXT_S = 'Instrução de Embarque'.
  WA_AFIELD-REF_TABLE = SPACE.                              "ZSDT0045
  WA_AFIELD-REF_FIELD = SPACE. "instrução
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = 'X'.
  WA_AFIELD-DD_OUTLEN    = '50'.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-F4AVAILABL    = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'QTD_CTNERS'.
  WA_AFIELD-SCRTEXT_S = 'Qtde. Container'.
  WA_AFIELD-REF_TABLE = SPACE.                              "ZSDT0045
  WA_AFIELD-REF_FIELD = SPACE. "instrução
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = 'X'.
  WA_AFIELD-DD_OUTLEN    = '16'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TEXTO_PEDIDO'.
  WA_AFIELD-SCRTEXT_S = 'Texto do Pedido'.
  WA_AFIELD-REF_TABLE = SPACE.                              "ZSDT0045
  WA_AFIELD-REF_FIELD = SPACE. "instrução
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-DD_OUTLEN    = '5'.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-ICON          = 'X'.
  WA_AFIELD-HOTSPOT       = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.

  SET PF-STATUS 'F_SET_PF' EXCLUDING FCODE.
  SET TITLEBAR  'ZFTITLE'.


  IF CL_CONTAINER_95 IS INITIAL.

    CREATE OBJECT CL_CONTAINER_95
      EXPORTING
        CONTAINER_NAME = 'CONTAINER'.
*      EXPORTING
*        SIDE  = '4'
*        RATIO = '80'.
  ENDIF.

  IF NOT CL_GRID IS INITIAL.

    PERFORM ZF_ALV_HEADER.
    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
        NO_MARGINS = 'X'.

    PERFORM ZF_ALV_HEADER .


    IF EDITCONTAINER IS INITIAL .
      CREATE OBJECT EDITCONTAINER
        EXPORTING
          CONTAINER_NAME = 'HEADER'.
    ENDIF .

    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.
*
*
    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT = CL_CONTAINER_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    WG_SAVE = 'X'.
    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

***    Eventos F4
    REFRESH LT_F4.
    LT_F4-FIELDNAME = 'ID_TRANSPORTE'.
    LT_F4-REGISTER  = 'X' .
    LT_F4-GETBEFORE = 'X' .
    APPEND LT_F4 .

    LT_F4-FIELDNAME = 'INSTRUCAO'.
    LT_F4-REGISTER  = 'X' .
    LT_F4-GETBEFORE = 'X' .
    APPEND LT_F4 .

    CALL METHOD CL_GRID->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = LT_F4[].

    WA_STABLE-ROW        = C_X.
    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING        "IS_VARIANT = WG_X_VARIANT
        "I_SAVE = WG_SAVE
        IS_LAYOUT       = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT[]
        IT_SORT         = I_SORT[]
        IT_OUTTAB       = IT_SAIDA[].


    CREATE OBJECT EVENT_RECEIVER.
    SET HANDLER EVENT_RECEIVER->ON_DATA_CHANGED          FOR CL_GRID.
    SET HANDLER EVENT_RECEIVER->ON_DATA_CHANGED_FINISHED FOR CL_GRID.
    SET HANDLER EVENT_RECEIVER->ON_F4                    FOR CL_GRID.
    SET HANDLER EVENT_RECEIVER->CATCH_HOTSPOT            FOR CL_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_TOP_OF_PAGE       FOR CL_GRID.



  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ALV_HEADER.
  DATA:   WL_DATA(10),
            WL_HORA(8),
            WL_LINHA(60),
            WL_TEXT TYPE SDYDO_TEXT_ELEMENT.

  IF P_EBELN   IS NOT INITIAL.
    IF P_EBELN-HIGH IS INITIAL.
      CONCATENATE 'Pedido  :' P_EBELN-LOW
       INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Pedido  :' P_EBELN-LOW 'à' P_EBELN-HIGH
      INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_LIFNR   IS NOT INITIAL.
    IF P_LIFNR-HIGH IS INITIAL.
      CONCATENATE 'Fornecedor  :' P_LIFNR-LOW
       INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Fornecedor  :' P_LIFNR-LOW 'à' P_LIFNR-HIGH
      INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*          SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_AEDAT IS NOT INITIAL.
    CONCATENATE P_AEDAT-LOW+6(2) P_AEDAT-LOW+4(2) P_AEDAT-LOW+0(4) INTO  WL_DATA SEPARATED BY '.'.
    IF P_AEDAT-HIGH IS INITIAL.
      CONCATENATE 'Dt. Pedido  :' WL_DATA
      INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Dt. Pedido :' WL_DATA  INTO WL_LINHA SEPARATED BY SPACE.
      CONCATENATE P_AEDAT-HIGH+6(2) P_AEDAT-HIGH+4(2) P_AEDAT-HIGH+0(4) INTO  WL_DATA SEPARATED BY '.'.
      CONCATENATE WL_LINHA 'à' WL_DATA  INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.
ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.


  IF NOT CL_GRID IS INITIAL.
    CALL METHOD CL_GRID->DISPATCH
      EXPORTING
        CARGO         = SY-UCOMM
        EVENTID       = 19
        IS_SHELLEVENT = ' '.

    IF SY-UCOMM IS INITIAL.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = IS_STABLE.
    ENDIF.
  ENDIF.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH IT_SAIDA.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN  'SAVE'.
      IF IT_SAIDA[] IS NOT INITIAL.
        PERFORM VERIFICA_ERROS.
        IF TG_MSG_RET[] IS NOT INITIAL.
          MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        ENDIF.
        PERFORM VERIFICA_INSTRUCAO.
        IF TG_MSG_RET[] IS NOT INITIAL.
          MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro na instrução.'.
        ENDIF.
        IF TG_MSG_RET[] IS INITIAL.
          "grava
          LOOP AT IT_SAIDA INTO WA_SAIDA.
            IF WA_SAIDA-ID_TRANSPORTE IS NOT  INITIAL OR ( WA_SAIDA-INSTRUCAO IS NOT INITIAL AND WA_SAIDA-BOOKING IS NOT INITIAL AND
                                                           WA_SAIDA-DS_NOME_TRANSPOR IS NOT INITIAL AND WA_SAIDA-QTD_CTNERS is NOT INITIAL )..
              MOVE:  WA_SAIDA-EBELN                TO WA_ZMMT0033-BANFN,
                     WA_SAIDA-EBELP                TO WA_ZMMT0033-BNFPO,
                     WA_SAIDA-BUKRS                TO WA_ZMMT0033-BUKRS,
                     WA_SAIDA-ID_TRANSPORTE        TO WA_ZMMT0033-ID_TRANSPORTE,
                     WA_SAIDA-DS_NOME_TRANSPOR     TO WA_ZMMT0033-DS_NOME_TRANSPOR,
                     SY-UNAME                      TO WA_ZMMT0033-USNAM,
                     SY-DATUM                      TO WA_ZMMT0033-CPUDT,
                     SY-UZEIT                      TO WA_ZMMT0033-CPUTM,
                     WA_SAIDA-ID_NOMEACAO_TRAN     TO WA_ZMMT0033-ID_NOMEACAO_TRAN,
                     WA_SAIDA-INSTRUCAO            TO WA_ZMMT0033-INSTRUCAO,
                     WA_SAIDA-DS_NOME_TRANSPOR     TO WA_ZMMT0033-NAVIO,
                     WA_SAIDA-QTD_CTNERS           TO WA_ZMMT0033-QTD_CTNERS,
                     WA_SAIDA-BOOKING              TO WA_ZMMT0033-BOOKING.

              MODIFY ZMMT0033 FROM       WA_ZMMT0033.
              COMMIT WORK AND WAIT.
            ENDIF.
          ENDLOOP.

          MESSAGE S836(SD) WITH 'Lançamentos gravados com sucesso!'.
*        ELSE.
*          MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
*          CALL FUNCTION 'Z_DOC_CHECK_NEW'
*            EXPORTING
*              I_SCREEN      = '0100'
*              I_SHOW        = C_X
*              I_REPID       = SY-REPID
*              I_PRESSED_TAB = ''
*              I_SET_FIELD   = 'X_FIELD'
*            IMPORTING
*              E_MESSAGEM    = WG_MENSAGEM
*            TABLES
*              IT_MSGS       = TG_MSG_RET.
        ENDIF.

      ENDIF.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_ERROS .
  DATA  WL_LINHA(6).
  REFRESH: TG_MSG_RET,IT_ZNOM_TRANSPORTE.
  CLEAR: TG_MSG_RET.
  SELECT ID_TRANSPORTE DS_NOME_TRANSPOR
            FROM   ZNOM_TRANSPORTE
            INTO TABLE IT_ZNOM_TRANSPORTE
            FOR ALL ENTRIES IN IT_SAIDA
            WHERE ID_TRANSPORTE EQ IT_SAIDA-ID_TRANSPORTE.
  SORT IT_ZNOM_TRANSPORTE BY ID_TRANSPORTE.

  LOOP AT IT_SAIDA INTO WA_SAIDA.
    IF WA_SAIDA-ID_TRANSPORTE IS NOT  INITIAL.
      WL_LINHA = SY-TABIX.
      READ TABLE IT_ZNOM_TRANSPORTE INTO WA_ZNOM_TRANSPORTE WITH KEY ID_TRANSPORTE = WA_SAIDA-ID_TRANSPORTE BINARY SEARCH.
      IF SY-SUBRC NE 0.
        CLEAR TG_MSG_RET-ABA.
        CONCATENATE TEXT-E02 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_INSTRUCAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_INSTRUCAO .
  DATA     WL_LINHA(6).
  REFRESH: TG_MSG_RET,IT_ZSDT0045.
  CLEAR:   TG_MSG_RET.

  SELECT *
      FROM  ZSDT0045
      INTO TABLE IT_ZSDT0045
      FOR ALL ENTRIES IN IT_SAIDA
       WHERE INSTRUCAO EQ IT_SAIDA-INSTRUCAO.
  SORT IT_ZSDT0045 BY INSTRUCAO.

  LOOP AT IT_SAIDA INTO WA_SAIDA.
    IF WA_SAIDA-INSTRUCAO IS NOT INITIAL.
      WL_LINHA = SY-TABIX.
      READ TABLE IT_ZSDT0045 INTO WA_ZSDT0045 WITH KEY INSTRUCAO = WA_SAIDA-INSTRUCAO.
      IF WA_SAIDA-INSTRUCAO NE WA_ZSDT0045-INSTRUCAO.
*        WA_ZMMT0033-INSTRUCAO = WA_SAIDA-INSTRUCAO.
        CLEAR TG_MSG_RET-ABA.
*        CONCATENATE TEXT-E03 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " VERIFICA_INSTRUCAO


*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR '200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  DATA: WL_ZMMT0033 TYPE ZMMT0033.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT' OR 'SAIR'.
      LEAVE TO SCREEN 0.
    WHEN 'ENTER' OR 'EST'.

      SELECT SINGLE *
        FROM ZMMT0033
        INTO CORRESPONDING FIELDS OF WL_ZMMT0033
       WHERE BANFN = WG_0200-BANFN
         AND BNFPO = WG_0200-BNFPO
         AND BUKRS = WG_0200-BUKRS.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE 'Vinculação não econtrada.' TYPE 'S'.
      ELSE.
        DELETE ZMMT0033 FROM WL_ZMMT0033.
        MESSAGE 'Vinculação removida' TYPE 'I'.
      ENDIF.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
