*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1103 .
*----------------------------------------------------------------------*

TYPE-POOLS: TRUXS.

TYPES: BEGIN OF TY_CONSOL.
TYPES:  BUKRS     TYPE ZGL029_DRE_DADOS-BUKRS,
        VERSN     TYPE ZGL029_DRE_DADOS-VERSN,
        MONAT     TYPE ZGL029_DRE_DADOS-MONAT,
        GJAHR     TYPE ZGL029_DRE_DADOS-GJAHR,
        SAKNR     TYPE ZGL029_DRE_DADOS-SAKNR,
        KOSTL     TYPE ZGL029_DRE_DADOS-KOSTL,
        PRCTR     TYPE ZGL029_DRE_DADOS-PRCTR,
        MATKL     TYPE ZGL029_DRE_DADOS-MATKL,
        VBUND     TYPE ZGL029_DRE_DADOS-VBUND,
        QTD_TON   TYPE ZGL029_DRE_DADOS-QTD_TON,
        VLR_REA   TYPE ZGL029_DRE_DADOS-VLR_REA,
        VLR_DOLAR TYPE ZGL029_DRE_DADOS-VLR_DOLAR,
        VLR_GRUPO TYPE ZGL029_DRE_DADOS-VLR_GRUPO.
TYPES: END OF TY_CONSOL.

DATA: TG_CONSOL TYPE TABLE OF TY_CONSOL WITH HEADER LINE.

*DATA:    BEGIN OF TG_CONSOL OCCURS 0,
*            BUKRS      TYPE ZGL029_DRE_DADOS-BUKRS,
*            VERSN      TYPE ZGL029_DRE_DADOS-VERSN,
*            MONAT      TYPE ZGL029_DRE_DADOS-MONAT,
*            GJAHR      TYPE ZGL029_DRE_DADOS-GJAHR,
*            SAKNR      TYPE ZGL029_DRE_DADOS-SAKNR,
*            KOSTL      TYPE ZGL029_DRE_DADOS-KOSTL,
*            PRCTR      TYPE ZGL029_DRE_DADOS-PRCTR,
*            MATKL      TYPE ZGL029_DRE_DADOS-MATKL,
*            VBUND      TYPE ZGL029_DRE_DADOS-VBUND,
*            QTD_TON    TYPE ZGL029_DRE_DADOS-QTD_TON,
*            VLR_REA    TYPE ZGL029_DRE_DADOS-VLR_REA,
*            VLR_DOLAR  TYPE ZGL029_DRE_DADOS-VLR_DOLAR,
*            VLR_GRUPO  TYPE ZGL029_DRE_DADOS-VLR_GRUPO,
*          END OF TG_CONSOL.

DATA: BEGIN OF C_COLUMN,
        COLUMN0 TYPE TV_ITMNAME VALUE 'NIVEL',              "#EC NOTEXT
        COLUMN1 TYPE TV_ITMNAME VALUE 'CONTA',              "#EC NOTEXT
        COLUMN2 TYPE TV_ITMNAME VALUE 'VALOR1',             "#EC NOTEXT
        COLUMN3 TYPE TV_ITMNAME VALUE 'VALOR2',             "#EC NOTEXT
        COLUMN4 TYPE TV_ITMNAME VALUE 'VALOR3',             "#EC NOTEXT
      END OF C_COLUMN,

      T_NOTAS  TYPE STANDARD TABLE OF ZRANGE_CLAS_NOT,
      T_NIVEIS TYPE STANDARD TABLE OF ZRANGE_NIVEL.


*----------------------------------------------------------------------*
*       CLASS LCL_TREE_VIEW_EVENT DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_TREE_VIEW_EVENT DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: ON_NODE_DOUBLE_CLICK
        FOR EVENT NODE_DOUBLE_CLICK OF CL_GUI_ALV_TREE IMPORTING NODE_KEY.
ENDCLASS.                    "LCL_TREE_VIEW_EVENT DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_TREE_VIEW_EVENT IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_TREE_VIEW_EVENT IMPLEMENTATION.
  METHOD ON_NODE_DOUBLE_CLICK.

    DATA: WA_ZGLT047_TREE TYPE TY_ZGLT047_TREE,
          WA_SKB1         TYPE SKB1,
          VG_MES(2),
          VG_ANO(4),
          S_MES_F         TYPE ZFIED005,
          S_MES_C         TYPE ZFIED005,
          S_MES_U         TYPE ZFIED005,
          LC_GJAHR        TYPE GJAHR,
          W_NOTAS         TYPE ZRANGE_CLAS_NOT,
          W_NIVEL         TYPE ZRANGE_NIVEL.

    CALL METHOD G_TREE3->GET_OUTTAB_LINE
      EXPORTING
        I_NODE_KEY     = NODE_KEY
      IMPORTING
        E_OUTTAB_LINE  = WA_ZGLT047_TREE
      EXCEPTIONS
        NODE_NOT_FOUND = 1
        OTHERS         = 2.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.

      "Double Click na Conta Razão
      IF ( WA_ZGLT047_TREE-SAKNR IS NOT INITIAL ) OR
         ( WA_ZGLT047_TREE-COD_CLAS_NOT IS NOT INITIAL ) OR
         ( WA_ZGLT047_TREE-NIVEL IS NOT INITIAL ).

        DATA: T_BUKRS TYPE STANDARD TABLE OF BUKRS_RAN,
              W_BUKRS TYPE BUKRS_RAN,
              T_CONTA TYPE STANDARD TABLE OF J_3RMSO_HKONT,
              W_CONTA TYPE J_3RMSO_HKONT,
              W_TOTAL TYPE TY_1103_TOTAIS.

        CLEAR: T_NOTAS, T_NIVEIS.

        W_BUKRS-SIGN   = 'I'.
        W_BUKRS-OPTION = 'EQ'.
        W_BUKRS-LOW    = P_BUKRS.
        W_BUKRS-HIGH   = P_BUKRS.
        APPEND W_BUKRS TO T_BUKRS.

        IF WA_ZGLT047_TREE-SAKNR IS NOT INITIAL.
          SELECT SINGLE * INTO WA_SKB1 "#EC CI_DB_OPERATION_OK[2431747]
            FROM SKB1
           WHERE BUKRS EQ P_BUKRS
             AND SAKNR EQ WA_ZGLT047_TREE-SAKNR.

          W_CONTA-SIGN   = 'I'.
          W_CONTA-OPTION = 'EQ'.
          W_CONTA-LOW    = WA_ZGLT047_TREE-SAKNR.
          W_CONTA-HIGH   = WA_ZGLT047_TREE-SAKNR.
          APPEND W_CONTA TO T_CONTA.
        ENDIF.

        IF WA_ZGLT047_TREE-COD_CLAS_NOT IS NOT INITIAL.
          W_NOTAS-SIGN   = 'I'.
          W_NOTAS-OPTION = 'EQ'.
          W_NOTAS-LOW    = WA_ZGLT047_TREE-COD_CLAS_NOT.
          W_NOTAS-HIGH   = WA_ZGLT047_TREE-COD_CLAS_NOT.
          APPEND W_NOTAS TO T_NOTAS.

          W_NIVEL-SIGN   = 'I'.
          W_NIVEL-OPTION = 'EQ'.
          W_NIVEL-LOW    = WA_ZGLT047_TREE-NIVEL.
          W_NIVEL-HIGH   = WA_ZGLT047_TREE-NIVEL.
          APPEND W_NIVEL TO T_NIVEIS.
        ELSE.
          PERFORM BUSCA_NOTAS_NIVEL TABLES T_NOTAS T_NIVEIS USING WA_ZGLT047_TREE-NIVEL.
          SORT T_NOTAS BY LOW.
          DELETE ADJACENT DUPLICATES FROM T_NOTAS COMPARING LOW.
          SORT T_NIVEIS BY LOW.
          DELETE ADJACENT DUPLICATES FROM T_NIVEIS COMPARING LOW.
        ENDIF.

        IF WA_ZGLT047_TREE-SAKNR IS INITIAL.
          CHECK T_NOTAS IS NOT INITIAL.
        ENDIF.

        " Mês Atual -------------------------------------------------------------------------
        MOVE: P_MONAT TO VG_MES,
              P_GJAHR TO VG_ANO.

        CONCATENATE VG_MES VG_ANO INTO S_MES_F.
        "************************************************************************************

        " Mês Anterior ----------------------------------------------------------------------
        LC_GJAHR = P_GJAHR.
        ADD -1 TO LC_GJAHR.

        MOVE: LC_GJAHR TO VG_ANO.

        CONCATENATE VG_MES VG_ANO INTO S_MES_C.
        "************************************************************************************

        " Mês Anterior Final Ano ------------------------------------------------------------
        VG_MES = '12'.
        CONCATENATE VG_MES VG_ANO INTO S_MES_U.
        "************************************************************************************

        CASE WA_SKB1-MITKZ.

            "Imobilizados
          WHEN 'A'.
            "Transação ZGL029 -- Relatório para Notas do Imobilizado
            SUBMIT ZGL023 USING SELECTION-SCREEN '1000'
                  WITH S_BUKRS EQ P_BUKRS
                  WITH S_EXERC EQ P_GJAHR
                  WITH S_PERIO EQ P_MONAT
                  WITH S_MOEDA EQ P_WAERS
                  WITH S_CONTA IN T_CONTA
                  WITH S_NOTA  IN T_NOTAS
                  AND RETURN .

          WHEN OTHERS.
            DATA: V_BUKRS_CONSOLIDADO TYPE BUKRS.
            IF P_CONSO IS NOT INITIAL.
              V_BUKRS_CONSOLIDADO = P_BUKR2.
            ELSE.
              CLEAR V_BUKRS_CONSOLIDADO.
            ENDIF.

            SELECT SINGLE *
              INTO WA_ZGLT046
              FROM ZGLT046
             WHERE VERSN  EQ P_ESTR.

            "Transação ZGL022 -- Conta Razão
            SUBMIT ZGL022 USING SELECTION-SCREEN '1000'
                  WITH S_BUKRS IN T_BUKRS
                  WITH S_BUKR2 EQ V_BUKRS_CONSOLIDADO
                  WITH S_MES_F EQ S_MES_F
                  WITH S_MES_C EQ S_MES_C
                  WITH S_MES_U EQ S_MES_U
                  WITH S_MOEDA EQ P_WAERS
                  WITH S_CONTA IN T_CONTA
                  WITH S_NOTA  IN T_NOTAS
                  WITH S_ESTRB EQ WA_ZGLT046-VERSNT
                  WITH S_VERSN EQ P_ESTR
                  WITH S_NIVEL IN T_NIVEIS
                  WITH S_DRE1  EQ T_DRE1
                  WITH S_DRE2  EQ T_DRE2
                  WITH S_DRE3  EQ T_DRE3
                  WITH S_DRE4  EQ T_DRE4
                  AND RETURN.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "ON_NODE_DOUBLE_CLICK
ENDCLASS.                    "LCL_TREE_VIEW_EVENT IMPLEMENTATION

*----------------------------------------------------------------------*
*   INCLUDE BCALV_TOOLBAR_EVENT_RECEIVER                               *
*----------------------------------------------------------------------*
CLASS LCL_TOOLBAR_EVENT DEFINITION.
  PUBLIC SECTION.
    METHODS: ON_FUNCTION_SELECTED
                  FOR EVENT FUNCTION_SELECTED OF CL_GUI_TOOLBAR
      IMPORTING FCODE.
ENDCLASS.                    "lcl_toolbar_event DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_toolbar_event IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_TOOLBAR_EVENT IMPLEMENTATION.
  METHOD ON_FUNCTION_SELECTED.
    CASE FCODE.
      WHEN 'EXCEL'.
        DATA: P_CAMINHO           TYPE STRING,
              CSV_CONVERTED_TABLE TYPE TRUXS_T_TEXT_DATA,
              LD_FILENAME         TYPE STRING,
              LD_PATH             TYPE STRING,
              LD_FULLPATH         TYPE STRING,
              IT_SAIDA            TYPE STANDARD TABLE OF TY_ZGLT047_TREE_S,
              WA_SAIDA            TYPE TY_ZGLT047_TREE_S,
              WA_ZGLT047_TREE     TYPE TY_ZGLT047_TREE,
              LC_DEF_FILE_NAME    TYPE STRING,
              LC_TEXTO(4096).

        IF P_DEM1 IS NOT INITIAL.
          LC_DEF_FILE_NAME = 'BalancoPatrimonial'.
        ELSE.
          LC_DEF_FILE_NAME = 'DRE'.
        ENDIF.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
          EXPORTING
            DEFAULT_EXTENSION    = 'csv'
            DEFAULT_FILE_NAME    = LC_DEF_FILE_NAME
          CHANGING
            FILENAME             = LD_FILENAME
            PATH                 = LD_PATH
            FULLPATH             = LD_FULLPATH
          EXCEPTIONS
            CNTL_ERROR           = 1
            ERROR_NO_GUI         = 2
            NOT_SUPPORTED_BY_GUI = 3
            OTHERS               = 4.

        IF SY-SUBRC IS NOT INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        IF LD_FULLPATH IS NOT INITIAL.

          CLEAR: IT_SAIDA.

          LOOP AT IT_ZGLT047_TREE INTO WA_ZGLT047_TREE.
            MOVE-CORRESPONDING WA_ZGLT047_TREE TO WA_SAIDA.

            IF WA_ZGLT047_TREE-VALOR_01 LT 0.
              MULTIPLY WA_ZGLT047_TREE-VALOR_01 BY -1.
              MOVE WA_ZGLT047_TREE-VALOR_01 TO WA_SAIDA-VALOR_01.
              SHIFT WA_SAIDA-VALOR_01 LEFT DELETING LEADING ' '.
              CONCATENATE '-' WA_SAIDA-VALOR_01 INTO WA_SAIDA-VALOR_01.
              REPLACE ALL OCCURRENCES OF '.' IN WA_SAIDA-VALOR_01 WITH ','.
            ELSE.
              WRITE WA_ZGLT047_TREE-VALOR_01 TO WA_SAIDA-VALOR_01.
            ENDIF.

            IF WA_ZGLT047_TREE-VALOR_02 LT 0.
              MULTIPLY WA_ZGLT047_TREE-VALOR_02 BY -1.
              MOVE WA_ZGLT047_TREE-VALOR_02 TO WA_SAIDA-VALOR_02.
              SHIFT WA_SAIDA-VALOR_02 LEFT DELETING LEADING ' '.
              CONCATENATE '-' WA_SAIDA-VALOR_02 INTO WA_SAIDA-VALOR_02.
              REPLACE ALL OCCURRENCES OF '.' IN WA_SAIDA-VALOR_02 WITH ','.
            ELSE.
              WRITE WA_ZGLT047_TREE-VALOR_02 TO WA_SAIDA-VALOR_02.
            ENDIF.

            IF WA_ZGLT047_TREE-VALOR_03 LT 0.
              MULTIPLY WA_ZGLT047_TREE-VALOR_03 BY -1.
              MOVE WA_ZGLT047_TREE-VALOR_03 TO WA_SAIDA-VALOR_03.
              SHIFT WA_SAIDA-VALOR_03 LEFT DELETING LEADING ' '.
              CONCATENATE '-' WA_SAIDA-VALOR_03 INTO WA_SAIDA-VALOR_03.
              REPLACE ALL OCCURRENCES OF '.' IN WA_SAIDA-VALOR_03 WITH ','.
            ELSE.
              WRITE WA_ZGLT047_TREE-VALOR_03 TO WA_SAIDA-VALOR_03.
            ENDIF.

            APPEND WA_SAIDA TO IT_SAIDA.
          ENDLOOP.

          CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
            EXPORTING
              I_FIELD_SEPERATOR    = ';'
            TABLES
              I_TAB_SAP_DATA       = IT_SAIDA[]
            CHANGING
              I_TAB_CONVERTED_DATA = CSV_CONVERTED_TABLE
            EXCEPTIONS
              CONVERSION_FAILED    = 1
              OTHERS               = 2.

          CONCATENATE TEXT-088 TEXT-089 TEXT-090
                      TEXT-091 TEXT-092 TEXT-093
                      TEXT-094 TEXT-095 TEXT-096
                      TEXT-097 TEXT-098 TEXT-099 TEXT-100
*                      'Estrutura' 'Nível' 'Descrição do Nível'
*                      'Sequência' 'Nível Pai' 'Nível Somatório'
*                      'Cód. Nota' 'Descriação da Nota' 'Conta Razão'
*                      'Descrição da Conta Razão' 'Valor 01' 'Valor 02' 'Valor 03'
                      INTO LC_TEXTO SEPARATED BY ';'.

          INSERT LC_TEXTO INTO CSV_CONVERTED_TABLE INDEX 1.

          P_CAMINHO  = LD_FULLPATH.
          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              FILENAME                = P_CAMINHO
            TABLES
              DATA_TAB                = CSV_CONVERTED_TABLE
            EXCEPTIONS
              FILE_WRITE_ERROR        = 1
              NO_BATCH                = 2
              GUI_REFUSE_FILETRANSFER = 3
              INVALID_TYPE            = 4
              NO_AUTHORITY            = 5
              UNKNOWN_ERROR           = 6
              HEADER_NOT_ALLOWED      = 7
              SEPARATOR_NOT_ALLOWED   = 8
              FILESIZE_NOT_ALLOWED    = 9
              HEADER_TOO_LONG         = 10
              DP_ERROR_CREATE         = 11
              DP_ERROR_SEND           = 12
              DP_ERROR_WRITE          = 13
              UNKNOWN_DP_ERROR        = 14
              ACCESS_DENIED           = 15
              DP_OUT_OF_MEMORY        = 16
              DISK_FULL               = 17
              DP_TIMEOUT              = 18
              FILE_NOT_FOUND          = 19
              DATAPROVIDER_EXCEPTION  = 20
              CONTROL_FLUSH_ERROR     = 21
              OTHERS                  = 22.
          IF SY-SUBRC <> 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ELSE.
            CALL METHOD CL_GUI_FRONTEND_SERVICES=>EXECUTE
              EXPORTING
                DOCUMENT               = P_CAMINHO
              EXCEPTIONS
                CNTL_ERROR             = 1
                ERROR_NO_GUI           = 2
                BAD_PARAMETER          = 3
                FILE_NOT_FOUND         = 4
                PATH_NOT_FOUND         = 5
                FILE_EXTENSION_UNKNOWN = 6
                ERROR_EXECUTE_FAILED   = 7
                SYNCHRONOUS_FAILED     = 8
                NOT_SUPPORTED_BY_GUI   = 9
                OTHERS                 = 10.
            IF SY-SUBRC <> 0.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ENDIF.

          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "on_function_selected
ENDCLASS.                    "lcl_toolbar_event IMPLEMENTATION

DATA: MR_TOOLBAR TYPE REF TO CL_GUI_TOOLBAR.
DATA: TOOLBAR_EVENT_RECEIVER TYPE REF TO LCL_TOOLBAR_EVENT.
DATA: ALV_TREE_VIEW_EVENT TYPE REF TO LCL_TREE_VIEW_EVENT.


"FIELD-SYMBOLS: <GT_SAIDA> TYPE ANY.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1103 OUTPUT.

  IF G_CUSTOM_CONTAINER2 IS INITIAL.
    PERFORM CREATE_CONTAINER_ALV_TREE.
  ENDIF.

  IF G_TREE3 IS INITIAL.
    PERFORM GERAR_RELATORIO_TREE_VIEW.
    PERFORM AJUSTA_TOTAIS.
    PERFORM CREATE_TREE_ALV_TREE3.
  ENDIF.

ENDMODULE.                 " STATUS_1103  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_ALV_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_ALV_TREE .

  DATA: URL(255) TYPE C.

* create a container for the tree control
  CREATE OBJECT G_CUSTOM_CONTAINER2
    EXPORTING
      CONTAINER_NAME              = 'TREE_CONTAINER'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.

  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

  CREATE OBJECT DG_DYNDOC_ID
    EXPORTING
      STYLE = 'ALV_GRID'.

  CREATE OBJECT DG_SPLITTER
    EXPORTING
      PARENT  = G_CUSTOM_CONTAINER2
      ROWS    = 2
      COLUMNS = 1.

  CALL METHOD DG_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = DG_PARENT_HTML.

  CREATE OBJECT DG_SPLITTER_2
    EXPORTING
      PARENT  = DG_PARENT_HTML
      ROWS    = 1
      COLUMNS = 2.

  CALL METHOD DG_SPLITTER_2->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = DG_PARENT_HTML1.

  CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
    EXPORTING
      ID    = 1
      WIDTH = 40.

  CALL METHOD DG_SPLITTER_2->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 2
    RECEIVING
      CONTAINER = DG_PARENT_HTML2.

  CREATE OBJECT PICTURE
    EXPORTING
      PARENT = DG_PARENT_HTML2.

  PERFORM F_PEGA_IMAGEM USING 'LOGO_NOVO' CHANGING URL.

  CALL METHOD PICTURE->LOAD_PICTURE_FROM_URL
    EXPORTING
      URL = URL.

  CALL METHOD PICTURE->SET_DISPLAY_MODE
    EXPORTING
      DISPLAY_MODE = PICTURE->DISPLAY_MODE_FIT_CENTER.

  CALL METHOD DG_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 2
      COLUMN    = 1
    RECEIVING
      CONTAINER = DG_PARENT_TREE.

  CALL METHOD DG_SPLITTER->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 16.

ENDFORM.                    " CREATE_CONTAINER_ALV_TREE

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1103 INPUT.

  CASE OK_CODE_1101.
    WHEN OK_EXCECAO.
      CALL SCREEN 1105 STARTING AT 30 08.
    WHEN OK_EXCECAO2.
      CALL SCREEN 1106 STARTING AT 30 08.

    WHEN OK_REG_EXC_CS.
      "CALL SCREEN 1107 STARTING AT 30 08.
      CALL SCREEN 1108 STARTING AT 005 03 ENDING AT 180 20.

    WHEN OK_ELIMINAD.
      PERFORM CONTAS_ELIMINADAS_DRE.

    WHEN OK_BACK.
      "**********************************************************************************
      """""""" Limpar Tela Voltar """""""""""""""""""""""""""""""""""""""""""""""""""""""
      PERFORM LIMPAR_TELA_1103_TREE.
      "**********************************************************************************
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1103  INPUT

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_1103_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_TELA_1103_TREE .

  IF G_TREE3 IS NOT INITIAL.
    CALL METHOD G_TREE3->FREE.
  ENDIF.

  IF PICTURE IS NOT INITIAL.
    CALL METHOD PICTURE->FREE.
  ENDIF.

  IF DG_PARENT_HTML2 IS NOT INITIAL.
    CALL METHOD DG_PARENT_HTML2->FREE.
  ENDIF.

  IF DG_HTML_CNTRL IS NOT INITIAL.
    CALL METHOD DG_HTML_CNTRL->FREE.
  ENDIF.

  IF DG_PARENT_HTML1 IS NOT INITIAL.
    CALL METHOD DG_PARENT_HTML1->FREE.
  ENDIF.

  IF DG_SPLITTER_2 IS NOT INITIAL.
    CALL METHOD DG_SPLITTER_2->FREE.
  ENDIF.

  IF DG_PARENT_HTML IS NOT INITIAL.
    CALL METHOD DG_PARENT_HTML->FREE.
  ENDIF.

  IF DG_PARENT_TREE IS NOT INITIAL.
    CALL METHOD DG_PARENT_TREE->FREE.
  ENDIF.

  IF DG_SPLITTER IS NOT INITIAL.
    CALL METHOD DG_SPLITTER->FREE.
  ENDIF.

  IF G_CUSTOM_CONTAINER2 IS NOT INITIAL.
    CALL METHOD G_CUSTOM_CONTAINER2->FREE.
  ENDIF.

  CLEAR: G_TREE3, G_CUSTOM_CONTAINER2, DG_DYNDOC_ID,
         DG_PARENT_HTML2, DG_PARENT_HTML1, DG_PARENT_HTML,
         DG_SPLITTER_2, DG_SPLITTER, DG_PARENT_TREE, PICTURE,
         DG_HTML_CNTRL.

ENDFORM.                    " LIMPAR_TELA_1103_TREE

*&---------------------------------------------------------------------*
*&      Form  CREATE_TREE_ALV_TREE3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_TREE_ALV_TREE3 .

  DATA: L_HIERARCHY_HEADER TYPE TREEV_HHDR,
        LT_LIST_COMMENTARY TYPE SLIS_T_LISTHEADER,
        L_LOGO             TYPE SDYDO_VALUE,
        LS_LINE            TYPE SLIS_LISTHEADER,
        LS_VARIANT         TYPE DISVARIANT,
        GT_FIELDCATALOG    TYPE LVC_T_FCAT,
        VG_MES(2),
        VG_ANO(4),
        LT_EVENTS          TYPE CNTL_SIMPLE_EVENTS,
        L_EVENT            TYPE CNTL_SIMPLE_EVENT,
        I_DEFAULT          TYPE CHAR01,
        WA_T001_B          TYPE T001.


  CREATE OBJECT G_TREE3
    EXPORTING
      PARENT                      = DG_PARENT_TREE
      NODE_SELECTION_MODE         = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_MULTIPLE
      ITEM_SELECTION              = SPACE
      NO_HTML_HEADER              = 'X'
      NO_TOOLBAR                  = ''
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      ILLEGAL_NODE_SELECTION_MODE = 5
      FAILED                      = 6
      ILLEGAL_COLUMN_NAME         = 7.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE X208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.

  CLEAR L_EVENT.
  L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  L_EVENT-APPL_EVENT = 'X'.
  APPEND L_EVENT TO LT_EVENTS.

  CALL METHOD G_TREE3->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS                    = LT_EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.

  CREATE OBJECT ALV_TREE_VIEW_EVENT.
  SET HANDLER ALV_TREE_VIEW_EVENT->ON_NODE_DOUBLE_CLICK FOR G_TREE3.

  L_HIERARCHY_HEADER-T_IMAGE = 'LOGO_NOVO'.
  L_HIERARCHY_HEADER-HEADING = TEXT-101. "'Hierarquia (Cabeçalho)'.
  L_HIERARCHY_HEADER-WIDTH   = 30.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = TEXT-005. "'Balanço Patrimonial'.
  APPEND LS_LINE TO LT_LIST_COMMENTARY.

  "Empresa p/ Plano de Contas do Razão
  SELECT * INTO TABLE IT_T001
    FROM T001
   WHERE BUKRS EQ P_BUKRS.

  SORT IT_T001 BY BUKRS.

  READ TABLE IT_T001 INDEX 1.
  CLEAR LS_LINE.
  LS_LINE-KEY  = TEXT-102. "'Empresa'.
  LS_LINE-TYP  = 'S'.
  LS_LINE-INFO = IT_T001-BUTXT.
  APPEND LS_LINE TO LT_LIST_COMMENTARY.

  IF P_CONSO IS NOT INITIAL.
    SELECT SINGLE * INTO WA_T001_B
      FROM T001
     WHERE BUKRS EQ P_BUKR2.

    CLEAR LS_LINE.
    LS_LINE-KEY  = TEXT-102. "'Empresa'.
    LS_LINE-TYP  = 'S'.
    LS_LINE-INFO = WA_T001_B-BUTXT.
    APPEND LS_LINE TO LT_LIST_COMMENTARY.
  ENDIF.

  LS_LINE-KEY  = TEXT-103. "'Período'.
  MOVE: P_MONAT TO VG_MES,
        P_GJAHR TO VG_ANO.
  CONCATENATE VG_MES '/' VG_ANO INTO LS_LINE-INFO.
  APPEND LS_LINE TO LT_LIST_COMMENTARY.

  CLEAR LS_LINE.
  LS_LINE-KEY  = TEXT-104. "'Moeda'.
  LS_LINE-TYP  = 'S'.
  LS_LINE-INFO = P_WAERS.
  APPEND LS_LINE TO LT_LIST_COMMENTARY.

  CLEAR LS_LINE.
  LS_LINE-KEY  = TEXT-105. "'Estrutura'.
  LS_LINE-TYP  = 'S'.
  CONCATENATE  P_ESTR '-' P_ESTT INTO LS_LINE-INFO SEPARATED BY SPACE.
  APPEND LS_LINE TO LT_LIST_COMMENTARY.

  CLEAR LS_LINE.
  LS_LINE-KEY  = TEXT-106. "'Data:'.
  LS_LINE-TYP  = 'S'.
  WRITE SY-DATUM TO LS_LINE-INFO.
  APPEND LS_LINE TO LT_LIST_COMMENTARY.

  L_LOGO = 'LOGO_NOVO'.

  PERFORM CATALOGO_TREE_ALV_TREE3 TABLES GT_FIELDCATALOG.

  IF VARIANT IS NOT INITIAL.
    MOVE VARIANT TO G_VARIANT-VARIANT.
    I_DEFAULT = ABAP_FALSE.
  ELSE.
    I_DEFAULT = ABAP_TRUE.
  ENDIF.

  CALL METHOD G_TREE3->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_STRUCTURE_NAME    = 'TY_ZGLT047_TREE'
      IS_HIERARCHY_HEADER = L_HIERARCHY_HEADER
      IT_LIST_COMMENTARY  = LT_LIST_COMMENTARY
      I_LOGO              = L_LOGO
      I_BACKGROUND_ID     = 'ALV_BACKGROUND'
      I_SAVE              = 'A'
      IS_VARIANT          = G_VARIANT
      I_DEFAULT           = I_DEFAULT
    CHANGING
      IT_OUTTAB           = IT_ZGLT047_TREE[]
      IT_FIELDCATALOG     = GT_FIELDCATALOG.

  DATA IT_NODE_KEY TYPE LVC_T_NKEY.

  PERFORM MOSTRA_TEXTO USING TEXT-063."'Montando Estrutura de Demonstrativo de Balanço Patrimonial'.
  PERFORM ESTRUTURA_SAIDA_ALV_TREE_VIEW TABLES IT_NODE_KEY.

* calculate totals
  CALL METHOD G_TREE3->UPDATE_CALCULATIONS.

* this method must be called to send the data to the frontend
  CALL METHOD G_TREE3->FRONTEND_UPDATE.

  CALL METHOD G_TREE3->EXPAND_NODES
    EXPORTING
      IT_NODE_KEY = IT_NODE_KEY.

  CALL METHOD G_TREE3->COLLAPSE_ALL_NODES.

  PERFORM CHANGE_TOOLBAR.

  PERFORM CRIA_HTML_CAB.

ENDFORM.                    " CREATE_TREE_ALV_TREE3

*&---------------------------------------------------------------------*
*&      Form  CATALOGO_TREE_ALV_TREE3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCATALOG  text
*----------------------------------------------------------------------*
FORM CATALOGO_TREE_ALV_TREE3  TABLES  IT_FIELDCATALOG STRUCTURE LVC_S_FCAT.

  DATA: FIELD       TYPE LVC_S_FCAT,
        VG_GJAHR    TYPE GJAHR,
        VG_MONAT    TYPE MONAT,
        VT_GJAHR(4),
        VT_MONAT(2).

  CLEAR: FIELD.
  MOVE P_GJAHR TO VT_GJAHR.
  MOVE P_MONAT TO VT_MONAT.
  CONCATENATE VT_MONAT '/' VT_GJAHR INTO FIELD-SCRTEXT_L.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'VALOR_01'.
  FIELD-TABNAME   = 'IT_ZGLT047_TREE'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 30.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_TRUE.
  FIELD-DATATYPE  = 'NUMC'.
  FIELD-JUST      = 'R'.
  APPEND FIELD TO IT_FIELDCATALOG.

  CLEAR: FIELD.
  IF P_CMONAT IS NOT INITIAL AND P_CGJAHR IS NOT INITIAL.
    MOVE P_CGJAHR TO VG_GJAHR.                      "/ Modificação CS22016000771
    MOVE P_CMONAT  TO VT_MONAT.
  ELSE.
    MOVE P_GJAHR TO VG_GJAHR.
    ADD -1 TO VG_GJAHR.
    MOVE P_MONAT  TO VT_MONAT.
  ENDIF.

  MOVE VG_GJAHR TO VT_GJAHR.

  CONCATENATE VT_MONAT '/' VT_GJAHR INTO FIELD-SCRTEXT_L.
  FIELD-COL_POS   = 3.
  FIELD-FIELDNAME = 'VALOR_02'.
  FIELD-TABNAME   = 'IT_ZGLT047_TREE'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 30.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_TRUE.
  FIELD-DATATYPE  = 'NUMC'.
  FIELD-JUST      = 'R'.
  APPEND FIELD TO IT_FIELDCATALOG.

  IF P_CMONAT IS INITIAL AND P_CGJAHR IS INITIAL. "/Modificação CS2016000771
    IF VT_MONAT NE '12'.
      CLEAR: FIELD.
      MOVE P_GJAHR TO VG_GJAHR.
      ADD -1 TO VG_GJAHR.
      MOVE VG_GJAHR TO VT_GJAHR.
      CONCATENATE  '12/' VT_GJAHR INTO FIELD-SCRTEXT_L.
      FIELD-COL_POS   = 2.
      FIELD-FIELDNAME = 'VALOR_03'.
      FIELD-TABNAME   = 'IT_ZGLT047_TREE'.
      FIELD-EMPHASIZE = 'K41'.
      FIELD-OUTPUTLEN = 30.
      FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
      FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
      FIELD-DO_SUM    = ABAP_TRUE.
      FIELD-DATATYPE  = 'NUMC'.
      FIELD-JUST      = 'R'.
      APPEND FIELD TO IT_FIELDCATALOG.
    ENDIF.
  ENDIF.

  CLEAR: FIELD.
  FIELD-COL_POS   = 4.
  FIELD-FIELDNAME = 'NIVEL'.
  FIELD-TABNAME   = 'IT_ZGLT047_TREE'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 10.
  FIELD-SCRTEXT_L = TEXT-107. "'Nível da Estrutura de Balanço Patrimonial'.
  FIELD-SCRTEXT_M = TEXT-108. "'Nível Estrutura'.
  FIELD-SCRTEXT_S = TEXT-108. "'Nível Estrutura'.
  APPEND FIELD TO IT_FIELDCATALOG.

  CLEAR: FIELD.
  FIELD-COL_POS   = 5.
  FIELD-FIELDNAME = 'DESNVL'.
  FIELD-TABNAME   = 'IT_ZGLT047_TREE'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 35.
  FIELD-SCRTEXT_L = TEXT-109. "'Descrição da Estrutura de Balanço Patrimonial'.
  FIELD-SCRTEXT_M = TEXT-110. "'Descrição Estrutura'.
  FIELD-SCRTEXT_S = TEXT-110. "'Descrição Estrutura'.
  APPEND FIELD TO IT_FIELDCATALOG.

  CLEAR: FIELD.
  FIELD-COL_POS   = 6.
  FIELD-FIELDNAME = 'COD_CLAS_NOT'.
  FIELD-TABNAME   = 'IT_ZGLT047_TREE'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 35.
  FIELD-SCRTEXT_L = TEXT-111. "'Nota de Classificação'.
  FIELD-SCRTEXT_M = TEXT-111. "'Nota de Classificação'.
  FIELD-SCRTEXT_S = TEXT-111. "'Nota de Classificação'.
  APPEND FIELD TO IT_FIELDCATALOG.

  CLEAR: FIELD.
  FIELD-COL_POS   = 7.
  FIELD-FIELDNAME = 'DESCR_NOTA'.
  FIELD-TABNAME   = 'IT_ZGLT047_TREE'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 35.
  FIELD-SCRTEXT_L = TEXT-112. "'Descrição da Nota de Classificação'.
  FIELD-SCRTEXT_M = TEXT-113. "'Desc. Nota Calss.'.
  FIELD-SCRTEXT_S = TEXT-113. "'Desc. Nota Calss.'.
  APPEND FIELD TO IT_FIELDCATALOG.

  CLEAR: FIELD.
  FIELD-COL_POS   = 8.
  FIELD-FIELDNAME = 'SAKNR'.
  FIELD-TABNAME   = 'IT_ZGLT047_TREE'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 35.
  FIELD-SCRTEXT_L = TEXT-114. "'Conta Razão'.
  FIELD-SCRTEXT_M = TEXT-114. "'Conta Razão'.
  FIELD-SCRTEXT_S = TEXT-114. "'Conta Razão'.
  FIELD-CONVEXIT  = 'ALPHA'.
  APPEND FIELD TO IT_FIELDCATALOG.

  CLEAR: FIELD.
  FIELD-COL_POS   = 9.
  FIELD-FIELDNAME = 'TEXTO'.
  FIELD-TABNAME   = 'IT_ZGLT047_TREE'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 35.
  FIELD-SCRTEXT_L = TEXT-115. "'Desc. Conta Razão'.
  FIELD-SCRTEXT_M = TEXT-115. "'Desc. Conta Razão'.
  FIELD-SCRTEXT_S = TEXT-115. "'Desc. Conta Razão'.
  APPEND FIELD TO IT_FIELDCATALOG.

ENDFORM.                    " CATALOGO_TREE_ALV_TREE3

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0190   text
*      <--P_URL  text
*----------------------------------------------------------------------*
FORM F_PEGA_IMAGEM  USING    NOME_LOGO
                    CHANGING URL.

  DATA: BEGIN OF GRAPHIC_TABLE OCCURS 0,
          LINE(255) TYPE X,
        END OF GRAPHIC_TABLE.
  DATA: L_GRAPHIC_XSTR TYPE XSTRING.
  DATA: GRAPHIC_SIZE   TYPE I.
  DATA: L_GRAPHIC_CONV TYPE I.
  DATA: L_GRAPHIC_OFFS TYPE I.

  REFRESH GRAPHIC_TABLE.
  CALL METHOD CL_SSF_XSF_UTILITIES=>GET_BDS_GRAPHIC_AS_BMP
    EXPORTING
      P_OBJECT = 'GRAPHICS'
      P_NAME   = NOME_LOGO
      P_ID     = 'BMAP'
      P_BTYPE  = 'BCOL'
    RECEIVING
      P_BMP    = L_GRAPHIC_XSTR.

  GRAPHIC_SIZE = XSTRLEN( L_GRAPHIC_XSTR ).
  L_GRAPHIC_CONV = GRAPHIC_SIZE.
  L_GRAPHIC_OFFS = 0.
  WHILE L_GRAPHIC_CONV > 255.
    GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(255).
    APPEND GRAPHIC_TABLE.
    L_GRAPHIC_OFFS = L_GRAPHIC_OFFS + 255.
    L_GRAPHIC_CONV = L_GRAPHIC_CONV - 255.
  ENDWHILE.
  GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(L_GRAPHIC_CONV).
  APPEND GRAPHIC_TABLE.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE     = 'IMAGE'
      SUBTYPE  = 'X-UNKNOWN'
      SIZE     = GRAPHIC_SIZE
      LIFETIME = 'T'
    TABLES
      DATA     = GRAPHIC_TABLE
    CHANGING
      URL      = URL.
ENDFORM.                    " F_PEGA_IMAGEM

*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONTAINER_HTML .

  DATA : DL_LENGTH        TYPE I,                           " Length
         DL_BACKGROUND_ID TYPE SDYDO_KEY VALUE SPACE. " Background_id

  IF DG_HTML_CNTRL IS INITIAL.
    CREATE OBJECT DG_HTML_CNTRL
      EXPORTING
        PARENT = DG_PARENT_HTML1.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      DOCUMENT = DG_DYNDOC_ID
      BOTTOM   = SPACE
    IMPORTING
      LENGTH   = DL_LENGTH.

  CALL METHOD DG_DYNDOC_ID->MERGE_DOCUMENT.

  CALL METHOD DG_DYNDOC_ID->SET_DOCUMENT_BACKGROUND
    EXPORTING
      PICTURE_ID = DL_BACKGROUND_ID.

  DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.

  CALL METHOD DG_DYNDOC_ID->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = DG_PARENT_HTML1
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.

ENDFORM.                    " CONTAINER_HTML

*&---------------------------------------------------------------------*
*&      Form  CRIA_HTML_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIA_HTML_CAB .

  DATA: TEXTO(40), VG_MES(2), VG_ANO(4),
        SDYDO_TEXT_ELEMENT(255),
        P_TEXT_TABLE            TYPE SDYDO_TEXT_TABLE,
        P_TEXT                  TYPE SDYDO_TEXT_ELEMENT,
        POSITION                TYPE I,
        WA_T001_B               TYPE T001.

  DATA: COLUMN         TYPE REF TO CL_DD_AREA,
        COLUMN_1       TYPE REF TO CL_DD_AREA,
        COLUMN_2       TYPE REF TO CL_DD_AREA,
        TABLE_ELEMENT  TYPE REF TO CL_DD_TABLE_ELEMENT,
        TABLE_ELEMENT2 TYPE REF TO CL_DD_TABLE_ELEMENT.

  CALL METHOD DG_DYNDOC_ID->INITIALIZE_DOCUMENT.

  CALL METHOD DG_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 1
      BORDER        = '0'
      WIDTH         = '100%'
    IMPORTING
      TABLE         = TABLE_ELEMENT.

  CALL METHOD TABLE_ELEMENT->ADD_COLUMN
    IMPORTING
      COLUMN = COLUMN.

  CALL METHOD TABLE_ELEMENT->SET_COLUMN_STYLE
    EXPORTING
      COL_NO    = 1
      SAP_ALIGN = 'CENTER'
      SAP_STYLE = CL_DD_DOCUMENT=>HEADING.

  P_TEXT = TEXT-005. "'Balanço Patrimonial'.
  CALL METHOD COLUMN->ADD_TEXT
    EXPORTING
      TEXT      = P_TEXT
      SAP_STYLE = 'HEADING'.

  CALL METHOD DG_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '100%'
    IMPORTING
      TABLE         = TABLE_ELEMENT2.

  CALL METHOD TABLE_ELEMENT2->ADD_COLUMN
    EXPORTING
      SAP_STYLE   = 'SAP_BOLD'
      STYLE_CLASS = 'SAP_BOLD'
    IMPORTING
      COLUMN      = COLUMN_1.

  CALL METHOD TABLE_ELEMENT2->ADD_COLUMN
    IMPORTING
      COLUMN = COLUMN_2.

  CALL METHOD TABLE_ELEMENT2->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 2
      SAP_ALIGN    = 'LEFT'
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM.

  SDYDO_TEXT_ELEMENT = TEXT-116. "'Empresa: '.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  IF P_CONSO IS NOT INITIAL.
    SDYDO_TEXT_ELEMENT = TEXT-116. "'Empresa: '.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  ENDIF.

  SDYDO_TEXT_ELEMENT = TEXT-117. "'Período: '.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = TEXT-118. "'Moeda: '.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = TEXT-119. "'Estrutura: '.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = TEXT-120. "'Data: '.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  CALL METHOD COLUMN_1->ADD_TEXT
    EXPORTING
      TEXT_TABLE = P_TEXT_TABLE
      FIX_LINES  = 'X'.

  CLEAR: P_TEXT_TABLE.

  READ TABLE IT_T001 WITH KEY BUKRS = P_BUKRS.
  SDYDO_TEXT_ELEMENT = IT_T001-BUTXT.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  IF P_CONSO IS NOT INITIAL.
*    SELECT SINGLE * INTO WA_T001_B
*      FROM T001
*     WHERE BUKRS EQ P_BUKR2.
    READ TABLE IT_T001 WITH KEY BUKRS = P_BUKR2.
    SDYDO_TEXT_ELEMENT = IT_T001-BUTXT.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  ENDIF.

  MOVE: P_MONAT TO VG_MES,
        P_GJAHR TO VG_ANO.
  CONCATENATE VG_MES '/' VG_ANO INTO TEXTO.
  SDYDO_TEXT_ELEMENT = TEXTO.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = P_WAERS.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  CONCATENATE P_ESTR '-' P_ESTT INTO TEXTO SEPARATED BY SPACE.
  SDYDO_TEXT_ELEMENT = TEXTO.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  WRITE SY-DATUM TO TEXTO.
  SDYDO_TEXT_ELEMENT = TEXTO.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  CALL METHOD COLUMN_2->ADD_TEXT
    EXPORTING
      TEXT_TABLE = P_TEXT_TABLE
      FIX_LINES  = 'X'.

  PERFORM CONTAINER_HTML.

ENDFORM.                    " CRIA_HTML_CAB

*&---------------------------------------------------------------------*
*&      Form  change_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_TOOLBAR.

* get toolbar control
  CALL METHOD G_TREE3->GET_TOOLBAR_OBJECT
    IMPORTING
      ER_TOOLBAR = MR_TOOLBAR.

  CHECK NOT MR_TOOLBAR IS INITIAL.

* add seperator to toolbar
  CALL METHOD MR_TOOLBAR->ADD_BUTTON
    EXPORTING
      FCODE     = ''
      ICON      = ''
      BUTN_TYPE = CNTB_BTYPE_SEP
      TEXT      = ''
      QUICKINFO = 'Seperator'.                              "#EC NOTEXT

* add Standard Button to toolbar (for Delete Subtree)
  CALL METHOD MR_TOOLBAR->ADD_BUTTON
    EXPORTING
      FCODE     = 'EXCEL'
      ICON      = ICON_IMPORT
      BUTN_TYPE = CNTB_BTYPE_BUTTON
      TEXT      = ''
      QUICKINFO = TEXT-121. "'Gerar Excel'.

  CREATE OBJECT TOOLBAR_EVENT_RECEIVER.
  SET HANDLER TOOLBAR_EVENT_RECEIVER->ON_FUNCTION_SELECTED FOR MR_TOOLBAR.

ENDFORM.                               " change_toolbar

*&---------------------------------------------------------------------*
*&      Form  GERAR_RELATORIO_TREE_VIEW
*&---------------------------------------------------------------------*
*       Busca Informações de Contas Razão da Estrutra Selecionada
*----------------------------------------------------------------------*
FORM GERAR_RELATORIO_TREE_VIEW .

  DATA: IT_SALDOS_ATUAL    TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
        IT_SALDOS_ANTER    TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
        IT_CONTAS          TYPE ZCT_EMP_CONTAS,
        WA_CONTAS          TYPE ZLC_EMP_CONTAS,
        VG_GJAHR           TYPE GJAHR,
        VG_MONAT           TYPE MONAT,
        VG_MONAT2          TYPE MONAT,
        WA_ZGLT061         TYPE ZGLT061,
        WA_ZGLT061_B       TYPE ZGLT061,
        IT_DRE_RESULT1     TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        IT_DRE_RESULT2     TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        IT_DRE_RESULT3     TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        "IT_DRE_RESULT1_B TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        "IT_DRE_RESULT2_B TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        "IT_DRE_RESULT3_B TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        VG_VLR_ACM_01      TYPE ZWRBTR_DRE_ACM,
        VG_VLR_ACM_02      TYPE ZWRBTR_DRE_ACM,
        VG_VLR_ACM_03      TYPE ZWRBTR_DRE_ACM,
        "VG_VLR_ACM_01_B  TYPE ZWRBTR_DRE_ACM,
        "VG_VLR_ACM_02_B  TYPE ZWRBTR_DRE_ACM,
        "VG_VLR_ACM_03_B  TYPE ZWRBTR_DRE_ACM,
        CK_BUSCA_DRE_01(1),
        CK_BUSCA_DRE_02(1),
        CK_BUSCA_DRE_03(1),
        VG_VLR_LUCRO       TYPE HSLXX12,
        VG_VLR_PREJU       TYPE HSLXX12,
        VG_LINES           TYPE I,
        LC_046             TYPE ZGLT046,
        IT_SKA1            TYPE TABLE OF SKA1 WITH HEADER LINE,
        ITZGLT043A         TYPE TABLE OF ZGLT043A WITH HEADER LINE,
        ITZGLT043B         TYPE TABLE OF ZGLT043B WITH HEADER LINE,
        IT_ZGLT041_A       TYPE TABLE OF ZGLT041  WITH HEADER LINE,
        IT_ZGLT041_B       TYPE TABLE OF ZGLT041  WITH HEADER LINE,
        IT_ZGLT057         TYPE TABLE OF ZGLT057  WITH HEADER LINE,
        IT_ZGLT060         TYPE TABLE OF ZGLT060  WITH HEADER LINE.
  "WA_ZGLT061         TYPE ZGLT061.

  DATA: REFE1 TYPE HSLXX12,
        REFE2 TYPE HSLXX12,
        REFE3 TYPE HSLXX12.

  CLEAR: IT_CONTAS, IT_SKA1.

  PERFORM ATUALIZA_TABELAS_CONSULTAS USING P_ESTR P_BUKRS P_BUKR2." P_GJAHR.  "/Modificação CS2017000372

  SELECT SINGLE * INTO LC_046
    FROM ZGLT046
   WHERE VERSN EQ P_ESTR.

  IF P_CONSO IS INITIAL.
    DELETE IT_ZGLT041 WHERE BUKRS NE P_BUKRS.
  ELSEIF P_BUKR2 IS NOT INITIAL.
    DELETE IT_ZGLT041 WHERE ( BUKRS NE P_BUKRS ) AND ( BUKRS NE P_BUKR2 ).
  ENDIF.

*  DELETE IT_ZGLT041 WHERE GJAHR NE P_GJAHR. "/Modificação CS2017000372

  IF P_VCSV IS NOT INITIAL.
    READ TABLE IT_ZGLT041 WITH KEY BUKRS = P_BUKRS.
    WA_CONTAS-BUKRS = IT_ZGLT041-BUKRS.
    WA_CONTAS-SAKNR = '*'.
    APPEND WA_CONTAS TO IT_CONTAS.

    "Consolidação """"""""""""""""""""""""""""""""""""""""""""""""""""
    IF P_CONSO IS NOT INITIAL.
      READ TABLE IT_ZGLT041 WITH KEY BUKRS = P_BUKR2.
      WA_CONTAS-BUKRS = IT_ZGLT041-BUKRS.
      WA_CONTAS-SAKNR = '*'.
      APPEND WA_CONTAS TO IT_CONTAS.
    ENDIF.
    "Consolidação """"""""""""""""""""""""""""""""""""""""""""""""""""

  ELSE.
    LOOP AT IT_ZGLT041 WHERE BUKRS EQ P_BUKRS.
      WA_CONTAS-BUKRS = IT_ZGLT041-BUKRS.
      WA_CONTAS-SAKNR = IT_ZGLT041-SAKNR.
      APPEND WA_CONTAS TO IT_CONTAS.
    ENDLOOP.

    "Consolidação """"""""""""""""""""""""""""""""""""""""""""""""""""
    IF P_CONSO IS NOT INITIAL.
      LOOP AT IT_ZGLT041 WHERE BUKRS EQ P_BUKR2.
        WA_CONTAS-BUKRS = IT_ZGLT041-BUKRS.
        WA_CONTAS-SAKNR = IT_ZGLT041-SAKNR.
        APPEND WA_CONTAS TO IT_CONTAS.
      ENDLOOP.
    ENDIF.
    "Consolidação """"""""""""""""""""""""""""""""""""""""""""""""""""
  ENDIF.

  SELECT SINGLE * INTO WA_ZGLT061 FROM ZGLT061 WHERE BUKRS EQ P_BUKRS AND GJAHR EQ P_GJAHR.

  "Consolidação """"""""""""""""""""""""""""""""""""""""""""""""""""
  IF P_CONSO IS NOT INITIAL.
    SELECT SINGLE * INTO WA_ZGLT061_B FROM ZGLT061 WHERE BUKRS EQ P_BUKR2 AND GJAHR EQ P_GJAHR.

    SELECT *
      INTO TABLE IT_ZGLT057
      FROM ZGLT057
     WHERE BUKRS_1 EQ P_BUKRS
       AND BUKRS_2 EQ P_BUKR2
       AND VERSN_1 EQ WA_ZGLT061-VERSN
       AND VERSN_2 EQ WA_ZGLT061_B-VERSN.

    READ TABLE IT_ZGLT057 INDEX 1.

    SELECT *
      INTO TABLE IT_ZGLT060
      FROM ZGLT060
     WHERE BUKRS1 EQ P_BUKRS
       AND WAERS  EQ P_WAERS.

    SELECT *
      APPENDING TABLE IT_ZGLT060
      FROM ZGLT060
     WHERE BUKRS1 EQ P_BUKR2
       AND WAERS  EQ P_WAERS.

    SORT IT_ZGLT060 BY BUKRS1 CTA_LAN.

  ENDIF.
  "Consolidação """"""""""""""""""""""""""""""""""""""""""""""""""""

  MOVE P_GJAHR TO VG_GJAHR.

  PERFORM MOSTRA_TEXTO USING TEXT-064."'Pesquisa: Saldo Contábeis do 1º Período'.

  CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
    EXPORTING
      RYEAR                = VG_GJAHR
      WAERS                = P_WAERS
      CONTAS               = IT_CONTAS
      P_GERAR_SOC_PARCEIRA = P_CONSO
    TABLES
      IT_SALDOS            = IT_SALDOS_ATUAL
    EXCEPTIONS
      MOEDA_NAO_ADM        = 1
      ERRO_LEDGER          = 2
      OTHERS               = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  PERFORM VERIFICA_LANC_LUCRO_PREJUIZO USING P_BUKRS VG_GJAHR P_MONAT
    CHANGING CK_BUSCA_DRE_01.

  IF CK_BUSCA_DRE_01 EQ ABAP_TRUE.
    PERFORM MOSTRA_TEXTO USING TEXT-065. "'Pesquisa: Saldo Contábeis DRE do 1º Período'.

    SELECT SINGLE *
      INTO WA_ZGLT061
      FROM ZGLT061
     WHERE BUKRS EQ P_BUKRS
       AND GJAHR EQ VG_GJAHR.

    CALL FUNCTION 'Z_PESQUISA_DRE'
      EXPORTING
        I_BUKRS      = P_BUKRS
        I_GJAHR      = VG_GJAHR
        I_MONATI     = 01
        I_MONATF     = P_MONAT
        I_VERSN      = WA_ZGLT061-VERSN
        I_WAERS      = P_WAERS
      TABLES
        T_ZGL030_EST = IT_DRE_RESULT1.

    DELETE IT_DRE_RESULT1 WHERE SAKNR IS INITIAL.

  ENDIF.

  IF P_CMONAT IS NOT INITIAL AND P_CGJAHR IS NOT INITIAL.
    MOVE P_CGJAHR TO VG_GJAHR.                                "/Modificação CS2016000771
    MOVE P_CMONAT TO VG_MONAT2.                               "/Modificação CS2016000771
  ELSE.
    ADD -1 TO VG_GJAHR.
    MOVE P_MONAT TO VG_MONAT2.                                "/Modificação CS2016000771
  ENDIF.

  PERFORM MOSTRA_TEXTO USING TEXT-066. "'Pesquisa: Saldo Contábeis do 2/3º Período'.

  CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
    EXPORTING
      RYEAR                = VG_GJAHR
      WAERS                = P_WAERS
      CONTAS               = IT_CONTAS
      P_GERAR_SOC_PARCEIRA = P_CONSO
    TABLES
      IT_SALDOS            = IT_SALDOS_ANTER
    EXCEPTIONS
      MOEDA_NAO_ADM        = 1
      ERRO_LEDGER          = 2
      OTHERS               = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  PERFORM VERIFICA_LANC_LUCRO_PREJUIZO USING P_BUKRS VG_GJAHR P_MONAT
    CHANGING CK_BUSCA_DRE_02.

  IF CK_BUSCA_DRE_02 EQ ABAP_TRUE.
    PERFORM MOSTRA_TEXTO USING TEXT-067. "'Pesquisa: Saldo Contábeis DRE do 2º Período'.

    SELECT SINGLE *
      INTO WA_ZGLT061
      FROM ZGLT061
     WHERE BUKRS EQ P_BUKRS
       AND GJAHR EQ VG_GJAHR.

    CALL FUNCTION 'Z_PESQUISA_DRE'
      EXPORTING
        I_BUKRS      = P_BUKRS
        I_GJAHR      = VG_GJAHR
        I_MONATI     = 01
        I_MONATF     = VG_MONAT2 "P_MONAT
        I_VERSN      = WA_ZGLT061-VERSN
        I_WAERS      = P_WAERS
      TABLES
        T_ZGL030_EST = IT_DRE_RESULT2.

    DELETE IT_DRE_RESULT2 WHERE SAKNR IS INITIAL.

  ENDIF.

  PERFORM VERIFICA_LANC_LUCRO_PREJUIZO USING P_BUKRS VG_GJAHR 12
    CHANGING CK_BUSCA_DRE_03.

  IF CK_BUSCA_DRE_03 EQ ABAP_TRUE.

    IF P_MONAT EQ 12.
      MOVE IT_DRE_RESULT2[] TO IT_DRE_RESULT3[].
    ELSE.

      PERFORM MOSTRA_TEXTO USING TEXT-068. "'Pesquisa: Saldo Contábeis DRE do 3º Período'.

      SELECT SINGLE *
        INTO WA_ZGLT061
        FROM ZGLT061
       WHERE BUKRS EQ P_BUKRS
         AND GJAHR EQ VG_GJAHR.

      CALL FUNCTION 'Z_PESQUISA_DRE'
        EXPORTING
          I_BUKRS      = P_BUKRS
          I_GJAHR      = VG_GJAHR
          I_MONATI     = 01
          I_MONATF     = 12
          I_VERSN      = WA_ZGLT061-VERSN
          I_WAERS      = P_WAERS
        TABLES
          T_ZGL030_EST = IT_DRE_RESULT3.

      DELETE IT_DRE_RESULT3 WHERE SAKNR IS INITIAL.

    ENDIF.
  ENDIF.

  VG_MONAT = P_MONAT.

  IF VG_MONAT EQ 12.
    VG_MONAT = 15.
  ENDIF.

  IF VG_MONAT2 EQ 12.
    VG_MONAT2 = 15.     "/Modificação CS2016000771
  ENDIF.

  CLEAR: IT_1103_TOTAIS[], IT_REMOVIDO_CONS[], IT_REMOVIDO_CONA[].

  DESCRIBE TABLE IT_ZGLT047 LINES VG_LINES.

  LOOP AT IT_ZGLT047.

    PERFORM MOSTRA_TEXTO_P USING TEXT-069 VG_LINES SY-TABIX. "'Totalizando Demonstrativo'

    LOOP AT IT_ZGLT049 WHERE NIVEL EQ IT_ZGLT047-NIVEL.
      LOOP AT IT_ZGLT041 WHERE COD_CLAS_BAL  EQ IT_ZGLT049-COD_CLAS_BAL
                           AND COD_CLAS_NOT2 EQ IT_ZGLT049-COD_CLAS_NOT.

        CLEAR: IT_1103_TOTAIS.

        IT_1103_TOTAIS-COD_CLAS_BAL = IT_ZGLT049-COD_CLAS_BAL.
        IT_1103_TOTAIS-COD_CLAS_NOT = IT_ZGLT049-COD_CLAS_NOT.

        IT_1103_TOTAIS-NIVEL        = IT_ZGLT047-NIVEL.
        IT_1103_TOTAIS-SAKNR        = IT_ZGLT041-SAKNR.

        "Pega Saldo Inicial do Saldo Mês Solicitado
        LOOP AT IT_SALDOS_ATUAL WHERE RACCT  EQ IT_ZGLT041-SAKNR
                                  AND RBUKRS EQ IT_ZGLT041-BUKRS.
          SY-SUBRC = 0.
          "Caso Seja Consiliação, bular registros de sociedade parceira parametrizado
          "Caso Seja Consiliação, bular registros de sociedade parceira parametrizado
          IF P_CONSO IS NOT INITIAL.
            READ TABLE IT_ZGLT060 WITH KEY BUKRS1  = IT_ZGLT041-BUKRS
                                           CTA_LAN = IT_ZGLT041-SAKNR
                                           BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              IF IT_ZGLT060-CK_NSOC_PARCEIRA EQ ABAP_TRUE.
                APPEND IT_SALDOS_ATUAL TO IT_REMOVIDO_CONS.
                SY-SUBRC = 4.
              ELSE.
                IF ( ( IT_SALDOS_ATUAL-RASSC EQ IT_ZGLT057-VBUND_1 ) AND ( IT_ZGLT041-BUKRS EQ IT_ZGLT057-BUKRS_1 ) ).
                  APPEND IT_SALDOS_ATUAL TO IT_REMOVIDO_CONS.
                  SY-SUBRC = 4.
                ENDIF.
                IF ( ( IT_SALDOS_ATUAL-RASSC EQ IT_ZGLT057-VBUND_2 ) AND ( IT_ZGLT041-BUKRS EQ IT_ZGLT057-BUKRS_2 ) ).
                  APPEND IT_SALDOS_ATUAL TO IT_REMOVIDO_CONS.
                  SY-SUBRC = 4.
                ENDIF.
              ENDIF.
            ELSE.
              SY-SUBRC = 0.
            ENDIF.
          ENDIF.
          "Caso Seja Consiliação, bular registros de sociedade parceira parametrizado
          "Caso Seja Consiliação, bular registros de sociedade parceira parametrizado

          IF SY-SUBRC IS INITIAL.
            IT_1103_TOTAIS-VALOR_01 = IT_1103_TOTAIS-VALOR_01 + IT_SALDOS_ATUAL-SLVT.

            "Percorre Meses até Mês Selecioanado (Mês Solicitado/Mês Anterior).
            DO VG_MONAT TIMES
              VARYING REFE1 FROM IT_SALDOS_ATUAL-SL01 NEXT IT_SALDOS_ATUAL-SL02.
              IT_1103_TOTAIS-VALOR_01 = IT_1103_TOTAIS-VALOR_01 + REFE1.
            ENDDO.
          ENDIF.
        ENDLOOP.

        "Pega Saldo Inicial do Saldo Mês Anterior
        LOOP AT IT_SALDOS_ANTER WHERE RACCT  EQ IT_ZGLT041-SAKNR
                                  AND RBUKRS EQ IT_ZGLT041-BUKRS.

          SY-SUBRC = 0.

          "Caso Seja Consiliação, bular registros de sociedade parceira parametrizado
          "Caso Seja Consiliação, bular registros de sociedade parceira parametrizado
          IF P_CONSO IS NOT INITIAL.

            READ TABLE IT_ZGLT060 WITH KEY BUKRS1  = IT_ZGLT041-BUKRS
                                           CTA_LAN = IT_ZGLT041-SAKNR
                                           BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              IF IT_ZGLT060-CK_NSOC_PARCEIRA EQ ABAP_TRUE.
                APPEND IT_SALDOS_ANTER TO IT_REMOVIDO_CONA.
                SY-SUBRC = 4.
              ELSE.
                IF ( IT_SALDOS_ANTER-RASSC EQ IT_ZGLT057-VBUND_1 ) AND
                   ( IT_ZGLT041-BUKRS      EQ IT_ZGLT057-BUKRS_1 ).
                  APPEND IT_SALDOS_ANTER TO IT_REMOVIDO_CONA.
                  SY-SUBRC = 4.
                ENDIF.
                IF ( IT_SALDOS_ANTER-RASSC EQ IT_ZGLT057-VBUND_2 ) AND
                   ( IT_ZGLT041-BUKRS      EQ IT_ZGLT057-BUKRS_2 ).
                  APPEND IT_SALDOS_ANTER TO IT_REMOVIDO_CONA.
                  SY-SUBRC = 4.
                ENDIF.
              ENDIF.
            ELSE.
              SY-SUBRC = 0.
            ENDIF.
          ENDIF.
          "Caso Seja Consiliação, bular registros de sociedade parceira parametrizado
          "Caso Seja Consiliação, bular registros de sociedade parceira parametrizado

          IF SY-SUBRC IS INITIAL.
            IT_1103_TOTAIS-VALOR_02 = IT_1103_TOTAIS-VALOR_02 + IT_SALDOS_ANTER-SLVT.
            IT_1103_TOTAIS-VALOR_03 = IT_1103_TOTAIS-VALOR_03 + IT_SALDOS_ANTER-SLVT.

            "Percorre Meses até Mês Selecioanado (Mês Solicitado/Mês Anterior).
            DO VG_MONAT2 TIMES                                                                      "/ Modificação CS2016000771
            VARYING REFE2 FROM IT_SALDOS_ANTER-SL01 NEXT IT_SALDOS_ANTER-SL02.
              IT_1103_TOTAIS-VALOR_02 = IT_1103_TOTAIS-VALOR_02 + REFE2.
            ENDDO.

            "Percorre Meses até Ultimo Mês do Periodo Anterior.
            DO 15 TIMES
              VARYING REFE3 FROM IT_SALDOS_ANTER-SL01 NEXT IT_SALDOS_ANTER-SL02.
              IT_1103_TOTAIS-VALOR_03 = IT_1103_TOTAIS-VALOR_03 + REFE3.
            ENDDO.
          ENDIF.
        ENDLOOP.

        COLLECT IT_1103_TOTAIS.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  "Busca Saldo DRE 01
  VG_VLR_ACM_01 = 0.
  "VG_VLR_ACM_01_B = 0.

  DATA: WT_T001 TYPE T001.

  IF CK_BUSCA_DRE_01 EQ ABAP_TRUE.
    "Buscar Saldo DRE
    PERFORM MOSTRA_TEXTO USING TEXT-070. "'Resultado DRE 1º Período'.

    LOOP AT IT_DRE_RESULT1.
      ADD IT_DRE_RESULT1-VLR_ACM TO VG_VLR_ACM_01.
    ENDLOOP.

*    LOOP AT IT_DRE_RESULT1_B.
*      ADD IT_DRE_RESULT1_B-VLR_ACM TO VG_VLR_ACM_01_B.
*    ENDLOOP.

    SELECT SINGLE * INTO WT_T001
      FROM T001
     WHERE BUKRS EQ P_BUKRS.

    IF ( WT_T001-WAERS EQ 'PYG' ) AND ( P_WAERS EQ 'PYG' ).
      VG_VLR_ACM_01   = VG_VLR_ACM_01 / 100.
      "VG_VLR_ACM_01_B = VG_VLR_ACM_01_B / 100.
    ENDIF.

  ENDIF.

  "Busca Saldo DRE 02
  VG_VLR_ACM_02 = 0.
  "VG_VLR_ACM_02_B = 0.

  IF CK_BUSCA_DRE_02 EQ ABAP_TRUE.
    "Buscar Saldo DRE
    PERFORM MOSTRA_TEXTO USING TEXT-071. "'Resultado DRE 2º Período'.

    LOOP AT IT_DRE_RESULT2.
      ADD IT_DRE_RESULT2-VLR_ACM TO VG_VLR_ACM_02.
    ENDLOOP.

    SELECT SINGLE * INTO WT_T001
      FROM T001
     WHERE BUKRS EQ P_BUKRS.

    IF ( WT_T001-WAERS EQ 'PYG' ) AND ( P_WAERS EQ 'PYG' ).
      VG_VLR_ACM_02   = VG_VLR_ACM_02 / 100.
    ENDIF.

  ENDIF.

  "Busca Saldo DRE 03
  VG_VLR_ACM_03 = 0.
  "VG_VLR_ACM_03_B = 0.

  IF CK_BUSCA_DRE_03 EQ ABAP_TRUE.
    "Buscar Saldo DRE
    PERFORM MOSTRA_TEXTO USING TEXT-072. "'Resultado DRE 3º Período'.

    LOOP AT IT_DRE_RESULT3.
      ADD IT_DRE_RESULT3-VLR_ACM TO VG_VLR_ACM_03.
    ENDLOOP.

    SELECT SINGLE * INTO WT_T001
      FROM T001
     WHERE BUKRS EQ P_BUKRS.

    IF ( WT_T001-WAERS EQ 'PYG' ) AND ( P_WAERS EQ 'PYG' ).
      VG_VLR_ACM_03   = VG_VLR_ACM_03 / 100.
    ENDIF.

  ENDIF.

  "Ajuste de Conta Razão - Lucro/Prejuizo 01
  IF CK_BUSCA_DRE_01 EQ ABAP_FALSE AND VG_MONAT EQ 12.

    PERFORM MOSTRA_TEXTO USING TEXT-073. "'Ajuste de Conta Razão - Lucro/Prejuizo 01'.

    LOOP AT IT_ZGLT041 WHERE SAKNR EQ IT_ZGLT046_ALV-SAKNR_LUCRO OR
                             SAKNR EQ IT_ZGLT046_ALV-SAKNR_PREJUIZO.
      "Buscar período 16 dos Saldos Contábeis.
      READ TABLE IT_1103_TOTAIS WITH KEY COD_CLAS_BAL = IT_ZGLT041-COD_CLAS_BAL
                                         COD_CLAS_NOT = IT_ZGLT041-COD_CLAS_NOT
                                         SAKNR        = IT_ZGLT041-SAKNR.
      IF SY-SUBRC IS INITIAL.
        IT_1103_TOTAIS-VALOR_01 = 0.
        IT_1103_TOTAIS-VALOR_02 = 0.
        IT_1103_TOTAIS-VALOR_03 = 0.
        READ TABLE IT_SALDOS_ATUAL WITH KEY RACCT = IT_ZGLT041-SAKNR.
        IF SY-SUBRC IS INITIAL.
          IT_1103_TOTAIS-VALOR_01 = IT_SALDOS_ATUAL-SL16.
          COLLECT IT_1103_TOTAIS.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Ajuste de Conta Razão - Lucro/Prejuizo 02/03
  IF ( CK_BUSCA_DRE_02 EQ ABAP_FALSE OR CK_BUSCA_DRE_03 EQ ABAP_FALSE ) AND ( VG_MONAT2 EQ 12 ).

    PERFORM MOSTRA_TEXTO USING TEXT-074. "'Ajuste de Conta Razão - Lucro/Prejuizo 02/03'.

    LOOP AT IT_ZGLT041 WHERE SAKNR EQ IT_ZGLT046_ALV-SAKNR_LUCRO OR
                             SAKNR EQ IT_ZGLT046_ALV-SAKNR_PREJUIZO.
      "Buscar período 16 dos Saldos Contábeis.
      READ TABLE IT_1103_TOTAIS WITH KEY COD_CLAS_BAL = IT_ZGLT041-COD_CLAS_BAL
                                         COD_CLAS_NOT = IT_ZGLT041-COD_CLAS_NOT
                                         SAKNR        = IT_ZGLT041-SAKNR.
      IF SY-SUBRC IS INITIAL.
        IT_1103_TOTAIS-VALOR_01 = 0.
        IT_1103_TOTAIS-VALOR_02 = 0.
        IT_1103_TOTAIS-VALOR_03 = 0.
        READ TABLE IT_SALDOS_ANTER WITH KEY RACCT = IT_ZGLT041-SAKNR.
        IF SY-SUBRC IS INITIAL.
          IF CK_BUSCA_DRE_02 EQ ABAP_FALSE.
            IT_1103_TOTAIS-VALOR_02 = IT_SALDOS_ANTER-SL16.
          ENDIF.
          IF CK_BUSCA_DRE_03 EQ ABAP_FALSE.
            IT_1103_TOTAIS-VALOR_03 = IT_SALDOS_ANTER-SL16.
          ENDIF.
          COLLECT IT_1103_TOTAIS.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.


  IF ( VG_VLR_ACM_01 NE 0 ). "OR ( VG_VLR_ACM_01_B NE 0 ).

    PERFORM MOSTRA_TEXTO USING TEXT-075. "'Ajuste - Lucro/Prejuizo 01'.

    VG_VLR_LUCRO = 0.
    VG_VLR_PREJU = 0.

    READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      ADD IT_1103_TOTAIS-VALOR_01 TO VG_VLR_LUCRO.
    ENDIF.

    READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_PREJUIZO.
    IF SY-SUBRC IS INITIAL.
      ADD IT_1103_TOTAIS-VALOR_01 TO VG_VLR_PREJU.
    ENDIF.

    "IF VG_VLR_ACM_01 LT 0.
    "DRE Negativa é Lucro
    ADD VG_VLR_ACM_01 TO VG_VLR_LUCRO.
    "ELSE.
    "  "DRE Positiva é Prejuizo
    "  ADD VG_VLR_ACM_01 TO VG_VLR_PREJU.
    "ENDIF.

*    IF VG_VLR_ACM_01_B LT 0.
*      "DRE Negativa é Lucro
*      ADD VG_VLR_ACM_01_B TO VG_VLR_LUCRO.
*    ELSE.
*      "DRE Positiva é Prejuizo
*      ADD VG_VLR_ACM_01_B TO VG_VLR_PREJU.
*    ENDIF.

    "Prejuizo e Lucro maior que 0 (zero)
*    IF ABS( VG_VLR_LUCRO ) GE VG_VLR_PREJU.
*      ADD VG_VLR_PREJU TO VG_VLR_LUCRO.
*      VG_VLR_PREJU = 0.
*    ELSE.
*      ADD VG_VLR_LUCRO TO VG_VLR_PREJU.
*      VG_VLR_LUCRO = 0.
*    ENDIF.

    READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      IT_1103_TOTAIS-VALOR_01 = VG_VLR_LUCRO.
      MODIFY IT_1103_TOTAIS INDEX SY-TABIX TRANSPORTING VALOR_01.
    ENDIF.

    "READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_PREJUIZO.
    "IF SY-SUBRC IS INITIAL.
    "  IT_1103_TOTAIS-VALOR_01 = VG_VLR_PREJU.
    "  MODIFY IT_1103_TOTAIS INDEX SY-TABIX TRANSPORTING VALOR_01.
    "ENDIF.

  ENDIF.

  IF ( VG_VLR_ACM_02 NE 0 ). " OR ( VG_VLR_ACM_02_B NE 0 ).

    PERFORM MOSTRA_TEXTO USING TEXT-076. "'Ajuste - Lucro/Prejuizo 02'.

    VG_VLR_LUCRO = 0.
    VG_VLR_PREJU = 0.

    READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      ADD IT_1103_TOTAIS-VALOR_02 TO VG_VLR_LUCRO.
    ENDIF.

    READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_PREJUIZO.
    IF SY-SUBRC IS INITIAL.
      ADD IT_1103_TOTAIS-VALOR_02 TO VG_VLR_PREJU.
    ENDIF.

    "IF VG_VLR_ACM_02 LT 0.
    "  "DRE Negativa é Lucro
    ADD VG_VLR_ACM_02 TO VG_VLR_LUCRO.
    "ELSE.
    "  "DRE Positiva é Prejuizo
    "  ADD VG_VLR_ACM_02 TO VG_VLR_PREJU.
    "ENDIF.

*    IF VG_VLR_ACM_02_B LT 0.
*      "DRE Negativa é Lucro
*      ADD VG_VLR_ACM_02_B TO VG_VLR_LUCRO.
*    ELSE.
*      "DRE Positiva é Prejuizo
*      ADD VG_VLR_ACM_02_B TO VG_VLR_PREJU.
*    ENDIF.

    "Prejuizo e Lucro maior que 0 (zero)
    "IF ABS( VG_VLR_LUCRO ) GE VG_VLR_PREJU.
    "  ADD VG_VLR_PREJU TO VG_VLR_LUCRO.
    "  VG_VLR_PREJU = 0.
    "ELSE.
    "  ADD VG_VLR_LUCRO TO VG_VLR_PREJU.
    "  VG_VLR_LUCRO = 0.
    "ENDIF.

    READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      IT_1103_TOTAIS-VALOR_02 = VG_VLR_LUCRO.
      MODIFY IT_1103_TOTAIS INDEX SY-TABIX TRANSPORTING VALOR_02.
    ENDIF.

*    READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_PREJUIZO.
*    IF SY-SUBRC IS INITIAL.
*      IT_1103_TOTAIS-VALOR_02 = VG_VLR_PREJU.
*      MODIFY IT_1103_TOTAIS INDEX SY-TABIX TRANSPORTING VALOR_02.
*    ENDIF.

  ENDIF.

  IF ( VG_VLR_ACM_03 NE 0 ). " OR ( VG_VLR_ACM_03_B NE 0 ).

    PERFORM MOSTRA_TEXTO USING TEXT-077. "'Ajuste - Lucro/Prejuizo 03'.

    VG_VLR_LUCRO = 0.
    VG_VLR_PREJU = 0.

    READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      ADD IT_1103_TOTAIS-VALOR_03 TO VG_VLR_LUCRO.
    ENDIF.

    READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_PREJUIZO.
    IF SY-SUBRC IS INITIAL.
      ADD IT_1103_TOTAIS-VALOR_03 TO VG_VLR_PREJU.
    ENDIF.

    "IF VG_VLR_ACM_03 LT 0.
    "  "DRE Negativa é Lucro
    ADD VG_VLR_ACM_03 TO VG_VLR_LUCRO.
    "ELSE.
    "  "DRE Positiva é Prejuizo
    "  ADD VG_VLR_ACM_03 TO VG_VLR_PREJU.
    "ENDIF.

*    IF VG_VLR_ACM_03_B LT 0.
*      "DRE Negativa é Lucro
*      ADD VG_VLR_ACM_03_B TO VG_VLR_LUCRO.
*    ELSE.
*      "DRE Positiva é Prejuizo
*      ADD VG_VLR_ACM_03_B TO VG_VLR_PREJU.
*    ENDIF.

    "Prejuizo e Lucro maior que 0 (zero)
*    IF ABS( VG_VLR_LUCRO ) GE VG_VLR_PREJU.
*      ADD VG_VLR_PREJU TO VG_VLR_LUCRO.
*      VG_VLR_PREJU = 0.
*    ELSE.
*      ADD VG_VLR_LUCRO TO VG_VLR_PREJU.
*      VG_VLR_LUCRO = 0.
*    ENDIF.

    READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      IT_1103_TOTAIS-VALOR_03 = VG_VLR_LUCRO.
      MODIFY IT_1103_TOTAIS INDEX SY-TABIX TRANSPORTING VALOR_03.
    ENDIF.

*    READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = LC_046-SAKNR_PREJUIZO.
*    IF SY-SUBRC IS INITIAL.
*      IT_1103_TOTAIS-VALOR_03 = VG_VLR_PREJU.
*      MODIFY IT_1103_TOTAIS INDEX SY-TABIX TRANSPORTING VALOR_03.
*    ENDIF.

  ENDIF.

  CLEAR: IT_CONTAS_SOLTAS[], IT_CONTAS_SALDO[].
  IF P_VCSV IS NOT INITIAL.

    DATA: VG_LIN1  TYPE I,
          VG_LIN2  TYPE I,
          VG_LIN22 TYPE I.

    DESCRIBE TABLE IT_SALDOS_ATUAL LINES VG_LIN1.
    DESCRIBE TABLE IT_SALDOS_ANTER LINES VG_LIN2.
    ADD VG_LIN2 TO VG_LIN1.
    PERFORM MOSTRA_TEXTO USING TEXT-078. "'Verificar Contas Sem Vinculos na Estrutura do Demonstrativo'.

    SELECT * INTO TABLE IT_SKA1 "#EC CI_DB_OPERATION_OK[2389136]
      FROM SKA1 "#EC CI_DB_OPERATION_OK[2431747]
       FOR ALL ENTRIES IN IT_T001
     WHERE KTOPL EQ IT_T001-KTOPL.

    SORT IT_SKA1 BY KTOPL SAKNR.

    SELECT * INTO TABLE ITZGLT043A
      FROM ZGLT043A
       FOR ALL ENTRIES IN IT_T001
     WHERE KTOPL EQ IT_T001-KTOPL
       AND BUKRS EQ IT_T001-BUKRS. "/Modificação 11.11.2016/

    SORT ITZGLT043A BY KTOKS.

    SELECT * INTO TABLE ITZGLT043B
      FROM ZGLT043B
       FOR ALL ENTRIES IN IT_T001
     WHERE KTOPL EQ IT_T001-KTOPL
       AND BUKRS EQ IT_T001-BUKRS. "/Modificação 11.11.2016/

    SORT ITZGLT043B BY SAKNR.

    LOOP AT IT_SALDOS_ATUAL.
      VG_LIN2 = SY-TABIX.
      PERFORM MOSTRA_TEXTO_P USING TEXT-079 VG_LIN1 VG_LIN2. "'Verificar Contas Sem Vinculos na Estrutura do Demonstrativo'

      READ TABLE IT_SKA1 WITH KEY SAKNR = IT_SALDOS_ATUAL-RACCT BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        READ TABLE ITZGLT043A WITH KEY KTOKS = IT_SKA1-KTOKS BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          CONTINUE.
        ENDIF.
        READ TABLE ITZGLT043B WITH KEY SAKNR = IT_SALDOS_ATUAL-RACCT BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = IT_SALDOS_ATUAL-RACCT.
      IF SY-SUBRC IS NOT INITIAL.
        CLEAR: IT_CONTAS_SOLTAS.
        READ TABLE IT_T001 WITH KEY BUKRS = IT_SALDOS_ATUAL-RBUKRS
                                    BINARY SEARCH.
        READ TABLE IT_SKAT WITH KEY KTOPL = IT_T001-KTOPL
                                    SAKNR = IT_SALDOS_ATUAL-RACCT
                                    BINARY SEARCH.
        IT_CONTAS_SOLTAS-SAKNR    = IT_SALDOS_ATUAL-RACCT.
        IT_CONTAS_SOLTAS-TEXTO    = IT_SKAT-TXT50.
        IT_CONTAS_SOLTAS-VALOR_01 = IT_SALDOS_ATUAL-SLVT.
        "Percorre Meses até Mês Selecioanado (Mês Solicitado/Mês Anterior).
        DO VG_MONAT TIMES
        VARYING REFE1 FROM IT_SALDOS_ATUAL-SL01 NEXT IT_SALDOS_ATUAL-SL02.
          IT_CONTAS_SOLTAS-VALOR_01 = IT_CONTAS_SOLTAS-VALOR_01 + REFE1.
        ENDDO.
        COLLECT IT_CONTAS_SOLTAS.
      ENDIF.
    ENDLOOP.

    LOOP AT IT_SALDOS_ANTER.
      VG_LIN22 = 0.
      ADD VG_LIN2  TO VG_LIN22.
      ADD SY-TABIX TO VG_LIN22.

      PERFORM MOSTRA_TEXTO_P USING TEXT-080 VG_LIN1 VG_LIN22. "'Verificar Contas Sem Vinculos na Estrutura do Demonstrativo'

      READ TABLE IT_SKA1 WITH KEY SAKNR = IT_SALDOS_ANTER-RACCT BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        READ TABLE ITZGLT043A WITH KEY KTOKS = IT_SKA1-KTOKS BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          CONTINUE.
        ENDIF.
        READ TABLE ITZGLT043B WITH KEY SAKNR = IT_SALDOS_ANTER-RACCT BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE IT_1103_TOTAIS WITH KEY SAKNR = IT_SALDOS_ANTER-RACCT.
      IF SY-SUBRC IS NOT INITIAL.
        CLEAR: IT_CONTAS_SOLTAS.
        READ TABLE IT_T001 WITH KEY BUKRS = IT_SALDOS_ANTER-RBUKRS
                                    BINARY SEARCH.
        READ TABLE IT_SKAT WITH KEY KTOPL = IT_T001-KTOPL
                                    SAKNR = IT_SALDOS_ANTER-RACCT
                                    BINARY SEARCH.
        IT_CONTAS_SOLTAS-SAKNR    = IT_SALDOS_ANTER-RACCT.
        IT_CONTAS_SOLTAS-TEXTO    = IT_SKAT-TXT50.
        IT_CONTAS_SOLTAS-VALOR_02 = IT_SALDOS_ANTER-SLVT.
        "Percorre Meses até Mês Selecioanado (Mês Solicitado/Mês Anterior).
        DO VG_MONAT2 TIMES
          VARYING REFE2 FROM IT_SALDOS_ANTER-SL01 NEXT IT_SALDOS_ANTER-SL02.
          IT_CONTAS_SOLTAS-VALOR_02 = IT_CONTAS_SOLTAS-VALOR_02 + REFE2.
        ENDDO.
        DO 15 TIMES
          VARYING REFE3 FROM IT_SALDOS_ANTER-SL01 NEXT IT_SALDOS_ANTER-SL02.
          IT_CONTAS_SOLTAS-VALOR_03 = IT_CONTAS_SOLTAS-VALOR_03 + REFE3.
        ENDDO.
        COLLECT IT_CONTAS_SOLTAS.
      ENDIF.
    ENDLOOP.

    DELETE IT_CONTAS_SOLTAS WHERE VALOR_01 EQ 0 AND VALOR_02 EQ 0 AND VALOR_03 EQ 0.

  ENDIF.

ENDFORM.                    " GERAR_RELATORIO_TREE_VIEW

*&---------------------------------------------------------------------*
*&      Form  ESTRUTURA_SAIDA_ALV_TREE_VIEW
*&---------------------------------------------------------------------*
*       Monta Estrutura Tree View a Partir do Nó Raiz
*----------------------------------------------------------------------*
FORM ESTRUTURA_SAIDA_ALV_TREE_VIEW TABLES IT_NODE_KEY TYPE LVC_T_NKEY.

  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI,
        LS_ITEM_LAYOUT TYPE LVC_S_LAYI,
        LS_NODE        TYPE LVC_S_LAYN,
        P_NODE_KEY     TYPE LVC_NKEY,
        P_RELAT_KEY    TYPE LVC_NKEY,
        L_NODE_TEXT    TYPE LVC_VALUE,
        WA_ZGLT047_LC  TYPE ZGLT047.

  "  DATA: VG_TABIX LIKE SY-TABIX.
  RANGES: IN_BUKRS FOR ZGLT041-BUKRS.


  CLEAR: IT_ZGLT047_TREE[], IT_ZGLT047[].

  "PERFORM ATUALIZA_TABELAS_CONSULTAS USING P_VERSN.

  "Níveis da Estrutura de Balanço Patrimonial
  SELECT *
    INTO TABLE IT_ZGLT047
    FROM ZGLT047
   WHERE VERSN EQ P_ESTR
   ORDER BY SQNIVEL.

  "Níveis Somatório da Estrutura de Balanço Patrimonial
  SELECT * INTO TABLE IT_ZGLT048
    FROM ZGLT048
   WHERE VERSN EQ P_ESTR.

  SORT IT_ZGLT048 BY VERSN NIVEL.

  "Notas de Classificação da Estrutura
  SELECT *
    INTO TABLE IT_ZGLT049
    FROM ZGLT049
   WHERE VERSN EQ P_ESTR
   ORDER BY SQNIVEL.

  "Notas de Classificação
  SELECT * INTO TABLE IT_ZGLT039
    FROM ZGLT039
     FOR ALL ENTRIES IN IT_ZGLT049
   WHERE CODIGO   EQ IT_ZGLT049-COD_CLAS_BAL
     AND COD_NOTA EQ IT_ZGLT049-COD_CLAS_NOT.

  SORT IT_ZGLT039 BY CODIGO COD_NOTA.

  IN_BUKRS-SIGN   = 'I' .
  IN_BUKRS-OPTION = 'EQ'.
  IN_BUKRS-LOW    = P_BUKRS.
  IN_BUKRS-HIGH   = P_BUKRS.
  APPEND IN_BUKRS.

  IF P_CONSO IS NOT INITIAL.
    IN_BUKRS-SIGN   = 'I' .
    IN_BUKRS-OPTION = 'EQ'.
    IN_BUKRS-LOW    = P_BUKR2.
    IN_BUKRS-HIGH   = P_BUKR2.
    APPEND IN_BUKRS.
  ENDIF.

  "Contas Razões das Notas de Classificação
  SELECT * INTO TABLE IT_ZGLT041
    FROM ZGLT041
   WHERE BUKRS IN IN_BUKRS.
*     AND GJAHR EQ P_GJAHR.  "/Modificação CS2017000372

  SORT IT_ZGLT041 BY SAKNR COD_CLAS_BAL COD_CLAS_NOT2.

  DELETE ADJACENT DUPLICATES FROM IT_ZGLT041 COMPARING SAKNR COD_CLAS_BAL COD_CLAS_NOT2.

  SORT IT_ZGLT041 BY BUKRS SAKNR.

  "Empresa p/ Plano de Contas do Razão
  SELECT * INTO TABLE IT_T001
    FROM T001
   WHERE BUKRS IN IN_BUKRS.

  SORT IT_T001 BY BUKRS.

  "Texto da Conta Razão
  SELECT * INTO TABLE IT_SKAT
    FROM SKAT
     FOR ALL ENTRIES IN IT_T001
   WHERE SPRAS EQ SY-LANGU
     AND KTOPL EQ IT_T001-KTOPL.

  SORT IT_SKAT BY KTOPL SAKNR.

  LOOP AT IT_ZGLT047 INTO WA_ZGLT047_LC WHERE NIVELPAI EQ ''.
    CLEAR: IT_ZGLT047_TREE.
    MOVE-CORRESPONDING WA_ZGLT047_LC TO IT_ZGLT047_TREE.

    IF WA_ZGLT047_LC-NIVELSUM EQ ABAP_TRUE.
      PERFORM BUSCA_TOTAIS_NIVEL_SOMA USING WA_ZGLT047_LC CHANGING IT_ZGLT047_TREE-VALOR_01 IT_ZGLT047_TREE-VALOR_02 IT_ZGLT047_TREE-VALOR_03.
    ELSE.
      PERFORM BUSCA_SALDO_NIVEL USING WA_ZGLT047_LC CHANGING IT_ZGLT047_TREE-VALOR_01 IT_ZGLT047_TREE-VALOR_02 IT_ZGLT047_TREE-VALOR_03.
      IF IT_ZGLT047_TREE-VALOR_01 EQ 0 AND
         IT_ZGLT047_TREE-VALOR_02 EQ 0 AND
         IT_ZGLT047_TREE-VALOR_03 EQ 0 AND
         P_ZERAD IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.
    "APPEND IT_ZGLT047_TREE.

    " Inclui nodos Raises da ALV TREE VIEW """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "*****************************************************************************************************************
    IF WA_ZGLT047_LC-NIVELSUM EQ ABAP_TRUE.
      LS_ITEM_LAYOUT-T_IMAGE = '@3Z@'.
      LS_NODE-N_IMAGE        = LS_ITEM_LAYOUT-T_IMAGE.
      LS_NODE-EXP_IMAGE      = LS_ITEM_LAYOUT-T_IMAGE.
    ELSE.
      CLEAR: LS_ITEM_LAYOUT-T_IMAGE.
      LS_NODE-N_IMAGE   = SPACE.
      LS_NODE-EXP_IMAGE = SPACE.
    ENDIF.
    LS_ITEM_LAYOUT-FIELDNAME = G_TREE3->C_HIERARCHY_COLUMN_NAME.
    LS_ITEM_LAYOUT-STYLE     = CL_GUI_COLUMN_TREE=>STYLE_DEFAULT.
    APPEND LS_ITEM_LAYOUT TO LT_ITEM_LAYOUT.
    CONCATENATE WA_ZGLT047_LC-NIVEL '-' WA_ZGLT047_LC-DESNVL INTO L_NODE_TEXT.
    CLEAR: P_RELAT_KEY.

    CALL METHOD G_TREE3->ADD_NODE
      EXPORTING
        I_RELAT_NODE_KEY = P_RELAT_KEY
        I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
        I_NODE_TEXT      = L_NODE_TEXT
        IS_OUTTAB_LINE   = IT_ZGLT047_TREE
        IS_NODE_LAYOUT   = LS_NODE
        IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
      IMPORTING
        E_NEW_NODE_KEY   = P_NODE_KEY.
    "*****************************************************************************************************************

    IF IT_ZGLT047_TREE-NIVELSUM EQ ABAP_FALSE.
      APPEND P_NODE_KEY TO IT_NODE_KEY.
    ENDIF.

    PERFORM BUSCA_NOTAS_FILHAS USING WA_ZGLT047_LC P_NODE_KEY.

    PERFORM BUSCA_FILHOS USING WA_ZGLT047_LC P_NODE_KEY.

  ENDLOOP.

ENDFORM.                    " ESTRUTURA_SAIDA_ALV_TREE_VIEW

*&---------------------------------------------------------------------*
*&      Form  BUSCA_FILHOS
*&---------------------------------------------------------------------*
*       Busca Níveis Filhos do Nó Raiz
*----------------------------------------------------------------------*
FORM BUSCA_FILHOS USING P_ZGLT047   TYPE ZGLT047
                        P_RELAT_KEY TYPE LVC_NKEY.

  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI,
        LS_ITEM_LAYOUT TYPE LVC_S_LAYI,
        LS_NODE        TYPE LVC_S_LAYN,
        P_NODE_KEY     TYPE LVC_NKEY,
        L_NODE_TEXT    TYPE LVC_VALUE,
        WA_ZGLT047_LC  TYPE ZGLT047.

  LOOP AT IT_ZGLT047 INTO WA_ZGLT047_LC WHERE NIVELPAI EQ P_ZGLT047-NIVEL.
    CLEAR: IT_ZGLT047_TREE.
    MOVE-CORRESPONDING WA_ZGLT047_LC TO IT_ZGLT047_TREE.
    IF WA_ZGLT047_LC-NIVELSUM EQ ABAP_TRUE.
      PERFORM BUSCA_TOTAIS_NIVEL_SOMA USING WA_ZGLT047_LC CHANGING IT_ZGLT047_TREE-VALOR_01 IT_ZGLT047_TREE-VALOR_02 IT_ZGLT047_TREE-VALOR_03.
    ELSE.
      PERFORM BUSCA_SALDO_NIVEL USING WA_ZGLT047_LC CHANGING IT_ZGLT047_TREE-VALOR_01 IT_ZGLT047_TREE-VALOR_02 IT_ZGLT047_TREE-VALOR_03.
      IF IT_ZGLT047_TREE-VALOR_01 EQ 0 AND
         IT_ZGLT047_TREE-VALOR_02 EQ 0 AND
         IT_ZGLT047_TREE-VALOR_03 EQ 0 AND
         P_ZERAD IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.
    "APPEND IT_ZGLT047_TREE.

    " Inclui nodos Raises da ALV TREE VIEW """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "*****************************************************************************************************************
    IF WA_ZGLT047_LC-NIVELSUM EQ ABAP_TRUE.
      LS_ITEM_LAYOUT-T_IMAGE = '@3Z@'.
      LS_NODE-N_IMAGE        = LS_ITEM_LAYOUT-T_IMAGE.
      LS_NODE-EXP_IMAGE      = LS_ITEM_LAYOUT-T_IMAGE.
    ELSE.
      CLEAR: LS_ITEM_LAYOUT-T_IMAGE.
      LS_NODE-N_IMAGE   = SPACE.
      LS_NODE-EXP_IMAGE = SPACE.
    ENDIF.
    LS_ITEM_LAYOUT-FIELDNAME = G_TREE3->C_HIERARCHY_COLUMN_NAME.
    LS_ITEM_LAYOUT-STYLE     = CL_GUI_COLUMN_TREE=>STYLE_DEFAULT.
    APPEND LS_ITEM_LAYOUT TO LT_ITEM_LAYOUT.
    CONCATENATE WA_ZGLT047_LC-NIVEL '-' WA_ZGLT047_LC-DESNVL INTO L_NODE_TEXT.

    CALL METHOD G_TREE3->ADD_NODE
      EXPORTING
        I_RELAT_NODE_KEY = P_RELAT_KEY
        I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
        I_NODE_TEXT      = L_NODE_TEXT
        IS_OUTTAB_LINE   = IT_ZGLT047_TREE
        IS_NODE_LAYOUT   = LS_NODE
        IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
      IMPORTING
        E_NEW_NODE_KEY   = P_NODE_KEY.
    "*****************************************************************************************************************

    PERFORM BUSCA_NOTAS_FILHAS USING WA_ZGLT047_LC P_NODE_KEY.

    PERFORM BUSCA_FILHOS USING WA_ZGLT047_LC P_NODE_KEY.

  ENDLOOP.

ENDFORM.                    " BUSCA_FILHOS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_NOTAS_FILHAS
*&---------------------------------------------------------------------*
*       Busca Notas de Classificação dos Níveis das Estruturas
*----------------------------------------------------------------------*
FORM BUSCA_NOTAS_FILHAS  USING P_ZGLT047   TYPE ZGLT047
                               P_RELAT_KEY TYPE LVC_NKEY.

  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI,
        LS_ITEM_LAYOUT TYPE LVC_S_LAYI,
        LS_NODE        TYPE LVC_S_LAYN,
        P_NODE_KEY     TYPE LVC_NKEY,
        L_NODE_TEXT    TYPE LVC_VALUE,
        WA_ZGLT049_LC  TYPE ZGLT049.

  LOOP AT IT_ZGLT049 INTO WA_ZGLT049_LC WHERE NIVEL EQ P_ZGLT047-NIVEL.

    CLEAR: IT_ZGLT047_TREE.
    MOVE-CORRESPONDING P_ZGLT047 TO IT_ZGLT047_TREE.
    READ TABLE IT_ZGLT039 WITH KEY CODIGO   = WA_ZGLT049_LC-COD_CLAS_BAL
                                   COD_NOTA = WA_ZGLT049_LC-COD_CLAS_NOT BINARY SEARCH.
    IT_ZGLT047_TREE-COD_CLAS_NOT = WA_ZGLT049_LC-COD_CLAS_NOT.
    IT_ZGLT047_TREE-DESCR_NOTA   = IT_ZGLT039-DESCR_NOTA.

    PERFORM BUSCA_TOTAIS_NOTAS_CONTAS USING WA_ZGLT049_LC CHANGING IT_ZGLT047_TREE-VALOR_01 IT_ZGLT047_TREE-VALOR_02 IT_ZGLT047_TREE-VALOR_03.
    IF IT_ZGLT047_TREE-VALOR_01 EQ 0 AND
       IT_ZGLT047_TREE-VALOR_02 EQ 0 AND
       IT_ZGLT047_TREE-VALOR_03 EQ 0 AND
       P_ZERAD IS NOT INITIAL.
      CONTINUE.
    ENDIF.
    " Inclui nodos Raises da ALV TREE VIEW """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "*****************************************************************************************************************
    "LS_ITEM_LAYOUT-T_IMAGE = '@3P@'.
    LS_ITEM_LAYOUT-FIELDNAME = G_TREE3->C_HIERARCHY_COLUMN_NAME.
    LS_ITEM_LAYOUT-STYLE     = CL_GUI_COLUMN_TREE=>STYLE_DEFAULT.
    APPEND LS_ITEM_LAYOUT TO LT_ITEM_LAYOUT.
    CONCATENATE WA_ZGLT049_LC-COD_CLAS_NOT '-' IT_ZGLT039-DESCR_NOTA INTO L_NODE_TEXT.
    LS_NODE-N_IMAGE   = ICON_WORKFLOW_INBOX.
    LS_NODE-EXP_IMAGE = ICON_OUTBOX.

    CALL METHOD G_TREE3->ADD_NODE
      EXPORTING
        I_RELAT_NODE_KEY = P_RELAT_KEY
        I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
        I_NODE_TEXT      = L_NODE_TEXT
        IS_OUTTAB_LINE   = IT_ZGLT047_TREE
        IS_NODE_LAYOUT   = LS_NODE
        IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
      IMPORTING
        E_NEW_NODE_KEY   = P_NODE_KEY.
    "*****************************************************************************************************************

    PERFORM BUSCA_NOTAS_CONTAS USING WA_ZGLT049_LC P_NODE_KEY.

  ENDLOOP.

ENDFORM.                    " BUSCA_NOTAS_FILHAS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_NOTAS_CONTAS
*&---------------------------------------------------------------------*
*       Busca Contas Razões das Notas de Clasdificação
*----------------------------------------------------------------------*
FORM BUSCA_NOTAS_CONTAS  USING P_ZGLT049   TYPE ZGLT049
                               P_RELAT_KEY TYPE LVC_NKEY.

  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI,
        LS_ITEM_LAYOUT TYPE LVC_S_LAYI,
        LS_NODE        TYPE LVC_S_LAYN,
        P_NODE_KEY     TYPE LVC_NKEY,
        L_NODE_TEXT    TYPE LVC_VALUE.

  LOOP AT IT_ZGLT041 WHERE COD_CLAS_BAL  EQ P_ZGLT049-COD_CLAS_BAL
                       AND COD_CLAS_NOT2 EQ P_ZGLT049-COD_CLAS_NOT.

    IT_ZGLT047_TREE-VALOR_01 = 0.
    IT_ZGLT047_TREE-VALOR_02 = 0.
    IT_ZGLT047_TREE-VALOR_03 = 0.

    LOOP AT IT_1103_TOTAIS WHERE NIVEL = P_ZGLT049-NIVEL
                             AND SAKNR = IT_ZGLT041-SAKNR.
      IT_ZGLT047_TREE-VALOR_01 = IT_ZGLT047_TREE-VALOR_01 + IT_1103_TOTAIS-VALOR_01.
      IT_ZGLT047_TREE-VALOR_02 = IT_ZGLT047_TREE-VALOR_02 + IT_1103_TOTAIS-VALOR_02.
      IT_ZGLT047_TREE-VALOR_03 = IT_ZGLT047_TREE-VALOR_03 + IT_1103_TOTAIS-VALOR_03.
    ENDLOOP.

    IF IT_ZGLT047_TREE-VALOR_01 EQ 0 AND
       IT_ZGLT047_TREE-VALOR_02 EQ 0 AND
       IT_ZGLT047_TREE-VALOR_03 EQ 0 AND
       P_ZERAD IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    IT_ZGLT047_TREE-SAKNR = IT_ZGLT041-SAKNR.
    CLEAR: IT_ZGLT047_TREE-TEXTO.

    READ TABLE IT_T001 WITH KEY BUKRS = IT_ZGLT041-BUKRS BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_SKAT WITH KEY KTOPL = IT_T001-KTOPL
                                  SAKNR = IT_ZGLT041-SAKNR.
      IF SY-SUBRC IS INITIAL.
        IT_ZGLT047_TREE-TEXTO = IT_SKAT-TXT50.
      ENDIF.
    ENDIF.
    "APPEND IT_ZGLT047_TREE.

    " Inclui nodos Raises da ALV TREE VIEW """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "*****************************************************************************************************************
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = IT_ZGLT047_TREE-SAKNR
      IMPORTING
        OUTPUT = L_NODE_TEXT.

    CONCATENATE L_NODE_TEXT '-' IT_ZGLT047_TREE-TEXTO INTO L_NODE_TEXT.
    LS_NODE-N_IMAGE   = ICON_SUBSCRIPTION.
    LS_NODE-EXP_IMAGE = ICON_SUBSCRIPTION.

    CALL METHOD G_TREE3->ADD_NODE
      EXPORTING
        I_RELAT_NODE_KEY = P_RELAT_KEY
        I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
        I_NODE_TEXT      = L_NODE_TEXT
        IS_OUTTAB_LINE   = IT_ZGLT047_TREE
        IS_NODE_LAYOUT   = LS_NODE
        IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
      IMPORTING
        E_NEW_NODE_KEY   = P_NODE_KEY.
    "*****************************************************************************************************************

    IT_ZGLT047_TREE-VALOR_01 = 0.
    IT_ZGLT047_TREE-VALOR_02 = 0.
    IT_ZGLT047_TREE-VALOR_03 = 0.
  ENDLOOP.

ENDFORM.                    " BUSCA_NOTAS_CONTAS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDO_NIVEL
*&---------------------------------------------------------------------*
*       Totaliza Níveis da Estrutura
*----------------------------------------------------------------------*
FORM BUSCA_SALDO_NIVEL  USING    P_ZGLT047  TYPE ZGLT047
                        CHANGING VALOR_01   TYPE HSLXX12
                                 VALOR_02   TYPE HSLXX12
                                 VALOR_03   TYPE HSLXX12.
  "Busca Saldo no Nível
  LOOP AT IT_ZGLT049 WHERE NIVEL EQ P_ZGLT047-NIVEL.
    LOOP AT IT_ZGLT041 WHERE COD_CLAS_BAL  EQ IT_ZGLT049-COD_CLAS_BAL
                         AND COD_CLAS_NOT2 EQ IT_ZGLT049-COD_CLAS_NOT.
      LOOP AT IT_1103_TOTAIS WHERE NIVEL        = IT_ZGLT049-NIVEL
                               AND COD_CLAS_BAL = IT_ZGLT049-COD_CLAS_BAL
                               AND COD_CLAS_NOT = IT_ZGLT049-COD_CLAS_NOT
                               AND SAKNR        = IT_ZGLT041-SAKNR.
        VALOR_01 = VALOR_01 + IT_1103_TOTAIS-VALOR_01.
        VALOR_02 = VALOR_02 + IT_1103_TOTAIS-VALOR_02.
        VALOR_03 = VALOR_03 + IT_1103_TOTAIS-VALOR_03.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  LOOP AT IT_ZGLT047 WHERE NIVELPAI EQ P_ZGLT047-NIVEL.
    PERFORM BUSCA_SALDO_NIVEL USING IT_ZGLT047 CHANGING VALOR_01 VALOR_02 VALOR_03.
  ENDLOOP.

ENDFORM.                    " BUSCA_SALDO_NIVEL

*&---------------------------------------------------------------------*
*&      Form  BUSCA_TOTAIS_NIVEL_SOMA
*&---------------------------------------------------------------------*
*       Totaliza Nível Somatório que não possui Filhos, mais dependências
*----------------------------------------------------------------------*
FORM BUSCA_TOTAIS_NIVEL_SOMA  USING    P_ZGLT047 TYPE ZGLT047
                              CHANGING VALOR_01  TYPE HSLXX12
                                       VALOR_02  TYPE HSLXX12
                                       VALOR_03  TYPE HSLXX12.

  DATA: WA_ZGLT047_LC TYPE ZGLT047.

  LOOP AT IT_ZGLT048 WHERE NIVEL EQ P_ZGLT047-NIVEL.
    READ TABLE IT_ZGLT047 INTO WA_ZGLT047_LC WITH KEY NIVEL = IT_ZGLT048-NIVELSUM.
    "Nivel Existe??
    IF SY-SUBRC IS INITIAL.
      PERFORM BUSCA_SALDO_NIVEL USING WA_ZGLT047_LC CHANGING VALOR_01 VALOR_02 VALOR_03.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " BUSCA_TOTAIS_NIVEL_SOMA

*&---------------------------------------------------------------------*
*&      Form  BUSCA_TOTAIS_NOTAS_CONTAS
*&---------------------------------------------------------------------*
*       Totaliza Contas de uma Nota
*----------------------------------------------------------------------*
FORM BUSCA_TOTAIS_NOTAS_CONTAS  USING    P_ZGLT049 TYPE ZGLT049
                                CHANGING VALOR_01 TYPE HSLXX12
                                         VALOR_02 TYPE HSLXX12
                                         VALOR_03 TYPE HSLXX12.

  VALOR_01 = 0.
  VALOR_02 = 0.
  VALOR_03 = 0.

  LOOP AT IT_ZGLT041 WHERE COD_CLAS_BAL  EQ P_ZGLT049-COD_CLAS_BAL
                       AND COD_CLAS_NOT2 EQ P_ZGLT049-COD_CLAS_NOT.

    LOOP AT IT_1103_TOTAIS WHERE NIVEL        = P_ZGLT049-NIVEL
                             AND COD_CLAS_BAL = P_ZGLT049-COD_CLAS_BAL
                             AND COD_CLAS_NOT = P_ZGLT049-COD_CLAS_NOT
                             AND SAKNR        = IT_ZGLT041-SAKNR.
      VALOR_01 = VALOR_01 + IT_1103_TOTAIS-VALOR_01.
      VALOR_02 = VALOR_02 + IT_1103_TOTAIS-VALOR_02.
      VALOR_03 = VALOR_03 + IT_1103_TOTAIS-VALOR_03.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " BUSCA_TOTAIS_NOTAS_CONTAS

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_LANC_LUCRO_PREJUIZO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VERIFICA_LANC_LUCRO_PREJUIZO  USING    L_BUKRS
                                            VG_GJAHR
                                            L_MONAT
                                   CHANGING CK_BUSCA_DRE.

  CK_BUSCA_DRE = ABAP_TRUE.

ENDFORM.                    " VERIFICA_LANC_LUCRO_PREJUIZO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_NOTAS_NIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUSCA_NOTAS_NIVEL  TABLES   P_T_NOTAS  STRUCTURE ZRANGE_CLAS_NOT
                                 P_T_NIVEIS STRUCTURE ZRANGE_NIVEL
                        USING    P_NIVEL    TYPE ZDENIVELBP.

  DATA: WA_ZGLT047_TREE_AUX TYPE TY_ZGLT047_TREE,
        W_NOTAS             TYPE ZRANGE_CLAS_NOT,
        W_NIVEL             TYPE ZRANGE_NIVEL.

  LOOP AT IT_ZGLT047_TREE INTO WA_ZGLT047_TREE_AUX WHERE NIVEL EQ P_NIVEL.
    IF WA_ZGLT047_TREE_AUX-COD_CLAS_NOT IS NOT INITIAL.
      W_NOTAS-SIGN   = 'I'.
      W_NOTAS-OPTION = 'EQ'.
      W_NOTAS-LOW    = WA_ZGLT047_TREE_AUX-COD_CLAS_NOT.
      W_NOTAS-HIGH   = WA_ZGLT047_TREE_AUX-COD_CLAS_NOT.
      APPEND W_NOTAS TO T_NOTAS.

      W_NIVEL-SIGN   = 'I'.
      W_NIVEL-OPTION = 'EQ'.
      W_NIVEL-LOW    = WA_ZGLT047_TREE_AUX-NIVEL.
      W_NIVEL-HIGH   = WA_ZGLT047_TREE_AUX-NIVEL.
      APPEND W_NIVEL TO T_NIVEIS.
    ELSE.
      LOOP AT IT_ZGLT047_TREE INTO WA_ZGLT047_TREE_AUX WHERE NIVELPAI EQ P_NIVEL.
        PERFORM BUSCA_NOTAS_NIVEL TABLES P_T_NOTAS P_T_NIVEIS USING WA_ZGLT047_TREE_AUX-NIVEL.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " BUSCA_NOTAS_NIVEL


*&---------------------------------------------------------------------*
*&      Form  CONTAS_ELIMINADAS_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CONTAS_ELIMINADAS_DRE .

  DATA: WA_ZGLT057          TYPE ZGLT057,
        IT_ZGL029_DRE_DADOS TYPE TABLE OF ZGL029_DRE_DADOS WITH HEADER LINE.

  SELECT SINGLE *
    FROM ZGLT057
    INTO WA_ZGLT057
   WHERE BUKRS_1  = P_BUKRS
     AND VERSN_1  = T_DRE1
     AND BUKRS_2  = P_BUKR2
     AND VERSN_2  = T_DRE3
     AND STATUS   = ''.

  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  " Dados processados sociedade parceira para DRE - MÊS
  SELECT * INTO TABLE IT_ZGL029_DRE_DADOS
    FROM ZGL029_DRE_DADOS
    WHERE BUKRS EQ P_BUKRS
     AND  VERSN EQ T_DRE1
     AND  MONAT EQ P_MONAT
     AND  GJAHR EQ P_GJAHR
     AND  VBUND EQ WA_ZGLT057-VBUND_1
    AND EXISTS ( SELECT * FROM ZGLT060
                  WHERE ZGLT060~BUKRS1  = P_BUKRS
                  AND   ZGLT060~WAERS   = P_WAERS
                  AND   ZGLT060~CTA_LAN = ZGL029_DRE_DADOS~SAKNR ).

  SELECT * APPENDING TABLE IT_ZGL029_DRE_DADOS
    FROM ZGL029_DRE_DADOS
    WHERE BUKRS EQ P_BUKR2
     AND  VERSN EQ T_DRE3
     AND  MONAT EQ P_MONAT
     AND  GJAHR EQ P_GJAHR
     AND  VBUND EQ WA_ZGLT057-VBUND_2
    AND EXISTS ( SELECT * FROM ZGLT060
                  WHERE ZGLT060~BUKRS1  = P_BUKR2
                  AND   ZGLT060~WAERS   = P_WAERS
                  AND   ZGLT060~CTA_LAN = ZGL029_DRE_DADOS~SAKNR ).

  " Inserindo contas virtuais - Empresa 1
  SELECT ZGL029_DRE_DADOS~MANDT
          ZGLT060~BUKRS2
          ZGL029_DRE_DADOS~VERSN   "*******
          ZGL029_DRE_DADOS~MONAT
          ZGL029_DRE_DADOS~GJAHR
          ZGLT060~CTA_VIRTUAL
          ZGLT060~KOSTL2
          ZGLT060~PRCTR2
          ZGLT060~MATKL2
          ZGL029_DRE_DADOS~VBUND     "*******
          ZGL029_DRE_DADOS~QTD_TON
          ZGL029_DRE_DADOS~VLR_REA
          ZGL029_DRE_DADOS~VLR_DOLAR
    ZGL029_DRE_DADOS~VLR_GRUPO
  APPENDING TABLE IT_ZGL029_DRE_DADOS
  FROM ZGL029_DRE_DADOS
  INNER JOIN ZGLT060
    ON  ZGLT060~BUKRS1  = ZGL029_DRE_DADOS~BUKRS
    AND ZGLT060~BUKRS2  = P_BUKR2
    AND ZGLT060~WAERS   = P_WAERS
    AND ZGLT060~CTA_LAN = ZGL029_DRE_DADOS~SAKNR
    AND ZGLT060~CTA_VIRTUAL NE ''
    AND ZGLT060~KOSTL1  = ZGL029_DRE_DADOS~KOSTL
    AND ZGLT060~PRCTR1  = ZGL029_DRE_DADOS~PRCTR
    AND ZGLT060~MATKL1  = ZGL029_DRE_DADOS~MATKL
  WHERE ZGL029_DRE_DADOS~BUKRS EQ P_BUKRS
   AND  ZGL029_DRE_DADOS~VERSN EQ T_DRE1
   AND  ZGL029_DRE_DADOS~MONAT EQ P_MONAT
   AND  ZGL029_DRE_DADOS~GJAHR EQ P_GJAHR
   AND  ZGL029_DRE_DADOS~VBUND EQ WA_ZGLT057-VBUND_1.

  " Inserindo contas virtuais - Empresa 2
  SELECT ZGL029_DRE_DADOS~MANDT
      ZGLT060~BUKRS2
      ZGL029_DRE_DADOS~VERSN
      ZGL029_DRE_DADOS~MONAT
      ZGL029_DRE_DADOS~GJAHR
      ZGLT060~CTA_VIRTUAL
      ZGLT060~KOSTL2
      ZGLT060~PRCTR2
      ZGLT060~MATKL2
      ZGL029_DRE_DADOS~VBUND
      ZGL029_DRE_DADOS~QTD_TON
      ZGL029_DRE_DADOS~VLR_REA
      ZGL029_DRE_DADOS~VLR_DOLAR
      ZGL029_DRE_DADOS~VLR_GRUPO
 APPENDING TABLE IT_ZGL029_DRE_DADOS
 FROM ZGL029_DRE_DADOS
 INNER JOIN ZGLT060
   ON  ZGLT060~BUKRS1  = ZGL029_DRE_DADOS~BUKRS
   AND ZGLT060~BUKRS2  = P_BUKRS
   AND ZGLT060~WAERS   = P_WAERS
   AND ZGLT060~CTA_LAN = ZGL029_DRE_DADOS~SAKNR
   AND ZGLT060~CTA_VIRTUAL NE ''
   AND ZGLT060~KOSTL1  = ZGL029_DRE_DADOS~KOSTL
   AND ZGLT060~PRCTR1  = ZGL029_DRE_DADOS~PRCTR
   AND ZGLT060~MATKL1  = ZGL029_DRE_DADOS~MATKL
 WHERE BUKRS EQ P_BUKR2
  AND  VERSN EQ WA_ZGLT057-VERSN_2
  AND  MONAT EQ P_MONAT
  AND  GJAHR EQ P_GJAHR
  AND  VBUND EQ WA_ZGLT057-VBUND_2.

  REFRESH TG_CONSOL.
  LOOP AT IT_ZGL029_DRE_DADOS.
    IF IT_ZGL029_DRE_DADOS-BUKRS = P_BUKRS.
      IF IT_ZGL029_DRE_DADOS-VERSN NE T_DRE1.
        IT_ZGL029_DRE_DADOS-VERSN     = T_DRE1.
        IT_ZGL029_DRE_DADOS-VBUND     = WA_ZGLT057-VBUND_1.
        IT_ZGL029_DRE_DADOS-VLR_REA   = IT_ZGL029_DRE_DADOS-VLR_REA * -1.
        IT_ZGL029_DRE_DADOS-VLR_DOLAR = IT_ZGL029_DRE_DADOS-VLR_DOLAR * -1.
        IT_ZGL029_DRE_DADOS-VLR_GRUPO = IT_ZGL029_DRE_DADOS-VLR_GRUPO * -1.
        IT_ZGL029_DRE_DADOS-QTD_TON   = IT_ZGL029_DRE_DADOS-QTD_TON * -1.
      ENDIF.
    ELSE.
      IF IT_ZGL029_DRE_DADOS-VERSN NE WA_ZGLT057-VERSN_2.
        IT_ZGL029_DRE_DADOS-VERSN     = WA_ZGLT057-VERSN_2.
        IT_ZGL029_DRE_DADOS-VBUND     = WA_ZGLT057-VBUND_2.
        IT_ZGL029_DRE_DADOS-VLR_REA   = IT_ZGL029_DRE_DADOS-VLR_REA * -1.
        IT_ZGL029_DRE_DADOS-VLR_DOLAR = IT_ZGL029_DRE_DADOS-VLR_DOLAR * -1.
        IT_ZGL029_DRE_DADOS-VLR_GRUPO = IT_ZGL029_DRE_DADOS-VLR_GRUPO * -1.
        IT_ZGL029_DRE_DADOS-QTD_TON   = IT_ZGL029_DRE_DADOS-QTD_TON * -1.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING IT_ZGL029_DRE_DADOS TO TG_CONSOL.
    APPEND TG_CONSOL.
  ENDLOOP.

  CALL SCREEN 1108 STARTING AT 005 03
                     ENDING AT 180 20.

ENDFORM.                    " CONTAS_ELIMINADAS_DRE
