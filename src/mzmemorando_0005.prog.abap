*&---------------------------------------------------------------------*
*&  Include           MZMEMORANDO_0005
*&---------------------------------------------------------------------*

CONSTANTS: C_9000 LIKE SY-DYNNR   VALUE '9000',
           C_9001 LIKE SY-DYNNR   VALUE '9001',
           C_GRID_COLOR_C200 TYPE C LENGTH 04 VALUE 'C000',
           C_GRID_COLOR_C400 TYPE C LENGTH 04 VALUE 'C300',
           BEGIN OF C_TEXTOS,
             C_001 TYPE C LENGTH 30 VALUE 'Empresa',
             C_002 TYPE C LENGTH 30 VALUE 'Local negócios',
             C_003 TYPE C LENGTH 30 VALUE 'Nº documento',
             C_004 TYPE C LENGTH 30 VALUE 'Nº item',
             C_005 TYPE C LENGTH 30 VALUE 'Data Documento',
             C_006 TYPE C LENGTH 30 VALUE 'Modelo',
             C_007 TYPE C LENGTH 30 VALUE 'Série',
             C_008 TYPE C LENGTH 30 VALUE 'Nº NF',
             C_009 TYPE C LENGTH 30 VALUE 'NF-e',
             C_010 TYPE C LENGTH 30 VALUE 'Nº NF-e',
             C_011 TYPE C LENGTH 30 VALUE 'Nº Material',
             C_012 TYPE C LENGTH 30 VALUE 'NCM',
             C_013 TYPE C LENGTH 30 VALUE 'Quantidade',
             C_014 TYPE C LENGTH 30 VALUE 'Unidade',
             C_015 TYPE C LENGTH 30 VALUE 'Desc. Material',
             C_016 TYPE C LENGTH 30 VALUE 'Saldo',
             C_017 TYPE C LENGTH 30 VALUE 'Vincular',
             C_019 TYPE C LENGTH 30 VALUE 'Qtd. Rec./Dev',
             C_020 TYPE C LENGTH 30 VALUE 'Numero Memo',
             C_T01 TYPE C LENGTH 70 VALUE 'Notas Fiscais de Saída - Finalidade Exportação',
             C_T02 TYPE C LENGTH 70 VALUE 'Notas Fiscais de Saída - Vinculadas',
             C_T03 TYPE C LENGTH 70 VALUE 'Notas Fiscais de Entrada - Finalidade Exportação',
             C_T04 TYPE C LENGTH 70 VALUE 'Notas Fiscais de Entrada - Vinculadas',
           END OF C_TEXTOS.

DATA: ALV_01                     TYPE REF TO CL_GUI_ALV_GRID,
      ALV_02                     TYPE REF TO CL_GUI_ALV_GRID,
      ALV_03                     TYPE REF TO CL_GUI_ALV_GRID,
      ALV_04                     TYPE REF TO CL_GUI_ALV_GRID,
      C_TOOLBARMANAGER_VINC  TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      C_TOOLBARMANAGER_01    TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER.


**----------------------------------------------------------------------*
**       CLASS lcl_event_receiver_01 DEFINITION
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*CLASS LCL_EVENT_RECEIVER_01 DEFINITION.
*  PUBLIC SECTION.
*    METHODS:
*    HANDLE_USER_COMMAND
*        FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
*            IMPORTING E_UCOMM.
*ENDCLASS.                    "lcl_event_receiver_01 DEFINITION
*
**----------------------------------------------------------------------*
**       CLASS lcl_event_receiver_01 IMPLEMENTATION
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*CLASS LCL_EVENT_RECEIVER_01 IMPLEMENTATION.
*  METHOD HANDLE_USER_COMMAND.
*
*    DATA: lt_rows TYPE lvc_t_row.
*
*    CASE E_UCOMM.
*      WHEN C_BTVINF.
*        IF VG_4002_6002 EQ C_6003.
*          CALL METHOD alv_01->get_selected_rows
*            IMPORTING
*              et_index_rows = lt_rows.
*
*          "CALL METHOD cl_gui_cfw=>flush.
*
*          PERFORM VINCULAR_NOTA_S_NEW.
*        ELSE.
*
*          CALL METHOD alv_03->get_selected_rows
*            IMPORTING
*              et_index_rows = lt_rows.
*
*          PERFORM VINCULAR_NOTA_NEW.
*        ENDIF.
*        LEAVE TO SCREEN 0005.
*      WHEN C_BTDENF.
*        IF VG_4002_6002 EQ C_6003.
*
*          CALL METHOD alv_02->get_selected_rows
*            IMPORTING
*              et_index_rows = lt_rows.
*
*          PERFORM DESVINCULAR_NOTA_S_NEW.
*        ELSE.
*
*          CALL METHOD alv_04->get_selected_rows
*            IMPORTING
*              et_index_rows = lt_rows.
*
*          PERFORM DESVINCULAR_NOTA_NEW.
*        ENDIF.
*        LEAVE TO SCREEN 0005.
*    ENDCASE.
*  ENDMETHOD.                           "handle_user_command
**-----------------------------------------------------------------
*
*ENDCLASS.                    "lcl_event_receiver_01 IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_VINC DEFINITION.
  PUBLIC SECTION.
    METHODS: CONSTRUCTOR IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
    ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING  E_OBJECT,
    HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_VINC IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_TOOLBARMANAGER_VINC
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

    DATA : VG_TOOLBAR TYPE STB_BUTTON.

    CLEAR: VG_TOOLBAR.
    VG_TOOLBAR-ICON      =  ICON_TREND_DOWN.
    VG_TOOLBAR-FUNCTION  =  C_BTVINF.
    VG_TOOLBAR-QUICKINFO = 'Vincular'.
    INSERT VG_TOOLBAR INTO E_OBJECT->MT_TOOLBAR INDEX 1.

    CLEAR: VG_TOOLBAR.
    VG_TOOLBAR-BUTN_TYPE = '3'.
    INSERT VG_TOOLBAR INTO E_OBJECT->MT_TOOLBAR INDEX 2.

    CLEAR: VG_TOOLBAR.
    VG_TOOLBAR-ICON      =  ICON_TREND_UP.
    VG_TOOLBAR-FUNCTION  =  C_BTDENF.
    VG_TOOLBAR-QUICKINFO = 'Desvincular'.
    INSERT VG_TOOLBAR INTO E_OBJECT->MT_TOOLBAR INDEX 3.

    CLEAR: VG_TOOLBAR.
    VG_TOOLBAR-BUTN_TYPE = '3'.
    INSERT VG_TOOLBAR INTO E_OBJECT->MT_TOOLBAR INDEX 4.

    CALL METHOD C_TOOLBARMANAGER_VINC->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN C_BTVINF.
        IF VG_4002_6002 EQ C_6003.
          PERFORM VINCULAR_NOTA_S_NEW.
        ELSE.
          PERFORM VINCULAR_NOTA_NEW.
        ENDIF.
        LEAVE TO SCREEN 0005.
      WHEN C_BTDENF.
        IF VG_4002_6002 EQ C_6003.
          PERFORM DESVINCULAR_NOTA_S_NEW.
        ELSE.
          PERFORM DESVINCULAR_NOTA_NEW.
        ENDIF.
        LEAVE TO SCREEN 0005.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "LCL_ALV_TOOLBAR_VINC

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_01 DEFINITION.
  PUBLIC SECTION.
    METHODS: CONSTRUCTOR IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
    ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING  E_OBJECT,
    HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM,
    ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.
ENDCLASS.                    "LCL_ALV_TOOLBAR_01

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_01 IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_TOOLBARMANAGER_01
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.

  ENDMETHOD.                    "constructor

  METHOD ON_DATA_CHANGED.

    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LC_SALDO TYPE J_1BNETQTY.

    LOOP AT ER_DATA_CHANGED->MT_MOD_CELLS INTO LS_GOOD.

      READ TABLE IT_NF_LIVRE INTO WA_NF_LIVRE INDEX LS_GOOD-ROW_ID.

      MOVE LS_GOOD-VALUE TO LC_SALDO.
      IF WA_NF_LIVRE-SALDO LT LC_SALDO.

      ENDIF.

    ENDLOOP.

*MT_FIELDCATALOG
*MT_MOD_CELLS
*MP_MOD_ROWS
*MT_PROTOCOL
*MT_GOOD_CELLS
*MR_PROTOCOL
*MT_DELETED_ROWS
*MT_INSERTED_ROWS
*MS_LAYOUT
*MT_ROID_FRONT
*MR_DIALOGBOX
*MR_CONTAINER
*M_LOG_HANDLE
*MR_CALLING_ALV
*M_CONTROL_HANDLE
*MT_MSG_HANDLE

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD ON_TOOLBAR.

    DATA : VG_TOOLBAR TYPE STB_BUTTON.

    VG_TOOLBAR-ICON      =  ICON_TREND_DOWN.
    VG_TOOLBAR-FUNCTION  =  C_BTVINF.
    VG_TOOLBAR-QUICKINFO = 'Vincular'.
    INSERT VG_TOOLBAR INTO E_OBJECT->MT_TOOLBAR INDEX 1.

    CLEAR: VG_TOOLBAR.
    VG_TOOLBAR-BUTN_TYPE = '3'.
    INSERT VG_TOOLBAR INTO E_OBJECT->MT_TOOLBAR INDEX 2.

    DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
    DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
    DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.

    CALL METHOD C_TOOLBARMANAGER_01->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN C_BTVINF.
        IF VG_4002_6002 EQ C_6003.
          PERFORM VINCULAR_NOTA_S_NEW.
        ELSE.
          PERFORM VINCULAR_NOTA_NEW.
        ENDIF.
        LEAVE TO SCREEN 0005.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "LCL_ALV_TOOLBAR_01


DATA: OK_CODE_9002               LIKE SY-UCOMM,
      IT_ET_ROW_NO               TYPE LVC_T_ROID,
      WA_SELECTED_ROWS           TYPE LVC_S_ROW,
      C_ALV_TOOLBARMANAGER_VINC  TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      IT_SELECTED_ROWS           TYPE LVC_T_ROW,

      "Tela de Saída
      G_CUSTOM_CONTAINER_01      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
*      EVENT_RECEIVER_01          TYPE REF TO LCL_EVENT_RECEIVER_01,
      DG_SPLITTER_01             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_01               TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_02               TYPE REF TO CL_GUI_CONTAINER,
      TOOLBAR_ALV_01             TYPE REF TO LCL_ALV_TOOLBAR_01,
      TOOLBAR_ALV_02             TYPE REF TO LCL_ALV_TOOLBAR_VINC,
      WA_LAYOUT_01               TYPE LVC_S_LAYO,
      GS_VARIANT_C_01            TYPE DISVARIANT,
      WA_LAYOUT_02               TYPE LVC_S_LAYO,
      GS_VARIANT_C_02            TYPE DISVARIANT,
      IT_FIELDCATALOG_01         TYPE LVC_T_FCAT,
      IT_FIELDCATALOG_02         TYPE LVC_T_FCAT,

      "Tela de Entrada
      G_CUSTOM_CONTAINER_03      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DG_SPLITTER_03             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_03               TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_04               TYPE REF TO CL_GUI_CONTAINER,
      TOOLBAR_ALV_03             TYPE REF TO LCL_ALV_TOOLBAR_01,
      TOOLBAR_ALV_04             TYPE REF TO LCL_ALV_TOOLBAR_VINC,
      WA_LAYOUT_03               TYPE LVC_S_LAYO,
      GS_VARIANT_C_03            TYPE DISVARIANT,
      WA_LAYOUT_04               TYPE LVC_S_LAYO,
      GS_VARIANT_C_04            TYPE DISVARIANT,
      IT_FIELDCATALOG_03         TYPE LVC_T_FCAT,
      IT_FIELDCATALOG_04         TYPE LVC_T_FCAT.


*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
    METHODS: CONSTRUCTOR IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
    ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING  E_OBJECT,
    HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER_VINC
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

    DATA : VG_TOOLBAR TYPE STB_BUTTON.

    VG_TOOLBAR-ICON      =  ICON_BIW_INFO_CATALOG.
    VG_TOOLBAR-FUNCTION  =  C_LEGENDA.
    VG_TOOLBAR-QUICKINFO = 'Legenda'.
    INSERT VG_TOOLBAR INTO E_OBJECT->MT_TOOLBAR INDEX 1.

    CLEAR: VG_TOOLBAR.
    VG_TOOLBAR-BUTN_TYPE = '3'.
    INSERT VG_TOOLBAR INTO E_OBJECT->MT_TOOLBAR INDEX 2.

    CALL METHOD C_ALV_TOOLBARMANAGER_VINC->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN C_LEGENDA.
        CALL SCREEN 9002 STARTING AT 10 5.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

DATA: TOOLBAR_ALV_MEMO   TYPE REF TO LCL_ALV_TOOLBAR.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0005 OUTPUT.

  IF VG_DYNNR_000 IS INITIAL.
    VG_DYNNR_000 = C_9000.
  ENDIF.

  CASE VG_DYNNR_000.
    WHEN C_9000.
      SET PF-STATUS 'PFCNSNEW' EXCLUDING IT_FCODE.
      SET TITLEBAR 'PFCNSNEW'.
    WHEN C_9001.
      CLEAR: IT_FCODE.
      SET PF-STATUS 'PFALVNEW' EXCLUDING IT_FCODE.
      SET TITLEBAR 'PFCNSNEW'.
    WHEN C_2000.
      CLEAR: IT_FCODE.
      IF VG_CONSUL_MEMO IS INITIAL.
        IF ZDOC_MEMORANDO IS INITIAL.
          SET TITLEBAR 'TLLANC'.
        ELSE.
          SET TITLEBAR 'TLALTE'.
        ENDIF.
        WA_FCODE = C_FCODE_CSNFMS.
        APPEND WA_FCODE TO IT_FCODE.
        WA_FCODE = C_FCODE_CSNFMM.
        APPEND WA_FCODE TO IT_FCODE.
      ELSE.
        SET TITLEBAR 'TLCONS'.
      ENDIF.
      IF ZDOC_MEMO_NF_EXP-PROPRIO IS INITIAL.
        WA_FCODE = C_FCODE_NFPROPRIA.
        APPEND WA_FCODE TO IT_FCODE.
      ENDIF.
      IF VG_ALTEROU_MEMORANDO IS INITIAL.
        IF VG_TOTAL_MEMORANDOS GE ZDOC_MEMO_NF_EXP-QUANTIDADE. "ALRS
          WA_FCODE = C_FCODE_SAVE.
          APPEND WA_FCODE TO IT_FCODE.
        ENDIF.
      ENDIF.
      SET PF-STATUS 'PFLANC' EXCLUDING IT_FCODE.
    WHEN C_4000.
      CLEAR: IT_FCODE.
      IF VG_ALTEROU_NOTAS IS INITIAL.
        WA_FCODE = C_FCODE_SAVE.
        APPEND WA_FCODE TO IT_FCODE.
      ENDIF.
      SET PF-STATUS 'PFVINC' EXCLUDING IT_FCODE.
      SET TITLEBAR 'TLVINC'.
    WHEN C_6000.
      CLEAR: IT_FCODE.
      IF VG_ALTEROU_NOTAS IS INITIAL.
        WA_FCODE = C_FCODE_SAVE.
        APPEND WA_FCODE TO IT_FCODE.
      ENDIF.
      SET PF-STATUS 'PFVINCS' EXCLUDING IT_FCODE.
      SET TITLEBAR 'TLVINCS'.
  ENDCASE.

ENDMODULE.                 " STATUS_0005  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0005 INPUT.

  CASE OK_CODE.
    WHEN C_CSMEMO.
      PERFORM PESQUISAR_MEMORANDOS.

      IF IT_MEMORANDOS[] IS NOT INITIAL.
        VG_DYNNR_000 = C_9001.
      ELSE.
        MESSAGE S000 WITH 'Não existem registros para esta seleção!' DISPLAY LIKE 'W'.
      ENDIF.

    WHEN C_LANCME.
      PERFORM CONSULTAR_MEMORANDOS.

    WHEN C_DLCANCEL.
      PERFORM CANCELAR_MEMORANDO.
      PERFORM PESQUISAR_MEMORANDOS.

    WHEN C_CSPROTO.
      PERFORM CHAMAR_PROTOCOLOS.

    WHEN C_DLMEMO.
      PERFORM ELIMINAR_MEMORANDOS.

    WHEN C_CSPRINT.
      PERFORM EMITIR_MEMORANDO.

    WHEN C_CSNOTAS.
      PERFORM EMITIR_NOTAS_VINCULADAS.

    WHEN C_EDMEMO.
      PERFORM EDITAR_MEMORANDOS.

    WHEN C_LANCNV.
      PERFORM LANCAR_MEMORANDOS.

    WHEN C_CSNFMM.
      PERFORM LANCAR_MEMORANDOS_NOTAS_NEW.

    WHEN C_CSNFMS.
      PERFORM LANCAR_MEMORANDOS_NOTAS_S_NEW.

    WHEN C_BACK OR C_EXIT OR C_CANCEL OR C_BACKV OR C_EXITV OR C_CANCELV.
      CLEAR: OK_CODE.
      CASE VG_DYNNR_000.
        WHEN C_9000.
          LEAVE PROGRAM.
        WHEN C_2000.
          VG_DYNNR_000 = C_9001.
        WHEN C_9001.
          VG_DYNNR_000 = C_9000.
        WHEN OTHERS.
          VG_DYNNR_000 = VG_DYNNR_ANT.
      ENDCASE.
  ENDCASE.

  CLEAR: OK_CODE.

ENDMODULE.                 " USER_COMMAND_0005  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_9001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_ALV_9001 OUTPUT.

  IF VG_PRIMEIRO_VISUAL IS INITIAL.

*   Create object for container
    CREATE OBJECT CTL_CCCONTAINER
      EXPORTING
        CONTAINER_NAME = 'MEMO_LISTA_N'.

    CREATE OBJECT CTL_ALV_MEMO
      EXPORTING
        I_PARENT = CTL_CCCONTAINER.

    PERFORM Z_ESTRUTURA_FIELDCAT USING:
   'ICONE'           SPACE    ' ' 01 03 'X'   ,
   'NUMERO_NOTA'     TEXT-T06 ' ' 02 09 'X'   ,
   'NUMERO_MEMO'     TEXT-T20 ' ' 03 08 'X'   ,
   'SERIE'           TEXT-T05 ' ' 04 03 'X'   ,

   'REPRESENTANTE'   TEXT-T26 ' ' 05 10 SPACE ,
   'REPRESENTAN'     TEXT-T27 ' ' 06 35 SPACE ,
   'REPRESENTACNPJ'  TEXT-T28 ' ' 07 18 SPACE ,

   'EMISSOR'         TEXT-T07 ' ' 08 10 SPACE ,
   'EMISSORN'        TEXT-T08 ' ' 09 35 SPACE ,
   'EMISSORCNPJ'     TEXT-T09 ' ' 10 18 SPACE ,

   'TXTSTATUS'       TEXT-T02 ' ' 11 10 SPACE ,
   'TXTDIRECAO'      TEXT-T04 ' ' 12 08 SPACE ,
   'TXTFINALIDADE'   TEXT-T03 ' ' 13 10 SPACE ,

   'DT_EMISSAO_NOTA' TEXT-T13 ' ' 14 10 SPACE ,

   'REMETENTE'       TEXT-T10 ' ' 15 10 SPACE ,
   'REMETENTEN'      TEXT-T11 ' ' 16 35 SPACE ,
   'REMETENTECNPJ'   TEXT-T12 ' ' 17 18 SPACE ,

   'PROPRIO'         TEXT-T14 ' ' 18 10 SPACE ,
   'DOCNUM'          TEXT-T15 ' ' 19 10 SPACE ,

   'MATERIAL'        TEXT-T16 ' ' 20 18 SPACE ,
   'MAKTG'           TEXT-T17 ' ' 21 35 SPACE ,

   'QUANTIDADE_MEMO' TEXT-T18 ' ' 22 13 SPACE ,
   'UNIDADE'         TEXT-T19 ' ' 23 03 SPACE ,

   'PAIS_ORIGEM'     TEXT-T21 ' ' 24 03 SPACE ,
   'UF_ORIGEM'       TEXT-T22 ' ' 25 03 SPACE ,
   'PAIS_DESTINO'    TEXT-T23 ' ' 26 03 SPACE ,
   'NR_DDE'          TEXT-T24 ' ' 27 15 SPACE ,
   'NR_RE'           TEXT-T25 ' ' 28 15 SPACE .

    CREATE OBJECT TOOLBAR_ALV_MEMO
      EXPORTING
        IO_ALV_GRID = CTL_ALV_MEMO.

    CALL METHOD CTL_ALV_MEMO->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = WA_LAYOUT_S
        IS_VARIANT      = GS_VARIANT_C
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG
        IT_OUTTAB       = IT_MEMORANDOS_ALV[].

    SET HANDLER TOOLBAR_ALV_MEMO->ON_TOOLBAR FOR CTL_ALV_MEMO.
    SET HANDLER TOOLBAR_ALV_MEMO->HANDLE_USER_COMMAND FOR CTL_ALV_MEMO.

    CALL METHOD CTL_ALV_MEMO->REFRESH_TABLE_DISPLAY.

    VG_PRIMEIRO_VISUAL = C_X.

  ENDIF.

ENDMODULE.                 " CRIA_ALV_9001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.
  SET PF-STATUS 'PFLEGEN'.
  SET TITLEBAR 'TLLEGEN'.

ENDMODULE.                 " STATUS_9002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_9002  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_6003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_6003 OUTPUT.

  CLEAR: VG_NOME_EMPRESA, VG_NOME_CENTRO.

  IF J_1BNFDOC-BUKRS IS INITIAL.
    SELECT SINGLE BUKRS INTO J_1BNFDOC-BUKRS
      FROM J_1BBRANCH
     WHERE BRANCH EQ ZDOC_MEMORANDO-REMETENTE+6(4).

    J_1BNFDOC-BRANCH = ZDOC_MEMORANDO-REMETENTE+6(4).
  ENDIF.

  SELECT SINGLE BUTXT INTO VG_NOME_EMPRESA
    FROM T001
   WHERE BUKRS EQ J_1BNFDOC-BUKRS.

  SELECT SINGLE NAME1 INTO VG_NOME_CENTRO
    FROM T001W
   WHERE WERKS EQ ZDOC_MEMORANDO-REMETENTE+6(4).

  IF G_CUSTOM_CONTAINER_01 IS INITIAL.
    PERFORM CRIA_CONTAINER_NOTAS.
  ENDIF.

  CALL METHOD ALV_01->REFRESH_TABLE_DISPLAY.
  CALL METHOD ALV_02->REFRESH_TABLE_DISPLAY.

ENDMODULE.                 " STATUS_6003  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_CONTAINER_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CRIA_CONTAINER_NOTAS .

  DATA: URL(255)  TYPE C.

* create a container for the tree control
  CREATE OBJECT G_CUSTOM_CONTAINER_01
    EXPORTING
      CONTAINER_NAME              = 'ALV_NOTAS_S'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.

  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

  CREATE OBJECT DG_SPLITTER_01
    EXPORTING
      PARENT  = G_CUSTOM_CONTAINER_01
      ROWS    = 2
      COLUMNS = 1.

  CALL METHOD DG_SPLITTER_01->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = DG_PARENT_01.

  CALL METHOD DG_SPLITTER_01->GET_CONTAINER
    EXPORTING
      ROW       = 2
      COLUMN    = 1
    RECEIVING
      CONTAINER = DG_PARENT_02.

  CALL METHOD DG_SPLITTER_01->SET_ROW_HEIGHT
    EXPORTING
      ID     = 2
      HEIGHT = 20.

  CREATE OBJECT ALV_01
    EXPORTING
      I_PARENT = DG_PARENT_01.

  CREATE OBJECT ALV_02
    EXPORTING
      I_PARENT = DG_PARENT_02.

  CREATE OBJECT TOOLBAR_ALV_01
    EXPORTING
      IO_ALV_GRID = ALV_01.

  CREATE OBJECT TOOLBAR_ALV_02
    EXPORTING
      IO_ALV_GRID = ALV_02.

  GS_VARIANT_C_01-REPORT = SY-REPID.
  GS_VARIANT_C_02-REPORT = SY-REPID.

  CLEAR: IT_FIELDCATALOG_01,
         IT_FIELDCATALOG_02.

  PERFORM Z_EST_FIELDCAT TABLES IT_FIELDCATALOG_01 USING:
  'IT_NF_LIVRE'   'BUKRS'    C_TEXTOS-C_001 ' ' 01 05 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'BRANCH'   C_TEXTOS-C_002 ' ' 02 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'DOCNUM'   C_TEXTOS-C_003 ' ' 03 10 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'ITMNUM'   C_TEXTOS-C_004 ' ' 04 06 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'DOCDAT'   C_TEXTOS-C_005 ' ' 05 10 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'MODEL'    C_TEXTOS-C_006 ' ' 06 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'SERIES'   C_TEXTOS-C_007 ' ' 07 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'NFNUM'    C_TEXTOS-C_008 ' ' 08 09 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'NFE'      C_TEXTOS-C_009 ' ' 09 02 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'NFENUM'   C_TEXTOS-C_010 ' ' 10 09 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'MATNR'    C_TEXTOS-C_011 ' ' 11 08 'X' 'MATN1' SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'MAKTX'    C_TEXTOS-C_015 ' ' 12 20 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'NBM'      C_TEXTOS-C_012 ' ' 13 10 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'MENGE'    C_TEXTOS-C_013 ' ' 14 15 'X' SPACE   'X'   SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'MEINS'    C_TEXTOS-C_014 ' ' 15 05 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'SALDO'    C_TEXTOS-C_016 ' ' 16 15 'X' SPACE   'X'   SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE'   'VINCULAR' C_TEXTOS-C_017 ' ' 17 15 'X' SPACE   'X'   SPACE SPACE C_GRID_COLOR_C400 'X' ' '.

  PERFORM Z_EST_FIELDCAT TABLES IT_FIELDCATALOG_02 USING:
  'IT_NF_VINCU_S' 'BUKRS'    C_TEXTOS-C_001 ' ' 01 05 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'BRANCH'   C_TEXTOS-C_002 ' ' 02 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'DOCNUM'   C_TEXTOS-C_003 ' ' 03 10 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'ITMNUM'   C_TEXTOS-C_004 ' ' 04 06 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'DOCDAT'   C_TEXTOS-C_005 ' ' 05 10 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'MODEL'    C_TEXTOS-C_006 ' ' 06 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'SERIES'   C_TEXTOS-C_007 ' ' 07 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'NFNUM'    C_TEXTOS-C_008 ' ' 08 09 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'NFE'      C_TEXTOS-C_009 ' ' 09 02 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'NFENUM'   C_TEXTOS-C_010 ' ' 10 09 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'MATNR'    C_TEXTOS-C_011 ' ' 11 08 'X' 'MATN1' SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'MAKTX'    C_TEXTOS-C_015 ' ' 12 30 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'NBM'      C_TEXTOS-C_012 ' ' 13 15 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'MENGE'    C_TEXTOS-C_013 ' ' 14 20 'X' SPACE   'X'   SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU_S' 'MEINS'    C_TEXTOS-C_014 ' ' 15 05 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' '.

  WA_LAYOUT_01-SEL_MODE   = 'A'.
  WA_LAYOUT_01-NO_ROWMOVE = ABAP_TRUE.
  WA_LAYOUT_01-TOTALS_BEF = ABAP_FALSE.
  WA_LAYOUT_01-GRID_TITLE = C_TEXTOS-C_T01.

  WA_LAYOUT_02-SEL_MODE   = 'A'.
  WA_LAYOUT_02-NO_ROWMOVE = ABAP_TRUE.
  WA_LAYOUT_02-TOTALS_BEF = ABAP_TRUE.
  WA_LAYOUT_02-GRID_TITLE = C_TEXTOS-C_T02.

  CALL METHOD ALV_01->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = WA_LAYOUT_01
      IS_VARIANT      = GS_VARIANT_C_01
      I_SAVE          = 'A'
    CHANGING
      IT_FIELDCATALOG = IT_FIELDCATALOG_01
      IT_OUTTAB       = IT_NF_LIVRE[].

  CALL METHOD ALV_02->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = WA_LAYOUT_02
      IS_VARIANT      = GS_VARIANT_C_02
      I_SAVE          = 'A'
    CHANGING
      IT_FIELDCATALOG = IT_FIELDCATALOG_02
      IT_OUTTAB       = IT_NF_VINCU_S[].

*  CREATE OBJECT EVENT_RECEIVER_01.
*  SET HANDLER EVENT_RECEIVER_01->HANDLE_USER_COMMAND FOR ALV_01.

  SET HANDLER TOOLBAR_ALV_01->ON_TOOLBAR FOR ALV_01.
  SET HANDLER TOOLBAR_ALV_01->HANDLE_USER_COMMAND FOR ALV_01.
  SET HANDLER TOOLBAR_ALV_01->ON_DATA_CHANGED FOR ALV_01.

  SET HANDLER TOOLBAR_ALV_02->ON_TOOLBAR FOR ALV_02.
*  SET HANDLER EVENT_RECEIVER_01->HANDLE_USER_COMMAND FOR ALV_02.
  SET HANDLER TOOLBAR_ALV_02->HANDLE_USER_COMMAND FOR ALV_02.

  CALL METHOD ALV_01->REFRESH_TABLE_DISPLAY.
  CALL METHOD ALV_02->REFRESH_TABLE_DISPLAY.

ENDFORM.                    " CRIA_CONTAINER_NOTAS


*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
FORM Z_EST_FIELDCAT TABLES IT_CATALOGO TYPE LVC_T_FCAT
                           USING P_TAB_NAME
                                 P_FIELDNAME
                                 P_TEXTO_GRANDE
                                 P_HOT
                                 P_POSICAO
                                 P_OUTPUTLEN
                                 P_FIX_COLUMN
                                 P_CONVEXIT
                                 P_DO_SUM
                                 P_ICON
                                 P_JUST
                                 P_EMPHASIZE
                                 P_EDIT
                                 P_CHECKBOX.

  DATA: WA_CATALOG TYPE LVC_S_FCAT.
  WA_CATALOG-TABNAME     = P_TAB_NAME.
  WA_CATALOG-FIELDNAME   = P_FIELDNAME.
  WA_CATALOG-SCRTEXT_L   = P_TEXTO_GRANDE.
  WA_CATALOG-SCRTEXT_M   = P_TEXTO_GRANDE.
  WA_CATALOG-SCRTEXT_S   = P_TEXTO_GRANDE.
  WA_CATALOG-HOTSPOT     = P_HOT.
  WA_CATALOG-COL_POS     = P_POSICAO.
  WA_CATALOG-OUTPUTLEN   = P_OUTPUTLEN.
  WA_CATALOG-FIX_COLUMN  = P_FIX_COLUMN.
  WA_CATALOG-CONVEXIT    = P_CONVEXIT.
  WA_CATALOG-DO_SUM      = P_DO_SUM.
  WA_CATALOG-ICON        = P_ICON.
  WA_CATALOG-JUST        = P_JUST.
  WA_CATALOG-EMPHASIZE   = P_EMPHASIZE.
  WA_CATALOG-EDIT        = P_EDIT.
  WA_CATALOG-CHECKBOX    = P_CHECKBOX.
  APPEND WA_CATALOG TO IT_CATALOGO.

ENDFORM.                    " Z_ESTRUTURA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_NOTA_S_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VINCULAR_NOTA_S_NEW .

  DATA: VG_TABIX            TYPE SY-TABIX,
        VG_SALDO_A_VINCULAR TYPE J_1BNETQTY,
        VG_TOTAL_VINCULADO  TYPE J_1BNETQTY.

  VG_QTD_VINCU = ZDOC_MEMORANDO-QUANTIDADE_MEMO.

  IF VG_QTD_VINCU GT 0.

    PERFORM REAJUSTA_NOTAS_NEW USING SPACE.

    VG_SALDO_A_VINCULAR = VG_QTD_VINCU - VG_VINCULADO.

    IF VG_SALDO_A_VINCULAR GT 0.

      CALL METHOD ALV_01->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SELECTED_ROWS
          ET_ROW_NO	    = IT_ET_ROW_NO.

      IF IT_SELECTED_ROWS IS NOT INITIAL.

        DELETE IT_SELECTED_ROWS WHERE ROWTYPE NE ''.

        LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.

          READ TABLE IT_NF_LIVRE INTO WA_NF_LIVRE INDEX WA_SELECTED_ROWS-INDEX.
          MOVE-CORRESPONDING WA_NF_LIVRE TO WA_NF_VINCU.

          IF VG_SALDO_A_VINCULAR GT WA_NF_LIVRE-VINCULAR.
            WA_NF_VINCU-MENGE = WA_NF_LIVRE-VINCULAR.
          ELSEIF VG_SALDO_A_VINCULAR GT 0.
            WA_NF_VINCU-MENGE = VG_SALDO_A_VINCULAR.
          ELSE.
            WA_NF_VINCU-MENGE = 0.
          ENDIF.

          IF WA_NF_VINCU-MENGE GT 0.

            VG_TOTAL_VINCULADO = VG_VINCULADO + WA_NF_VINCU-MENGE.

            IF VG_TOTAL_VINCULADO GE ZDOC_MEMORANDO-QUANTIDADE_MEMO.
              WA_NF_VINCU-MENGE = ZDOC_MEMORANDO-QUANTIDADE_MEMO - VG_VINCULADO.
            ELSEIF VG_TOTAL_VINCULADO EQ ZDOC_MEMORANDO-QUANTIDADE_MEMO.
              WA_NF_VINCU-MENGE = 0.
            ENDIF.

            IF WA_NF_VINCU-MENGE GT 0.

              WA_NF_LIVRE-SALDO    = WA_NF_LIVRE-VINCULAR - WA_NF_VINCU-MENGE.
              WA_NF_LIVRE-VINCULAR = WA_NF_LIVRE-SALDO.

              VG_SALDO             = VG_SALDO - WA_NF_VINCU-MENGE.

              VG_VINCULADO        = VG_VINCULADO + WA_NF_VINCU-MENGE.
              VG_SALDO_A_VINCULAR = VG_SALDO_A_VINCULAR - WA_NF_VINCU-MENGE.

              DELETE IT_NF_DESV WHERE DOCNUM EQ WA_NF_VINCU-DOCNUM AND ITMNUM EQ WA_NF_VINCU-ITMNUM.

              READ TABLE IT_NF_VINCU_S INTO WA_NF_AUX WITH KEY DOCNUM = WA_NF_VINCU-DOCNUM
                                                               ITMNUM = WA_NF_VINCU-ITMNUM.
              IF SY-SUBRC EQ 0.
                WA_NF_AUX-MENGE = WA_NF_AUX-MENGE + WA_NF_VINCU-MENGE.
                MODIFY IT_NF_VINCU_S INDEX SY-TABIX FROM WA_NF_AUX TRANSPORTING MENGE.
              ELSE.
                APPEND WA_NF_VINCU TO IT_NF_VINCU_S.
              ENDIF.

              MODIFY IT_NF_LIVRE INDEX WA_SELECTED_ROWS-INDEX FROM WA_NF_LIVRE TRANSPORTING SALDO VINCULAR MARK.
              VG_ALTEROU_NOTAS = C_X.
            ENDIF.
          ENDIF.

        ENDLOOP.

      ELSE.

        LOOP AT IT_NF_LIVRE INTO WA_NF_LIVRE.

          VG_TABIX = SY-TABIX.
          CLEAR: WA_NF_LIVRE-MARK.
          MOVE-CORRESPONDING WA_NF_LIVRE TO WA_NF_VINCU.

          IF VG_SALDO_A_VINCULAR GT WA_NF_LIVRE-VINCULAR.
            WA_NF_VINCU-MENGE = WA_NF_LIVRE-VINCULAR.
          ELSEIF VG_SALDO_A_VINCULAR GT 0.
            WA_NF_VINCU-MENGE = VG_SALDO_A_VINCULAR.
          ELSE.
            WA_NF_VINCU-MENGE = 0.
          ENDIF.

          IF WA_NF_VINCU-MENGE GT 0.

            VG_TOTAL_VINCULADO = VG_VINCULADO + WA_NF_VINCU-MENGE.

            IF VG_TOTAL_VINCULADO GE ZDOC_MEMORANDO-QUANTIDADE_MEMO.
              WA_NF_VINCU-MENGE = ZDOC_MEMORANDO-QUANTIDADE_MEMO - VG_VINCULADO.
            ELSEIF VG_TOTAL_VINCULADO EQ ZDOC_MEMORANDO-QUANTIDADE_MEMO.
              WA_NF_VINCU-MENGE = 0.
            ENDIF.

            IF WA_NF_VINCU-MENGE GT 0.

              WA_NF_LIVRE-SALDO    = WA_NF_LIVRE-VINCULAR - WA_NF_VINCU-MENGE.
              WA_NF_LIVRE-VINCULAR = WA_NF_LIVRE-SALDO.

              VG_SALDO             = VG_SALDO - WA_NF_VINCU-MENGE.

              VG_VINCULADO        = VG_VINCULADO + WA_NF_VINCU-MENGE.
              VG_SALDO_A_VINCULAR = VG_SALDO_A_VINCULAR - WA_NF_VINCU-MENGE.

              DELETE IT_NF_DESV WHERE DOCNUM EQ WA_NF_VINCU-DOCNUM AND ITMNUM EQ WA_NF_VINCU-ITMNUM.

              READ TABLE IT_NF_VINCU_S INTO WA_NF_AUX WITH KEY DOCNUM = WA_NF_VINCU-DOCNUM
                                                               ITMNUM = WA_NF_VINCU-ITMNUM.
              IF SY-SUBRC EQ 0.
                WA_NF_AUX-MENGE = WA_NF_AUX-MENGE + WA_NF_VINCU-MENGE.
                MODIFY IT_NF_VINCU_S INDEX SY-TABIX FROM WA_NF_AUX TRANSPORTING MENGE.
              ELSE.
                APPEND WA_NF_VINCU TO IT_NF_VINCU_S.
              ENDIF.

              MODIFY IT_NF_LIVRE INDEX VG_TABIX FROM WA_NF_LIVRE TRANSPORTING SALDO VINCULAR MARK.
              VG_ALTEROU_NOTAS = C_X.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE S025 DISPLAY LIKE C_E.
  ENDIF.

  CALL METHOD ALV_01->REFRESH_TABLE_DISPLAY.
  CALL METHOD ALV_02->REFRESH_TABLE_DISPLAY.

ENDFORM.                    " VINCULAR_NOTA_S_NEW

*&---------------------------------------------------------------------*
*&      Form  REAJUSTA_NOTAS_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM REAJUSTA_NOTAS_NEW USING OP TYPE C .

  DATA: VG_TABIX  TYPE SY-TABIX.

  VG_QUANTIDADE = 0.
  VG_SALDO      = 0.

  LOOP AT IT_NF_LIVRE INTO WA_NF_LIVRE.

    VG_TABIX = SY-TABIX.

    IF NOT OP IS INITIAL.
      READ TABLE IT_NF_DESV WITH KEY DOCNUM = WA_NF_LIVRE-DOCNUM ITMNUM = WA_NF_LIVRE-ITMNUM.
    ENDIF.

    IF ( SY-SUBRC EQ 0 ) OR ( OP IS INITIAL ).
      IF OP IS INITIAL.
        WA_NF_LIVRE-SALDO    = WA_NF_LIVRE-MENGE.
        WA_NF_LIVRE-VINCULAR = WA_NF_LIVRE-MENGE.
      ENDIF.
      PERFORM SALDO_DOC_NUM_ITM USING WA_NF_LIVRE-DOCNUM  WA_NF_LIVRE-ITMNUM WA_NF_LIVRE-MENGE WA_NF_LIVRE-SALDO.
      WA_NF_LIVRE-VINCULAR = WA_NF_LIVRE-SALDO.
      MODIFY IT_NF_LIVRE INDEX VG_TABIX FROM WA_NF_LIVRE TRANSPORTING SALDO VINCULAR.
    ENDIF.

    VG_QUANTIDADE = VG_QUANTIDADE + WA_NF_LIVRE-MENGE.
    VG_SALDO      = VG_SALDO      + WA_NF_LIVRE-SALDO.

  ENDLOOP.

  "SORT IT_NF_LIVRE BY DOCDAT.

  VG_VINCULADO = 0.
  LOOP AT IT_NF_VINCU INTO WA_NF_VINCU.
    VG_VINCULADO = VG_VINCULADO + WA_NF_VINCU-MENGE.
  ENDLOOP.

  LOOP AT IT_NF_VINCU_S INTO WA_NF_VINCU.
    VG_VINCULADO = VG_VINCULADO + WA_NF_VINCU-MENGE.
  ENDLOOP.

  CLEAR: IT_NF_DESV[].

ENDFORM.                    " REAJUSTA_NOTAS_NEW

*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR_NOTA_S_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DESVINCULAR_NOTA_S_NEW .

  CALL METHOD ALV_02->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  IF IT_SELECTED_ROWS IS NOT INITIAL.

    DELETE IT_SELECTED_ROWS WHERE ROWTYPE NE ''.

    LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
      READ TABLE IT_NF_VINCU_S INTO WA_NF_VINCU INDEX WA_SELECTED_ROWS-INDEX.
      IT_NF_VINCU_S-MENGE = 0.
      MODIFY IT_NF_VINCU_S INDEX WA_SELECTED_ROWS-INDEX TRANSPORTING MENGE.
      APPEND WA_NF_VINCU TO IT_NF_DESV.
      VG_ALTEROU_NOTAS = C_X.
    ENDLOOP.

    DELETE IT_NF_VINCU_S WHERE MENGE EQ 0.

    PERFORM REAJUSTA_NOTAS_S USING C_X.
  ELSE.
    LOOP AT IT_NF_VINCU_S INTO WA_NF_VINCU.
      CLEAR: WA_NF_VINCU-MARK.
      IT_NF_VINCU_S-MENGE = 0.
      MODIFY IT_NF_VINCU_S INDEX SY-TABIX TRANSPORTING MENGE.
      APPEND WA_NF_VINCU TO IT_NF_DESV.
      VG_ALTEROU_NOTAS = C_X.
    ENDLOOP.

    DELETE IT_NF_VINCU_S WHERE MENGE EQ 0.

    PERFORM REAJUSTA_NOTAS_S USING C_X.
  ENDIF.

  CALL METHOD ALV_01->REFRESH_TABLE_DISPLAY.
  CALL METHOD ALV_02->REFRESH_TABLE_DISPLAY.

ENDFORM.                    " DESVINCULAR_NOTA_S_NEW

*&---------------------------------------------------------------------*
*&      Form  LANCAR_MEMORANDOS_NOTAS_S_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LANCAR_MEMORANDOS_NOTAS_S_NEW .

  DATA: VG_VERIFICA_SELECAO TYPE SY-SUBRC.

  PERFORM VERIFICA_SELECAO_MEMORANDO USING VG_VERIFICA_SELECAO.

  IF VG_VERIFICA_SELECAO EQ 0.
    IF WA_MEMORANDO-DIRECAO EQ C_2.
      PERFORM VERIFICA_BLOQUEIO USING WA_MEMORANDO-NR_MEMORANDO.

      VG_4002_6002     = C_6003.
      VG_DYNNR_ANT     = VG_DYNNR_000.
      VG_DYNNR_000     = C_6000.
      J_1BNFDOC-DOCDAT = SY-DATUM.
      J_1BNFDOC-PSTDAT = SY-DATUM.
      VG_QTD_VINCU     = 0.
      MOVE-CORRESPONDING WA_MEMORANDO TO ZDOC_MEMORANDO.
      MOVE-CORRESPONDING WA_MEMORANDO TO ZDOC_MEMO_NF_EXP.
      PERFORM POPULAR_NOTAS_VINCULADAS_S USING ZDOC_MEMORANDO-NR_MEMORANDO.
    ELSE.
      MESSAGE TEXT-E09 TYPE C_S DISPLAY LIKE C_E.
    ENDIF.
  ENDIF.

ENDFORM.                    " LANCAR_MEMORANDOS_NOTAS_S_NEW

*&---------------------------------------------------------------------*
*&      Form  LANCAR_MEMORANDOS_NOTAS_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM LANCAR_MEMORANDOS_NOTAS_NEW .

  DATA: VG_VERIFICA_SELECAO TYPE SY-SUBRC.

  PERFORM VERIFICA_SELECAO_MEMORANDO USING VG_VERIFICA_SELECAO.

  IF VG_VERIFICA_SELECAO EQ 0.

    IF WA_MEMORANDO-DIRECAO EQ C_1.

      PERFORM VERIFICA_BLOQUEIO USING WA_MEMORANDO-NR_MEMORANDO.

      VG_4002_6002     = C_4003.
      VG_DYNNR_ANT     = VG_DYNNR_000.
      VG_DYNNR_000     = C_4000.
      J_1BNFDOC-DOCDAT = SY-DATUM.
      J_1BNFDOC-PSTDAT = SY-DATUM.
      VG_QTD_VINCU     = 0.
      MOVE-CORRESPONDING WA_MEMORANDO TO ZDOC_MEMORANDO.
      MOVE-CORRESPONDING WA_MEMORANDO TO ZDOC_MEMO_NF_EXP.
      PERFORM POPULAR_NOTAS_VINCULADAS USING ZDOC_MEMORANDO-NR_MEMORANDO.
    ELSE.
      MESSAGE TEXT-E09 TYPE C_S DISPLAY LIKE C_E.
    ENDIF.

  ENDIF.

ENDFORM.                    " LANCAR_MEMORANDOS_NOTAS_NEW

*&---------------------------------------------------------------------*
*&      Module  STATUS_4003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_4003 OUTPUT.

  CLEAR: VG_NOME_EMPRESA, VG_NOME_CENTRO.

  IF J_1BNFDOC-BUKRS IS INITIAL.
    SELECT SINGLE BUKRS INTO J_1BNFDOC-BUKRS
      FROM J_1BBRANCH
     WHERE BRANCH EQ ZDOC_MEMORANDO-REMETENTE+6(4).

    J_1BNFDOC-BRANCH = ZDOC_MEMORANDO-REMETENTE+6(4).
  ENDIF.

  SELECT SINGLE BUTXT INTO VG_NOME_EMPRESA
    FROM T001
   WHERE BUKRS EQ J_1BNFDOC-BUKRS.

  SELECT SINGLE NAME1 INTO VG_NOME_CENTRO
    FROM T001W
   WHERE WERKS EQ ZDOC_MEMORANDO-REMETENTE+6(4).

  IF G_CUSTOM_CONTAINER_03 IS INITIAL.
    PERFORM CRIA_CONTAINER_NOTAS_03.
  ENDIF.

  CALL METHOD ALV_03->REFRESH_TABLE_DISPLAY.
  CALL METHOD ALV_04->REFRESH_TABLE_DISPLAY.

ENDMODULE.                 " STATUS_4003  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_CONTAINER_NOTAS_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CRIA_CONTAINER_NOTAS_03 .

  DATA: URL(255)  TYPE C.

* create a container for the tree control
  CREATE OBJECT G_CUSTOM_CONTAINER_03
    EXPORTING
      CONTAINER_NAME              = 'ALV_NOTAS'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.

  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

  CREATE OBJECT DG_SPLITTER_03
    EXPORTING
      PARENT  = G_CUSTOM_CONTAINER_03
      ROWS    = 2
      COLUMNS = 1.

  CALL METHOD DG_SPLITTER_03->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = DG_PARENT_03.

  CALL METHOD DG_SPLITTER_03->GET_CONTAINER
    EXPORTING
      ROW       = 2
      COLUMN    = 1
    RECEIVING
      CONTAINER = DG_PARENT_04.

  CALL METHOD DG_SPLITTER_03->SET_ROW_HEIGHT
    EXPORTING
      ID     = 2
      HEIGHT = 20.

  CREATE OBJECT ALV_03
    EXPORTING
      I_PARENT = DG_PARENT_03.

  CREATE OBJECT ALV_04
    EXPORTING
      I_PARENT = DG_PARENT_04.

  CREATE OBJECT TOOLBAR_ALV_03
    EXPORTING
      IO_ALV_GRID = ALV_03.

  CREATE OBJECT TOOLBAR_ALV_04
    EXPORTING
      IO_ALV_GRID = ALV_04.

  GS_VARIANT_C_03-REPORT = SY-REPID.
  GS_VARIANT_C_04-REPORT = SY-REPID.

  CLEAR: IT_FIELDCATALOG_03,
         IT_FIELDCATALOG_04.

  PERFORM Z_EST_FIELDCAT TABLES IT_FIELDCATALOG_03 USING:
  'IT_NF_LIVRE' 'BUKRS'    C_TEXTOS-C_001 ' ' 01 05 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'BRANCH'   C_TEXTOS-C_002 ' ' 02 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'DOCNUM'   C_TEXTOS-C_003 ' ' 03 10 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'ITMNUM'   C_TEXTOS-C_004 ' ' 04 06 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'DOCDAT'   C_TEXTOS-C_005 ' ' 05 10 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'MODEL'    C_TEXTOS-C_006 ' ' 06 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'SERIES'   C_TEXTOS-C_007 ' ' 07 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'NFNUM'    C_TEXTOS-C_008 ' ' 08 09 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'NFE'      C_TEXTOS-C_009 ' ' 09 02 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'NFENUM'   C_TEXTOS-C_010 ' ' 10 09 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'MATNR'    C_TEXTOS-C_011 ' ' 11 08 'X' 'MATN1' SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'MAKTX'    C_TEXTOS-C_015 ' ' 12 20 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'NBM'      C_TEXTOS-C_012 ' ' 13 10 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'MENGE'    C_TEXTOS-C_013 ' ' 14 15 'X' SPACE   'X'   SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'MEINS'    C_TEXTOS-C_014 ' ' 15 05 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'SALDO'    C_TEXTOS-C_016 ' ' 16 15 'X' SPACE   'X'   SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_LIVRE' 'VINCULAR' C_TEXTOS-C_017 ' ' 17 15 'X' SPACE   'X'   SPACE SPACE C_GRID_COLOR_C400 'X' ' '.

  PERFORM Z_EST_FIELDCAT TABLES IT_FIELDCATALOG_04 USING:
  'IT_NF_VINCU' 'BUKRS'    C_TEXTOS-C_001 ' ' 01 05 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'BRANCH'   C_TEXTOS-C_002 ' ' 02 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'DOCNUM'   C_TEXTOS-C_003 ' ' 03 10 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'ITMNUM'   C_TEXTOS-C_004 ' ' 04 06 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'DOCDAT'   C_TEXTOS-C_005 ' ' 05 10 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'MODEL'    C_TEXTOS-C_006 ' ' 06 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'SERIES'   C_TEXTOS-C_007 ' ' 07 04 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'NFNUM'    C_TEXTOS-C_008 ' ' 08 09 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'NFE'      C_TEXTOS-C_009 ' ' 09 02 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'NFENUM'   C_TEXTOS-C_010 ' ' 10 09 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'MATNR'    C_TEXTOS-C_011 ' ' 11 08 'X' 'MATN1' SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'MAKTX'    C_TEXTOS-C_015 ' ' 12 30 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'NBM'      C_TEXTOS-C_012 ' ' 13 15 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'MENGE'    C_TEXTOS-C_013 ' ' 14 20 'X' SPACE   'X'   SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
  'IT_NF_VINCU' 'MEINS'    C_TEXTOS-C_014 ' ' 15 05 'X' SPACE   SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' '.

  WA_LAYOUT_03-SEL_MODE   = 'A'.
  WA_LAYOUT_03-NO_ROWMOVE = ABAP_TRUE.
  WA_LAYOUT_03-TOTALS_BEF = ABAP_TRUE.
  WA_LAYOUT_03-GRID_TITLE = C_TEXTOS-C_T03.

  WA_LAYOUT_04-SEL_MODE   = 'A'.
  WA_LAYOUT_04-NO_ROWMOVE = ABAP_TRUE.
  WA_LAYOUT_04-TOTALS_BEF = ABAP_FALSE.
  WA_LAYOUT_04-GRID_TITLE = C_TEXTOS-C_T04.

  CALL METHOD ALV_03->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = WA_LAYOUT_03
      IS_VARIANT      = GS_VARIANT_C_03
      I_SAVE          = 'A'
    CHANGING
      IT_FIELDCATALOG = IT_FIELDCATALOG_03
      IT_OUTTAB       = IT_NF_LIVRE[].

  CALL METHOD ALV_04->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = WA_LAYOUT_04
      IS_VARIANT      = GS_VARIANT_C_04
      I_SAVE          = 'A'
    CHANGING
      IT_FIELDCATALOG = IT_FIELDCATALOG_04
      IT_OUTTAB       = IT_NF_VINCU[].

  SET HANDLER TOOLBAR_ALV_03->ON_TOOLBAR FOR ALV_03.
  SET HANDLER TOOLBAR_ALV_03->HANDLE_USER_COMMAND FOR ALV_03.
  SET HANDLER TOOLBAR_ALV_03->ON_DATA_CHANGED FOR ALV_03.

  SET HANDLER TOOLBAR_ALV_04->ON_TOOLBAR FOR ALV_04.
  SET HANDLER TOOLBAR_ALV_04->HANDLE_USER_COMMAND FOR ALV_04.

  CALL METHOD ALV_03->REFRESH_TABLE_DISPLAY.
  CALL METHOD ALV_04->REFRESH_TABLE_DISPLAY.

ENDFORM.                    " CRIA_CONTAINER_NOTAS_03

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_NOTA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VINCULAR_NOTA_NEW .

  DATA: VG_TABIX            TYPE SY-TABIX,
        VG_SALDO_A_VINCULAR TYPE J_1BNETQTY,
        VG_TOTAL_VINCULADO  TYPE J_1BNETQTY.

  VG_QTD_VINCU = ZDOC_MEMORANDO-QUANTIDADE_MEMO.

  IF VG_QTD_VINCU GT 0.

    PERFORM REAJUSTA_NOTAS_NEW USING SPACE.

    VG_SALDO_A_VINCULAR = VG_QTD_VINCU - VG_VINCULADO.

    IF VG_SALDO_A_VINCULAR GT 0.

      CALL METHOD ALV_03->GET_SCROLL_INFO_VIA_ID.

      CALL METHOD ALV_03->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SELECTED_ROWS
          ET_ROW_NO	    = IT_ET_ROW_NO.

      IF IT_SELECTED_ROWS IS NOT INITIAL.

        DELETE IT_SELECTED_ROWS WHERE ROWTYPE NE ''.

        LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.

          READ TABLE IT_NF_LIVRE INTO WA_NF_LIVRE INDEX WA_SELECTED_ROWS-INDEX.
          MOVE-CORRESPONDING WA_NF_LIVRE TO WA_NF_VINCU.

          IF VG_SALDO_A_VINCULAR GT WA_NF_LIVRE-VINCULAR.
            WA_NF_VINCU-MENGE = WA_NF_LIVRE-VINCULAR.
          ELSEIF VG_SALDO_A_VINCULAR GT 0.
            WA_NF_VINCU-MENGE = VG_SALDO_A_VINCULAR.
          ELSE.
            WA_NF_VINCU-MENGE = 0.
          ENDIF.

          IF WA_NF_VINCU-MENGE GT 0.

            VG_TOTAL_VINCULADO = VG_VINCULADO + WA_NF_VINCU-MENGE.

            IF VG_TOTAL_VINCULADO GE ZDOC_MEMORANDO-QUANTIDADE_MEMO.
              WA_NF_VINCU-MENGE = ZDOC_MEMORANDO-QUANTIDADE_MEMO - VG_VINCULADO.
            ELSEIF VG_TOTAL_VINCULADO EQ ZDOC_MEMORANDO-QUANTIDADE_MEMO.
              WA_NF_VINCU-MENGE = 0.
            ENDIF.

            IF WA_NF_VINCU-MENGE GT 0.

              WA_NF_LIVRE-SALDO    = WA_NF_LIVRE-VINCULAR - WA_NF_VINCU-MENGE.
              WA_NF_LIVRE-VINCULAR = WA_NF_LIVRE-SALDO.

              VG_SALDO             = VG_SALDO - WA_NF_VINCU-MENGE.

              VG_VINCULADO         = VG_VINCULADO + WA_NF_VINCU-MENGE.
              VG_SALDO_A_VINCULAR  = VG_SALDO_A_VINCULAR - WA_NF_VINCU-MENGE.

              DELETE IT_NF_DESV WHERE DOCNUM EQ WA_NF_VINCU-DOCNUM AND ITMNUM EQ WA_NF_VINCU-ITMNUM.

              READ TABLE IT_NF_VINCU INTO WA_NF_AUX WITH KEY DOCNUM = WA_NF_VINCU-DOCNUM
                                                             ITMNUM = WA_NF_VINCU-ITMNUM.
              IF SY-SUBRC EQ 0.
                WA_NF_AUX-MENGE = WA_NF_AUX-MENGE + WA_NF_VINCU-MENGE.
                MODIFY IT_NF_VINCU INDEX SY-TABIX FROM WA_NF_AUX TRANSPORTING MENGE.
              ELSE.
                APPEND WA_NF_VINCU TO IT_NF_VINCU.
              ENDIF.

              MODIFY IT_NF_LIVRE INDEX WA_SELECTED_ROWS-INDEX FROM WA_NF_LIVRE TRANSPORTING SALDO VINCULAR MARK.
              VG_ALTEROU_NOTAS = C_X.
            ENDIF.
          ENDIF.

        ENDLOOP.

      ELSE.

        LOOP AT IT_NF_LIVRE INTO WA_NF_LIVRE.

          VG_TABIX = SY-TABIX.
          CLEAR: WA_NF_LIVRE-MARK.
          MOVE-CORRESPONDING WA_NF_LIVRE TO WA_NF_VINCU.

          IF VG_SALDO_A_VINCULAR GT WA_NF_LIVRE-VINCULAR.
            WA_NF_VINCU-MENGE = WA_NF_LIVRE-VINCULAR.
          ELSEIF VG_SALDO_A_VINCULAR GT 0.
            WA_NF_VINCU-MENGE = VG_SALDO_A_VINCULAR.
          ELSE.
            WA_NF_VINCU-MENGE = 0.
          ENDIF.

          IF WA_NF_VINCU-MENGE GT 0.

            VG_TOTAL_VINCULADO = VG_VINCULADO + WA_NF_VINCU-MENGE.

            IF VG_TOTAL_VINCULADO GE ZDOC_MEMORANDO-QUANTIDADE_MEMO.
              WA_NF_VINCU-MENGE = ZDOC_MEMORANDO-QUANTIDADE_MEMO - VG_VINCULADO.
            ELSEIF VG_TOTAL_VINCULADO EQ ZDOC_MEMORANDO-QUANTIDADE_MEMO.
              WA_NF_VINCU-MENGE = 0.
            ENDIF.

            IF WA_NF_VINCU-MENGE GT 0.

              WA_NF_LIVRE-SALDO    = WA_NF_LIVRE-VINCULAR - WA_NF_VINCU-MENGE.
              WA_NF_LIVRE-VINCULAR = WA_NF_LIVRE-SALDO.

              VG_SALDO             = VG_SALDO - WA_NF_VINCU-MENGE.

              VG_VINCULADO        = VG_VINCULADO + WA_NF_VINCU-MENGE.
              VG_SALDO_A_VINCULAR = VG_SALDO_A_VINCULAR - WA_NF_VINCU-MENGE.

              DELETE IT_NF_DESV WHERE DOCNUM EQ WA_NF_VINCU-DOCNUM AND ITMNUM EQ WA_NF_VINCU-ITMNUM.

              READ TABLE IT_NF_VINCU INTO WA_NF_AUX WITH KEY DOCNUM = WA_NF_VINCU-DOCNUM
                                                             ITMNUM = WA_NF_VINCU-ITMNUM.
              IF SY-SUBRC EQ 0.
                WA_NF_AUX-MENGE = WA_NF_AUX-MENGE + WA_NF_VINCU-MENGE.
                MODIFY IT_NF_VINCU INDEX SY-TABIX FROM WA_NF_AUX TRANSPORTING MENGE.
              ELSE.
                APPEND WA_NF_VINCU TO IT_NF_VINCU.
              ENDIF.

              MODIFY IT_NF_LIVRE INDEX VG_TABIX FROM WA_NF_LIVRE TRANSPORTING SALDO VINCULAR MARK.
              VG_ALTEROU_NOTAS = C_X.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE S025 DISPLAY LIKE C_E.
  ENDIF.

  CALL METHOD ALV_03->REFRESH_TABLE_DISPLAY.
  CALL METHOD ALV_04->REFRESH_TABLE_DISPLAY.

ENDFORM.                    " VINCULAR_NOTA_NEW

*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR_NOTA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DESVINCULAR_NOTA_NEW .

  CALL METHOD ALV_04->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  IF IT_SELECTED_ROWS IS NOT INITIAL.

    DELETE IT_SELECTED_ROWS WHERE ROWTYPE NE ''.

    LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
      READ TABLE IT_NF_VINCU INTO WA_NF_VINCU INDEX WA_SELECTED_ROWS-INDEX.
      IT_NF_VINCU_S-MENGE = 0.
      MODIFY IT_NF_VINCU INDEX WA_SELECTED_ROWS-INDEX TRANSPORTING MENGE.
      APPEND WA_NF_VINCU TO IT_NF_DESV.
      VG_ALTEROU_NOTAS = C_X.
    ENDLOOP.

    DELETE IT_NF_VINCU WHERE MENGE EQ 0.

    PERFORM REAJUSTA_NOTAS_NEW USING C_X.
  ELSE.
    LOOP AT IT_NF_VINCU INTO WA_NF_VINCU.
      CLEAR: WA_NF_VINCU-MARK.
      IT_NF_VINCU_S-MENGE = 0.
      MODIFY IT_NF_VINCU INDEX SY-TABIX TRANSPORTING MENGE.
      APPEND WA_NF_VINCU TO IT_NF_DESV.
      VG_ALTEROU_NOTAS = C_X.
    ENDLOOP.

    DELETE IT_NF_VINCU WHERE MENGE EQ 0.

    PERFORM REAJUSTA_NOTAS_NEW USING C_X.
  ENDIF.

  CALL METHOD ALV_03->REFRESH_TABLE_DISPLAY.
  CALL METHOD ALV_04->REFRESH_TABLE_DISPLAY.

ENDFORM.                    " DESVINCULAR_NOTA_NEW
