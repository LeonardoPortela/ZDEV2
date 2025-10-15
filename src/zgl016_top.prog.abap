*&---------------------------------------------------------------------*
*& Include ZGL016_TOP                                        PoolMóds.        ZGL016
*&
*&---------------------------------------------------------------------*

PROGRAM  ZGL016.
DATA  WGZGLT035-LOTE(10).
DATA  WGZGLT036-BUKRS(15).
DATA  WGZGLT031-_DPTO_RESP(20).
INCLUDE: <ICON>.


*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF TY_CABECALHO,
         BUKRS        TYPE ZGLT035-BUKRS,
         BUTXT        TYPE T001-BUTXT,
         LOTE         TYPE ZGLT035-LOTE,
         DESCR_LOTE   TYPE ZGLT034-DESCR_LOTE,
         DPTO_RESP    TYPE ZGLT035-DPTO_RESP,
         DEPARTAMENTO TYPE ZGLT033-DEPARTAMENTO,
       END OF TY_CABECALHO,

       BEGIN OF TY_ITEM,
         MARK(1),
         STATUS          TYPE ICON-ID,
         DOC_LCTO        TYPE ZGLT036-DOC_LCTO,
         TP_LCTO         TYPE ZGLT036-TP_LCTO,
         MOEDA_DOC       TYPE ZGLT035-MOEDA_DOC,
*          dt_doc            TYPE zglt035-dt_doc,
*          dt_lcto           TYPE zglt035-dt_lcto,
         BSCHL           TYPE ZGLT036-BSCHL,
         HKONT           TYPE ZGLT036-HKONT,
         DESCR           TYPE CHAR30,
         UMSKZ           TYPE ZGLT036-UMSKZ,
         ANBWA           TYPE ZGLT036-ANBWA,
         BEWAR           TYPE ZGLT036-BEWAR,
         VBUND           TYPE ZGLT036-VBUND,
         KOSTL           TYPE ZGLT036-KOSTL,
         PRCTR           TYPE ZGLT036-PRCTR,
         AUFNR           TYPE ZGLT036-AUFNR,
         MATNR           TYPE ZGLT036-MATNR,
         MATNR_FI        TYPE ZGLT036-MATNR_FI,
         D_C             TYPE CHAR1,
         GSBER           TYPE ZGLT036-GSBER,
         ZUONR           TYPE ZGLT036-ZUONR,
         SGTXT           TYPE ZGLT036-SGTXT,
         VLR_MOEDA_DOC   TYPE ZGLT036-VLR_MOEDA_DOC,
         VLR_MOEDA_INT   TYPE ZGLT036-VLR_MOEDA_INT,
         VLR_MOEDA_FORTE TYPE ZGLT036-VLR_MOEDA_FORTE,
         VLR_MOEDA_GRUPO TYPE ZGLT036-VLR_MOEDA_GRUPO,
* Início - CS2019001942 - Sara Oikawa - Jun/2020
         IVA             TYPE MWSKZ,
         IVA_TXT         TYPE TEXT1_007S,
* Fim - CS2019001942 - Sara Oikawa - Jun/2020
       END OF TY_ITEM.

* Início - CS2019001942 - Sara Oikawa - Jun/2020
TYPES: BEGIN OF TY_T005,
         LAND1 TYPE T005-LAND1,
         KALSM TYPE T005-KALSM,
       END OF TY_T005.
* Fim - CS2019001942 - Sara Oikawa - Jun/2020

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: OK-CODE         TYPE SY-UCOMM,
      TG_SELECTEDCELL TYPE LVC_T_CELL,
      WG_SELECTEDCELL TYPE LVC_S_CELL,
      X_FIELD(30).

DATA: TG_FIELDCATALOG     TYPE LVC_T_FCAT,
      WG_FIELDCATALOG     TYPE LVC_S_FCAT,
      WA_LAYOUT           TYPE LVC_S_LAYO,
      WA_STABLE           TYPE LVC_S_STBL,
      TG_SORT             TYPE LVC_T_SORT,
      WG_SORT             TYPE LVC_S_SORT,
      IT_ZGLT035          TYPE TABLE OF ZGLT035,
      WA_ZGLT035          TYPE ZGLT035,
      WA_ZIB_CONTABIL_CHV TYPE ZIB_CONTABIL_CHV.

DATA: WG_CABECALHO TYPE TY_CABECALHO,
      TG_ITEM      TYPE TABLE OF TY_ITEM,
      WG_ITEM      TYPE TY_ITEM.

DATA: OK_CODE         LIKE SY-UCOMM,
      WG_MENSAGEM(30),
      WG_ACAO(30).
DATA:WA_BOTAOTESTE(10).
*Class definition for ALV toolbar
CLASS:  LCL_ALV_TOOLBAR     DEFINITION DEFERRED.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: G_CONTAINER          TYPE SCRFNAME VALUE 'CC_ITEM',
      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,
      SPLITTER             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GRID1                TYPE REF TO CL_GUI_ALV_GRID,
      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,

      WA_STYLE             TYPE LVC_S_STYL,
      STYLE                TYPE LVC_T_STYL   WITH HEADER LINE,
      STYLE2               TYPE LVC_T_STYL   WITH HEADER LINE.

*Declaration for toolbar buttons
DATA: TY_TOOLBAR            TYPE STB_BUTTON.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS:
  C_0              TYPE C VALUE '0',
  C_1              TYPE C VALUE '1',
  C_X              TYPE C VALUE 'X',
  C_I              TYPE C VALUE 'I',
  C_N              TYPE C VALUE 'N',
  C_NE(2)          TYPE C VALUE 'NE',
  C_ADD(3)         TYPE C VALUE 'ADD',
  C_DEL(3)         TYPE C VALUE 'DEL',
  C_EXIT(4)        TYPE C VALUE 'EXIT',
  C_BACK(4)        TYPE C VALUE 'BACK',
  C_SAVE(4)        TYPE C VALUE 'SAVE',
  C_MODIF(5)       TYPE C VALUE 'MODIF',
  C_CANCEL(6)      TYPE C VALUE 'CANCEL',
  C_DELDOC(6)      TYPE C VALUE 'DELDOC',
  C_SEARCH(6)      TYPE C VALUE 'SEARCH',
  C_DISPLA(6)      TYPE C VALUE 'DISPLA',
  C_ATUALI(6)      TYPE C VALUE 'ATUALI',
  C_LIBERAR(7)     TYPE C VALUE 'LIBERAR',
  C_CLOS_MSG(8)    TYPE C VALUE 'CLOS_MSG',
  C_REINICIAR(9)   TYPE C VALUE 'REINICIAR',
  C_SHOW_MSGRE(10) TYPE C VALUE 'SHOW_MSGRE',
  C_BOTAOTESTE(10) TYPE C VALUE 'BOTAOTESTE'.


*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.

    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      CONSTRUCTOR
        IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: WL_DESACTIVE.

*    wl_desactive = 1.

    TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  = C_ADD.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  = C_DEL.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.
**    DATA: tl_item_aux   TYPE TABLE OF ty_item,
**          wl_item       TYPE ty_item.
**
**    REFRESH: tl_item_aux.
**
**    CASE e_ucomm.
**      WHEN c_add.
**        tl_item_aux[] = tg_item[].
**        REFRESH: tg_item.
**        LOOP AT tl_item_aux INTO wl_item.
**          APPEND wl_item TO tg_item.
**        ENDLOOP.
**
**        CLEAR: wl_item.
**        APPEND wl_item TO tg_item.
**
**        CALL METHOD grid1->refresh_table_display
**          EXPORTING
**            is_stable = wa_stable.
**      WHEN c_del.
**
**    ENDCASE.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*

"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD ON_DOUBLE_CLICK.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD ON_DATA_CHANGED.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD ON_DATA_CHANGED_FINISHED.

**    PERFORM: f_atualiza_alv.

*** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

**    PERFORM f_verifica_erros.
**    CALL FUNCTION 'Z_DOC_CHECK_NEW'
**      EXPORTING
**        i_screen      = '100'
**        i_show        = space
**        i_repid       = sy-repid
**        i_pressed_tab = 'TS_100-PRESSED_TAB'
**        i_set_field   = 'X_FIELD'
**      IMPORTING
**        e_messagem    = wg_mensagem
**      TABLES
**        it_msgs       = tg_msg_ret.

  ENDMETHOD.                    "on_data_changed_finished

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
