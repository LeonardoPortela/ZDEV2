*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio Luiz R. da Silva                                &*
*& Data.....: 21/05/2013                                              &*
*& Descrição: Solicitação de Ordem de Venda - INSUMOS                 &*
*& Transação: ZMM0045                                                 &*
*---------------------------------------------------------------------&*

REPORT  ZMMR034.


*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*

TYPES: BEGIN OF TY_CADLAN,
         NRO_SOL_CP TYPE ZMMT0035-NRO_SOL_CP,
         SAFRA      TYPE ZMMT0035-SAFRA,
         BSTYP      TYPE ZMMT0035-BSTYP,
         BSART      TYPE ZMMT0035-BSART,
         BATXT      TYPE T161T-BATXT,
         EKGRP      TYPE ZMMT0035-EKGRP,
         EKNAM      TYPE T024-EKNAM,
         WERKS      TYPE T001W-WERKS,
         NAME1_W    TYPE T001W-NAME1,
         LIFNR      TYPE LFA1-LIFNR,
         NAME1_L    TYPE LFA1-NAME1,
         PED_FORN   TYPE ZMMT0035-PED_FORN,
         WAERS      TYPE ZMMT0035-WAERS,
         WKURS      TYPE ZMMT0035-WKURS,
         ZTERM      TYPE T052-ZTERM,
         TEXT1      TYPE T052U-TEXT1,
         EBELN      TYPE ZMMT0035-EBELN,
         BANFN      TYPE ZMMT0035-BANFN,
         IHRAN      TYPE ZMMT0035-IHRAN,
         TEXTO_NEG  TYPE ZMMT0035-TEXTO_NEG,
         BVTYP      TYPE ZMMT0035-BVTYP,
       END OF TY_CADLAN,

       BEGIN OF TY_FIELDS,
         CAMPO(30) TYPE C,
         GROUP1(5) TYPE C,
         VALUE     TYPE SY-TABIX,
         INVISIBLE TYPE SY-TABIX,
       END   OF TY_FIELDS,

       BEGIN OF TY_EDITOR,
         LINE(72),
       END   OF TY_EDITOR.


*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: OK-CODE         TYPE SY-UCOMM,
      WG_CADLAN       TYPE TY_CADLAN,
      TG_SELECTEDCELL TYPE LVC_T_CELL,
      WG_SELECTEDCELL TYPE LVC_S_CELL,
      X_FIELD(30),
      VTEXTO(50),


      BEGIN OF TG_FATURA OCCURS 0,
        MARK(1),
        DT_VCTO    TYPE ZMMT0036-DT_VCTO,
        PERCENTUAL TYPE ZMMT0036-PERCENTUAL,
        VALOR      TYPE ZMMT0036-VALOR,
        MENGE      TYPE ZMMT0036-MENGE,
      END OF TG_FATURA,

      BEGIN OF TG_PRODUTO OCCURS 0,
        MARK(1),
        EBELP       TYPE ZMMT0037-EBELP,
        MATNR       TYPE MAKT-MATNR,
        MAKTX       TYPE MAKT-MAKTX,
        LGORT       TYPE ZMMT0037-LGORT,
        MENGE       TYPE ZMMT0037-MENGE,
        MEINS       TYPE ZMMT0037-MEINS,
        VALOR       TYPE ZMMT0037-NETPR,
        NETPR       TYPE ZMMT0037-NETPR,
        NETPR_DESC  TYPE ZMMT0037-NETPR_DESC,
        NETPR_FINAL TYPE ZMMT0037-NETPR,
        PEINH       TYPE ZMMT0037-PEINH,
        MWSKZ       TYPE ZMMT0037-MWSKZ,
      END OF TG_PRODUTO,

      BEGIN OF TG_PROGRAMA OCCURS 0,
        MARK(1),
        EBELP      TYPE ZMMT0038-EBELP,
        MATNR      TYPE MAKT-MATNR,
        MAKTX      TYPE MAKT-MAKTX,
        DATA_PROGR TYPE ZMMT0038-DATA_PROGR,
        MENGE      TYPE ZMMT0038-MENGE,
        OBSERV     TYPE ZMMT0038-OBSERV,
      END OF TG_PROGRAMA.

** Criação de tabela dinamica
DATA: T_FIELDCATALOG TYPE LVC_T_FCAT,
      W_FIELDCATALOG TYPE LVC_S_FCAT,
      WA_LAYOUT      TYPE LVC_S_LAYO,
      GT_F4          TYPE LVC_T_F4 WITH HEADER LINE,
      WA_STABLE      TYPE LVC_S_STBL,
      WG_EDITOR      TYPE TY_EDITOR,

      TG_FIELDS      TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      TG_EDITOR      TYPE TABLE OF TY_EDITOR,
      TG_MSG_RET     TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE.


*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
CONSTANTS: BEGIN OF C_TAB_STRIP_IMP,
             TAB1 LIKE SY-UCOMM VALUE 'TAB_STRIP_IMP_FC1',
             TAB2 LIKE SY-UCOMM VALUE 'TAB_STRIP_IMP_FC2',
             TAB3 LIKE SY-UCOMM VALUE 'TAB_STRIP_IMP_FC3',
           END OF C_TAB_STRIP_IMP.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
CONTROLS:  TAB_STRIP_IMP TYPE TABSTRIP.
DATA: BEGIN OF G_TAB_STRIP_IMP,
        SUBSCREEN   LIKE SY-DYNNR,
        PROG        LIKE SY-REPID VALUE 'ZMMR034',
        PRESSED_TAB LIKE SY-UCOMM VALUE C_TAB_STRIP_IMP-TAB1,
      END OF G_TAB_STRIP_IMP.

DATA: OK_CODE         LIKE SY-UCOMM,
      WG_MENSAGEM(30),
      E_DB_CLICK      TYPE  ZFIWRS0002,
      WG_ACAO(30),
      XMODIF(1).


*Class definition for ALV toolbar
CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.
*            lcl_alv_toolbar2  definition deferred.
*            LCL_ALV_TOOLBAR3  DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: G_CONTAINER_FATURA   TYPE SCRFNAME VALUE 'CC_FAT_IMP',
      G_CUSTOM_CONT_FAT    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

      G_CONTAINER_PRODUTO  TYPE SCRFNAME VALUE 'CC_PROD_IMP',
      G_CUSTOM_CONT_PROD   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

      G_CONTAINER_PROGRAMA TYPE SCRFNAME VALUE 'CC_PROG_IMP',
      G_CUSTOM_CONT_PROG   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

      OBG_CONTEINER_ERR    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      CONTAINER_2          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 2
      SPLITTER             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GRID1                TYPE REF TO CL_GUI_ALV_GRID,
      GRID2                TYPE REF TO CL_GUI_ALV_GRID,
      GRID3                TYPE REF TO CL_GUI_ALV_GRID,
      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      OBG_TOOLBAR2         TYPE REF TO LCL_ALV_TOOLBAR,
      OBG_TOOLBAR3         TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      G_DESCBOX            TYPE SCRFNAME VALUE 'CC_DESC',
      G_CC_ERR             TYPE SCRFNAME VALUE 'CC_ERR',
      OBG_DESCBOX          TYPE REF TO CL_GUI_TEXTEDIT,
      G_CUSTOM_CONT_DESC   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBG_DOCKING          TYPE REF TO CL_GUI_DOCKING_CONTAINER,

      WA_STYLE             TYPE LVC_S_STYL,
      STYLE                TYPE LVC_T_STYL  WITH HEADER LINE,
      STYLE2               TYPE LVC_T_STYL WITH HEADER LINE.

* alrs
*Declaration for toolbar buttons
DATA : TY_TOOLBAR TYPE STB_BUTTON.
*** TREE DE MENSAGENS.
DATA NODE_ITAB LIKE NODE_STR OCCURS 0.
DATA NODE LIKE NODE_STR.

DATA CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA SPLITTER_MSG TYPE REF TO CL_GUI_EASY_SPLITTER_CONTAINER.
DATA RIGHT TYPE REF TO CL_GUI_CONTAINER.
DATA LEFT  TYPE REF TO CL_GUI_CONTAINER.

DATA EDITOR TYPE REF TO CL_GUI_TEXTEDIT.
DATA TREE TYPE REF TO CL_GUI_SIMPLE_TREE.

DATA BEHAVIOUR_LEFT TYPE REF TO CL_DRAGDROP.
DATA BEHAVIOUR_RIGHT TYPE REF TO CL_DRAGDROP.

DATA HANDLE_TREE TYPE I.
DATA NUM_ROW TYPE I VALUE 0.
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
           C_REINICIA(8)     TYPE C VALUE 'REINICIA',
           C_DELDOC(6)       TYPE C VALUE 'DELDOC',
           C_DISPLA(6)       TYPE C VALUE 'DISPLA',
           C_DCLICK(6)       TYPE C VALUE 'DCLICK',
           C_SEARCH(6)       TYPE C VALUE 'SEARCH',
           C_ATUALI(6)       TYPE C VALUE 'ATUALI',
           C_ADD_MSG(7)      TYPE C VALUE 'ADD_MSG',
           C_DEL_MSG(7)      TYPE C VALUE 'DEL_MSG',
           C_CLOS_MSG(8)     TYPE C VALUE 'CLOS_MSG',
           C_SAVE_MSG(8)     TYPE C VALUE 'SAVE_MSG',
           C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE',
           C_GERA_REQ(8)     TYPE C VALUE 'GERA_REQ',
           C_GERA_PED(8)     TYPE C VALUE 'GERA_PED'.


*ALRS
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

    CLASS-METHODS:
      ON_DATA_CHANGED2 FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .
    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED2 FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      ON_DATA_CHANGED3 FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .
    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED3 FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      ON_F4                      FOR EVENT ONF4                 OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME
                  ES_ROW_NO
                  ER_EVENT_DATA
                  ET_BAD_CELLS
                  E_DISPLAY.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_treeobject DEFINITION
*---------------------------------------------------------------------*
*       Definition of Data Container                                  *
*---------------------------------------------------------------------*
CLASS LCL_DRAG_OBJECT DEFINITION.
  PUBLIC SECTION.
    DATA TEXT TYPE MTREESNODE-TEXT.
ENDCLASS.                    "lcl_drag_object DEFINITION
*---------------------------------------------------------------------*
*       CLASS dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_DRAGDROP_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      NODE_DOUBLE_CLICK FOR EVENT NODE_DOUBLE_CLICK OF CL_GUI_SIMPLE_TREE
        IMPORTING NODE_KEY.

ENDCLASS.                    "lcl_dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR
      IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,

      HANDLE_USER_COMMAND2 FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,

      HANDLE_USER_COMMAND3 FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
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
    CLEAR WL_DESACTIVE.
    IF ( WG_CADLAN-NRO_SOL_CP IS INITIAL  ) .
      WL_DESACTIVE = 1.
    ENDIF.

    IF WG_ACAO NE C_MODIF.
      WL_DESACTIVE = 1.
    ENDIF.
    TY_TOOLBAR-ICON      =  ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  =  C_ADD.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.


    TY_TOOLBAR-ICON      =  ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  =  C_DEL.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
*   variable for Toolbar Button
    TY_TOOLBAR-ICON      =  ICON_VIEW_CLOSE.
    TY_TOOLBAR-FUNCTION  =  C_CLOS_MSG.
    TY_TOOLBAR-DISABLED  = SPACE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar
  METHOD HANDLE_USER_COMMAND.
    DATA: TL_FATURA_AUX LIKE TABLE OF TG_FATURA,
          WL_FATURA     LIKE LINE OF TG_FATURA,
          WL_LINES      TYPE SY-TABIX.
    REFRESH:  TL_FATURA_AUX .

    IF WG_CADLAN-NRO_SOL_CP IS  NOT INITIAL.
      CASE E_UCOMM.
        WHEN C_ADD.
          TL_FATURA_AUX[] =  TG_FATURA[].
          REFRESH: TG_FATURA.
          LOOP AT TL_FATURA_AUX INTO WL_FATURA.
            APPEND WL_FATURA TO TG_FATURA.
          ENDLOOP.
          CLEAR: WL_FATURA.
          APPEND WL_FATURA TO TG_FATURA.

          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        WHEN C_DEL.
          CALL METHOD GRID1->GET_SELECTED_CELLS
            IMPORTING
              ET_CELL = TG_SELECTEDCELL.

          LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
            DELETE TG_FATURA INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
          ENDLOOP.

          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

  METHOD HANDLE_USER_COMMAND2.
    DATA: TL_PRODUTO_AUX LIKE TABLE OF TG_PRODUTO,
          WL_PRODUTO     LIKE LINE OF TG_PRODUTO,
          WL_LINES       TYPE SY-TABIX,
          VEBELP         TYPE ZMMT0037-EBELP.
    REFRESH:  TL_PRODUTO_AUX .

    IF WG_CADLAN-NRO_SOL_CP IS  NOT INITIAL.
      CASE E_UCOMM.
        WHEN C_ADD.
          TL_PRODUTO_AUX[] =  TG_PRODUTO[].
          REFRESH: TG_PRODUTO.
          CLEAR VEBELP.
          LOOP AT TL_PRODUTO_AUX INTO WL_PRODUTO.
            APPEND WL_PRODUTO TO TG_PRODUTO.
            VEBELP = WL_PRODUTO-EBELP.
          ENDLOOP.
          CLEAR: WL_PRODUTO.
          ADD 10 TO VEBELP.
          WL_PRODUTO-EBELP = VEBELP.
          APPEND WL_PRODUTO TO TG_PRODUTO.

          CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        WHEN C_DEL.
          CALL METHOD GRID2->GET_SELECTED_CELLS
            IMPORTING
              ET_CELL = TG_SELECTEDCELL.

          LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
            DELETE TG_PRODUTO INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
          ENDLOOP.

          CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

  METHOD HANDLE_USER_COMMAND3.
    DATA: TL_PROGRAMA_AUX LIKE TABLE OF TG_PROGRAMA,
          WL_PROGRAMA     LIKE LINE OF TG_PROGRAMA,
          WL_PRODUTO      LIKE LINE OF TG_PRODUTO,
          WL_LINES        TYPE SY-TABIX,
          VEBELP          TYPE ZMMT0038-EBELP.
    REFRESH:  TL_PROGRAMA_AUX .

    IF WG_CADLAN-NRO_SOL_CP IS  NOT INITIAL.
      CASE E_UCOMM.
        WHEN C_ADD.
          IF TG_PROGRAMA[] IS INITIAL.
            CLEAR WL_PROGRAMA.
            LOOP AT TG_PRODUTO INTO WL_PRODUTO.
              WL_PROGRAMA-EBELP   = WL_PRODUTO-EBELP.
              WL_PROGRAMA-MATNR   = WL_PRODUTO-MATNR.
              WL_PROGRAMA-MAKTX   = WL_PRODUTO-MAKTX.
              WL_PROGRAMA-MENGE   = WL_PRODUTO-MENGE.
              APPEND WL_PROGRAMA TO TG_PROGRAMA.
              CLEAR WL_PROGRAMA.
            ENDLOOP.
          ELSE.
            TL_PROGRAMA_AUX[] =  TG_PROGRAMA[].
            REFRESH: TG_PROGRAMA.
            CLEAR VEBELP.
            LOOP AT TL_PROGRAMA_AUX INTO WL_PROGRAMA.
              APPEND WL_PROGRAMA TO TG_PROGRAMA.
              VEBELP = WL_PROGRAMA-EBELP.
            ENDLOOP.
            CLEAR: WL_PROGRAMA.
            ADD 10 TO VEBELP.
            WL_PROGRAMA-EBELP = VEBELP.
            APPEND WL_PROGRAMA TO TG_PROGRAMA.
          ENDIF.
          CALL METHOD GRID3->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        WHEN C_DEL.
          CALL METHOD GRID3->GET_SELECTED_CELLS
            IMPORTING
              ET_CELL = TG_SELECTEDCELL.

          LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
            DELETE TG_PROGRAMA INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
          ENDLOOP.

          CALL METHOD GRID3->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
      ENDCASE.
    ENDIF.
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
* Método de  execução para Duplo-click
  METHOD ON_DOUBLE_CLICK.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD ON_F4.
    TYPES: BEGIN OF T_F4_STRUCTURE,
             FIELDTEXT TYPE DFIES-FIELDTEXT,
             FIELDNAME TYPE DFIES-FIELDNAME,
           END OF T_F4_STRUCTURE.

    FIELD-SYMBOLS: <ITAB> TYPE LVC_T_MODI.

    DATA: LS_MODI TYPE LVC_S_MODI.


    CASE E_FIELDNAME.

      WHEN 'LGORT'.
        TYPES : BEGIN OF TY_LGORT,
                  LGORT TYPE T001L-LGORT,
                  LGOBE TYPE T001L-LGOBE,
                END OF TY_LGORT.

        DATA: WL_RETURN_LG TYPE  DDSHRETVAL,
              WL_DSELCLG   TYPE  DSELC,
              TL_LGORT     TYPE TABLE OF TY_LGORT,
              TL_RETURN_LG TYPE TABLE OF DDSHRETVAL,
              TL_DSELCLG   TYPE TABLE OF DSELC.

        SELECT  LGORT LGOBE
           FROM T001L
           INTO TABLE TL_LGORT
            WHERE  WERKS = WG_CADLAN-WERKS
          ORDER BY LGORT ASCENDING.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            RETFIELD        = 'LGORT'
            VALUE_ORG       = 'S'
          " dynpprog        = sy-repid
          " dynpnr          = sy-dynnr
          " dynprofield     =
          TABLES
            VALUE_TAB       = TL_LGORT
            RETURN_TAB      = TL_RETURN_LG
            DYNPFLD_MAPPING = TL_DSELCLG.

        READ TABLE TL_RETURN_LG INTO WL_RETURN_LG INDEX 1.
        IF SY-SUBRC = 0 AND WL_RETURN_LG-FIELDVAL <> ''.
          ASSIGN ER_EVENT_DATA->M_DATA->* TO <ITAB>.
          LS_MODI-ROW_ID    = ES_ROW_NO-ROW_ID.
          LS_MODI-FIELDNAME = 'LGORT'.
          LS_MODI-VALUE     = WL_RETURN_LG-FIELDVAL.
          APPEND LS_MODI TO <ITAB>.
          ER_EVENT_DATA->M_EVENT_HANDLED = 'X'.
        ENDIF.
    ENDCASE.



  ENDMETHOD. "on_f4

  METHOD ON_DATA_CHANGED2.
    DATA: LS_GOOD    TYPE LVC_S_MODI,
          LV_VALUE   TYPE LVC_VALUE,
          LV_VALUE2  TYPE LVC_VALUE,
          VL_VALUE   TYPE LVC_VALUE,
          WL_MAKT    TYPE MAKT,
          WL_MARA    TYPE MARA,
          VMENGE     TYPE ZMMT0037-MENGE,
          VNETPR     TYPE ZMMT0037-NETPR,
          WL_PRODUTO LIKE LINE OF TG_PRODUTO.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'MATNR'.
      LV_VALUE = LS_GOOD-VALUE.
      LV_VALUE2 = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE *
        FROM MAKT
        INTO WL_MAKT
          WHERE SPRAS EQ 'P'
          AND MATNR EQ LV_VALUE.

      IF SY-SUBRC IS INITIAL.
        MOVE WL_MAKT-MAKTX TO LV_VALUE.

      ELSE.
        CLEAR LV_VALUE.
      ENDIF.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'MAKTX'
          I_VALUE     = LV_VALUE.

      SELECT SINGLE *
        FROM MARA
        INTO WL_MARA
          WHERE  MATNR EQ LV_VALUE2.

      IF SY-SUBRC IS INITIAL.
        MOVE WL_MARA-MEINS TO LV_VALUE.
      ELSE.
        CLEAR LV_VALUE.
      ENDIF.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'MEINS'
          I_VALUE     = LV_VALUE.
    ENDLOOP.


    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'NETPR'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      VNETPR = LV_VALUE.
      READ TABLE TG_PRODUTO INTO WL_PRODUTO INDEX LS_GOOD-ROW_ID.

      IF WL_PRODUTO-PEINH GT 0.
        VNETPR =  WL_PRODUTO-MENGE * ( ( VNETPR ) / WL_PRODUTO-PEINH ) .
      ELSE.
        VNETPR = 0.
      ENDIF.
      LV_VALUE = VNETPR.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'VALOR'
          I_VALUE     = LV_VALUE.

      VNETPR = VNETPR - WL_PRODUTO-NETPR_DESC.
      LV_VALUE = VNETPR.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'NETPR_FINAL'
          I_VALUE     = LV_VALUE.


    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                                INTO LS_GOOD
                                WHERE  FIELDNAME = 'NETPR_DESC'.

      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      VNETPR = LV_VALUE.
      READ TABLE TG_PRODUTO INTO WL_PRODUTO INDEX LS_GOOD-ROW_ID.

      LV_VALUE = WL_PRODUTO-VALOR - VNETPR.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'NETPR_FINAL'
          I_VALUE     = LV_VALUE.

    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                            INTO LS_GOOD
                            WHERE FIELDNAME = 'MENGE'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      VNETPR = LV_VALUE.
      READ TABLE TG_PRODUTO INTO WL_PRODUTO INDEX LS_GOOD-ROW_ID.
      IF WL_PRODUTO-PEINH GT 0.
        VNETPR =  VNETPR * ( ( WL_PRODUTO-NETPR ) / WL_PRODUTO-PEINH ) .
      ELSE.
        VNETPR = 0.
      ENDIF.
      LV_VALUE = VNETPR.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'VALOR'
          I_VALUE     = LV_VALUE.


      VNETPR = VNETPR - WL_PRODUTO-NETPR_DESC.
      LV_VALUE = VNETPR.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'NETPR_FINAL'
          I_VALUE     = LV_VALUE.

    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                            INTO LS_GOOD
                            WHERE   FIELDNAME = 'PEINH'.

      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      VNETPR = LV_VALUE.

      READ TABLE TG_PRODUTO INTO WL_PRODUTO INDEX LS_GOOD-ROW_ID.
      IF VNETPR GT 0.
        VNETPR =  WL_PRODUTO-MENGE * ( ( WL_PRODUTO-NETPR  ) / VNETPR ) .
      ELSE.
        VNETPR = 0.
      ENDIF.
      LV_VALUE = VNETPR.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'VALOR'
          I_VALUE     = LV_VALUE.

      LV_VALUE = VNETPR - WL_PRODUTO-NETPR_DESC .
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'NETPR_FINAL'
          I_VALUE     = LV_VALUE.

    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED2

  METHOD ON_DATA_CHANGED.
    DATA: LS_GOOD   TYPE LVC_S_MODI,
          LV_VALUE  TYPE LVC_VALUE,
          VL_VALUE  TYPE LVC_VALUE,
          WL_FATURA LIKE LINE OF TG_FATURA.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                                INTO LS_GOOD
                                WHERE FIELDNAME = 'PERCENTUAL'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      IF LV_VALUE > 0.
        READ TABLE TG_FATURA INTO WL_FATURA INDEX LS_GOOD-ROW_ID.
        IF WL_FATURA-VALOR NE 0 OR WL_FATURA-MENGE NE 0.
          LV_VALUE = 0.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'PERCENTUAL'
              I_VALUE     = LV_VALUE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                                INTO LS_GOOD
                                WHERE FIELDNAME = 'VALOR'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      IF LV_VALUE > 0.
        READ TABLE TG_FATURA INTO WL_FATURA INDEX LS_GOOD-ROW_ID.
        IF WL_FATURA-PERCENTUAL NE 0 OR WL_FATURA-MENGE NE 0.
          LV_VALUE = 0.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'VALOR'
              I_VALUE     = LV_VALUE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                                INTO LS_GOOD
                                WHERE FIELDNAME = 'MENGE'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      IF LV_VALUE > 0.
        READ TABLE TG_FATURA INTO WL_FATURA INDEX LS_GOOD-ROW_ID.
        IF WL_FATURA-PERCENTUAL NE 0 OR WL_FATURA-VALOR NE 0.
          LV_VALUE = 0.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'MENGE'
              I_VALUE     = LV_VALUE.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD ON_DATA_CHANGED_FINISHED.

*** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    PERFORM VERIFICA_ERROS.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.

  ENDMETHOD.                    "on_data_changed_finisheD

  METHOD ON_DATA_CHANGED_FINISHED2.
    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    PERFORM VERIFICA_ERROS.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED2

  METHOD ON_DATA_CHANGED3.
    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          VL_VALUE TYPE LVC_VALUE,
          WL_MAKT  TYPE MAKT.


    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'MATNR'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE *
        FROM MAKT
        INTO WL_MAKT
          WHERE SPRAS EQ 'P'
          AND MATNR EQ LV_VALUE.

      IF SY-SUBRC IS INITIAL.
        MOVE WL_MAKT-MAKTX TO LV_VALUE.

      ELSE.
        CLEAR LV_VALUE.
      ENDIF.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'MAKTX'
          I_VALUE     = LV_VALUE.
    ENDLOOP.


  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD ON_DATA_CHANGED_FINISHED3.
    CALL METHOD GRID3->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    PERFORM VERIFICA_ERROS.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.
  ENDMETHOD.                    "on_data_changed_finished3
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS DRAGDROP_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_DRAGDROP_RECEIVER IMPLEMENTATION.
  METHOD NODE_DOUBLE_CLICK.

  ENDMETHOD.                    "drop_complete
ENDCLASS.                    "lcl_dragdrop_receiver IMPLEMENTATION

*ALRS fim
*&---------------------------------------------------------------------*
*&      Module  TAB_STRIP_IMP_ACTIVE_TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TAB_STRIP_IMP_ACTIVE_TAB_SET OUTPUT.
  PERFORM VERIFICA_ERROS.
  TAB_STRIP_IMP-ACTIVETAB = G_TAB_STRIP_IMP-PRESSED_TAB.
  CASE G_TAB_STRIP_IMP-PRESSED_TAB.
    WHEN C_TAB_STRIP_IMP-TAB1.
      G_TAB_STRIP_IMP-SUBSCREEN = '0200'.
    WHEN C_TAB_STRIP_IMP-TAB2.
      G_TAB_STRIP_IMP-SUBSCREEN = '0300'.
    WHEN C_TAB_STRIP_IMP-TAB3.
      G_TAB_STRIP_IMP-SUBSCREEN = '0400'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                 " TAB_STRIP_IMP_ACTIVE_TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_ERROS .
  DATA: WL_LINHA(6) ,

        WL_FATURA       LIKE LINE OF TG_FATURA,
        WL_FATURA_AUX   LIKE LINE OF TG_FATURA,
        TG_FATURA_AUX   LIKE TABLE OF TG_FATURA,

        WL_PRODUTO      LIKE LINE OF TG_PRODUTO,
        WL_PROGRAMA     LIKE LINE OF TG_PROGRAMA,
        WL_PROGRAMA_AUX LIKE LINE OF TG_PROGRAMA,
        TG_PRODUTO_AUX  LIKE TABLE OF TG_PRODUTO,
        TG_PROGRAMA_AUX LIKE TABLE OF TG_PROGRAMA,

        TL_MAKT         TYPE TABLE OF MAKT    WITH HEADER LINE,
        TL_T007A        TYPE TABLE OF T007A   WITH HEADER LINE,
        TL_T006         TYPE TABLE OF T006   WITH HEADER LINE,
        TL_T001L        TYPE TABLE OF T001L   WITH HEADER LINE,

        WL_T024         TYPE T024,
        WL_ZSDT0044     TYPE ZSDT0044,
        WL_T001W        TYPE T001W,
        WL_LFA1         TYPE LFA1,
        WL_T052U        TYPE T052U,
        WL_TCURC        TYPE TCURC,

        V_MENGE1        TYPE ZMMT0037-MENGE,
        V_MENGE2        TYPE ZMMT0037-MENGE,
        V_PERC          TYPE ZMMT0036-PERCENTUAL,
        V_SAFRA1        TYPE I,
        V_SAFRA2        TYPE I,
        V_QTDE          TYPE I VALUE 0,
        V_QTDE_FAT      TYPE ZMMT0036-MENGE,
        V_QTDE_PROD     TYPE ZMMT0037-MENGE,
        V_VALOR_FAT     TYPE ZMMT0036-VALOR,
        V_VALOR_PROD    TYPE ZMMT0036-VALOR,
        V_LIFNR         TYPE LFA1-LIFNR.


  REFRESH: TG_MSG_RET.
  CLEAR: TG_MSG_RET.

  IF WG_CADLAN-IHRAN IS INITIAL.
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
        'WG_CADLAN-IHRAN'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Dt. Cotação' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.
  IF WG_CADLAN-EKGRP IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADLAN-EKGRP'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Grupo de compradores' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
       FROM T024
       INTO WL_T024
        WHERE  EKGRP EQ WG_CADLAN-EKGRP.
    IF SY-SUBRC NE 0.
      CONCATENATE TEXT-E04 'Grupo de compradores' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      MOVE 'WG_CADLAN-EKGRP'         TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WG_CADLAN-ZTERM IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADLAN-ZTERM'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Condição de Pagamento' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
       FROM T052U
       INTO WL_T052U
        WHERE  ZTERM EQ WG_CADLAN-ZTERM.
    IF SY-SUBRC NE 0.
      CONCATENATE TEXT-E04 'Condição de Pagamento' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      MOVE 'WG_CADLAN-ZTERM'         TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WG_CADLAN-SAFRA IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADLAN-SAFRA'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'SAFRA' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
       FROM ZSDT0044
       INTO WL_ZSDT0044
        WHERE  SAFRA EQ WG_CADLAN-SAFRA+0(4).
    IF SY-SUBRC NE 0.
      CONCATENATE TEXT-E04 ' SAFRA' INTO  TG_MSG_RET-MSG  SEPARATED BY SPACE..
      MOVE 'WG_CADLAN-SAFRA'         TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      V_SAFRA1 = WG_CADLAN-SAFRA+0(4).
      V_SAFRA2 = WG_CADLAN-SAFRA+5(4).
      V_SAFRA1 = V_SAFRA2 - V_SAFRA1.
      IF V_SAFRA1 NE 1 OR WG_CADLAN-SAFRA+4(1) NE '/'.
        CONCATENATE TEXT-E08 '' INTO  TG_MSG_RET-MSG  SEPARATED BY SPACE..
        MOVE 'WG_CADLAN-SAFRA'         TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

    ENDIF.
  ENDIF.

  IF WG_CADLAN-WERKS IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADLAN-FILIAL'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Filial' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
       FROM T001W
       INTO WL_T001W
        WHERE  WERKS EQ WG_CADLAN-WERKS.
    IF SY-SUBRC NE 0.
      CONCATENATE TEXT-E04 ' Filial' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE..
      MOVE 'WG_CADLAN-FILIAL'         TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
    SELECT *
      FROM T001L
      INTO TABLE TL_T001L
      WHERE WERKS = WG_CADLAN-WERKS ORDER BY PRIMARY KEY .
  ENDIF.

  IF WG_CADLAN-LIFNR IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADLAN-LIFNR'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Fornecedor' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_CADLAN-LIFNR
      IMPORTING
        OUTPUT = V_LIFNR.
    SELECT SINGLE *
       FROM LFA1
       INTO WL_LFA1
        WHERE  LIFNR EQ V_LIFNR.
    IF SY-SUBRC NE 0.
      CONCATENATE TEXT-E04 'Fornecedor' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE..
      MOVE 'WG_CADLAN-LIFNR'           TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WG_CADLAN-WAERS IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADLAN-WAERS'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Moeda' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
       FROM TCURC
       INTO WL_TCURC
        WHERE  WAERS EQ WG_CADLAN-WAERS.
    IF SY-SUBRC NE 0.
      CONCATENATE TEXT-E04 ' Moeda' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      MOVE 'WG_CADLAN-WAERS'         TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  TG_FATURA_AUX[] = TG_FATURA[].
  SORT: TG_FATURA_AUX BY DT_VCTO  .
  V_PERC = 0.
  V_QTDE_FAT = 0.
  V_VALOR_FAT = 0.
  LOOP AT TG_FATURA INTO WL_FATURA.
    WL_LINHA = SY-TABIX.
    V_QTDE = 0.
    ADD WL_FATURA-PERCENTUAL TO V_PERC.
    ADD WL_FATURA-MENGE TO V_QTDE_FAT.
    ADD WL_FATURA-VALOR TO V_VALOR_FAT.
    LOOP AT TG_FATURA_AUX INTO WL_FATURA_AUX WHERE DT_VCTO = WL_FATURA-DT_VCTO .
      ADD 1 TO V_QTDE.
    ENDLOOP.

    IF V_QTDE > 1.
      MOVE:  C_TAB_STRIP_IMP-TAB1 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E03 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDLOOP.

  IF V_PERC GT 100.
    MOVE:  C_TAB_STRIP_IMP-TAB1 TO TG_MSG_RET-ABA.
    CONCATENATE TEXT-E07 '' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  LOOP AT TG_FATURA INTO WL_FATURA.
    WL_LINHA = SY-TABIX.
    IF WL_FATURA-PERCENTUAL IS INITIAL  AND
       WL_FATURA-VALOR     IS INITIAL AND
       WL_FATURA-MENGE IS INITIAL.
      MOVE:  C_TAB_STRIP_IMP-TAB1 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E11 'LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.


*    IF wl_fatura-percentual IS INITIAL.
*      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
*      CONCATENATE text-e01 'Percentual LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.
*
*    IF wl_fatura-valor IS INITIAL.
*      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
*      CONCATENATE text-e01 'Valor LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.
*
*    IF wl_fatura-menge IS INITIAL.
*      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
*      CONCATENATE text-e01 'Quantidade LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.

  ENDLOOP.


  IF TG_PRODUTO[] IS NOT INITIAL.
    SELECT *
      FROM MAKT
      INTO TABLE TL_MAKT
      FOR ALL ENTRIES IN TG_PRODUTO
      WHERE MATNR = TG_PRODUTO-MATNR.

    SELECT *
      FROM T007A
      INTO TABLE TL_T007A
      FOR ALL ENTRIES IN TG_PRODUTO
      WHERE MWSKZ  = TG_PRODUTO-MWSKZ.

    SELECT *
      FROM T006
      INTO TABLE TL_T006
      FOR ALL ENTRIES IN TG_PRODUTO
      WHERE MSEHI = TG_PRODUTO-MEINS.

  ENDIF.

  SORT: TL_MAKT   BY MATNR,
        TL_T007A  BY MWSKZ,
        TL_T006   BY MSEHI.

  V_QTDE_PROD = 0.
  V_VALOR_PROD = 0.
  LOOP AT TG_PRODUTO INTO  WL_PRODUTO .
    WL_LINHA = SY-TABIX.
    ADD WL_PRODUTO-MENGE TO V_QTDE_PROD .
    ADD WL_PRODUTO-VALOR TO V_VALOR_PROD .
    IF WL_PRODUTO-MATNR  IS INITIAL.
      MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Material LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      READ TABLE TL_MAKT
        WITH KEY MATNR = WL_PRODUTO-MATNR
                 BINARY SEARCH.
      IF SY-SUBRC NE 0.
        MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E04 'Material' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF WL_PRODUTO-NETPR_FINAL LT 0.
      MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E12 'LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WL_PRODUTO-LGORT  IS INITIAL.
      MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Depósito LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      READ TABLE TL_T001L
        WITH KEY WERKS = WG_CADLAN-WERKS
                 LGORT = WL_PRODUTO-LGORT
                 BINARY SEARCH.
      IF SY-SUBRC NE 0.
        MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E04 'Depósito' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF WL_PRODUTO-MWSKZ  IS INITIAL.
      MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'IVA LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      READ TABLE TL_T007A
        WITH KEY MWSKZ = WL_PRODUTO-MWSKZ
                 BINARY SEARCH.
      IF SY-SUBRC NE 0.
        MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E04 'IVA' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF WL_PRODUTO-MEINS  IS INITIAL.
      MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Unid. LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      READ TABLE TL_T006
        WITH KEY MSEHI = WL_PRODUTO-MEINS
                 BINARY SEARCH.
      IF SY-SUBRC NE 0.
        MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E04 'Unid.' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF WL_PRODUTO-MENGE IS INITIAL.
      MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Quantidade LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WL_PRODUTO-PEINH IS INITIAL.
      MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Por LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

  ENDLOOP.

  IF V_QTDE_FAT GT 0 AND V_QTDE_PROD GT V_QTDE_FAT.
    MOVE:  C_TAB_STRIP_IMP-TAB1 TO TG_MSG_RET-ABA.
    CONCATENATE TEXT-E09 '' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

*  Modificação CS2016001138
*  IF V_VALOR_FAT GT 0 AND V_VALOR_PROD GT V_VALOR_FAT.
*    MOVE:  C_TAB_STRIP_IMP-TAB1 TO TG_MSG_RET-ABA.
*    CONCATENATE TEXT-E10 '' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

  REFRESH  TL_MAKT.
  IF TG_PROGRAMA[] IS NOT INITIAL.
    SELECT *
      FROM MAKT
      INTO TABLE TL_MAKT
      FOR ALL ENTRIES IN TG_PROGRAMA
      WHERE MATNR = TG_PROGRAMA-MATNR.
  ENDIF.

  TG_PRODUTO_AUX[] = TG_PRODUTO[].
  TG_PROGRAMA_AUX[] = TG_PROGRAMA[].
  SORT: TG_PRODUTO_AUX  BY MATNR,
        TG_PROGRAMA_AUX BY MATNR,
        TL_MAKT         BY MATNR.

  LOOP AT TG_PROGRAMA INTO  WL_PROGRAMA .
    WL_LINHA = SY-TABIX.
    IF WL_PROGRAMA-MATNR  IS INITIAL.
      MOVE:  C_TAB_STRIP_IMP-TAB3 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Material LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      READ TABLE TL_MAKT
        WITH KEY MATNR = WL_PROGRAMA-MATNR
                 BINARY SEARCH.
      IF SY-SUBRC NE 0.
        MOVE:  C_TAB_STRIP_IMP-TAB3 TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E04 'Material' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
      READ TABLE TG_PRODUTO_AUX INTO WL_PRODUTO
      WITH KEY MATNR = WL_PROGRAMA-MATNR
                 BINARY SEARCH.
      IF SY-SUBRC NE 0.
        MOVE:  C_TAB_STRIP_IMP-TAB3 TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E05 WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF WL_PROGRAMA-DATA_PROGR IS INITIAL.
      MOVE:  C_TAB_STRIP_IMP-TAB3 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Data Programação LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WL_PROGRAMA-MENGE IS INITIAL.
      MOVE:  C_TAB_STRIP_IMP-TAB3 TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-E01 'Quantidade LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      CLEAR: V_MENGE1,V_MENGE2.
      LOOP AT  TG_PRODUTO_AUX INTO WL_PRODUTO WHERE MATNR = WL_PROGRAMA-MATNR.
        ADD WL_PRODUTO-MENGE TO V_MENGE1.
      ENDLOOP.

      LOOP AT  TG_PROGRAMA_AUX INTO WL_PROGRAMA_AUX WHERE MATNR = WL_PROGRAMA-MATNR.
        ADD WL_PROGRAMA_AUX-MENGE TO V_MENGE2.
      ENDLOOP.
      IF V_MENGE2 GT V_MENGE1 .
        MOVE:  C_TAB_STRIP_IMP-TAB3 TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E06  WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

    ENDIF.

  ENDLOOP.



ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.
  LOOP AT TG_FIELDS.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ TG_FIELDS-CAMPO
      OR SCREEN-GROUP1 EQ TG_FIELDS-GROUP1.
        SCREEN-INPUT     = TG_FIELDS-VALUE.
        SCREEN-INVISIBLE = TG_FIELDS-INVISIBLE.
        MODIFY SCREEN.
*        EXIT.
      ENDIF.
      IF    SCREEN-NAME = 'WG_CADLAN-EBELN'.
        IF WG_CADLAN-EBELN IS NOT INITIAL.
          SCREEN-INPUT     = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD."
  ENDIF.
  CLEAR: TG_FIELDS.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.

  REFRESH: FCODE.


  IF WG_ACAO IS INITIAL OR WG_ACAO = C_DISPLA.
    APPEND C_SAVE TO FCODE.
    APPEND C_DELDOC TO FCODE.
    IF XMODIF = 'X' OR WG_ACAO IS INITIAL OR WG_CADLAN-NRO_SOL_CP IS INITIAL.
      APPEND C_MODIF TO FCODE.
    ENDIF.
  ELSEIF XMODIF = 'X' .
    APPEND C_MODIF TO FCODE.
  ENDIF.
  SET PF-STATUS 'Z001' EXCLUDING FCODE.
  CALL METHOD CL_GUI_CFW=>DISPATCH.
  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  DATA: EVENT       TYPE CNTL_SIMPLE_EVENT,
        EVENTS      TYPE CNTL_SIMPLE_EVENTS,
        TL_FILTER   TYPE LVC_T_FILT,
        WL_FILTER   TYPE LVC_S_FILT,
        TL_FUNCTION TYPE UI_FUNCTIONS,
        WL_FUNCTION LIKE TL_FUNCTION WITH HEADER LINE.

  IF G_CUSTOM_CONT_FAT IS INITIAL.
*    WA_LAYOUT-CWIDTH_OPT = C_X.
    WA_LAYOUT-ZEBRA      = C_X.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
    WA_LAYOUT-NO_ROWMARK = C_X.
    WA_LAYOUT-COL_OPT    = C_X.
    WA_STABLE-ROW        = C_X.
    WA_LAYOUT-SEL_MODE   = 'A'.
    WA_LAYOUT-CWIDTH_OPT   = 'X'.
    WA_LAYOUT-BOX_FNAME    = 'MARK'.
    WA_LAYOUT-NO_TOOLBAR = SPACE.

    CREATE OBJECT G_CUSTOM_CONT_FAT
      EXPORTING
        CONTAINER_NAME = G_CONTAINER_FATURA.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = G_CUSTOM_CONT_FAT
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CONTAINER_1.

    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = CONTAINER_1.


    PERFORM MONTAR_LAYOUT.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID1.

*      * Register event handler
    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_FATURA[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER:
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR GRID1.

*    posiciona spliter na altura x
    CALL METHOD SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 100.
  ELSE.
    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = T_FIELDCATALOG[].

    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
      W_FIELDCATALOG-OUTPUTLEN = '15'.
      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
    ENDLOOP.

    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG[].

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.


  "GRID2
  IF G_CUSTOM_CONT_PROD IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONT_PROD
      EXPORTING
        CONTAINER_NAME = G_CONTAINER_PRODUTO.

    CREATE OBJECT GRID2
      EXPORTING
        I_PARENT = G_CUSTOM_CONT_PROD.

    PERFORM MONTAR_LAYOUT_PROD.

    CREATE OBJECT OBG_TOOLBAR2
      EXPORTING
        IO_ALV_GRID = GRID2.

*      * Register event handler
    SET HANDLER OBG_TOOLBAR2->ON_TOOLBAR FOR GRID2.
    SET HANDLER OBG_TOOLBAR2->HANDLE_USER_COMMAND2 FOR GRID2.

    REFRESH: TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.


    CALL METHOD GRID2->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = WA_LAYOUT
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
      CHANGING
        IT_FILTER            = TL_FILTER
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_PRODUTO[].

    CALL METHOD GRID2->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID2->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    PERFORM BUILD_F4_CAT.

    SET HANDLER:
*              lcl_event_handler=>on_double_click FOR grid1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED2 FOR GRID2,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED2 FOR GRID2,
              LCL_EVENT_HANDLER=>ON_F4 FOR GRID2.


  ELSE.
    CALL METHOD GRID2->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = T_FIELDCATALOG[].

    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
      W_FIELDCATALOG-OUTPUTLEN = '12'.
      IF W_FIELDCATALOG-FIELDNAME EQ 'EBELP'.
        W_FIELDCATALOG-OUTPUTLEN = '08'.
      ELSEIF W_FIELDCATALOG-FIELDNAME EQ 'MAKTX'.
        W_FIELDCATALOG-OUTPUTLEN = '30'.
      ELSEIF W_FIELDCATALOG-FIELDNAME EQ 'MEINS'.
        W_FIELDCATALOG-OUTPUTLEN = '10'.
      ELSEIF W_FIELDCATALOG-FIELDNAME EQ 'PEINH'.
        W_FIELDCATALOG-OUTPUTLEN = '10'.
      ELSEIF W_FIELDCATALOG-FIELDNAME EQ 'MWSKZ'.
        W_FIELDCATALOG-OUTPUTLEN = '10'.
      ENDIF.
      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
    ENDLOOP.

    CALL METHOD GRID2->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG[].

    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

  "GRID3
  IF G_CUSTOM_CONT_PROG IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONT_PROG
      EXPORTING
        CONTAINER_NAME = G_CONTAINER_PROGRAMA.

    CREATE OBJECT GRID3
      EXPORTING
        I_PARENT = G_CUSTOM_CONT_PROG.

    PERFORM MONTAR_LAYOUT_PROG.

    CREATE OBJECT OBG_TOOLBAR3
      EXPORTING
        IO_ALV_GRID = GRID3.

*      * Register event handler
    SET HANDLER OBG_TOOLBAR3->ON_TOOLBAR FOR GRID3.
    SET HANDLER OBG_TOOLBAR3->HANDLE_USER_COMMAND3 FOR GRID3.


    REFRESH: TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    CALL METHOD GRID3->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = WA_LAYOUT
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
      CHANGING
        IT_FILTER            = TL_FILTER
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_PROGRAMA[].

    CALL METHOD GRID3->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID3->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER:
*              lcl_event_handler=>on_double_click FOR grid1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED3 FOR GRID3,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED3 FOR GRID3.

  ELSE.

    CALL METHOD GRID3->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = T_FIELDCATALOG[].

    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
      W_FIELDCATALOG-OUTPUTLEN = '12'.
      IF W_FIELDCATALOG-FIELDNAME EQ 'EBELP'.
        W_FIELDCATALOG-OUTPUTLEN = '08'.
      ELSEIF W_FIELDCATALOG-FIELDNAME EQ 'MAKTX'.
        W_FIELDCATALOG-OUTPUTLEN = '30'.
      ELSEIF W_FIELDCATALOG-FIELDNAME EQ 'OBSERV'.
        W_FIELDCATALOG-OUTPUTLEN = '50'.
      ENDIF.
      MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
    ENDLOOP.

    CALL METHOD GRID3->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG[].

    CALL METHOD GRID3->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

  IF G_CUSTOM_CONT_DESC IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONT_DESC
      EXPORTING
        CONTAINER_NAME = G_DESCBOX.

    IF G_CUSTOM_CONT_DESC IS NOT INITIAL.
      CREATE OBJECT OBG_DESCBOX
        EXPORTING
          PARENT            = G_CUSTOM_CONT_DESC
          WORDWRAP_MODE     = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
          WORDWRAP_POSITION = 72
          MAX_NUMBER_CHARS  = 350.

      CALL METHOD OBG_DESCBOX->SET_TOOLBAR_MODE
        EXPORTING
          TOOLBAR_MODE = '0'.

      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 1.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT .
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        1 'ZMMT0036'         'DT_VCTO'      'TG_FATURA' 'DT_VCTO'       'Data Vencto'    '20' 'X' ' ' ' ',
        2 'ZMMT0036'         'PERCENTUAL'   'TG_FATURA' 'PERCENTUAL'    'Percentual'     '20' 'X' 'X' ' ',
        3 'ZMMT0036'         'VALOR'        'TG_FATURA' 'VALOR'         'Valor'          '20' 'X' 'X' ' ',
        4 'ZMMT0036'         'MENGE'        'TG_FATURA' 'MENGE'         'Quantidade'     '20' 'X' 'X' ' '.
ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT)
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE).

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY           = ' '.
  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS         = P_COL_POS.
  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  ENDIF.
  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  IF P_FIELD EQ 'CHECKBOX'.
    W_FIELDCATALOG-CHECKBOX = C_X.
  ENDIF.

  IF P_FIELD EQ 'LGORT'.
    W_FIELDCATALOG-F4AVAILABL = C_X.
  ENDIF.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_PROD .
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        1 'ZMMT0037'         'EBELP'      'TG_PRODUTO' 'EBELP'       'Item'        '10' ' ' ' ' ' ',
        1 'MAKT'             'MATNR'      'TG_PRODUTO' 'MATNR'       'Material'    '15' 'X' ' ' ' ',
        1 'MAKT'             'MAKTX'      'TG_PRODUTO' 'MAKTX'       'Texto Breve' '30' ' ' ' ' ' ',
        1 ' '                ' '          'TG_PRODUTO' 'LGORT'       'Depósito'    '10' 'X' ' ' ' ',
        1 'ZMMT0037'         'MENGE'      'TG_PRODUTO' 'MENGE'       'Qtde.Pedido' '15' 'X' 'X' ' ',
        1 'ZMMT0037'         'MEINS'      'TG_PRODUTO' 'MEINS'       'Unid.'       '10' ' ' ' ' ' ',
        1 'ZMMT0037'         'NETPR'      'TG_PRODUTO' 'NETPR'       'Preço Liq.'  '15' 'X' ' ' ' ',
        1 'ZMMT0037'         'PEINH'      'TG_PRODUTO' 'PEINH'       'Por'         '10' 'X' ' ' ' ',
        1 'ZMMT0037'         'NETPR'      'TG_PRODUTO' 'VALOR'       'Valor'       '15' ' ' 'X' ' ',
        1 'ZMMT0037'         'NETPR_DESC' 'TG_PRODUTO' 'NETPR_DESC'  'Desconto'    '15' 'X' ' ' ' ',
        1 'ZMMT0037'         'NETPR'      'TG_PRODUTO' 'NETPR_FINAL' 'Valor Final' '15' ' ' 'X' ' ',
        1 'ZMMT0037'         'MWSKZ'      'TG_PRODUTO' 'MWSKZ'       'IVA'         '10' 'X' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_PROD
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_PROG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_PROG .
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        1 'ZMMT0038'         'EBELP'      'TG_PROGRAMA' 'EBELP'       'Item'         '10' ' ' ' ' ' ',
        1 'MAKT'             'MATNR'      'TG_PROGRAMA' 'MATNR'       'Material'     '15' 'X' ' ' ' ',
        1 'MAKT'             'MAKTX'      'TG_PROGRAMA' 'MAKTX'       'Texto Breve'  '30' ' ' ' ' ' ',
        1 'ZMMT0038'         'DATA_PROGR' 'TG_PROGRAMA' 'DATA_PROGR'  'Data Entrega' '15' 'X' ' ' ' ',
        1 'ZMMT0038'         'MENGE'      'TG_PROGRAMA' 'MENGE'       'Quantidade'   '15' 'X' 'X' ' ',
        1 'ZMMT0038'         'OBSERV'     'TG_PROGRAMA' 'OBSERV'      'Observação'   '50' 'X' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT_PROG
*&---------------------------------------------------------------------*
*&      Module  TAB_STRIP_IMP_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TAB_STRIP_IMP_ACTIVE_TAB_GET INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN C_TAB_STRIP_IMP-TAB1.
      G_TAB_STRIP_IMP-PRESSED_TAB = C_TAB_STRIP_IMP-TAB1.
    WHEN C_TAB_STRIP_IMP-TAB2.
      G_TAB_STRIP_IMP-PRESSED_TAB = C_TAB_STRIP_IMP-TAB2.
    WHEN C_TAB_STRIP_IMP-TAB3.
      G_TAB_STRIP_IMP-PRESSED_TAB = C_TAB_STRIP_IMP-TAB3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                 " TAB_STRIP_IMP_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT INPUT.
  CASE OK-CODE.
    WHEN C_BACK.
      SET SCREEN 0.

    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA W_ANSWER.
  DATA V_NRO_SOL TYPE ZMMT0035-NRO_SOL_CP.

  CASE OK-CODE.
    WHEN C_DELDOC.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR              = ' '
*         DIAGNOSE_OBJECT       = ' '
          TEXT_QUESTION         = 'Confirma a exclusão da Solicitação?'
          TEXT_BUTTON_1         = 'Sim'(001)
          ICON_BUTTON_1         = 'ICON_OKAY '
          TEXT_BUTTON_2         = 'Não'(002)
          ICON_BUTTON_2         = 'ICON_CANCEL'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ' '
*         USERDEFINED_F1_HELP   = ' '
          START_COLUMN          = 25
          START_ROW             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        IMPORTING
          ANSWER                = W_ANSWER
*       TABLES
*         PARAMETER             =
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.
      .
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF W_ANSWER = '1'.
        PERFORM ELIMINAR_SOLICITACAO.
      ENDIF.
    WHEN C_SEARCH.
      PERFORM BUSCA_DADOS.
    WHEN C_GERA_REQ.

      DELETE TG_FATURA WHERE  DT_VCTO IS INITIAL.
      DELETE TG_PRODUTO WHERE  MATNR IS INITIAL.
      DELETE TG_PROGRAMA WHERE  MATNR IS INITIAL.

      CALL METHOD GRID1->CHECK_CHANGED_DATA.
      PERFORM VERIFICA_ERROS.

      IF TG_MSG_RET[] IS NOT INITIAL.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ELSE.
        PERFORM CRIA_REQ.
      ENDIF.

    WHEN C_GERA_PED.

      DELETE TG_FATURA WHERE  DT_VCTO IS INITIAL.
      DELETE TG_PRODUTO WHERE  MATNR IS INITIAL.
      DELETE TG_PROGRAMA WHERE  MATNR IS INITIAL.

      CALL METHOD GRID1->CHECK_CHANGED_DATA.
      PERFORM VERIFICA_ERROS.

      IF TG_MSG_RET[] IS NOT INITIAL.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ELSE.
        "IF WG_CADLAN-BANFN IS INITIAL.
        "  MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E13.
        IF WG_CADLAN-BSART IS INITIAL.
          MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E15.
        ELSE.
          PERFORM CRIA_PED.
        ENDIF.
      ENDIF.

    WHEN C_SAVE.
      DELETE TG_FATURA WHERE  DT_VCTO IS INITIAL.
      DELETE TG_PRODUTO WHERE  MATNR IS INITIAL.
      DELETE TG_PROGRAMA WHERE  MATNR IS INITIAL.

      CALL METHOD GRID1->CHECK_CHANGED_DATA.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.
        PERFORM GRAVA_DADOS.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                 'GR2'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 1.

      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_BACK.
      CLEAR WG_ACAO.
    WHEN C_ADD.
      WG_ACAO = C_MODIF.
      PERFORM LIMPA_CAMPOS.
      PERFORM OBTEM_PROXIMO.
      REFRESH: TG_FIELDS.
      PERFORM TRATA_CAMPOS USING SPACE
                                 'GR2'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM TRATA_CAMPOS USING SPACE
                                'GR1'
                                 C_0       "INPUT 1     NO INPUT 0
                                 C_0.      "INVISIBLE 1 VISIBLE 0

      CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TG_EDITOR.
      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 0.

    WHEN C_DISPLA.
      WG_ACAO = C_DISPLA.
      V_NRO_SOL = WG_CADLAN-NRO_SOL_CP.
      PERFORM LIMPA_CAMPOS.
      WG_CADLAN-NRO_SOL_CP = V_NRO_SOL.
      PERFORM BUSCA_DADOS.
      REFRESH: TG_FIELDS.
      PERFORM TRATA_CAMPOS USING SPACE
                           'GR2'
                              C_0       "INPUT 1     NO INPUT 0
                              C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM TRATA_CAMPOS USING SPACE
                                'GR1'
                                 C_1       "INPUT 1     NO INPUT 0
                                 C_0.      "INVISIBLE 1 VISIBLE 0
    WHEN C_CANCEL.
      CLEAR WG_ACAO.
    WHEN C_ATUALI.

    WHEN C_MODIF.
      IF WG_ACAO = C_MODIF.
        CLEAR WG_ACAO.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                 'GR2'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 1.
      ELSE.
        WG_ACAO = C_MODIF.
        PERFORM TRATA_CAMPOS USING SPACE
                                   'GR2'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_0       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0
        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 0.

      ENDIF.
    WHEN C_SHOW_MSGRE.
      "CLEAR wg_acao.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_EXIT.

      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
FORM TRATA_CAMPOS  USING    P_FIELD
                            P_GROUP1
                            P_VALUE
                            P_INVISIBLE.

  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.

ENDFORM.                    " TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS .
  DATA: WL_ZMMT0035  TYPE ZMMT0035,
        TL_ZMMT0036  TYPE TABLE OF ZMMT0036 WITH HEADER LINE,
        TL_ZMMT0037  TYPE TABLE OF ZMMT0037 WITH HEADER LINE,
        TL_ZMMT0038  TYPE TABLE OF ZMMT0038 WITH HEADER LINE,
        WL_MAKT      TYPE MAKT,
        WL_LFA1      TYPE LFA1,
        WL_T001W     TYPE T001W,
        WL_T052U     TYPE T052U,
        WL_T024      TYPE T024,
        WL_T161T     TYPE T161T,
        WL_CONT      TYPE SY-TABIX,
        WL_CONT_AUX  TYPE SY-TABIX,
        WL_CONT_AUX2 TYPE SY-TABIX,
        V_LIFNR      TYPE LFA1-LIFNR.


  IF WG_CADLAN-NRO_SOL_CP IS NOT INITIAL AND WG_ACAO IS NOT INITIAL .
    CLEAR: WL_LFA1, WL_T001W, WL_T052U,WL_T024.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_CADLAN-LIFNR
      IMPORTING
        OUTPUT = V_LIFNR.
    SELECT SINGLE *
      FROM LFA1
      INTO WL_LFA1
      WHERE LIFNR = V_LIFNR.

    IF SY-SUBRC = 0.
      WG_CADLAN-NAME1_L  = WL_LFA1-NAME1.
    ENDIF.

    SELECT SINGLE *
      FROM T001W
      INTO WL_T001W
      WHERE WERKS = WG_CADLAN-WERKS.
    IF SY-SUBRC = 0.
      WG_CADLAN-NAME1_W  = WL_T001W-NAME1.
    ENDIF.

    SELECT SINGLE *
      FROM T052U
      INTO WL_T052U
      WHERE SPRAS = 'P'
      AND ZTERM = WG_CADLAN-ZTERM.
    IF SY-SUBRC = 0.
      WG_CADLAN-TEXT1    = WL_T052U-TEXT1.
    ENDIF.

    SELECT SINGLE *
      FROM T024
      INTO WL_T024
      WHERE EKGRP = WG_CADLAN-EKGRP.
    IF SY-SUBRC = 0.
      WG_CADLAN-EKNAM   = WL_T024-EKNAM.
    ENDIF.

    SELECT SINGLE *
      FROM T161T
      INTO WL_T161T
      WHERE SPRAS = 'P'
      AND   BSART = WG_CADLAN-BSART
      AND   BSTYP = 'F'.

    IF SY-SUBRC = 0.
      WG_CADLAN-BATXT   = WL_T161T-BATXT.
    ENDIF.
  ENDIF.

  IF WG_CADLAN-NRO_SOL_CP IS NOT INITIAL  AND WG_ACAO EQ C_DISPLA.
    SELECT SINGLE *
     FROM ZMMT0035
     INTO  WL_ZMMT0035
      WHERE  NRO_SOL_CP EQ WG_CADLAN-NRO_SOL_CP.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Nº de Solicitação não encontrada!'.
      LEAVE TO SCREEN 100.
    ELSEIF WL_ZMMT0035-LOEKZ IS NOT INITIAL.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Nº de Solicitação foi eliminada!'.
      LEAVE TO SCREEN 100.
    ELSE.
      MOVE-CORRESPONDING WL_ZMMT0035 TO WG_CADLAN.

      REFRESH: TG_EDITOR.
      CLEAR: WL_CONT_AUX2, WL_CONT_AUX, WL_CONT.
      WL_CONT = STRLEN( WL_ZMMT0035-TEXTO_NEG ).
      WL_CONT_AUX = WL_CONT / 72.
      DO.
        MOVE: WL_ZMMT0035-TEXTO_NEG+WL_CONT_AUX2 TO WG_EDITOR-LINE.
        ADD 72 TO WL_CONT_AUX2.
        APPEND WG_EDITOR TO TG_EDITOR.

        IF WL_CONT_AUX2 GT WL_CONT.
          EXIT.

        ENDIF.
      ENDDO.
      CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TG_EDITOR.
      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 1.

      " Condições pagamento
      SELECT *
         FROM ZMMT0036
         INTO TABLE TL_ZMMT0036
         WHERE NRO_SOL_CP = WG_CADLAN-NRO_SOL_CP.

      REFRESH: TG_FATURA.
      CLEAR TG_FATURA.
      LOOP AT TL_ZMMT0036.
        MOVE-CORRESPONDING TL_ZMMT0036 TO TG_FATURA.
        APPEND TG_FATURA.
      ENDLOOP.

      " produtos
      SELECT *
      FROM ZMMT0037
      INTO TABLE TL_ZMMT0037
      WHERE NRO_SOL_CP = WG_CADLAN-NRO_SOL_CP.

      REFRESH: TG_PRODUTO.
      CLEAR TG_PRODUTO.
      LOOP AT TL_ZMMT0037.
        MOVE-CORRESPONDING TL_ZMMT0037 TO TG_PRODUTO.
        SELECT SINGLE *
          FROM MAKT
          INTO WL_MAKT
          WHERE MATNR = TG_PRODUTO-MATNR
          AND SPRAS   = 'P'.

        TG_PRODUTO-MAKTX = WL_MAKT-MAKTX.
        IF TG_PRODUTO-PEINH GT 0.
          TG_PRODUTO-VALOR = TG_PRODUTO-MENGE * ( ( TG_PRODUTO-NETPR ) / TG_PRODUTO-PEINH ) .
        ELSE.
          TG_PRODUTO-VALOR = 0.
        ENDIF.
        TG_PRODUTO-NETPR_FINAL  =  TG_PRODUTO-VALOR - TG_PRODUTO-NETPR_DESC.
        APPEND TG_PRODUTO.
      ENDLOOP.

      " programação entrega
      SELECT *
      FROM ZMMT0038
      INTO TABLE TL_ZMMT0038
      WHERE NRO_SOL_CP = WG_CADLAN-NRO_SOL_CP.

      REFRESH: TG_PROGRAMA.
      CLEAR TG_PROGRAMA.
      LOOP AT TL_ZMMT0038.
        MOVE-CORRESPONDING TL_ZMMT0038 TO TG_PROGRAMA.
        SELECT SINGLE *
          FROM MAKT
          INTO WL_MAKT
          WHERE MATNR = TG_PROGRAMA-MATNR
          AND SPRAS   = 'P'.

        TG_PROGRAMA-MAKTX = WL_MAKT-MAKTX.
        APPEND TG_PROGRAMA.
      ENDLOOP.

    ENDIF.
  ENDIF.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVA_DADOS .
  DATA: WL_INPUT_CADLAN   TYPE ZMMT0035,
        TL_INPUT_ZMMT0036 TYPE TABLE OF ZMMT0036 WITH HEADER LINE,
        TL_INPUT_ZMMT0037 TYPE TABLE OF ZMMT0037 WITH HEADER LINE,
        TL_INPUT_ZMMT0038 TYPE TABLE OF ZMMT0038 WITH HEADER LINE.

  MOVE-CORRESPONDING WG_CADLAN TO WL_INPUT_CADLAN.
  MOVE: SY-MANDT TO WL_INPUT_CADLAN-MANDT,
        SY-UNAME TO WL_INPUT_CADLAN-USNAM,
        SY-DATUM TO WL_INPUT_CADLAN-DATA_ATUAL,
        SY-UZEIT TO WL_INPUT_CADLAN-HORA_ATUAL,
        'F'      TO WL_INPUT_CADLAN-BSTYP .

  REFRESH: TG_EDITOR.
  IF OBG_DESCBOX IS NOT INITIAL.
    CALL METHOD OBG_DESCBOX->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE = TG_EDITOR.

    LOOP AT TG_EDITOR INTO WG_EDITOR.
      IF SY-TABIX EQ 1.
        WL_INPUT_CADLAN-TEXTO_NEG = WG_EDITOR-LINE.

      ELSEIF SY-TABIX GE 2.
        CONCATENATE WL_INPUT_CADLAN-TEXTO_NEG  WG_EDITOR-LINE INTO WL_INPUT_CADLAN-TEXTO_NEG. " SEPARATED BY space.

      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT TG_FATURA.
    MOVE : WG_CADLAN-NRO_SOL_CP     TO TL_INPUT_ZMMT0036-NRO_SOL_CP,
           TG_FATURA-DT_VCTO        TO TL_INPUT_ZMMT0036-DT_VCTO,
           TG_FATURA-PERCENTUAL     TO TL_INPUT_ZMMT0036-PERCENTUAL,
           TG_FATURA-VALOR          TO TL_INPUT_ZMMT0036-VALOR,
           TG_FATURA-MENGE          TO TL_INPUT_ZMMT0036-MENGE,
           SY-MANDT                 TO TL_INPUT_ZMMT0036-MANDT,
           SY-UNAME                 TO TL_INPUT_ZMMT0036-USNAM,
           SY-DATUM                 TO TL_INPUT_ZMMT0036-DATA_ATUAL,
           SY-UZEIT                 TO TL_INPUT_ZMMT0036-HORA_ATUAL.

    APPEND TL_INPUT_ZMMT0036.
  ENDLOOP.

  LOOP AT TG_PRODUTO.
    MOVE :  SY-MANDT                 TO TL_INPUT_ZMMT0037-MANDT,
            WG_CADLAN-NRO_SOL_CP     TO TL_INPUT_ZMMT0037-NRO_SOL_CP,
            TG_PRODUTO-EBELP         TO TL_INPUT_ZMMT0037-EBELP,
            TG_PRODUTO-MATNR         TO TL_INPUT_ZMMT0037-MATNR,
            TG_PRODUTO-LGORT         TO TL_INPUT_ZMMT0037-LGORT,
            TG_PRODUTO-MENGE         TO TL_INPUT_ZMMT0037-MENGE,
            TG_PRODUTO-MEINS         TO TL_INPUT_ZMMT0037-MEINS,
            TG_PRODUTO-NETPR         TO TL_INPUT_ZMMT0037-NETPR,
            TG_PRODUTO-NETPR_DESC    TO TL_INPUT_ZMMT0037-NETPR_DESC,
            TG_PRODUTO-PEINH         TO TL_INPUT_ZMMT0037-PEINH,
            TG_PRODUTO-MWSKZ         TO TL_INPUT_ZMMT0037-MWSKZ,
            SY-UNAME                 TO TL_INPUT_ZMMT0037-USNAM,
            SY-DATUM                 TO TL_INPUT_ZMMT0037-DATA_ATUAL,
            SY-UZEIT                 TO TL_INPUT_ZMMT0037-HORA_ATUAL.

    APPEND TL_INPUT_ZMMT0037.
  ENDLOOP.

  LOOP AT TG_PROGRAMA.
    MOVE: SY-MANDT                 TO TL_INPUT_ZMMT0038-MANDT,
          WG_CADLAN-NRO_SOL_CP     TO TL_INPUT_ZMMT0038-NRO_SOL_CP,
          TG_PROGRAMA-EBELP        TO TL_INPUT_ZMMT0038-EBELP,
          TG_PROGRAMA-MATNR        TO TL_INPUT_ZMMT0038-MATNR,
          TG_PROGRAMA-DATA_PROGR   TO TL_INPUT_ZMMT0038-DATA_PROGR,
          TG_PROGRAMA-MENGE        TO TL_INPUT_ZMMT0038-MENGE,
          TG_PROGRAMA-OBSERV       TO TL_INPUT_ZMMT0038-OBSERV,
          SY-UNAME                 TO TL_INPUT_ZMMT0038-USNAM,
          SY-DATUM                 TO TL_INPUT_ZMMT0038-DATA_ATUAL,
          SY-UZEIT                 TO TL_INPUT_ZMMT0038-HORA_ATUAL.

    APPEND TL_INPUT_ZMMT0038.
  ENDLOOP.

  DELETE FROM ZMMT0036 WHERE NRO_SOL_CP = WG_CADLAN-NRO_SOL_CP.
  DELETE FROM ZMMT0037 WHERE NRO_SOL_CP = WG_CADLAN-NRO_SOL_CP.
  DELETE FROM ZMMT0038 WHERE NRO_SOL_CP = WG_CADLAN-NRO_SOL_CP.
  MODIFY ZMMT0035 FROM       WL_INPUT_CADLAN.
  MODIFY ZMMT0036 FROM TABLE TL_INPUT_ZMMT0036.
  MODIFY ZMMT0037 FROM TABLE TL_INPUT_ZMMT0037.
  MODIFY ZMMT0038 FROM TABLE TL_INPUT_ZMMT0038.


  MESSAGE S836(SD) WITH 'Lançamento'
                         WG_CADLAN-NRO_SOL_CP
                         ', criado/modificado com sucesso!'.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_CAMPOS .
  CLEAR: WG_CADLAN , TG_EDITOR,WG_MENSAGEM,X_FIELD.
  WG_CADLAN-WAERS = 'BRL'.
  REFRESH: TG_FATURA,TG_PRODUTO,TG_PROGRAMA, TG_EDITOR.

ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OBTEM_PROXIMO .
  DATA: VNUM(10) TYPE C,
        VSEQ(10) TYPE P.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR = '01'
      OBJECT      = 'ZID_SOV'
    IMPORTING
      NUMBER      = VSEQ.

  VNUM = VSEQ .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = VNUM
    IMPORTING
      OUTPUT = VNUM.

  WG_CADLAN-NRO_SOL_CP = VNUM.

ENDFORM.                    " OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIALIZA_TELA OUTPUT.
  IF WG_ACAO IS INITIAL.
    REFRESH: TG_FIELDS.
    PERFORM TRATA_CAMPOS USING SPACE
                               'GR2'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM TRATA_CAMPOS USING SPACE
                              'GR1'
                               C_1       "INPUT 1     NO INPUT 0
                               C_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.
ENDMODULE.                 " INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_SAFRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_SAFRA INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE,
        VSAFRA        TYPE ZSDT0044-SAFRA.

  DATA: BEGIN OF TL_SAFRA OCCURS 0,
          SAFRA4 TYPE ZSDT0044-SAFRA,
          SAFRA  TYPE ZMMT0035-SAFRA,
        END OF TL_SAFRA.

  SELECT SAFRA
    FROM ZSDT0044
    INTO TABLE TL_SAFRA
    ORDER BY SAFRA ASCENDING.

  LOOP AT TL_SAFRA.
    VSAFRA = TL_SAFRA-SAFRA4.
    ADD 1 TO VSAFRA .
    CONCATENATE TL_SAFRA-SAFRA4 '/' VSAFRA INTO TL_SAFRA-SAFRA.
    MODIFY TL_SAFRA INDEX SY-TABIX TRANSPORTING SAFRA.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'SAFRA'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_CADLAN-SAFRA'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_SAFRA
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_SAFRA  INPUT
*&---------------------------------------------------------------------*
*&      Form  ELIMINAR_SOLICITACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ELIMINAR_SOLICITACAO .
  DATA: WL_ZMMT0035 TYPE ZMMT0035.

  SELECT  SINGLE *
    FROM ZMMT0035
    INTO WL_ZMMT0035
     WHERE NRO_SOL_CP = WG_CADLAN-NRO_SOL_CP.

  IF SY-SUBRC IS INITIAL.
    IF WL_ZMMT0035-LOEKZ IS INITIAL.
      MOVE: C_X TO WL_ZMMT0035-LOEKZ.
      MODIFY ZMMT0035 FROM WL_ZMMT0035.
      MESSAGE S836(SD) WITH 'O documento foi eliminado!'.
    ELSE.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Impossivel eliminar, o documento'
                            'já foi marcado para eliminação!'.
    ENDIF.
  ENDIF.
ENDFORM.                    " ELIMINAR_SOLICITACAO
*&---------------------------------------------------------------------*
*&      Module  SEARCH_ZTERM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_ZTERM INPUT.
  DATA: TL_RETURN_TERM TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELCT      TYPE TABLE OF DSELC      WITH HEADER LINE.


  DATA: BEGIN OF TL_TERM OCCURS 0,
          ZTERM TYPE T052-ZTERM,
          TEXT1 TYPE T052U-TEXT1,
        END OF TL_TERM.


  SELECT T052~ZTERM T052U~TEXT1
     FROM T052
     INNER JOIN T052U ON T052U~ZTERM = T052~ZTERM
     INTO TABLE TL_TERM
      WHERE  T052~KOART = ''
      AND    T052U~SPRAS = 'P'
      AND    T052U~ZTAGG = T052~ZTAGG
    ORDER BY T052~ZTERM ASCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'ZTERM'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_CADLAN-ZTERM'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_TERM
      RETURN_TAB      = TL_RETURN_TERM
      DYNPFLD_MAPPING = TL_DSELCT.
ENDMODULE.                 " SEARCH_ZTERM  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_F4_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_F4_CAT.
  GT_F4-FIELDNAME = 'LGORT'.
  GT_F4-REGISTER = 'X'.
  GT_F4-GETBEFORE = 'X'.
  GT_F4-CHNGEAFTER ='X'.
  APPEND GT_F4.
  CALL METHOD GRID2->REGISTER_F4_FOR_FIELDS
    EXPORTING
      IT_F4 = GT_F4[].

ENDFORM.                    "BUILD_F4_CAT
*&---------------------------------------------------------------------*
*&      Form  CRIA_REQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIA_REQ .

  DATA: IT_REQUISITION_ITEMS TYPE STANDARD TABLE OF BAPIEBANC,
        WA_REQUISITION_ITEMS TYPE BAPIEBANC,
        IT_RETURN            TYPE STANDARD TABLE OF BAPIRETURN,
        WA_RETURN            TYPE BAPIRETURN.

  DATA: WA_MAKT TYPE MAKT,
        WA_MARA TYPE MARA.

  DATA: IT_EDITOR_REQ TYPE STANDARD TABLE OF TY_EDITOR,  "Tabela para extração do texto da solicitação de compra
        WA_EDITOR_REQ TYPE TY_EDITOR,
        IT_LINES      TYPE STANDARD TABLE OF TLINE,          "Tabela para gravar texto do cabeçalho
        WA_LINES      TYPE TLINE,
        X_HEADER      TYPE THEAD,                            "Tabela-parâmetro para gravar texto do cabeçalho
        WA_EDITOR     TYPE TY_EDITOR,                        "WA texto do cabeçalho na solicitação de compra Agro
        W_NUMBER      LIKE BAPIEBANC-PREQ_NO,                "Número da Requisição
        C_E           TYPE C VALUE 'I',
        C_X           TYPE C VALUE 'X'.

  REFRESH:IT_REQUISITION_ITEMS, IT_RETURN.
  CLEAR: WA_REQUISITION_ITEMS, WA_RETURN.

  LOOP AT TG_PRODUTO.

    WA_REQUISITION_ITEMS-DOC_TYPE   = 'NB'.                   "Tipo de requisição de compra (P/ Agro sempre NB)
    WA_REQUISITION_ITEMS-PREQ_ITEM  = TG_PRODUTO-EBELP.       "N Item

* > 05/07/2023 - Migração S4 - LM
*    WA_REQUISITION_ITEMS-MATERIAL   = TG_PRODUTO-MATNR.       "N Material

    IF STRLEN( TG_PRODUTO-MATNR ) > 18.
      WA_REQUISITION_ITEMS-MATERIAL_LONG = TG_PRODUTO-MATNR.       "N Material - LONG
    ELSE.
      WA_REQUISITION_ITEMS-MATERIAL      = TG_PRODUTO-MATNR.       "N Material
    ENDIF.
* > 05/07/2023 - Migração S4 - LM

    SELECT SINGLE * INTO WA_MAKT FROM MAKT WHERE MATNR EQ WA_REQUISITION_ITEMS-MATERIAL AND SPRAS EQ SY-LANGU.
    WA_REQUISITION_ITEMS-SHORT_TEXT = WA_MAKT-MAKTX.          "Texto Breve Material

    WA_REQUISITION_ITEMS-STORE_LOC  = TG_PRODUTO-LGORT.       "Depósito
    WA_REQUISITION_ITEMS-QUANTITY   = TG_PRODUTO-MENGE.       "Quantidade
    WA_REQUISITION_ITEMS-PUR_GROUP  = WG_CADLAN-EKGRP.        "Grupo de Comprador
    WA_REQUISITION_ITEMS-PLANT = WG_CADLAN-WERKS.             "Centro

    SELECT SINGLE * INTO WA_MARA FROM MARA WHERE MATNR EQ WA_REQUISITION_ITEMS-MATERIAL.
    WA_REQUISITION_ITEMS-MAT_GRP    = WA_MARA-MATKL.          "Grupo de Mercadorias
    WA_REQUISITION_ITEMS-UNIT       = WA_MARA-MEINS.          "Unidade do Material

    WA_REQUISITION_ITEMS-DELIV_DATE = SY-DATUM.               "Data da remessa
    WA_REQUISITION_ITEMS-DEL_DATCAT = 1.                      "Tipo de data da remessa
    APPEND WA_REQUISITION_ITEMS TO IT_REQUISITION_ITEMS.

  ENDLOOP.

* > 05/07/2023 - Migração S4 - LM
*  CALL FUNCTION 'BAPI_REQUISITION_CREATE' "#EC CI_USAGE_OK[2438131]
*    IMPORTING
*      NUMBER            = W_NUMBER
*    TABLES
*      REQUISITION_ITEMS = IT_REQUISITION_ITEMS
*      "REQUISITION_ITEM_TEXT = REQUISITION_ITEM_TEXT
*      RETURN            = IT_RETURN.

  CALL FUNCTION '/MIGNOW/FM_MM_PR_CREATE_DP'
    IMPORTING
      NUMBER            = W_NUMBER
    TABLES
      REQUISITION_ITEMS = IT_REQUISITION_ITEMS
      "REQUISITION_ITEM_TEXT = REQUISITION_ITEM_TEXT
      RETURN            = IT_RETURN.
* > 05/07/2023 - Migração S4 - LM

  READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = C_E.
  IF SY-SUBRC EQ 0.

    "Commit
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = C_X.

    "Grava o cabeçalho
    X_HEADER-TDOBJECT = 'EBANH'.
    X_HEADER-TDNAME   = W_NUMBER.
    X_HEADER-TDID     = 'B01'.
    X_HEADER-TDSPRAS  = SY-LANGU.


    IF OBG_DESCBOX IS NOT INITIAL.
      CALL METHOD OBG_DESCBOX->GET_TEXT_AS_R3TABLE
        IMPORTING
          TABLE = IT_EDITOR_REQ.
    ENDIF.

    REFRESH: IT_LINES.
    CLEAR: WA_LINES.

    LOOP AT IT_EDITOR_REQ INTO WA_EDITOR_REQ.
      WA_LINES-TDFORMAT = '*'.
      WA_LINES-TDLINE = WA_EDITOR_REQ-LINE.
      APPEND WA_LINES TO IT_LINES.
    ENDLOOP.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        CLIENT          = SY-MANDT
        HEADER          = X_HEADER
        SAVEMODE_DIRECT = 'X'
      TABLES
        LINES           = IT_LINES
      EXCEPTIONS
        ID              = 1
        LANGUAGE        = 2
        NAME            = 3
        OBJECT          = 4
        OTHERS          = 5.

    MESSAGE S000(ZWRM001) DISPLAY LIKE 'S' WITH 'Requisição de Compra nº' W_NUMBER 'criada.'.
    WG_CADLAN-BANFN = W_NUMBER.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    READ TABLE IT_RETURN INTO WA_RETURN INDEX 1.
    MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH WA_RETURN-MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CRIA_PED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIA_PED .
types: BEGIN OF ty_editor,
          line(72),
        END   OF ty_editor.
data: tg_editor            TYPE TABLE OF ty_editor,
       wl_name  TYPE thead-tdname.
DATA: lt_lines  TYPE TABLE OF tline,
      lst_lines LIKE LINE OF  lt_lines,
      wl_header TYPE thead.

  DATA: "IT_RETURN_REQ             TYPE STANDARD TABLE OF BAPIRET2,
    "IT_ITEM_REQ               TYPE STANDARD TABLE OF BAPIMEREQITEM,
    "WA_ITEM_REQ               TYPE BAPIMEREQITEM,
    "IT_PRHEADERTEXT_REQ       TYPE STANDARD TABLE OF BAPIMEREQHEADTEXT,
    "WA_PRHEADERTEXT_REQ       TYPE BAPIMEREQHEADTEXT,
    IT_EDITOR_PED             TYPE STANDARD TABLE OF TY_EDITOR,  "Tabela para extração do texto da solicitação de compra
    WA_EDITOR_PED             TYPE TY_EDITOR,
    IT_RETURN_PED             TYPE STANDARD TABLE OF BAPIRET2, "TABLE OF BAPIRET2 WITH HEADER LINE,
    WA_RETURN_PED             TYPE BAPIRET2,
    IT_POITEM_PED             TYPE STANDARD TABLE OF BAPIMEPOITEM, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
    WA_POITEM_PED             TYPE BAPIMEPOITEM,
    IT_POITEMX_PED            TYPE STANDARD TABLE OF BAPIMEPOITEMX,
    WA_POITEMX_PED            TYPE BAPIMEPOITEMX,
    WA_POHEADER_PED           TYPE BAPIMEPOHEADER,
    WA_POHEADERX_PED          TYPE BAPIMEPOHEADERX,
    IT_BAPIMEPOTEXTHEADER_PED TYPE STANDARD TABLE OF BAPIMEPOTEXTHEADER,
    WA_BAPIMEPOTEXTHEADER_PED TYPE BAPIMEPOTEXTHEADER,
    PURCHASEORDER             LIKE BAPIMEPOHEADER-PO_NUMBER,
    V_EKORG                   TYPE T024W-EKORG,
    V_BRANCH                  TYPE T001W-J_1BBRANCH,
    V_BUKRS                   TYPE J_1BBRANCH-BUKRS,
    V_LIFNR                   TYPE LFA1-LIFNR,
    W_ZMMT0035                TYPE ZMMT0035.

DATA: V_BANKS TYPE BANKS,
     V_BANKL TYPE BANKL,
     V_BANKN TYPE BANKN,
     V_BVTYP TYPE BVTYP.

  SELECT SINGLE *
    FROM ZMMT0035
    INTO W_ZMMT0035
    WHERE NRO_SOL_CP = WG_CADLAN-NRO_SOL_CP.

  IF SY-SUBRC NE 0.
    MESSAGE 'Grave a solicitação antes, para gerar o pedido!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF WG_CADLAN-EBELN IS NOT INITIAL.
    MESSAGE 'Pedido já gerado para esta solicitação!' TYPE 'I'.
    EXIT.
  ENDIF.


* --- Gera Pedido sem Requisição
  IF TG_PRODUTO[] IS NOT INITIAL.

    REFRESH: IT_RETURN_PED, IT_POITEM_PED, IT_POITEMX_PED,
             IT_BAPIMEPOTEXTHEADER_PED.
    CLEAR: WA_RETURN_PED, WA_POITEM_PED, WA_POITEMX_PED,
           WA_POHEADER_PED, WA_POHEADERX_PED, WA_BAPIMEPOTEXTHEADER_PED.

    "Itens--------------
    LOOP AT TG_PRODUTO.
      WA_POITEM_PED-PO_ITEM    = TG_PRODUTO-EBELP.                          "Item

* > 05/07/2023 - Migração S4 - LM
*      WA_POITEM_PED-MATERIAL   = TG_PRODUTO-MATNR.                          "Material

      IF STRLEN( TG_PRODUTO-MATNR ) > 18.
        WA_POITEM_PED-MATERIAL_LONG = TG_PRODUTO-MATNR.   "Material - LONG
      ELSE.
        WA_POITEM_PED-MATERIAL      = TG_PRODUTO-MATNR.   "Material
      ENDIF.
* > 05/07/2023 - Migração S4 - LM

      WA_POITEM_PED-QUANTITY   = TG_PRODUTO-MENGE.                          "Quantidade
      WA_POITEM_PED-PO_PRICE   = 1.                                         "Transferência do preço: 1 = bruto, 2 = líquido
      WA_POITEM_PED-NET_PRICE  = TG_PRODUTO-NETPR.                          "Preço
      WA_POITEM_PED-TAX_CODE   = TG_PRODUTO-MWSKZ.                          "Código do Imposto
      WA_POITEM_PED-PLANT      = WG_CADLAN-WERKS.                           "Centro
      WA_POITEM_PED-STGE_LOC   = TG_PRODUTO-LGORT.                          "Depósito
      APPEND WA_POITEM_PED TO IT_POITEM_PED.
      WA_POITEMX_PED-PO_ITEM   = TG_PRODUTO-EBELP.                          "Item
      WA_POITEMX_PED-PO_ITEMX  = 'X'.                                       "Item
      WA_POITEMX_PED-MATERIAL  = 'X'.                                       "Material
      WA_POITEMX_PED-QUANTITY  = 'X'.                                       "Quantidade
      WA_POITEMX_PED-PO_PRICE  = 'X'.                                       "Transferência do preço: 1 = bruto, 2 = líquido
      WA_POITEMX_PED-NET_PRICE = 'X'.                                       "Preço
      WA_POITEMX_PED-TAX_CODE  = 'X'.                                       "Código do Imposto
      WA_POITEMX_PED-PLANT     = 'X'.                                       "Centro
      WA_POITEMX_PED-STGE_LOC  = 'X'.                                       "Depósito
      APPEND WA_POITEMX_PED TO IT_POITEMX_PED.
    ENDLOOP.

    "Cabeçalho----------
    SELECT SINGLE J_1BBRANCH
      FROM T001W
      INTO V_BRANCH
      WHERE WERKS EQ WG_CADLAN-WERKS.

    SELECT SINGLE BUKRS
      FROM J_1BBRANCH
      INTO V_BUKRS
      WHERE BRANCH EQ V_BRANCH.

    WA_POHEADER_PED-COMP_CODE = V_BUKRS.                  "Empresa
    WA_POHEADER_PED-DOC_TYPE = WG_CADLAN-BSART.           "Tipo de Pedido

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_CADLAN-LIFNR
      IMPORTING
        OUTPUT = V_LIFNR.

    WA_POHEADER_PED-VENDOR   = V_LIFNR.                   "Fornecedor pela ZMM0045

    SELECT SINGLE EKORG
      FROM T024W
      INTO V_EKORG
      WHERE WERKS EQ WG_CADLAN-WERKS.

    WA_POHEADER_PED-PURCH_ORG = V_EKORG.                  "Organização de Compras
    WA_POHEADER_PED-DOC_DATE  = SY-DATUM.                 "Data do Pedido
    WA_POHEADER_PED-LANGU     = SY-LANGU.                 "Idioma
    WA_POHEADER_PED-PUR_GROUP = WG_CADLAN-EKGRP.          "Grupo de Compradores
    WA_POHEADER_PED-CURRENCY  = WG_CADLAN-WAERS.          "Moeda pela ZMM0045
    IF WG_CADLAN-WKURS IS NOT INITIAL.
      WA_POHEADER_PED-EXCH_RATE = WG_CADLAN-WKURS.        "Taxa de Câmbio pela ZMM0045
    ELSE.
      WA_POHEADER_PED-EXCH_RATE = 1.                      "Taxa de Câmbio pela ZMM0045
    ENDIF.
    WA_POHEADER_PED-OUR_REF   = WG_CADLAN-SAFRA.          "Safra
    WA_POHEADER_PED-QUOT_DATE = WG_CADLAN-IHRAN.          "data cotação

    WA_POHEADERX_PED-COMP_CODE = 'X'.                     "Empresa
    WA_POHEADERX_PED-DOC_TYPE  = 'X'.                     "Tipo de Pedido
    WA_POHEADERX_PED-VENDOR    = 'X'.                     "Fornecedor pela ZMM0045
    WA_POHEADERX_PED-PURCH_ORG = 'X'.                     "Organização de Compras
    WA_POHEADERX_PED-DOC_DATE  = 'X'.                     "Data do Pedido
    WA_POHEADERX_PED-LANGU     = 'X'.                     "Idioma
    WA_POHEADERX_PED-PUR_GROUP = 'X'.                     "Grupo de Compradores
    WA_POHEADERX_PED-CURRENCY  = 'X'.                     "Moeda pela ZMM0045
    WA_POHEADERX_PED-EXCH_RATE = 'X'.                     "Taxa pela ZMM0045
    WA_POHEADERX_PED-OUR_REF   = 'X'.                     "Safra
    WA_POHEADERX_PED-QUOT_DATE = 'X'.                     "Data Cotação

    "Texto Cabeçalho----
    IF OBG_DESCBOX IS NOT INITIAL.
      CALL METHOD OBG_DESCBOX->GET_TEXT_AS_R3TABLE
        IMPORTING
          TABLE = IT_EDITOR_PED.
    ENDIF.

    LOOP AT IT_EDITOR_PED INTO WA_EDITOR_PED.
      WA_BAPIMEPOTEXTHEADER_PED-TEXT_ID = 'F01'.
      WA_BAPIMEPOTEXTHEADER_PED-TEXT_FORM = '*'.
      WA_BAPIMEPOTEXTHEADER_PED-TEXT_LINE = WA_EDITOR_PED-LINE.
      APPEND WA_BAPIMEPOTEXTHEADER_PED TO IT_BAPIMEPOTEXTHEADER_PED.
    ENDLOOP.

    CALL FUNCTION 'BAPI_PO_CREATE1' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        POHEADER         = WA_POHEADER_PED
        POHEADERX        = WA_POHEADERX_PED
      IMPORTING
        EXPPURCHASEORDER = PURCHASEORDER
      TABLES
        RETURN           = IT_RETURN_PED
        POITEM           = IT_POITEM_PED
        POITEMX          = IT_POITEMX_PED
        POTEXTHEADER     = IT_BAPIMEPOTEXTHEADER_PED.

    READ TABLE IT_RETURN_PED INTO WA_RETURN_PED WITH KEY TYPE = 'S' ID = '06' NUMBER = '017'.
    IF SY-SUBRC EQ 0.
      "Commit
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
      MESSAGE S000(ZWRM001) DISPLAY LIKE 'S' WITH WA_RETURN_PED-MESSAGE.
      WG_CADLAN-EBELN = PURCHASEORDER.
      UPDATE ZMMT0035 SET EBELN = WG_CADLAN-EBELN
         WHERE NRO_SOL_CP = WG_CADLAN-NRO_SOL_CP.

      "  Informar conta bancária no ped. compras #128658 - BG
  IF   WG_CADLAN-BVTYP IS NOT  INITIAL.

    SELECT SINGLE BVTYP BANKS BANKL BANKN
      FROM LFBK INTO ( V_BVTYP, V_BANKS, V_BANKL, V_BANKN )
      WHERE BVTYP = WG_CADLAN-BVTYP.



      wl_name = wg_cadlan-EBELN.

      CONCATENATE V_BVTYP V_BANKS V_BANKL V_BANKN INTO lst_lines SEPARATED BY SPACE.
      APPEND lst_lines TO lt_lines.
      wl_header-tdobject = 'EKKO'.
      wl_header-tdid     = 'F07'.
      wl_header-tdspras  = sy-langu.
      wl_header-tdname  = wl_name.


      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          client          = sy-mandt
          header          = wl_header
          savemode_direct = 'X'
        TABLES
          lines           = lt_lines[]
        EXCEPTIONS
          id              = 1
          language        = 2
          name            = 3
          object          = 4
          OTHERS          = 5.

      CLEAR lt_lines.

*      CALL FUNCTION 'COMMIT_TEXT'
*        EXPORTING
*          object          = 'EKKO'
*          name            = wl_header-tdname
*          id              = 'F07'
*          language        = sy-langu
*          savemode_direct = ' '.
*          .
    ENDIF.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      SORT IT_RETURN_PED BY NUMBER DESCENDING.
      READ TABLE IT_RETURN_PED INTO WA_RETURN_PED WITH KEY TYPE = 'E'.
      MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH WA_RETURN_PED-MESSAGE.
    ENDIF.

  ELSE.
    MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E14.
  ENDIF.

* --- Gera Pedido com Requisição
*  REFRESH: IT_RETURN_REQ, IT_ITEM_REQ, IT_PRHEADERTEXT_REQ.
*  CLEAR: WA_ITEM_REQ, WA_PRHEADERTEXT_REQ.
*
*  CALL FUNCTION 'BAPI_PR_GETDETAIL'
*    EXPORTING
*      NUMBER       = WG_CADLAN-BANFN
*      HEADER_TEXT  = 'X'
*    TABLES
*      RETURN       = IT_RETURN_REQ
*      PRITEM       = IT_ITEM_REQ
*      PRHEADERTEXT = IT_PRHEADERTEXT_REQ.
*
*IF IT_ITEM_REQ IS NOT INITIAL.
*
*    REFRESH: IT_RETURN_PED, IT_POITEM_PED, IT_POITEMX_PED,
*             IT_BAPIMEPOTEXTHEADER_PED.
*    CLEAR: WA_RETURN_PED, WA_POITEM_PED, WA_POITEMX_PED,
*           WA_POHEADER_PED, WA_POHEADERX_PED, WA_BAPIMEPOTEXTHEADER_PED.
*
*    "Itens--------------
*    LOOP AT IT_ITEM_REQ INTO WA_ITEM_REQ.
*      READ TABLE TG_PRODUTO WITH KEY EBELP = WA_ITEM_REQ-PREQ_ITEM.
*      WA_POITEM_PED-PO_ITEM    = WA_ITEM_REQ-PREQ_ITEM.                     "Item
*      WA_POITEM_PED-QUANTITY   = TG_PRODUTO-MENGE.                          "Quantidade
*      WA_POITEM_PED-NET_PRICE  = TG_PRODUTO-NETPR.                          "Preço
*      WA_POITEM_PED-PREQ_NO    = WG_CADLAN-BANFN.                           "Número da RC
*      WA_POITEM_PED-PREQ_ITEM  = WA_ITEM_REQ-PREQ_ITEM.                     "Item da RC
*      WA_POITEM_PED-TAX_CODE   = TG_PRODUTO-MWSKZ.                          "Código do Imposto
*      APPEND WA_POITEM_PED TO IT_POITEM_PED.
*      WA_POITEMX_PED-PO_ITEM   = WA_ITEM_REQ-PREQ_ITEM.                     "Item
*      WA_POITEMX_PED-PO_ITEMX  = 'X'.                                       "Item
*      WA_POITEMX_PED-QUANTITY  = 'X'.                                       "Quantidade
*      WA_POITEMX_PED-NET_PRICE = 'X'.                                       "Preço
*      WA_POITEMX_PED-PREQ_NO   = 'X'.                                       "Número da RC
*      WA_POITEMX_PED-PREQ_ITEM = 'X'.                                       "Item da RC
*      WA_POITEMX_PED-TAX_CODE  = 'X'.                                       "Código do Imposto
*      APPEND WA_POITEMX_PED TO IT_POITEMX_PED.
*    ENDLOOP.
*
*    "Cabeçalho----------
*    READ TABLE IT_ITEM_REQ INTO WA_ITEM_REQ INDEX 1.
*
*    SELECT SINGLE J_1BBRANCH
*      FROM T001W
*      INTO V_BRANCH
*      WHERE WERKS EQ WA_ITEM_REQ-PLANT.
*
*    SELECT SINGLE BUKRS
*      FROM J_1BBRANCH
*      INTO V_BUKRS
*      WHERE BRANCH EQ V_BRANCH.
*
*    WA_POHEADER_PED-COMP_CODE = V_BUKRS.                  "Empresa
*    WA_POHEADER_PED-DOC_TYPE = WG_CADLAN-BSART.           "'ZNB'.                     "Tipo de Pedido
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = WG_CADLAN-LIFNR
*      IMPORTING
*        OUTPUT = V_LIFNR.
*
*    WA_POHEADER_PED-VENDOR   = V_LIFNR.                   "Fornecedor pela ZMM0045
*
*    SELECT SINGLE EKORG
*      FROM T024W
*      INTO V_EKORG
*      WHERE WERKS EQ WA_ITEM_REQ-PLANT.
*
*    WA_POHEADER_PED-PURCH_ORG = V_EKORG.                  "Organização de Compras
*    WA_POHEADER_PED-DOC_DATE  = SY-DATUM.                 "Data do Pedido
*    WA_POHEADER_PED-LANGU     = SY-LANGU.                 "Idioma
*    WA_POHEADER_PED-PUR_GROUP = WA_ITEM_REQ-PUR_GROUP.    "Grupo de Compradores
*    WA_POHEADER_PED-CURRENCY  = WG_CADLAN-WAERS.          "Moeda pela ZMM0045
*
*    WA_POHEADERX_PED-COMP_CODE = 'X'.                     "Empresa
*    WA_POHEADERX_PED-DOC_TYPE  = 'X'.                     "Tipo de Pedido
*    WA_POHEADERX_PED-VENDOR    = 'X'.                     "Fornecedor pela ZMM0045
*    WA_POHEADERX_PED-PURCH_ORG = 'X'.                     "Organização de Compras
*    WA_POHEADERX_PED-DOC_DATE  = 'X'.                     "Data do Pedido
*    WA_POHEADERX_PED-LANGU     = 'X'.                     "Idioma
*    WA_POHEADERX_PED-PUR_GROUP = 'X'.                     "Grupo de Compradores
*    WA_POHEADERX_PED-CURRENCY  = 'X'.                     "Moeda pela ZMM0045
*
*    "Texto Cabeçalho----
*    LOOP AT IT_PRHEADERTEXT_REQ INTO WA_PRHEADERTEXT_REQ.
*      WA_BAPIMEPOTEXTHEADER_PED-TEXT_ID = 'F01'.
*      WA_BAPIMEPOTEXTHEADER_PED-TEXT_FORM = '*'.
*      WA_BAPIMEPOTEXTHEADER_PED-TEXT_LINE = WA_PRHEADERTEXT_REQ-TEXT_LINE.
*      APPEND WA_BAPIMEPOTEXTHEADER_PED TO IT_BAPIMEPOTEXTHEADER_PED.
*    ENDLOOP.
*
*    CALL FUNCTION 'BAPI_PO_CREATE1'
*      EXPORTING
*        POHEADER         = WA_POHEADER_PED
*        POHEADERX        = WA_POHEADERX_PED
*      IMPORTING
*        EXPPURCHASEORDER = PURCHASEORDER
*      TABLES
*        RETURN           = IT_RETURN_PED
*        POITEM           = IT_POITEM_PED
*        POITEMX          = IT_POITEMX_PED
*        POTEXTHEADER     = IT_BAPIMEPOTEXTHEADER_PED.
*
*    READ TABLE IT_RETURN_PED INTO WA_RETURN_PED WITH KEY TYPE = 'S' ID = '06' NUMBER = '017'.
*    IF SY-SUBRC EQ 0.
*      "Commit
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          WAIT = 'X'.
*      MESSAGE S000(ZWRM001) DISPLAY LIKE 'S' WITH WA_RETURN_PED-MESSAGE.
*      WG_CADLAN-EBELN = PURCHASEORDER.
*    ELSE.
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*      SORT IT_RETURN_PED BY NUMBER DESCENDING.
*      READ TABLE IT_RETURN_PED INTO WA_RETURN_PED WITH KEY TYPE = 'E'.
*      MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH WA_RETURN_PED-MESSAGE.
*    ENDIF.
*
*  ELSE.
*    MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E14.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_BSART  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_BSART INPUT.

  TYPE-POOLS: VRM.

  DATA: OPTIONS_BSART TYPE VRM_VALUES,
        VALUE_BSART   LIKE LINE OF OPTIONS_BSART,
        LINHA_03      TYPE I.

  "Criando opções na listbox WA_TRANS-TRANSF_APROV
  VALUE_BSART-KEY = 'ZDEF'.
  VALUE_BSART-TEXT = 'ZDEF'.
  APPEND VALUE_BSART TO OPTIONS_BSART.

  VALUE_BSART-KEY = 'ZFTE'.
  VALUE_BSART-TEXT = 'ZFTE'.
  APPEND VALUE_BSART TO OPTIONS_BSART.

  VALUE_BSART-KEY = 'ZSEM'.
  VALUE_BSART-TEXT = 'ZSEM'.
  APPEND VALUE_BSART TO OPTIONS_BSART.

  VALUE_BSART-KEY = 'ZNB'.
  VALUE_BSART-TEXT = 'ZNB'.
  APPEND VALUE_BSART TO OPTIONS_BSART.

  VALUE_BSART-KEY = 'ZEF'.
  VALUE_BSART-TEXT = 'ZEF'.
  APPEND VALUE_BSART TO OPTIONS_BSART.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = 'WG_CADLAN-BSART'
      VALUES = OPTIONS_BSART.

  CLEAR: OPTIONS_BSART, VALUE_BSART.

ENDMODULE.

*{   INSERT         DEVK9A1WY9                                        1
*&---------------------------------------------------------------------*
*&      Module  SEARCH_BVTYP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_BVTYP INPUT.
 DATA: TL_RETURN_BVTYP TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC_BVTYP      TYPE TABLE OF DSELC      WITH HEADER LINE,
        VBVTYP        TYPE LFBK-BVTYP.
data: v_lifnr TYPE lifnr.

  DATA: BEGIN OF TL_BVTYP OCCURS 0,
          tpbn TYPE LFBK-BANKS,
          PAIS  TYPE LFBK-BANKL,
          CH_banco TYPE LFBK-BANKN,
         C_BANCARIA TYPE LFBK-BVTYP,
        END OF TL_BVTYP.

if WG_CADLAN-LIFNR is NOT INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT         = WG_CADLAN-LIFNR
         IMPORTING
           OUTPUT        = v_lifnr.


  SELECT BANKS BANKL BANKN BVTYP
    FROM LFBK
    INTO TABLE TL_BVTYP
    WHERE LIFNR  EQ v_lifnr.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'CONTA BANCARIA'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_CADLAN-BVTYP'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_BVTYP
      RETURN_TAB      = TL_RETURN_BVTYP
      DYNPFLD_MAPPING = TL_DSELC_BVTYP.

  endif.
ENDMODULE.
*}   INSERT
