************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 25.03.2009                                          *
* Tipo de prg ...: Include                                             *
* Objetivo    ...: Relatório de aprensantação da DRE Gerencial         *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 25.03.2009    Marcus.Barbara       Criação              DEVK905716   *
************************************************************************
***INCLUDE ZGL009_INCLUDE .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Metodo e clases do evendo duplo clique
*&---------------------------------------------------------------------*

CLASS LCL_TREE_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    METHODS HANDLE_ITEM_DOUBLE_CLICK
    FOR EVENT ITEM_DOUBLE_CLICK OF CL_GUI_ALV_TREE
    IMPORTING NODE_KEY
      FIELDNAME.

ENDCLASS.                    "lcl_tree_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_tree_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_TREE_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_ITEM_DOUBLE_CLICK.
    DATA: LT_SELECTED_NODES TYPE LVC_T_NKEY,
          L_NODE_KEY        TYPE LVC_NKEY,
          L_FIELDNAME       TYPE LVC_FNAME.

    REFRESH: LT_SELECTED_NODES.
    CALL METHOD TREE1->GET_SELECTED_NODES
      CHANGING
        CT_SELECTED_NODES = LT_SELECTED_NODES.
    IF LT_SELECTED_NODES[] IS INITIAL.
      CALL METHOD TREE1->GET_SELECTED_ITEM
        IMPORTING
          E_SELECTED_NODE = L_NODE_KEY
          E_FIELDNAME     = L_FIELDNAME.
    ELSE.
      READ TABLE LT_SELECTED_NODES INTO L_NODE_KEY INDEX 1.
    ENDIF.
    IF NOT L_NODE_KEY IS INITIAL.
      PERFORM F_TREE_DOUBLE_CLICK USING L_NODE_KEY.
    ENDIF.
  ENDMETHOD.                    "handle_item_double_click
ENDCLASS.                    "lcl_tree_event_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  F_INICIA_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM F_INICIA_TREE .

  DATA: VL_TREE_CONT_NAME         TYPE C LENGTH 30,
        VL_CUSTOM_CONTAINER       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
        VL_LIST_COMMENTARY        TYPE SLIS_T_LISTHEADER,
        VL_LOGO                   TYPE SDYDO_VALUE,
        VL_VARIANT                TYPE DISVARIANT,
        VL_HIERARCHY_HEADER       TYPE TREEV_HHDR.

  SORT IT_DADOS BY ORDNV TXT50 LTEXT.

* Crio a fieldcatalog para a structure do relatório
  PERFORM F_FIELDCATALOG.

* Crio a container para alv-tree
  VL_TREE_CONT_NAME = 'TREE1'.

  IF SY-BATCH IS INITIAL.
    IF VL_CUSTOM_CONTAINER IS INITIAL.
      CREATE OBJECT VL_CUSTOM_CONTAINER
        EXPORTING
          CONTAINER_NAME              = VL_TREE_CONT_NAME
        EXCEPTIONS
          CNTL_ERROR                  = 1
          CNTL_SYSTEM_ERROR           = 2
          CREATE_ERROR                = 3
          LIFETIME_ERROR              = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5.
      IF SY-SUBRC <> 0.
        MESSAGE X208(00) WITH 'ERROR'.                      "#EC NOTEXT
      ENDIF.
    ENDIF.
  ENDIF.

* Crio tree control
  IF TREE1 IS INITIAL.
    CREATE OBJECT TREE1
      EXPORTING
        PARENT                      = VL_CUSTOM_CONTAINER
        NODE_SELECTION_MODE         = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
        ITEM_SELECTION              = 'X'
        NO_HTML_HEADER              = ''
        NO_TOOLBAR                  = ''
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        ILLEGAL_NODE_SELECTION_MODE = 5
        FAILED                      = 6
        ILLEGAL_COLUMN_NAME         = 7.
    IF SY-SUBRC <> 0.
      MESSAGE X208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.
  ENDIF.

  PERFORM F_HIERARCHY_HEADER_ CHANGING VL_HIERARCHY_HEADER.

* Monto cabeçalho do ALV
  PERFORM F_CABECALHO USING VL_LIST_COMMENTARY.

  VL_VARIANT-REPORT = SY-REPID.

* create emty tree-control
  CALL METHOD TREE1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_HIERARCHY_HEADER = VL_HIERARCHY_HEADER
      IT_LIST_COMMENTARY  = VL_LIST_COMMENTARY
*      i_logo              = vl_logo
      I_BACKGROUND_ID     = 'ALV_BACKGROUND'
      I_SAVE              = 'A'
      IS_VARIANT          = VL_VARIANT
    CHANGING
      IT_OUTTAB           = IT_ALVTREE"table must be emty !!
      IT_FIELDCATALOG     = IT_FIELDCATALOG.

* create hierarchy
*  perform f_create_hierarchy.
  PERFORM SELECT_DATA_AND_FILL_COL_TREE.

* add own functioncodes to the toolbar
  PERFORM F_CHANGE_TOOLBAR.

* register events
  PERFORM F_REGISTER_EVENTS.
*
** adjust column_width
** call method tree1->COLUMN_OPTIMIZE.
ENDFORM.                    " F_INICIA_TREE
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_FIELDCATALOG .
  DATA: WA_FIELDCATALOG  TYPE LVC_S_FCAT.

* busco fieldcatalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZST_GL009_DADOS'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

*  sort it_fieldcatalog by scrtext_l.

* Altero fieldcatalog
  LOOP AT IT_FIELDCATALOG INTO WA_FIELDCATALOG.
    CASE WA_FIELDCATALOG-FIELDNAME.
      WHEN 'NIVEL'  OR 'SAKNR' OR 'DESNVL' OR 'ORDNV' OR
           'TLEVEL' OR 'TXT50' OR 'KOSTL'  OR 'AUFNR' OR
           'PRCTR'  OR 'LTEXT' OR 'NVL1' OR 'NVL2' OR
           'NVL3'   OR 'NVL4'  OR 'NVL5' OR 'NVL6' OR
           'NVL7'   OR 'NVL8'  OR 'NVL9' OR 'NVL10'.
        WA_FIELDCATALOG-NO_OUT = 'X'.
        WA_FIELDCATALOG-KEY    = ''.
      WHEN 'QTD_TON'.
        WA_FIELDCATALOG-REPTEXT = '[Mês]Ton'.
        WA_FIELDCATALOG-DO_SUM = 'X'.
        WA_FIELDCATALOG-VALEXI = 'X'.
      WHEN 'VLR_REA'.
        WA_FIELDCATALOG-REPTEXT = '[Mês]Realizado'.
        WA_FIELDCATALOG-DO_SUM = 'X'.
        WA_FIELDCATALOG-VALEXI = 'X'.
      WHEN 'VLR_MOV'.
        WA_FIELDCATALOG-REPTEXT = '[Mês]Ajuste Ger.'.
        WA_FIELDCATALOG-DO_SUM = 'X'.
        WA_FIELDCATALOG-VALEXI = 'X'.
      WHEN 'VLR_CALC'.
        WA_FIELDCATALOG-REPTEXT = '[Mês]Real Aj.'.
        WA_FIELDCATALOG-DO_SUM = 'X'.
        WA_FIELDCATALOG-VALEXI = 'X'.
      WHEN 'ANA_VLR'.
        WA_FIELDCATALOG-REPTEXT = '[Mês]Análise Vlr/Ton'.
      WHEN 'QTD_ACM'.
        WA_FIELDCATALOG-REPTEXT = '[Acumulado]Ton'.
        WA_FIELDCATALOG-DO_SUM = 'X'.
        WA_FIELDCATALOG-VALEXI = 'X'.
      WHEN 'VLR_ACM'.
        WA_FIELDCATALOG-REPTEXT = '[Acumulado]Realizado'.
        WA_FIELDCATALOG-DO_SUM = 'X'.
        WA_FIELDCATALOG-VALEXI = 'X'.
      WHEN 'VLR_MOV_ACM'.
        WA_FIELDCATALOG-REPTEXT = '[Acumulado]Ajuste Ger.'.
        WA_FIELDCATALOG-DO_SUM = 'X'.
        WA_FIELDCATALOG-VALEXI = 'X'.
      WHEN 'VLR_CALC_ACM'.
        WA_FIELDCATALOG-REPTEXT = '[Acumulado]Real Aj.'.
        WA_FIELDCATALOG-DO_SUM = 'X'.
        WA_FIELDCATALOG-VALEXI = 'X'.
      WHEN 'ANA_ACM'.
        WA_FIELDCATALOG-REPTEXT = '[Acumulado]Análise Vlr/Ton'.
    ENDCASE.

    MODIFY IT_FIELDCATALOG FROM WA_FIELDCATALOG.
  ENDLOOP.
ENDFORM.                    " F_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  F_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CABECALHO  USING    P_LIST_COMMENTARY TYPE SLIS_T_LISTHEADER.
  DATA: VL_LINE          TYPE SLIS_LISTHEADER,
        VL_TXT           TYPE C LENGTH 50,
        VL_EMPRESA       TYPE C LENGTH 50,
        VL_DATA          TYPE C LENGTH 14,
        VL_HORA          TYPE C LENGTH 8.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      LANGUAGE    = SY-LANGU
    TABLES
      MONTH_NAMES = IT_MES.

  READ TABLE IT_MES INTO WA_MES WITH KEY MNR = WA_DRE_005-MONAT.
  VL_TXT = WA_MES-LTX.
  SELECT SINGLE BUTXT
    FROM T001
    INTO VL_EMPRESA
   WHERE BUKRS EQ WA_DRE_005-BUKRS.

* Primeira linha: TYPE H
  CONCATENATE VG_TITULO '['
              VL_TXT '-' WA_DRE_005-GJAHR ']' INTO VL_TXT.

  CLEAR VL_LINE.
  VL_LINE-TYP  = 'H'.
  VL_LINE-INFO = VL_TXT.
  APPEND VL_LINE TO P_LIST_COMMENTARY.
  CLEAR VL_LINE.
  VL_LINE-TYP  = 'S'.
  VL_LINE-KEY = VL_EMPRESA.
  APPEND VL_LINE TO P_LIST_COMMENTARY.

  CLEAR VL_LINE.
  VL_LINE-TYP  = 'S'.
  VL_LINE-KEY  = 'Moeda:'.
  VL_LINE-INFO = WA_DRE_001-WAERS.
  APPEND VL_LINE TO P_LIST_COMMENTARY.

  CONCATENATE WA_DRE_005-DATUM+6(2)
              '.' WA_DRE_005-DATUM+4(2)
              '.' WA_DRE_005-DATUM(4) INTO VL_DATA.

  CONCATENATE WA_DRE_005-UZEIT(2)
              ':' WA_DRE_005-UZEIT+2(2)
              ':' WA_DRE_005-UZEIT+4(2) INTO VL_HORA.

  CONCATENATE  WA_DRE_005-UNAME
              'em' VL_DATA
              '-' VL_HORA INTO VL_TXT
              SEPARATED BY SPACE.
  CLEAR VL_LINE.
  VL_LINE-TYP  = 'S'.
  VL_LINE-KEY  = 'Gerado por:'.
  VL_LINE-INFO = VL_TXT.
  APPEND VL_LINE TO P_LIST_COMMENTARY.

*  p_logo = 'ENJOYSAP_LOGO'.
ENDFORM.                    " F_CABECALHO

*&---------------------------------------------------------------------*
*&      Form  F_EXIT_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_EXIT_PROGRAM .
  CALL METHOD TREE1->FREE.
  CALL METHOD GO_TBM->SAVE_STATE.
ENDFORM.                    " F_EXIT_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CHANGE_TOOLBAR.
  DATA:
        LT_BUTTON_ALV  TYPE TTB_BUTTON,
        LT_BUTTON_APPL TYPE TTB_BUTTON.

* get toolbar control
  CALL METHOD TREE1->GET_TOOLBAR_OBJECT
    IMPORTING
      ER_TOOLBAR = MR_TOOLBAR.

  CHECK NOT MR_TOOLBAR IS INITIAL.

  LT_BUTTON_ALV = MR_TOOLBAR->M_TABLE_BUTTON.

  CALL METHOD MR_TOOLBAR->DELETE_ALL_BUTTONS
    EXCEPTIONS
      CNTL_ERROR = 1
      OTHERS     = 2.

* Add Standard Button to toolbar (for Delete Subtree)
  CALL METHOD MR_TOOLBAR->ADD_BUTTON
    EXPORTING
      FCODE     = 'DELETE'
      ICON      = '@18@'
      BUTN_TYPE = CNTB_BTYPE_BUTTON
      TEXT      = ''
      QUICKINFO = 'Delete subtree'.                         "#EC NOTEXT

* add Dropdown Button to toolbar (for Insert Line)
  CALL METHOD MR_TOOLBAR->ADD_BUTTON
    EXPORTING
      FCODE     = 'INSERT_LC'
      ICON      = '@17@'
      BUTN_TYPE = CNTB_BTYPE_DROPDOWN
      TEXT      = ''
      QUICKINFO = 'Insert Line'.                            "#EC NOTEXT

* add seperator to toolbar
  CALL METHOD MR_TOOLBAR->ADD_BUTTON
    EXPORTING
      FCODE     = ''
      ICON      = ''
      BUTN_TYPE = CNTB_BTYPE_SEP
      TEXT      = ''
      QUICKINFO = 'This is a Seperator'.                    "#EC NOTEXT

  LT_BUTTON_APPL = MR_TOOLBAR->M_TABLE_BUTTON.

  INSERT LINES OF LT_BUTTON_APPL INTO LT_BUTTON_ALV INDEX 1.

  CALL METHOD MR_TOOLBAR->DELETE_ALL_BUTTONS
    EXCEPTIONS
      CNTL_ERROR = 1
      OTHERS     = 2.

  CALL METHOD MR_TOOLBAR->ADD_BUTTON_GROUP
    EXPORTING
      DATA_TABLE       = LT_BUTTON_ALV
    EXCEPTIONS
      DP_ERROR         = 1
      CNTB_ERROR_FCODE = 2
      OTHERS           = 3.

* set event-handler for toolbar-control
  CREATE OBJECT TOOLBAR_EVENT_RECEIVER.

  SET HANDLER: TOOLBAR_EVENT_RECEIVER->ON_FUNCTION_SELECTED FOR MR_TOOLBAR,
  TOOLBAR_EVENT_RECEIVER->ON_TOOLBAR_DROPDOWN  FOR MR_TOOLBAR.

  CREATE OBJECT GO_TBM
    EXPORTING
      IO_ALV_TREE = TREE1.

  GO_TBM->REORGANIZE( ).

ENDFORM.                    " F_CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  F_REGISTER_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_REGISTER_EVENTS .
* define the events which will be passed to the backend
  DATA: LT_EVENTS TYPE CNTL_SIMPLE_EVENTS,
        L_EVENT TYPE CNTL_SIMPLE_EVENT.

* define the events which will be passed to the backend
  CALL METHOD TREE1->GET_REGISTERED_EVENTS
    IMPORTING
      EVENTS = LT_EVENTS.

  L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  APPEND L_EVENT TO LT_EVENTS.
**  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_checkbox_change.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_header_context_men_req.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
**  append l_event to lt_events.
**
  CALL METHOD TREE1->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS                    = LT_EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.
  IF SY-SUBRC <> 0.
    MESSAGE X208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.
**
*** set Handler
  DATA: L_EVENT_RECEIVER TYPE REF TO LCL_TREE_EVENT_RECEIVER.
*  create object l_event_receiver.
*  set handler l_event_receiver->handle_item_double_click
*  for tree1.
**  set handler l_event_receiver->handle_node_ctmenu_selected
**  for tree1.
**  set handler l_event_receiver->handle_item_ctmenu_request
**  for tree1.
**  set handler l_event_receiver->handle_item_ctmenu_selected
**  for tree1.
***  *§4d. Register events on backend (ABAP Objects event handling)
  CREATE OBJECT L_EVENT_RECEIVER.
  SET HANDLER L_EVENT_RECEIVER->HANDLE_ITEM_DOUBLE_CLICK FOR TREE1.
ENDFORM.                    " F_REGISTER_EVENTS

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO OUTPUT.
  IF SY-DYNNR = 1000.
    IF ( SY-UCOMM EQ 'BACK') OR ( SY-UCOMM EQ 'EXIT' ) OR ( SY-UCOMM EQ 'CANC' ).
      LEAVE PROGRAM.
    ELSEIF SY-UCOMM EQ 'EXEC'.
      CALL SCREEN 100.
    ELSE.
      SET PF-STATUS 'TELA_1000'.
    ENDIF.
  ENDIF.

  IF SY-DYNNR = 100.
    IF OK_CODE IS INITIAL.

        SET TITLEBAR 'TITULO'.
        SET PF-STATUS 'TELA_2000'.
        PERFORM F_INICIA_TREE.
        CALL METHOD CL_GUI_CFW=>FLUSH
          EXCEPTIONS
            CNTL_SYSTEM_ERROR = 1
            CNTL_ERROR        = 2.

    ELSEIF ( OK_CODE EQ 'BACK' ) OR ( OK_CODE EQ 'EXIT') OR ( OK_CODE EQ 'CANC' ).
      SET PF-STATUS 'TELA_1000'.
      LEAVE TO SCREEN 1000.
*      CALL SELECTION-SCREEN 1000.
*      IF ( OK_CODE EQ 'BACK' ) OR ( OK_CODE EQ 'EXIT') OR ( OK_CODE EQ 'CANC').
*        LEAVE PROGRAM.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " PBO  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE PAI INPUT.

  IF SY-DYNNR = 100.
    IF ( OK_CODE EQ 'BACK' ) OR ( OK_CODE EQ 'EXIT') OR ( OK_CODE EQ 'CANC').
      CLEAR SY-UCOMM.
      PERFORM F_EXIT_PROGRAM.
*      SET PF-STATUS 'TELA_1000'.
*      LEAVE SCREEN.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  IF SY-DYNNR = 1000.
    IF ( SY-UCOMM EQ 'BACK') OR ( SY-UCOMM EQ 'EXIT' ) OR ( SY-UCOMM EQ 'CANC' ).
      LEAVE PROGRAM.
    ELSEIF SY-UCOMM EQ 'EXEC'.
      CLEAR: OK_CODE.
      CALL SCREEN 100.
    ENDIF.
  ENDIF.

ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_AND_FILL_COL_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_DATA_AND_FILL_COL_TREE.
  DATA: L_NODE_TEXT      TYPE LVC_VALUE,
        L_NVL1_KEY       TYPE LVC_NKEY,
        L_NVL2_KEY       TYPE LVC_NKEY,
        L_NVL3_KEY       TYPE LVC_NKEY,
        L_NVL4_KEY       TYPE LVC_NKEY,
        L_NVL5_KEY       TYPE LVC_NKEY,
        L_NVL6_KEY       TYPE LVC_NKEY,
        L_NVL7_KEY       TYPE LVC_NKEY,
        L_NVL8_KEY       TYPE LVC_NKEY,
        L_NVL9_KEY       TYPE LVC_NKEY,
        L_NVL10_KEY      TYPE LVC_NKEY,
        L_ROOT_KEY       TYPE LVC_NKEY,
        L_SAK_KEY        TYPE LVC_NKEY,
        L_AUX_KEY        TYPE LVC_NKEY,
        L_TEXT_KEY       TYPE LVC_NKEY,
        LT_LAYOUT_ITEM   TYPE LVC_T_LAYI,
        VL_LGNVL         LIKE ZGL002_DRE_EST-LGNVL,
        VL_IDEX          TYPE SY-TABIX,
        TEXTO(50).

  CLEAR VL_LGNVL.
  LOOP AT IT_DADOS INTO WA_DADOS.
    VL_IDEX = SY-TABIX.

    ON CHANGE OF WA_DADOS-NVL1.
      IF WA_DADOS-NVL1 GT 0.
        PERFORM CREATE_ITEM_LAYOUTS CHANGING LT_LAYOUT_ITEM.
        L_NODE_TEXT = WA_DADOS-DESNVL.

        READ TABLE IT_DRE_008 INTO WA_DRE_008 WITH KEY NIVEL = WA_DADOS-NIVEL.
        IF SY-SUBRC EQ 0.
          CALL METHOD TREE1->ADD_NODE
            EXPORTING
              I_RELAT_NODE_KEY = L_ROOT_KEY
              I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
              I_NODE_TEXT      = L_NODE_TEXT
              IS_OUTTAB_LINE   = WA_DADOS
              IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
            IMPORTING
              E_NEW_NODE_KEY   = L_NVL1_KEY.
        ELSE.
          CALL METHOD TREE1->ADD_NODE
            EXPORTING
              I_RELAT_NODE_KEY = L_ROOT_KEY
              I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
              I_NODE_TEXT      = L_NODE_TEXT
              IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
            IMPORTING
              E_NEW_NODE_KEY   = L_NVL1_KEY.
        ENDIF.
        L_AUX_KEY = L_NVL1_KEY.
        WA_DADOS-NKEY = L_AUX_KEY.
        APPEND WA_DADOS TO IT_ALVTREE.
      ENDIF.
    ENDON.

    ON CHANGE OF WA_DADOS-NVL2.
      IF WA_DADOS-NVL2 GT 0.
        L_NODE_TEXT = WA_DADOS-DESNVL.
        CALL METHOD TREE1->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_NVL1_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
          IMPORTING
            E_NEW_NODE_KEY   = L_NVL2_KEY.
        L_AUX_KEY = L_NVL2_KEY.
        WA_DADOS-NKEY = L_AUX_KEY.
        APPEND WA_DADOS TO IT_ALVTREE.
      ENDIF.
    ENDON.

    ON CHANGE OF WA_DADOS-NVL3.
      IF WA_DADOS-NVL3 GT 0.
        L_NODE_TEXT = WA_DADOS-DESNVL.
        CALL METHOD TREE1->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_NVL2_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
          IMPORTING
            E_NEW_NODE_KEY   = L_NVL3_KEY.
        L_AUX_KEY = L_NVL3_KEY.
        WA_DADOS-NKEY = L_AUX_KEY.
        APPEND WA_DADOS TO IT_ALVTREE.
      ENDIF.
    ENDON.

    ON CHANGE OF WA_DADOS-NVL4.
      IF WA_DADOS-NVL4 GT 0.
        L_NODE_TEXT = WA_DADOS-DESNVL.
        CALL METHOD TREE1->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_NVL3_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
          IMPORTING
            E_NEW_NODE_KEY   = L_NVL4_KEY.
        L_AUX_KEY = L_NVL4_KEY.
        WA_DADOS-NKEY = L_AUX_KEY.
        APPEND WA_DADOS TO IT_ALVTREE.
      ENDIF.
    ENDON.

    ON CHANGE OF WA_DADOS-NVL5.
      IF WA_DADOS-NVL5 GT 0.
        L_NODE_TEXT = WA_DADOS-DESNVL.
        CALL METHOD TREE1->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_NVL4_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
          IMPORTING
            E_NEW_NODE_KEY   = L_NVL5_KEY.
        L_AUX_KEY = L_NVL5_KEY.
        WA_DADOS-NKEY = L_AUX_KEY.
        APPEND WA_DADOS TO IT_ALVTREE.
      ENDIF.
    ENDON.

    ON CHANGE OF WA_DADOS-NVL6.
      IF WA_DADOS-NVL6 GT 0.
        L_NODE_TEXT = WA_DADOS-DESNVL.
        CALL METHOD TREE1->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_NVL5_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
          IMPORTING
            E_NEW_NODE_KEY   = L_NVL6_KEY.
        L_AUX_KEY = L_NVL6_KEY.
        WA_DADOS-NKEY = L_AUX_KEY.
        APPEND WA_DADOS TO IT_ALVTREE.
      ENDIF.
    ENDON.

    ON CHANGE OF WA_DADOS-NVL7.
      IF WA_DADOS-NVL7 GT 0.
        L_NODE_TEXT = WA_DADOS-DESNVL.
        CALL METHOD TREE1->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_NVL6_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
          IMPORTING
            E_NEW_NODE_KEY   = L_NVL7_KEY.
        L_AUX_KEY = L_NVL7_KEY.
        WA_DADOS-NKEY = L_AUX_KEY.
        APPEND WA_DADOS TO IT_ALVTREE.
      ENDIF.
    ENDON.

    ON CHANGE OF WA_DADOS-NVL8.
      IF WA_DADOS-NVL8 GT 0.
        L_NODE_TEXT = WA_DADOS-DESNVL.
        CALL METHOD TREE1->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_NVL7_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
          IMPORTING
            E_NEW_NODE_KEY   = L_NVL8_KEY.
        L_AUX_KEY = L_NVL8_KEY.
        WA_DADOS-NKEY = L_AUX_KEY.
        APPEND WA_DADOS TO IT_ALVTREE.
      ENDIF.
    ENDON.

    ON CHANGE OF WA_DADOS-NVL9.
      IF WA_DADOS-NVL9 GT 0.
        L_NODE_TEXT = WA_DADOS-DESNVL.
        CALL METHOD TREE1->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_NVL8_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
          IMPORTING
            E_NEW_NODE_KEY   = L_NVL9_KEY.
        L_AUX_KEY = L_NVL9_KEY.
        WA_DADOS-NKEY = L_AUX_KEY.
        APPEND WA_DADOS TO IT_ALVTREE.
      ENDIF.
    ENDON.

    ON CHANGE OF WA_DADOS-NVL10.
      IF WA_DADOS-NVL10 GT 0.
        L_NODE_TEXT = WA_DADOS-DESNVL.
        CALL METHOD TREE1->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_NVL9_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
          IMPORTING
            E_NEW_NODE_KEY   = L_NVL10_KEY.
        L_AUX_KEY = L_NVL10_KEY.
        WA_DADOS-NKEY = L_AUX_KEY.
        APPEND WA_DADOS TO IT_ALVTREE.
      ENDIF.
    ENDON.

    ON CHANGE OF WA_DADOS-SAKNR.
      IF WA_DADOS-TXT50 IS NOT INITIAL.
        PERFORM CREATE_ITEM_LAYOUTS CHANGING LT_LAYOUT_ITEM.
        CONCATENATE WA_DADOS-SAKNR+4 '-' WA_DADOS-TXT50 INTO TEXTO.
        L_NODE_TEXT = TEXTO.
        CALL METHOD TREE1->ADD_NODE
          EXPORTING
            I_RELAT_NODE_KEY = L_AUX_KEY
            I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
            I_NODE_TEXT      = L_NODE_TEXT
            IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
          IMPORTING
            E_NEW_NODE_KEY   = L_SAK_KEY.
        WA_DADOS-NKEY = L_SAK_KEY.
        APPEND WA_DADOS TO IT_ALVTREE.
      ENDIF.
    ENDON.

    IF WA_DADOS-LTEXT IS NOT INITIAL.
      PERFORM CREATE_ITEM_LAYOUTS CHANGING LT_LAYOUT_ITEM.
      WRITE WA_DADOS-LTEXT TO L_NODE_TEXT.
      CALL METHOD TREE1->ADD_NODE
        EXPORTING
          I_RELAT_NODE_KEY = L_SAK_KEY
          I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
          IS_OUTTAB_LINE   = WA_DADOS
          I_NODE_TEXT      = L_NODE_TEXT
          IT_ITEM_LAYOUT   = LT_LAYOUT_ITEM
        IMPORTING
          E_NEW_NODE_KEY   = L_TEXT_KEY.
      WA_DADOS-NKEY = L_TEXT_KEY.
      APPEND WA_DADOS TO IT_ALVTREE.
    ENDIF.

  ENDLOOP.

  CALL METHOD TREE1->UPDATE_CALCULATIONS.
  CALL METHOD TREE1->FRONTEND_UPDATE.
ENDFORM.                    " SELECT_DATA_AND_FILL_COL_TREE
*&---------------------------------------------------------------------*
*&      Form  CREATE_ITEM_LAYOUTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_LAYOUT_ITEM  text
*----------------------------------------------------------------------*
FORM CREATE_ITEM_LAYOUTS  CHANGING PT_ITEM_LAYOUT TYPE LVC_T_LAYI.
  DATA: LS_FIELDCATALOG TYPE LVC_S_FCAT,
        LS_ITEM_LAYOUT TYPE LVC_S_LAYI.

  CLEAR PT_ITEM_LAYOUT.
  LOOP AT IT_FIELDCATALOG INTO LS_FIELDCATALOG.
    CLEAR LS_ITEM_LAYOUT.
    IF LS_FIELDCATALOG-NO_OUT EQ SPACE.
      LS_ITEM_LAYOUT-FIELDNAME = LS_FIELDCATALOG-FIELDNAME.
      APPEND LS_ITEM_LAYOUT TO PT_ITEM_LAYOUT.
    ENDIF.

  ENDLOOP.

  CLEAR LS_ITEM_LAYOUT.
  LS_ITEM_LAYOUT-FIELDNAME = TREE1->C_HIERARCHY_COLUMN_NAME.
  APPEND LS_ITEM_LAYOUT TO PT_ITEM_LAYOUT.
ENDFORM.                    " CREATE_ITEM_LAYOUTS
*&---------------------------------------------------------------------*
*&      Form  F_HIERARCHY_HEADER_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
FORM F_HIERARCHY_HEADER_  CHANGING P_HIERARCHY_HEADER TYPE TREEV_HHDR.
*  data: vl_header        type treev_hhdr.

  P_HIERARCHY_HEADER-HEADING = 'Itens'.
  P_HIERARCHY_HEADER-TOOLTIP = 'Itens'.
  P_HIERARCHY_HEADER-WIDTH = 35.
  P_HIERARCHY_HEADER-WIDTH_PIX = ''.
ENDFORM.                    " F_HIERARCHY_HEADER_

*&---------------------------------------------------------------------*
*&      Form  F_TREE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_NODE_KEY  text
*----------------------------------------------------------------------*
FORM F_TREE_DOUBLE_CLICK  USING P_NODE_KEY TYPE LVC_NKEY.

  DATA: WA_ALVTREE       TYPE ZST_GL009_DADOS,
        LS_EXIT          TYPE SLIS_EXIT_BY_USER.

  READ TABLE IT_ALVTREE INTO WA_ALVTREE WITH KEY NKEY = P_NODE_KEY.
  IF SY-SUBRC = 0.

    CLEAR: IT_BDCDATA.

    IF WA_ALVTREE-SAKNR IS NOT INITIAL.
      IF ( WA_ALVTREE-KOSTL IS NOT INITIAL ). "Centro Custo KSB1
        PERFORM F_TREE_KSB1 USING WA_ALVTREE-KOSTL TXDATAINI TXDATAFIM.
      ELSEIF ( WA_ALVTREE-AUFNR IS NOT INITIAL ). "Ordem Interna KOB1
        PERFORM F_TREE_KOB1 USING WA_ALVTREE-AUFNR TXDATAINI TXDATAFIM.
      ELSEIF ( WA_ALVTREE-PRCTR IS NOT INITIAL ). "Centro de lucro KE5Z
        PERFORM F_TREE_KE5Z USING WA_ALVTREE-PRCTR WA_ALVTREE-SAKNR WA_DRE_005-BUKRS WA_DRE_005-MONAT WA_DRE_005-GJAHR.
      ELSE.
        PERFORM F_TREE_FAGLB03 USING WA_ALVTREE-SAKNR WA_DRE_005-BUKRS WA_DRE_005-GJAHR.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_TREE_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  F_INSERT_SHDB
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM F_INSERT_SHDB  USING VALUE(P_CODE)
                          VALUE(P_FNAM)
                          VALUE(P_FVAL).

  CLEAR WA_BDCDATA.

  WA_BDCDATA-DYNBEGIN = P_CODE.

  IF (  P_CODE EQ C_MARK  ).
    WA_BDCDATA-PROGRAM  = P_FNAM.
    WA_BDCDATA-DYNPRO   = P_FVAL.
  ELSE.
    WA_BDCDATA-FNAM     = P_FNAM.
    WA_BDCDATA-FVAL     = P_FVAL.
  ENDIF.

  APPEND WA_BDCDATA TO IT_BDCDATA.

ENDFORM.                    "f_insert_shdb

*&---------------------------------------------------------------------*
*&      Form  F_TREE_KE5Z
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ALVTREE  text
*      -->P_WA_DRE_005_BUKRS  text
*      -->P_WA_DRE_005_MONAT  text
*      -->P_WA_DRE_005_GJAHR  text
*----------------------------------------------------------------------*
FORM F_TREE_KE5Z  USING    P_WA_ALVTREE_PRCTR
                           P_WA_ALVTREE_SAKNR
                           P_WA_DRE_005_BUKRS
                           P_WA_DRE_005_MONAT
                           P_WA_DRE_005_GJAHR.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      TCODE  = 'KE5Z'
    EXCEPTIONS
      OK     = 1
      NOT_OK = 2.
  IF SY-SUBRC = 2.
    MESSAGE E077(S#) WITH 'KE5Z'.
  ENDIF.


  PERFORM F_INSERT_SHDB USING: 'X' 'RCOPCA02'	'1000',
                               ' ' 'BDC_CURSOR' 'P_VARI',
                               ' ' 'BDC_OKCODE' '=ONLI',
                               ' ' 'RRCTY-LOW'  '0',
                               ' ' 'RVERS-LOW'  '0',
                               ' ' 'KOKRS-LOW'  'MAGI',
                               ' ' 'BUKRS-LOW'  P_WA_DRE_005_BUKRS,
                               ' ' 'POPER-LOW'  P_WA_DRE_005_MONAT,
                               ' ' 'RYEAR-LOW'  P_WA_DRE_005_GJAHR,
                               ' ' 'PRCTR-LOW'  P_WA_ALVTREE_PRCTR,
                               ' ' 'RACCT-LOW'  P_WA_ALVTREE_SAKNR,
                               ' ' 'P_VARI'	'/DRE_CL',
                               'X' 'RCOPCA02'	'1000',
                               ' ' 'BDC_OKCODE'	'/EENDE',
                               ' ' 'BDC_CURSOR'	'RRCTY-LOW'.

  CALL TRANSACTION 'KE5Z' USING IT_BDCDATA
                           MODE 'E'
                       MESSAGES INTO IT_MESSAGE.

ENDFORM.                    " F_TREE_KE5Z

*&---------------------------------------------------------------------*
*&      Form  F_TREE_FAGLB03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ALVTREE_SAKNR  text
*      -->P_WA_DRE_005_BUKRS  text
*      -->P_WA_DRE_005_GJAHR  text
*----------------------------------------------------------------------*
FORM F_TREE_FAGLB03  USING    P_WA_ALVTREE_SAKNR
                              P_WA_DRE_005_BUKRS
                              P_WA_DRE_005_GJAHR.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      TCODE  = 'FAGLB03'
    EXCEPTIONS
      OK     = 1
      NOT_OK = 2.
  IF SY-SUBRC = 2.
    MESSAGE E077(S#) WITH 'FAGLB03'.
  ENDIF.
  SET PARAMETER ID: 'ACC' FIELD P_WA_ALVTREE_SAKNR,
                    'BUK' FIELD P_WA_DRE_005_BUKRS,
                    'GJR' FIELD P_WA_DRE_005_GJAHR.
  CALL TRANSACTION 'FAGLB03' AND SKIP FIRST SCREEN.

ENDFORM.                    " F_TREE_FAGLB03

*&---------------------------------------------------------------------*
*&      Form  F_TREE_KOB1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ALVTREE_AUFNR  text
*      -->P_TXDATAINI  text
*      -->P_TXDATAFIM  text
*----------------------------------------------------------------------*
FORM F_TREE_KOB1  USING    P_WA_ALVTREE_AUFNR
                           P_TXDATAINI
                           P_TXDATAFIM.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      TCODE  = 'KOB1'
    EXCEPTIONS
      OK     = 1
      NOT_OK = 2.
  IF SY-SUBRC = 2.
    MESSAGE E077(S#) WITH 'KOB1'.
  ENDIF.

  PERFORM F_INSERT_SHDB USING: 'X' 'SAPLSPO4'	'0300',
                               ' ' 'BDC_CURSOR'	'SVALD-VALUE(01)',
                               ' ' 'BDC_OKCODE'	'=FURT',
                               ' ' 'SVALD-VALUE(01)'  'MAGI',
                               'X' 'RKAEP000'	'0110',
                               ' ' 'BDC_CURSOR'	'P_DISVAR',
                               ' ' 'BDC_OKCODE'	'=ONLI',
                               ' ' 'AUFNR-LOW' P_WA_ALVTREE_AUFNR,
                               ' ' 'R_BUDAT-LOW' P_TXDATAINI,
                               ' ' 'R_BUDAT-HIGH'	P_TXDATAFIM,
                               ' ' 'P_DISVAR'	'/B_ORDEM06',
                               'X' 'SAPLSLVC_FULLSCREEN' '0500',
                               ' ' 'BDC_OKCODE'	'=&F12',
                               'X' 'SAPLSPO1'	'0100',
                               ' ' 'BDC_OKCODE'	'=YES',
                               'X' 'RKAEP000'	'0110',
                               ' ' 'BDC_OKCODE'	'/EENDE',
                               ' ' 'BDC_CURSOR'	'AUFNR-LOW'.

  CALL TRANSACTION 'KOB1' USING IT_BDCDATA
                           MODE 'E'
                       MESSAGES INTO IT_MESSAGE.

ENDFORM.                    " F_TREE_KOB1

*&---------------------------------------------------------------------*
*&      Form  F_TREE_KSB1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ALVTREE_KOSTL  text
*      -->P_TXDATAINI  text
*      -->P_TXDATAFIM  text
*----------------------------------------------------------------------*
FORM F_TREE_KSB1  USING    P_WA_ALVTREE_KOSTL
                           P_TXDATAINI
                           P_TXDATAFIM.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      TCODE  = 'KSB1'
    EXCEPTIONS
      OK     = 1
      NOT_OK = 2.
  IF SY-SUBRC = 2.
    MESSAGE E077(S#) WITH 'KSB1'.
  ENDIF.


  PERFORM F_INSERT_SHDB USING: 'X' 'SAPLSPO4'	'0300',
                               ' ' 'BDC_CURSOR'	'SVALD-VALUE(01)',
                               ' ' 'BDC_OKCODE'	'=FURT',
                               ' ' 'SVALD-VALUE(01)'  'MAGI',
                               'X' 'RKAEP000'	'0100',
                               ' ' 'BDC_OKCODE'	'/EE'.

  CALL TRANSACTION 'KSB1' USING IT_BDCDATA
                           MODE 'N'
                       MESSAGES INTO IT_MESSAGE.

  CLEAR IT_BDCDATA.

  PERFORM F_INSERT_SHDB USING: 'X' 'RKAEP000'  '0100',
                               ' ' 'BDC_CURSOR'  'P_DISVAR',
                               ' ' 'KOSTL-LOW' P_WA_ALVTREE_KOSTL,
                               ' ' 'R_BUDAT-LOW'  P_TXDATAINI,
                               ' ' 'R_BUDAT-HIGH'	P_TXDATAFIM,
                               ' ' 'P_DISVAR'  '/B_CUSTOS3',
                               ' ' 'BDC_OKCODE'  '=ONLI',
                               'X' 'RKAEP000'  '0100',
                               ' ' 'BDC_OKCODE'  '/EENDE'.

  CALL TRANSACTION 'KSB1' USING IT_BDCDATA
                           MODE 'E'
                       MESSAGES INTO IT_MESSAGE.

ENDFORM.                    " F_TREE_KSB1
