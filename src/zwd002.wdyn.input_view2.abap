METHOD BUILD_SELECT_OPTIONS .
  TYPES:
  TY_R_VKORG TYPE RANGE OF VKORG,
  TY_R_VTWEG TYPE RANGE OF VTWEG,
  TY_R_SPART TYPE RANGE OF SPART,
  TY_R_VKBUR TYPE RANGE OF VKBUR,
  TY_R_KUNNR TYPE RANGE OF KUNNR,
  TY_R_TP_VENDA TYPE RANGE OF ZSDED012,
  TY_S_VKORG    TYPE LINE OF TY_R_VKORG,
  TY_S_VTWEG    TYPE LINE OF TY_R_VTWEG,
  TY_S_SPART    TYPE LINE OF TY_R_SPART,
  TY_S_VKBUR    TYPE LINE OF TY_R_VKBUR,
  TY_S_KUNNR    TYPE LINE OF TY_R_KUNNR,
  TY_S_TP_VENDA TYPE LINE OF TY_R_TP_VENDA.
** Reference variable used instantiate the select-options component
  DATA
    LR_CMP_USAGE TYPE REF TO IF_WD_COMPONENT_USAGE.
* Variables used to create the select-options fields and
* define its initial values
  DATA:
    LR_FIELD TYPE REF TO DATA,
    LS_VKORG  TYPE TY_S_VKORG,
    LS_VTWEG  TYPE TY_S_VTWEG,
    LS_SPART  TYPE TY_S_SPART,
    LS_VKBUR  TYPE TY_S_VKBUR,
    LS_KUNNR  TYPE TY_S_KUNNR,
    LS_TP_VENDA  TYPE TY_S_TP_VENDA.

  FIELD-SYMBOLS:
    <FS_FIELD> TYPE ANY,
    <FS_RANGE> TYPE INDEX TABLE.


* Instantiate the select-options component
  LR_CMP_USAGE = WD_THIS->WD_CPUSE_CMP_SEL_OPT( ).
  IF LR_CMP_USAGE->HAS_ACTIVE_COMPONENT( ) IS INITIAL.
    LR_CMP_USAGE->CREATE_COMPONENT( ).
  ENDIF.
* Sets the helper reference
  WD_THIS->M_SEL_OPT = WD_THIS->WD_CPIFC_CMP_SEL_OPT( ).
  WD_THIS->M_HELPER  = WD_THIS->M_SEL_OPT->INIT_SELECTION_SCREEN( ).

* Hide the standard select-options components.
  WD_THIS->M_HELPER->SET_GLOBAL_OPTIONS(
    I_DISPLAY_BTN_CANCEL  = ABAP_FALSE
    I_DISPLAY_BTN_CHECK   = ABAP_FALSE
    I_DISPLAY_BTN_EXECUTE = ABAP_FALSE
    I_DISPLAY_BTN_RESET   = ABAP_FALSE ).



* Adding a block (type Tray) to the select-options
  WD_THIS->M_HELPER->ADD_BLOCK(
    I_ID         = `BL01`
    I_WIDTH      = `500px`
*    I_BLOCK_TYPE = IF_WD_SELECT_OPTIONS=>MC_BLOCK_TYPE_TRAY
    I_TITLE      = `Solicitação de Ordem de Venda` ).

* Adding a parameter field to the created block
* Create a reference to the type of airline code
*  CREATE DATA lr_field TYPE s_carr_id.
** Sets the airline code initial value
*  ASSIGN lr_field->* TO <fs_field>.
*  <fs_field> = 'AA '.
** Add the parameter to the group
*  wd_this->m_helper->add_parameter_field(
*    i_id           = `CARRID`
*    i_within_block = `BL01`
*    i_value        = lr_field ).
*  FREE lr_field.
*  UNASSIGN <fs_field>.


* Adding a select-options field to the created block
* Create a reference to the connection number range table
  LR_FIELD = WD_THIS->M_HELPER->CREATE_RANGE_TABLE( `VKORG` ).
* Add the select-option to the group
  WD_THIS->M_HELPER->ADD_SELECTION_FIELD(
    I_ID           = `VKORG`
    I_WITHIN_BLOCK = `BL01`
    IT_RESULT      = LR_FIELD ).
  FREE LR_FIELD.

  LR_FIELD = WD_THIS->M_HELPER->CREATE_RANGE_TABLE( `VTWEG` ).
  WD_THIS->M_HELPER->ADD_SELECTION_FIELD(
    I_ID           = `VTWEG`
    I_WITHIN_BLOCK = `BL01`
    IT_RESULT      = LR_FIELD ).
  FREE LR_FIELD.

  LR_FIELD = WD_THIS->M_HELPER->CREATE_RANGE_TABLE( `SPART` ).
  WD_THIS->M_HELPER->ADD_SELECTION_FIELD(
    I_ID           = `SPART`
    I_WITHIN_BLOCK = `BL01`
    IT_RESULT      = LR_FIELD ).
  FREE LR_FIELD.

  LR_FIELD = WD_THIS->M_HELPER->CREATE_RANGE_TABLE( `KUNNR` ).
  WD_THIS->M_HELPER->ADD_SELECTION_FIELD(
    I_ID           = `KUNNR`
    I_WITHIN_BLOCK = `BL01`
    IT_RESULT      = LR_FIELD ).
  FREE LR_FIELD.

  LR_FIELD = WD_THIS->M_HELPER->CREATE_RANGE_TABLE( `ZSDED012` ).
  WD_THIS->M_HELPER->ADD_SELECTION_FIELD(
    I_ID           = `TP_VENDA`
    I_WITHIN_BLOCK = `BL01`
    IT_RESULT      = LR_FIELD ).
  FREE LR_FIELD.

*  DATA: LR_RADIO TYPE REF TO CL_WD_RADIOBUTTON.
*  DATA: LR_CONTAINR TYPE REF TO CL_WD_TRANSPARENT_CONTAINER.
*  DATA: LR_DATA TYPE REF TO CL_WD_FLOW_DATA.
*
**   lr_containr ?= view->get_element( 'ROOTUIELEMENTCONTAINER' ).
*  LR_RADIO = CL_WD_RADIOBUTTON=>NEW_RADIOBUTTON(
*  ID = 'RADIO'
*  TEXT = 'teste'
*  BIND_SELECTED_KEY = 'RB_G1.RB_TP_EXEC'
*  KEY_TO_SELECT = 'RB_G1.RB_TP_EXEC' ).

*    lr_data = cl_wd_flow_data=>new_flow_data( element = lr_radio ).
*    lr_radio->set_layout_data( lr_data ).
*    lr_containr->add_child( lr_radio ).

** Adding a select-options field to the created block
** Create a reference to the flight date range table
*  lr_field = wd_this->m_helper->create_range_table( `S_DATE` ).
*  ASSIGN lr_field->* TO <fs_range>.
*  ls_date-sign   = 'I'.
*  ls_date-option = 'EQ'.
*  ls_date-low    = sy-datum - 7.
*  ls_date-high   = sy-datum.
*  APPEND ls_date TO <fs_range>.
** Add the select-option to the group
*  wd_this->m_helper->add_selection_field(
*    i_id           = `FLDATE`
*    i_within_block = `BL01`
*    it_result      = lr_field ).
endmethod.

METHOD onactionaction_find .
*
*

********  Radio button ******
*  DATA : V_ELEMENT TYPE REF TO IF_WD_CONTEXT_ELEMENT,
*         ITEMS_NODE TYPE REF TO IF_WD_CONTEXT_NODE,
*         V_INDEX TYPE I,
*         V_TEXT TYPE STRING,
*         ITEMLIST TYPE STANDARD TABLE OF IF_INPUT_VIEW=>ELEMENT_RB_G1,
*         ITEMLIST2 TYPE STANDARD TABLE OF IF_INPUT_VIEW=>ELEMENT_RB_G2,
*         W_LIST LIKE LINE OF ITEMLIST,
*         W_LIST2 LIKE LINE OF ITEMLIST2.
************  end radio button
*
*
*  DATA: NODE_NODE_SOL           TYPE REF TO IF_WD_CONTEXT_NODE,
*    ELEM_NODE_SOL                      TYPE REF TO IF_WD_CONTEXT_ELEMENT,
*    STRU_NODE_SOL                        TYPE IF_INPUT_VIEW=>ELEMENT_NODE_SOL .
*
** navigate from <CONTEXT> to <NODE_VBAK> via lead selection
*  NODE_NODE_SOL = WD_CONTEXT->GET_CHILD_NODE( NAME = IF_INPUT_VIEW=>WDCTX_NODE_SOL ).
*
** get element via lead selection
*  ELEM_NODE_SOL = NODE_NODE_SOL->GET_ELEMENT(  ).
*
** get all declared attributes
*  ELEM_NODE_SOL->GET_STATIC_ATTRIBUTES(
*    IMPORTING
*      STATIC_ATTRIBUTES = STRU_NODE_SOL ).
*
*
*  DATA: LS_WHERE(72) TYPE C,
*        LT_WHERE LIKE TABLE OF LS_WHERE,
*        WL_COCKPIT TYPE ZSDS009,
*        TL_COCKPIT TYPE STANDARD TABLE OF ZSDS009.
*
*
*  DATA: RG_VKORG TYPE TABLE OF BAPI_RANGESVKORG ,
*        WA_VKORG LIKE LINE OF RG_VKORG,
*        RG_VTWEG TYPE TABLE OF BAPI_RANGESVTWEG ,
*        WA_VTWEG LIKE LINE OF RG_VTWEG,
*        RG_SPART TYPE TABLE OF BAPI_RANGESSPART ,
*        WA_SPART LIKE LINE OF RG_SPART,
*        RG_KUNNR TYPE TABLE OF BAPI_RANGESKUNNR ,
*        WA_KUNNR LIKE LINE OF RG_KUNNR,
*        RG_TP_VENDA TYPE TABLE OF ZBAPI_RANGESTP_VENDA,
*        WA_TP_VENDA LIKE LINE OF RG_TP_VENDA,
*        WL_GERA,
*        WL_CONS,
*        WL_FORM,
*        WL_VENDA.
*
*
** create where condition
**  IF NOT stru_node_sol-vkorg EQ ''.
**    CONCATENATE 'VKORG = ''' stru_node_SOL-VKORG '''' INTO ls_where.
**    APPEND ls_where TO lt_where.
**  ENDIF.
**  IF NOT stru_node_vbak-erdat EQ '00000000'.
**    CONCATENATE 'ERDAT = ''' stru_node_vbak-erdat '''' INTO ls_where.
**    IF stru_node_vbak-vbeln NE ''.
**      CONCATENATE 'AND' ls_where INTO ls_where SEPARATED BY space.
**    ENDIF.
**    APPEND ls_where TO lt_where.
**  ENDIF.
*
*  IF STRU_NODE_SOL-VKORG IS NOT INITIAL.
*    WA_VKORG-SIGN = 'I'.
*    WA_VKORG-OPTION = 'EQ'.
*    WA_VKORG-LOW = STRU_NODE_SOL-VKORG.
*    APPEND WA_VKORG TO RG_VKORG.
*    CLEAR: WA_VKORG.
*  ENDIF.
*  BREAK-POINT.
*  BREAK ABAP.
*
*  IF STRU_NODE_SOL-VTWEG IS NOT INITIAL.
*    WA_VTWEG-SIGN = 'I'.
*    WA_VTWEG-OPTION = 'EQ'.
*    WA_VTWEG-LOW = STRU_NODE_SOL-VTWEG.
*    APPEND WA_VTWEG TO RG_VTWEG.
*    CLEAR: WA_VTWEG.
*  ENDIF.
*
*  IF STRU_NODE_SOL-SPART IS NOT INITIAL.
*    WA_SPART-SIGN = 'I'.
*    WA_SPART-OPTION = 'EQ'.
*    WA_SPART-LOW = STRU_NODE_SOL-SPART.
*    APPEND WA_SPART TO RG_SPART.
*    CLEAR: WA_SPART.
*  ENDIF.
*
*  IF STRU_NODE_SOL-KUNNR IS NOT INITIAL.
*    WA_KUNNR-SIGN = 'I'.
*    WA_KUNNR-OPTION = 'EQ'.
*    WA_KUNNR-LOW = STRU_NODE_SOL-KUNNR.
*    APPEND WA_KUNNR TO RG_KUNNR.
*    CLEAR: WA_KUNNR.
*  ENDIF.
*
*  IF STRU_NODE_SOL-TP_VENDA IS NOT INITIAL.
*    WA_TP_VENDA-SIGN = 'I'.
*    WA_TP_VENDA-OPTION = 'EQ'.
*    WA_TP_VENDA-LOW = STRU_NODE_SOL-TP_VENDA.
*    APPEND WA_TP_VENDA TO RG_TP_VENDA.
*    CLEAR: WA_TP_VENDA.
*  ENDIF.
*
**  SELECT VBELN ERDAT ERZET ERNAM ANGDT BNDDT AUDAT VBTYP TRVOG AUART
**         AUGRU GWLDT SUBMI LIFSK FAKSK NETWR WAERK VKORG VTWEG SPART
**         VKGRP VKBUR GSBER GSKST GUEBG GUEEN KNUMV
**  FROM VBAK INTO TABLE LT_VBAK WHERE (LT_WHERE).
*
*  ITEMS_NODE = WD_CONTEXT->GET_CHILD_NODE( NAME = `RB_G1` ).
*  ITEMS_NODE->GET_STATIC_ATTRIBUTES_TABLE( IMPORTING TABLE = ITEMLIST ).
*  V_INDEX = ITEMS_NODE->GET_LEAD_SELECTION_INDEX( ).
*  CLEAR: W_LIST, WL_GERA, WL_CONS.
*  READ TABLE ITEMLIST INTO W_LIST INDEX V_INDEX.
*  IF W_LIST-RB_TP_EXEC EQ 'Gera OV'.
*    WL_GERA = 'X'.
*
*
*  ELSEIF W_LIST-RB_TP_EXEC = 'Consultar OV Geradas'.
*    WL_CONS = 'X'.
*  ENDIF.
**  WD_THIS->FIRE_OUT_RB_1_PLG(
**       RB_G1 = W_LIST-RB_TP_EXEC
**  ).
*
*  ITEMS_NODE = WD_CONTEXT->GET_CHILD_NODE( NAME = `RB_G2` ).
*  ITEMS_NODE->GET_STATIC_ATTRIBUTES_TABLE( IMPORTING TABLE = ITEMLIST2 ).
*  V_INDEX = ITEMS_NODE->GET_LEAD_SELECTION_INDEX( ).
*  CLEAR: W_LIST2, WL_FORM, WL_VENDA.
*  READ TABLE ITEMLIST2 INTO W_LIST2 INDEX V_INDEX.
*  IF W_LIST2-RB_TP_SOL EQ 'Formação de Lote'.
*    WL_FORM = 'X'.
*  ELSEIF W_LIST2-RB_TP_SOL = 'Venda Normal'.
*    WL_VENDA = 'X'.
*  ENDIF.
**  WD_THIS->FIRE_OUT_RB_2_PLG(
**         RB_G2 = W_LIST-RB_TP_EXEC
**    ).
*
**      WD_THIS->FIRE_IN_RB_PLG(
**      RB_G1 =     W_LIST-RB_TP_EXEC                        " char20
**      RB_G2 =     W_LIST2-RB_TP_SOL                        " char20
**    ).
*
*  CALL FUNCTION 'ZSDMF006_BUSCA_DADOS_NR_SOL'
*    EXPORTING
*      P_GERA      = WL_GERA
*      P_CONS      = WL_CONS
*      P_FORM      = WL_FORM
*      P_VENDA     = WL_VENDA
*    TABLES
*      RG_VKORG    = RG_VKORG
*      RG_VTWEG    = RG_VTWEG
*      RG_SPART    = RG_SPART
*      RG_KUNNR    = RG_KUNNR
*      RG_TP_VENDA = RG_TP_VENDA
*      ET_COCKPIT  = TL_COCKPIT.
*
*
*
*
**  LOOP AT TL_COCKPIT INTO WL_COCKPIT.
**    IF SY-TABIX EQ 1.
**      WL_COCKPIT-STATUS = 'ICON_RED_LIGHT'.
**    ELSE.
**
**      WL_COCKPIT-STATUS = 'ICON_YELLOW_LIGHT'.
**    ENDIF.
**    MODIFY TL_COCKPIT FROM WL_COCKPIT.
**  ENDLOOP.
*  DATA:
*    NODE_NODE_ALV                       TYPE REF TO IF_WD_CONTEXT_NODE,
*    STRU_NODE_ALV                       TYPE IF_INPUT_VIEW=>ELEMENT_NODE_ALV .
*
** navigate from <CONTEXT> to <NODE_ALV> via lead selection
*  NODE_NODE_ALV = WD_CONTEXT->GET_CHILD_NODE( NAME = IF_INPUT_VIEW=>WDCTX_NODE_ALV ).
*
*
** get all declared attributes
*  NODE_NODE_ALV->BIND_TABLE( TL_COCKPIT ).

*  WD_COMP_CONTROLLER->ONFIND( ).
*  METHOD onfind .
*******  Radio button ******
  DATA : v_element TYPE REF TO if_wd_context_element,
         items_node TYPE REF TO if_wd_context_node,
         v_index TYPE i,
         v_text TYPE string,
         itemlist TYPE STANDARD TABLE OF if_input_view2=>element_rb_g1,
         itemlist2 TYPE STANDARD TABLE OF if_input_view2=>element_rb_g2,
         w_list LIKE LINE OF itemlist,
         w_list2 LIKE LINE OF itemlist2.
***********  end radio button

  TYPES:lty_r_vkorg TYPE RANGE OF vkorg,
        lty_r_vtweg TYPE RANGE OF vtweg,
        lty_r_spart TYPE RANGE OF spart,
        lty_r_vkbur TYPE RANGE OF vkbur,
        lty_r_kunnr TYPE RANGE OF kunnr,
        lty_r_tp_venda TYPE RANGE OF zsded012.

* Variables used to retrieve the values of select-options fields
  DATA
    lt_sel_item TYPE if_wd_select_options=>tt_selection_screen_item.

  FIELD-SYMBOLS:
    <fs_sel_item> LIKE LINE OF lt_sel_item,
    <fs_vkorg>   TYPE lty_r_vkorg,
    <fs_vtweg>   TYPE lty_r_vtweg,
    <fs_spart>   TYPE lty_r_spart,
*    <fs_vkbur>   TYPE lty_r_vkbur,
    <fs_kunnr>   TYPE lty_r_kunnr,
    <fs_tp_venda>   TYPE lty_r_tp_venda.


*** Reference variable used instantiate the select-options component
*  DATA
*    LR_CMP_USAGE TYPE REF TO IF_WD_COMPONENT_USAGE.
*
** Instantiate the select-options component
*  LR_CMP_USAGE = WD_THIS->WD_CPUSE_CMP_SEL_OPT( ).
*  IF LR_CMP_USAGE->HAS_ACTIVE_COMPONENT( ) IS INITIAL.
*    LR_CMP_USAGE->CREATE_COMPONENT( ).
*  ENDIF.

** Sets the helper reference
*  WD_THIS->M_SEL_OPT = WD_THIS->WD_CPIFC_CMP_SEL_OPT( ).
*  WD_THIS->M_HELPER  = WD_THIS->M_SEL_OPT->INIT_SELECTION_SCREEN( ).

* Get the selection-screen items
  wd_this->m_helper->get_selection_screen_items(
    IMPORTING et_selection_screen_items = lt_sel_item ).

*  DATA: items_node TYPE REF TO if_wd_context_node,
*        itemlist1 TYPE STANDARD TABLE OF if_input_view2=>element_rg_VKORG,
*        itemlist2 TYPE STANDARD TABLE OF if_input_view2=>element_rg_VTWEG,
*        itemlist3 TYPE STANDARD TABLE OF if_input_view2=>element_rg_SPART,
*        itemlist4 TYPE STANDARD TABLE OF if_input_view2=>ELEMENT_RG_KUNNR,
*        itemlist5 TYPE STANDARD TABLE OF if_input_view2=>element_rg_TP_VENDA,
*        w_list  LIKE LINE OF itemlist.

* Retrieve the values from the select-options items

  LOOP AT lt_sel_item ASSIGNING <fs_sel_item>.
    CASE <fs_sel_item>-m_id.
      WHEN `VKORG`.
        ASSIGN <fs_sel_item>-mt_range_table->* TO <fs_vkorg>.
      WHEN `VTWEG`.
        ASSIGN <fs_sel_item>-mt_range_table->* TO <fs_vtweg>.
      WHEN `SPART`.
        ASSIGN <fs_sel_item>-mt_range_table->* TO <fs_spart>.
      WHEN `KUNNR`.
        ASSIGN <fs_sel_item>-mt_range_table->* TO <fs_kunnr>.
      WHEN `TP_VENDA`.
        ASSIGN <fs_sel_item>-mt_range_table->* TO <fs_tp_venda>.
    ENDCASE.
  ENDLOOP.
*  LOOP AT <fs_kunnr> INTO W_LIST
*        W_LIST-sign = <fs_kunnr>-sign.
*         W_LIST-option = <fs_kunnr>-option.
*         W_LIST-low = <fs_kunnr>-low.
*         W_LIST-high = <fs_kunnr>-high.
*         APPEND W_LIST TO ITEMLIST.

*ITEMLIST[] =
*  items_node = wd_context->get_child_node( name = `NODE_SOL` ).
  items_node = wd_context->get_child_node( name = `RG_VKORG` ).
  items_node->bind_table( <fs_vkorg> ).

*  items_node = wd_context->get_child_node( name = `NODE_SOL` ).
  items_node = wd_context->get_child_node( name = `RG_VTWEG` ).
  items_node->bind_table( <fs_vtweg> ).

*  items_node = wd_context->get_child_node( name = `NODE_SOL` ).
  items_node = wd_context->get_child_node( name = `RG_SPART` ).
  items_node->bind_table( <fs_spart> ).

*  items_node = wd_context->get_child_node( name = `NODE_SOL` ).
  items_node = wd_context->get_child_node( name = `RG_KUNNR` ).
  items_node->bind_table( <fs_kunnr> ).

*  items_node = wd_context->get_child_node( name = `NODE_SOL` ).
  items_node = wd_context->get_child_node( name = `RG_TP_VENDA` ).
  items_node->bind_table( <fs_tp_venda> ).

  DATA: node_node_sol           TYPE REF TO if_wd_context_node,
    elem_node_sol                      TYPE REF TO if_wd_context_element,
    stru_node_sol                        TYPE if_input_view=>element_node_sol .

* navigate from <CONTEXT> to <NODE_VBAK> via lead selection
  node_node_sol = wd_context->get_child_node( name = if_input_view=>wdctx_node_sol ).

* get element via lead selection
  elem_node_sol = node_node_sol->get_element(  ).

* get all declared attributes
  elem_node_sol->get_static_attributes(
    IMPORTING
      static_attributes = stru_node_sol ).


  DATA: ls_where(72) TYPE c,
        lt_where LIKE TABLE OF ls_where,
        wl_cockpit TYPE zsds009,
        tl_cockpit TYPE STANDARD TABLE OF zsds009.


  DATA:
* ---> S4 Migration - 06/07/2023 - FC
*        rg_vkorg TYPE TABLE OF bapi_rangesvkorg ,
*        rg_vtweg TYPE TABLE OF bapi_rangesvtweg ,
*        rg_spart TYPE TABLE OF bapi_rangesspart ,
*        rg_kunnr TYPE TABLE OF bapi_rangeskunnr ,
        rg_vkorg TYPE RANGE OF vkorg,
        rg_vtweg TYPE RANGE OF vtweg,
        rg_spart TYPE RANGE OF spart,
        rg_kunnr TYPE RANGE OF kunnr,
* <--- S4 Migration - 06/07/2023 - FC

        wa_vkorg LIKE LINE OF rg_vkorg,
        wa_vtweg LIKE LINE OF rg_vtweg,
        wa_spart LIKE LINE OF rg_spart,
        wa_kunnr LIKE LINE OF rg_kunnr,
        rg_tp_venda TYPE TABLE OF zbapi_rangestp_venda,
        wa_tp_venda LIKE LINE OF rg_tp_venda,
        wl_gera,
        wl_cons,
        wl_form,
        wl_venda.


* create where condition
*  IF NOT stru_node_sol-vkorg EQ ''.
*    CONCATENATE 'VKORG = ''' stru_node_SOL-VKORG '''' INTO ls_where.
*    APPEND ls_where TO lt_where.
*  ENDIF.
*  IF NOT stru_node_vbak-erdat EQ '00000000'.
*    CONCATENATE 'ERDAT = ''' stru_node_vbak-erdat '''' INTO ls_where.
*    IF stru_node_vbak-vbeln NE ''.
*      CONCATENATE 'AND' ls_where INTO ls_where SEPARATED BY space.
*    ENDIF.
*    APPEND ls_where TO lt_where.
*  ENDIF.

  IF stru_node_sol-vkorg IS NOT INITIAL.
    wa_vkorg-sign = 'I'.
    wa_vkorg-option = 'EQ'.
    wa_vkorg-low = stru_node_sol-vkorg.
    APPEND wa_vkorg TO rg_vkorg.
    CLEAR: wa_vkorg.
  ENDIF.
  BREAK-POINT.
  break abap.

  IF stru_node_sol-vtweg IS NOT INITIAL.
    wa_vtweg-sign = 'I'.
    wa_vtweg-option = 'EQ'.
    wa_vtweg-low = stru_node_sol-vtweg.
    APPEND wa_vtweg TO rg_vtweg.
    CLEAR: wa_vtweg.
  ENDIF.

  IF stru_node_sol-spart IS NOT INITIAL.
    wa_spart-sign = 'I'.
    wa_spart-option = 'EQ'.
    wa_spart-low = stru_node_sol-spart.
    APPEND wa_spart TO rg_spart.
    CLEAR: wa_spart.
  ENDIF.

  IF stru_node_sol-kunnr IS NOT INITIAL.
    wa_kunnr-sign = 'I'.
    wa_kunnr-option = 'EQ'.
    wa_kunnr-low = stru_node_sol-kunnr.
    APPEND wa_kunnr TO rg_kunnr.
    CLEAR: wa_kunnr.
  ENDIF.

  IF stru_node_sol-tp_venda IS NOT INITIAL.
    wa_tp_venda-sign = 'I'.
    wa_tp_venda-option = 'EQ'.
    wa_tp_venda-low = stru_node_sol-tp_venda.
    APPEND wa_tp_venda TO rg_tp_venda.
    CLEAR: wa_tp_venda.
  ENDIF.

*  SELECT VBELN ERDAT ERZET ERNAM ANGDT BNDDT AUDAT VBTYP TRVOG AUART
*         AUGRU GWLDT SUBMI LIFSK FAKSK NETWR WAERK VKORG VTWEG SPART
*         VKGRP VKBUR GSBER GSKST GUEBG GUEEN KNUMV
*  FROM VBAK INTO TABLE LT_VBAK WHERE (LT_WHERE).

  items_node = wd_context->get_child_node( name = `RB_G1` ).
  items_node->get_static_attributes_table( IMPORTING table = itemlist ).
  v_index = items_node->get_lead_selection_index( ).
  CLEAR: w_list, wl_gera, wl_cons.
  READ TABLE itemlist INTO w_list INDEX v_index.
  IF w_list-rb_tp_exec EQ 'Gera OV'.
    wl_gera = 'X'.


  ELSEIF w_list-rb_tp_exec = 'Consultar OV Geradas'.
    wl_cons = 'X'.
  ENDIF.
*  WD_THIS->FIRE_OUT_RB_1_PLG(
*       RB_G1 = W_LIST-RB_TP_EXEC
*  ).

  items_node = wd_context->get_child_node( name = `RB_G2` ).
  items_node->get_static_attributes_table( IMPORTING table = itemlist2 ).
  v_index = items_node->get_lead_selection_index( ).
  CLEAR: w_list2, wl_form, wl_venda.
  READ TABLE itemlist2 INTO w_list2 INDEX v_index.
  IF w_list2-rb_tp_sol EQ 'Formação de Lote'.
    wl_form = 'X'.
  ELSEIF w_list2-rb_tp_sol = 'Venda Normal'.
    wl_venda = 'X'.
  ENDIF.
*  WD_THIS->FIRE_OUT_RB_2_PLG(
*         RB_G2 = W_LIST-RB_TP_EXEC
*    ).

*      WD_THIS->FIRE_IN_RB_PLG(
*      RB_G1 =     W_LIST-RB_TP_EXEC                        " char20
*      RB_G2 =     W_LIST2-RB_TP_SOL                        " char20
*    ).

  CALL FUNCTION 'ZSDMF006_BUSCA_DADOS_NR_SOL'
    EXPORTING
      p_gera      = wl_gera
      p_cons      = wl_cons
      p_form      = wl_form
      p_venda     = wl_venda
    TABLES
      rg_vkorg    = <fs_vkorg> "rg_vkorg
      rg_vtweg    = <fs_vtweg> "rg_vtweg
      rg_spart    = <fs_spart> "rg_spart
      rg_kunnr    = <fs_kunnr> "rg_kunnr
      rg_tp_venda = <fs_tp_venda> "rg_tp_venda
      et_cockpit  = tl_cockpit.




*  LOOP AT TL_COCKPIT INTO WL_COCKPIT.
*    IF SY-TABIX EQ 1.
*      WL_COCKPIT-STATUS = 'ICON_RED_LIGHT'.
*    ELSE.
*
*      WL_COCKPIT-STATUS = 'ICON_YELLOW_LIGHT'.
*    ENDIF.
*    MODIFY TL_COCKPIT FROM WL_COCKPIT.
*  ENDLOOP.
  DATA:
    node_node_alv                       TYPE REF TO if_wd_context_node,
    stru_node_alv                       TYPE if_input_view=>element_node_alv .

* navigate from <CONTEXT> to <NODE_ALV> via lead selection
  node_node_alv = wd_context->get_child_node( name = if_input_view=>wdctx_node_alv ).


* get all declared attributes
  node_node_alv->bind_table( tl_cockpit ).
*ENDMETHOD.
ENDMETHOD.

method WDDOAFTERACTION .
endmethod.

method WDDOBEFOREACTION .
*  data lo_api_controller type ref to if_wd_view_controller.
*  data lo_action         type ref to if_wd_action.

*  lo_api_controller = wd_this->wd_get_api( ).
*  lo_action = lo_api_controller->get_current_action( ).

*  if lo_action is bound.
*    case lo_action->name.
*      when '...'.

*    endcase.
*  endif.
endmethod.

method WDDOEXIT .
endmethod.

METHOD WDDOINIT .
*  DATA LO_CMP_USAGE TYPE REF TO IF_WD_COMPONENT_USAGE.
*
*  LO_CMP_USAGE =   WD_THIS->WD_CPUSE_ALV_SOLICITACAO( ).
*  IF LO_CMP_USAGE->HAS_ACTIVE_COMPONENT( ) IS INITIAL.
*    LO_CMP_USAGE->CREATE_COMPONENT( ).
*  ENDIF.
*
** Get config model
*
*  DATA: L_REF_INTERFACECONTROLLER TYPE REF TO IWCI_SALV_WD_TABLE .
*
*  L_REF_INTERFACECONTROLLER = WD_THIS->WD_CPIFC_ALV_SOLICITACAO( ).
*
*  DATA: L_VALUE TYPE REF TO CL_SALV_WD_CONFIG_TABLE.
*
*  L_VALUE = L_REF_INTERFACECONTROLLER->GET_MODEL( ).
*
*  L_VALUE->IF_SALV_WD_TABLE_SETTINGS~SET_VISIBLE_ROW_COUNT( '10' ).
*
** Sort rows by seatsocc descending
*
*** data: lr_field type ref to cl_salv_wd_field.
***
*** lr_field = l_value->if_salv_wd_field_settings~get_field( 'SEATSOCC' ).
***
*** lr_field->if_salv_wd_sort~create_sort_rule( sort_order =
***
*** if_salv_wd_c_sort=>sort_order_descending ).
*
** Display icon in column seatsocc
*
*  DATA: LR_COLUMN TYPE REF TO CL_SALV_WD_COLUMN,
*
*  LR_IMAGE TYPE REF TO CL_SALV_WD_UIE_IMAGE,
*
*  LV_ICON TYPE STRING.
*
*  LR_COLUMN = L_VALUE->IF_SALV_WD_COLUMN_SETTINGS~GET_COLUMN( 'STATUS_EX' ).
*
*  CREATE OBJECT LR_IMAGE.
*  BREAK-POINT.
*  LR_IMAGE->SET_SOURCE_FIELDNAME( 'STATUS' ).
*
*  LR_COLUMN->SET_CELL_EDITOR( LR_IMAGE ). "Display traffic light images in column 'SEATSOCCC'
** delete column STATUS
*
*  L_VALUE->IF_SALV_WD_COLUMN_SETTINGS~DELETE_COLUMN( 'STATUS' ).
  wd_this->build_select_options(
  ).

***************** Radio button *****************
  DATA : V_ELEMENT TYPE REF TO IF_WD_CONTEXT_ELEMENT,
          ITEMS_NODE TYPE REF TO IF_WD_CONTEXT_NODE,
          V_INDEX TYPE I,
          V_TEXT TYPE STRING,
          ITEMLIST TYPE STANDARD TABLE OF IF_input_view2=>ELEMENT_RB_G1,
          ITEMLIST2 TYPE STANDARD TABLE OF IF_input_view2=>ELEMENT_RB_G2,
          W_LIST  LIKE LINE OF ITEMLIST,
          W_LIST2 LIKE LINE OF ITEMLIST2.
* Appending elements to "itemList"
  W_LIST-RB_TP_EXEC = 'Gera OV'.
  APPEND W_LIST TO ITEMLIST.

  W_LIST-RB_TP_EXEC = 'Consultar OV Geradas'.
  APPEND W_LIST TO ITEMLIST.


  ITEMS_NODE = WD_CONTEXT->GET_CHILD_NODE( NAME = `RB_G1` ).
  ITEMS_NODE->BIND_TABLE( ITEMLIST ).
  ITEMS_NODE->SET_LEAD_SELECTION_INDEX( 2 ).
  V_INDEX = ITEMS_NODE->GET_LEAD_SELECTION_INDEX( ).
  CLEAR W_LIST.

* Appending elements to "itemList"
  W_LIST2-RB_tp_sol = 'Formação de Lote'.
  APPEND W_LIST2 TO ITEMLIST2.

  W_LIST2-RB_tp_sol = 'Venda Normal'.
  APPEND W_LIST2 TO ITEMLIST2.


  ITEMS_NODE = WD_CONTEXT->GET_CHILD_NODE( NAME = `RB_G2` ).
  ITEMS_NODE->BIND_TABLE( ITEMLIST2 ).
  ITEMS_NODE->SET_LEAD_SELECTION_INDEX( 2 ).
  V_INDEX = ITEMS_NODE->GET_LEAD_SELECTION_INDEX( ).
  CLEAR W_LIST2.
*  READ TABLE ITEMLIST INTO W_LIST INDEX V_INDEX.
*  V_TEXT = W_LIST-EBELN.
*  WD_CONTEXT->SET_ATTRIBUTE( EXPORTING VALUE = V_TEXT  NAME = 'VIEWTEXT'  ).
ENDMETHOD.

method WDDOMODIFYVIEW .

endmethod.

method WDDOONCONTEXTMENU .
endmethod.

