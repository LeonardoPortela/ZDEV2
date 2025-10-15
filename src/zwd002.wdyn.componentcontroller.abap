METHOD onfind .

*******  Radio button ******
  DATA : v_element TYPE REF TO if_wd_context_element,
         items_node TYPE REF TO if_wd_context_node,
         v_index TYPE i,
         v_text TYPE string,
         itemlist TYPE STANDARD TABLE OF if_input_view=>element_rb_g1,
         itemlist2 TYPE STANDARD TABLE OF if_input_view=>element_rb_g2,
         w_list LIKE LINE OF itemlist,
         w_list2 LIKE LINE OF itemlist2.
***********  end radio button

  TYPES:lty_r_vkorg TYPE RANGE OF vkorg,
        lty_r_vtweg TYPE RANGE OF vtweg,
        lty_r_spart TYPE RANGE OF spart,
        lty_r_vkbur TYPE RANGE OF vkbur,
        lty_r_kunnr TYPE RANGE OF kunnr,
        lty_r_tp_venda TYPE RANGE OF zsded012.

** Variables used to retrieve the values of select-options fields
*  DATA
*    lt_sel_item TYPE if_wd_select_options=>tt_selection_screen_item.

*  FIELD-SYMBOLS:
*    <fs_sel_item> LIKE LINE OF lt_sel_item,
*    <fs_vkorg>   TYPE lty_r_vkorg,
*    <fs_vtweg>   TYPE lty_r_vtweg,
*    <fs_spart>   TYPE lty_r_spart,
**    <fs_vkbur>   TYPE lty_r_vkbur,
*    <fs_kunnr>   TYPE lty_r_kunnr,
*    <fs_tp_venda>   TYPE lty_r_tp_venda.


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
*  wd_this->m_helper->get_selection_screen_items(
*    IMPORTING et_selection_screen_items = lt_sel_item ).
*
** Retrieve the values from the select-options items
*  LOOP AT lt_sel_item ASSIGNING <fs_sel_item>.
*    CASE <fs_sel_item>-m_id.
*      WHEN `VKORG`.
*        ASSIGN <fs_sel_item>-mt_range_table->* TO <fs_vkorg>.
*      WHEN `VTWEG`.
*        ASSIGN <fs_sel_item>-mt_range_table->* TO <fs_vtweg>.
*      WHEN `SPART`.
*        ASSIGN <fs_sel_item>-mt_range_table->* TO <fs_spart>.
*      WHEN `KUNNR`.
*        ASSIGN <fs_sel_item>-mt_range_table->* TO <fs_kunnr>.
*      WHEN `TP_VENDA`.
*        ASSIGN <fs_sel_item>-mt_range_table->* TO <fs_tp_venda>.
*    ENDCASE.
*  ENDLOOP.


  DATA: node_node_sol                  TYPE REF TO if_wd_context_node,
    elem_node_sol                      TYPE REF TO if_wd_context_element,
    stru_node_sol                      TYPE if_input_view=>element_node_sol .

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
*        rg_spart TYPE TABLE OF bapi_rangesspart ,
*        rg_kunnr TYPE TABLE OF bapi_rangeskunnr ,
*        rg_vtweg TYPE TABLE OF bapi_rangesvtweg ,
        rg_vkorg TYPE RANGE OF vkorg,
        rg_spart TYPE RANGE OF spart,
        rg_kunnr TYPE RANGE OF kunnr,
        rg_vtweg TYPE RANGE OF vtweg,
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

*  items_node = wd_context->get_child_node( name = `NODE_SOL` ).
  items_node = wd_context->get_child_node( name = `RG_VKORG` ).
  items_node->get_static_attributes_table( IMPORTING table = rg_vkorg ).
  IF stru_node_sol-vkorg IS NOT INITIAL.
    wa_vkorg-sign = 'I'.
    wa_vkorg-option = 'EQ'.
    wa_vkorg-low = stru_node_sol-vkorg.
    APPEND wa_vkorg TO rg_vkorg.
    CLEAR: wa_vkorg.
  ENDIF.
  BREAK-POINT.
  break abap.

*items_node = wd_context->get_child_node( name = `NODE_SOL` ).
  items_node = wd_context->get_child_node( name = `RG_VTWEG` ).
  items_node->get_static_attributes_table( IMPORTING table = rg_vtweg ).
  IF stru_node_sol-vtweg IS NOT INITIAL.
    wa_vtweg-sign = 'I'.
    wa_vtweg-option = 'EQ'.
    wa_vtweg-low = stru_node_sol-vtweg.
    APPEND wa_vtweg TO rg_vtweg.
    CLEAR: wa_vtweg.
  ENDIF.

*items_node = wd_context->get_child_node( name = `NODE_SOL` ).
  items_node = wd_context->get_child_node( name = `RG_SPART` ).
  items_node->get_static_attributes_table( IMPORTING table = rg_spart ).
  IF stru_node_sol-spart IS NOT INITIAL.
    wa_spart-sign = 'I'.
    wa_spart-option = 'EQ'.
    wa_spart-low = stru_node_sol-spart.
    APPEND wa_spart TO rg_spart.
    CLEAR: wa_spart.
  ENDIF.

*  items_node = wd_context->get_child_node( name = `NODE_SOL` ).
  items_node = wd_context->get_child_node( name = `RG_KUNNR` ).
  items_node->get_static_attributes_table( IMPORTING table = rg_kunnr ).
  IF stru_node_sol-kunnr IS NOT INITIAL.
    wa_kunnr-sign = 'I'.
    wa_kunnr-option = 'EQ'.
    wa_kunnr-low = stru_node_sol-kunnr.
    APPEND wa_kunnr TO rg_kunnr.
    CLEAR: wa_kunnr.
  ENDIF.

*items_node = wd_context->get_child_node( name = `NODE_SOL` ).
  items_node = wd_context->get_child_node( name = `RG_TP_VENDA` ).
  items_node->get_static_attributes_table( IMPORTING table = rg_tp_venda ).
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
      rg_vkorg    = rg_vkorg
      rg_vtweg    = rg_vtweg
      rg_spart    = rg_spart
      rg_kunnr    = rg_kunnr
      rg_tp_venda = rg_TP_venda
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
ENDMETHOD.

method WDDOAPPLICATIONSTATECHANGE .
endmethod.

method WDDOBEFORENAVIGATION .
endmethod.

method WDDOEXIT .
endmethod.

METHOD WDDOINIT .


ENDMETHOD.

method WDDOPOSTPROCESSING .
endmethod.

