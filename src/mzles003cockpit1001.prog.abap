*----------------------------------------------------------------------*
***INCLUDE MZLES003COCKPIT1001 .
*----------------------------------------------------------------------*

***********************************************************************
* LOCAL CLASSES
***********************************************************************
*---------- Definition -----------------------------------------------*
CLASS cockip_event_handler_item DEFINITION.

  PUBLIC SECTION.
    METHODS cockpit_hotspot_click_item
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id es_row_no.

ENDCLASS.                    "cockip_event_handler_lotes DEFINITION
*---------- Implementation -------------------------------------------*
CLASS cockip_event_handler_item IMPLEMENTATION.

  METHOD cockpit_hotspot_click_item.
    PERFORM cockpit_hotspot_click_item USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "cockpit_hotspot_click_lotes

ENDCLASS.                    "cockip_event_handler_lotes IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  cockpit_hotspot_click_item
*&---------------------------------------------------------------------*
FORM cockpit_hotspot_click_item  USING value(row_id) LIKE lvc_s_roid-row_id value(fieldname) LIKE lvc_s_col-fieldname.

  DATA: wa_lotes_item_alv TYPE zpfe_lote_item_alv,
        vg_belnr     TYPE belnr_d,
        vg_gjahr     TYPE gjahr,
        vg_bukrs     TYPE bukrs.

  CASE fieldname.
    WHEN 'ICOSTATUS'.
      READ TABLE it_lotes_item_alv  INDEX row_id INTO wa_lotes_item_alv.
      CHECK ( sy-subrc IS INITIAL ) AND ( wa_lotes_item_alv-nm_lote IS NOT INITIAL ).
      CALL FUNCTION 'Z_PFE_MSG_CONTAB'
        EXPORTING
          p_nm_lote      = wa_lotes_item_alv-nm_lote
          p_nm_lote_item = wa_lotes_item_alv-nm_lote_item.
    WHEN 'TKNUM'.
      READ TABLE it_lotes_item_alv  INDEX row_id INTO wa_lotes_item_alv.
      CHECK: ( sy-subrc IS INITIAL ) AND ( NOT wa_lotes_item_alv-tknum IS INITIAL ).
      SET PARAMETER ID: 'TNR' FIELD wa_lotes_item_alv-tknum.
      CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
    WHEN 'DOCNUM'.
      READ TABLE it_lotes_item_alv  INDEX row_id INTO wa_lotes_item_alv.
      CHECK ( sy-subrc IS INITIAL ) AND ( wa_lotes_item_alv-nm_lote IS NOT INITIAL ).
      PERFORM visualizar_nota USING wa_lotes_item_alv-docnum.
    WHEN 'CD_CIOT'.
      READ TABLE it_lotes_item_alv  INDEX row_id INTO wa_lotes_item_alv.
      CHECK ( sy-subrc IS INITIAL ) AND ( wa_lotes_item_alv-nm_lote IS NOT INITIAL ).
      PERFORM visualizar_ciot USING wa_lotes_item_alv-cd_ciot.
    WHEN 'BELNR'.
      READ TABLE it_lotes_item_alv  INDEX row_id INTO wa_lotes_item_alv.
      CHECK ( sy-subrc IS INITIAL ) AND ( wa_lotes_item_alv-nm_lote IS NOT INITIAL ).
      IF wa_lotes_item_alv-belnr IS INITIAL.
        MESSAGE s006 WITH wa_lotes_item_alv-nm_lote.
        CHECK 1 = 2.
      ENDIF.
      vg_belnr = wa_lotes_item_alv-belnr.
      vg_gjahr = wa_lotes_item_alv-gjahr.
      vg_bukrs = wa_lotes_alv-bukrs.
      SET PARAMETER ID: 'BLN' FIELD vg_belnr, 'BUK' FIELD vg_bukrs, 'GJR' FIELD vg_gjahr.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.                    " handle_hotspot_click

DATA: cockpit_event_handler_item  TYPE REF TO cockip_event_handler_item.

*&---------------------------------------------------------------------*
*&      Module  cria_item_lotes OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_item_lotes OUTPUT.

  PERFORM pesquisa_itens_lote.

  PERFORM cockpit_cria_item_alv.

  CALL METHOD cockpit_alv_lotes_item->refresh_table_display.

ENDMODULE.                 " CRIA_LACT_LOTES  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  COCKPIT_CRIA_ITEM_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cockpit_cria_item_alv .

  DATA: it_exclude_fcode TYPE ui_functions,
        wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

  CONSTANTS: tabela_item TYPE string VALUE 'IT_LOTES_ITEM_ALV'.

  IF cockpit_prim_item IS INITIAL.

    CLEAR: it_exclude_fcode.

    wa_exclude_fcode = '&PRINT'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&AVERAGE'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&MB_VIEW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&MB_FILTER'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&INFO'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

*   Create object for container
    CREATE OBJECT cockpit_container_lotes_item
      EXPORTING
        container_name = 'CTN_ITEM'.

    CREATE OBJECT cockpit_alv_lotes_item
      EXPORTING
        i_parent = cockpit_container_lotes_item.

    CLEAR: it_cockpit_catalog.

    PERFORM z_estrutura_fieldcat TABLES it_cockpit_catalog USING:
        tabela_item 'ICOSTATUS'      text-l00 'X' 01 02 space space   space space space space space space,
        tabela_item 'NR_LOTE_ADM'    text-l22 ' ' 02 10 space 'ALPHA' space space space space space space,
        tabela_item 'NM_LOTE'        text-l01 ' ' 02 10 space 'ALPHA' space space space space space space,
        tabela_item 'NM_LOTE_ITEM'   text-d02 ' ' 03 10 space 'ALPHA' space space space space space space,
        tabela_item 'CHVID'          text-d03 ' ' 04 02 space space   space space space space space space,
        tabela_item 'DESCHVID'       text-d04 ' ' 05 25 space space   space space space space space space,
        tabela_item 'CD_CIOT'        text-d05 'X' 06 12 space space   space space space space space space,
        tabela_item 'NR_CIOT'        text-d06 ' ' 07 12 space space   space space space space space space,
        tabela_item 'NUCONTRATO'     text-d12 ' ' 08 12 space space   space space space space space space,
        tabela_item 'TKNUM'          text-d20 'X' 09 12 space space   space space space space space space,
        tabela_item 'DOCNUM'         text-d07 'X' 10 10 space space   space space space space space space,
        tabela_item 'CTENUM'         text-d13 ' ' 11 09 space space   space space space space space space,
        tabela_item 'CTESERIE'       text-d14 ' ' 12 03 space space   space space space space space space,
        tabela_item 'DT_TRANSACAO'   text-d08 ' ' 13 10 space space   space space space space space space,
        tabela_item 'DT_BAIXA'       text-d21 ' ' 13 10 space space   space space space space space space,
        tabela_item 'VL_TRANSACAO'   text-d09 ' ' 14 15 space space   space space space space space space,
        tabela_item 'VL_PAGO_LOTE'   text-d19 ' ' 15 15 space space   space space space space space space,
        tabela_item 'VL_CONFERIDO'   text-d15 ' ' 16 15 space space   space space space space space space,
        tabela_item 'VL_DIFERENCA'   text-d16 ' ' 17 15 space space   space space space space space space,
        tabela_item 'DT_CHEGADA'     text-d10 ' ' 18 10 space space   space space space space space space,
        tabela_item 'PESO_ORIGEM'    text-d17 ' ' 19 13 space space   space space space space space space,
        tabela_item 'PESO_IMPORTADO' text-d18 ' ' 20 13 space space   space space space space space space,
        tabela_item 'PESO_CHEGADA'   text-d11 ' ' 21 13 space space   space space space space space space,
        tabela_item 'BELNR'          text-l13 'X' 22 10 space 'ALPHA' space space space space space space,
        tabela_item 'GJAHR'          text-l14 ' ' 23 06 space space   space space space space space space,
        tabela_item 'DS_USUARIO_CTB' text-l15 ' ' 24 13 space space   space space space space space space.

    CLEAR: cockpit_gs_layout.

    is_variant-report = sy-repid.
    is_variant-handle = '1001'.
    cockpit_gs_layout-info_fname = 'ROWCOLOR_ATUAL'.

    CALL METHOD cockpit_alv_lotes_item->set_table_for_first_display
      EXPORTING
        i_default            = space
        is_layout            = cockpit_gs_layout
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = is_variant
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_cockpit_catalog
        it_outtab            = it_lotes_item_alv[].

*   Create Object for Event Handler
    CREATE OBJECT cockpit_event_handler_item.

    SET HANDLER cockpit_event_handler_item->cockpit_hotspot_click_item FOR cockpit_alv_lotes_item.

    cockpit_prim_item = c_x.

  ENDIF.

ENDFORM.                    " COCKPIT_CRIA_LACT_ALV

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_ITENS_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pesquisa_itens_lote .

  CLEAR: it_lotes_item[], it_lotes_item_alv[].

  CALL FUNCTION 'Z_PFE_PSQ_ITENS'
    EXPORTING
      p_lote_alv  = wa_lotes_alv
    TABLES
      p_itens     = it_lotes_item
      p_itens_alv = it_lotes_item_alv
    EXCEPTIONS
      sem_itens   = 1
      OTHERS      = 2.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " PESQUISA_ITENS_LOTE

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LOTES_ITEM_ALV_DOCNUM  text
*----------------------------------------------------------------------*
FORM visualizar_nota  USING  p_docnum TYPE j_1bdocnum.

  DATA gf_nfobjn  LIKE j_1binterf-nfobjn.

  CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
    EXPORTING
      doc_number         = p_docnum
    IMPORTING
      obj_number         = gf_nfobjn
    EXCEPTIONS
      document_not_found = 1
      docum_lock         = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
    EXPORTING
      obj_number         = gf_nfobjn
    EXCEPTIONS
      object_not_found   = 1
      scr_ctrl_not_found = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " VISUALIZAR_NOTA

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_CIOT
*&---------------------------------------------------------------------*
FORM visualizar_ciot  USING p_cd_ciot TYPE zciot.

  CALL FUNCTION 'Z_SD_EMITE_CIOT'
    EXPORTING
      p_cd_ciot        = p_cd_ciot
    EXCEPTIONS
      sem_dados_ciot   = 1
      nao_ciot         = 2
      erro_status      = 3
      erro_web_service = 4
      OTHERS           = 5.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " VISUALIZAR_CIOT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1001 INPUT.

  DATA: wa_aux_lotes_item     TYPE zpfe_lote_item,
        wa_aux_lotes_item_alv TYPE zpfe_lote_item_alv,
        wa_aux_lote           TYPE zpfe_lote,
        wa_aux_lote_alv       TYPE zpfe_lote_alv.

  CASE ok_code.
    WHEN ok_atualiza.
      PERFORM pesquisa_itens_lote.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1001  INPUT

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM seleciona_item  USING    p_vg_selecionou.

  CLEAR: it_selected_rows_it, p_vg_selecionou, it_lotes_item_sel[].

  CALL METHOD cockpit_alv_lotes_conf->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows_it.

  LOOP AT it_selected_rows_it INTO wa_selected_rows_it.
    READ TABLE it_lotes_item_alv INTO wa_lotes_item_alv INDEX wa_selected_rows_it-index.
    APPEND wa_lotes_item_alv TO it_lotes_item_sel.
    p_vg_selecionou = c_x.
  ENDLOOP.

  READ TABLE it_selected_rows_it INTO wa_selected_rows_it INDEX 1.
  READ TABLE it_lotes_item_alv INTO wa_lotes_item_alv INDEX wa_selected_rows_it-index.
  READ TABLE it_lotes_item     INTO wa_lotes_item WITH KEY nm_lote      = wa_lotes_item_alv-nm_lote
                                                           nm_lote_item = wa_lotes_item_alv-nm_lote_item.

ENDFORM.                    " SELECIONA_ITEM
