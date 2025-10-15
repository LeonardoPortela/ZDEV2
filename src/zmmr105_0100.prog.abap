*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0100 .
*----------------------------------------------------------------------*

DATA: splitter_0503        TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer_0503 TYPE REF TO cl_gui_container,
      ctl_alv_0503         TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog_0503 TYPE lvc_t_fcat,
      gs_variant_0503      TYPE disvariant,
      gs_layout_0503       TYPE lvc_s_layo,
      it_cargas            TYPE TABLE OF zde_imp_algodao_chegada.

CLASS lcl_events_d0100 DEFINITION.

  PUBLIC SECTION.
    DATA: error_in_data TYPE c.
    METHODS:
      data_changed  FOR EVENT data_changed
        OF cl_gui_alv_grid
        IMPORTING er_data_changed
                  e_onf4
                  e_onf4_before
                  e_onf4_after,

      data_changed_finished FOR EVENT data_changed_finished
        OF cl_gui_alv_grid
        IMPORTING e_modified
                  et_good_cells.

  PRIVATE SECTION.

    METHODS: perform_semantic_checks
      IMPORTING
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                    "lcl_events_d0100 DEFINITION

DATA: gr_events_d0100 TYPE REF TO lcl_events_d0100.

CLASS lcl_events_d0100 IMPLEMENTATION.
  METHOD data_changed.
    error_in_data = space.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    "IF ERROR_IN_DATA = 'X'.
    "  CALL METHOD ER_DATA_CHANGED->DISPLAY_PROTOCOL.
    "ENDIF.
  ENDMETHOD.                    "data_changed

  METHOD perform_semantic_checks.

    DATA: lc_tp_autorizado TYPE zde_tp_autorizacao,
          lc_tp_aprovacao  TYPE zde_tip_aprovacao.

    FIELD-SYMBOLS: <fs_cell> TYPE lvc_s_modi.

    READ TABLE pr_data_changed->mt_good_cells ASSIGNING <fs_cell> WITH KEY fieldname = 'CK_CHEGADA_DOC'.
    IF sy-subrc IS INITIAL.

      READ TABLE it_cte_alv INDEX <fs_cell>-row_id INTO wa_cte_alv.

      IF wa_cte_alv-ck_chegada_doc EQ abap_true.
        error_in_data = 'X'.
        MESSAGE s165 DISPLAY LIKE 'E'.
      ENDIF.

      CHECK wa_cte_alv-ck_chegada_doc EQ abap_false.

      "01	Chegada de Documentos
      lc_tp_aprovacao = '01'.

      "01	Autorizado
      "02	Negado
      IF <fs_cell>-value EQ abap_true.
        lc_tp_autorizado = '01'.
      ELSE.
        lc_tp_autorizado = '02'.
      ENDIF.

      CALL METHOD zcl_cte_dist_g=>autorizacao
        EXPORTING
          i_cd_chave_cte  = wa_cte_alv-cd_chave_cte
        IMPORTING
          e_tp_autorizado = lc_tp_autorizado
        CHANGING
          i_tp_aprovacao  = lc_tp_aprovacao
        EXCEPTIONS
          erro            = 1
          cte_finalizado  = 2
          cte_nao_loc     = 3
          cte_bloqueada   = 4
          sem_autorizacao = 5
          OTHERS          = 6.

      IF sy-subrc IS NOT INITIAL.
        error_in_data = 'X'.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD data_changed_finished.

    DATA: wa_stable_cte TYPE lvc_s_stbl,
          ls_modi       TYPE lvc_s_modi.

    IF e_modified IS NOT INITIAL AND error_in_data IS NOT INITIAL.

      LOOP AT et_good_cells INTO ls_modi.
        READ TABLE it_cte_alv INDEX ls_modi-row_id INTO wa_cte_alv.

        IF ls_modi-value EQ abap_true.
          wa_cte_alv-ck_chegada_doc = abap_false.
        ELSE.
          wa_cte_alv-ck_chegada_doc = abap_true.
        ENDIF.

        MODIFY it_cte_alv INDEX sy-tabix FROM wa_cte_alv TRANSPORTING ck_chegada_doc.
      ENDLOOP.


      wa_stable_cte-row = abap_true.
      wa_stable_cte-col = abap_true.

      CALL METHOD ctl_alv_cte->refresh_table_display
        EXPORTING
          is_stable = wa_stable_cte.
    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_


ENDCLASS.


CONSTANTS:
  ok_dacte      TYPE sy-ucomm VALUE 'DACTE',
  ok_refresh    TYPE sy-ucomm VALUE 'REFRESH',
  ok_autorizar  TYPE sy-ucomm VALUE 'AUTORIZAR',
  ok_gerar_vt   TYPE sy-ucomm VALUE 'GERAR_VT',
  ok_anexar     TYPE sy-ucomm VALUE 'ANEXAR',
  ok_mulanexar  TYPE sy-ucomm VALUE 'MULANEXAR',
  ok_atuliza_01 TYPE sy-ucomm VALUE 'ATULIZA_01',
  "OK_LIBDIFPESO TYPE SY-UCOMM VALUE 'LIBDIFPESO',
  ok_reiniciar  TYPE sy-ucomm VALUE 'REINICIAR',
  ok_gerarfisc  TYPE sy-ucomm VALUE 'GERARFISC',
  ok_estorfisc  TYPE sy-ucomm VALUE 'ESTORFISC',
  ok_viscte     TYPE sy-ucomm VALUE 'VISCTE',
  ok_tabparc    TYPE sy-ucomm VALUE 'TABPARC',
  ok_tabnf55    TYPE sy-ucomm VALUE 'TABNF55',
  ok_tabnf01    TYPE sy-ucomm VALUE 'TABNF01',
  ok_tabdocs    TYPE sy-ucomm VALUE 'TABDOCS',
  ok_tabct57    TYPE sy-ucomm VALUE 'TABCT57',
  ok_tabmodal   TYPE sy-ucomm VALUE 'TABMODAL',
  ok_inbalgodt  TYPE sy-ucomm VALUE 'INBALGODT',

  ok_tabveic    TYPE sy-ucomm VALUE 'TABVEIC',
  ok_tabmoto    TYPE sy-ucomm VALUE 'TABMOTO',

  ok_salvar     TYPE sy-ucomm VALUE 'SALVAR',
  ok_rateio     TYPE sy-ucomm VALUE 'RATEIO',
  ok_delete     TYPE sy-ucomm VALUE 'DELETE',
  ok_cancel     TYPE sy-ucomm VALUE 'CANCEL',
  ok_compvalor  TYPE sy-ucomm VALUE 'COMPVALOR',
  ok_detvagoes  TYPE sy-ucomm VALUE 'DETVAGOES',
  ok_duplicata  TYPE sy-ucomm VALUE 'DUPLICATA',
  ok_comptext   TYPE sy-ucomm VALUE 'COMPTEXT',
  ok_enter      TYPE sy-ucomm VALUE 'ENTER',
  ok_aprovar    TYPE sy-ucomm VALUE 'APROVAR',
  ok_inc_nfe    TYPE sy-ucomm VALUE 'INC_NFE',
  ok_del_nfe    TYPE sy-ucomm VALUE 'DEL_NFE',
  ok_verificar  TYPE sy-ucomm VALUE 'VERIFICAR',
  ok_confirmar  TYPE sy-ucomm VALUE 'CONFIRMA',
  ok_faturar    TYPE sy-ucomm VALUE 'DIGITAR',
  tl_0102       TYPE sy-dynnr VALUE '0102',
  tl_0103       TYPE sy-dynnr VALUE '0103',
  tl_0104       TYPE sy-dynnr VALUE '0104',
  tl_0105       TYPE sy-dynnr VALUE '0105',
  tl_0106       TYPE sy-dynnr VALUE '0106',
  tl_0113       TYPE sy-dynnr VALUE '0113',
  tl_0114       TYPE sy-dynnr VALUE '0114',
  tl_0115       TYPE sy-dynnr VALUE '0115',
  tl_0116       TYPE sy-dynnr VALUE '0116',
  tl_0117       TYPE sy-dynnr VALUE '0117',
  tl_0118       TYPE sy-dynnr VALUE '0118',
  tl_0119       TYPE sy-dynnr VALUE '0119',
  tl_0120       TYPE sy-dynnr VALUE '0120',
  cl_c710       TYPE c LENGTH 04 VALUE 'C710'.

CONTROLS: tabprinc TYPE TABSTRIP,
          tabrodo  TYPE TABSTRIP,
          tabparc  TYPE TABLEVIEW USING SCREEN 0102,
          tabnf55  TYPE TABLEVIEW USING SCREEN 0103,
          tabnf01  TYPE TABLEVIEW USING SCREEN 0104,
          tabdocs  TYPE TABLEVIEW USING SCREEN 0105,
          tabct57  TYPE TABLEVIEW USING SCREEN 0106.

DATA: vg_tela_0101 TYPE sy-dynnr,
      vg_tela_0113 TYPE sy-dynnr,
      vg_tela_0114 TYPE sy-dynnr.

*&---------------------------------------------------------------------*
*&      Form  POPULA_SELECAO_CTE
*&---------------------------------------------------------------------*
FORM popula_selecao_cte .

  CLEAR it_selected_rows.

  CALL METHOD ctl_alv_cte->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CLEAR it_cte_select[].

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_cte_alv INTO wa_cte_alv INDEX wa_selected_rows-index.
    MOVE-CORRESPONDING wa_cte_alv TO it_cte_select.
    APPEND it_cte_select.
  ENDLOOP.

ENDFORM.                    " POPULA_SELECAO_CTE

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  PERFORM limpar_bloqs.

  CASE ok_code.
    WHEN ok_dacte.
      PERFORM visualizar_dacte.
    WHEN ok_refresh.
      PERFORM pesquisar_pagamentos.
      PERFORM atualiza_tela.
    WHEN ok_reiniciar.
      PERFORM reiniciar_pagamentos.
    WHEN ok_gerarfisc.
      PERFORM gerar_pagamentos USING abap_false.
    WHEN ok_estorfisc.
      PERFORM gerar_pagamentos USING abap_true.
    WHEN ok_faturar.
      PERFORM digitar_info_fatura_cte.
    WHEN ok_viscte.
      PERFORM visualizar_cte.
    WHEN ok_autorizar.
      PERFORM autorizar_pagamento_cte.
    WHEN ok_gerar_vt.
      PERFORM gerar_documento_transporte.
    WHEN ok_anexar.
      PERFORM habilitar_workflow_documentos.
    WHEN ok_mulanexar.
      PERFORM anexar_multiplos_documentos.
      PERFORM atualiza_tela.
    WHEN ok_atuliza_01.
      PERFORM atualizar_tabela_base_custo.
    WHEN ok_inbalgodt.
      PERFORM imp_arquivo_chegada_algodao.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: wa_obj  TYPE borident,
        ip_mode TYPE sgs_rwmod.

  SET PF-STATUS 'PFPAGA'.

  "IF CTL_CCCONTAINER IS INITIAL.
  IF dg_splitter IS INITIAL.

    "CREATE OBJECT CTL_CCCONTAINER
    "  EXPORTING
    "    CONTAINER_NAME = 'CT_CTE'.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0 "CTL_CCCONTAINER
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = ctl_cccontainer2.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = ctl_cccontainer3.

    CALL METHOD dg_splitter->set_row_height
      EXPORTING
        id     = 2
        height = 20.

    CREATE OBJECT ctl_alv_cte
      EXPORTING
        i_parent = ctl_cccontainer2.

    PERFORM fill_it_fieldcatalog.

*   Fill info for layout variant
    PERFORM fill_gs_variant.

*   Set layout parameters for ALV grid
    gs_layout-grid_title = TEXT-100.
    gs_layout-sel_mode   = 'A'.

    CLEAR: it_exclude_fcode, it_exclude_fcode[].

    IF cg_t4 EQ abap_true.
      gs_layout-edit_mode  = abap_true.

      APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_exclude_fcode.

      CREATE OBJECT gr_events_d0100.
      SET HANDLER gr_events_d0100->data_changed          FOR ctl_alv_cte.
      SET HANDLER gr_events_d0100->data_changed_finished FOR ctl_alv_cte.

      CALL METHOD ctl_alv_cte->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.
    ENDIF.

    CALL METHOD ctl_alv_cte->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_cte_alv.

    CREATE OBJECT ctl_alv_cte_hist
      EXPORTING
        i_parent = ctl_cccontainer3.

    PERFORM fill_it_fieldcatalog2.

*   Fill info for layout variant2
    PERFORM fill_gs_variant2.

    CALL METHOD ctl_alv_cte_hist->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout2
        is_variant           = gs_variant2
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog      = it_fieldcatalog2
        it_outtab            = it_log_alv.

    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_hotspot_click FOR ctl_alv_cte.
    SET HANDLER event_handler->handle_double_click  FOR ctl_alv_cte.

    CREATE OBJECT event_handler_log.
    SET HANDLER event_handler_log->handle_hotspot_click FOR ctl_alv_cte_hist.
  ENDIF.

  CALL METHOD ctl_alv_cte->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col
      es_row_no   = gs_scroll_row.

  CALL METHOD ctl_alv_cte_hist->set_scroll_info_via_id
    EXPORTING
      is_col_info = gs_scroll_col2
      is_row_no   = gs_scroll_row2.

  IF manager IS NOT INITIAL.
    CALL METHOD manager->unpublish.
    CLEAR: manager.
  ENDIF.

  IF it_cte_selectw[] IS NOT INITIAL.
    READ TABLE it_cte_selectw INDEX 1.
    wa_obj-objtype = 'ZMM0079'.
    CONCATENATE it_cte_selectw-mandt it_cte_selectw-cd_chave_cte INTO wa_obj-objkey.

    IF gf_authorization_ft_09 EQ abap_true.
      ip_mode = 'E'.
    ELSE.
      ip_mode = 'D'.
    ENDIF.

    CREATE OBJECT manager
      EXPORTING
        is_object        = wa_obj
        ip_no_commit     = 'R'
        ip_mode          = ip_mode
      EXCEPTIONS
        object_invalid   = 1
        callback_invalid = 2
        OTHERS           = 3.

    "Workflow CT-e: Nr.: &1 Sr.: &2 Dt.: &3 Forn.: &4
    SELECT SINGLE * INTO wa_cte_dist
      FROM zib_cte_dist_ter
     WHERE cd_chave_cte EQ it_cte_selectw-cd_chave_cte.

    SET TITLEBAR 'TLWORK' WITH wa_cte_dist-numr_cte wa_cte_dist-numr_serie wa_cte_dist-dt_emissao wa_cte_dist-emit_rsocial.
    CLEAR: wa_cte_dist.
  ELSE.
    SET TITLEBAR 'TLPAGA'.
  ENDIF.



ENDMODULE.                 " STATUS_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_CTE_DIST_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

  IF cg_t4 EQ abap_true.
    lc_col_pos = 2.
  ELSE.
    lc_col_pos = 1.
  ENDIF.

  LOOP AT it_fieldcatalog ASSIGNING <fs_cat>.

    CASE <fs_cat>-fieldname.
      WHEN 'CK_CHEGADA_DOC'.
        IF cg_t4 EQ abap_true.
          <fs_cat>-col_pos   = 1.
          <fs_cat>-no_out    = abap_false.
          <fs_cat>-checkbox  = abap_true.
          <fs_cat>-edit      = abap_true.
          <fs_cat>-outputlen = 4.
        ELSE.
          <fs_cat>-no_out = abap_true.
        ENDIF.
      WHEN 'INICIO_MUNI' OR 'TERMINO_MUNI'.
        <fs_cat>-outputlen = 20.
      WHEN 'STATUS'.
        <fs_cat>-hotspot = abap_true.
        <fs_cat>-just    = 'C'.
      WHEN 'DOCNUM_CTE'.
        <fs_cat>-hotspot = abap_true.
      WHEN 'EBELN' OR 'BELNR'.
        <fs_cat>-hotspot = abap_true.
      WHEN 'GJAHR'.
        <fs_cat>-outputlen = 05.
      WHEN 'DOCNUM_CTE_C'.
        <fs_cat>-hotspot = abap_true.
      WHEN 'DOCNUM_CTE_A'.
        <fs_cat>-hotspot = abap_true.
      WHEN 'DOCNUM_CTE_S'.
        <fs_cat>-hotspot = abap_true.
      WHEN 'DOCNUM_CTE_SUB'.
        <fs_cat>-hotspot = abap_true.
      WHEN 'IC_VIEWLOG'. "115183 CS2023000462 ZMM0079 - criação de opção de listagem de autorizações realizadas CTes - PSA
        <fs_cat>-hotspot = abap_true.
        <fs_cat>-just    = 'C'.
    ENDCASE.

    IF <fs_cat>-fieldname <> 'CK_CHEGADA_DOC'.
      <fs_cat>-col_pos = lc_col_pos.
      ADD 1 TO lc_col_pos.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0001'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG2
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog2 .

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_CTE_DIST_LOG_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog2.

  LOOP AT it_fieldcatalog2 ASSIGNING <fs_cat>.
    CASE <fs_cat>-fieldname.
      WHEN 'MESSAGE'.
        <fs_cat>-outputlen = 50.
      WHEN 'MESSAGE_V1' OR 'MESSAGE_V2' OR 'MESSAGE_V3' OR 'MESSAGE_V4'.
        <fs_cat>-outputlen = 20.
      WHEN 'IC_MESSAGE'.
        <fs_cat>-just    = 'C'.
      WHEN 'IC_TEXTO'.
        <fs_cat>-hotspot = abap_true.
        <fs_cat>-just    = 'C'.
      WHEN 'CK_ESTRATEGIA'.
        <fs_cat>-no_out  = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG2

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_cte_alv INDEX row_id INTO wa_cte_alv.

  PERFORM limpar_bloqs.

  CASE fieldname.
    WHEN 'DOCNUM_CTE'.
      PERFORM mostrar_doc_fiscal USING wa_cte_alv-docnum_cte.
    WHEN 'DOCNUM_CTE_C'.
      PERFORM mostrar_doc_fiscal USING wa_cte_alv-docnum_cte_c.
    WHEN 'DOCNUM_CTE_A'.
      PERFORM mostrar_doc_fiscal USING wa_cte_alv-docnum_cte_a.
    WHEN 'DOCNUM_CTE_S'.
      PERFORM mostrar_doc_fiscal USING wa_cte_alv-docnum_cte_s.
    WHEN 'DOCNUM_CTE_SUB'.
      PERFORM mostrar_doc_fiscal USING wa_cte_alv-docnum_cte_sub.
    WHEN 'EBELN'.
      PERFORM mostrar_pedido USING wa_cte_alv-ebeln.
    WHEN 'BELNR'.
      PERFORM mostrar_fatura USING wa_cte_alv-belnr wa_cte_alv-gjahr.
    WHEN 'STATUS'.
      PERFORM busca_logs_cte USING wa_cte_alv-cd_chave_cte.
    WHEN 'IC_VIEWLOG'.                                        "115183 CS2023000462 ZMM0079 - criação de opção de listagem de autorizações realizadas CTes - PSA
      PERFORM  visualizar_log.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click_log
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  DATA: i_cd_aprovacao  TYPE zde_est_aprovacao.

  READ TABLE it_log_alv INDEX row_id INTO wa_log_alv.

  CASE fieldname.
    WHEN 'IC_TEXTO'.
      IF wa_log_alv-ck_estrategia EQ abap_true.

        MOVE wa_log_alv-message_v4 TO i_cd_aprovacao.

        CHECK i_cd_aprovacao IS NOT INITIAL.

        CALL METHOD zcl_cte_dist_g=>autorizacao_view
          EXPORTING
            i_cd_aprovacao          = i_cd_aprovacao
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            nao_encontrado          = 8
            OTHERS                  = 9.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_exit100 INPUT.

  CASE ok_code.
    WHEN 'BACK'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_EXIT100  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUSCA_LOGS_CTE
*&---------------------------------------------------------------------*
FORM busca_logs_cte  USING p_chave TYPE zde_chave_doc_e.

  DATA: it_logs TYPE zde_cte_dist_log_alv_t,
        wa_logs TYPE zde_cte_dist_log_alv.

  CLEAR: it_log_alv.

  CALL METHOD obj_cte->busca_log_proc_cte
    EXPORTING
      p_chave = p_chave
    IMPORTING
      e_logs  = it_logs.

  LOOP AT it_logs INTO wa_logs.
    MOVE-CORRESPONDING wa_logs TO wa_log_alv.
    APPEND wa_log_alv TO it_log_alv.
  ENDLOOP.

  CALL METHOD ctl_alv_cte_hist->refresh_table_display.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " BUSCA_LOGS_CTE

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_PAGAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pesquisar_pagamentos .

  DATA: it_nfes    TYPE TABLE OF zib_cte_dist_n55 WITH HEADER LINE,
        it_ctes    TYPE TABLE OF zib_cte_dist_c57 WITH HEADER LINE,
        it_vfkp    TYPE TABLE OF vfkp WITH HEADER LINE,
        it_vttk    TYPE TABLE OF vttk WITH HEADER LINE,
        it_vttp    TYPE TABLE OF vttp WITH HEADER LINE,
        it_vbfa    TYPE TABLE OF vbfa WITH HEADER LINE,
        it_j1li    TYPE TABLE OF j_1bnflin WITH HEADER LINE,
        it_actv    TYPE TABLE OF j_1bnfe_active WITH HEADER LINE,
        lc_matnr   TYPE zmatnr,
        lc_matkl   TYPE matkl,
        ck_chave99 TYPE c LENGTH 1.

  RANGES: ck_xx      FOR zib_cte_dist_ter-ck_finalizado,
          ck_at      FOR zib_cte_dist_ter-ck_autorizado,
          rg_nr_ce   FOR zib_cte_dist_ter-numr_cte,
          rg_nr_ce1  FOR zib_cte_dist_ter-numr_cte,
          ch_nr_nfe  FOR zib_cte_dist_n55-n55_chave_acesso,
          ch_nr_cte  FOR zib_cte_dist_ter-cd_chave_cte,
          ch_nr_cte2 FOR zib_cte_dist_ter-cd_chave_cte,
          ch_nr_cte3 FOR zib_cte_dist_ter-cd_chave_cte,
          nr_nr_cte  FOR zib_cte_dist_ter-cd_chave_cte,
          nr_nr_cte2 FOR zib_cte_dist_ter-cd_chave_cte,
*          ck_cancela FOR zib_cte_dist_ter-cd_status_sefaz,
          ck_cancel  FOR zib_cte_dist_ter-cancel.

  DATA: it_cte TYPE zib_cte_dist_ter_t,
        it_alv TYPE zde_cte_dist_alv_t,
        wa_alv TYPE zde_cte_dist_alv.

  PERFORM limpar_bloqs.

  CLEAR: it_cte_alv, it_cte, ck_chave99.

  CLEAR: it_cte_selectw[].

  CASE abap_true.
    WHEN ck_02.
      ck_xx-sign   = 'I'.
      ck_xx-option = 'EQ'.
      ck_xx-low    = abap_false.
      ck_xx-high   = abap_false.
      APPEND ck_xx.
    WHEN ck_03.
      ck_xx-sign   = 'I'.
      ck_xx-option = 'EQ'.
      ck_xx-low    = abap_true.
      ck_xx-high   = abap_true.
      APPEND ck_xx.
  ENDCASE.

  CASE abap_true.
    WHEN ck_a2.
      ck_at-sign   = 'I'.
      ck_at-option = 'EQ'.
      ck_at-low    = abap_true.
      ck_at-high   = abap_true.
      APPEND ck_at.
    WHEN ck_a3.
      ck_at-sign   = 'I'.
      ck_at-option = 'EQ'.
      ck_at-low    = abap_false.
      ck_at-high   = abap_false.
      APPEND ck_at.
  ENDCASE.

  LOOP AT numrct INTO rg_nr_ce.
    IF rg_nr_ce-low IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = rg_nr_ce-low
        IMPORTING
          output = rg_nr_ce-low.
    ENDIF.
    IF rg_nr_ce-high IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = rg_nr_ce-high
        IMPORTING
          output = rg_nr_ce-high.
    ENDIF.
    APPEND rg_nr_ce.
  ENDLOOP.

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Consulta de Chaves """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF achvnfe IS NOT INITIAL.

    SELECT * INTO TABLE it_nfes
      FROM zib_cte_dist_n55
     WHERE n55_chave_acesso IN achvnfe
       AND tknum IN nrdoctr
       AND fknum IN nrdocvi.

    IF sy-subrc IS INITIAL.
      "Pesquisa por documento de transporte e documento de custo e não localizou nf-e
      "buscar pela remessa da VT a nota fiscal e procurar documento de transporte
      LOOP AT it_nfes.
        ch_nr_cte-sign   = 'I'.
        ch_nr_cte-option = 'EQ'.
        ch_nr_cte-low    = it_nfes-cd_chave_cte.
        ch_nr_cte-high   = it_nfes-cd_chave_cte.
        APPEND ch_nr_cte.
      ENDLOOP.
    ELSE.
      ck_chave99 = abap_true.
    ENDIF.
  ENDIF.

  IF nrdoctr IS NOT INITIAL OR nrdocvi IS NOT INITIAL.

    CLEAR: it_vttk[], it_vttp[], it_vbfa[], it_j1li[], it_nfes[].

    "Filtro Documento de Custo
    IF nrdocvi IS NOT INITIAL.
      CLEAR: it_vfkp[].
      SELECT * INTO TABLE it_vfkp
        FROM vfkp
       WHERE fknum IN nrdocvi.
      LOOP AT it_vfkp.
        it_vttk-tknum = it_vfkp-rebel.
        APPEND it_vttk.
      ENDLOOP.
    ENDIF.

    IF it_vttk[] IS NOT INITIAL.
      SELECT * INTO TABLE it_vttk
        FROM vttk
         FOR ALL ENTRIES IN it_vttk
       WHERE tknum EQ it_vttk-tknum.
    ENDIF.

    IF nrdoctr IS NOT INITIAL.
      SELECT * APPENDING TABLE it_vttk
        FROM vttk
       WHERE tknum IN nrdoctr.
    ENDIF.

    IF it_vttk[] IS NOT INITIAL.
      SELECT * INTO TABLE it_vttp
        FROM vttp
         FOR ALL ENTRIES IN it_vttk
       WHERE tknum EQ it_vttk-tknum.
    ENDIF.

    IF it_vttp[] IS NOT INITIAL.
      SELECT * INTO TABLE it_vbfa
        FROM vbfa
         FOR ALL ENTRIES IN it_vttp
       WHERE vbtyp_n EQ 'M'
         AND vbtyp_v EQ 'J'
         AND vbelv   EQ it_vttp-vbeln.
    ENDIF.

    IF it_vbfa[] IS NOT INITIAL.
      LOOP AT it_vbfa.
        it_j1li-reftyp = 'BI'.
        it_j1li-refkey = it_vbfa-vbeln.
        it_j1li-refitm = it_vbfa-posnn.
        APPEND it_j1li.
      ENDLOOP.

      SELECT * INTO TABLE it_j1li
        FROM j_1bnflin
         FOR ALL ENTRIES IN it_j1li
       WHERE reftyp EQ it_j1li-reftyp
         AND refkey EQ it_j1li-refkey
         AND refitm EQ it_j1li-refitm.
    ENDIF.

    IF it_j1li[] IS NOT INITIAL.
      SELECT * INTO TABLE it_actv
        FROM j_1bnfe_active
         FOR ALL ENTRIES IN it_j1li
       WHERE docnum EQ it_j1li-docnum.

      CLEAR: it_nfes[].
      LOOP AT it_actv.
        CONCATENATE it_actv-regio it_actv-nfyear it_actv-nfmonth it_actv-stcd1
                    it_actv-model it_actv-serie  it_actv-nfnum9  it_actv-docnum9
                    it_actv-cdv INTO it_nfes-n55_chave_acesso.
        APPEND it_nfes.
      ENDLOOP.

      IF it_nfes[] IS NOT INITIAL.
        SELECT * INTO TABLE it_nfes
          FROM zib_cte_dist_n55
           FOR ALL ENTRIES IN it_nfes
         WHERE n55_chave_acesso EQ it_nfes-n55_chave_acesso.

        IF sy-subrc IS NOT INITIAL.
          CLEAR: it_nfes[].
        ENDIF.
        "Pesquisa por documento de transporte e documento de custo e não localizou nf-e
        "buscar pela remessa da VT a nota fiscal e procurar documento de transporte
        LOOP AT it_nfes.
          ch_nr_cte-sign   = 'I'.
          ch_nr_cte-option = 'EQ'.
          ch_nr_cte-low    = it_nfes-cd_chave_cte.
          ch_nr_cte-high   = it_nfes-cd_chave_cte.
          APPEND ch_nr_cte.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF it_nfes[] IS INITIAL.
      ck_chave99 = abap_true.
    ENDIF.

  ENDIF.

  IF achvcte IS NOT INITIAL.
    SELECT * INTO TABLE it_ctes
      FROM zib_cte_dist_c57
     WHERE c57_chave_acesso IN achvnfe.
    IF sy-subrc IS INITIAL.
      LOOP AT it_ctes.
        ch_nr_cte2-sign   = 'I'.
        ch_nr_cte2-option = 'EQ'.
        ch_nr_cte2-low    = it_ctes-cd_chave_cte.
        ch_nr_cte2-high   = it_ctes-cd_chave_cte.
        APPEND ch_nr_cte2.
      ENDLOOP.
    ELSE.
      ck_chave99 = abap_true.
    ENDIF.
  ENDIF.

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Consulta de Números NF-e """"""""""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF anurnfe IS NOT INITIAL.
    LOOP AT anurnfe.
      ch_nr_nfe-sign   = 'I'.
      ch_nr_nfe-option = 'CP'.
      IF anurnfe-low IS NOT INITIAL.
        rg_nr_ce1-low  = anurnfe-low.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = rg_nr_ce1-low
          IMPORTING
            output = rg_nr_ce1-low.
        CONCATENATE '*' rg_nr_ce1-low '*' INTO ch_nr_nfe-low.
      ENDIF.
      IF anurnfe-high IS NOT INITIAL.
        rg_nr_ce1-high = anurnfe-high.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = rg_nr_ce1-high
          IMPORTING
            output = rg_nr_ce1-high.
        CONCATENATE '*' rg_nr_ce1-high '*' INTO ch_nr_nfe-high.
      ENDIF.
      APPEND ch_nr_nfe.
    ENDLOOP.

    CLEAR: it_nfes[].

    SELECT * INTO TABLE it_nfes
      FROM zib_cte_dist_n55
     WHERE n55_chave_acesso IN ch_nr_nfe.

    IF sy-subrc IS INITIAL.
      LOOP AT it_nfes.
        nr_nr_cte-sign   = 'I'.
        nr_nr_cte-option = 'EQ'.
        nr_nr_cte-low    = it_nfes-cd_chave_cte.
        nr_nr_cte-high   = it_nfes-cd_chave_cte.
        APPEND nr_nr_cte.
      ENDLOOP.
    ELSE.
      ck_chave99 = abap_true.
    ENDIF.
  ENDIF.

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Consulta de Números CT-e """"""""""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF anurcte IS NOT INITIAL.
    CLEAR: rg_nr_ce1.
    LOOP AT anurcte.
      ch_nr_cte3-sign   = 'I'.
      ch_nr_cte3-option = 'CP'.
      IF anurcte-low IS NOT INITIAL.
        rg_nr_ce1-low  = anurcte-low.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = rg_nr_ce1-low
          IMPORTING
            output = rg_nr_ce1-low.
        CONCATENATE '*' rg_nr_ce1-low '*' INTO ch_nr_cte3-low.
      ENDIF.
      IF anurcte-high IS NOT INITIAL.
        rg_nr_ce1-high = anurcte-high.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = rg_nr_ce1-high
          IMPORTING
            output = rg_nr_ce1-high.
        CONCATENATE '*' rg_nr_ce1-high '*' INTO ch_nr_cte3-high.
      ENDIF.
      APPEND ch_nr_cte3.
    ENDLOOP.
    CLEAR: it_ctes[].
    SELECT * INTO TABLE it_ctes
      FROM zib_cte_dist_c57
     WHERE c57_chave_acesso IN ch_nr_cte3.

    IF sy-subrc IS INITIAL.
      LOOP AT it_ctes.
        nr_nr_cte2-sign   = 'I'.
        nr_nr_cte2-option = 'EQ'.
        nr_nr_cte2-low    = it_ctes-cd_chave_cte.
        nr_nr_cte2-high   = it_ctes-cd_chave_cte.
        APPEND nr_nr_cte2.
      ENDLOOP.
    ELSE.
      ck_chave99 = abap_true.
    ENDIF.
  ENDIF.

  DATA: exc_ref    TYPE REF TO cx_sy_native_sql_error,
        error_text TYPE string.

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Consulta de Grupo de Mercadoria da NFe """"""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Consulta de Produtos NFe """"""""""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  IF codprod IS NOT INITIAL OR gruprod IS NOT INITIAL.
    CLEAR: it_nfes[].

    lc_matnr = codprod-low.
    lc_matkl = gruprod-low.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lc_matnr
      IMPORTING
        output = lc_matnr.

    IF codprod-low IS NOT INITIAL AND gruprod-low IS INITIAL.
      TRY.
          EXEC SQL.
            OPEN DOCUMENTOS FOR
              SELECT N.CD_CHAVE_CTE
                FROM SAPSR3.J_1BNFE_ACTIVE   A,
                     SAPSR3.ZIB_CTE_DIST_N55 N
               WHERE EXISTS ( SELECT * FROM SAPSR3.J_1BNFLIN L
                               WHERE L.DOCNUM = A.DOCNUM
                                 AND L.MATNR  = :LC_MATNR )
                 AND A.REGIO||A.NFYEAR||A.NFMONTH||A.STCD1||A.MODEL||A.SERIE||A.NFNUM9||A.DOCNUM9||A.CDV = N.N55_CHAVE_ACESSO
          ENDEXEC.
        CATCH cx_sy_native_sql_error INTO exc_ref.
          error_text = exc_ref->get_text( ).
          MESSAGE error_text TYPE 'E' RAISING erro_sql.
      ENDTRY.
    ELSEIF codprod-low IS NOT INITIAL AND gruprod-low IS NOT INITIAL.
      TRY.
          EXEC SQL.
            OPEN DOCUMENTOS FOR
              SELECT N.CD_CHAVE_CTE
                FROM SAPSR3.J_1BNFE_ACTIVE   A,
                     SAPSR3.ZIB_CTE_DIST_N55 N
               WHERE EXISTS ( SELECT * FROM SAPSR3.J_1BNFLIN L
                               WHERE L.DOCNUM = A.DOCNUM
                                 AND L.MATNR  = :LC_MATNR
                                 AND L.MATKL  = :LC_MATKL )
                 AND A.REGIO||A.NFYEAR||A.NFMONTH||A.STCD1||A.MODEL||A.SERIE||A.NFNUM9||A.DOCNUM9||A.CDV = N.N55_CHAVE_ACESSO
          ENDEXEC.
        CATCH cx_sy_native_sql_error INTO exc_ref.
          error_text = exc_ref->get_text( ).
          MESSAGE error_text TYPE 'E' RAISING erro_sql.
      ENDTRY.
    ELSEIF codprod-low IS INITIAL AND gruprod-low IS NOT INITIAL.
      TRY.
          EXEC SQL.
            OPEN DOCUMENTOS FOR
              SELECT N.CD_CHAVE_CTE
                FROM SAPSR3.J_1BNFE_ACTIVE   A,
                     SAPSR3.ZIB_CTE_DIST_N55 N
               WHERE EXISTS ( SELECT * FROM SAPSR3.J_1BNFLIN L
                               WHERE L.DOCNUM = A.DOCNUM
                                 AND L.MATKL  = :LC_MATKL )
                 AND A.REGIO||A.NFYEAR||A.NFMONTH||A.STCD1||A.MODEL||A.SERIE||A.NFNUM9||A.DOCNUM9||A.CDV = N.N55_CHAVE_ACESSO
          ENDEXEC.
        CATCH cx_sy_native_sql_error INTO exc_ref.
          error_text = exc_ref->get_text( ).
          MESSAGE error_text TYPE 'E' RAISING erro_sql.
      ENDTRY.
    ENDIF.

    DO.
      EXEC SQL.
        FETCH NEXT DOCUMENTOS INTO :IT_NFES-CD_CHAVE_CTE
      ENDEXEC.
      IF sy-subrc IS NOT INITIAL.
        EXIT.
      ELSE.
        APPEND it_nfes.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE DOCUMENTOS
    ENDEXEC.

    IF it_nfes[] IS NOT INITIAL.
      LOOP AT it_nfes.
        nr_nr_cte2-sign   = 'I'.
        nr_nr_cte2-option = 'EQ'.
        nr_nr_cte2-low    = it_nfes-cd_chave_cte.
        nr_nr_cte2-high   = it_nfes-cd_chave_cte.
        APPEND nr_nr_cte2.
      ENDLOOP.
    ELSE.
      ck_chave99 = abap_true.
    ENDIF.

  ENDIF.

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Documentos Cancelados """""""""""""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*  IF ck_canc EQ abap_true.
*    ck_cancela-sign   = 'I'.
*    ck_cancela-option = 'EQ'.
*    ck_cancela-low    = '101'. "Autorizado o Cancelamento
*    ck_cancela-high   = '101'. "Autorizado o Cancelamento
*    APPEND ck_cancela.
*  ENDIF.

  CASE abap_true.
    WHEN ck_auto.
      ck_cancel-sign   = 'I'.
      ck_cancel-option = 'EQ'.
      ck_cancel-low    = ' '. "Autorizado
      ck_cancel-high   = ' '.
      APPEND ck_cancel.
      CLEAR ck_cancel.
    WHEN ck_canc.
      ck_cancel-sign   = 'I'.
      ck_cancel-option = 'EQ'.
      ck_cancel-low    = 'X'. "Cancelado
      ck_cancel-high   = ' '.
      APPEND ck_cancel.
      CLEAR ck_cancel.
  ENDCASE.

  IF ck_chave99 EQ abap_true.
    CLEAR: ch_nr_cte, ch_nr_cte2.
    ch_nr_cte-sign   = 'I'.
    ch_nr_cte-option = 'EQ'.
    ch_nr_cte-low    = '9999999999999999999999999999999999999999999'.
    ch_nr_cte-high   = '9999999999999999999999999999999999999999999'.
    APPEND ch_nr_cte.
  ENDIF.

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Consulta de CT-e p/ Pagamento """""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  SELECT *
    INTO TABLE it_cte_dist
    FROM zib_cte_dist_ter
   WHERE cd_chave_cte    IN chavec
     AND cd_chave_cte    IN ch_nr_cte   "CT-e que possuem a Chave NF-e Referênciada
     AND cd_chave_cte    IN ch_nr_cte2  "CT-e que possuem a Chave CT-e Referênciada
     AND cd_chave_cte    IN nr_nr_cte   "CT-e que possuem o Número da NF-e Referênciada
     AND cd_chave_cte    IN nr_nr_cte2  "CT-e que possuem o Número da CT-e Referênciada
*     AND cd_status_sefaz IN ck_cancela  "Somente documentos cancelados
     AND cancel          IN ck_cancel
     AND ck_finalizado   IN ck_xx
     AND tp_processo_cte IN pproc
     AND docnum_cte      IN docnum
     AND numr_cte        IN rg_nr_ce
     AND dt_emissao      IN dtemit
     AND cd_tomador      IN tomado
     AND cd_modal        IN pmodal
     AND cd_tipo_servico IN tservi
     AND cd_tipo_cte     IN tipcte
     AND e_tomadora      IN etomad
     AND f_tomadora      IN ftomad
     AND p_emissor       IN ptomad
     AND emit_cnpj       IN emcnpj
     AND e_emissor       IN eemiss
     AND f_emissor       IN femiss
     AND inicio_uf       IN ufinic
     AND termino_uf      IN ufterm
     AND reme_cnpj       IN recnpj
     AND dest_cnpj       IN decnpj
     AND zdt_vencto      IN dtvenci
     AND zdt_mov         IN dtmovim
     AND dt_chegada      IN dtchega
     AND ck_autorizado   IN ck_at.

  CLEAR: it_doc_eap[].

  "01	Chegada de Documentos
  IF it_cte_dist[] IS NOT INITIAL AND ( cg_t2 EQ abap_true OR cg_t3 EQ abap_true ).
    SELECT * APPENDING TABLE it_doc_eap
      FROM zib_cte_dist_eap
       FOR ALL ENTRIES IN it_cte_dist
     WHERE cd_chave_cte  EQ it_cte_dist-cd_chave_cte
       AND tp_aprovacao  EQ '01'
       AND ck_ultimo     EQ abap_true.
  ENDIF.

  "02	Autorização de Pagamento Complemento
  IF it_cte_dist[] IS NOT INITIAL AND ( cc_t2 EQ abap_true OR cc_t3 EQ abap_true OR cc_t4 EQ abap_true ).
    SELECT * APPENDING TABLE it_doc_eap
      FROM zib_cte_dist_eap
       FOR ALL ENTRIES IN it_cte_dist
     WHERE cd_chave_cte  EQ it_cte_dist-cd_chave_cte
       AND tp_aprovacao  EQ '02'
       AND ck_ultimo     EQ abap_true.
  ENDIF.

  "03	Travar Pagamento
  IF it_cte_dist[] IS NOT INITIAL AND ( ck_t2 EQ abap_true OR ck_t3 EQ abap_true ).
    SELECT * APPENDING TABLE it_doc_eap
      FROM zib_cte_dist_eap
       FOR ALL ENTRIES IN it_cte_dist
     WHERE cd_chave_cte  EQ it_cte_dist-cd_chave_cte
       AND tp_aprovacao  EQ '03'
       AND ck_ultimo     EQ abap_true.
  ENDIF.


  SORT it_doc_eap BY cd_chave_cte tp_aprovacao.

  LOOP AT it_cte_dist.

    sy-subrc = 0.
    "01	Chegada de Documentos
    CASE abap_true.
      WHEN cg_t2.
        CLEAR: it_doc_eap.
        READ TABLE it_doc_eap WITH KEY cd_chave_cte = it_cte_dist-cd_chave_cte tp_aprovacao = '01' BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL OR it_doc_eap-tp_autorizado EQ '02'.
          sy-subrc = 0.
        ELSE.
          sy-subrc = 4.
        ENDIF.
      WHEN cg_t3.
        CLEAR: it_doc_eap.
        READ TABLE it_doc_eap WITH KEY cd_chave_cte = it_cte_dist-cd_chave_cte tp_aprovacao = '01' BINARY SEARCH.
        IF sy-subrc IS INITIAL AND it_doc_eap-tp_autorizado EQ '01'.
          sy-subrc = 0.
        ELSE.
          sy-subrc = 4.
        ENDIF.
    ENDCASE.

    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    IF it_cte_dist-cd_tipo_cte EQ '1'.
      "02	Autorização de Pagamento Complemento
      CASE abap_true.
        WHEN cc_t2.  "Pendentes
          CLEAR: it_doc_eap.
          READ TABLE it_doc_eap WITH KEY cd_chave_cte = it_cte_dist-cd_chave_cte tp_aprovacao = '02' BINARY SEARCH .
          IF sy-subrc IS NOT INITIAL.
            sy-subrc = 0.
          ELSE.
            sy-subrc = 4.
          ENDIF.
        WHEN cc_t3.  "Autorizado
          CLEAR: it_doc_eap.
          READ TABLE it_doc_eap WITH KEY cd_chave_cte = it_cte_dist-cd_chave_cte tp_aprovacao = '02' BINARY SEARCH.
          IF sy-subrc IS INITIAL AND it_doc_eap-tp_autorizado EQ '01'.
            sy-subrc = 0.
          ELSE.
            sy-subrc = 4.
          ENDIF.
        WHEN cc_t4.  "Bloqueado
          CLEAR: it_doc_eap.
          READ TABLE it_doc_eap WITH KEY cd_chave_cte = it_cte_dist-cd_chave_cte tp_aprovacao = '02' BINARY SEARCH.
          IF sy-subrc IS INITIAL AND it_doc_eap-tp_autorizado EQ '02'.
            sy-subrc = 0.
          ELSE.
            sy-subrc = 4.
          ENDIF.
      ENDCASE.

      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    "03	Travar Pagamento
    CASE abap_true.
      WHEN ck_t2. "Bloqueado
        CLEAR: it_doc_eap.
        READ TABLE it_doc_eap WITH KEY cd_chave_cte = it_cte_dist-cd_chave_cte tp_aprovacao = '03' BINARY SEARCH .
        IF sy-subrc IS INITIAL AND it_doc_eap-tp_autorizado EQ '01'.
          sy-subrc = 0.
        ELSE.
          sy-subrc = 4.
        ENDIF.
      WHEN ck_t3. "Desbloqueado
        CLEAR: it_doc_eap.
        READ TABLE it_doc_eap WITH KEY cd_chave_cte = it_cte_dist-cd_chave_cte tp_aprovacao = '03' BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL OR it_doc_eap-tp_autorizado EQ '02'.
          sy-subrc = 0.
        ELSE.
          sy-subrc = 4.
        ENDIF.
    ENDCASE.

    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    APPEND it_cte_dist TO it_cte.

  ENDLOOP.

  IF it_cte IS NOT INITIAL.

    CALL METHOD obj_cte->prepara_reg_alv_saida
      EXPORTING
        p_cte_dist = it_cte
      IMPORTING
        e_alv      = it_alv.


    "115183 CS2023000462 ZMM0079 - criação de opção de listagem de autorizações realizadas CTes - PSA
    DATA: lr_cte TYPE RANGE OF zib_cte_dist_log-cd_chave_cte.

    lr_cte[] = VALUE #( FOR wa_params1 IN it_alv WHERE ( cd_chave_cte IS NOT INITIAL ) ( option = 'EQ' sign = 'I' low = wa_params1-cd_chave_cte ) ).

    SORT lr_cte ASCENDING.

    DELETE ADJACENT DUPLICATES FROM lr_cte.

    TYPES: BEGIN OF log,
             cd_chave_cte TYPE zib_cte_dist_log-cd_chave_cte,
             message_v4   TYPE zib_cte_dist_log-message_v4,
           END OF log.
    DATA: vw_log TYPE TABLE OF log .

    SELECT DISTINCT *
       FROM zib_cte_dist_log
             INTO CORRESPONDING FIELDS OF TABLE vw_log
      WHERE cd_chave_cte IN lr_cte.
    DELETE vw_log WHERE message_v4 = space OR message_v4 = ' ' OR message_v4 = '' OR message_v4 = '000000'.

    SORT vw_log ASCENDING BY cd_chave_cte.

    DELETE ADJACENT DUPLICATES FROM vw_log COMPARING ALL FIELDS.

    LOOP AT it_alv INTO wa_alv.

      IF wa_alv-id_viagem IS INITIAL."173890 CS2021000253 ZMM0079 Incluir coluna id_viagem PSA
        SELECT SINGLE tknum FROM zib_cte_dist_n55 WHERE cd_chave_cte EQ @wa_alv-cd_chave_cte INTO @DATA(_tknum).
        IF sy-subrc = 0.
          SELECT SINGLE id_viagem FROM vttk WHERE tknum EQ @_tknum INTO @wa_alv-id_viagem.
        ENDIF.
      ENDIF.

      READ TABLE vw_log TRANSPORTING NO FIELDS WITH KEY cd_chave_cte = wa_alv-cd_chave_cte.
      IF sy-subrc = 0.
        wa_alv-ic_viewlog = icon_display.
      ENDIF.
      MOVE-CORRESPONDING wa_alv TO wa_cte_alv.
      APPEND wa_cte_alv TO it_cte_alv.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " PESQUISAR_PAGAMENTOS

*&---------------------------------------------------------------------*
*&      Form  REINICIAR_PAGAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM reiniciar_pagamentos .

  CLEAR: it_cte_selectw[].

  IF gf_authorization_ft_04 IS INITIAL.
    MESSAGE s113.
    EXIT.
  ENDIF.

  IF it_cte_select[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  LOOP AT it_cte_select.
    CALL METHOD obj_cte->ler_dados_xi
      EXPORTING
        p_chave_cte  = it_cte_select-cd_chave_cte
      EXCEPTIONS
        foreign_lock = 1
        OTHERS       = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.

  PERFORM atualizar_selecao.

ENDFORM.                    " REINICIAR_PAGAMENTOS

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info INPUT.

  CALL METHOD ctl_alv_cte->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col
      es_row_no   = gs_scroll_row.

ENDMODULE.                 " GET_SCROLL_INFO  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows INPUT.
  PERFORM popula_selecao_cte.
ENDMODULE.                 " GET_SELECTED_ROWS  INPUT

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_SELECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualizar_selecao.

  DATA: it_cte TYPE zib_cte_dist_ter_t,
        it_alv TYPE zde_cte_dist_alv_t,
        wa_alv TYPE zde_cte_dist_alv.

  CHECK it_cte_select[] IS NOT INITIAL.

  SELECT * INTO TABLE it_cte
    FROM zib_cte_dist_ter
     FOR ALL ENTRIES IN it_cte_select
   WHERE cd_chave_cte EQ it_cte_select-cd_chave_cte.

  CALL METHOD obj_cte->prepara_reg_alv_saida
    EXPORTING
      p_cte_dist = it_cte
    IMPORTING
      e_alv      = it_alv.

  LOOP AT it_alv INTO wa_alv.
    READ TABLE it_cte_alv INTO wa_cte_alv WITH TABLE KEY cd_chave_cte = wa_alv-cd_chave_cte.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING wa_alv TO wa_cte_alv.
      MODIFY it_cte_alv FROM wa_cte_alv INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  PERFORM atualiza_tela.

ENDFORM.                    " ATUALIZAR_SELECAO

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM atualiza_tela .

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  gs_alv_refres_cond-row = abap_true.
  gs_alv_refres_cond-col = abap_true.

  CALL METHOD ctl_alv_cte->refresh_table_display
    EXPORTING
      is_stable      = gs_alv_refres_cond
      i_soft_refresh = abap_true.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " ATUALIZA_TELA

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CTE_ALV_  text
*----------------------------------------------------------------------*
FORM mostrar_doc_fiscal  USING p_fiscal TYPE j_1bdocnum.

  DATA: gf_nfobjn LIKE j_1binterf-nfobjn.

  CHECK p_fiscal IS NOT INITIAL.

  CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
    EXPORTING
      doc_number         = p_fiscal
    IMPORTING
      obj_number         = gf_nfobjn
    EXCEPTIONS
      document_not_found = 1
      docum_lock         = 2
      OTHERS             = 3.

  CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
    EXPORTING
      obj_number         = gf_nfobjn
    EXCEPTIONS
      object_not_found   = 1
      scr_ctrl_not_found = 2
      OTHERS             = 3.

ENDFORM.                    " MOSTRAR_DOC_FISCAL

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_PEDIDO
*&---------------------------------------------------------------------*
FORM mostrar_pedido  USING  p_ebeln TYPE ebeln.

  IF p_ebeln IS NOT INITIAL.
    SET PARAMETER ID 'BES' FIELD p_ebeln.
    CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_PEDIDO

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_FATURA
*&---------------------------------------------------------------------*
FORM mostrar_fatura  USING    p_belnr TYPE re_belnr
                              p_gjahr TYPE gjahr.
  IF p_belnr IS NOT INITIAL AND p_gjahr IS NOT INITIAL.
    SET PARAMETER ID 'RBN' FIELD p_belnr.
    SET PARAMETER ID 'GJR' FIELD p_gjahr.
    CALL TRANSACTION  'MIR4' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.                    " MOSTRAR_FATURA

*&---------------------------------------------------------------------*
*&      Form  GERAR_PAGAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gerar_pagamentos USING p_estornar TYPE char01.

  CLEAR: it_cte_selectw[].

  IF gf_authorization_ft_05 IS INITIAL AND p_estornar IS INITIAL.
    MESSAGE s114.
    EXIT.
  ENDIF.

  IF gf_authorization_ft_06 IS INITIAL AND p_estornar IS NOT INITIAL.
    MESSAGE s115.
    EXIT.
  ENDIF.

  IF it_cte_select[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  LOOP AT it_cte_select INTO wa_cte_select.

    CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
      EXPORTING
        chave          = wa_cte_select-cd_chave_cte
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      CONTINUE.
    ENDIF.

    it_chaves_bloq-cd_chave_cte = wa_cte_select-cd_chave_cte.
    APPEND it_chaves_bloq.

    CALL METHOD obj_cte->gerar_fatura_frete
      EXPORTING
        p_chave_cte         = wa_cte_select-cd_chave_cte
        p_estornar          = p_estornar
      CHANGING
        p_cte               = wa_cte_select
      EXCEPTIONS
        nao_enc_frete       = 1
        fatura              = 2
        pedido              = 3
        cod_iva             = 4
        banco_parceiro      = 5
        param_ctb           = 6
        banco_empresa       = 7
        sem_vt              = 8
        erro_entrada_fiscal = 9
        miro_compensada     = 11
        peso_chegada        = 12
        OTHERS              = 13.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
      EXPORTING
        chave = wa_cte_select-cd_chave_cte.

    DELETE it_chaves_bloq WHERE cd_chave_cte EQ wa_cte_select-cd_chave_cte.

  ENDLOOP.

  PERFORM atualizar_selecao.

ENDFORM.                    " GERAR_PAGAMENTOS

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM visualizar_cte .

  CLEAR: it_cte_selectw[].

  IF gf_authorization_ft_03 IS INITIAL.
    MESSAGE s112.
    EXIT.
  ENDIF.

  IF it_cte_select[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  READ TABLE it_cte_select INDEX 1.

  SELECT SINGLE * INTO zib_cte_dist_ter
    FROM zib_cte_dist_ter
   WHERE cd_chave_cte EQ it_cte_select-cd_chave_cte.

  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
      EXPORTING
        chave          = zib_cte_dist_ter-cd_chave_cte
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    PERFORM busca_info_notas_cte_cte.

    CALL SCREEN 0101 STARTING AT 15 01.

    CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
      EXPORTING
        chave = zib_cte_dist_ter-cd_chave_cte.
  ENDIF.

ENDFORM.                    " VISUALIZAR_CTE

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit0101 INPUT.

  CLEAR: vg_tela_0101.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " USER_COMMAND_EXIT0101  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  CLEAR: it_exclude_fcode.

  "IF ZIB_CTE_DIST_TER-CK_FINALIZADO IS NOT INITIAL.
  wa_exclude_fcode = ok_salvar.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  "ENDIF.

  "04	Ferroviário
  IF zib_cte_dist_ter-cd_modal NE '04'.
    wa_exclude_fcode = ok_detvagoes.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
  ENDIF.

  SET PF-STATUS 'PFMODALP' EXCLUDING it_exclude_fcode.
  SET TITLEBAR 'TL0101'.

  IF vg_tela_0101 IS INITIAL.
    vg_tela_0101 = tl_0102.
    tabprinc-activetab = ok_tabparc.
  ENDIF.

ENDMODULE.                 " STATUS_0101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT0101C  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit0101c INPUT.

  CASE ok_code.
    WHEN ok_tabparc.
      vg_tela_0101 = tl_0102.
      tabprinc-activetab = ok_tabparc.
    WHEN ok_tabnf55.
      vg_tela_0101 = tl_0103.
      tabprinc-activetab = ok_tabnf55.
    WHEN ok_tabnf01.
      vg_tela_0101 = tl_0104.
      tabprinc-activetab = ok_tabnf01.
    WHEN ok_tabdocs.
      vg_tela_0101 = tl_0105.
      tabprinc-activetab = ok_tabdocs.
    WHEN ok_tabct57.
      vg_tela_0101 = tl_0106.
      tabprinc-activetab = ok_tabct57.
    WHEN ok_tabmodal.
      vg_tela_0101 = tl_0113.
      tabprinc-activetab = ok_tabmodal.
    WHEN ok_salvar.
      PERFORM salvar_ajuste_manual.
    WHEN ok_compvalor.
      PERFORM mostrar_compo_vlr_pretacao.
    WHEN ok_detvagoes.
      PERFORM mostrar_detalhe_vagoes.
    WHEN ok_duplicata.
      PERFORM mostrar_duplicatas.
    WHEN ok_comptext.
      PERFORM mostrar_dados_comple_texto.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_EXIT0101C  INPUT

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_FATURA_VF
*&---------------------------------------------------------------------*
FORM mostrar_fatura_vf  USING p_vbeln_vf TYPE vbeln_vf.

  IF p_vbeln_vf IS NOT INITIAL.
    SET PARAMETER ID 'VF' FIELD p_vbeln_vf.
    CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_FATURA_VF

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_REMESSA_VL
*&---------------------------------------------------------------------*
FORM mostrar_remessa_vl  USING p_vbeln_vl TYPE vbeln_vl.

  IF p_vbeln_vl IS NOT INITIAL.
    SET PARAMETER ID 'VL' FIELD p_vbeln_vl.
    CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_REMESSA_VL

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_TRANSPORTE
*&---------------------------------------------------------------------*
FORM mostrar_doc_transporte  USING  p_tknum TYPE tknum.

  IF p_tknum IS NOT INITIAL.
    SET PARAMETER ID 'TNR' FIELD p_tknum.
    CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
  ENDIF.


ENDFORM.                    " MOSTRAR_DOC_TRANSPORTE

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_CUSTO_TRANS
*&---------------------------------------------------------------------*
FORM mostrar_doc_custo_trans  USING p_fknum TYPE fknum.

  IF p_fknum IS NOT INITIAL.
    SET PARAMETER ID 'FKK' FIELD p_fknum.
    CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_CUSTO_TRANS

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mostrar_doc_material  USING p_mblnr TYPE mblnr
                                 p_mjahr TYPE mjahr.

  IF p_mblnr IS NOT INITIAL.
* ---> S4 Migration - 19/07/2023 - LO
*    SET PARAMETER ID 'MBN' FIELD p_mblnr.
*    SET PARAMETER ID 'MJA' FIELD p_mjahr.
*    CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

    CALL FUNCTION 'MIGO_DIALOG'
      EXPORTING
        i_action            = 'A04'
        i_refdoc            = 'R02'
        i_notree            = 'X'
        i_no_auth_check     = ''
        i_skip_first_screen = 'X'
        i_deadend           = 'X'
        i_okcode            = 'OK_GO'
        i_mblnr             = p_mblnr
        i_mjahr             = p_mjahr
      EXCEPTIONS
        illegal_combination = 1
        OTHERS              = 2.
* <--- S4 Migration - 19/07/2023 - LO
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  SALVAR_AJUSTE_MANUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_ajuste_manual .

  DATA: lc_cte_n55 TYPE zib_cte_dist_n55,
        lc_cte_n01 TYPE zib_cte_dist_n01.

  LOOP AT it_cte_nit INTO wa_cte_nit.
    MODIFY zib_cte_dist_nit FROM wa_cte_nit.
  ENDLOOP.

  LOOP AT it_cte_n55 INTO wa_cte_n55.
    MOVE-CORRESPONDING wa_cte_n55 TO lc_cte_n55.
    MODIFY zib_cte_dist_n55 FROM lc_cte_n55.
  ENDLOOP.

  LOOP AT it_cte_n01 INTO wa_cte_n01.
    MOVE-CORRESPONDING wa_cte_n01 TO lc_cte_n01.
    MODIFY zib_cte_dist_n01 FROM lc_cte_n01.
  ENDLOOP.

  MODIFY zib_cte_dist_ter.

  MESSAGE s059.

  LEAVE TO SCREEN 0.

ENDFORM.                    " SALVAR_AJUSTE_MANUAL

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant2 .

  gs_variant2-report      = sy-repid.
  gs_variant2-handle      = '0002'.
  gs_variant2-log_group   = abap_false.
  gs_variant2-username    = abap_false.
  gs_variant2-variant     = abap_false.
  gs_variant2-text        = abap_false.
  gs_variant2-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT2

*&---------------------------------------------------------------------*
*&      Form  DIGITAR_INFO_FATURA_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM digitar_info_fatura_cte .

  DATA: it_index_rows TYPE lvc_t_row,
        wa_index_rows TYPE lvc_s_row,
        it_cte        TYPE zib_cte_dist_ter_t.

  CLEAR: it_cte_selectw[].

  IF gf_authorization_ft_02 IS INITIAL.
    MESSAGE s111.
    EXIT.
  ENDIF.

  IF it_cte_select[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  CLEAR: it_cte.

  LOOP AT it_cte_select.
    APPEND it_cte_select TO it_cte.
  ENDLOOP.

  CALL METHOD zcl_cte_dist_g=>informar_dados_faturamento
    EXPORTING
      i_cte = it_cte.

*    LOOP AT IT_CTE_SELECT.
*
*      CLEAR: ZIB_CTE_DIST_TER.
*
*      SELECT SINGLE * INTO ZIB_CTE_DIST_TER
*        FROM ZIB_CTE_DIST_TER
*       WHERE CD_CHAVE_CTE EQ IT_CTE_SELECT-CD_CHAVE_CTE.
*
*      IF SY-SUBRC IS INITIAL.
*
*        CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
*          EXPORTING
*            CHAVE          = IT_CTE_SELECT-CD_CHAVE_CTE
*          EXCEPTIONS
*            FOREIGN_LOCK   = 1
*            SYSTEM_FAILURE = 2
*            OTHERS         = 3.
*
*        IF SY-SUBRC IS NOT INITIAL.
*          MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*          PERFORM ATUALIZAR_SELECAO.
*          EXIT.
*        ENDIF.
*
*        PERFORM CHAMA_TELA_FATURAMENTO CHANGING ZIB_CTE_DIST_TER.
*
*        READ TABLE IT_CTE_ALV INTO WA_CTE_ALV WITH KEY CD_CHAVE_CTE = IT_CTE_SELECT-CD_CHAVE_CTE.
*        IF SY-SUBRC IS INITIAL.
*          WA_INDEX_ROWS-INDEX = SY-TABIX.
*          APPEND WA_INDEX_ROWS TO IT_INDEX_ROWS.
*
*          CALL METHOD CTL_ALV_CTE->SET_SELECTED_ROWS
*            EXPORTING
*              IT_INDEX_ROWS = IT_INDEX_ROWS.
*        ENDIF.
*
*        CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
*          EXPORTING
*            CHAVE = IT_CTE_SELECT-CD_CHAVE_CTE.
*
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDIF.

  PERFORM atualizar_selecao.

ENDFORM.                    " DIGITAR_INFO_FATURA_CTE


*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM handle_double_click  USING p_row TYPE lvc_s_row.
  DATA: lc_row TYPE lvc_t_row.

  PERFORM limpar_bloqs.

  IF p_row-rowtype IS INITIAL.

    APPEND p_row TO lc_row.

    CALL METHOD ctl_alv_cte->set_selected_rows
      EXPORTING
        it_index_rows = lc_row.

    READ TABLE it_cte_alv INDEX p_row-index INTO wa_cte_alv.

    PERFORM popula_selecao_cte.

    IF wa_cte_alv-ck_finalizado EQ abap_true.
      PERFORM visualizar_cte.
    ELSE.
      PERFORM digitar_info_fatura_cte.
    ENDIF.
  ENDIF.
ENDFORM.                    " HANDLE_DOUBLE_CLICK


*&---------------------------------------------------------------------*
*&      Form  BUSCA_INFO_NOTAS_CTE_CTE
*&---------------------------------------------------------------------*
FORM busca_info_notas_cte_cte.

  DATA: it_n55_t TYPE zib_cte_dist_n55_t,
        it_n01_t TYPE zib_cte_dist_n01_t,
        it_nit_t TYPE zib_cte_dist_nit_t,
        it_c57_t TYPE zib_cte_dist_c57_t,
        it_vei_t TYPE zib_cte_dist_vei_t,
        it_mot_t TYPE zib_cte_dist_mot_t,
        wa_n55_t TYPE zib_cte_dist_n55,
        wa_n01_t TYPE zib_cte_dist_n01,
        wa_nit_t TYPE zib_cte_dist_nit,
        wa_c57_t TYPE zib_cte_dist_c57,
        wa_vei_t TYPE zib_cte_dist_vei,
        wa_mot_t TYPE zib_cte_dist_mot.

  CLEAR: it_cte_n55, it_cte_n01, wa_info_forne.

  "Busca Notas Fiscais Modelo 55
  CALL METHOD zcl_cte_dist_g=>busca_notas_n55
    EXPORTING
      p_chave_cte = zib_cte_dist_ter-cd_chave_cte
    IMPORTING
      e_n55_t     = it_n55_t.

  CLEAR: it_cte_n55.
  LOOP AT it_n55_t INTO wa_n55_t.
    MOVE-CORRESPONDING wa_n55_t TO wa_cte_n55.
    wa_cte_n55-ic_dadosc = icon_ws_truck.
    IF ( wa_cte_n55-ck_peso_digitado EQ abap_true ) OR ( zib_cte_dist_ter-ck_peso_chegada EQ abap_true ).
      wa_cte_n55-ic_editar = icon_set_state.
    ELSE.
      wa_cte_n55-ic_editar = icon_change_number.
    ENDIF.
    APPEND wa_cte_n55 TO it_cte_n55.
  ENDLOOP.

  "Busca Notas Fiscais Modelo 01
  CALL METHOD zcl_cte_dist_g=>busca_notas_n01
    EXPORTING
      p_chave_cte = zib_cte_dist_ter-cd_chave_cte
    IMPORTING
      e_n01_t     = it_n01_t.

  CLEAR: it_cte_n01.
  LOOP AT it_n01_t INTO wa_n01_t.
    MOVE-CORRESPONDING wa_n01_t TO wa_cte_n01.
    IF ( wa_cte_n01-ck_peso_digitado EQ abap_true ) OR ( zib_cte_dist_ter-ck_peso_chegada EQ abap_true ).
      wa_cte_n01-ic_editar = icon_set_state.
    ELSE.
      wa_cte_n01-ic_editar = icon_change_number.
    ENDIF.
    APPEND wa_cte_n01 TO it_cte_n01.
  ENDLOOP.

  "Busca Conhecimento de Transporte Modelo 57
  CALL METHOD zcl_cte_dist_g=>busca_notas_c57
    EXPORTING
      p_chave_cte = zib_cte_dist_ter-cd_chave_cte
    IMPORTING
      e_c57_t     = it_c57_t.

  CLEAR: it_cte_c57.
  LOOP AT it_c57_t INTO wa_c57_t.
    MOVE-CORRESPONDING wa_c57_t TO wa_cte_c57.
    APPEND wa_cte_c57 TO it_cte_c57.
  ENDLOOP.

  CALL METHOD zcl_cte_dist_g=>busca_notas_itens
    EXPORTING
      p_chave_cte = zib_cte_dist_ter-cd_chave_cte
    IMPORTING
      e_nit_t     = it_nit_t.

  CLEAR: it_cte_nit.

  LOOP AT it_nit_t INTO wa_nit_t.
    MOVE-CORRESPONDING wa_nit_t TO wa_cte_nit.
    APPEND wa_cte_nit TO it_cte_nit.
  ENDLOOP.

  "Busca Veículos do CT-e
  CALL METHOD zcl_cte_dist_g=>busca_veiculos
    EXPORTING
      p_chave_cte = zib_cte_dist_ter-cd_chave_cte
    IMPORTING
      e_vei_t     = it_vei_t.

  CLEAR: it_cte_vei.
  LOOP AT it_vei_t INTO wa_vei_t.
    MOVE-CORRESPONDING wa_vei_t TO wa_cte_vei.
    APPEND wa_cte_vei TO it_cte_vei.
  ENDLOOP.

  "Busca Motoristas do CT-e
  CALL METHOD zcl_cte_dist_g=>busca_motoristas
    EXPORTING
      p_chave_cte = zib_cte_dist_ter-cd_chave_cte
    IMPORTING
      e_mot_t     = it_mot_t.

  CLEAR: it_cte_mot.
  LOOP AT it_mot_t INTO wa_mot_t.
    MOVE-CORRESPONDING wa_mot_t TO wa_cte_mot.
    APPEND wa_cte_mot TO it_cte_mot.
  ENDLOOP.

ENDFORM.                    " BUSCA_INFO_NOTAS_CTE_CTE

*&---------------------------------------------------------------------*
*&      Form  AUTORIZAR_PAGAMENTO_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM autorizar_pagamento_cte .

  DATA: lc_autoriza     TYPE c LENGTH 1,
        lc_matnr        TYPE matnr,
        lc_grupo        TYPE matkl,
        lc_tipo         TYPE zde_tp_aut_frete,
        e_tp_aprovacao  TYPE zde_tip_aprovacao,
        e_domname       TYPE char30,
        e_cd_aprovacao  TYPE zde_est_aprovacao,
        e_tp_autorizado TYPE zde_tp_autorizacao.

  DATA: lv_domname    TYPE char30,
        lv_domvalue_l TYPE zde_tip_aprovacao,
        lv_ddtext	    TYPE val_text.

  DATA: gv_longtext_tab TYPE catsxt_longtext_itab,
        gv_confirmado   TYPE abap_bool,
        gv_prim         TYPE abap_bool.

  "IF GF_AUTHORIZATION_FT_01 IS INITIAL.
  "  MESSAGE S149.
  "  EXIT.
  "ENDIF.

  CLEAR: it_cte_selectw[].

  IF it_cte_select[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  FREE MEMORY ID 'B_MULT'.
  FREE MEMORY ID 'B_MULTI'.
  FREE MEMORY ID 'B_MULT_P'.
  MOVE abap_false TO gv_prim.
  EXPORT: gv_prim TO MEMORY ID 'B_MULT_P'.

*  READ TABLE IT_CTE_SELECT INDEX 1.
  LOOP AT it_cte_select.

    CALL METHOD zcl_cte_dist_g=>autorizacao
      EXPORTING
        i_cd_chave_cte  = it_cte_select-cd_chave_cte
      IMPORTING
        e_cd_aprovacao  = e_cd_aprovacao
        e_tp_autorizado = e_tp_autorizado
      CHANGING
        i_e_domname     = e_domname
        i_tp_aprovacao  = e_tp_aprovacao
      EXCEPTIONS
        erro            = 1
        cte_finalizado  = 2
        cte_nao_loc     = 3
        cte_bloqueada   = 4
        sem_autorizacao = 5
        OTHERS          = 6.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    IF e_tp_aprovacao EQ '01' AND e_domname EQ 'ZDM_AUT_FRETE_TER'.

      lc_autoriza = abap_false.

      SELECT SINGLE * INTO zib_cte_dist_ter
        FROM zib_cte_dist_ter
       WHERE cd_chave_cte EQ it_cte_select-cd_chave_cte.

      IF sy-subrc IS INITIAL AND zib_cte_dist_ter-cd_modal NE '04'.
        PERFORM busca_info_notas_cte_cte.

        lc_autoriza = abap_false.

        LOOP AT it_cte_nit INTO wa_cte_nit.
          IF wa_cte_nit-docnum IS NOT INITIAL.

            SELECT SINGLE matnr INTO lc_matnr
              FROM j_1bnflin
             WHERE docnum EQ wa_cte_nit-docnum
               AND itmnum EQ wa_cte_nit-itmnum.

            SELECT SINGLE matkl INTO lc_grupo
              FROM mara
             WHERE matnr EQ lc_matnr.

            SELECT SINGLE tp_aut_frete
              INTO lc_tipo
              FROM zib_cte_dist_gm
             WHERE matkl EQ lc_grupo.

            AUTHORITY-CHECK OBJECT 'ZAUTOFTTER' ID 'ZAUTOFTTER' FIELD lc_tipo.
            IF sy-subrc IS INITIAL.
              lc_autoriza = abap_true.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF lc_autoriza EQ abap_true.

          CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
            EXPORTING
              chave          = zib_cte_dist_ter-cd_chave_cte
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc IS NOT INITIAL.
            MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            EXIT.
          ENDIF.

          CASE lc_tipo.
            WHEN '01'. "Altorização de Pagamento de Venda de Fardos de Algodão
              PERFORM popula_informacoes_algodao.
              IF sy-subrc IS INITIAL.
                CALL SCREEN 0401 STARTING AT 15 02.
              ENDIF.
          ENDCASE.

          CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
            EXPORTING
              chave = zib_cte_dist_ter-cd_chave_cte.

        ENDIF.
      ENDIF.
    ELSEIF e_tp_aprovacao EQ '08' AND e_domname EQ 'ZDM_AUT_FRETE_TER'.
      PERFORM liberar_diferenca_peso.
    ELSEIF e_cd_aprovacao IS NOT INITIAL.
      PERFORM atualizar_selecao.
    ENDIF.

  ENDLOOP.

  FREE: lv_domname, lv_domvalue_l, lv_ddtext.
  EXPORT: lv_domname
          lv_domvalue_l
          lv_ddtext TO MEMORY ID 'B_MULT'.

  FREE: gv_longtext_tab, gv_confirmado, gv_prim.
  EXPORT: gv_longtext_tab
          gv_confirmado TO MEMORY ID 'B_MULTI'.

  EXPORT: gv_prim TO MEMORY ID 'B_MULT_P'.

  FREE MEMORY ID: 'B_MULT',
                  'B_MULTI',
                  'B_MULT_P'.

ENDFORM.                    " AUTORIZAR_PAGAMENTO_CTE

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_BLOQS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM limpar_bloqs .

  LOOP AT it_chaves_bloq.
    CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
      EXPORTING
        chave = it_chaves_bloq-cd_chave_cte.
  ENDLOOP.

  CLEAR: it_chaves_bloq[].

ENDFORM.                    " LIMPAR_BLOQS

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_COMPO_VLR_PRETACAO
*&---------------------------------------------------------------------*
*       Mostrar Componentes do Valor da Prestação
*----------------------------------------------------------------------*
FORM mostrar_compo_vlr_pretacao .

  CLEAR: it_cte_cvl[].

  SELECT * INTO TABLE it_cte_cvl
    FROM zib_cte_dist_cvl
   WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte.

  IF it_cte_cvl[] IS NOT INITIAL.
    CALL SCREEN 0107 STARTING AT 30 5.
  ELSE.
    MESSAGE s116.
  ENDIF.

ENDFORM.                    " MOSTRAR_COMPO_VLR_PRETACAO

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DETALHE_VAGOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mostrar_detalhe_vagoes .

  CLEAR: it_cte_vga[].

  SELECT * INTO TABLE it_cte_vga
    FROM zib_cte_dist_vga
   WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte.

  IF it_cte_vga[] IS NOT INITIAL.
    CALL SCREEN 0108 STARTING AT 30 5.
  ELSE.
    MESSAGE s117.
  ENDIF.

ENDFORM.                    " MOSTRAR_DETALHE_VAGOES

*&---------------------------------------------------------------------*
*&      Form  GERAR_DOCUMENTO_TRANSPORTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM gerar_documento_transporte .

  CLEAR: it_cte_selectw[].

  IF gf_authorization_ft_07 IS INITIAL.
    MESSAGE s128.
    EXIT.
  ENDIF.

  IF it_cte_select[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  LOOP AT it_cte_select INTO wa_cte_select.

    CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
      EXPORTING
        chave          = wa_cte_select-cd_chave_cte
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      CONTINUE.
    ENDIF.

    it_chaves_bloq-cd_chave_cte = wa_cte_select-cd_chave_cte.
    APPEND it_chaves_bloq.

    "178025 CS2023000574 Job dinâmico PSA
*    IF 1 = 1.
*      PERFORM job_create CHANGING wa_cte_select-cd_chave_cte.
*    ELSE.
    "178025 CS2023000574 Job dinâmico PSA
    CALL METHOD obj_cte->gerar_doc_transporte
      EXPORTING
        p_cte_chave = wa_cte_select-cd_chave_cte
      EXCEPTIONS
        doc_transp  = 1
        OTHERS      = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*    ENDIF.



    "178025 CS2023000574 Job dinâmico PSA
*    CALL METHOD obj_cte->gerar_doc_transporte
*      EXPORTING
*        p_cte_chave = wa_cte_select-cd_chave_cte
*      EXCEPTIONS
*        doc_transp  = 1
*        OTHERS      = 2.

*    IF sy-subrc IS NOT INITIAL.
*      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

    CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
      EXPORTING
        chave = wa_cte_select-cd_chave_cte.

    DELETE it_chaves_bloq WHERE cd_chave_cte EQ wa_cte_select-cd_chave_cte.

  ENDLOOP.

  PERFORM atualizar_selecao.

ENDFORM.                    " GERAR_DOCUMENTO_TRANSPORTE

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DUPLICATAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mostrar_duplicatas .

  CLEAR: it_cte_dup[].

  SELECT * INTO TABLE it_cte_dup
    FROM zib_cte_dist_dup
   WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte.

  IF it_cte_dup[] IS NOT INITIAL.
    CALL SCREEN 0111 STARTING AT 30 5.
  ELSE.
    MESSAGE s145.
  ENDIF.

ENDFORM.                    " MOSTRAR_DUPLICATAS

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DADOS_COMPLE_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mostrar_dados_comple_texto .

  CLEAR: it_cte_cpl[].

  SELECT * INTO TABLE it_cte_cpl
    FROM zib_cte_dist_cpl
   WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte.

  IF it_cte_cpl[] IS NOT INITIAL.
    CALL SCREEN 0112 STARTING AT 30 5.
  ELSE.
    MESSAGE s146.
  ENDIF.

ENDFORM.                    " MOSTRAR_DADOS_COMPLE_TEXTO

*&---------------------------------------------------------------------*
*&      Form  LIBERAR_DIFERENCA_PESO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM liberar_diferenca_peso .

  "IF GF_AUTHORIZATION_FT_08 IS INITIAL.
  "  MESSAGE S149.
  "  EXIT.
  "ENDIF.

*  IF IT_CTE_SELECT[] IS INITIAL.
*    MESSAGE S022.
*    RETURN.
*  ENDIF.

  LOOP AT it_cte_select INTO wa_cte_select.

    CALL METHOD obj_cte->ler_dados_xi
      EXPORTING
        p_chave_cte  = wa_cte_select-cd_chave_cte
      EXCEPTIONS
        foreign_lock = 1
        OTHERS       = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
        EXPORTING
          chave          = wa_cte_select-cd_chave_cte
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        CONTINUE.
      ENDIF.

      it_chaves_bloq-cd_chave_cte = wa_cte_select-cd_chave_cte.
      APPEND it_chaves_bloq.

      CALL SCREEN 0501 STARTING AT 30 10.

      CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
        EXPORTING
          chave = wa_cte_select-cd_chave_cte.

      DELETE it_chaves_bloq WHERE cd_chave_cte EQ wa_cte_select-cd_chave_cte.
    ENDIF.

  ENDLOOP.

  PERFORM atualizar_selecao.

ENDFORM.                    " LIBERAR_DIFERENCA_PESO

*&---------------------------------------------------------------------*
*&      Form  HABILITAR_WORKFLOW_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM habilitar_workflow_documentos .

  CLEAR: it_cte_selectw, it_cte_selectw[].

*  IF GF_AUTHORIZATION_FT_09 IS INITIAL.
*    MESSAGE S128.
*    EXIT.
*  ENDIF.
  "Somente validar acesso para modificar
  IF it_cte_select[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  READ TABLE it_cte_select INDEX 1 INTO it_cte_selectw.
  APPEND it_cte_selectw.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_TABELA_BASE_CUSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualizar_tabela_base_custo .

  CLEAR: it_cte_selectw[].

  IF gf_authorization_ft_10 IS INITIAL.
    MESSAGE s190.
    EXIT.
  ENDIF.

  IF it_cte_select[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  LOOP AT it_cte_select.

    CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
      EXPORTING
        chave          = it_cte_select-cd_chave_cte
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    CALL METHOD zcl_cte_dist_g=>atribui_dados_vt
      EXPORTING
        p_chave_cte = it_cte_select-cd_chave_cte
      EXCEPTIONS
        erro        = 1
        OTHERS      = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
      EXPORTING
        chave = it_cte_select-cd_chave_cte.

  ENDLOOP.

  PERFORM atualizar_selecao.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_DACTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM visualizar_dacte .

  IF it_cte_select[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  LOOP AT it_cte_select INTO DATA(wa_cte_select).
    TRY .
        zcl_cte_dist_g=>dacte( i_cte = wa_cte_select-cd_chave_cte ).
      CATCH zcx_cte_inbound INTO DATA(ex_cte_inbound).
        ex_cte_inbound->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  IMP_ARQUIVO_CHEGADA_ALGODAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imp_arquivo_chegada_algodao .

  DATA: wa_cargas     TYPE zde_imp_algodao_chegada,
        lc_file_table	TYPE filetable,
        lc_rc	        TYPE i,
        lc_cnpj       TYPE c LENGTH 14.

  CALL FUNCTION 'NUMBER_RANGE_OBJECT_GET_INFO'
    EXPORTING
      object           = 'ZIDIMPALGO'
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s213 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title            = 'Arquivo de Chegada de Carga de Algodão'
      default_extension       = '.xls'
      file_filter             = '.xls'
    CHANGING
      file_table              = lc_file_table
      rc                      = lc_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5 ).

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lc_file_table INTO DATA(wa_file_table).
    TRY .
        DATA(lc_arquivo) = zcl_arquivo=>get_file_xls( i_file_name = CONV #( wa_file_table-filename ) ).
      CATCH zcx_arquivo INTO DATA(erro_arq).    "
        erro_arq->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDLOOP.

  TRY .
      LOOP AT lc_arquivo INTO DATA(wa_arquivo).
        IF wa_arquivo-row LE 3.
          CONTINUE.
        ENDIF.

        CASE wa_arquivo-col.
          WHEN 1. "Placa
            CLEAR: wa_cargas.
            wa_cargas-ds_placa = wa_arquivo-value.
          WHEN 2. "CNPJ

            REPLACE ALL OCCURRENCES OF '.' IN wa_arquivo-value WITH '' IGNORING CASE.
            REPLACE ALL OCCURRENCES OF '/' IN wa_arquivo-value WITH '' IGNORING CASE.
            REPLACE ALL OCCURRENCES OF '-' IN wa_arquivo-value WITH '' IGNORING CASE.
            lc_cnpj = wa_arquivo-value.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = lc_cnpj
              IMPORTING
                output = lc_cnpj.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lc_cnpj
              IMPORTING
                output = lc_cnpj.

            CALL FUNCTION 'CONVERSION_EXIT_CGCBR_INPUT'
              EXPORTING
                input  = lc_cnpj
              IMPORTING
                output = wa_cargas-reme_cnpj.

            wa_cargas-reme_cnpj = wa_arquivo-value.

          WHEN 3. "Nota Fiscal
            IF wa_cargas-ds_placa NE '-'.
              wa_cargas-nfenum = wa_arquivo-value.
              CONDENSE wa_cargas-nfenum NO-GAPS.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_cargas-nfenum
                IMPORTING
                  output = wa_cargas-nfenum.
            ENDIF.
          WHEN 4. "Lote
            IF wa_cargas-ds_placa NE '-'.
              wa_cargas-lote = wa_arquivo-value.
            ENDIF.
          WHEN 5. "Qtd. Fardos
            IF wa_cargas-ds_placa NE '-'.
              wa_cargas-qtd_fardos = wa_arquivo-value.
              wa_cargas-und_voleh  = 'FD'.
            ENDIF.
          WHEN 6. "Data da Chegada
            IF wa_cargas-ds_placa NE '-'.
              CONCATENATE wa_arquivo-value+6(4) wa_arquivo-value+3(2) wa_arquivo-value(2) INTO wa_cargas-dt_chegada .
            ENDIF.
          WHEN 7. "Instrução
            IF wa_cargas-ds_placa NE '-'.
              wa_cargas-instrucao   = wa_arquivo-value.
              wa_cargas-dt_registro = sy-datlo.
              wa_cargas-hr_registro = sy-timlo.
              wa_cargas-us_registro = sy-uname.
              APPEND wa_cargas TO it_cargas.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    CATCH cx_root.
      DATA(texto) = 'Placa:' && wa_cargas-ds_placa && ' Erro em colina/linha: ' && wa_arquivo-col && '/' && wa_arquivo-row && ' Valor: ' && wa_arquivo-value.
      MESSAGE texto TYPE 'W'.
  ENDTRY.

  LOOP AT it_cargas ASSIGNING FIELD-SYMBOL(<fs_carga>) WHERE reme_cnpj IS NOT INITIAL.

    SELECT SINGLE * INTO @DATA(wa_j_1bnfe_active)
      FROM j_1bnfe_active
     WHERE stcd1  EQ @<fs_carga>-reme_cnpj
       AND model  EQ '55'
       AND serie  EQ '000'
       AND nfnum9 EQ @<fs_carga>-nfenum.

    IF sy-subrc IS INITIAL.
      <fs_carga>-docnum = wa_j_1bnfe_active-docnum.
      <fs_carga>-series = wa_j_1bnfe_active-serie.

      SELECT SINGLE * INTO @DATA(wa_romaneio)
        FROM zsdt0001
       WHERE nro_nf_prod EQ @wa_j_1bnfe_active-docnum.

      IF sy-subrc IS INITIAL.
        <fs_carga>-ch_referencia = wa_romaneio-ch_referencia.
        <fs_carga>-nr_romaneio   = wa_romaneio-nr_romaneio.
        <fs_carga>-nr_safra      = wa_romaneio-nr_safra.
        <fs_carga>-bukrs         = wa_romaneio-bukrs.
        <fs_carga>-branch        = wa_romaneio-branch.
        <fs_carga>-doc_rem       = wa_romaneio-doc_rem.
        <fs_carga>-tknum         = wa_romaneio-doc_transp.
        <fs_carga>-fknum         = wa_romaneio-fknum.
      ENDIF.

    ENDIF.

  ENDLOOP.

  CALL SCREEN 0503.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0503_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0503_exit INPUT.
  PERFORM limpar_0503.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0503  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0503 OUTPUT.

  SET PF-STATUS 'PF0503'.
  SET TITLEBAR 'TL0503'.

  IF splitter_0503 IS INITIAL.

    CREATE OBJECT splitter_0503
      EXPORTING
        parent  = cl_gui_container=>screen0 "CTL_CCCONTAINER
        rows    = 1
        columns = 1.

    CALL METHOD splitter_0503->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = ctl_cccontainer_0503.

    CREATE OBJECT ctl_alv_0503
      EXPORTING
        i_parent = ctl_cccontainer_0503.

    PERFORM fill_it_fieldcatalog_0503.

    PERFORM fill_it_hints_0503.

    PERFORM fill_gs_variant_0503.

    ctl_alv_0503->set_table_for_first_display(
      EXPORTING
        is_layout       = gs_layout_0503
        is_variant      = gs_variant_0503
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_0503
        it_outtab       = it_cargas[] ).

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0503
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0503 .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_0503[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_IMP_ALGODAO_CHEGADA'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_0503.

*  LOOP AT IT_FIELDCATALOG_0001 ASSIGNING <FS_CAT>.
*    <FS_CAT>-TABNAME = 'ZDE_IMP_ALGODAO_CHEGADA'.

*    CASE <FS_CAT>-FIELDNAME.
*      WHEN 'ICO_CARGA'.
*        <FS_CAT>-HOTSPOT = ABAP_TRUE.
*        <FS_CAT>-ICON    = ABAP_TRUE.
*        <FS_CAT>-JUST    = 'C'.
*        <FS_CAT>-COL_POS = 1.
*      WHEN 'ICO_ST_FISCAL'.
*        <FS_CAT>-HOTSPOT = ABAP_TRUE.
*        <FS_CAT>-ICON    = ABAP_TRUE.
*        <FS_CAT>-JUST    = 'C'.
*      WHEN 'ICO_ST_FISICO'.
*        <FS_CAT>-HOTSPOT = ABAP_TRUE.
*        <FS_CAT>-ICON    = ABAP_TRUE.
*        <FS_CAT>-JUST    = 'C'.
*      WHEN 'ICO_ST_ARMAZEM'.
*        <FS_CAT>-HOTSPOT = ABAP_TRUE.
*        <FS_CAT>-ICON    = ABAP_TRUE.
*        <FS_CAT>-JUST    = 'C'.
*      WHEN 'DOCNUM_NFE'.
*        <FS_CAT>-HOTSPOT = ABAP_TRUE.
*      WHEN 'EBELN' OR 'BELNR' OR 'VBELN' OR 'TKNUM' OR 'FKNUM' OR 'MBLNR' OR 'MJAHR'.
*        <FS_CAT>-HOTSPOT = ABAP_TRUE.
*      WHEN 'DS_PORTO' OR 'DS_TERMINAL' OR 'DS_NOME_TRANSPOR'.
*        <FS_CAT>-OUTPUTLEN = 20.
*    ENDCASE.
*
*    IF <FS_CAT>-FIELDNAME <> 'ICO_CARGA'.
*      <FS_CAT>-COL_POS = LC_COL_POS.
*      ADD 1 TO LC_COL_POS.
*    ENDIF.
*
*  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_HINTS_0503
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_hints_0503 .

*  DATA: IT_DD07V        TYPE TABLE OF DD07V WITH HEADER LINE,
*        WA_EXCEPT_QINFO LIKE LINE OF IT_EXCEPT_QINFO,
*        LC_TP_STATUS    TYPE ZDE_STATUS_CARGA,
*        LC_ICO_CARGA    TYPE CHAR04.
**
*  CLEAR: IT_EXCEPT_QINFO[].
**
*  "Informações Documento
*  CALL FUNCTION 'GET_DOMAIN_VALUES'
*    EXPORTING
*      DOMNAME    = 'ZDM_STATUS_CARGA'
*    TABLES
*      VALUES_TAB = IT_DD07V.
*
*  LOOP AT IT_DD07V WHERE DOMVALUE_L IS NOT INITIAL.
*    WA_EXCEPT_QINFO-TYPE  = CL_SALV_TOOLTIP=>C_TYPE_SYMBOL.
*    LC_TP_STATUS = CONV #( IT_DD07V-DOMVALUE_L ).
*    PERFORM SETA_ICONE_STATUS USING LC_TP_STATUS CHANGING LC_ICO_CARGA.
*    WA_EXCEPT_QINFO-VALUE = LC_ICO_CARGA.
*    WA_EXCEPT_QINFO-TEXT  = IT_DD07V-DDTEXT.
*    WA_EXCEPT_QINFO-TABNAME   = 'ZDE_ZSDT0001CG_ALV'.
*    WA_EXCEPT_QINFO-FIELDNAME = 'ICO_CARGA'.
*    APPEND WA_EXCEPT_QINFO TO IT_EXCEPT_QINFO.
*  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0503
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0503 .

  gs_layout_0503-sel_mode   = 'A'.
  "GS_LAYOUT_0503-INFO_FNAME = 'LINE_COLOR'.
  "GS_LAYOUT_0503-STYLEFNAME = 'STYLE'.
  "GS_LAYOUT_0503-CTAB_FNAME = 'COLOR_CELL'.
  "GS_LAYOUT_0503-ZEBRA      = ABAP_FALSE.

  gs_variant_0503-report      = sy-repid.
  gs_variant_0503-handle      = '0503'.
  gs_variant_0503-log_group   = abap_false.
  gs_variant_0503-username    = abap_false.
  gs_variant_0503-variant     = abap_false.
  gs_variant_0503-text        = abap_false.
  gs_variant_0503-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_0503
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM limpar_0503 .

  IF ctl_alv_0503 IS NOT INITIAL.
    ctl_alv_0503->free( ).
  ENDIF.
  CLEAR: ctl_alv_0503.

  IF ctl_cccontainer_0503 IS NOT INITIAL.
    ctl_cccontainer_0503->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_0503.

  IF splitter_0503 IS NOT INITIAL.
    splitter_0503->free( ).
  ENDIF.
  CLEAR: splitter_0503.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0503  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0503 INPUT.

  CASE ok_code.
    WHEN 'SAVE'.
      "Econtrar as CT-es para Liberação Automática
      PERFORM save_arq_importacao USING it_cargas.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SAVE_ARQ_IMPORTACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CARGAS  text
*----------------------------------------------------------------------*
FORM save_arq_importacao  USING p_cargas TYPE zde_imp_algodao_chegada_t.

  DATA: wa_zlest0173 TYPE zlest0173,
        answer       TYPE c.

  DATA: lc_titlebar      TYPE string,
        lc_text_question TYPE string,
        lc_texto_number  TYPE c LENGTH 20.

  DATA(it_teste) = p_cargas[].
  DELETE it_teste WHERE docnum IS NOT INITIAL.
  DESCRIBE TABLE it_teste LINES DATA(lc_qtd_linhas).

  CLEAR: it_teste, it_teste[].
  it_teste = p_cargas[].
  DELETE it_teste WHERE docnum IS INITIAL.

  WRITE lc_qtd_linhas TO lc_texto_number.
  CONDENSE lc_texto_number NO-GAPS.
  MESSAGE s214 INTO lc_titlebar.
  MESSAGE s215 INTO lc_text_question WITH lc_texto_number.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO lc_titlebar WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = lc_titlebar
      text_question         = lc_text_question
      text_button_1         = TEXT-020
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = TEXT-021
      icon_button_2         = 'ICON_INCOMPLETE'
      default_button        = '2'
      display_cancel_button = ' '
    IMPORTING
      answer                = answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK answer EQ '1'.

  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
    EXPORTING
      object           = 'ZIDIMPALGO'
    EXCEPTIONS
      foreign_lock     = 1
      object_not_found = 2
      system_failure   = 3
      OTHERS           = 4.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
    EXIT.
  ENDIF.

  CHECK it_teste IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_zlest0173)
    FROM zlest0173
     FOR ALL ENTRIES IN @it_teste
   WHERE docnum EQ @it_teste-docnum.

  SORT it_zlest0173 BY reme_cnpj nfenum lote.

  LOOP AT p_cargas INTO DATA(wa_cargas) WHERE docnum IS NOT INITIAL.
    CLEAR: wa_zlest0173.
    READ TABLE it_zlest0173 WITH KEY reme_cnpj = wa_cargas-reme_cnpj
                                     nfenum    = wa_cargas-nfenum
                                     lote      = wa_cargas-lote
                            ASSIGNING FIELD-SYMBOL(<fs_0173>).
    IF sy-subrc IS INITIAL.
      <fs_0173>-ds_placa   = wa_cargas-ds_placa.
      <fs_0173>-qtd_fardos = wa_cargas-qtd_fardos.
      <fs_0173>-dt_chegada = wa_cargas-dt_chegada.
    ELSE.
      MOVE-CORRESPONDING wa_cargas TO wa_zlest0173.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZIDIMPALGO'
          quantity                = '00000000000000000001'
          ignore_buffer           = 'X'
        IMPORTING
          number                  = wa_zlest0173-id_importacao
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      APPEND wa_zlest0173 TO it_zlest0173.
    ENDIF.

  ENDLOOP.

* Desbloqueia o objeto de numeração
  CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
    EXPORTING
      object           = 'ZIDIMPALGO'
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MODIFY zlest0173 FROM TABLE it_zlest0173.
  COMMIT WORK.

  "Deletar Registros importados incorretamente anteriormente para cada NF
  LOOP AT it_zlest0173 INTO DATA(lwa_zlest0173) WHERE docnum        IS NOT INITIAL
                                                  AND id_importacao IS NOT INITIAL.

    READ TABLE p_cargas INTO DATA(lwa_carga) WITH KEY reme_cnpj = lwa_zlest0173-reme_cnpj
                                                      nfenum    = lwa_zlest0173-nfenum
                                                      lote      = lwa_zlest0173-lote.
    IF sy-subrc NE 0.
      DELETE FROM zlest0173 WHERE id_importacao = lwa_zlest0173-id_importacao.
    ENDIF.
  ENDLOOP.

  MESSAGE s059.

  PERFORM limpar_0503.
  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ANEXAR_MULTIPLOS_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM anexar_multiplos_documentos .
  "Somente validar acesso para modificar
  IF it_cte_select[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  PERFORM f_anexar_mult_docs.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ANEXAR_MULT_DOCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_anexar_mult_docs .

  DATA: folder_id TYPE sofdk,
        vl_error  TYPE c.

  DATA: it_file_table	  TYPE filetable,
        wa_file_table   TYPE file_table,
        lc_rc	          TYPE i,
        vl_file_table_c TYPE string.

  CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
    EXPORTING
      region    = 'B'
    IMPORTING
      folder_id = folder_id
    EXCEPTIONS
      OTHERS    = 0.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Importar Arquivo '
      file_filter             = 'Files (*.*)|*.*|'
      multiselection          = abap_true
      initial_directory       = 'C:\Amaggi\'
    CHANGING
      file_table              = it_file_table
      rc                      = lc_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  IF it_file_table IS NOT INITIAL.
    LOOP AT it_cte_select.

      FREE: vl_error.
      LOOP AT it_file_table INTO wa_file_table.
        vl_file_table_c = wa_file_table.
        PERFORM f_create_anexos USING it_cte_select folder_id vl_file_table_c vl_error.
        IF vl_error IS NOT INITIAL.
          RETURN.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ANEXOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_anexos  USING  p_saida_0100    TYPE zib_cte_dist_ter
                             p_folder_id     TYPE sofdk
                             p_path_and_file TYPE string
                    CHANGING p_error.

*  MESSAGE s003(vpd).

  DATA: back LIKE sy-ucomm VALUE 'BAC',  " Back
        canc LIKE sy-ucomm VALUE 'ESC',  " Cancel
        stop LIKE sy-ucomm VALUE 'RET'.  " Terminate SAPoffice

  DATA is_object  TYPE borident.
  DATA attachment TYPE borident.
  DATA documents  TYPE STANDARD TABLE OF sood4.

  DATA document   TYPE sood4.
  DATA ep_attachment TYPE char40.

  DATA l_cancelled   LIKE sonv-flag.
  DATA bin_filesize  LIKE soxwd-doc_length.
  DATA file_format   LIKE rlgrap-filetype.
  DATA path_and_file TYPE string.
  DATA object_type   LIKE soodk-objtp.
  DATA put_to_kpro   LIKE sonv-flag.
  DATA p_objcont     TYPE TABLE OF soli.
  DATA p_objhead     TYPE TABLE OF soli.
  DATA p_objpara     TYPE TABLE OF selc.
  DATA p_objparb     TYPE TABLE OF soop1.

  DATA doc_id      LIKE soodk.
  DATA att_id      LIKE soodk.
  DATA hd_dat      LIKE sood1.
  DATA fm_dat      LIKE sofm1.
  DATA new_doc_id  LIKE soodk.
  DATA new_att_id  LIKE soodk.
  DATA new_hd_dat  LIKE sood2.
  DATA new_fm_dat  LIKE sofm2.
  DATA old_doc_id  LIKE soodk.
  DATA fol_dat     LIKE sofdd.
  DATA old_enccnt  LIKE sood-enccnt.
  DATA attach_list LIKE sood5 OCCURS 0 WITH HEADER LINE.
  DATA link_list   LIKE soodk OCCURS 0 WITH HEADER LINE.
  DATA l_reappear  LIKE sonv-flag.
  DATA l_answer.
  DATA l_filename           TYPE string.
  DATA reference_type_kpro VALUE 'K'.      "KPro reference
  DATA obj_type             LIKE soodk-objtp.
  DATA owner_dat            LIKE soud3.

  DATA  p_header_data LIKE sood2.
  DATA  p_folmem_data LIKE sofm2.
  DATA  l_name TYPE string.                                 "1041757 >>
  DATA  g_document     LIKE sood4.
  DATA  lo_objhead TYPE REF TO cl_bcs_objhead.

  IF owner IS INITIAL.
    CLEAR owner_dat.
    MOVE sy-uname TO owner_dat-sapnam.
    CALL FUNCTION 'SO_NAME_CONVERT'
      EXPORTING
        name_in               = owner_dat
        no_address_name       = 'X'
      IMPORTING
        name_out              = owner_dat
      EXCEPTIONS
        communication_failure = 71
        office_name_not_exist = 19
        parameter_error       = 23
        sap_name_not_exist    = 29
        system_failure        = 72
        user_not_exist        = 34.
    IF sy-subrc NE 0.
      p_error = 'X'.
      RETURN.
    ENDIF.

    MOVE owner_dat-usrnam TO owner.

    CALL FUNCTION 'SO_FOLDER_HEADER_READ'
      EXPORTING
        folder_id                  = p_folder_id
        owner                      = owner
      IMPORTING
        folder_data                = sofd_dat
      EXCEPTIONS
        communication_failure      = 71
        folder_not_exist           = 6
        operation_no_authorization = 21
        system_failure             = 72.

    IF sy-subrc NE 0.
      p_error = 'X'.
      RETURN.
    ENDIF.

  ENDIF.

  g_document-foltp = p_folder_id-foltp.
  g_document-folyr = p_folder_id-folyr.
  g_document-folno = p_folder_id-folno.
  g_document-folrg = sofd_dat-folrg.

  CALL FUNCTION 'SO_OBJECT_UPLOAD'
    EXPORTING
      filetype                = 'BIN'
      path_and_file           = p_path_and_file
      no_dialog               = 'X'
    IMPORTING
      f_cancelled             = l_cancelled
      filelength              = bin_filesize
      act_filetype            = file_format
      act_filename            = path_and_file
      act_objtype             = object_type
      file_put_to_kpro        = put_to_kpro
    TABLES
      objcont                 = p_objcont
    EXCEPTIONS
      invalid_type            = 1
      object_type_not_allowed = 2
      kpro_insert_error       = 3
      file_to_large           = 4
      OTHERS                  = 5.

  CASE sy-subrc.
    WHEN 0.
      IF NOT l_cancelled IS INITIAL.
        MESSAGE s118(so). g_document-okcode = canc. EXIT.
      ENDIF.
      hd_dat-objlen = bin_filesize.

      IF NOT put_to_kpro IS INITIAL.
        hd_dat-extct = reference_type_kpro.
      ENDIF.

      CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
        EXPORTING
          full_name     = path_and_file
        IMPORTING
          stripped_name = l_filename.

      CLEAR: p_objhead, p_objhead[].
      lo_objhead = cl_bcs_objhead=>create( p_objhead[] ).
      lo_objhead->set_filename( l_filename ).
      lo_objhead->set_format( file_format ).
      lo_objhead->set_vsi_profile( cl_bcs_vsi_profile=>get_profile( ) ).
      p_objhead[] = lo_objhead->mt_objhead.
      hd_dat-file_ext = object_type.

      "if hd_dat-objdes is initial.
**       split l_filename at '.' into hd_dat-objdes l_filename.
      PERFORM so_split_file_and_extension(saplso30)
                                          USING l_filename
                                                hd_dat-objdes
                                                object_type.
      g_document-objdes = hd_dat-objdes.
      "endif.

    WHEN 1.
      p_error = 'X'.
      MESSAGE i422(so) WITH file_format. g_document-okcode = canc.
      EXIT.
    WHEN 2.
      p_error = 'X'.
      MESSAGE i322(so). g_document-okcode = canc.
      EXIT.
    WHEN 3.
      p_error = 'X'.
      MESSAGE i444(so). g_document-okcode = canc.
      EXIT.
    WHEN 4.
      p_error = 'X'.
      MESSAGE i425(so). g_document-okcode = canc.
      EXIT.
    WHEN 5.
      p_error = 'X'.
      DATA lv_done TYPE c.
      lv_done = cl_bcs_vsi_profile=>output_vsi_error( ).
      IF lv_done IS INITIAL.
        MESSAGE i424(so).
      ENDIF.
      g_document-okcode = 'ESC'.
      EXIT.
  ENDCASE.

  hd_dat-file_ext = object_type.
  obj_type = 'EXT'.

  CALL FUNCTION 'SO_OBJECT_INSERT'
    EXPORTING
      folder_id                  = p_folder_id
      object_type                = obj_type
      object_hd_change           = hd_dat
      object_fl_change           = fm_dat
      owner                      = owner
    IMPORTING
      object_id                  = new_doc_id
      object_hd_display          = new_hd_dat
      object_fl_display          = new_fm_dat
    TABLES
      objcont                    = p_objcont
      objhead                    = p_objhead
      objpara                    = p_objpara
      objparb                    = p_objparb
    EXCEPTIONS
      active_user_not_exist      = 35
      communication_failure      = 71
      component_not_available    = 1
      dl_name_exist              = 3
      folder_no_authorization    = 5
      folder_not_exist           = 6
      object_type_not_exist      = 17
      operation_no_authorization = 21
      owner_not_exist            = 22
      parameter_error            = 23
      substitute_not_active      = 31
      substitute_not_defined     = 32
      system_failure             = 72
      x_error                    = 1000.

  IF sy-subrc > 0.
    p_error = 'X'.
    RETURN.
  ELSE.
    MOVE: new_doc_id-objtp TO g_document-objtp,
          new_doc_id-objyr TO g_document-objyr,
          new_doc_id-objno TO g_document-objno,
          new_hd_dat       TO p_header_data,
          new_fm_dat       TO p_folmem_data.
    g_document-objnam = new_hd_dat-objnam.
    g_document-objdes = new_hd_dat-objdes.
    g_document-okcode = 'CREA'.
    IF sy-batch IS INITIAL AND sy-binpt IS INITIAL.
      MESSAGE s109(so).
    ENDIF.
  ENDIF.

  is_object-objtype = 'ZMM0079'.
  CONCATENATE p_saida_0100-mandt p_saida_0100-cd_chave_cte
  INTO is_object-objkey.

  IF g_document-okcode = 'CREA' OR g_document-okcode = 'CHNG'.
    attachment-objtype = 'MESSAGE'.
    attachment-objkey  =  g_document(34).
    CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
      EXPORTING
        obj_rolea    = is_object
        obj_roleb    = attachment
        relationtype = 'ATTA'
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc = 0.
      MESSAGE 'Arquivo(s) Anexado(s)' TYPE 'S'.
    ENDIF.
  ENDIF.

  FREE: it_cte_selectw.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DADOS_CHEGADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CTE_N55_DOCNUM_NFE  text
*----------------------------------------------------------------------*
FORM mostrar_dados_chegada  USING p_fiscal TYPE j_1bdocnum.

  DATA: lv_n(3)   TYPE n,
        lv_i      TYPE i,
        lv_cz(3)  TYPE c,
        lv_csz(3) TYPE c.

  IF p_fiscal IS NOT INITIAL.
    SELECT SINGLE docnum
            FROM zlest0039
            INTO @DATA(lv_fiscal)
          WHERE docnum  EQ @p_fiscal.

    IF sy-subrc IS NOT INITIAL AND lv_fiscal IS INITIAL.
      SELECT SINGLE docnum, branch, nfenum, parid, series
        FROM j_1bnfdoc
        INTO @DATA(ls_fiscal)
          WHERE docnum  EQ @p_fiscal.

      IF sy-subrc IS INITIAL AND ls_fiscal IS NOT INITIAL.
        lv_n = ls_fiscal-series.
        lv_cz = lv_n.
        lv_i = ls_fiscal-series.
        lv_csz = lv_i.

        SELECT SINGLE docnum
         FROM zlest0041
         INTO @DATA(lv_docnum)
           WHERE centro_comprador  EQ @ls_fiscal-branch
             AND nr_nf             EQ @ls_fiscal-nfenum
             AND cod_cliente       EQ @ls_fiscal-parid
             AND ( serie             EQ @lv_cz
              OR serie               EQ @lv_csz ).

        IF sy-subrc IS INITIAL AND lv_docnum IS NOT INITIAL.
          SET PARAMETER ID 'JEF' FIELD lv_docnum.
          CALL TRANSACTION 'ZLES0050' AND SKIP FIRST SCREEN.
        ELSE.
          MESSAGE i227.
        ENDIF.

      ELSE.
        MESSAGE i227.
      ENDIF.

    ELSE.

      SET PARAMETER ID 'JEF' FIELD p_fiscal.
      CALL TRANSACTION 'ZLES0050' AND SKIP FIRST SCREEN.
    ENDIF.
  ELSE.
    MESSAGE i227.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprimir_dados .

  DATA: xs_events TYPE          slis_alv_event,
        events    TYPE          slis_t_event,
        t_print   TYPE          slis_print_alv.

  DATA: wl_layout TYPE slis_layout_alv.

  IF it_cte_alv[] IS NOT INITIAL.

    PERFORM fill_it_fieldcatalog.

    MOVE-CORRESPONDING it_fieldcatalog[] TO  estrutura[].

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = v_report
*       I_CALLBACK_USER_COMMAND  = 'XUSER_COMMAND' "sem 2º click
        it_fieldcat        = estrutura[]
        is_layout          = wl_layout
        i_save             = 'A'
        it_events          = events
        is_print           = t_print
*       it_sort            = t_sort
      TABLES
        t_outtab           = it_cte_alv[]
      EXCEPTIONS
        program_error      = 1.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM visualizar_log . "115183 CS2023000462 ZMM0079 - criação de opção de listagem de autorizações realizadas CTes - PSA
  IF wa_cte_alv-ic_viewlog IS NOT INITIAL.


    TYPES: BEGIN OF displaylog,
             dt_atualizacao TYPE zib_cte_dist_log-dt_atualizacao,
             hr_atualizacao TYPE zib_cte_dist_log-hr_atualizacao,
             nr_sequencia   TYPE zib_cte_dist_log-nr_sequencia,
             message        TYPE zib_cte_dist_log-message,
             bname          TYPE zib_cte_dist_log-bname,
             message_v4     TYPE zib_cte_dist_log-message_v4,
             menssagem      TYPE string,
           END OF displaylog.

    DATA: gt_displaylog TYPE TABLE OF displaylog.

    SELECT *
         FROM zib_cte_dist_log
       INTO CORRESPONDING FIELDS OF TABLE gt_displaylog
      WHERE cd_chave_cte = wa_cte_alv-cd_chave_cte.

    DELETE gt_displaylog WHERE message_v4 = space OR message_v4 = ' ' OR message_v4 = '' OR message_v4 = '000000'.

    DATA BEGIN OF tl_lines OCCURS 10.
    INCLUDE STRUCTURE tline.
    DATA END OF tl_lines.

    DATA: vl_id       LIKE  thead-tdid,
          vl_language LIKE  thead-tdspras,
          vl_object   LIKE  thead-tdobject,
          vl_name     LIKE  thead-tdname.



    LOOP AT gt_displaylog ASSIGNING FIELD-SYMBOL(<wa_displaylog>).

      CLEAR: tl_lines.
      vl_id       = 'ZCTE'.
      vl_language = sy-langu.
      vl_object   = 'ZAPROVACAO'.
      vl_name     = sy-mandt && <wa_displaylog>-message_v4.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id       = vl_id
          language = vl_language
          name     = vl_name
          object   = vl_object
        TABLES
          lines    = tl_lines
        EXCEPTIONS
          OTHERS   = 1.

      IF sy-subrc IS INITIAL.
        LOOP AT tl_lines INTO DATA(wa_lines).
          IF <wa_displaylog>-menssagem IS INITIAL .
            <wa_displaylog>-menssagem = wa_lines-tdline.
          ELSE.
            <wa_displaylog>-menssagem = <wa_displaylog>-menssagem && ', ' && wa_lines-tdline.
          ENDIF.

        ENDLOOP.
      ENDIF.

    ENDLOOP.

    SORT gt_displaylog DESCENDING BY dt_atualizacao hr_atualizacao nr_sequencia.

    cl_demo_output=>display( gt_displaylog ).



  ENDIF.


ENDFORM.


FORM job_create CHANGING p_KEY TYPE zib_cte_dist_ter-cd_chave_cte.

  DATA: lv_jobname  TYPE tbtcjob-jobname,
        lv_jobcount TYPE tbtcjob-jobcount,
        lv_variant  TYPE raldb-variant VALUE 'ZMMR0208_JOB',
        lv_report   TYPE sy-repid VALUE 'ZMMR0208',
        lv_finished TYPE abap_bool.

  DATA: lt_rsparams TYPE STANDARD TABLE OF rsparams,
        ls_rsparams TYPE rsparams.

  DATA: ls_vari_desc   TYPE varid.
  DATA: lt_vari_text TYPE STANDARD TABLE OF varit,
        ls_vari_text TYPE varit.

  lv_jobname = |ZMMR0208_{ sy-datum }{ sy-uzeit }|.

* Preenche a estrutura de definição da variante
  ls_vari_desc-report  = lv_report.
  ls_vari_desc-variant = lv_variant.
  ls_vari_desc-ename   = sy-uname.

* Descrição da variante
  ls_vari_text-langu   = sy-langu.
  ls_vari_text-report  = lv_report.
  ls_vari_text-variant = lv_variant.
  ls_vari_text-vtext   = 'Variante temporária para job'.
  APPEND ls_vari_text TO lt_vari_text.

* Parâmetros da variante
  CLEAR lt_rsparams.
  ls_rsparams-selname = 'P_KEY'.
  ls_rsparams-kind    = 'S'.
  ls_rsparams-sign    = 'I'.
  ls_rsparams-option  = 'EQ'.
  ls_rsparams-low     = p_key.
  APPEND ls_rsparams TO lt_rsparams.

** Remove a variante anterior, se existir
*  SELECT SINGLE * FROM vari WHERE report = @lv_report AND variant = @lv_variant INTO @DATA(ls_VARI).
*  IF sy-subrc = 0.
*    DELETE FROM vari WHERE report = lv_report AND variant = lv_variant.
*  ENDIF.
*
*  SELECT SINGLE * FROM varit WHERE report = @lv_report AND variant = @lv_variant INTO @DATA(ls_VARIt).
*  IF sy-subrc = 0.
*    DELETE FROM varit WHERE report = lv_report AND variant = lv_variant.
*  ENDIF.

  CALL FUNCTION 'RS_CHANGE_CREATED_VARIANT'
    EXPORTING
      curr_report               = lv_report
      curr_variant              = lv_variant
      vari_desc                 = ls_vari_desc
*     ONLY_CONTENTS             =
*     P_XML_TAB                 =
*     SUPPRESS_AUTHORITY_CHECK  =
    TABLES
      vari_contents             = lt_rsparams
*     VARI_CONTENTS_L           =
      vari_text                 = lt_vari_text
*     VARI_SEL_DESC             =
*     OBJECTS                   =
*     FREE_SELECTIONS_VALUE     =
*     FREE_SELECTIONS_OBJ       =
*     VARIVDATS                 =
    EXCEPTIONS
      illegal_report_or_variant = 1
      illegal_variantname       = 2
      not_authorized            = 3
      not_executed              = 4
      report_not_existent       = 5
      report_not_supplied       = 6
      variant_doesnt_exist      = 7
      variant_locked            = 8
      selections_no_match       = 9
      OTHERS                    = 10.

  IF sy-subrc                  = 0.
* Abre o job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_jobname
      IMPORTING
        jobcount         = lv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc = 0.
* Submete o programa usando a variante criada
      CALL FUNCTION 'JOB_SUBMIT'
        EXPORTING
          jobname                 = lv_jobname
          jobcount                = lv_jobcount
          report                  = lv_report
          variant                 = lv_variant
          authcknam               = sy-uname
        EXCEPTIONS
          bad_priparams           = 1
          bad_xpgflags            = 2
          invalid_jobdata         = 3
          jobname_missing         = 4
          job_notex               = 5
          job_submit_failed       = 6
          lock_failed             = 7
          program_missing         = 8
          prog_abap_and_extpg_set = 9
          OTHERS                  = 10.

      IF sy-subrc = 0.
* Agenda para execução imediata
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobname              = lv_jobname
            jobcount             = lv_jobcount
            strtimmed            = 'X'
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            invalid_target       = 8
            invalid_time_zone    = 9
            OTHERS               = 10.

        IF sy-subrc = 0.

          DATA: _t TYPE i.

          lv_finished = abap_false.
          DO 5 TIMES. " máximo de tentativas (ajuste conforme necessário)

            _t = _t + 1.

            SELECT SINGLE status
              FROM tbtco
              WHERE jobname  = @lv_jobname
                AND jobcount = @lv_jobcount INTO @DATA(lv_jobstatus).

            IF sy-subrc = 0.

              IF lv_jobstatus = 'F' OR lv_jobstatus = 'C'.
                lv_finished = abap_true.
                EXIT.
              ELSEIF lv_jobstatus = 'A' OR lv_jobstatus = 'R'. " Active or Released
                WAIT UP TO 3 SECONDS. " espera antes de checar novamente
              ELSE.

              ENDIF.
            ELSE.
              EXIT.
            ENDIF.

            IF _t >= 5.
              EXIT.
            ENDIF.

          ENDDO.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.
