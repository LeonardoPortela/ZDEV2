*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_9004.
*----------------------------------------------------------------------*

CLASS lcl_event_receiver_9004 DEFINITION DEFERRED.

DATA: ctl_cccontainer_9004 TYPE REF TO cl_gui_custom_container,
      ctl_alv_9004         TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog_9004 TYPE lvc_t_fcat,
      gs_variant_9004      TYPE disvariant,
      gs_layout_9004       TYPE lvc_s_layo,
      gs_scroll_col_9004   TYPE lvc_s_col,
      gs_scroll_row_9004   TYPE lvc_s_roid,
      wa_stable_9004       TYPE lvc_s_stbl.

DATA: event_handler_9004 TYPE REF TO lcl_event_receiver_9004.

DATA: ck_conferiu TYPE char01.

CLASS lcl_event_receiver_9004 DEFINITION.

  PUBLIC SECTION.
    DATA: validar_data  TYPE c,
          error_in_data TYPE c,
          ls_good       TYPE lvc_s_modi,
          lv_value      TYPE lvc_value.

    METHODS: data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.
    METHODS: data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.
    "METHODS: SUBTOTAL_TEXT FOR EVENT SUBTOTAL_TEXT OF CL_GUI_ALV_GRID IMPORTING ES_SUBTOTTXT_INFO EP_SUBTOT_LINE E_EVENT_DATA.

  PRIVATE SECTION.

    TYPES: ddshretval_table TYPE TABLE OF ddshretval.

    METHODS: perform_semantic_checks
      IMPORTING
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS: atualiza_nota
      IMPORTING
        i_ck_chave_deb_cred TYPE char01
        pr_data_changed     TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                    "lcl_event_receiver DEFINITION

CLASS lcl_event_receiver_9004 IMPLEMENTATION.

  METHOD atualiza_nota.

    DATA: wa_tbsl     TYPE tbsl,
          wa_zglt032  TYPE zglt032,
          tp_deb_cred TYPE shkzg.

*    MOVE ZDE_MOV_LCT_BANCO-SAKNR TO LV_VALUE.
*    CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
*      EXPORTING
*        I_ROW_ID    = LS_GOOD-ROW_ID
*        I_FIELDNAME = 'SAKNR'
*        I_VALUE     = LV_VALUE.

*    MOVE ZDE_MOV_LCT_BANCO-DMBE2 TO LV_VALUE.
*    CONDENSE LV_VALUE NO-GAPS.
*    CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
*      EXPORTING
*        I_ROW_ID    = LS_GOOD-ROW_ID
*        I_FIELDNAME = 'DMBE2'
*        I_VALUE     = LV_VALUE.

  ENDMETHOD.                    "ATUALIZA_NOTA

*  METHOD SUBTOTAL_TEXT.
*    DATA: WA_MOV_LCT_BANCO2 TYPE ZDE_MOV_LCT_BANCO,
*          WA_MOV_LCT_BANCO  TYPE ZDE_MOV_LCT_BANCO.
*
*    FIELD-SYMBOLS: <FS>  TYPE ANY.
*    FIELD-SYMBOLS: <FS2> TYPE ANY.
*
*    ASSIGN E_EVENT_DATA->M_DATA->* TO <FS>.
*    IF SY-SUBRC EQ 0.
*
*      IF ES_SUBTOTTXT_INFO(07) EQ 'TP_LCTO'.
*
*        ASSIGN EP_SUBTOT_LINE->* TO <FS2>.
*        WA_MOV_LCT_BANCO2 = <FS2>.
*
*        READ TABLE IT_MOV_LCT_BANCO INTO WA_MOV_LCT_BANCO WITH KEY TP_LCTO = WA_MOV_LCT_BANCO2-TP_LCTO.
*        <FS> = WA_MOV_LCT_BANCO-DESCRICAO.
*
*        WA_MOV_LCT_BANCO2-DMBTR = 0.
*        WA_MOV_LCT_BANCO2-DMBE2 = 0.
*        LOOP AT IT_MOV_LCT_BANCO INTO WA_MOV_LCT_BANCO WHERE TP_LCTO = WA_MOV_LCT_BANCO2-TP_LCTO.
*          WA_MOV_LCT_BANCO2-DMBTR = WA_MOV_LCT_BANCO2-DMBTR + WA_MOV_LCT_BANCO-DMBTR.
*          WA_MOV_LCT_BANCO2-DMBE2 = WA_MOV_LCT_BANCO2-DMBE2 + WA_MOV_LCT_BANCO-DMBE2.
*        ENDLOOP.
*
*        WA_MOV_LCT_BANCO = <FS2>.
*        WA_MOV_LCT_BANCO-DMBTR = WA_MOV_LCT_BANCO2-DMBTR.
*        WA_MOV_LCT_BANCO-DMBE2 = WA_MOV_LCT_BANCO2-DMBE2.
*        <FS2> = WA_MOV_LCT_BANCO.
*      ENDIF.
*    ENDIF.

*  ENDMETHOD.                    "subtotal_text

  "BCALV_EDIT_03
  METHOD perform_semantic_checks.

    DATA: lc_nm_peso_subtotal	TYPE zde_nm_peso_subtotal,
          lc_nm_peso_liquido  TYPE zde_nm_peso_liquido,
          lc_ds_observacao    TYPE zde_observacao.

    FIELD-SYMBOLS: <fs_cell> TYPE lvc_s_modi.

    LOOP AT pr_data_changed->mt_good_cells INTO DATA(ls_good)
      WHERE fieldname EQ 'DS_OBSERVACAO' OR fieldname EQ 'NM_PESO_LIQUIDO'.

      CASE ls_good-fieldname.
        WHEN 'NM_PESO_LIQUIDO'.

          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.

          READ TABLE it_notas ASSIGNING FIELD-SYMBOL(<fs_nota>) INDEX ls_good-row_id.

          IF ls_good-value IS INITIAL.
            CONTINUE.
          ENDIF.

          CASE ls_good-fieldname.
            WHEN 'NM_PESO_SUBTOTAL'.
              MOVE lv_value TO lc_nm_peso_subtotal.
              lc_nm_peso_liquido = <fs_nota>-nm_peso_liquido.
            WHEN 'NM_PESO_LIQUIDO'.
              MOVE lv_value TO lc_nm_peso_liquido.
              lc_nm_peso_subtotal = <fs_nota>-nm_peso_subtotal.
          ENDCASE.

          TRY .

              objeto->set_pesos_notas(
                EXPORTING
                  i_id_carga      = <fs_nota>-id_carga     " Id. da Carga
                  i_id_nota       = <fs_nota>-id_nota      " Id. Nota Fiscal
                  i_peso_subtotal = lc_nm_peso_subtotal    " Peso SubTotal do Caminhão
                  i_peso_liquido  = lc_nm_peso_liquido     " Peso Líquido
                IMPORTING
                  e_nota          = <fs_nota>  ).

            CATCH zcx_carga INTO ex_carga.
              error_in_data = abap_true.
              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = ex_carga->msgid
                  i_msgno     = ex_carga->msgno
                  i_msgty     = ex_carga->msgty
                  i_msgv1     = ex_carga->msgv1
                  i_msgv2     = ex_carga->msgv2
                  i_msgv3     = ex_carga->msgv3
                  i_msgv4     = ex_carga->msgv4
                  i_fieldname = ls_good-fieldname
                  i_row_id    = ls_good-row_id.
          ENDTRY.

        WHEN 'DS_OBSERVACAO'.

          lv_value = ls_good-value.
          READ TABLE it_notas ASSIGNING <fs_nota> INDEX ls_good-row_id.
          lc_ds_observacao = lv_value.

          TRY .
              objeto->set_observacao_nota(
                EXPORTING
                  i_id_carga      = <fs_nota>-id_carga    " Id. da Carga
                  i_id_nota       = <fs_nota>-id_nota     " Id. Nota Fiscal
                  i_ds_observacao = lc_ds_observacao    " Peso SubTotal do Caminhão
                IMPORTING
                  e_nota          = <fs_nota> ).
            CATCH zcx_carga INTO ex_carga.
              error_in_data = abap_true.
              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = ex_carga->msgid
                  i_msgno     = ex_carga->msgno
                  i_msgty     = ex_carga->msgty
                  i_msgv1     = ex_carga->msgv1
                  i_msgv2     = ex_carga->msgv2
                  i_msgv3     = ex_carga->msgv3
                  i_msgv4     = ex_carga->msgv4
                  i_fieldname = ls_good-fieldname
                  i_row_id    = ls_good-row_id.
          ENDTRY.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.                    "perform_semantic_checks

  METHOD data_changed.
    error_in_data = space.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.
  ENDMETHOD.                    "on_data_chaged

  METHOD data_changed_finished.

    IF e_modified IS NOT INITIAL.

      wa_stable_9004-row = abap_true.
      wa_stable_9004-col = abap_true.
      CALL METHOD ctl_alv_9004->refresh_table_display
        EXPORTING
          is_stable = wa_stable_9004.
    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9004 OUTPUT.
  SET PF-STATUS 'PF9004'.
  SET TITLEBAR 'TL9004' WITH zde_zsdt0001cg_alv-nr_ticket.

  IF ctl_alv_9004 IS INITIAL.

    CREATE OBJECT ctl_cccontainer_9004
      EXPORTING
        container_name = 'ALV_9004'.

    CREATE OBJECT ctl_alv_9004
      EXPORTING
        i_parent = ctl_cccontainer_9004.

    PERFORM fill_it_fieldcatalog_9004.

    PERFORM fill_gs_variant_9004.

    gs_layout_9004-sel_mode   = 'A'.
    gs_layout_9004-zebra      = abap_false.
    gs_layout_9004-cwidth_opt = abap_true.
    gs_layout_9004-no_toolbar = abap_true.

    IF NOT ( objeto->carga-ck_enviado_opus = abap_true AND objeto->carga-ck_recebido_opus EQ abap_false ).
      gs_layout_9004-edit_mode  = 'X'.
    ENDIF.

    CALL METHOD ctl_alv_9004->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_9004
        is_variant      = gs_variant_9004
      CHANGING
        it_fieldcatalog = it_fieldcatalog_9004
        it_outtab       = it_notas[].

    CALL METHOD ctl_alv_9004->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv_9004->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT event_handler_9004.
    SET HANDLER event_handler_9004->data_changed_finished FOR ctl_alv_9004.
    SET HANDLER event_handler_9004->data_changed          FOR ctl_alv_9004.
*    SET HANDLER EVENT_HANDLER_9004->SUBTOTAL_TEXT         FOR CTL_ALV_9004.
    CALL METHOD ctl_alv_9004->refresh_table_display.

  ENDIF.

  wa_stable_9004-row = abap_true.
  wa_stable_9004-col = abap_true.

  CALL METHOD ctl_alv_9004->refresh_table_display
    EXPORTING
      is_stable      = wa_stable_9004
      i_soft_refresh = abap_true.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9004 INPUT.

  DATA: ck_retorno TYPE sy-subrc.
  DATA: ck_np      TYPE sy-subrc.
  DATA: ck_saida_automatica TYPE char01.

  CLEAR: ck_saida_automatica,ck_np.
  DATA erro TYPE c.

  CASE ok_code.
    WHEN 'CONFIRMA'.
      CLEAR: ok_code.
* CS2021000183 Parte 1 - Classificação automática - US 74975 - BG - INICIO
      LOOP AT objeto->resultado INTO DATA(resultado).

        SELECT SINGLE * FROM zsdt0001rsw INTO @DATA(wa_zsdt0001rsw) WHERE werks = @objeto->carga-id_branch AND tp_caracteristica  =  @resultado-tp_caracteristica.

        IF sy-subrc EQ 0.
          IF wa_zsdt0001rsw-nr_percentual_com <> resultado-nr_percentual_com.
            erro = 'X'.
            EXIT.
          ENDIF.
        ELSE.
          erro = 'X'.
          EXIT.
        ENDIF.

      ENDLOOP.
      IF  erro IS INITIAL.
        LOOP AT objeto->resultado INTO DATA(wa_resultado).
          CASE wa_resultado-tp_caracteristica.
            WHEN '01'.
              zde_zsdt0001cg_valida-nr_perc_umi = wa_resultado-nr_percentual_com.
              zde_zsdt0001cg_valida-nr_qtde_umi = wa_resultado-nr_quantidade_com.
            WHEN '02'.
              zde_zsdt0001cg_valida-nr_perc_imp = wa_resultado-nr_percentual_com.
              zde_zsdt0001cg_valida-nr_qtde_imp = wa_resultado-nr_quantidade_com.
            WHEN '03'.
              zde_zsdt0001cg_valida-nr_perc_ava = wa_resultado-nr_percentual_com.
              zde_zsdt0001cg_valida-nr_qtde_ava = wa_resultado-nr_quantidade_com.
            WHEN '04'.
              zde_zsdt0001cg_valida-nr_perc_ard = wa_resultado-nr_percentual_com.
              zde_zsdt0001cg_valida-nr_qtde_ard = wa_resultado-nr_quantidade_com.
            WHEN '05'.
              zde_zsdt0001cg_valida-nr_perc_que = wa_resultado-nr_percentual_com.
              zde_zsdt0001cg_valida-nr_qtde_que = wa_resultado-nr_quantidade_com.
            WHEN '06'.
              zde_zsdt0001cg_valida-nr_perc_esv = wa_resultado-nr_percentual_com.
              zde_zsdt0001cg_valida-nr_qtde_esv = wa_resultado-nr_quantidade_com.
            WHEN '07'.
              zde_zsdt0001cg_valida-nr_perc_car = wa_resultado-nr_percentual_com.
              zde_zsdt0001cg_valida-nr_qtde_car = wa_resultado-nr_quantidade_com.
          ENDCASE.

        ENDLOOP.


*   LOOP AT objeto->RESULTADO_AVARIADO INTO DATA(WA_RESUL_AVARIADO).
*        CASE WA_RESUL_AVARIADO-TP_SUB_CARAC_AVARIADO.
*        WHEN '1'.
*           zde_zsdt0001cg_valida-nr_ = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
*        WHEN '2'.
*          zde_zsdt0001cg_valida-NR_PERC_AVA_QUE = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
*        WHEN '3'.
*          zde_zsdt0001cg_valida-NR_PERC_AVA_MOF = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
*        WHEN '4'.
*          zde_zsdt0001cg_valida-NR_PERC_AVA_PIC = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
*        WHEN '5'.
*          zde_zsdt0001cg_valida-NR_PERC_AVA_FER = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
*        WHEN '6'.
*          zde_zsdt0001cg_valida-NR_PERC_AVA_GER = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
*        WHEN '7'.
*          zde_zsdt0001cg_valida-NR_PERC_AVA_ARD = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
*        WHEN '8'.
*          zde_zsdt0001cg_valida-NR_PERC_AVA_GES = WA_RESUL_AVARIADO-NR_PERCENTUAL_COM.
*      ENDCASE.
*
*   ENDLOOP.
        "PREENCER TELA DE CLASSIFICAÇÃO
      ENDIF.
* CS2021000183 Parte 1 - Classificação automática - US 74975 - BG - FIM

      ck_np = 1. "não tem nota propria
      IF objeto->at_manutencao NE abap_true.
        LOOP AT objeto->documento_fiscal INTO DATA(w_notas).
          SELECT  SINGLE   docnum_ref
             INTO @DATA(lc_docnum_np)
             FROM zib_nfe_forn
            WHERE nu_chave  = @w_notas-nr_chave_nfe.
          IF sy-subrc EQ 0 and lc_docnum_np is not INITIAL.
            ck_np = 0.
          ENDIF.
        ENDLOOP.
        IF NOT ( objeto->carga-ck_enviado_opus = abap_true AND objeto->carga-ck_recebido_opus EQ abap_false ) and ck_np = 1.
          PERFORM confere_carga CHANGING ck_retorno ck_saida_automatica.
        ELSE.
          ck_retorno = 0.
        ENDIF.
      ELSE.
        ck_retorno = 0 .
      ENDIF.

      CHECK ck_retorno IS INITIAL.

      TRY .
          objeto->set_conferido(
             EXPORTING i_proximo_passo_automatico = ck_saida_automatica
             IMPORTING e_conferiu = DATA(e_conferiu) ).

          IF NOT e_conferiu EQ abap_true.
            EXIT.
          ENDIF.

          objeto->get_registro( IMPORTING e_registro = DATA(e_registro) ).

          SET PARAMETER ID 'ZIDCARGA' FIELD e_registro-id_carga.

        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
          EXIT.

        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
          EXIT.

        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
          EXIT.

        CATCH zcx_job INTO ex_job.
          ex_job->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
          EXIT.

        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
          EXIT.

        CATCH zcx_pedido_compra_exception INTO ex_pedido.
          ex_pedido->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
          EXIT.

        CATCH zcx_miro_exception INTO ex_miro.
          ex_miro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
          EXIT.

        CATCH zcx_ordem_carregamento INTO ex_ordem.
          ex_ordem->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
          EXIT.

      ENDTRY.

      ck_conferiu = abap_true.

      PERFORM limpar_9004.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9004_exit INPUT.

  PERFORM limpar_9004.

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info_9004 INPUT.

  IF ctl_alv_9004 IS NOT INITIAL.
    CALL METHOD ctl_alv_9004->get_scroll_info_via_id
      IMPORTING
        es_col_info = gs_scroll_col_9004
        es_row_no   = gs_scroll_row_9004.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_9004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_9004 .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_9004[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZSDT0001NT_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_9004.

  LOOP AT it_fieldcatalog_9004 ASSIGNING <fs_cat>.
    <fs_cat>-tabname = 'ZDE_ZSDT0001NT_ALV'.

    IF <fs_cat>-fieldname     EQ 'DS_FORNECEDOR' OR
       <fs_cat>-fieldname     EQ 'NR_NOTA'       OR
       <fs_cat>-fieldname     EQ 'NM_SERIE'      OR
       <fs_cat>-fieldname     EQ 'DT_EMISSAO'    OR
       <fs_cat>-fieldname     EQ 'NR_QUANTIDADE' OR
       <fs_cat>-fieldname     EQ 'NR_VALOR'      OR
       <fs_cat>-fieldname     EQ 'NM_PESO_SUBTOTAL' OR
       <fs_cat>-fieldname     EQ 'NM_PESO_LIQUIDO' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_UMI' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_IMP' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_AVA' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_ARD' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_QUE' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_ESV' OR
       <fs_cat>-fieldname     EQ 'DS_OBSERVACAO'.
      <fs_cat>-no_out = abap_false.
    ELSE.
      <fs_cat>-no_out = abap_true.
    ENDIF.

    IF <fs_cat>-fieldname     EQ 'NM_PESO_SUBTOTAL' OR
       <fs_cat>-fieldname     EQ 'NM_PESO_LIQUIDO' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_UMI' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_IMP' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_AVA' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_ARD' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_QUE' OR
       <fs_cat>-fieldname     EQ 'NR_QTDE_ESV'.
      <fs_cat>-do_sum = abap_true.
    ENDIF.

    IF ( <fs_cat>-fieldname   EQ 'NM_PESO_LIQUIDO' ) OR ( <fs_cat>-fieldname   EQ 'DS_OBSERVACAO' ) .

      TRY .
          objeto->get_tp_status( IMPORTING e_tp_status = DATA(e_tp_status) ).
        CATCH zcx_carga.
      ENDTRY.

      IF e_tp_status EQ zif_carga=>st_status_fechado.
        IF NOT ( objeto->carga-ck_enviado_opus = abap_true AND objeto->carga-ck_recebido_opus EQ abap_false ).
          <fs_cat>-edit = abap_true.
        ELSE.
          <fs_cat>-edit = abap_false.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_9004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_9004.

  gs_variant_9004-report      = sy-repid.
  gs_variant_9004-handle      = '9004'.
  gs_variant_9004-log_group   = abap_false.
  gs_variant_9004-username    = abap_false.
  gs_variant_9004-variant     = abap_false.
  gs_variant_9004-text        = abap_false.
  gs_variant_9004-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONFERE_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CK_RETORNO  text
*----------------------------------------------------------------------*
FORM confere_carga  CHANGING p_retorno TYPE sy-subrc p_automatico TYPE char01.

  DATA: lc_contador TYPE zde_sequencia_nota.

  p_retorno = 9.
  ck_validacao_conferencia = abap_false.
  ck_validacao_saida_autom = abap_false.
  ck_efetuar_saida_autom   = abap_false.

  DESCRIBE TABLE it_notas LINES lc_qtd_notas.

  CLEAR: it_valida_notas.

  lc_contador = 0.
  DO lc_qtd_notas TIMES.
    ADD 1 TO lc_contador.
    CLEAR: wa_valida_notas.
    wa_valida_notas-id_numero = lc_contador.
    APPEND wa_valida_notas TO it_valida_notas.
  ENDDO.

  READ TABLE it_valida_notas INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_valida_nota>).
  MOVE-CORRESPONDING <fs_valida_nota> TO zde_zsdt0001cg_valida_nota.
  <fs_valida_nota>-line_color = cs_line_color_selecionada.

  screen_tela = 9007.

  CALL SCREEN 9010.

  IF ck_validacao_conferencia EQ abap_true AND ck_validacao_saida_autom EQ abap_true.
    p_retorno = 0.
  ELSE.
    p_retorno = 1.
  ENDIF.

  p_automatico = ck_efetuar_saida_autom.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_9004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_9004 .

  CLEAR: event_handler_9004.

  IF ctl_alv_9004 IS NOT INITIAL.
    ctl_alv_9004->free( ).
  ENDIF.

  CLEAR: ctl_alv_9004.

  IF ctl_cccontainer_9004 IS NOT INITIAL.
    ctl_cccontainer_9004->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_9004.

ENDFORM.
