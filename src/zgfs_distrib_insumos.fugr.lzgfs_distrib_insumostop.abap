FUNCTION-POOL zgfs_distrib_insumos.         "MESSAGE-ID ..

**********************************************
* TYPES
**********************************************
TYPES: BEGIN OF ty_saida.
         INCLUDE         TYPE zsdt0415.
TYPES:   qte_sol2 TYPE char20.
TYPES:   descr_etapa     TYPE char50.
TYPES:   cancelar_dist   TYPE icon_d.
TYPES:   rejeitado       TYPE char01.
TYPES:   status_back     TYPE zsded045.
TYPES:   integra_safra   TYPE char01.
TYPES:   icon_status     TYPE icon_d.
TYPES:   dist_cancelada  TYPE icon_d.
TYPES: END   OF ty_saida.

TYPES: BEGIN OF ty_saida_estoque,
         nro_sol         TYPE zsdt0415-nro_sol,
         seq             TYPE zsdt0415-seq,
         vbeln           TYPE zsdt0415-vbeln,
         posnr           TYPE zsdt0415-posnr,
         nro_sol_new     TYPE zsdt0415-nro_sol,
         vbeln_new       TYPE zsdt0415-vbeln,
         posnr_new       TYPE zsdt0415-posnr,
         nro_sol_ref     TYPE zsdt0415-nro_sol,
         seq_ref         TYPE zsdt0415-seq,
         qte_sol         TYPE kwmeng,
         solic_pai       TYPE zsdt0415-solic_pai,
         nro_sol_filha   TYPE zsdt0415-nro_sol_filha,
         solic_cancelada TYPE zsdt0415-solic_cancelada,
         user_create     TYPE zsdt0415-user_create,
         date_create     TYPE zsdt0415-date_create,
         time_create     TYPE zsdt0415-time_create,
         line_color      TYPE char4.
TYPES: END   OF ty_saida_estoque.

**********************************************
* VARIAVEIS GLOBAIS
**********************************************
DATA: lc_zsds093               TYPE zsds093,
      lc_zsds094               TYPE zsds094_tt,
      wl_zsds094               TYPE zsds094,
      lc_distribuicao_insumos  TYPE REF TO zcl_distribuicao_insumos,
      w_zsdt0082               TYPE zsdt0082,
      t_zsdt0415               TYPE zsdt0415_t,
      t_zsdt0415_pend          TYPE zsdt0415_t,
*
      t_saida                  TYPE TABLE OF ty_saida,
      t_saida_estoque          TYPE TABLE OF ty_saida_estoque,
      w_saida                  TYPE ty_saida,
      w_saida_estoque          TYPE ty_saida_estoque,
      lv_start                 TYPE char01,
      lv_emproc                TYPE char01,
      lv_visualizar            TYPE char01,
      lv_total_consum          TYPE kwmeng,
*
      g_grid                   TYPE REF TO cl_gui_alv_grid,
      g_custom_container       TYPE REF TO cl_gui_custom_container,
*
      c_alv_toolbarmanager     TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1              TYPE REF TO cl_gui_container,
      cl_container_95          TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id            TYPE REF TO cl_dd_document,
      picture                  TYPE REF TO cl_gui_picture,
      l_graphic_conv           TYPE i,
      l_graphic_offs           TYPE i,
      graphic_size             TYPE i,
      l_graphic_xstr           TYPE xstring,
      url(255)                 TYPE c,
      graphic_url(255),
      t_function               TYPE ui_functions,
      w_function               TYPE ui_func,
      ok_code                  TYPE sy-ucomm,
*
      lc_dados                 TYPE zmme_dados_safra,
      lo_order_itens           TYPE REF TO zcl_int_ob_safra_crt_ord_itens,
      l_msg_error              TYPE string,
      ls_retorno_consulta_item TYPE zsds392,
      ls_order_itens           TYPE zde_safra_control_ordem_itens,
      lc_insumos               TYPE REF TO zcl_distribuicao_insumos,
*
      t_fieldcat               TYPE lvc_t_fcat,
      w_fieldcat               TYPE lvc_s_fcat,
      t_sort                   TYPE lvc_t_sort,
      w_sort                   TYPE lvc_s_sort,
      t_color                  TYPE lvc_t_scol,
      w_color                  TYPE lvc_s_scol,
      t_ucomm                  TYPE TABLE OF syst_ucomm,
      w_ucomm                  TYPE syst_ucomm,
      t_exctab                 TYPE slis_t_extab,
      w_exctab                 TYPE slis_extab,
      w_layout                 TYPE lvc_s_layo,
      w_layout_parc            TYPE lvc_s_layo,
      w_layout_wf              TYPE lvc_s_layo,
      w_layout_item            TYPE lvc_s_layo,
      w_stable                 TYPE lvc_s_stbl    VALUE 'XX',
      t_style                  TYPE lvc_t_styl,
      w_style                  TYPE lvc_s_styl,
      t_rows                   TYPE lvc_t_row,
      t_rows_head              TYPE lvc_t_row,
      w_rows                   TYPE lvc_s_row.

**********************************************
* CLASSE
**********************************************
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no,

      handle_on_button_click FOR EVENT button_click  OF cl_gui_alv_grid IMPORTING es_col_id es_row_no.

    CLASS-METHODS:
      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      toolbar               FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      toolbar_item          FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_on_button_click.

    DATA: lv_var_answer  TYPE c.

    FREE:  lc_insumos, lo_order_itens.         "*-CS2025000249-04.07.2025-#181842-JT
    CREATE OBJECT: lc_insumos, lo_order_itens. "*-CS2025000249-04.07.2025-#181842-JT

    READ TABLE t_saida INTO w_saida INDEX es_row_no-row_id.

    IF es_col_id = 'CANCELAR_DIST'.
      CHECK w_saida-cancelar_dist <> abap_off.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      DATA(_msg_erro_canc) = zcl_distribuicao_insumos=>cancelar_distribuicao(
        EXPORTING
          i_nro_sol   = w_saida-nro_sol_ref
          i_seq       = w_saida-seq_ref
          i_vbeln     = w_saida-vbeln
          i_posnr     = w_saida-posnr ).

      IF _msg_erro_canc is NOT INITIAL.
        MESSAGE _msg_erro_canc TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

*      SELECT SINGLE *
*        FROM zsdt0082
*        INTO @DATA(_zsdt0082)
*       WHERE nro_sol = @w_saida-nro_sol_ref
*         AND seq     = @w_saida-seq_ref
*         AND vbeln   = @w_saida-vbeln
*         AND posnr   = @w_saida-posnr.
*
*      CHECK sy-subrc = 0.
*
*      IF _zsdt0082-seq = 1.
*        MESSAGE s024(sd) WITH 'Não é possível cancelar esse registro, ' 'pois ele não é uma Distribuição!' DISPLAY LIKE 'E'.
*      ELSE.
*        CASE _zsdt0082-status.
*          WHEN 4.
*            MESSAGE s024(sd) WITH 'Distribuição já Cancelada!' DISPLAY LIKE 'E'.
*
*          WHEN 5.
*            MESSAGE s024(sd) WITH 'Distribuição já se encontra em fase ' 'de Planejamento/Produção/Entrega!' DISPLAY LIKE 'E'.
*
*          WHEN 2.
*            CALL FUNCTION 'POPUP_TO_CONFIRM'
*              EXPORTING
*                titlebar              = 'Confirmação'
*                text_question         = 'Deseja Cancelar esta Distribuição?'
*                text_button_1         = 'Sim'
*                text_button_2         = 'Não'
*                default_button        = '1'
*                display_cancel_button = ''
*                start_column          = 40
*                start_row             = 10
*              IMPORTING
*                answer                = lv_var_answer
*              EXCEPTIONS
*                text_not_found        = 1
*                OTHERS                = 2.
*
*            CHECK lv_var_answer = '1'.
*
*
*            TRY .
*                CALL METHOD lo_order_itens->set_metodo_http
*                  EXPORTING
*                    i_metodo = 'GET'.
*
*                ls_order_itens-externalid = _zsdt0082-nro_sol && _zsdt0082-seq.
*
*                CALL METHOD lo_order_itens->zif_integracao_outbound~execute_request
*                  EXPORTING
*                    i_info_request  = ls_order_itens
*                  IMPORTING
*                    e_id_integracao = DATA(lv_id_integracao)
*                    e_integracao    = DATA(lwa_integracao).
*
*                /ui2/cl_json=>deserialize( EXPORTING json        = lwa_integracao-ds_data_retorno
*                                                     pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*                                            CHANGING data        = ls_retorno_consulta_item ).
*
*                IF ls_retorno_consulta_item IS NOT INITIAL.
*                  w_saida-integra_safra = abap_true.              "*-CS2025000249-04.07.2025-#181842-JT
*                  w_saida-status_back   = _zsdt0082-status.
*                  MODIFY t_saida     FROM w_saida INDEX es_row_no-row_id.
*                ENDIF.
*              CATCH zcx_integracao INTO DATA(lo_integracao).
*              CATCH zcx_error INTO DATA(lo_error).
*            ENDTRY.
*
*            UPDATE zsdt0082 SET status    = 4
*                                user_canc = sy-uname
*                                dt_canc   = sy-datum
*                          WHERE nro_sol   = _zsdt0082-nro_sol
*                            AND vbeln     = _zsdt0082-vbeln
*                            AND posnr     = _zsdt0082-posnr
*                            AND seq       = _zsdt0082-seq.
*            COMMIT WORK AND WAIT .
*
*            "*-CS2025000249-04.07.2025-#181842-JT-inicio
*            IF w_saida-integra_safra = abap_true.
*              lc_dados-nro_sol = _zsdt0082-nro_sol.
*              lc_dados-seq     = _zsdt0082-seq.
*              lc_dados-vbeln   = _zsdt0082-vbeln.
*              lc_dados-posnr   = _zsdt0082-posnr.
*              lc_insumos->set_integrar_safra_control( EXPORTING i_dados = lc_dados IMPORTING e_msg_erro = l_msg_error ).
*
*              IF l_msg_error IS NOT INITIAL.
*                MESSAGE s024(sd) WITH l_msg_error DISPLAY LIKE 'E'.
*
*                UPDATE zsdt0082 SET status    = w_saida-status_back
*                                    user_canc = sy-uname
*                                    dt_canc   = sy-datum
*                              WHERE nro_sol   = _zsdt0082-nro_sol
*                                AND vbeln     = _zsdt0082-vbeln
*                                AND posnr     = _zsdt0082-posnr
*                                AND seq       = _zsdt0082-seq.
*                COMMIT WORK AND WAIT.
*                RETURN.
*              ENDIF.
*              MESSAGE s024(sd) WITH 'Distribuição Cancelada!'.
*            ENDIF.
*        ENDCASE.
*      ENDIF.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    ENDIF.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_hotspot_click.
  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.
  ENDMETHOD.

  METHOD toolbar.
  ENDMETHOD.

  METHOD toolbar_item.
  ENDMETHOD.

ENDCLASS.

************************************************************************************
************************************************************************************
