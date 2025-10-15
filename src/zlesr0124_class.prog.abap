*&---------------------------------------------------------------------*
*&  Include           ZLESR0111_CLASS
*&---------------------------------------------------------------------*


CLASS lcl_alv_toolbar_0100 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    REFRESH: tg_parametros.

    CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
      EXPORTING
        user_name           = sy-uname
      TABLES
        user_parameters     = tg_parametros
      EXCEPTIONS
        user_name_not_exist = 1
        OTHERS              = 2.

    READ TABLE tg_parametros INTO DATA(wl_parametros) WITH KEY parid = 'ZCCT_CANC_RCC_IMP'.
    IF sy-subrc EQ 0.
      ty_toolbar-icon      = icon_storno.
      ty_toolbar-function  = c_cancel_recep_carga.
      ty_toolbar-text      = 'Cancelar Recepção Carga'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    ty_toolbar-icon      = icon_history.
    ty_toolbar-function  = c_log_cancel.
    ty_toolbar-text      = 'Log Cancelamento'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_viewer_optical_archive.
    ty_toolbar-function  = c_doc_rat_recepcao.
    ty_toolbar-text      = 'Documentos Recepção'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

**---CS2019001391 - Jaime Tassoni - 26.11.2020 - inicio
*    ty_toolbar-icon      = icon_database_table.
*    ty_toolbar-function  = c_log_cct.
*    ty_toolbar-text      = 'Consulta Documentos CCT'.
*    ty_toolbar-butn_type = 0.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.
**---CS2019001391 - Jaime Tassoni - 26.11.2020 - inicio

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN c_log_cancel.
        PERFORM f_log_cancel_recepcao.
      WHEN c_cancel_recep_carga.
        PERFORM f_cancel_recep_carga.
      WHEN c_doc_rat_recepcao.
        PERFORM f_doc_rat_recepcao.
*---CS2019001391 - Jaime Tassoni - 26.11.2020 - inicio
*      WHEN c_log_cct.
*        CALL FUNCTION 'ZSDMF_CONSULTA_CCT'.
*---CS2019001391 - Jaime Tassoni - 26.11.2020 - fim
    ENDCASE.

    CASE e_ucomm.
      WHEN c_cancel_recep_carga.

        PERFORM: f_selecionar_dados,
                 f_processa_dados.

        LEAVE TO SCREEN 0100.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_event_handler_0100 IMPLEMENTATION.


  METHOD catch_hotspot.

    DATA: it_rsparams TYPE TABLE OF rsparams,
          wa_rsparams TYPE rsparams.

    DATA: opt     TYPE ctu_params,
          vl_lote TYPE zglt034-lote.

    CASE e_column_id.
      WHEN 'LOTE'.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.


FORM f_doc_rat_recepcao.

  DATA: tg_0168_out TYPE TABLE OF zlest0168 WITH HEADER LINE.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  IF lines( it_sel_rows[] ) > 1.
    MESSAGE 'Selecione somente uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
  CHECK sy-subrc EQ 0.

  READ TABLE it_saida_0100_01 INTO wa_saida_0100_01 INDEX wa_sel_rows-index.

  CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100_01-id_recepcao IS NOT INITIAL ).

  CLEAR: tg_0168_out[].

  SELECT *
    FROM zlest0168 INTO CORRESPONDING FIELDS OF TABLE tg_0168_out
   WHERE id_recepcao  EQ wa_saida_0100_01-id_recepcao.

  CHECK tg_0168_out[] IS NOT INITIAL.

  REFRESH estrutura.
  PERFORM f_montar_estrutura USING:
     01  ''   ''            'TG_0168_OUT' 'ID_RECEPCAO'   'Id.Rec.'         '10' '',
     02  ''   ''            'TG_0168_OUT' 'CHAVE_NFE'     'Chave NF-e'      '44' '',
     03  ''   ''            'TG_0168_OUT' 'CHAVE_NFF'     'Chave NF-f'      '37' '',
     04  ''   ''            'TG_0168_OUT' 'DOCNUM'        'Docnum'          '10' '',
     05  ''   ''            'TG_0168_OUT' 'PESO_AFERIDO'  'Peso Aferido'    '13' ''.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat           = estrutura[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 135
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tg_0168_out.


ENDFORM.
