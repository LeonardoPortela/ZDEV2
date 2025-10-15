*&---------------------------------------------------------------------*
*& Include          ZLESR0183TOP
*&---------------------------------------------------------------------*

TABLES zsdt0001.

TYPES: BEGIN OF ty_field_cat,
         fieldname  TYPE lvc_s_fcat-fieldname,
         coltext    TYPE lvc_s_fcat-coltext,
         edit       TYPE lvc_s_fcat-edit,
         outputlen  TYPE lvc_s_fcat-outputlen,
         f4availabl TYPE lvc_s_fcat-f4availabl,
         no_out     TYPE lvc_s_fcat-no_out,
         hotspot    TYPE lvc_s_fcat-hotspot,    "Adicionado campo hotspot
         just       TYPE lvc_s_fcat-just,
       END OF ty_field_cat,

       BEGIN OF ty_saida,
         bukrs          TYPE zsdt0001-bukrs,
         branch         TYPE zsdt0001-branch,
         vbeln          TYPE zsdt0001-vbeln,
         nr_romaneio    TYPE zsdt0001-nr_romaneio,
         dt_movimento   TYPE zsdt0001-dt_movimento,
         agente_frete   TYPE zsdt0001-agente_frete,
         nr_ordem       TYPE zsdt0001od-nr_ordem,
         id_ordem       TYPE zsdt0001-id_ordem,
         viagem_id      TYPE zlest0185-viagem_id,
         id_lote_frete  TYPE zlest0185-id_lote_frete,
         id_localizador TYPE zlest0185-id_localizador,
         ztrocanota     TYPE vbak-ztrocanota,
         status         TYPE zsdt0001-status,
       END OF ty_saida.

DATA: t_field_cat TYPE STANDARD TABLE OF ty_field_cat WITH EMPTY KEY,
      wa_fieldcat TYPE lvc_s_fcat,
      t_fieldcat  TYPE lvc_t_fcat,
      t_saida     TYPE TABLE OF ty_saida,
      o_alv       TYPE REF TO cl_gui_alv_grid.

CLASS lcl_alv DEFINITION.
  PUBLIC SECTION.


    METHODS:
*      on_f4_out3 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells,
*      on_hotspot3 FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id e_row_id es_row_no,
      on_toolbar_out3 FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,
      on_user_command_out3 FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.
  METHOD on_toolbar_out3.

    DATA : mt_toolbar TYPE stb_button.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'DESFAZER_GRID'.   "fcode
    mt_toolbar-icon = icon_system_undo.
    mt_toolbar-quickinfo = 'Desfazer vinculo'.
    mt_toolbar-text = 'Desfazer vinculo'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'LOG_GRID'.   "fcode
    mt_toolbar-icon = icon_protocol.
    mt_toolbar-quickinfo = 'Log Modificações'.
    mt_toolbar-text = 'Log Modificações'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'ENVIADO_GRID'.   "fcode
    mt_toolbar-icon = icon_mail.
    mt_toolbar-quickinfo = 'Enviado strada'.
    mt_toolbar-text = 'Enviado strada'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD on_user_command_out3.
    DATA: lt_rows     TYPE lvc_t_row,
          ls_row      TYPE lvc_s_roid,
          qtd_rows    TYPE int4,
          lt_zsdt0001 TYPE TABLE OF zsdt0001,
          ls_254      TYPE zlest0254,
          o_alv_log   TYPE REF TO cl_salv_table,
          lt_log      TYPE TABLE OF zlest0254.

    CALL METHOD o_alv->get_selected_rows IMPORTING et_index_rows = lt_rows.
    qtd_rows = lines( lt_rows ).

    DESCRIBE TABLE lt_rows LINES DATA(l_lines).

    IF l_lines > 2
     OR l_lines EQ 0.
      MESSAGE s001(zsd) DISPLAY LIKE 'E' WITH TEXT-002 space space.
      EXIT.
    ENDIF.

    CASE e_ucomm.
      WHEN 'DESFAZER_GRID'. "OR 'LOG_GRID'.

        CALL METHOD o_alv->get_selected_rows IMPORTING et_index_rows = lt_rows.
        qtd_rows = lines( lt_rows ).

        IF lt_rows IS NOT INITIAL.
          LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).
            READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <fs_rows>-index.

            IF sy-subrc IS INITIAL.

              SELECT * UP TO 1 ROWS
                FROM zsdt0001
                INTO @DATA(ls_0001)
                WHERE nr_romaneio  = @<fs_saida>-nr_romaneio
                  AND vbeln        = @<fs_saida>-vbeln
                  AND dt_movimento = @<fs_saida>-dt_movimento
                  AND bukrs        = @<fs_saida>-bukrs
                  AND branch       = @<fs_saida>-branch
                  AND status       = @<fs_saida>-status
                  AND agente_frete = @<fs_saida>-agente_frete
                  AND id_ordem     = @<fs_saida>-id_ordem.
              ENDSELECT.

              IF sy-subrc IS INITIAL.

                SELECT tknum, belnr UP TO 1 ROWS
                  FROM zlest0032
                  INTO @DATA(ls_0032)
                  WHERE tknum = @ls_0001-doc_transp.
                ENDSELECT.

                IF sy-subrc IS INITIAL.
                  IF ls_0032-belnr IS NOT INITIAL.
                    MESSAGE s001(zsd) DISPLAY LIKE 'E' WITH TEXT-001 ls_0032-belnr space.
                  ELSE.
                    IF ls_0001-docs_enviado_carguero IS NOT INITIAL.
                      CLEAR ls_0001-docs_enviado_carguero.
                      MODIFY: zsdt0001 FROM ls_0001.

                      ls_254-ch_referencia = ls_0001-ch_referencia.
                      ls_254-uname         = sy-uname.
                      ls_254-datum         = sy-datum.
                      ls_254-uzeit         = sy-uzeit.

                      MODIFY zlest0254 FROM ls_254.

                      CLEAR <fs_saida>-id_ordem.
                    ENDIF.

                    SELECT SINGLE *
                      FROM zlest0185
                      INTO @DATA(ls_0185)
                      WHERE viagem_id = @<fs_saida>-viagem_id.

                    IF sy-subrc IS INITIAL.
                      IF ls_0185-id_ordem IS NOT INITIAL.

                        CLEAR: ls_0185-id_ordem,
*                               ls_0185-viagem_id,
*                               ls_0185-id_lote_frete,
*                               ls_0185-id_localizador,
                               <fs_saida>-id_ordem,
                               <fs_saida>-viagem_id,
                               <fs_saida>-id_lote_frete,
                               <fs_saida>-id_localizador.

                        MODIFY: zlest0185 FROM ls_0185.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
              CALL METHOD o_alv->refresh_table_display( ).
            ENDIF.
          ENDLOOP.
        ENDIF.
      WHEN 'LOG_GRID'.

        CALL METHOD o_alv->get_selected_rows IMPORTING et_index_rows = lt_rows.
        qtd_rows = lines( lt_rows ).

        IF lt_rows IS NOT INITIAL.
          LOOP AT lt_rows ASSIGNING <fs_rows>.
            READ TABLE t_saida INTO DATA(ls_saida) INDEX <fs_rows>-index.

            IF sy-subrc IS INITIAL.

              SELECT * UP TO 1 ROWS
                FROM zsdt0001
                INTO ls_0001
                WHERE nr_romaneio  = ls_saida-nr_romaneio
                  AND vbeln        = ls_saida-vbeln
                  AND dt_movimento = ls_saida-dt_movimento
                  AND bukrs        = ls_saida-bukrs
                  AND branch       = ls_saida-branch
                  AND status       = ls_saida-status
                  AND agente_frete = ls_saida-agente_frete.
*                  AND id_ordem     = ls_saida-id_ordem.
              ENDSELECT.

              IF ls_0001-ch_referencia IS NOT INITIAL.

                SELECT SINGLE *
                  FROM zlest0254
                  INTO @DATA(ls_log)
                  WHERE ch_referencia = @ls_0001-ch_referencia.

                IF sy-subrc IS INITIAL.

                  APPEND ls_log TO lt_log.
                  TRY.
                      cl_salv_table=>factory(
                        IMPORTING
                          r_salv_table = o_alv_log
                        CHANGING
                          t_table      = lt_log[] ). "Internal Table

                    CATCH cx_salv_msg.
                  ENDTRY.
                  o_alv_log->display( ).
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      WHEN 'ENVIADO_GRID'.

        CALL METHOD o_alv->get_selected_rows IMPORTING et_index_rows = lt_rows.
        qtd_rows = lines( lt_rows ).

        IF lt_rows IS NOT INITIAL.
          LOOP AT lt_rows ASSIGNING <fs_rows>.
            READ TABLE t_saida INTO ls_saida INDEX <fs_rows>-index.

            IF sy-subrc IS INITIAL.

              SELECT * UP TO 1 ROWS
                FROM zsdt0001
                INTO ls_0001
                WHERE nr_romaneio  = ls_saida-nr_romaneio
                  AND vbeln        = ls_saida-vbeln
                  AND dt_movimento = ls_saida-dt_movimento
                  AND bukrs        = ls_saida-bukrs
                  AND branch       = ls_saida-branch
                  AND status       = ls_saida-status
                  AND agente_frete = ls_saida-agente_frete
                  AND id_ordem     = ls_saida-id_ordem.
              ENDSELECT.

              IF sy-subrc IS INITIAL.

                IF ls_saida-ztrocanota EQ abap_true.
                  ls_0001-docs_enviado_carguero = abap_true.
                ENDIF.
                APPEND ls_0001 TO lt_zsdt0001.
                CLEAR ls_0001.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF lt_zsdt0001 IS NOT INITIAL.

          READ TABLE lt_zsdt0001 TRANSPORTING NO FIELDS
               WITH KEY docs_enviado_carguero = abap_true.

          IF sy-subrc IS INITIAL.
            MESSAGE s001(zsd)
            DISPLAY LIKE 'S'
            WITH TEXT-s01.

          ELSE.
            MESSAGE s001(zsd)
            DISPLAY LIKE 'E'
            WITH TEXT-e04.
          ENDIF.

          MODIFY zsdt0001 FROM TABLE lt_zsdt0001.
        ENDIF.

    ENDCASE.

    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.

DATA: lo_report_alv TYPE REF TO lcl_alv.

SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-c01.
  SELECT-OPTIONS: s_ov     FOR zsdt0001-vbeln, "OBLIGATORY,
                  s_ordem  FOR zsdt0001-vbeln,
                  s_nr     FOR zsdt0001-nr_romaneio,
                  s_centro FOR zsdt0001-branch,
                  s_dt_m   FOR zsdt0001-dt_movimento.
SELECTION-SCREEN END OF BLOCK part1.
