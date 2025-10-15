*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      toolbar               FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_hotspot_click.
    READ TABLE t_saida INTO w_saida INDEX e_row_id-index.

    CASE e_column_id.
      WHEN 'ID_SEQ'.
        SUBMIT zsdr0147_2
        WITH s_lote EQ w_saida-id_seq
        WITH s_bukrs EQ w_saida-bukrs
        WITH s_werks EQ w_saida-werks AND RETURN.

        PERFORM f_selecao_dados    USING 0
                                CHANGING l_erro.
        IF l_erro = abap_false.
          PERFORM f_processa_dados USING abap_true.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.

    CASE e_ucomm.

      WHEN 'VINCULAR'.
        l_editar = abap_false.

        PERFORM f_vincular.
        PERFORM f_selecao_dados    USING 0
                                CHANGING l_erro.
        IF l_erro = abap_false.
          PERFORM f_processa_dados USING abap_false.
        ENDIF.

      WHEN 'DESVINCULAR'.
        l_editar = abap_false.

        PERFORM f_desvincular      USING e_ucomm.
        PERFORM f_selecao_dados    USING 0
                                CHANGING l_erro.
        IF l_erro = abap_false.
          PERFORM f_processa_dados USING abap_false.
        ENDIF.

      WHEN 'EDIT_LOTE'.
        l_editar = abap_false.

        PERFORM f_editar_lote.

      WHEN 'DEL_LOTE'.
        l_editar = abap_false.

        PERFORM f_desvincular        USING e_ucomm.
        PERFORM f_selecao_dados      USING l_lote_editado
                                  CHANGING l_erro.
        IF l_erro = abap_false.
          PERFORM f_processa_dados   USING abap_false.
        ENDIF.

      WHEN 'ADD_LOTE'.
        "140791 CS2023000951 Melhoria ZSDT0158 - PSA
**********************************************************************
        READ TABLE t_saida[] INTO DATA(_filter_list) INDEX 1.

        DATA: lt_saida TYPE STANDARD TABLE OF ty_saida,
              lr_saida TYPE ty_saida.
        FREE: lt_saida, t_ekpo, t_zsdt0306, t_zsdt0225.

        SELECT ekpo~ebeln,ekpo~ebelp,eket~etenr,eket~charg,
               ekpo~loekz,ekko~aedat,ekko~bukrs,ekpo~werks,
               ekpo~lgort,ekpo~matnr,ekpo~matkl,ekpo~txz01,
               ekpo~menge,ekpo~meins,lfa1~lifnr,lfa1~name1,
               ekko~unsez
          FROM ekko
         INNER JOIN ekpo ON ekpo~ebeln = ekko~ebeln
         INNER JOIN eket ON eket~ebeln = ekko~ebeln
         INNER JOIN lfa1 ON lfa1~lifnr = ekko~lifnr
         WHERE 1 = 1
           "AND ekko~ebeln IN s_ebeln
           "AND ekko~lifnr IN s_lifnr
           AND ekko~bukrs = @_filter_list-bukrs
           AND ekpo~werks = @_filter_list-werks
          AND ekpo~matnr = @_filter_list-matnr
          AND ekpo~matkl = @_filter_list-matkl
           "AND ekko~aedat IN s_data
           AND ekpo~loekz  = @abap_off
                  INTO TABLE @DATA(t_ekpo).

        CHECK t_ekpo[] IS NOT INITIAL.

        SORT t_ekpo BY ebeln ebelp.
        DELETE ADJACENT DUPLICATES FROM t_ekpo
                              COMPARING ebeln ebelp.

        SELECT *
          FROM zsdt0306
          INTO TABLE t_zsdt0306
           FOR ALL ENTRIES IN t_ekpo
         WHERE ebeln = t_ekpo-ebeln
           AND ebelp = t_ekpo-ebelp.

        IF t_zsdt0306[] IS NOT INITIAL.
          SELECT *
            FROM zsdt0225
            INTO TABLE t_zsdt0225
             FOR ALL ENTRIES IN t_zsdt0306
           WHERE id_seq = t_zsdt0306-id_seq.
        ENDIF.

        "f_processa_dados
        FREE: lt_saida.

        LOOP AT t_ekpo INTO w_ekpo.

          CLEAR: lr_saida, w_zsdt0306, w_zsdt0225.

          READ TABLE t_zsdt0306 INTO w_zsdt0306 WITH KEY ebeln  = w_ekpo-ebeln
                                                         ebelp  = w_ekpo-ebelp.
          CHECK sy-subrc <> 0.

          READ TABLE t_zsdt0225 INTO w_zsdt0225 WITH KEY id_seq = w_zsdt0306-id_seq.

          lr_saida-status      = COND #( WHEN w_zsdt0306 IS INITIAL THEN icon_wf_unlink
                                                                   ELSE icon_wf_link ).

          lr_saida-id_seq      = w_zsdt0306-id_seq.
          lr_saida-lifnr       = w_ekpo-lifnr.
          lr_saida-lifnr_name  = w_ekpo-name1.
          lr_saida-ebeln       = w_ekpo-ebeln.
          lr_saida-ebelp       = w_ekpo-ebelp.
          lr_saida-bukrs       = w_ekpo-bukrs.
          lr_saida-bukrs_fat   = w_zsdt0306-bukrs_fat.
          lr_saida-werks_fat   = w_zsdt0306-werks_fat.
          lr_saida-werks       = w_ekpo-werks.
          lr_saida-matnr       = w_ekpo-matnr.
          lr_saida-matkl       = w_ekpo-matkl.
          lr_saida-lgort       = w_ekpo-lgort.
          lr_saida-charg       = w_ekpo-charg.
          lr_saida-unsez       = w_ekpo-unsez.
          lr_saida-txz01       = w_ekpo-txz01.
          lr_saida-menge       = w_ekpo-menge.
          lr_saida-meins       = w_ekpo-meins.
          lr_saida-nr_ov       = w_zsdt0225-nr_ov.
          lr_saida-docnum      = w_zsdt0225-docnum.
          APPEND lr_saida     TO lt_saida.
        ENDLOOP.

        DATA: lt_return TYPE TABLE OF ddshretval,
              ls_return TYPE ddshretval,
              ls_t001   TYPE t001.

        IF lt_saida IS NOT INITIAL.

          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
*             DDIC_STRUCTURE  = ' '
              retfield        = 'EBELN'
*             PVALKEY         = ' '
*             DYNPPROG        = ' '
*             DYNPNR          = ' '
*             DYNPROFIELD     = ' '
*             STEPL           = 0
*             WINDOW_TITLE    =
*             VALUE           = ' '
              value_org       = 'S'
*             MULTIPLE_CHOICE = ' '
*             DISPLAY         = ' '
*             CALLBACK_PROGRAM       = ' '
*             CALLBACK_FORM   = ' '
*             MARK_TAB        =
*       _RESET            IMPORTING
*             USER            =
            TABLES
              value_tab       = lt_saida
*             FIELD_TAB       =
              return_tab      = lt_return
*             DYNPFLD_MAPPING =
            EXCEPTIONS
              parameter_error = 1
              no_values_found = 2
              OTHERS          = 3.

          IF sy-subrc = 0.
            READ TABLE lt_return INTO ls_return INDEX 1.
            IF sy-subrc = 0.
              DATA: l_EBELN TYPE ekpo-ebeln.
              CLEAR: l_EBELN.
              l_EBELN = ls_return-fieldval.

              READ TABLE lt_saida INTO DATA(wa_rowvalue) WITH KEY ebeln = l_ebeln.
              CLEAR: w_zsdt0306.
              w_zsdt0306-mandt          = sy-mandt.
              w_zsdt0306-id_seq         = _filter_list-id_seq.
              w_zsdt0306-ebeln          = wa_rowvalue-ebeln.
              w_zsdt0306-ebelp          = wa_rowvalue-ebelp.
              w_zsdt0306-lifnr          = wa_rowvalue-lifnr.
              w_zsdt0306-bukrs          = wa_rowvalue-bukrs.
              w_zsdt0306-bukrs_fat      = wa_rowvalue-bukrs_fat.    "*-CS2022000686-09.02.2023-#102944-JT
              w_zsdt0306-werks_fat      = wa_rowvalue-werks_fat.    "*-CS2022000686-09.02.2023-#102944-JT
              w_zsdt0306-werks          = wa_rowvalue-werks.
              w_zsdt0306-lgort          = wa_rowvalue-lgort.
              w_zsdt0306-charg          = wa_rowvalue-charg.
              w_zsdt0306-unsez          = wa_rowvalue-unsez.
              w_zsdt0306-matnr          = wa_rowvalue-matnr.
              w_zsdt0306-quantidade     = wa_rowvalue-menge.
              w_zsdt0306-emite_nfs      = abap_false.
              w_zsdt0306-usuario        = sy-uname.
              w_zsdt0306-data_registro  = sy-datum.
              w_zsdt0306-hora_registro  = sy-uzeit.
              "w_zsdt0306-operacao       = _filter_list-operacao.
              MODIFY zsdt0306        FROM w_zsdt0306.


              w_zsdt0225-mandt          = sy-mandt.
              w_zsdt0225-id_seq         = _filter_list-id_seq.
              w_zsdt0225-bukrs          = wa_rowvalue-bukrs_fat.          "*-CS2022000686-09.02.2023-#102944-JT
              w_zsdt0225-werks          = wa_rowvalue-werks_fat.          "*-CS2022000686-09.02.2023-#102944-JT
              w_zsdt0225-ano_viagem     = '0000'.
              w_zsdt0225-cl_codigo      = |{ wa_rowvalue-werks ALPHA = IN }|.
              w_zsdt0225-safra          = wa_rowvalue-lgort.
              w_zsdt0225-cod_material   = wa_rowvalue-matnr.
              w_zsdt0225-origem         = 'PI'.
              "w_zsdt0225-operacao       = 'IM'.  "*-CS2022000686-09.02.2023-#102944-JT
              w_zsdt0225-peso_vinculado = wa_rowvalue-menge.
              w_zsdt0225-usuario        = sy-uname.
              w_zsdt0225-data_registro  = sy-datum.
              w_zsdt0225-hora_registro  = sy-uzeit.
              APPEND w_zsdt0225        TO t_0225.


              PERFORM f_selecao_dados      USING l_lote_editado
                                        CHANGING l_erro.

              IF l_erro = abap_false.
                PERFORM f_processa_dados   USING abap_false.
              ENDIF.

            ENDIF.

          ENDIF.
        ELSE.
          DATA(msg_001) = |Não existem dados para associar a este Lote!|.
          MESSAGE msg_001 TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
**********************************************************************
        "140791 CS2023000951 Melhoria ZSDT0158 - PSA
*        l_editar = abap_false.
*
*        PERFORM f_adicionar_lote  CHANGING l_erro.
*        IF l_erro = abap_false.
*          PERFORM f_selecao_dados    USING l_lote_editado
*                                  CHANGING l_erro.
*          IF l_erro = abap_false.
*            PERFORM f_processa_dados USING abap_false.
*          ENDIF.
*        ENDIF.

    ENDCASE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.

  ENDMETHOD.

  METHOD toolbar.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    CLEAR wa_tool.
    wa_tool-function  = 'VINCULAR'.
    wa_tool-icon      = icon_relation.
    wa_tool-disabled  = COND #( WHEN l_editar = abap_false AND t_saida[] IS NOT INITIAL THEN abap_false
                                                                                        ELSE abap_true ).
    wa_tool-quickinfo = 'Vincular'.
    wa_tool-text      = 'Vincular'.
    APPEND wa_tool TO e_object->mt_toolbar.

    CLEAR wa_tool.
    wa_tool-function  = 'DESVINCULAR'.
    wa_tool-disabled  = COND #( WHEN l_editar = abap_false AND t_saida[] IS NOT INITIAL THEN abap_false
                                                                                        ELSE abap_true ).
    wa_tool-icon      = icon_failure.
    wa_tool-quickinfo = 'Desvincular'.
    wa_tool-text      = 'Desvincular'.
    APPEND wa_tool TO e_object->mt_toolbar.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.
    wa_tool-function  = 'EDIT_LOTE'.
    wa_tool-icon      = '@0Q@'.
    wa_tool-disabled  = COND #( WHEN l_editar = abap_false AND t_saida[] IS NOT INITIAL THEN abap_false
                                                                                        ELSE abap_true ).
    wa_tool-quickinfo = 'Editar Lote'.
    wa_tool-text      = 'Editar Lote'.
    APPEND wa_tool TO e_object->mt_toolbar.

    CLEAR wa_tool.
    wa_tool-function  = 'ADD_LOTE'.
    wa_tool-icon      = icon_add_row.
    wa_tool-disabled  = COND #( WHEN l_editar = abap_true  AND t_saida[] IS NOT INITIAL THEN abap_false
                                                                                        ELSE abap_true ).
    wa_tool-quickinfo = 'Adicionar ao Lote'.
    wa_tool-text      = 'Adicionar ao Lote'.
    APPEND wa_tool TO e_object->mt_toolbar.

    CLEAR wa_tool.
    wa_tool-function  = 'DEL_LOTE'.
    wa_tool-icon      = icon_remove_row.
    wa_tool-disabled  = COND #( WHEN l_editar = abap_true  AND t_saida[] IS NOT INITIAL THEN abap_false
                                                                                        ELSE abap_true ).
    wa_tool-quickinfo = 'Desvincular do Lote'.
    wa_tool-text      = 'Desvincular do Lote'.
    APPEND wa_tool TO e_object->mt_toolbar.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
