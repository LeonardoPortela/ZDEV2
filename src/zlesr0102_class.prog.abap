*&---------------------------------------------------------------------*
*&  Include           ZLESR0102_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD catch_hotspot.

    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

    CHECK sy-subrc = 0.

    CASE e_column_id.
      WHEN 'ICON'.
        PERFORM f_action_user_icon USING wa_saida.
      WHEN 'SEQ_LCTO'. " Documento ZNFW
        PERFORM f_action_user_doc_znfw USING wa_saida.
      WHEN 'DANFEZ'.
        PERFORM f_action_user_danfe_znfw USING wa_saida.
      WHEN 'AVISO'.
        PERFORM f_action_user_aviso USING wa_saida.
      WHEN 'REMESSA'.
        PERFORM f_action_user_remessa USING wa_saida.
      WHEN 'FATURA'.
        PERFORM f_action_user_fatura USING wa_saida 'L' CHANGING it_saida[] t_return[].
      WHEN 'DANFE'.
        PERFORM f_action_user_danfe USING wa_saida.
      WHEN 'TRANSP'.
        PERFORM f_action_user_transp USING wa_saida 'L' CHANGING it_saida[] t_return[].
*----CS2021000508 - 07.06.2021 - JT - inicio
      WHEN 'DOCS_CARGUERO'.
        PERFORM f_action_user_docs_carguero USING wa_saida 'L' CHANGING it_saida[] t_return[].
*----CS2021000508 - 07.06.2021 - JT - fim
      WHEN 'DOCCUS'.
        IF wa_saida-doccus NE icon_icon_list.
          SET PARAMETER ID 'FKK'  FIELD wa_saida-doccus.
          CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'OVSERV'.
        IF wa_saida-ovserv NE icon_icon_list.
          SET PARAMETER ID 'AUN'    FIELD wa_saida-ovserv.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'VBELN'.
        CHECK wa_saida-tipo = 'O'.

        IF wa_saida-vbeln NE icon_icon_list.
          SET PARAMETER ID 'AUN'  FIELD wa_saida-vbeln.
          CALL TRANSACTION 'VA03'  WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN .
        ENDIF.
      WHEN 'FATSERV'.
        IF wa_saida-fatserv  NE icon_icon_list.
          SET PARAMETER ID 'VF'   FIELD wa_saida-fatserv.
          CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'DACTE'.
        PERFORM f_action_user_dacte USING wa_saida.
    ENDCASE.

  ENDMETHOD.                    "CATCH_HOTSPOT


  METHOD on_data_changed.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value.

    DATA: vpeso_retido_i    TYPE i.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'LIFNR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      READ TABLE it_saida INTO wa_saida INDEX ls_good-row_id.

      READ TABLE it_zsdt0001 ASSIGNING FIELD-SYMBOL(<fs_zsdt0001>) WITH KEY ch_referencia = wa_saida-ch_referencia BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      "Atualiza variaveis de frete
      REFRESH it_zsdt0001_fre.
      <fs_zsdt0001>-agente_frete = lv_value.
      APPEND <fs_zsdt0001> TO it_zsdt0001_fre.
      PERFORM f_pega_frete.
      "<fs_zsdt0001>-agente_frete = lv_value.

      READ TABLE it_zsdt0001_fre INTO DATA(lwa_zsdt001_fre) INDEX 1.

      PERFORM f_atual_frete USING lwa_zsdt001_fre 'L' CHANGING wa_saida.
      REFRESH it_zsdt0001_fre.

      lv_value = wa_saida-kbetr.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'KBETR'
          i_value     = lv_value.

      lv_value = wa_saida-konwa.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'KONWA'
          i_value     = lv_value.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'EBELN'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      READ TABLE it_saida INTO wa_saida INDEX ls_good-row_id.

      READ TABLE it_zsdt0001 ASSIGNING <fs_zsdt0001> WITH KEY ch_referencia = wa_saida-ch_referencia BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      "Atualiza variaveis de frete
      REFRESH it_zsdt0001_fre.
      <fs_zsdt0001>-ebeln = lv_value.
      APPEND <fs_zsdt0001> TO it_zsdt0001_fre.
      PERFORM f_pega_frete.

      READ TABLE it_zsdt0001_fre INTO lwa_zsdt001_fre INDEX 1.
      IF sy-subrc EQ 0.
        <fs_zsdt0001>-shtyp  = lwa_zsdt001_fre-shtyp.
        <fs_zsdt0001>-ebeln  = lwa_zsdt001_fre-ebeln.
        wa_saida-shtyp       = lwa_zsdt001_fre-shtyp.
      ENDIF.

      PERFORM f_atual_frete USING lwa_zsdt001_fre 'L' CHANGING wa_saida.
      REFRESH it_zsdt0001_fre.

      lv_value = wa_saida-kbetr.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'KBETR'
          i_value     = lv_value.

      lv_value = wa_saida-konwa.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'KONWA'
          i_value     = lv_value.

      lv_value = wa_saida-shtyp.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'SHTYP'
          i_value     = lv_value.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE fieldname = 'PESO_DESCARGA'.
      READ TABLE it_saida INTO wa_saida INDEX ls_good-row_id.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      wa_saida-peso_descarga = lv_value.

      READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY ch_referencia = wa_saida-ch_referencia BINARY SEARCH.

      READ TABLE t_fatura_agrupada INTO w_fatura_agrupada WITH KEY
*                                                                   werks = p_branch
                                                                   werks = wa_zsdt0001-branch "RJF
                                                                   matnr = wa_saida-matnr
                                                                   kunnr = wa_saida-kunnr
                                                                   inco1 = vinco1
                                                                   cfop  = wa_saida-cfop.
      IF sy-subrc = 0.
        wa_saida-peso_retido = wa_saida-peso_descarga * ( w_fatura_agrupada-perc_ret / 100 ).
        vpeso_retido_i = wa_saida-peso_retido.
        wa_saida-peso_retido = wa_saida-peso_retido - ( wa_saida-peso_retido - vpeso_retido_i ).

        wa_saida-peso_liq_pos_ret  = wa_saida-peso_descarga - wa_saida-peso_retido.

        lv_value = wa_saida-peso_retido.
        CONDENSE lv_value NO-GAPS.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'PESO_RETIDO'
            i_value     = lv_value.

        lv_value = wa_saida-peso_liq_pos_ret.
        CONDENSE lv_value NO-GAPS.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'PESO_LIQ_POS_RET'
            i_value     = lv_value.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD on_f4.
    TYPES: BEGIN OF t_f4_structure,
             fieldtext TYPE dfies-fieldtext,
             fieldname TYPE dfies-fieldname,
           END OF t_f4_structure.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA: ls_modi TYPE lvc_s_modi.

    TYPES : BEGIN OF ty_t005s,
              bland TYPE t005s-bland,
            END OF ty_t005s,

            BEGIN OF ty_pedido,
              ebeln    TYPE zsdt0062-ebeln,
              ebelp    TYPE zsdt0062-ebelp, "CS2017002682 - 29.11.2017 - Ini
              qtd_vinc TYPE zsdt0062-qtd_vinc,
            END OF ty_pedido.

    DATA: wl_return_chv TYPE  ddshretval,
          wl_dselchv    TYPE  dselc,
          tl_t005s      TYPE TABLE OF ty_t005s,
          wl_t005s      TYPE ty_t005s,
          tl_pedido     TYPE TABLE OF ty_pedido,
          wl_pedido     TYPE ty_pedido,
          tl_return_chv TYPE TABLE OF ddshretval,
          tl_dselchv    TYPE TABLE OF dselc,
          wa_style      TYPE lvc_s_styl.

    READ TABLE it_saida INTO wa_saida INDEX es_row_no-row_id.
    CASE e_fieldname.
      WHEN 'EBELN'.
        IF wa_saida-tipo = 'O'. "Somente Ordem

          CLEAR: tl_pedido[].
          LOOP AT it_zsdt0062 INTO DATA(lwa_zsdt0062) WHERE vbeln = wa_saida-vbeln.
            CLEAR: wl_pedido.
            wl_pedido-ebeln     = lwa_zsdt0062-ebeln.
            wl_pedido-ebelp     = lwa_zsdt0062-ebelp.
            wl_pedido-qtd_vinc  = lwa_zsdt0062-qtd_vinc.
            APPEND wl_pedido TO tl_pedido.
          ENDLOOP.

          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              retfield        = 'EBELN'
              value_org       = 'S'
            TABLES
              value_tab       = tl_pedido
              return_tab      = tl_return_chv
              dynpfld_mapping = tl_dselchv.
          READ TABLE it_saida INTO wa_saida INDEX es_row_no-row_id.
          READ TABLE wa_saida-style INTO wa_style WITH KEY fieldname = 'EBELN'.
          IF sy-subrc NE 0.
            READ TABLE tl_return_chv INTO wl_return_chv INDEX 1.
            IF sy-subrc = 0 AND wl_return_chv-fieldval <> ''.
              ASSIGN er_event_data->m_data->* TO <itab>.
              ls_modi-row_id    = es_row_no-row_id.
              ls_modi-fieldname = 'EBELN'.
              ls_modi-value     = wl_return_chv-fieldval.
              APPEND ls_modi TO <itab>.

              er_event_data->m_event_handled = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 'REGION'.
        SELECT DISTINCT bland
          FROM t005s
          INTO TABLE tl_t005s
          WHERE land1 = 'BR'.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'REGION'
            value_org       = 'S'
          TABLES
            value_tab       = tl_t005s
            return_tab      = tl_return_chv
            dynpfld_mapping = tl_dselchv.

        READ TABLE it_saida INTO wa_saida INDEX es_row_no-row_id.
        READ TABLE wa_saida-style INTO wa_style WITH KEY fieldname = 'REGION'.
        IF sy-subrc NE 0.
          READ TABLE tl_return_chv INTO wl_return_chv INDEX 1.
          IF sy-subrc = 0 AND wl_return_chv-fieldval <> ''.
            ASSIGN er_event_data->m_data->* TO <itab>.
            ls_modi-row_id    = es_row_no-row_id.
            ls_modi-fieldname = 'REGION'.
            ls_modi-value     = wl_return_chv-fieldval.
            APPEND ls_modi TO <itab>.

            er_event_data->m_event_handled = 'X'.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD. "on_f4

  METHOD set_toolbar.

    APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.
    APPEND VALUE #( function  = 'BTN_TXT_ITM_DLV'
                    icon      = icon_doc_position_proposal
                    butn_type = 0
                    text      = 'Txt. Item Remessa' ) TO e_object->mt_toolbar.

    IF vg_cockpit = '03'. "Agrupamento de Fatura
      IF NOT t_fatura_agrupada IS INITIAL AND r_dt_a IS NOT INITIAL.
        APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.

        APPEND VALUE #( function  = 'BTN_AGRUPAR'
                        icon      = icon_deselect_block
                        butn_type = 0
                        text      = 'Agrupar faturas' ) TO e_object->mt_toolbar.
      ENDIF.
    ENDIF.

*-CS2024000522-11.09.2024-JT-#151751-inicio
    IF vg_cockpit = '07'. "Fertilizantes
      APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.
      APPEND VALUE #( function  = 'ZLES0200'
                      icon      = icon_ps_relationship
                      butn_type = 0
                      text      = 'Vinc.NF.Conta e Ordem' ) TO e_object->mt_toolbar.
    ENDIF.
*-CS2024000522-11.09.2024-JT-#151751-fim

  ENDMETHOD.

  METHOD handle_user_command.

    DATA: row_record  TYPE ty_saida,
          rows_record TYPE TABLE OF ty_saida.

    CASE e_ucomm.

      WHEN 'BTN_TXT_ITM_DLV'.
        CALL METHOD cl_grid->get_selected_rows
          IMPORTING
            et_index_rows = DATA(_rows).

        IF _rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ELSEIF lines( _rows[] ) > 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        LOOP AT _rows INTO DATA(_row).
          row_record = it_saida[ _row-index ].

          IF row_record-remessa IS INITIAL OR row_record-remessa = icon_execute_object.
            MESSAGE 'Nenhuma remessa gerada para a linha selecionada!' TYPE 'S'.
            RETURN.
          ENDIF.

          IF ( row_record-fatura IS NOT INITIAL ) AND
             ( row_record-fatura(1) NE '@'      ) .
            MESSAGE 'Fatura já gerada! Operação não permitida!' TYPE 'S'.
            RETURN.
          ENDIF.

          IF ( row_record-transp IS NOT INITIAL ) AND
             ( row_record-transp(1) NE '@'      ) .
            MESSAGE 'Transporte já gerado! Operação não permitida!' TYPE 'S'.
            RETURN.
          ENDIF.

          PERFORM f_item_text_delivery USING row_record-remessa.

        ENDLOOP.

      WHEN 'BTN_AGRUPAR'.

        CHECK vg_cockpit = '03'. "Agrupamento de Fatura

        CALL METHOD cl_grid->get_selected_rows
          IMPORTING
            et_index_rows = _rows.

        IF _rows IS NOT INITIAL.
          TRY.
              LOOP AT _rows INTO _row.
                row_record = it_saida[ _row-index ].

                IF row_record-remessa IS INITIAL OR row_record-remessa = icon_execute_object.
                  CLEAR row_record-remessa.
                ENDIF.

                IF row_record-fatura IS INITIAL OR row_record-fatura = icon_execute_object.
                  CLEAR row_record-fatura.
                ELSE.
                  RAISE EXCEPTION TYPE cx_abap_util_exception.
                ENDIF.

                IF row_record-danfe IS INITIAL OR row_record-danfe = icon_execute_object.
                  CLEAR row_record-danfe.
                ENDIF.

                APPEND row_record TO rows_record.
              ENDLOOP.

            CATCH cx_abap_util_exception.
              MESSAGE |O romaneio { row_record-nr_romaneio } já possui fatura, não é possível agrupar.| TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.
          ENDTRY.

          it_saida[] = rows_record.

          LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
            IF <fs_saida>-remessa IS INITIAL AND NOT line_exists(
                         it_saida[ dt_movimento = <fs_saida>-dt_movimento
                                   matnr        = <fs_saida>-matnr
                                   kunnr        = <fs_saida>-kunnr
                                   operacao(4)  = <fs_saida>-operacao(4)
*                                   INCO1        = VINCO1
                                   remessa      = icon_execute_object
                                 ] ).

              <fs_saida>-remessa = icon_execute_object.
            ENDIF.

            IF NOT line_exists(
                         it_saida[ dt_movimento = <fs_saida>-dt_movimento
                                   matnr        = <fs_saida>-matnr
                                   kunnr        = <fs_saida>-kunnr
                                   operacao(4)  = <fs_saida>-operacao(4)
*                                   INCO1        = VINCO1
                                   fatura       = icon_execute_object
                                 ] ).

              <fs_saida>-fatura  = icon_execute_object.
            ENDIF.

            IF NOT line_exists(
                         it_saida[ dt_movimento = <fs_saida>-dt_movimento
                                   matnr        = <fs_saida>-matnr
                                   kunnr        = <fs_saida>-kunnr
                                   operacao(4)  = <fs_saida>-operacao(4)
*                                   INCO1        = VINCO1
                                   danfe        = icon_execute_object
                                 ] ).

              <fs_saida>-danfe   = icon_execute_object.
            ENDIF.
          ENDLOOP.

          CALL METHOD cl_grid->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        ELSE.
          MESSAGE 'É necessário selecionar os romaneios que deseja agrupar.' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.

*-CS2024000522-11.09.2024-JT-#151751-inicio
      WHEN 'ZLES0200'.
        CALL METHOD cl_grid->get_selected_rows
          IMPORTING
            et_index_rows = _rows.

        IF _rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ELSEIF lines( _rows[] ) > 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        PERFORM f_vincula_zles0200 USING _rows.
*-CS2024000522-11.09.2024-JT-#151751-fim

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
