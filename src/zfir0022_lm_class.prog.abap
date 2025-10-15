*&-----------------------------------*
*& Include          ZFIR0022_LM_CLASS
*&-----------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i08.
  ENDMETHOD.
  METHOD on_before_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i09.
  ENDMETHOD.
  METHOD on_after_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i10.
  ENDMETHOD.                 " on_link_click2

  METHOD on_hotspot_click.

    CASE column.
      WHEN 'VALID'.
        PERFORM validacoes.
        gr_table->refresh( ).
        READ TABLE it_lanc_mass ASSIGNING FIELD-SYMBOL(<call_popup>) INDEX row.
        CHECK <call_popup>-valid = icon_led_red.
        IF <call_popup> IS NOT INITIAL.
          MESSAGE <call_popup>-error TYPE 'I'.
        ENDIF.
      WHEN  'CHECK_CALC'.

        DATA: ans TYPE c. "answer.
        CLEAR: ans.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = ' '
*           DIAGNOSE_OBJECT       = ' '
            text_question         = 'Deseja Limpar o valor do Check Calculo ?'
            text_button_1         = 'Sim'
            "icon_button_1  = 'ICON_OKAY'
            text_button_2         = 'Não'
            "icon_button_2  = 'ICON_OKAY'
            default_button        = '2'
            display_cancel_button = ''
          IMPORTING
            answer                = ans
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF ans EQ '1'.
          READ TABLE it_lanc_mass ASSIGNING FIELD-SYMBOL(<call_popup2>) INDEX row.
          <call_popup2>-check_calc = abap_false.
          <call_popup2>-vlr_multa_rbdo = abap_false.
          <call_popup2>-vlr_juros_rbdo = abap_false.
*          PERFORM calcular.
*          on_data_changed_finished(
*            e_modified = 'X'
*          ).
          gr_table->refresh( ).
        ELSE.
          EXIT.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD make_toolbar.
    DATA mt_toolbar TYPE stb_button.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   " separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
      " 3 DESABILITA E 0 HABILITA
      IF <fs_tollbar>-function = '&LOCAL&COPY_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&CREATE_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&APPEND'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&INSERT_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&DELETE_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&REFRESH'.
        <fs_tollbar>-butn_type = '3'.
      ENDIF.
*      IF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
*        <fs_tollbar>-function = 'INSERT_ROW'.
*      ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
*        <fs_tollbar>-function = 'DELETE_ROW'.
*      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_data_changed_finished.
    CHECK e_modified = 'X'.


    LOOP AT et_good_cells[] ASSIGNING FIELD-SYMBOL(<_check_calc>).
      READ TABLE it_alter_values ASSIGNING FIELD-SYMBOL(<_vlr_old>) WITH KEY row_id = <_check_calc>-row_id.
      IF sy-subrc = 0 .
        READ TABLE it_lanc_mass ASSIGNING FIELD-SYMBOL(<_vlr_new>) INDEX <_check_calc>-row_id.
        IF sy-subrc = 0 .
          IF <_vlr_old>-mont_rbdo IS NOT INITIAL AND <_vlr_old>-mont_rbdo <> <_vlr_new>-mont_rbdo.
            <_vlr_new>-check_calc = abap_false.
          ENDIF.
          IF <_vlr_old>-data_pgto IS NOT INITIAL AND <_vlr_old>-data_pgto <> <_vlr_new>-data_pgto.
            <_vlr_new>-check_calc = abap_false.
          ENDIF.
          IF <_vlr_old>-vlr_juros_rbdo <> <_vlr_new>-vlr_juros_rbdo OR <_vlr_old>-vlr_multa_rbdo <> <_vlr_new>-vlr_multa_rbdo AND <_vlr_new>-check_calc = 'C'.
            <_vlr_new>-check_calc = 'M'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.


    FREE : it_alter_values.
    MOVE-CORRESPONDING it_lanc_mass TO it_alter_values.

    LOOP AT it_alter_values ASSIGNING FIELD-SYMBOL(<_get>).
      <_get>-row_id = sy-tabix.
    ENDLOOP.
    LOOP AT et_good_cells[] ASSIGNING FIELD-SYMBOL(<_prep>).
      READ TABLE it_alter_values ASSIGNING FIELD-SYMBOL(<_alter>) WITH KEY row_id = <_prep>-row_id.

      CONDENSE <_prep>-value NO-GAPS.
      IF ( <_prep>-fieldname = 'VLR_MULTA_RBDO' AND <_prep>-value <> '' ) OR
         ( <_prep>-fieldname = 'VLR_JUROS_RBDO' AND <_prep>-value <> '' ) OR
         ( <_prep>-fieldname = 'MONT_RBDO'      AND <_prep>-value <> '' ).
        REPLACE ALL OCCURRENCES OF ',' IN <_prep>-value WITH '.'.
        CLEAR: _new_vlr.
        _new_vlr = <_prep>-value.
      ENDIF.

      CASE <_prep>-fieldname.
        WHEN 'VLR_MULTA_RBDO'.
          <_alter>-vlr_multa_rbdo = _new_vlr.
        WHEN 'VLR_JUROS_RBDO'.
          <_alter>-vlr_juros_rbdo = _new_vlr.
        WHEN 'MONT_RBDO'.
          <_alter>-mont_rbdo = _new_vlr.
        WHEN 'DATA_PGTO'.
          <_alter>-data_pgto = <_prep>-value.
        WHEN 'REC_VLR_TOTAL'.
          IF <_prep>-fieldname = 'REC_VLR_TOTAL' AND ( <_prep>-value = 'X' OR <_prep>-value = 'x' ).
            <_alter>-rec_vlr_total = 'X'.
          ELSE.
            CLEAR: <_alter>-rec_vlr_total.
          ENDIF.
        WHEN 'FORMA_PAG'.
          IF <_prep>-fieldname = 'FORMA_PAG'.
            <_alter>-forma_pag = <_prep>-value.
          ENDIF.
        WHEN 'ZTERM'.
          IF <_prep>-fieldname = 'ZTERM'.
            <_alter>-zterm = <_prep>-value.
          ENDIF.
      ENDCASE.

    ENDLOOP.

    LOOP AT it_alter_values ASSIGNING FIELD-SYMBOL(<_view>).

      READ TABLE it_lanc_mass ASSIGNING <_valida> INDEX <_view>-row_id.

      IF <_valida>-rec_vlr_total = abap_false
      AND <_valida>-data_pgto = '00000000'
      AND <_valida>-mont_rbdo IS INITIAL
      AND <_valida>-num_comp_adiant IS INITIAL
      AND <_valida>-vlr_multa_rbdo IS INITIAL
      AND <_valida>-vlr_juros_rbdo IS INITIAL
      AND <_valida>-forma_pag IS INITIAL
      AND <_valida>-zterm IS INITIAL
      AND <_valida>-observacao IS INITIAL
        AND <_valida>-check_calc IS INITIAL.
        "<_valida>-valid = icon_led_yellow.

      ELSE.

        IF <_valida>-data_pgto <> '00000000'
        AND <_valida>-mont_rbdo > 0
        AND <_valida>-forma_pag <> ''
        AND <_valida>-zterm <> ''
          AND <_valida>-check_calc IS NOT INITIAL.
          <_valida>-valid = icon_led_green.
        ELSE.
          <_valida>-valid = icon_led_red.
        ENDIF.

        IF <_valida>-valid = icon_led_green.

*          IF ( <_view>-vlr_multa_rbdo >= 0 OR <_view>-vlr_juros_rbdo >= 0 ).
*            <_valida>-vlr_multa_rbdo  = <_view>-vlr_multa_rbdo.
*            <_valida>-vlr_juros_rbdo  = <_view>-vlr_juros_rbdo.
*            <_valida>-mont_rbdo       = <_view>-mont_rbdo.
*          ENDIF.

        ENDIF.
      ENDIF.
      <_valida>-mont_moeda      = ( <_valida>-mont_rbdo - <_valida>-vlr_multa_rbdo - <_valida>-vlr_juros_rbdo ).
    ENDLOOP.

    gr_table->refresh( ).
  ENDMETHOD.
ENDCLASS.
