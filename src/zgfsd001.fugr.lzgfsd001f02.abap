*----------------------------------------------------------------------*
***INCLUDE LZGFSD001F02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_9000
*&---------------------------------------------------------------------*
FORM f_refresh_9000 .

  CLEAR: gv_ucomm_9000, gt_alv_9000, gv_coment, gv_motivo, gv_venc_9000.

*  FREE: go_cc_alv_9000, go_alv_9000,go_cc_coment_9000,
*        go_coment_9000,go_cc_motivo_9000,go_motivo_9000.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_9000
*&---------------------------------------------------------------------*
FORM f_init_9000_alv .

  DATA lt_fieldcat TYPE lvc_t_fcat.

  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.

  DATA lo_handler TYPE REF TO lcl_event_handler.

  IF go_cc_alv_9000 IS INITIAL.

    CREATE OBJECT go_cc_alv_9000
      EXPORTING
        container_name              = 'CC_001'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc NE 0.
      MESSAGE i000(d2) WITH 'The custom control could not be created'.
      RETURN.
    ENDIF.

  ENDIF.

  lw_layout-sel_mode = 'A'.

  lw_layout-col_opt = 'X'.
  lw_layout-cwidth_opt = 'X'.

  lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.
  lw_variant-report       = sy-repid.
  lw_variant-username     = sy-uname.
  lw_layout-no_toolbar = 'X'.

  IF go_alv_9000 IS INITIAL.

    CREATE OBJECT go_alv_9000
      EXPORTING
        i_parent = go_cc_alv_9000.

    PERFORM f_monta_fieldcat USING 'ZSDE013' CHANGING lt_fieldcat.

    CALL METHOD go_alv_9000->set_table_for_first_display
      EXPORTING
        is_layout       = lw_layout
        is_variant      = lw_variant
        i_save          = 'A'
      CHANGING
        it_outtab       = gt_alv_9000
        it_fieldcatalog = lt_fieldcat.

    CREATE OBJECT lo_handler.

    SET HANDLER lo_handler->handle_double_click FOR go_alv_9000.

  ELSE.

    CALL METHOD go_alv_9000->set_frontend_layout
      EXPORTING
        is_layout = lw_layout.

    CALL METHOD go_alv_9000->refresh_table_display( ).

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT_4000_1
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat USING p_struct TYPE tabname
                   CHANGING p_field_cat TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_struct
    CHANGING
      ct_fieldcat            = p_field_cat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_9000_COMENT
*&---------------------------------------------------------------------*
FORM f_init_9000_coment .

  IF go_cc_coment_9000 IS NOT BOUND.

    CREATE OBJECT go_cc_coment_9000
      EXPORTING
        container_name = 'CC_002'
        repid          = sy-repid
        dynnr          = sy-dynnr.

  ENDIF.

  IF go_coment_9000 IS NOT BOUND.

    CREATE OBJECT go_coment_9000
      EXPORTING
        wordwrap_mode     = 1
        wordwrap_position = 254
        parent            = go_cc_coment_9000.

    "go_text_2000->set_enable( abap_false ).
    go_coment_9000->set_readonly_mode( 1 ).

    go_coment_9000->set_toolbar_mode( '0' ).
    "go_text_2000->set_wordwrap

    go_coment_9000->set_statusbar_mode( 0 ).

*    CALL METHOD go_coment_9000->set_textstream
*      EXPORTING
*        text                   = gv_coment
*      EXCEPTIONS
*        error_cntl_call_method = 1
*        not_supported_by_gui   = 2
*        OTHERS                 = 3.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT_9000_MOTIVO
*&---------------------------------------------------------------------*
FORM f_init_9000_motivo .

  IF go_cc_motivo_9000 IS NOT BOUND.

    CREATE OBJECT go_cc_motivo_9000
      EXPORTING
        container_name = 'CC_003'
        repid          = sy-repid
        dynnr          = sy-dynnr.

  ENDIF.

  IF go_motivo_9000 IS NOT BOUND.

    CREATE OBJECT go_motivo_9000
      EXPORTING
        wordwrap_mode     = 1
        wordwrap_position = 254
        parent            = go_cc_motivo_9000.

    "go_text_2000->set_enable( abap_false ).

    IF gv_edit_9000 IS INITIAL.
      go_motivo_9000->set_readonly_mode( 1 ).
    ENDIF.

    go_motivo_9000->set_toolbar_mode( '0' ).
    go_motivo_9000->set_statusbar_mode( 0 ).

    CALL METHOD go_motivo_9000->set_textstream
      EXPORTING
        text                   = gv_motivo
      EXCEPTIONS
        error_cntl_call_method = 1
        not_supported_by_gui   = 2
        OTHERS                 = 3.

  ELSE.

    CALL METHOD go_motivo_9000->set_textstream
      EXPORTING
        text                   = gv_motivo
      EXCEPTIONS
        error_cntl_call_method = 1
        not_supported_by_gui   = 2
        OTHERS                 = 3.


  ENDIF.

ENDFORM.
FORM f_on_click USING e_row TYPE lvc_s_row
                      e_column TYPE lvc_s_col
                      es_row_no TYPE lvc_s_roid.

  BREAK-POINT.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_EVENT
*&---------------------------------------------------------------------*
FORM f_check_event .

  DATA lv_str_in TYPE string.
  DATA lv_string TYPE string.

  DATA lv_erro.

  PERFORM f_check_selec CHANGING lv_erro.

  IF lv_erro IS INITIAL.

    READ TABLE gt_alv_9000 ASSIGNING FIELD-SYMBOL(<fs_alv>)
      WITH KEY selec = 'X'.

    IF sy-subrc EQ 0.
      lv_str_in = <fs_alv>-coment.
      PERFORM f_remove_unicode USING lv_str_in CHANGING lv_string.
    ELSE.
      lv_string = space.
    ENDIF.

  ELSE.

    lv_string = space.

  ENDIF.

  CHECK go_coment_9000 IS BOUND.

  CALL METHOD go_coment_9000->set_textstream
    EXPORTING
      text                   = lv_string
    EXCEPTIONS
      error_cntl_call_method = 1
      not_supported_by_gui   = 2
      OTHERS                 = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_SELEC
*&---------------------------------------------------------------------*
FORM f_check_selec CHANGING p_erro TYPE c.

  DATA lv_count TYPE i.

  LOOP AT gt_alv_9000 ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.
    ADD 1 TO lv_count.
  ENDLOOP.

  IF lv_count > 1.

    LOOP AT gt_alv_9000 ASSIGNING <fs_alv>.
      <fs_alv>-selec = space.
    ENDLOOP.

    p_erro = 'X'.

    MESSAGE 'Selecionar apenas uma linha' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_MOTIVO_9000
*&---------------------------------------------------------------------*
FORM f_get_motivo_9000 .

  CHECK go_motivo_9000 IS BOUND.

  CLEAR gv_motivo.

  CALL METHOD go_motivo_9000->get_textstream
    EXPORTING
      only_when_modified     = 0
    IMPORTING
      text                   = gv_motivo
*     is_modified            =
    EXCEPTIONS
      error_cntl_call_method = 1
      not_supported_by_gui   = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA lt_text TYPE TABLE OF char300.

  CALL METHOD go_motivo_9000->get_text_as_stream
    EXPORTING
      only_when_modified     = cl_gui_textedit=>true
    IMPORTING
      text                   = lt_text
    EXCEPTIONS
      error_dp               = 1
      error_cntl_call_method = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "BREAK-POINT.

*  CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
*    EXPORTING
*      intext            = gv_motivo
*    IMPORTING
*      outtext           = gv_motivo
*    EXCEPTIONS
*      invalid_codepage  = 1
*      codepage_mismatch = 2
*      internal_error    = 3
*      cannot_convert    = 4
*      fields_not_type_c = 5
*      OTHERS            = 6.
*
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*  REPLACE ALL OCCURRENCES OF '.' IN gv_motivo WITH ` `.

  CONDENSE gv_motivo.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_SELEC_9000
*&---------------------------------------------------------------------*
FORM f_check_selec_9000 CHANGING p_erro TYPE c.

  DATA lv_boleta_perto.
  DATA lv_ret.
  CLEAR p_erro.

  READ TABLE gt_alv_9000 ASSIGNING FIELD-SYMBOL(<fs_9000>)
    WITH KEY selec = 'X'.

  CHECK sy-subrc EQ 0.

  IF gv_motivo IS INITIAL.

    MESSAGE 'Favor preencher motivo' TYPE 'S' DISPLAY LIKE 'E'.

    p_erro = 'X'.

    EXIT.

  ENDIF.

  IF gv_venc_9000 < <fs_9000>-data_venc.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = 'Boleta selecionada está com a data de vencimento diferente do simulador!'
        text_button_1         = 'Continuar'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'Cancelar'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = lv_ret
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lv_ret <> '1'.

      p_erro = 'X'.

      EXIT.

    ENDIF.

    LOOP AT gt_alv_9000 ASSIGNING FIELD-SYMBOL(<fs_check>) WHERE data_venc < <fs_9000>-data_venc.

      IF <fs_check>-data_venc >= gv_venc_9000.
        lv_boleta_perto = 'X'.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF lv_boleta_perto = 'X'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = 'Existe boleta com vencimento mais próximo ao Vencimento do Simulador!'
          text_button_1         = 'Continuar'
          icon_button_1         = 'ICON_OKAY'
          text_button_2         = 'Cancelar'
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ''
          popup_type            = 'ICON_MESSAGE_WARNING'
        IMPORTING
          answer                = lv_ret
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      IF lv_ret <> '1'.

        p_erro = 'X'.

        EXIT.

      ENDIF.



    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SIGAM
*&---------------------------------------------------------------------*
FORM f_data_sigam USING p_date TYPE datum
               CHANGING p_date_out TYPE string.

  DATA(lv_data) = p_date.

  IF lv_data IS INITIAL.
    lv_data = sy-datum.
  ENDIF.

  p_date_out = lv_data(4) && '-' && lv_data+4(2) && '-' && lv_data+6(2).

ENDFORM.
