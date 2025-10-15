FUNCTION z_caixa_texto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_TEXTO) TYPE  EEW_STRING OPTIONAL
*"     REFERENCE(IV_TITLE) TYPE  SYTITLE DEFAULT 'Justificativa'
*"     REFERENCE(IV_LIMIT) TYPE  INT4 DEFAULT 1000
*"     REFERENCE(IV_EDIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(EV_TEXTO) TYPE  EEW_STRING
*"----------------------------------------------------------------------

  CLEAR: gv_canc, ev_texto, gt_text.

  IF iv_texto IS NOT INITIAL.
    APPEND iv_texto TO gt_text.
  ENDIF.

  gv_title = iv_title.
  gv_limit = iv_limit.
  gv_edit = iv_edit.

  CALL SCREEN 9090 STARTING AT 15 10.

  CHECK gv_canc IS INITIAL.

  CALL METHOD go_editor->get_text_as_stream
    EXPORTING
      only_when_modified     = 0
    IMPORTING
      text                   = gt_text
    EXCEPTIONS
      error_dp               = 1
      error_cntl_call_method = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT gt_text ASSIGNING FIELD-SYMBOL(<fs_text>).

    ev_texto = ev_texto && <fs_text>.

  ENDLOOP.

  REPLACE ALL OCCURRENCES OF REGEX `[\t\v\n\r]` IN ev_texto WITH space.

  go_editor->free( ).

  FREE go_editor.

ENDFUNCTION.
