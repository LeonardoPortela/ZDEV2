*&---------------------------------------------------------------------*
*& Report ZSDR0199
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0199.

" Para adicionar uma transação nova:
" 1 - Criar o botao PARAMETERS - exemplo comentado abaixo linha 24
" 2 - Colocar o texto do parametro com a transação entre parenteses
" 3 - Ativar

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.

  PARAMETERS p_01 RADIOBUTTON GROUP g01 DEFAULT 'X'.
  PARAMETERS p_02 RADIOBUTTON GROUP g01.
  PARAMETERS p_03 RADIOBUTTON GROUP g01.
  PARAMETERS p_04 RADIOBUTTON GROUP g01.
  PARAMETERS p_05 RADIOBUTTON GROUP g01.
  PARAMETERS p_06 RADIOBUTTON GROUP g01.
  PARAMETERS p_07 RADIOBUTTON GROUP g01.
  PARAMETERS p_08 RADIOBUTTON GROUP g01.
  PARAMETERS p_09 RADIOBUTTON GROUP g01.
  PARAMETERS p_10 RADIOBUTTON GROUP g01."192298 - 02-10-2025 - SMC
  "PARAMETERS p_11 RADIOBUTTON GROUP g01. EXEMPLO BOTAO NOVO p_11
SELECTION-SCREEN END OF BLOCK b01.

START-OF-SELECTION.
  PERFORM f_executar.

*&---------------------------------------------------------------------*
*& Form f_get_transaction
*&---------------------------------------------------------------------*
FORM f_get_transaction USING uv_param TYPE c.

  DATA lv_transaction TYPE sytcode.
  DATA lt_all_text TYPE STANDARD TABLE OF textpool WITH DEFAULT KEY.

  READ TEXTPOOL sy-repid INTO lt_all_text LANGUAGE sy-langu.

  READ TABLE lt_all_text ASSIGNING FIELD-SYMBOL(<fs_text>)
    WITH KEY key = uv_param.

  CHECK sy-subrc EQ 0.

  PERFORM f_get_text USING <fs_text>-entry CHANGING lv_transaction.

  CHECK lv_transaction IS NOT INITIAL.

  SELECT COUNT(*) FROM tstc
    WHERE tcode = lv_transaction.

  IF sy-dbcnt = 0.
    MESSAGE s074(eu) WITH lv_transaction DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF lv_transaction = 'ZSDT0095'.
    PERFORM f_call_zsdt0095.
  ELSE.
    CALL TRANSACTION lv_transaction.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_text
*&---------------------------------------------------------------------*
FORM f_get_text USING uv_entry TYPE c
               CHANGING cv_transac TYPE sytcode.

  CHECK uv_entry IS NOT INITIAL.

  FIND FIRST OCCURRENCE OF '(' IN uv_entry RESULTS DATA(ls_result).

  CHECK ls_result-offset > 0.

  cv_transac = uv_entry+ls_result-offset.

  REPLACE '(' IN cv_transac WITH space.
  REPLACE ')' IN cv_transac WITH space.

  CONDENSE cv_transac NO-GAPS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_executar
*&---------------------------------------------------------------------*
FORM f_executar .

  DATA lv_val TYPE c LENGTH 2.
  DATA lv_field TYPE c LENGTH 20.

  DO.

    UNPACK sy-index TO lv_val.

    lv_field = 'P_' && lv_val.

    ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_val>).

    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    IF <fs_val> = abap_true.
      PERFORM f_get_transaction USING lv_field.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_call_zsdt0095
*&---------------------------------------------------------------------*
FORM f_call_zsdt0095 .

  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      action    = 'S'
      view_name = 'ZSDT0101'.


ENDFORM.
