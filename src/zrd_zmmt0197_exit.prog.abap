*&---------------------------------------------------------------------*
*& Report  ZRD_zmmt0197_EXIT
*&
*&---------------------------------------------------------------------*
*& * #169622 - CS2025000249 - M - ZMM0015 - Avaliar ajustes Expansão Materiais
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0197_exit.

FORM f_exit_zmmt0197_0001 CHANGING p_registro_manter TYPE any.


  DATA: w_zmmt0197 TYPE zmmt0197.

  CLEAR: w_zmmt0197.

  MOVE-CORRESPONDING p_registro_manter  TO w_zmmt0197.
  w_zmmt0197-user_create    =  sy-uname.
  w_zmmt0197-date_create    =  sy-datum.
  w_zmmt0197-time_create    =  sy-uzeit.


  MOVE-CORRESPONDING w_zmmt0197 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0197_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zmmt0197 TYPE zmmt0197.

  MOVE-CORRESPONDING p_registro_manter  TO w_zmmt0197.

  IF w_zmmt0197-werks IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar centro' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


ENDFORM.
