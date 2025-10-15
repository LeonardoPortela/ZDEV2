*&---------------------------------------------------------------------*
*& Report  ZRD_zmmt0198_EXIT
*&
*&---------------------------------------------------------------------*
*& * #169622 - CS2025000249 - M - ZMM0015 - Avaliar ajustes Expansão Materiais
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0198_exit .


FORM f_exit_zmmt0198_0001 CHANGING p_registro_manter TYPE any.


  DATA: w_zmmt0198 TYPE zmmt0198.

  CLEAR: w_zmmt0198.

  MOVE-CORRESPONDING p_registro_manter  TO w_zmmt0198.
  w_zmmt0198-user_create    =  sy-uname.
  w_zmmt0198-date_create    =  sy-datum.
  w_zmmt0198-time_create    =  sy-uzeit.


  MOVE-CORRESPONDING w_zmmt0198 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0198_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zmmt0198 TYPE zmmt0198.

  MOVE-CORRESPONDING p_registro_manter  TO w_zmmt0198.

  IF w_zmmt0198-mtart IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar tipo material' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


ENDFORM.
