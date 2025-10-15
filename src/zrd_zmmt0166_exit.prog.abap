*&---------------------------------------------------------------------*
*& Report  ZRD_zmmt0166_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0166_exit.

FORM f_exit_zmmt0166_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0166 TYPE zmmt0166.

  CLEAR: wl_zmmt0166.

  MOVE-CORRESPONDING wl_zmmt0166 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0166_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zmmt0166 TYPE zmmt0166.

  CLEAR: wl_zmmt0166.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0166.
*
*  CLEAR: p_error.
*
*  IF wl_zmmt0166-class_aval IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Classe Avaliação é um campo obrigatório!' TYPE 'S'.
*    EXIT.
*  ENDIF.
*
*  IF wl_zmmt0166-area_aval IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Área Avaliação é um campo obrigatório!' TYPE 'S'.
*    EXIT.
*  ENDIF.
*
*  IF wl_zmmt0166-lgort IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Depósito é um campo obrigatório!' TYPE 'S'.
*    EXIT.
*  ENDIF.

ENDFORM.

FORM f_exit_zmmt0166_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0166 TYPE zmmt0166.

  CLEAR: wl_zmmt0166.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0166.

  MOVE-CORRESPONDING wl_zmmt0166 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0166_0004 CHANGING p_saida TYPE any.

  DATA: wl_zmmt0166_out TYPE zmmt0166_out.

  CLEAR: wl_zmmt0166_out.

  MOVE-CORRESPONDING p_saida TO wl_zmmt0166_out.

  MOVE-CORRESPONDING wl_zmmt0166_out TO p_saida.

ENDFORM.
