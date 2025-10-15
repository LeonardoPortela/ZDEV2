*&---------------------------------------------------------------------*
*& Report  ZRD_ZPPT0024_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zppt0024_exit.

*&---------------------------------------------------------------------*
*&  "Set atributos iniciais ao incluir um novo registro
*&---------------------------------------------------------------------*
FORM f_exit_zppt0024_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zppt0024 TYPE zppt0024.

  CLEAR: wl_zppt0024.

  wl_zppt0024-us_registro_cri = sy-uname.
  wl_zppt0024-dt_registro     = sy-datum.
  wl_zppt0024-us_registro     = sy-uname.

  MOVE-CORRESPONDING wl_zppt0024 TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Validações antes da gravação do registro
*&---------------------------------------------------------------------*
FORM f_exit_zppt0024_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

* Declaração de Estruturas
  DATA: w_zppt0024 TYPE zppt0024.
  CLEAR: w_zppt0024.

  MOVE-CORRESPONDING p_registro_manter TO w_zppt0024.

* Busca Centros
  IF w_zppt0024-werks IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Centro é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF w_zppt0024-stlan IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Utilização é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Set atributos antes de gravar o registro
*&---------------------------------------------------------------------*
FORM f_exit_zppt0024_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zppt0024 TYPE zppt0024.

  CLEAR: wl_zppt0024.

  MOVE-CORRESPONDING p_registro_manter TO wl_zppt0024.

  wl_zppt0024-dt_registro     = sy-datum.
  wl_zppt0024-us_registro     = sy-uname.

  MOVE-CORRESPONDING wl_zppt0024 TO p_registro_manter.

ENDFORM.
