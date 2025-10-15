*&---------------------------------------------------------------------*
*& Report  ZRD_ZGLT0102_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0209_exit.

FORM f_exit_zfit0209_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0209 TYPE zfit0209.

  CLEAR: wl_zfit0209.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0209.

  IF wl_zfit0209-usuario IS INITIAL.
    wl_zfit0209-data      = sy-datum.
    wl_zfit0209-hora      = sy-uzeit.
    wl_zfit0209-usuario   = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zfit0209 TO p_registro_manter.

ENDFORM.


FORM f_exit_zfit0209_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.


  DATA: wl_zfit0209        TYPE zfit0209.


  CLEAR: wl_zfit0209.
  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0209.

  IF wl_zfit0209-tipo_valor EQ '01'.
    wl_zfit0209-desc_tipo_valor = 'Valor Principal'.
  ELSEIF wl_zfit0209-tipo_valor EQ '02'.
    wl_zfit0209-desc_tipo_valor = 'Valor 13º'.
  ENDIF.

  IF wl_zfit0209-lgart IS NOT INITIAL.
    SELECT SINGLE lgtxt
           FROM t512t
           INTO wl_zfit0209-desc_lgart
           WHERE sprsl EQ 'P'
           AND   molga EQ '37'
           AND   lgart EQ wl_zfit0209-lgart.
  ENDIF.

  MOVE-CORRESPONDING wl_zfit0209 TO p_registro_manter.

  CLEAR: p_error.

ENDFORM.


FORM f_exit_zfit0209_0003 CHANGING p_saida TYPE any.

*  DATA: wl_zfit0209 TYPE zfit0209.
*
*  CLEAR: wl_zfit0209.
*
*  MOVE-CORRESPONDING p_saida TO wl_zfit0209.
*
*  MOVE-CORRESPONDING wl_zfit0209 TO p_saida.


ENDFORM.

FORM f_exit_zfit0209_0004 CHANGING p_saida TYPE any.


  DATA: wl_zfit0209_out    TYPE zfit0209_out.


  CLEAR: wl_zfit0209_out.
  MOVE-CORRESPONDING p_saida TO wl_zfit0209_out.

  IF wl_zfit0209_out-tipo_valor EQ '01'.
    wl_zfit0209_out-desc_tipo_valor = 'Valor Principal'.
  ELSEIF wl_zfit0209_out-tipo_valor EQ '02'.
    wl_zfit0209_out-desc_tipo_valor = 'Valor 13º'.
  ENDIF.

  IF wl_zfit0209_out-lgart IS NOT INITIAL.
    SELECT SINGLE lgtxt
           FROM t512t
           INTO wl_zfit0209_out-desc_lgart
           WHERE sprsl EQ 'P'
           AND   molga EQ '37'
           AND   lgart EQ wl_zfit0209_out-lgart.
  ENDIF.

  MOVE-CORRESPONDING wl_zfit0209_out TO p_saida.

ENDFORM.

FORM f_exit_zfit0209_0005 CHANGING p_registro_manter TYPE any.



  DATA: wl_zfit0209_out    TYPE zfit0209_out.


  CLEAR: wl_zfit0209_out.
  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0209_out.

  IF wl_zfit0209_out-tipo_valor EQ '01'.
    wl_zfit0209_out-desc_tipo_valor = 'Valor Principal'.
  ELSEIF wl_zfit0209_out-tipo_valor EQ '02'.
    wl_zfit0209_out-desc_tipo_valor = 'Valor 13º'.
  ENDIF.

  IF wl_zfit0209_out-lgart IS NOT INITIAL.
    SELECT SINGLE lgtxt
           FROM t512t
           INTO wl_zfit0209_out-desc_lgart
           WHERE sprsl EQ 'P'
           AND   molga EQ '37'
           AND   lgart EQ wl_zfit0209_out-lgart.
  ENDIF.

  MOVE-CORRESPONDING wl_zfit0209_out TO p_registro_manter.



ENDFORM.

FORM  f_exit_zfit0209_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zfit0209'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
