*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0185_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0185_exit.

FORM f_exit_ZFIT0185_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_ZFIT0185 TYPE ZFIT0185.


  MOVE-CORRESPONDING wl_ZFIT0185 TO p_registro_manter.

ENDFORM.


FORM f_exit_ZFIT0185_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_ZFIT0185 TYPE ZFIT0185.
  DATA: wl_ZFIT0185_out TYPE ZFIT0185_out.
  CLEAR: wl_ZFIT0185, wl_ZFIT0185_out.
  MOVE-CORRESPONDING p_registro_manter TO wl_ZFIT0185.

  IF wl_ZFIT0185-taxa IS NOT INITIAL and  wl_ZFIT0185-taxa1 is NOT  INITIAL.
        MESSAGE 'Informar apena um valor de cotação' TYPE 'E'.
    EXIT.
  ENDIF.

  if wl_ZFIT0185-taxa IS NOT INITIAL.
   wl_ZFIT0185-taxa = wl_ZFIT0185-taxa * -1.
   ELSEIF wl_ZFIT0185-taxa1 IS NOT INITIAL.
     wl_ZFIT0185-taxa = wl_ZFIT0185_out-taxa1.
     wl_ZFIT0185_out-taxa1 = 0.
     ENDIF.



  MOVE-CORRESPONDING wl_ZFIT0185 TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_ZFIT0185_0003 CHANGING p_saida TYPE any.

*  DATA: wl_ZFIT0185 TYPE ZFIT0185.
*
*  CLEAR: wl_ZFIT0185.
*
*  MOVE-CORRESPONDING p_saida TO wl_ZFIT0185.
*
*
*
*  MOVE-CORRESPONDING wl_ZFIT0185 TO p_saida.


ENDFORM.

FORM f_exit_ZFIT0185_0004 CHANGING p_saida TYPE any.

  DATA: wl_ZFIT0185_out TYPE ZFIT0185_out.
  DATA: wl_ZFIT0185 TYPE ZFIT0185.
  CLEAR: wl_ZFIT0185_out.

  MOVE-CORRESPONDING p_saida TO wl_ZFIT0185_out.
  CLEAR p_saida.

  IF wl_ZFIT0185_out-empresa IS NOT INITIAL.
    SELECT SINGLE  butxt FROM t001 INTO wl_ZFIT0185_out-desc_emp WHERE bukrs = wl_ZFIT0185_out-empresa.
  ENDIF.

  if wl_ZFIT0185_out-taxa > 0.
    wl_ZFIT0185_out-taxa1 = wl_ZFIT0185_out-taxa.
    wl_ZFIT0185_out-taxa = 0.

    endif.


  MOVE-CORRESPONDING wl_ZFIT0185_out TO p_saida.
ENDFORM.

FORM f_exit_ZFIT0185_0005 CHANGING p_registro_manter TYPE any.
*  DATA: wl_ZFIT0185_out TYPE ZFIT0185_out.
*  DATA wl_ZFIT0185  TYPE ZFIT0185.
*  DATA: wl_t001 TYPE  t001,
*        wl_lfa1 TYPE  lfa1.
*
*  CLEAR: wl_ZFIT0185_out, wl_t001, wl_lfa1.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_ZFIT0185_out.
*
*  IF wl_ZFIT0185_out-empresa IS NOT INITIAL .
*    SELECT SINGLE  butxt FROM t001 INTO wl_ZFIT0185_out-desc_emp WHERE bukrs = wl_ZFIT0185_out-empresa.
*
*  ENDIF.
*
*
*  MOVE-CORRESPONDING wl_ZFIT0185_out TO p_registro_manter.

ENDFORM.
