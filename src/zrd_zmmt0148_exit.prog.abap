*&---------------------------------------------------------------------*
*& Report  ZRD_zmmt0148_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0148_exit.

FORM f_exit_zmmt0148_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zmmt0148 TYPE zmmt0148.
  CLEAR:  wa_zmmt0148.

  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0148.

*  CLEAR: p_error.
*  IF p_error IS INITIAL.
*    IF wa_zmmt0148-matnr IS INITIAL.
*      p_error = abap_true.
*      MESSAGE 'Campo Material obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*  IF p_error IS INITIAL.
*    IF wa_zmmt0148-cod_imp IS INITIAL.
*      p_error = abap_true.
*      MESSAGE 'Campo Código de Imposto obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*  IF p_error IS INITIAL.
*    IF wa_zmmt0148-werks IS INITIAL.
*      p_error = abap_true.
*      MESSAGE 'Campo Centro obrigatório ' TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*  IF p_error IS INITIAL.
*    IF wa_zmmt0148-cod_pgto IS INITIAL.
*      p_error = abap_true.
*      MESSAGE 'Campo Condição de Pagamento Obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*  IF p_error IS INITIAL.
*    IF wa_zmmt0148-matnr  IS NOT INITIAL.
*      SELECT SINGLE matnr INTO wa_zmmt0148-matnr
*        FROM makt
*        WHERE matnr = wa_zmmt0148-matnr.
*      IF sy-subrc NE 0.
*        MESSAGE 'Material não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
*        p_error = abap_true.
*        EXIT.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  IF p_error IS INITIAL.
*    IF wa_zmmt0148-werks IS NOT INITIAL.
*      SELECT SINGLE werks
*          INTO wa_zmmt0148-werks
*             FROM t001w
*        WHERE werks EQ wa_zmmt0148-werks.
*      IF sy-subrc NE 0.
*        MESSAGE 'Centro não existe!' TYPE 'S' DISPLAY LIKE 'E'.
*        p_error = abap_true.
*        EXIT.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  IF p_error IS INITIAL.
*    IF wa_zmmt0148-cod_pgto IS NOT INITIAL.
*      SELECT SINGLE zterm INTO wa_zmmt0148-cod_pgto
*       FROM t052u
*    WHERE zterm = wa_zmmt0148-cod_pgto
*        AND spras =  sy-langu.
*      IF sy-subrc NE 0.
*        p_error = abap_true.
*        MESSAGE 'Condição de Pagamento não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  IF p_error IS INITIAL.
*    IF wa_zmmt0148-cod_imp IS NOT INITIAL.
*      SELECT SINGLE mwskz INTO wa_zmmt0148-cod_imp
*       FROM t007a
*      WHERE mwskz = wa_zmmt0148-cod_imp
*            AND kalsm = 'TAXBRA'.
*      IF sy-subrc NE 0.
*        p_error = abap_true.
*        MESSAGE 'Código de Imposto não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.
*    ENDIF.
*  ENDIF.

ENDFORM.

FORM f_exit_zmmt0148_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zmmt0148 TYPE zmmt0148.
  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0148.
*  wa_zmmt0148-usnam_cad = sy-uname.
*  wa_zmmt0148-dt_cad = sy-datum.
*  wa_zmmt0148-hr_cad = sy-uzeit.
  MOVE-CORRESPONDING wa_zmmt0148 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0148_0005 CHANGING p_registro_manter TYPE any.

  DATA: wa_zmmt0148 TYPE zmmt0148.
  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0148.
  MOVE-CORRESPONDING wa_zmmt0148 TO p_registro_manter.

ENDFORM.
