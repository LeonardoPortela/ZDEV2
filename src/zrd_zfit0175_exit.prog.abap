*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0175_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0175_exit.

FORM f_exit_zfit0175_0001 CHANGING p_registro_manter TYPE any.

  DATA: wa_zfit0175       TYPE zfit0175,
        vseq(4)           TYPE c,
        lva_cod_parametro TYPE zfit0175-cod_parametro.

  CLEAR:  wa_zfit0175.

*  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
*    EXPORTING
*      object           = 'ZSEQFI0175'
*    EXCEPTIONS
*      foreign_lock     = 1
*      object_not_found = 2
*      system_failure   = 3
*      OTHERS           = 4.

  SELECT MAX( cod_parametro ) INTO lva_cod_parametro
    FROM zfit0175.

  IF lva_cod_parametro IS INITIAL.
    vseq = '1'.
  ELSE.
    vseq = lva_cod_parametro + 1.
  ENDIF.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vseq
    IMPORTING
      output = vseq.

  wa_zfit0175-cod_parametro = vseq.
  wa_zfit0175-usnam_cad     = sy-uname.
  wa_zfit0175-dt_cad        = sy-datum.
  wa_zfit0175-hr_cad        = sy-uzeit.

  MOVE-CORRESPONDING wa_zfit0175 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0175_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zfit0175 TYPE zfit0175.
  CLEAR:  wa_zfit0175.

  MOVE-CORRESPONDING p_registro_manter TO wa_zfit0175.

  CLEAR: p_error.
  IF p_error IS INITIAL.
    IF wa_zfit0175-bukrs_pgto IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Empresa Pagamento é obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zfit0175-bukrs_pgto = wa_zfit0175-bukrs_rcbto .
      p_error = abap_true.
      MESSAGE 'Campo Empresa Pagamento não pode ser igual a empresa recebimento!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.


  IF p_error IS INITIAL.
    IF wa_zfit0175-bukrs_rcbto IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Empresa Recebimento é obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zfit0175-bsart IS INITIAL AND wa_zfit0175-matkl IS INITIAL AND wa_zfit0175-matnr IS INITIAL .
      p_error = abap_true.
      MESSAGE 'Obrigatório 1 ser preenchido (Tipo Pedido / Grupo Material / Código Material )  ' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zfit0175-matnr  IS NOT INITIAL.
      SELECT SINGLE matnr INTO wa_zfit0175-matnr
        FROM mara
        WHERE matnr = wa_zfit0175-matnr.
      IF sy-subrc NE 0.
        MESSAGE 'Material não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = abap_true.
        EXIT.
      ENDIF.
    ENDIF.

    IF wa_zfit0175-matkl IS NOT INITIAL.
      SELECT SINGLE matkl INTO wa_zfit0175-matkl
        FROM t023
        WHERE matkl = wa_zfit0175-matkl.
      IF sy-subrc NE 0.
        MESSAGE 'Grupo Material não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zfit0175-bukrs_pgto IS NOT INITIAL.
      SELECT SINGLE bukrs
          INTO wa_zfit0175-bukrs_pgto
             FROM  t001
        WHERE bukrs EQ wa_zfit0175-bukrs_pgto.
      IF sy-subrc NE 0.
        MESSAGE 'Empresa pgto não existe!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zfit0175-bukrs_rcbto IS NOT INITIAL.
      SELECT SINGLE bukrs
          INTO wa_zfit0175-bukrs_rcbto
             FROM  t001
        WHERE bukrs EQ wa_zfit0175-bukrs_rcbto.
      IF sy-subrc NE 0.
        MESSAGE 'Empresa rcbto não existe!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
*
*  IF p_error IS INITIAL.
*    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
*      EXPORTING
*        object           = 'ZSEQFI0175'
*      EXCEPTIONS
*        object_not_found = 1
*        OTHERS           = 2.
*  ENDIF.
ENDFORM.

FORM f_exit_zfit0175_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zfit0175 TYPE zfit0175.
  MOVE-CORRESPONDING p_registro_manter TO wa_zfit0175.
  wa_zfit0175-usnam_cad = sy-uname.
  wa_zfit0175-dt_cad = sy-datum.
  wa_zfit0175-hr_cad = sy-uzeit.
  MOVE-CORRESPONDING wa_zfit0175 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0175_0005 CHANGING p_registro_manter TYPE any.

  DATA: wa_zfit0175 TYPE zfit0175.
  MOVE-CORRESPONDING p_registro_manter TO wa_zfit0175.
  MOVE-CORRESPONDING wa_zfit0175 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0175_0008 CHANGING p_col_pos
                                   p_ref_tabname
                                   p_ref_fieldname
                                   p_tabname
                                   p_field
                                   p_scrtext_l
                                   p_outputlen
                                   p_edit
                                   p_sum
                                   p_emphasize
                                   p_just
                                   p_hotspot
                                   p_f4
                                   p_check.

  IF p_ref_tabname = 'ZFIT0175' AND
   p_field       = 'COD_PARAMETRO'.
    p_scrtext_l    = 'Cód. Parametro'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175' AND
     p_field       = 'BUKRS_PGTO'.
    p_scrtext_l    = 'Empresa Pagamento'.
    p_outputlen    = 25.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175' AND
     p_field       = 'BUKRS_RCBTO'.
    p_scrtext_l    = 'Empresa Recebimento'.
    p_outputlen    = 25.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175' AND
   p_field       = 'BSART'.
    p_scrtext_l    = 'Tipo Pedido'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175' AND
 p_field       = 'MATKL'.
    p_scrtext_l    = 'Grupo Material'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175' AND
    p_field       = 'MATNR'.
    p_scrtext_l    = 'Código Material'.
    p_outputlen    = 20.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175' AND
  p_field       = 'UMSKS'.
    p_scrtext_l    = 'Rz. Expecial'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175' AND
  p_field       = 'UMSKZ'.
    p_scrtext_l    = 'Partida Memo'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175_OUT' AND
 p_field       = 'COD_PARAMETRO'.
    p_scrtext_l    = 'Cód. Parametro'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175_OUT' AND
   p_field       = 'BUKRS_PGTO'.
    p_scrtext_l    = 'Empresa Pagamento'.
    p_outputlen    = 25.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175_OUT' AND
     p_field       = 'BUKRS_RCBTO'.
    p_scrtext_l    = 'Empresa Recebimento'.
    p_outputlen    = 25.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175_OUT' AND
   p_field       = 'BSART'.
    p_scrtext_l    = 'Tipo Pedido'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175_OUT' AND
 p_field       = 'MATKL'.
    p_scrtext_l    = 'Grupo Material'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175_OUT' AND
    p_field       = 'MATNR'.
    p_scrtext_l    = 'Código Material'.
    p_outputlen    = 20.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175_OUT' AND
  p_field       = 'UMSKS'.
    p_scrtext_l    = 'Rz. Expecial'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0175_OUT' AND
  p_field       = 'UMSKZ'.
    p_scrtext_l    = 'Partida Memo' .
    p_outputlen    = 15.
  ENDIF.
ENDFORM.
