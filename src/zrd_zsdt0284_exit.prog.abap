*&---------------------------------------------------------------------*
*& Report  ZRD_zsdt0284_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0284_exit.

FORM f_exit_zsdt0284_0001 CHANGING p_registro_manter TYPE any.
  DATA: wl_zsdt0284 TYPE zsdt0284.

  CLEAR: wl_zsdt0284.
  wl_zsdt0284-us_registro = sy-uname.
  wl_zsdt0284-dt_registro = sy-datum.
  wl_zsdt0284-hr_registro = sy-uzeit.

  MOVE-CORRESPONDING wl_zsdt0284 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0284_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0284 TYPE zsdt0284,
        wl_vbak     TYPE  vbak,
        wl_vbpa     TYPE  vbpa,
        wl_lfa1     TYPE  lfa1,
        wl_kna1     TYPE kna1.




  CLEAR: wl_zsdt0284, p_error.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0284.

  IF wl_zsdt0284-vbeln IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha a ordem de venda' TYPE 'E'.
    EXIT.
  ENDIF.

*  IF wl_zsdt0284-bukrs IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Preencha a empresa' TYPE 'E'.
*    EXIT.
*  ENDIF.

*  IF wl_zsdt0284-auart IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Preencha o tipo de ordem' TYPE 'E'.
*    EXIT.
*  ENDIF.

  "Valida se existe a OV informanda.
  SELECT SINGLE * FROM vbak INTO wl_vbak WHERE vbeln EQ  wl_zsdt0284-vbeln.
  IF sy-subrc NE 0.
    p_error = abap_true.
    MESSAGE 'OV não existe na base de dados!' TYPE 'E'.
    EXIT.
  ENDIF.

  "Valida se OV pertence a empresa 0035.
  IF wl_vbak-vkorg NE '0035'.
    p_error = abap_true.
    MESSAGE 'OV não pertence a empresa 0035!' TYPE 'E'.
    EXIT.
  ENDIF.

  "Valida se OV informada é do tipo 'ZRFL' ou 'ZRDC.
  IF wl_vbak-auart EQ 'ZRFL' OR wl_vbak-auart EQ 'ZRDC'.
  ELSE.
    p_error = abap_true.
    MESSAGE 'OV  não é do tipo ZRFL ou ZRDC!' TYPE 'E'.
    EXIT.
  ENDIF.


  "Valida se emissor da ordem e parceiro PC divergentes.
  SELECT SINGLE * FROM vbpa INTO wl_vbpa WHERE vbeln EQ  wl_zsdt0284-vbeln AND parvw IN ( 'PC', ' SP' ).
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ wl_vbpa-lifnr AND  land1 EQ 'BR'.
    SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr  EQ wl_vbpa-lifnr AND  land1 EQ 'BR'.

    IF wl_lfa1-stcd1 NE wl_kna1-stcd1.
      p_error = abap_true.
      DATA(msg) = |Emissor OV: { wl_vbpa-kunnr } divergente do ponto coleta: { wl_vbpa-lifnr }|.
      MESSAGE msg  TYPE 'E'.
      EXIT.
    ENDIF.

  ENDIF.
ENDFORM.

FORM f_exit_zsdt0284_0004 CHANGING p_saida TYPE any.

*  DATA: wl_zsdt0284_out TYPE zsdt0284_out.
*  DATA: wl_vbak     TYPE  vbak.
*
*  CLEAR: wl_zsdt0284_out, wl_vbak.
*
*  MOVE-CORRESPONDING p_saida TO wl_zsdt0284_out.
*
*
*  SELECT SINGLE * FROM vbak INTO wl_vbak WHERE vbeln EQ  wl_zsdt0284_out-vbeln.
*  IF sy-subrc EQ 0.
*    wl_zsdt0284_out-bukrs = wl_vbak-vkorg.
*    wl_zsdt0284_out-auart = wl_vbak-auart.
*  ENDIF.
*
*  MOVE-CORRESPONDING wl_zsdt0284_out TO p_saida.

ENDFORM.

FORM f_exit_zsdt0284_0005 CHANGING p_registro_manter TYPE any.
  DATA: wl_zsdt0284_out TYPE zsdt0284_out.
  DATA: wl_vbak     TYPE  vbak.

  CLEAR: wl_zsdt0284_out, wl_vbak.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0284_out.


  SELECT SINGLE * FROM vbak INTO wl_vbak WHERE vbeln EQ  wl_zsdt0284_out-vbeln.
  IF sy-subrc EQ 0.
    wl_zsdt0284_out-bukrs = wl_vbak-vkorg.
    wl_zsdt0284_out-auart = wl_vbak-auart.
  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0284_out TO p_registro_manter.

ENDFORM.


FORM f_exit_zsdt0284_0006 USING p_saida TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0284_out TYPE zsdt0284_out,
        wl_vbak         TYPE  vbak,
        wl_vbpa         TYPE  vbpa,
        wl_lfa1         TYPE  lfa1,
        wl_kna1         TYPE kna1.

  CLEAR: wl_zsdt0284_out, p_error.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0284_out.

  "Valida se existe a OV informanda.
  SELECT SINGLE * FROM vbak INTO wl_vbak WHERE vbeln EQ  wl_zsdt0284_out-vbeln.
  IF sy-subrc NE 0.
    p_error = abap_true.
    MESSAGE 'OV não existe na base de dados!' TYPE 'E'.
    EXIT.
  ENDIF.

  "Valida se OV pertence a empresa 0035.
  IF wl_vbak-vkorg NE '0035'.
    p_error = abap_true.
    MESSAGE 'OV não pertence a empresa 0035!' TYPE 'E'.
    EXIT.
  ENDIF.

  "Valida se OV informada é do tipo 'ZRFL' ou 'ZRDC.
  IF wl_vbak-auart EQ 'ZRFL' OR wl_vbak-auart EQ 'ZRDC'.
  ELSE.
    p_error = abap_true.
    MESSAGE 'OV  não é do tipo ZRFL ou ZRDC!' TYPE 'E'.
    EXIT.
  ENDIF.

ENDFORM.
