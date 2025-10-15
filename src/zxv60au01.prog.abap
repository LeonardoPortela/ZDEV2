*&---------------------------------------------------------------------*
*&  Include           ZXV60AU01
*&---------------------------------------------------------------------*

DATA: sl_komfk TYPE komfk,
      vl_fkarv TYPE likp-fkarv,
      vl_vbelv TYPE vbfa-vbelv,
      vl_vbeln TYPE vbfa-vbeln,
      vl_bukrs TYPE bkpf-bukrs,
      vl_belnr TYPE bkpf-belnr,
      vl_gjahr TYPE bkpf-gjahr,
      vl_augbl TYPE bsad-augbl.

*-CS2020001314 - 18.01.2021 - JT- inicio
CLEAR: vl_bukrs, vl_belnr, vl_gjahr, vl_augbl.

IF sy-tcode = 'VF11' OR
   sy-tcode = 'ZLES0136'.
  READ TABLE xkomfk INTO sl_komfk INDEX 1.

  SELECT      bukrs     belnr     gjahr
    INTO ( vl_bukrs, vl_belnr, vl_gjahr )
      UP TO 1 ROWS
    FROM bkpf
   WHERE awkey = sl_komfk-vbeln.
  ENDSELECT.

  IF sy-subrc = 0.
    SELECT    augbl
      INTO vl_augbl
        UP TO 1 ROWS
      FROM bsad
     WHERE bukrs = vl_bukrs
       AND belnr = vl_belnr
       AND gjahr = vl_gjahr.
    ENDSELECT.
  ENDIF.

  IF vl_augbl IS NOT INITIAL.
    MESSAGE e836(sd) WITH 'Esta Fatura está Compensada. Para prosseguir, '
                          'solicitar o cancelamento para o Contas a Receber. '
                          'Documento compensação: ' vl_augbl.
  ENDIF.
ENDIF.
*-CS2020001314 - 18.01.2021 - JT- fim

* Controle DCO - Remessa para Formação de Lote
IF sy-tcode EQ 'VF01'.
  READ TABLE xkomfk INTO sl_komfk INDEX 1.
  IF sy-subrc IS INITIAL.
    SELECT SINGLE fkarv
      FROM likp
      INTO vl_fkarv
    WHERE  vbeln EQ sl_komfk-vbeln.
  ENDIF.
  IF vl_fkarv EQ 'ZRDC'.
*    SELECT SINGLE vbelv
*      FROM vbfa
*      INTO vl_vbelv
*    WHERE  vbeln EQ sl_komfk-vbeln.
*  ENDIF.
*    IF NOT sl_komfk-vbeln IS INITIAL.
    SELECT SINGLE vbeln
      FROM zdco_vinculo
      INTO vl_vbeln
    WHERE  vbeln EQ sl_komfk-vbeln.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE e836(sd) WITH 'Remessa sem vinculo com DCO.'.
    ENDIF.
  ENDIF.
ENDIF.
