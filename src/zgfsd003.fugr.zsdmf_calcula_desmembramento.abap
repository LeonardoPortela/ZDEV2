FUNCTION zsdmf_calcula_desmembramento.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_VBELN) TYPE  VBELN
*"     REFERENCE(IV_DOC_SIMULACAO) TYPE  ZSDED003 OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"  TABLES
*"      IT_OV_DESMEMB STRUCTURE  ZSDS081 OPTIONAL
*"      ET_OV_DESMEMB STRUCTURE  ZSDS015 OPTIONAL
*"  EXCEPTIONS
*"      SEM_SALDO
*"----------------------------------------------------------------------

  DATA lv_menge TYPE menge_d.
  DATA lv_netpr TYPE netwr_ap.

  DATA lt_saldo TYPE zsdc080.

  CALL FUNCTION 'ZSDMF_GET_SALDO_OV'
    EXPORTING
      iv_vbeln         = iv_vbeln
      iv_doc_simulacao = iv_doc_simulacao
    IMPORTING
      et_saldo         = lt_saldo.

  IF lt_saldo IS NOT INITIAL.

    SELECT matnr,spart,groes FROM mara
      INTO TABLE @DATA(lt_mara)
        FOR ALL ENTRIES IN @lt_saldo
          WHERE matnr = @lt_saldo-matnr.

    SELECT kschl,kmein,kbetr,knumv,kposn FROM konv
      INTO TABLE @DATA(lt_konv)
        FOR ALL ENTRIES IN @lt_saldo
          WHERE knumv = @lt_saldo-knumv
            AND kschl = 'PR00'.

  ENDIF.

  LOOP AT lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

    READ TABLE lt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>)
      WITH KEY matnr = <fs_saldo>-matnr.

    CHECK sy-subrc EQ 0.

    APPEND INITIAL LINE TO et_ov_desmemb ASSIGNING FIELD-SYMBOL(<fs_desmem>).

    <fs_desmem>-vbeln = iv_vbeln.
    <fs_desmem>-posnr = <fs_saldo>-posnr.

    <fs_desmem>-matnr = <fs_saldo>-matnr.
    <fs_desmem>-spart = <fs_mara>-spart.

    " validação se for somente inteiro via expressao regular
    IF cl_abap_matcher=>matches( pattern = '^\d+$' text = <fs_mara>-groes ) = 'X'.
      <fs_desmem>-groes = <fs_mara>-groes.
    ELSE.
      <fs_desmem>-groes = '1'.
    ENDIF.

    "<fs_desmem>-charg = <fs_saldo>-charg.
    <fs_desmem>-meins = <fs_saldo>-meins.
    <fs_desmem>-werks = <fs_saldo>-werks.
    "<fs_desmem>-lgort = <fs_saldo>-lgort.

    READ TABLE it_ov_desmemb ASSIGNING FIELD-SYMBOL(<fs_entr>)
      WITH KEY vbeln = iv_vbeln
               posnr = <fs_saldo>-posnr.

    IF sy-subrc EQ 0.

      <fs_desmem>-zmeng = <fs_entr>-qt_tran.
      <fs_desmem>-netpr = <fs_entr>-qt_tran * <fs_saldo>-netpr.

    ELSE.

      <fs_desmem>-zmeng = <fs_saldo>-menge_saldo.
      <fs_desmem>-netpr = <fs_saldo>-netwr_saldo.


    ENDIF.

    READ TABLE lt_konv ASSIGNING FIELD-SYMBOL(<fs_konv>)
      WITH KEY knumv = <fs_saldo>-knumv
               kposn = <fs_saldo>-posnr.

    IF sy-subrc EQ 0.

      <fs_desmem>-kmeinv = <fs_konv>-kmein.
      <fs_desmem>-netprv = <fs_konv>-kbetr.

    ENDIF.

  ENDLOOP.

  DELETE et_ov_desmemb WHERE zmeng = 0.

ENDFUNCTION.
