FUNCTION zsdmf_get_saldo_ov.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_VBELN) TYPE  VBELN
*"     REFERENCE(IV_DOC_SIMULACAO) TYPE  ZSDED003 OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_SALDO) TYPE  ZSDC080
*"----------------------------------------------------------------------


  DATA lv_tabix TYPE sytabix.
  DATA ls_saldo TYPE zsds080.

  CHECK iv_vbeln IS NOT INITIAL.

  CLEAR et_saldo[].

  SELECT vbap~vbeln,posnr,knumv,matnr,charg,werks,umziz,kwmeng,meins,vbap~waerk,vbap~netpr,vbap~netwr FROM vbap
    INNER JOIN vbak ON vbak~vbeln = vbap~vbeln
    INTO TABLE @DATA(lt_vbap)
      WHERE vbap~vbeln = @iv_vbeln.

  CHECK lt_vbap IS NOT INITIAL.

  SELECT * FROM vbfa
    INTO TABLE @DATA(lt_vbfa)
    FOR ALL ENTRIES IN @lt_vbap
    WHERE vbelv EQ @lt_vbap-vbeln
      AND posnv EQ @lt_vbap-posnr
      AND vbtyp_n EQ 'J'
      AND vbtyp_v EQ 'C'.

  IF lt_vbfa[] IS NOT INITIAL.

    SELECT * FROM vbfa
      INTO TABLE @DATA(lt_vbfa_aux)
       FOR ALL ENTRIES IN @lt_vbfa
       WHERE vbeln EQ @lt_vbfa-vbeln
         AND vbtyp_n EQ 'J'
         AND vbtyp_v EQ 'J'.

    LOOP AT lt_vbfa_aux ASSIGNING FIELD-SYMBOL(<fs_aux>).
      DELETE lt_vbfa WHERE vbeln EQ <fs_aux>-vbeln.
    ENDLOOP.

  ENDIF.

  SORT lt_vbfa BY vbelv.

  LOOP AT lt_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>).

    CLEAR ls_saldo.

    LOOP AT lt_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>)
      WHERE vbelv EQ <fs_vbap>-vbeln
        AND posnv EQ <fs_vbap>-posnr.

      ls_saldo-vbeln = <fs_vbfa>-vbelv.
      ls_saldo-posnr = <fs_vbfa>-posnv.
      ls_saldo-werks = <fs_vbap>-werks.
      ls_saldo-waerk = <fs_vbap>-waerk.

      IF <fs_vbfa>-meins EQ <fs_vbfa>-vrkme.
        ls_saldo-menge_saldo  = <fs_vbfa>-rfmng.
      ELSE.
        ls_saldo-menge_saldo = <fs_vbfa>-rfmng / <fs_vbap>-umziz.
      ENDIF.

      COLLECT ls_saldo INTO et_saldo.

      lv_tabix = sy-tabix.

    ENDLOOP.

    READ TABLE et_saldo INTO ls_saldo INDEX lv_tabix.

    ls_saldo-knumv = <fs_vbap>-knumv.
    ls_saldo-netpr = <fs_vbap>-netpr.
    ls_saldo-netwr = <fs_vbap>-netwr.

    ls_saldo-matnr = <fs_vbap>-matnr.
    ls_saldo-menge = <fs_vbap>-kwmeng.
    ls_saldo-meins = <fs_vbap>-meins.
    ls_saldo-doc_simulacao = iv_doc_simulacao.

    ls_saldo-menge_saldo = ( ls_saldo-menge - ls_saldo-menge_saldo ).

    ls_saldo-netwr_saldo = <fs_vbap>-netpr * ls_saldo-menge_saldo.

    ls_saldo-menge_fat = ls_saldo-menge - ls_saldo-menge_saldo.
    ls_saldo-netwr_fat = ls_saldo-netwr - ls_saldo-netwr_saldo.


    IF lv_tabix IS NOT INITIAL.

      MODIFY et_saldo FROM ls_saldo INDEX lv_tabix.

    ELSE.

      ls_saldo-vbeln = <fs_vbap>-vbeln.
      ls_saldo-werks = <fs_vbap>-werks.
      ls_saldo-posnr = <fs_vbap>-posnr.
      APPEND ls_saldo TO et_saldo.

    ENDIF.

    CLEAR lv_tabix.

  ENDLOOP.

ENDFUNCTION.
