FUNCTION zsdmf_grava_reg_zsdt0315.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_VBELN) TYPE  VBELN
*"     REFERENCE(IV_VBELN_NEW) TYPE  VBELV OPTIONAL
*"     REFERENCE(IV_WAERS) TYPE  WAERS DEFAULT 'USD'
*"     REFERENCE(IV_VALOR_OV) TYPE  NETWR_AP OPTIONAL
*"     REFERENCE(IV_LIQUI) TYPE  NETWR_AP OPTIONAL
*"     REFERENCE(IV_VLR_DESM) TYPE  NETWR_AP OPTIONAL
*"     REFERENCE(IV_COMMIT) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(IV_ESTORNO) TYPE  FLAG OPTIONAL
*"  TABLES
*"      ET_ZSDT0315 STRUCTURE  ZSDT0315 OPTIONAL
*"  EXCEPTIONS
*"      OV_100_LIQUIDADA
*"      VLR_DESM_MAIOR_OV
*"      DESM_E_LIQUI
*"      VLR_LIQUI_MAIOR_OV
*"      INFORMAR_OV_DESMEM
*"      VLR_OV_DESATUALIZADO
*"      OV_NOVA_EXISTENTE
*"----------------------------------------------------------------------

  DATA lv_vlr_restante TYPE netwr_ap.
  DATA lv_vlr_liquida TYPE netwr_ap.
  DATA lv_vlr_ov TYPE netwr_ap.

  CHECK iv_vbeln IS NOT INITIAL.

  IF iv_liqui IS NOT INITIAL AND iv_vlr_desm IS NOT INITIAL.
    MESSAGE e108(zsd) RAISING desm_e_liqui.
  ENDIF.

  CLEAR et_zsdt0315[].

  SELECT * FROM zsdt0315
    INTO TABLE et_zsdt0315
      WHERE vbeln = iv_vbeln
        AND cancelado = space.

  SORT et_zsdt0315 BY udate uzeit DESCENDING.

  READ TABLE et_zsdt0315 ASSIGNING FIELD-SYMBOL(<fs_315_old>)
    INDEX 1.

  IF sy-subrc EQ 0.

    IF <fs_315_old>-vl_liquidado = <fs_315_old>-valor_ov.

      MESSAGE e106(zsd) RAISING ov_100_liquidada.

    ENDIF.

    <fs_315_old>-cancelado = 'X'.

    lv_vlr_liquida = <fs_315_old>-vl_liquidado.

    IF iv_liqui IS NOT INITIAL.
      lv_vlr_ov = iv_liqui.
    ELSE.
      lv_vlr_ov = <fs_315_old>-valor_ov.
    ENDIF.

  ENDIF.

  IF iv_estorno IS INITIAL AND iv_valor_ov IS NOT INITIAL.

    " se o valor da ov estiver diferente pega o que achou na tabela
    IF lv_vlr_ov IS  INITIAL.
      lv_vlr_ov = iv_valor_ov.
    ENDIF.

    APPEND INITIAL LINE TO et_zsdt0315 ASSIGNING FIELD-SYMBOL(<fs_315_new>).

    <fs_315_new>-vbeln = iv_vbeln.

    PERFORM f_get_sequencial
      USING <fs_315_new>-vbeln
   CHANGING <fs_315_new>-seqnr.

    "<fs_315_new>-vbelv = IV_VBELn_new.
    <fs_315_new>-valor_ov = lv_vlr_ov.
    <fs_315_new>-vl_liquidado = lv_vlr_liquida + iv_liqui.
    <fs_315_new>-waers = iv_waers.
    <fs_315_new>-cancelado = space.
    <fs_315_new>-uname = sy-uname.
    <fs_315_new>-udate = sy-datum.
    <fs_315_new>-uzeit = sy-uzeit.

    IF <fs_315_new>-vl_liquidado >= <fs_315_new>-valor_ov.

      MESSAGE e109(zsd) RAISING vlr_liqui_maior_ov.

    ENDIF.

    IF iv_vlr_desm IS NOT INITIAL.

      IF iv_vlr_desm > lv_vlr_ov.

        MESSAGE e107(zsd) RAISING vlr_desm_maior_ov.

      ENDIF.

      IF iv_vbeln_new IS INITIAL.
        MESSAGE e110(zsd) RAISING informar_ov_desmem.
      ENDIF.

      IF iv_vbeln = iv_vbeln_new.
        MESSAGE e110(zsd) RAISING informar_ov_desmem.
      ENDIF.

      " ver se já existe desmembramento para essa ov nova
      SELECT COUNT(*) FROM zsdt0315
        WHERE vbeln = iv_vbeln_new
          AND cancelado = space.

      IF sy-dbcnt > 0.
        MESSAGE e111(zsd) RAISING ov_nova_existente.
      ENDIF.

      " o valor da ov fica o valor atual menos o desmembramento
      <fs_315_new>-valor_ov = lv_vlr_ov - iv_vlr_desm.

      " verifica o valor do desmembramento é menor que o valor ja liqui
      IF iv_vlr_desm < lv_vlr_liquida.

        " coloca a diferença como liquidado
        <fs_315_new>-vl_liquidado = lv_vlr_liquida - iv_vlr_desm.

        " o valor restante é o que já foi menos o que ficou na anterior
        lv_vlr_restante = lv_vlr_liquida - <fs_315_new>-vl_liquidado.

        " se o valor do desmembramento é igual ao já liquidado
      ELSEIF iv_vlr_desm EQ lv_vlr_liquida.

        " então a nova inicia sem valor liquidado,
        <fs_315_new>-vl_liquidado = 0.

        " o valor restante é o que já foi menos o que ficou na anterior
        lv_vlr_restante = lv_vlr_liquida - <fs_315_new>-vl_liquidado.

      ENDIF.

      " nova linha com o valor restante
      APPEND INITIAL LINE TO et_zsdt0315 ASSIGNING FIELD-SYMBOL(<fs_315_desm>).

      <fs_315_desm>-vbeln = iv_vbeln_new.

      PERFORM f_get_sequencial
        USING <fs_315_desm>-vbeln
     CHANGING <fs_315_desm>-seqnr.

      <fs_315_desm>-vbelv = iv_vbeln.

      " valor da ov é o valor do desmembramento
      <fs_315_desm>-valor_ov = iv_vlr_desm.

      " o valor liquidado é o total menos o valor liquidado na anterior.
      <fs_315_desm>-vl_liquidado = lv_vlr_restante.
      <fs_315_desm>-waers = iv_waers.
      <fs_315_desm>-cancelado = space.
      <fs_315_desm>-uname = sy-uname.
      <fs_315_desm>-udate = sy-datum.
      <fs_315_desm>-uzeit = sy-uzeit.

    ENDIF.

    "ELSEIF iv_vlr_desm > 0.
  ELSE.

    IF iv_estorno IS INITIAL.

      IF iv_vbeln_new IS INITIAL.
        MESSAGE e110(zsd) RAISING informar_ov_desmem.
      ENDIF.

      " lança uma nova linha para a ov atual
      APPEND INITIAL LINE TO et_zsdt0315 ASSIGNING <fs_315_new>.

      <fs_315_new>-vbeln = iv_vbeln.

      PERFORM f_get_sequencial
        USING <fs_315_new>-vbeln
     CHANGING <fs_315_new>-seqnr.

      " valor liquidado é igual ao que foi liquidado anteriormente
      <fs_315_new>-vl_liquidado = <fs_315_old>-vl_liquidado.

      " o valor da ov, fica igual ao valor que foi liquidado agora
      <fs_315_new>-valor_ov = <fs_315_new>-vl_liquidado.

      <fs_315_new>-waers = iv_waers.
      <fs_315_new>-cancelado = space.
      <fs_315_new>-uname = sy-uname.
      <fs_315_new>-udate = sy-datum.
      <fs_315_new>-uzeit = sy-uzeit.

      " --------------- lançamento para a nova OV

      " nova linha com o valor restante
      APPEND INITIAL LINE TO et_zsdt0315 ASSIGNING <fs_315_desm>.

      <fs_315_desm>-vbeln = iv_vbeln_new.
      <fs_315_desm>-vbelv = iv_vbeln.

      PERFORM f_get_sequencial
        USING <fs_315_desm>-vbeln
     CHANGING <fs_315_desm>-seqnr.

      IF iv_valor_ov IS INITIAL.
        " valor da ov é o valor da OV antiga - o total que ficou liquidado
        <fs_315_desm>-valor_ov = lv_vlr_ov - <fs_315_new>-vl_liquidado.
      ELSE.
        <fs_315_desm>-valor_ov = iv_valor_ov.
      ENDIF.

      " o valor liquidado é zero
      <fs_315_desm>-vl_liquidado = 0.
      <fs_315_desm>-waers = iv_waers.
      <fs_315_desm>-cancelado = space.
      <fs_315_desm>-uname = sy-uname.
      <fs_315_desm>-udate = sy-datum.
      <fs_315_desm>-uzeit = sy-uzeit.

    ENDIF.

  ENDIF.

  IF iv_commit = 'X'.

    MODIFY zsdt0315 FROM TABLE et_zsdt0315.

    COMMIT WORK AND WAIT.

  ENDIF.

ENDFUNCTION.
