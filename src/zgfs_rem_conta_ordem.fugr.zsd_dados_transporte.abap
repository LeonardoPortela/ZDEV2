FUNCTION zsd_dados_transporte.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN_VENDA) TYPE  VBELN_VA OPTIONAL
*"     REFERENCE(I_OV_DUMMY) TYPE  VBELN_VA OPTIONAL
*"     REFERENCE(I_REMESSA_DUMMY) TYPE  VBELN_VL OPTIONAL
*"     REFERENCE(I_REFKEY) TYPE  J_1BREFKEY OPTIONAL
*"     REFERENCE(I_AG_FRETE) TYPE  LIFNR OPTIONAL
*"     REFERENCE(I_NF_REMESSA) TYPE  J_1BNFNUM9 OPTIONAL
*"     REFERENCE(I_NF_VENDA) TYPE  J_1BNFNUM9 OPTIONAL
*"     REFERENCE(I_TIPO) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_REM_VBELN) TYPE  VBELN_VL OPTIONAL
*"     REFERENCE(I_REM_POSNR) TYPE  POSNR_VL OPTIONAL
*"     REFERENCE(I_PED_BSART) TYPE  ESART OPTIONAL
*"     REFERENCE(I_CHAVE_NF_VENDA) TYPE  ZDE_CHAVE_NFE OPTIONAL
*"----------------------------------------------------------------------

  DATA: l_tpfrete TYPE likp-inco1.

  FREE: ok_code,         l_vgbel,     w_zsdt0001,
        t_transp,        w_transp,    w_vbkd,
        l_cod_motorista, w_zlest0211,
        l_nome_motorista,
        l_cpf_motorista,
        l_tem_romaneio,
        l_tem_lancamento,
        l_safra_ordem_car,
        l_nro_ordem_car,
        l_peso_bruto,
        l_peso_tara,
        g_custom_container,
        g_grid.

  g_vbeln_venda    = i_vbeln_venda.
  g_ov_dummy       = i_ov_dummy.
  g_remessa_dummy  = i_remessa_dummy.
  g_refkey         = i_refkey.
  g_ag_frete       = i_ag_frete.
  g_nf_remessa     = i_nf_remessa.
  g_nf_venda       = i_nf_venda.
  g_chave_nf_venda = i_chave_nf_venda.  "*-CS2024000522-18.07.2024-JT-#143588

*** US - 92467 - Inicio - CBRAND

  g_safra     = l_safra_ordem_car.
  g_nr_ordem  = l_nro_ordem_car.
  g_tipo_proc = i_tipo.
  g_rem_vbeln = i_rem_vbeln.
  g_rem_posnr = i_rem_posnr.
  g_bsart     = i_ped_bsart.

  g_p_bruto   = l_peso_bruto.
  g_p_liquido = l_peso_tara.


  SELECT SINGLE *
   FROM lfa1
   INTO @DATA(wlfa1)
   WHERE lifnr = @g_ag_frete .

  IF wlfa1-ktokk = 'ZFIC'.
    vg_tipo_frete = 'CIF'.
  ELSE.
    vg_tipo_frete = 'CPT'.
  ENDIF.

  IF i_tipo = 'T'. "Transferencia

    DATA: t_lips TYPE STANDARD TABLE OF lips.

    SELECT SINGLE inco1    "*-CS2024000522-29.08.2024-JT-#150113-inicio
      INTO l_tpfrete
      FROM likp
     WHERE vbeln = i_rem_vbeln.

    SELECT *
      INTO TABLE t_lips
      FROM lips
     WHERE vbeln = i_rem_vbeln.

    READ TABLE t_lips INTO DATA(w_lips_aux)  WITH KEY vbeln = i_rem_vbeln.

    SELECT *
      INTO w_zlest0211
      FROM zlest0211
        UP TO 1 ROWS
      WHERE vbeln = i_rem_vbeln
      AND ebeln = w_lips_aux-vgbel
      AND ebelp = w_lips_aux-vgpos.
    ENDSELECT.

*   IF sy-subrc = 0.
*-----------------------------
* monta linhas alv
*-----------------------------

    IF  ( vg_tipo_frete = 'CPT' ) OR ( w_zlest0211-placa_cav IS NOT INITIAL ).
      w_transp-tipo_placa = 'Placa Cavalo'.
      APPEND w_transp    TO t_transp.
      w_transp-tipo_placa = 'Placa Car1'.
      APPEND w_transp    TO t_transp.
      w_transp-tipo_placa = 'Placa Car2'.
      APPEND w_transp    TO t_transp.
      w_transp-tipo_placa = 'Placa Car3'.
      APPEND w_transp    TO t_transp.

      MOVE-CORRESPONDING w_zlest0211  TO w_zsdt0001.
    ENDIF.
* US - 92467 - Inicio - CBRAND
    l_peso_bruto =  w_zlest0211-peso_bruto.
    l_peso_tara  =  w_zlest0211-peso_tara.
* US - 92467 - Fim - CBRAND

    l_safra_ordem_car = w_zlest0211-safra_ordem_car.  "*-CS2024000522-29.08.2024-JT-#150113-inicio
    l_nro_ordem_car   = w_zlest0211-nro_ordem_car.    "*-CS2024000522-29.08.2024-JT-#150113-inicio

    PERFORM f_monta_dados.

*   ENDIF.

  ELSE.
*** US - 92467 - Fim - CBRAND
*-----------------------------
* dados ov
*-----------------------------
    SELECT *
      INTO w_vbkd
      FROM vbkd
        UP TO 1 ROWS
     WHERE vbeln = g_ov_dummy.
    ENDSELECT.

    IF sy-subrc = 0.  "*-CS2024000522-29.08.2024-JT-#150113-inicio
      l_tpfrete = w_vbkd-inco1.
    ENDIF.

*-----------------------------
* monta linhas alv
*-----------------------------
    w_transp-tipo_placa = 'Placa Cavalo'.
    APPEND w_transp    TO t_transp.
    w_transp-tipo_placa = 'Placa Car1'.
    APPEND w_transp    TO t_transp.
    w_transp-tipo_placa = 'Placa Car2'.
    APPEND w_transp    TO t_transp.
    w_transp-tipo_placa = 'Placa Car3'.
    APPEND w_transp    TO t_transp.

*-----------------------------
* verifica transporte / romaneio
*-----------------------------
    SELECT vgbel
      INTO l_vgbel
      FROM vbrp
        UP TO 1 ROWS
     WHERE vbeln = i_refkey.
    ENDSELECT.

    IF sy-subrc = 0.
      SELECT *
        INTO w_zsdt0001
        FROM zsdt0001
          UP TO 1 ROWS
       WHERE doc_rem = l_vgbel.
      ENDSELECT.

      IF sy-subrc = 0.
        l_tem_romaneio = abap_true.

        CLEAR: w_zsdt0001-placa_cav, w_zsdt0001-placa_car1, w_zsdt0001-placa_car2, w_zsdt0001-placa_car3,  "*-CS2024000522-12.09.2024-JT-#152417
               w_zsdt0001-motorista.

        SELECT *
          INTO w_zlest0211
          FROM zlest0211
            UP TO 1 ROWS
         WHERE vbeln = g_remessa_dummy.
        ENDSELECT.

        IF sy-subrc = 0.
          MOVE-CORRESPONDING w_zlest0211  TO w_zsdt0001.

* US - 92467 - Inicio - CBRAND
          l_peso_bruto =  w_zlest0211-peso_bruto.
          l_peso_tara  =  w_zlest0211-peso_tara.
* US - 92467 - Fim - CBRAND

          l_safra_ordem_car = w_zlest0211-safra_ordem_car.  "*-CS2024000522-29.08.2024-JT-#150113-inicio
          l_nro_ordem_car   = w_zlest0211-nro_ordem_car.    "*-CS2024000522-29.08.2024-JT-#150113-inicio

        ENDIF.

        PERFORM f_monta_dados.
      ELSE.
        SELECT *
          INTO w_zlest0211
          FROM zlest0211
            UP TO 1 ROWS
         WHERE vbeln = g_remessa_dummy.
        ENDSELECT.

        IF sy-subrc = 0.
          MOVE-CORRESPONDING w_zlest0211  TO w_zsdt0001.

* US - 92467 - Inicio - CBRAND
          l_peso_bruto =  w_zlest0211-peso_bruto.
          l_peso_tara  =  w_zlest0211-peso_tara.
* US - 92467 - Fim - CBRAND

          PERFORM f_monta_dados.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF l_tpfrete IS NOT INITIAL.  "*-CS2024000522-29.08.2024-JT-#150113-inicio
    vg_tipo_frete = l_tpfrete.
  ENDIF.

*-----------------------------
* dados transporte
*-----------------------------
*-CS2024000522-29.08.2024-JT-#150113-inicio
  IF vg_tipo_frete = 'CIF'.
    CALL SCREEN 110 STARTING AT  34  4
                      ENDING AT 156 22.  "ENDING AT 156 15.

  ELSE.
    IF     w_zlest0211 IS INITIAL.
      PERFORM f_valida_dados USING l_ok.
      IF l_ok = abap_true AND w_zlest0211-doc_transp IS INITIAL.
        PERFORM f_grava_transp.
      ENDIF.
    ELSEIF w_zlest0211-doc_transp IS INITIAL.
      PERFORM f_elimina_dados.
    ENDIF.
  ENDIF.
*-CS2024000522-29.08.2024-JT-#150113-fim

ENDFUNCTION.
