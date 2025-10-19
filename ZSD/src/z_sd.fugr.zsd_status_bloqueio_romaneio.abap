FUNCTION zsd_status_bloqueio_romaneio.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(CH_REFERENCIA) TYPE  ZCH_REF
*"  TABLES
*"      IT_BLOQUEIO STRUCTURE  ZSDT0001_BLOQ
*"----------------------------------------------------------------------

  DATA: wa_romaneio TYPE zsdt0001.
  DATA: wa_bloqueio TYPE zsdt0001_bloq.

  SELECT SINGLE *
    INTO wa_romaneio
    FROM zsdt0001
   WHERE ch_referencia EQ ch_referencia.

  CHECK sy-subrc EQ 0.

  CLEAR: wa_bloqueio.
  wa_bloqueio-ch_referencia    = wa_romaneio-ch_referencia.
  wa_bloqueio-tp_movimento     = wa_romaneio-tp_movimento.
  wa_bloqueio-doc_rem          = wa_romaneio-doc_rem.
  wa_bloqueio-st_cct           = wa_romaneio-st_cct.
  wa_bloqueio-ct_aquav         = wa_romaneio-ct_aquav.
  wa_bloqueio-doc_material_arm = wa_romaneio-doc_material.

  CASE wa_bloqueio-tp_movimento.
    WHEN 'E'.

      IF ( wa_romaneio-st_cct IS NOT INITIAL AND wa_romaneio-st_cct NE '01' AND wa_romaneio-st_cct NE '03' ).
        wa_bloqueio-bloquear          = 'S'.
        wa_bloqueio-mensagem_bloqueio = 'Romaneio de Entrada já enviado para o CCT! Operação não permitida!'.
      ELSEIF  wa_romaneio-ct_aquav IS NOT INITIAL.
        wa_bloqueio-bloquear          = 'S'.
        wa_bloqueio-mensagem_bloqueio = 'Romaneio de Entrada já vinculado a uma Viagem Aquaviária! Operação não permitida!'.
      ELSEIF wa_romaneio-doc_material IS NOT INITIAL.
        wa_bloqueio-bloquear          = 'S'.
        wa_bloqueio-mensagem_bloqueio = 'Já houve um movimentação de estoque para o Romaneio de Entrada no SAP! Operação não permitida!'.
      ELSE.
        wa_bloqueio-bloquear    = 'N'.
      ENDIF.

      "Issue 156798 - API Permissão Alt. Romaneio - WPP
      IF wa_bloqueio-bloquear = 'N'.
        PERFORM f_check_entrada_estoque USING wa_romaneio-ch_referencia CHANGING wa_bloqueio.
      ENDIF.
      "Issue 156798 - API Permissão Alt. Romaneio - WPP

    WHEN 'S'.

      "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Ini
      zcl_comercializacao_algodao=>get_mov_sobra_perda_romaneio(
         EXPORTING  i_ch_referencia = CONV #( wa_romaneio-ch_referencia )
         IMPORTING e_mblnr          = DATA(e_mblnr_perda_sobra)  ).

      SELECT SINGLE *
        FROM zsdt0330 INTO @DATA(lwa_zsdt330_proc)
       WHERE ch_referencia EQ @wa_romaneio-ch_referencia
         AND cancelado     EQ @abap_false
         AND status_fardo  NE '3'.

      DATA(lit_fardos_ativos) = zcl_comercializacao_algodao=>get_fardos_romaneio(  EXPORTING  i_ch_referencia = CONV #( wa_romaneio-ch_referencia ) ).
      "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim

      IF wa_romaneio-doc_rem IS NOT INITIAL.
        wa_bloqueio-bloquear          = 'S'.
        wa_bloqueio-mensagem_bloqueio = 'Já houve um remessa criada para o Romaneio de Saida no SAP! Operação não permitida!'.
      ELSEIF wa_romaneio-seq_lcto IS NOT INITIAL.
        wa_bloqueio-bloquear          = 'S'.
        wa_bloqueio-mensagem_bloqueio = 'Já houve um lançamento ZNFW criado para o Romaneio de Saida no SAP! Operação não permitida!'.
      ELSEIF wa_romaneio-doc_material IS NOT INITIAL.
        wa_bloqueio-bloquear          = 'S'.
        wa_bloqueio-mensagem_bloqueio = 'Já houve um movimentação de estoque para o Romaneio de Saida no SAP! Operação não permitida!'.
        "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Ini
      ELSEIF lwa_zsdt330_proc IS NOT INITIAL.
        wa_bloqueio-bloquear          = 'S'.
        wa_bloqueio-mensagem_bloqueio = 'Romaneio de saída no SAP, possui uma movimentação de estoque em processamento! Operação não permitida!'.
      ELSEIF e_mblnr_perda_sobra IS NOT INITIAL.
        wa_bloqueio-bloquear          = 'S'.
        wa_bloqueio-mensagem_bloqueio = 'Romaneio de saída no SAP, possui uma movimentação de Sobra/Perda de Algodão! Operação não permitida!'.
      elseif lit_fardos_ativos[] is NOT INITIAL.
        wa_bloqueio-bloquear          = 'S'.
        wa_bloqueio-mensagem_bloqueio = 'Romaneio de saída no SAP, possui fardos ativos vinculados a um Carregamento no sistema Trace Cotton!'.
        "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim
      ELSE.
        wa_bloqueio-bloquear    = 'N'.
      ENDIF.

  ENDCASE.


  APPEND wa_bloqueio TO it_bloqueio.

ENDFUNCTION.

FORM f_check_entrada_estoque USING p_ch_referencia TYPE zsdt0001-ch_referencia
                          CHANGING c_bloqueio TYPE zsdt0001_bloq.

  SELECT obj_key
    FROM zmmt_ee_zgr INTO TABLE @DATA(lit_zmmt_ee_zgr)
   WHERE ch_referencia EQ @p_ch_referencia.

  CHECK lit_zmmt_ee_zgr[] IS NOT INITIAL.

  SELECT obj_key, mm_mblnr, mm_mblnr_sobra , ft_belnr, ft_gjahr, docnum
    FROM zmmt_ee_zgr_docs INTO TABLE @DATA(lit_zmmt_ee_zgr_docs)
     FOR ALL ENTRIES IN @lit_zmmt_ee_zgr
   WHERE obj_key EQ @lit_zmmt_ee_zgr-obj_key.

  CHECK lit_zmmt_ee_zgr_docs[] IS NOT INITIAL.

  LOOP AT lit_zmmt_ee_zgr_docs INTO DATA(lwa_zmmr_ee_zgr_docs).

    IF lwa_zmmr_ee_zgr_docs-mm_mblnr IS NOT INITIAL. "Documento Estoque Gerado
      SELECT SINGLE mblnr
        FROM mseg INTO @DATA(lwa_mseg)
       WHERE smbln EQ @lwa_zmmr_ee_zgr_docs-mm_mblnr.

      IF sy-subrc NE 0. "Não possui Estorno
        c_bloqueio-bloquear          = 'S'.
        c_bloqueio-mensagem_bloqueio = 'Romaneio de Entrada já possui entrada de estoque processada! Operação não permitida!'.
        RETURN.
      ENDIF.
    ENDIF.

    IF lwa_zmmr_ee_zgr_docs-mm_mblnr_sobra IS NOT INITIAL. "Documento Estoque Gerado
      SELECT SINGLE mblnr
        FROM mseg INTO lwa_mseg
       WHERE smbln = lwa_zmmr_ee_zgr_docs-mm_mblnr_sobra.

      IF sy-subrc NE 0. "Não possui Estorno
        c_bloqueio-bloquear          = 'S'.
        c_bloqueio-mensagem_bloqueio = 'Romaneio de Entrada já possui entrada de estoque processada! Operação não permitida!'.
        RETURN.
      ENDIF.
    ENDIF.

    IF lwa_zmmr_ee_zgr_docs-ft_belnr IS NOT INITIAL. "Documento Miro Gerada
      SELECT SINGLE belnr, gjahr, stblg
        FROM rbkp INTO @DATA(lwa_rbkp)
       WHERE belnr EQ @lwa_zmmr_ee_zgr_docs-ft_belnr
         AND gjahr EQ @lwa_zmmr_ee_zgr_docs-ft_gjahr.

      IF sy-subrc EQ 0 AND lwa_rbkp-stblg IS INITIAL. "Não possui Estorno
        c_bloqueio-bloquear          = 'S'.
        c_bloqueio-mensagem_bloqueio = 'Romaneio de Entrada já possui entrada de estoque processada! Operação não permitida!'.
        RETURN.
      ENDIF.
    ENDIF.

    IF lwa_zmmr_ee_zgr_docs-docnum IS NOT INITIAL. "Documento Fiscal Gerado
      SELECT SINGLE docnum, candat
        FROM j_1bnfdoc INTO @DATA(lwa_doc)
       WHERE docnum EQ @lwa_zmmr_ee_zgr_docs-docnum.

      IF sy-subrc EQ 0 AND lwa_doc-candat IS INITIAL. "Não possui Estorno
        c_bloqueio-bloquear          = 'S'.
        c_bloqueio-mensagem_bloqueio = 'Romaneio de Entrada já possui entrada de estoque processada! Operação não permitida!'.
        RETURN.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
