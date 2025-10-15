FUNCTION zles_fat_contingencia_0002.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CH_REFERENCIA) TYPE  ZCH_REF OPTIONAL
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(I_GET_DADOS_FAT_ECC) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_NOT_SHOW_DIVERGENCIA) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_CHECK_FRETE_OK) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_CHECK_DANFE_OK) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_CHECK_DANFE_ZNFW_OK) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_CHECK_DANFE_ARM_OK) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_CHECK_VALOR) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_OK) TYPE  CHAR01
*"     REFERENCE(E_MSG_RETORNO) TYPE  STRING
*"     VALUE(E_DADOS_FATURAMENTO_ECC) TYPE  ZDE_COMPARE_FATURAMENTO
*"     VALUE(E_IMPOSTOS_NFE) TYPE  STRING
*"----------------------------------------------------------------------

  DATA: lva_destination TYPE char40.

  DATA: lva_ok          TYPE char1,
        lva_msg_retorno TYPE string.

  DATA: lva_vbeln_ecc  TYPE  vbeln.
  DATA: lva_inco1      TYPE  vbkd-inco1.

  DATA: lwa_dados_faturamento_s4  TYPE zde_compare_faturamento,
        lwa_dados_faturamento_ecc TYPE zde_compare_faturamento.

  CLEAR: e_dados_faturamento_ecc, e_ok, e_msg_retorno, lva_vbeln_ecc.

  DATA: lit_zsdt0001_s4 TYPE TABLE OF zsdt0001.

  CASE sy-sysid.
    WHEN 'DEV'.
      lva_destination =  'DEV_ECC'.
    WHEN 'QAS'.
      lva_destination =  'QAS_ECC'.
    WHEN 'PRD'.
      lva_destination =  'PRD_ECC'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  CHECK lva_destination IS NOT INITIAL.

  IF i_vbeln IS NOT INITIAL.
    SELECT SINGLE *
      FROM zlest0108 INTO @DATA(lwa_zlest0108)
     WHERE vbeln = @i_vbeln.

    IF sy-subrc EQ 0.
      lva_vbeln_ecc = lwa_zlest0108-vbeln_ecc.
    ENDIF.
  ENDIF.

  IF i_get_dados_fat_ecc EQ abap_false.

    IF i_ch_referencia IS NOT INITIAL.
      PERFORM f_preenche_dados_fat_rom USING i_ch_referencia
                                    CHANGING lwa_dados_faturamento_s4.

    ELSEIF i_vbeln IS NOT INITIAL.

      PERFORM f_preenche_dados_fat_fre_int USING i_vbeln
                                    CHANGING lwa_dados_faturamento_s4.
    ELSE.
      EXIT.
    ENDIF.

  ENDIF.

  "Função que Le o XML no GRC
  CALL FUNCTION 'ZLES_FATURAMENTO_CONTINGENCIA' DESTINATION lva_destination
    EXPORTING
      i_operacao          = '02'
      i_ch_referencia     = i_ch_referencia
      i_vbeln             = lva_vbeln_ecc
    IMPORTING
      e_impostos_nfe      = e_impostos_nfe
      e_dados_faturamento = lwa_dados_faturamento_ecc.

  IF i_get_dados_fat_ecc EQ abap_false.

    if lwa_dados_faturamento_s4-tip_st_ciot <> lwa_dados_faturamento_ecc-tip_st_ciot.
      CLEAR: lwa_dados_faturamento_s4-tip_st_ciot, lwa_dados_faturamento_ecc-tip_st_ciot.
    endif.

    IF i_check_danfe_ok      EQ abap_true OR
       i_check_frete_ok      EQ abap_true OR
       i_check_danfe_znfw_ok EQ abap_true OR
       i_check_danfe_arm_ok  EQ abap_true.

      SELECT SINGLE *
        FROM tvarvc INTO @DATA(tvarvc_fat)
       WHERE name = 'FAT_CONT_CHECK_STEPS'
         AND low = @abap_true.

      IF sy-subrc NE 0.
        e_ok = abap_true.
        RETURN.
      ENDIF.

      IF i_check_danfe_ok EQ abap_true.



        IF i_check_valor EQ abap_true.

          IF lwa_dados_faturamento_s4-data_lcto_nf <> lwa_dados_faturamento_ecc-data_lcto_nf OR
             lwa_dados_faturamento_s4-netwr_nf <> lwa_dados_faturamento_ecc-netwr_nf OR
             lwa_dados_faturamento_s4-cfop <> lwa_dados_faturamento_ecc-cfop OR
             lwa_dados_faturamento_s4-nfenum_nfe <> lwa_dados_faturamento_ecc-nfenum_nfe.

            e_ok = abap_false.
            e_msg_retorno = 'Nota Fiscal com divergencias! Conferir detalhes na Opção Check Faturamento ECC!'.
          ELSE.
            e_ok = abap_true.
            RETURN.
          ENDIF.

        ELSE.

          IF "lwa_dados_faturamento_s4-data_lcto_nf <> lwa_dados_faturamento_ecc-data_lcto_nf OR
             "lwa_dados_faturamento_s4-netwr_nf <> lwa_dados_faturamento_ecc-netwr_nf OR
             lwa_dados_faturamento_s4-cfop <> lwa_dados_faturamento_ecc-cfop.
            e_ok = abap_false.
            e_msg_retorno = 'Nota Fiscal com divergencias! Conferir detalhes na Opção Check Faturamento ECC!'.
          ELSE.
            e_ok = abap_true.
            RETURN.
          ENDIF.


        ENDIF.


        RETURN.
      ENDIF.

      IF i_check_danfe_znfw_ok EQ abap_true.

        IF i_check_valor EQ abap_true.

          IF lwa_dados_faturamento_s4-data_lcto_nf_rem <> lwa_dados_faturamento_ecc-data_lcto_nf_rem OR
             lwa_dados_faturamento_s4-netwr_nf_rem     <> lwa_dados_faturamento_ecc-netwr_nf_rem OR
             lwa_dados_faturamento_s4-nfenum_nfe_nfw   <> lwa_dados_faturamento_ecc-nfenum_nfe_nfw.

            e_ok = abap_false.
            e_msg_retorno = 'Nota Fiscal ZNFW com divergencias! Conferir detalhes na Opção Check Faturamento ECC!'.
          ELSE.
            e_ok = abap_true.
            RETURN.
          ENDIF.

        ELSE.

          e_ok = abap_true.

        ENDIF.

        RETURN.
      ENDIF.

      IF i_check_frete_ok EQ abap_true.


        IF i_check_valor EQ abap_true.

          IF lwa_dados_faturamento_s4-data_lcto_cte   <> lwa_dados_faturamento_ecc-data_lcto_cte   OR
            lwa_dados_faturamento_s4-netwr_cte       <> lwa_dados_faturamento_ecc-netwr_cte      OR
            lwa_dados_faturamento_s4-nfenum_cte      <> lwa_dados_faturamento_ecc-nfenum_cte      OR
            lwa_dados_faturamento_s4-kbetr_zfre      <> lwa_dados_faturamento_ecc-kbetr_zfre     OR
            lwa_dados_faturamento_s4-kbetr_zseg      <> lwa_dados_faturamento_ecc-kbetr_zseg   OR
            lwa_dados_faturamento_s4-kbetr_ziof      <> lwa_dados_faturamento_ecc-kbetr_ziof   OR
            lwa_dados_faturamento_s4-kbetr_zped      <> lwa_dados_faturamento_ecc-kbetr_zped   OR
            lwa_dados_faturamento_s4-kbetr_zadm      <> lwa_dados_faturamento_ecc-kbetr_zadm   OR
            lwa_dados_faturamento_s4-kbetr_zpis      <> lwa_dados_faturamento_ecc-kbetr_zpis   OR
            lwa_dados_faturamento_s4-kbetr_zcof      <> lwa_dados_faturamento_ecc-kbetr_zcof   OR
            lwa_dados_faturamento_s4-cpf_cnpj_pv     <> lwa_dados_faturamento_ecc-cpf_cnpj_pv   OR
            lwa_dados_faturamento_s4-cpf_cnpj_mt     <> lwa_dados_faturamento_ecc-cpf_cnpj_mt   OR
            lwa_dados_faturamento_s4-cpf_cnpj_lr     <> lwa_dados_faturamento_ecc-cpf_cnpj_lr   OR
            lwa_dados_faturamento_s4-cpf_cnpj_sp     <> lwa_dados_faturamento_ecc-cpf_cnpj_sp   OR
            lwa_dados_faturamento_s4-cpf_cnpj_pc     <> lwa_dados_faturamento_ecc-cpf_cnpj_pc   OR
            lwa_dados_faturamento_s4-cpf_cnpj_z1     <> lwa_dados_faturamento_ecc-cpf_cnpj_z1   OR
            lwa_dados_faturamento_s4-cpf_cnpj_sg     <> lwa_dados_faturamento_ecc-cpf_cnpj_sg.

            e_ok = abap_false.
            e_msg_retorno = 'Documentos de Frete com divergencias! Conferir detalhes na Opção Check Faturamento ECC!'.
          ELSE.
            e_ok = abap_true.
          ENDIF.

        ELSE.

          IF "lwa_dados_faturamento_s4-data_lcto_cte   <> lwa_dados_faturamento_ecc-data_lcto_cte   OR
           "lwa_dados_faturamento_s4-netwr_cte       <> lwa_dados_faturamento_ecc-netwr_cte      OR
           lwa_dados_faturamento_s4-kbetr_zfre      <> lwa_dados_faturamento_ecc-kbetr_zfre     OR
           lwa_dados_faturamento_s4-kbetr_zseg      <> lwa_dados_faturamento_ecc-kbetr_zseg   OR
           lwa_dados_faturamento_s4-kbetr_ziof      <> lwa_dados_faturamento_ecc-kbetr_ziof   OR
           lwa_dados_faturamento_s4-kbetr_zped      <> lwa_dados_faturamento_ecc-kbetr_zped   OR
           lwa_dados_faturamento_s4-kbetr_zadm      <> lwa_dados_faturamento_ecc-kbetr_zadm   OR
           lwa_dados_faturamento_s4-kbetr_zpis      <> lwa_dados_faturamento_ecc-kbetr_zpis   OR
           lwa_dados_faturamento_s4-kbetr_zcof      <> lwa_dados_faturamento_ecc-kbetr_zcof   OR
           lwa_dados_faturamento_s4-cpf_cnpj_pv     <> lwa_dados_faturamento_ecc-cpf_cnpj_pv   OR
           lwa_dados_faturamento_s4-cpf_cnpj_mt     <> lwa_dados_faturamento_ecc-cpf_cnpj_mt   OR
           lwa_dados_faturamento_s4-cpf_cnpj_lr     <> lwa_dados_faturamento_ecc-cpf_cnpj_lr   OR
           lwa_dados_faturamento_s4-cpf_cnpj_sp     <> lwa_dados_faturamento_ecc-cpf_cnpj_sp   OR
           lwa_dados_faturamento_s4-cpf_cnpj_pc     <> lwa_dados_faturamento_ecc-cpf_cnpj_pc   OR
           lwa_dados_faturamento_s4-cpf_cnpj_z1     <> lwa_dados_faturamento_ecc-cpf_cnpj_z1   OR
           lwa_dados_faturamento_s4-cpf_cnpj_sg     <> lwa_dados_faturamento_ecc-cpf_cnpj_sg.

            e_ok = abap_false.
            e_msg_retorno = 'Documentos de Frete com divergencias! Conferir detalhes na Opção Check Faturamento ECC!'.
          ELSE.
            e_ok = abap_true.
          ENDIF.

        ENDIF.



        RETURN.
      ENDIF.


      IF i_check_danfe_arm_ok EQ abap_true.

        IF i_check_valor EQ abap_true.

          IF lwa_dados_faturamento_s4-data_lcto_nf_arm <> lwa_dados_faturamento_ecc-data_lcto_nf_arm OR
             lwa_dados_faturamento_s4-netwr_nf_arm    <> lwa_dados_faturamento_ecc-netwr_nf_arm OR
             lwa_dados_faturamento_s4-nfenum_nfe_arm   <> lwa_dados_faturamento_ecc-nfenum_nfe_arm.

            e_ok = abap_false.
            e_msg_retorno = 'Nota Fiscal Armazenagem com divergencias! Conferir detalhes na Opção Check Faturamento ECC!'.
          ELSE.
            e_ok = abap_true.
            RETURN.
          ENDIF.

        ELSE.

          e_ok = abap_true.

        ENDIF.

        RETURN.
      ENDIF.

      RETURN.

    ENDIF. "Fim check Faturamento por Etapa

    DATA(_considera_diferenca) = abap_true.
    IF lwa_dados_faturamento_s4 <> lwa_dados_faturamento_ecc AND i_ch_referencia IS NOT INITIAL.

      SELECT SINGLE *
        FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
       WHERE ch_referencia EQ @i_ch_referencia.

      IF sy-subrc EQ 0 AND lwa_zsdt0001-tp_movimento EQ 'S' AND lwa_zsdt0001-vbeln IS NOT INITIAL.

        CLEAR: lva_inco1.

        SELECT SINGLE inco1
          FROM vbkd INTO lva_inco1
         WHERE vbeln EQ lwa_zsdt0001-vbeln.

        IF sy-subrc NE 0.
          SELECT SINGLE inco1
            FROM ekpo INTO lva_inco1
           WHERE ebeln EQ lwa_zsdt0001-vbeln.
        ENDIF.

        IF lva_inco1 = 'CPT'.

          "Verificar diferença somente nas outras etapas
          IF  lwa_dados_faturamento_s4-ch_referencia              =   lwa_dados_faturamento_ecc-ch_referencia       AND
              lwa_dados_faturamento_s4-vbeln                      =   lwa_dados_faturamento_ecc-vbeln               AND
              lwa_dados_faturamento_s4-data_lcto_nf               =   lwa_dados_faturamento_ecc-data_lcto_nf        AND
              lwa_dados_faturamento_s4-data_lcto_mdfe             =   lwa_dados_faturamento_ecc-data_lcto_mdfe      AND
              lwa_dados_faturamento_s4-netwr_nf                   =   lwa_dados_faturamento_ecc-netwr_nf            AND
              lwa_dados_faturamento_s4-netwr_mdfe                 =   lwa_dados_faturamento_ecc-netwr_mdfe          AND
              lwa_dados_faturamento_s4-chave_nfe                  =   lwa_dados_faturamento_ecc-chave_nfe           AND
              lwa_dados_faturamento_s4-chave_mdfe                 =   lwa_dados_faturamento_ecc-chave_mdfe          AND
              lwa_dados_faturamento_s4-averb_nr_averbacao         =   lwa_dados_faturamento_ecc-averb_nr_averbacao  AND
              lwa_dados_faturamento_s4-averb_nr_protocolo         =   lwa_dados_faturamento_ecc-averb_nr_protocolo  AND
              lwa_dados_faturamento_s4-authcode_nfe               =   lwa_dados_faturamento_ecc-authcode_nfe        AND
              lwa_dados_faturamento_s4-authcode_mdfe              =   lwa_dados_faturamento_ecc-authcode_mdfe       AND
              lwa_dados_faturamento_s4-mdfe_encerrado             =   lwa_dados_faturamento_ecc-mdfe_encerrado      AND
              lwa_dados_faturamento_s4-data_lcto_nf_rem           =   lwa_dados_faturamento_ecc-data_lcto_nf_rem    AND
              lwa_dados_faturamento_s4-netwr_nf_rem               =   lwa_dados_faturamento_ecc-netwr_nf_rem        AND
              lwa_dados_faturamento_s4-chave_nfe_rem              =   lwa_dados_faturamento_ecc-chave_nfe_rem       AND
              lwa_dados_faturamento_s4-authcode_nfe_rem           =   lwa_dados_faturamento_ecc-authcode_nfe_rem    AND
              lwa_dados_faturamento_s4-data_lcto_nf_arm           =   lwa_dados_faturamento_ecc-data_lcto_nf_arm    AND
              lwa_dados_faturamento_s4-netwr_nf_arm               =   lwa_dados_faturamento_ecc-netwr_nf_arm        AND
              lwa_dados_faturamento_s4-chave_nfe_arm              =   lwa_dados_faturamento_ecc-chave_nfe_arm       AND
              lwa_dados_faturamento_s4-authcode_nfe_arm           =   lwa_dados_faturamento_ecc-authcode_nfe_arm    AND
              lwa_dados_faturamento_s4-cfop                       =   lwa_dados_faturamento_ecc-cfop                AND
              lwa_dados_faturamento_s4-nfenum_nfe                 =   lwa_dados_faturamento_ecc-nfenum_nfe          AND
              lwa_dados_faturamento_s4-nfenum_mdfe                =   lwa_dados_faturamento_ecc-nfenum_mdfe         AND
              lwa_dados_faturamento_s4-nfenum_nfe_nfw             =   lwa_dados_faturamento_ecc-nfenum_nfe_nfw      AND
              lwa_dados_faturamento_s4-nfenum_nfe_arm             =   lwa_dados_faturamento_ecc-nfenum_nfe_arm.

            _considera_diferenca = abap_false.
          ENDIF.

        ENDIF.

      ENDIF."IF sy-subrc EQ 0 AND lwa_zsdt0001-tp_movimento EQ 'S' AND lwa_zsdt0001-vbeln IS NOT INITIAL.

    ENDIF.


    IF lwa_dados_faturamento_s4 <> lwa_dados_faturamento_ecc AND _considera_diferenca = abap_true.
      IF i_not_show_divergencia EQ abap_false.
        PERFORM f_show_divergencia_faturamento USING lwa_dados_faturamento_s4
                                                     lwa_dados_faturamento_ecc.
      ENDIF.


      e_ok            = abap_false.
      e_msg_retorno   = 'Faturamentos com divergencias: Chave Ref.:' && i_ch_referencia. " && 'CFOP HANA' && lwa_dados_faturamento_s4-cfop  && 'CFOP ECC: ' && lwa_dados_faturamento_ecc-cfop  .
    ELSE.
      e_ok            = abap_true.
      e_msg_retorno   = 'Faturamentos sem divergencias: Chave Ref.:' && i_ch_referencia .
    ENDIF.
  ENDIF.


  IF lwa_dados_faturamento_s4-cfop <> lwa_dados_faturamento_ecc-cfop.
    CLEAR: e_msg_retorno.
    e_ok            = abap_false.
    e_msg_retorno   = 'CFOP COM DIVERGENCIA ' && i_ch_referencia  && 'CFOP HANA' && lwa_dados_faturamento_s4-cfop  && 'CFOP ECC: ' && lwa_dados_faturamento_ecc-cfop  .
  ELSE.

  ENDIF.
  e_dados_faturamento_ecc = lwa_dados_faturamento_ecc.



ENDFUNCTION.
