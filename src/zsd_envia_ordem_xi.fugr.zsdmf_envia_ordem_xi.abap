FUNCTION zsdmf_envia_ordem_xi.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ORDEM) TYPE  ZSDE0001
*"----------------------------------------------------------------------

  DATA: wa_zsdt0268 TYPE zsdt0268.

  DATA: e_fert_producao TYPE char01.


  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZSEQ0009'
    IMPORTING
      number                  = wa_zsdt0268-seq
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  MOVE-CORRESPONDING ordem TO wa_zsdt0268.

  wa_zsdt0268-programa = sy-cprog.

  LOOP AT ordem-vbpa INTO DATA(wa_vbpa).

    IF wa_vbpa-kunnr IS NOT INITIAL.
      DATA(parceiro) = |{ wa_vbpa-kunnr ALPHA = OUT }|.
    ENDIF.

    IF wa_vbpa-lifnr IS NOT INITIAL.
      parceiro = |{ wa_vbpa-lifnr ALPHA = OUT }|.
    ENDIF.

    IF wa_zsdt0268-parceiros IS INITIAL.
      wa_zsdt0268-parceiros = |{ wa_vbpa-parvw }-{ parceiro }|.
    ELSE.
      wa_zsdt0268-parceiros = |{ wa_zsdt0268-parceiros }/{ wa_vbpa-parvw }-{ parceiro }|.
    ENDIF.

  ENDLOOP.

  "Verifica material.
  CLEAR: e_fert_producao.
  PERFORM f_check_item_material USING ordem CHANGING e_fert_producao.

*--> 24.08.2023 18:31:46 - Migração S4 – ML - Início
*  CALL FUNCTION 'Z_SD_OUTBOUND_ORD_VENDA' IN BACKGROUND TASK
*    DESTINATION 'XI_ORDEM_VENDA'
*    EXPORTING
*      nu_ordem_venda = ordem-nu_ordem_venda
*      tp_ordem_venda = ordem-tp_ordem_venda
*      nu_item        = ordem-nu_item
*      dt_ordem_venda = ordem-dt_ordem_venda
*      tp_frete       = ordem-tp_frete
*      id_cliente     = ordem-id_cliente
*      qt_ordem_venda = ordem-qt_ordem_venda
*      cd_material    = ordem-cd_material
*      vr_unitario    = ordem-vr_unitario
*      cd_safra       = ordem-cd_safra
*      cd_empresa     = ordem-cd_empresa
*      cd_centro      = ordem-cd_centro
*      cd_moeda       = ordem-cd_moeda
*      desc_material  = ordem-desc_material
*      st_atualizacao = ordem-st_atualizacao
*      status         = ordem-status
*      st_atividade   = ordem-st_atividade
*      dt_atualizacao = ordem-dt_atualizacao
*      hr_atualizacao = ordem-hr_atualizacao
*      rg_atualizado  = ordem-rg_atualizado
*      id_interface   = ordem-id_interface
*      fert_producao  = e_fert_producao
*    TABLES
*      it_vbpa        = ordem-vbpa.

  "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - INICIO
  zcl_eudr_utils=>check_ov_pedido_eudr(
    EXPORTING
      i_vbeln = ordem-nu_ordem_venda     " Nº do documento de compras
    RECEIVING
      r_eudr  =   DATA(v_eudr)               " Atende critérios Europeu
  ).
  "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - FIM

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_SD_OUTBOUND_ORD_VENDA'.

  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = c_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      EXPORTING
        nu_ordem_venda = ordem-nu_ordem_venda
        tp_ordem_venda = ordem-tp_ordem_venda
        nu_item        = ordem-nu_item
        dt_ordem_venda = ordem-dt_ordem_venda
        tp_frete       = ordem-tp_frete
        id_cliente     = ordem-id_cliente
        qt_ordem_venda = ordem-qt_ordem_venda
        cd_material    = ordem-cd_material
        vr_unitario    = ordem-vr_unitario
        cd_safra       = ordem-cd_safra
        cd_empresa     = ordem-cd_empresa
        cd_centro      = ordem-cd_centro
        cd_moeda       = ordem-cd_moeda
        desc_material  = ordem-desc_material
        st_atualizacao = ordem-st_atualizacao
        status         = ordem-status
        st_atividade   = ordem-st_atividade
        dt_atualizacao = ordem-dt_atualizacao
        hr_atualizacao = ordem-hr_atualizacao
        rg_atualizado  = ordem-rg_atualizado
        id_interface   = ordem-id_interface
        fert_producao  = e_fert_producao
        tipo_documento = 'OV'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
        eudr           = v_eudr "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
      TABLES
        it_vbpa        = ordem-vbpa.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      EXPORTING
        nu_ordem_venda = ordem-nu_ordem_venda
        tp_ordem_venda = ordem-tp_ordem_venda
        nu_item        = ordem-nu_item
        dt_ordem_venda = ordem-dt_ordem_venda
        tp_frete       = ordem-tp_frete
        id_cliente     = ordem-id_cliente
        qt_ordem_venda = ordem-qt_ordem_venda
        cd_material    = ordem-cd_material
        vr_unitario    = ordem-vr_unitario
        cd_safra       = ordem-cd_safra
        cd_empresa     = ordem-cd_empresa
        cd_centro      = ordem-cd_centro
        cd_moeda       = ordem-cd_moeda
        desc_material  = ordem-desc_material
        st_atualizacao = ordem-st_atualizacao
        status         = ordem-status
        st_atividade   = ordem-st_atividade
        dt_atualizacao = ordem-dt_atualizacao
        hr_atualizacao = ordem-hr_atualizacao
        rg_atualizado  = ordem-rg_atualizado
        id_interface   = ordem-id_interface
        fert_producao  = e_fert_producao
        tipo_documento = 'OV'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
        eudr           = v_eudr "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
      TABLES
        it_vbpa        = ordem-vbpa.
  ENDIF.

  COMMIT WORK.
*<-- 24.08.2023 18:31:46 - Migração S4 – ML – Fim

  IF sy-subrc IS INITIAL.
    wa_zsdt0268-us_envio = |{ sy-uname }-{ sy-subrc }|.
  ENDIF.

  wa_zsdt0268-dt_envio = sy-datum.
  wa_zsdt0268-hr_envio = sy-uzeit.

  MODIFY zsdt0268 FROM wa_zsdt0268.

  WAIT UP TO 1 SECONDS.

ENDFUNCTION.
