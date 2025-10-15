FUNCTION z_sd_outbound_ord_venda.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(NU_ORDEM_VENDA) TYPE  VBAK-VBELN OPTIONAL
*"     VALUE(TP_ORDEM_VENDA) TYPE  VBAK-AUART OPTIONAL
*"     VALUE(NU_ITEM) TYPE  VBAP-POSNR OPTIONAL
*"     VALUE(NU_DIV_REMESSA) TYPE  VBEP-ETENR OPTIONAL
*"     VALUE(DT_ORDEM_VENDA) TYPE  VBAK-ERDAT OPTIONAL
*"     VALUE(TP_FRETE) TYPE  VBKD-INCO1 OPTIONAL
*"     VALUE(ID_CLIENTE) TYPE  VBAK-KUNNR OPTIONAL
*"     VALUE(QT_ORDEM_VENDA) TYPE  VBAP-KWMENG OPTIONAL
*"     VALUE(CD_MATERIAL) TYPE  VBAP-MATNR OPTIONAL
*"     VALUE(DESC_MATERIAL) TYPE  VBAP-ARKTX OPTIONAL
*"     VALUE(VR_UNITARIO) TYPE  VBAP-NETPR OPTIONAL
*"     VALUE(CD_SAFRA) TYPE  VBAP-CHARG OPTIONAL
*"     VALUE(CD_EMPRESA) TYPE  VBAK-VKORG OPTIONAL
*"     VALUE(CD_CENTRO) TYPE  VBAP-WERKS OPTIONAL
*"     VALUE(CD_MOEDA) TYPE  RV45A-KOEIN OPTIONAL
*"     VALUE(ST_ATIVIDADE) TYPE  VBAP-SPART OPTIONAL
*"     VALUE(ST_ATUALIZACAO) TYPE  CHAR1 OPTIONAL
*"     VALUE(STATUS) TYPE  VBUK-GBSTK OPTIONAL
*"     VALUE(DT_ATUALIZACAO) TYPE  VBAK-ERDAT OPTIONAL
*"     VALUE(HR_ATUALIZACAO) TYPE  VBAK-ERZET OPTIONAL
*"     VALUE(RG_ATUALIZADO) TYPE  CHAR1 OPTIONAL
*"     VALUE(ID_INTERFACE) TYPE  NUM2 OPTIONAL
*"     VALUE(TRANSGENIA) TYPE  VBAK-KVGR3 OPTIONAL
*"     VALUE(ID_LOTE_FRETE) TYPE  ZDE_ID_LOTE_FRETE OPTIONAL
*"     VALUE(FERT_PRODUCAO) TYPE  CHAR01 OPTIONAL
*"     VALUE(EUDR) TYPE  ZEUDR_ORDEM_PEDIDO OPTIONAL
*"     VALUE(TIPO_DOCUMENTO) TYPE  CHAR02 OPTIONAL
*"  TABLES
*"      IT_VBPA STRUCTURE  VBPA OPTIONAL
*"----------------------------------------------------------------------
*--> 24.08.2023 18:14:38 - Migração S4 – ML - Início

  DATA: cl_proxy TYPE REF TO zco_z_sd_outbound_ord_venda_po,
        v_input	 TYPE zzsd_outbound_ord_venda_inpu1,
        "v_output TYPE zzsd_outbound_ord_venda_outp1,
        v_item   TYPE zvbpa.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-it_vbpa-item[].

      CLEAR: v_input.
      v_input-cd_centro = cd_centro.

      v_input-cd_empresa = cd_empresa.
      v_input-cd_material = cd_material.
      v_input-cd_moeda = cd_moeda.
      v_input-cd_safra = cd_safra.
      v_input-desc_material = desc_material.
      v_input-dt_atualizacao = dt_atualizacao.
      v_input-dt_ordem_venda = dt_ordem_venda.
      v_input-fert_producao = fert_producao.
      v_input-hr_atualizacao = hr_atualizacao.
      v_input-id_cliente = id_cliente.
      v_input-id_interface = id_interface.
      v_input-id_lote_frete = id_lote_frete.
      v_input-nu_div_remessa = nu_div_remessa.
      v_input-nu_item = nu_item.
      v_input-nu_ordem_venda = nu_ordem_venda.
      v_input-qt_ordem_venda = qt_ordem_venda.
      v_input-rg_atualizado = rg_atualizado.
      v_input-status = status.
      v_input-st_atividade = st_atividade.
      v_input-st_atualizacao = st_atualizacao.
      v_input-tp_frete = tp_frete.
      v_input-tp_ordem_venda = tp_ordem_venda.
      v_input-transgenia = transgenia.
      v_input-vr_unitario = vr_unitario.

      "us: 153332 novos atributos -LP
      v_input-EUDR = EUDR.
      v_input-TIPO_DOCUMENTO = TIPO_DOCUMENTO.

      "END us: 153332 novos atributos -LP

      LOOP AT it_vbpa INTO DATA(w_vbpa).
        CLEAR: v_item.
        v_item-mandt = w_vbpa-mandt.
        v_item-vbeln = w_vbpa-vbeln.
        v_item-posnr = w_vbpa-posnr.
        v_item-parvw = w_vbpa-parvw.
        v_item-kunnr = w_vbpa-kunnr.
        v_item-lifnr = w_vbpa-lifnr.
        v_item-pernr = w_vbpa-pernr.
        v_item-parnr = w_vbpa-parnr.
        v_item-adrnr = w_vbpa-adrnr.
        v_item-ablad = w_vbpa-ablad.
        v_item-land1 = w_vbpa-land1.
        v_item-adrda = w_vbpa-adrda.
        v_item-xcpdk = w_vbpa-xcpdk.
        v_item-hityp = w_vbpa-hityp.
        v_item-prfre = w_vbpa-prfre.
        v_item-bokre = w_vbpa-bokre.
        v_item-histunr = w_vbpa-histunr.
        v_item-knref = w_vbpa-knref.
        v_item-lzone = w_vbpa-lzone.
        v_item-hzuor = w_vbpa-hzuor.
        v_item-stceg = w_vbpa-stceg.
        v_item-parvw_ff = w_vbpa-parvw_ff.
        v_item-adrnp = w_vbpa-adrnp.
        v_item-kale = w_vbpa-kale.
        APPEND v_item TO v_input-it_vbpa-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_sd_outbound_ord_venda
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 24.08.2023 18:14:38 - Migração S4 – ML – Fim
ENDFUNCTION.
