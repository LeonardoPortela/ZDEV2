FUNCTION z_les_outbound_veiculo.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_VEICULO STRUCTURE  ZLEST0002 OPTIONAL
*"----------------------------------------------------------------------
*--> 25.08.2023 16:07:48 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_z_les_outbound_veiculo_por,
        v_input	 TYPE zzles_outbound_veiculo_input1,
        "v_output TYPE zzles_outbound_veiculo_outpu1,
        v_item   TYPE zzlest0002.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-it_veiculo-item[].

      LOOP AT it_veiculo INTO DATA(w_veiculo).
        CLEAR: v_item.
        v_item-mandt = w_veiculo-mandt.
        v_item-pc_veiculo = w_veiculo-pc_veiculo.
        v_item-proprietario = w_veiculo-proprietario.
        v_item-cd_cidade = w_veiculo-cd_cidade.
        v_item-cd_uf = w_veiculo-cd_uf.
        v_item-chassi = w_veiculo-chassi.
        v_item-ano = w_veiculo-ano.
        v_item-ct_veiculo = w_veiculo-ct_veiculo.
        v_item-tp_carroceria = w_veiculo-tp_carroceria.
        v_item-cd_renavam = w_veiculo-cd_renavam.
        v_item-observacoes = w_veiculo-observacoes.
        v_item-irregularidade = w_veiculo-irregularidade.
        v_item-agregado = w_veiculo-agregado.
        v_item-st_bloqueio = w_veiculo-st_bloqueio.
        v_item-tp_veiculo = w_veiculo-tp_veiculo.
        v_item-tp_rodado = w_veiculo-tp_rodado.
        v_item-tp_carroceria2 = w_veiculo-tp_carroceria2.
        v_item-tara = w_veiculo-tara.
        v_item-cap_kg = w_veiculo-cap_kg.
        v_item-cap_m3 = w_veiculo-cap_m3.
        v_item-qt_eixo = w_veiculo-qt_eixo.
        v_item-status = w_veiculo-status.
        v_item-erdat = w_veiculo-erdat.
        v_item-erzet = w_veiculo-erzet.
        v_item-ernam = w_veiculo-ernam.
        v_item-country = w_veiculo-country.
        v_item-taxjurcode = w_veiculo-taxjurcode.
        v_item-kalsm = w_veiculo-kalsm.
        v_item-spras = w_veiculo-spras.
        v_item-marca = w_veiculo-marca.
        v_item-modelo = w_veiculo-modelo.
        v_item-cor = w_veiculo-cor.
        v_item-pstlz = w_veiculo-pstlz.
        v_item-cto_comodato = w_veiculo-cto_comodato.
        v_item-dt_venc_cto = w_veiculo-dt_venc_cto.
        v_item-dt_modificacao = w_veiculo-dt_modificacao.
        v_item-usr_modificacao = w_veiculo-usr_modificacao.
        APPEND v_item TO v_input-it_veiculo-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_les_outbound_veiculo
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 25.08.2023 16:07:48 - Migração S4 – ML – Fim
ENDFUNCTION.
