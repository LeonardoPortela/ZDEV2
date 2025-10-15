@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Simulação X OV - Cabeçalho'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_DADOS_SIM_OV
  as select from    vbak               as vbak
    inner join      vbkd               as vbkd  on  vbkd.vbeln = vbak.vbeln
                                                and vbkd.posnr = '000000'
    inner join      ZI_IN_OV_SUM_ITEMS as vbap  on vbap.vbeln = vbak.vbeln
    inner join      t052u              as T052U on  T052U.zterm = vbkd.zterm
                                                and T052U.spras = $session.system_language
    inner join      ZI_IN_OV_SIMULACAO as sim   on vbak.vbeln = sim.vbeln
    inner join      zsdt0040           as t0040 on t0040.doc_simulacao = sim.doc_simulacao
    inner join      zsdt0038           as t0038 on t0038.cultura = t0040.cultura
    inner join      I_Customer         as kna1  on kna1.Customer = vbak.kunnr
    left outer join tvgrt              as tvgrt on  tvgrt.vkgrp = vbak.vkgrp
                                                and tvgrt.spras = $session.system_language
  //left outer join zfit0026           as z0026 on z0026.vbeln = sim.vbeln
{
  key sim.doc_simulacao,
  key sim.vbeln_p,
  key sim.vbeln,
      t0040.safra,
      t0040.id_order_ecommerce,
      t0040.cultura,
      t0038.descricao   as cultura_descricao,
      vbak.erdat,
      vbak.auart,
      vbkd.zterm,
      t052u.text1,
      vbak.vkbur,
      vbak.kunnr,
      kna1.CustomerName as name1,
      vbak.waerk,
      vbkd.valdt        as DATA_VENC,
      //z0026.observacao,
      vbak.vkorg,
      vbak.vkgrp,
      tvgrt.bezei,
      t0040.juros_ano   as tx_juros,
      case t0040.meio_pago
        when 'D' then 'Deposito em Conta'
        when 'A' then 'Acerto'
        when 'B' then 'Boleto Bancário'
        else 'Não Atencipado'
      end               as meio_pagamento,
      'IN'              as tprel,
      //case sim.vbeln when sim.vbeln_p
      //then 'H'
      //else
      //'I' end           as tplin,
      'H' as tpLin,
      vbap.xtotalq_ov,
      vbap.xtotalvl_ov,
      vbap.netwr_l,
      vbap.mwsbp

}
