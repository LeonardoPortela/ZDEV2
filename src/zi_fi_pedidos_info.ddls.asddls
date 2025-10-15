@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Informações Pedido ZUB'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_PEDIDOS_INFO
  as select from zsdt0051
    inner join   zsdt0053                       on zsdt0053.nro_sol_ov = zsdt0051.nro_sol_ov
    inner join   zi_fi_pedidos_zub_mseg as mseg on  mseg.ebeln = zsdt0053.vbeln
                                                and mseg.xauto = 'X'
    inner join   mara                           on mara.matnr = mseg.matnr
    inner join   makt                           on  makt.matnr = mseg.matnr
                                                and makt.spras = $session.system_language
    inner join   kna1                           on kna1.kunnr = mseg.kunnr
    inner join   bkpf                           on bkpf.awkey = mseg.awkey
    inner join   bseg                           on  bseg.bukrs = bkpf.bukrs
                                                and bseg.belnr = bkpf.belnr
                                                and bseg.gjahr = bkpf.gjahr
                                                and bseg.buzei = '001'
    inner join   t001                           on t001.bukrs = bkpf.bukrs
{
  key bkpf.awkey,
      bkpf.bukrs,
      bseg.shkzg,
      mseg.gsber,
      mseg.kunnr,
      kna1.name1,
      'Ped. Transf.'                as tipo,
      bkpf.belnr,
      mseg.mjahr,
      mseg.zeile,
      ''                            as augbl,
      ''                            as augdt,
      bkpf.budat,
      zsdt0053.vbeln                as vbel2,
      mseg.mblnr                    as vbeln,
      zsdt0051.auart,
      zsdt0053.nro_sol_ov,
      zsdt0051.tp_venda,
      mseg.meins,
      @Semantics.quantity.unitOfMeasure: 'meins'
      mseg.menge,
      bkpf.waers,
      @Semantics.amount.currencyCode : 'waers'
      case when zsdt0053.dmbtr is not initial then
        zsdt0053.dmbtr 
      else
        cast( 1 as abap.curr( 23, 2 ))
      end                         as dmbtr_0053,
      @Semantics.amount.currencyCode : 'waers'
      case when bseg.dmbtr  is not initial then
        bseg.dmbtr
      else
        cast( 1 as abap.curr( 23, 2 ))
      end                           as dmbtr_bseg,
      @Semantics.amount.currencyCode : 'waers'
      case when bseg.dmbe2  is not initial then
        bseg.dmbe2
      else
        cast( 1 as abap.curr( 23, 2 ))
      end                           as dmbe2_bseg,
      'Transferência entre Filiais' as banco_liq,
      bkpf.budat                    as zfbdt,
      t001.butxt,
      mseg.matnr,
      mara.matkl,
      mara.gewei                    as gewei,

      case  mara.gewei when 'KG' then 1000
        else 1 end                  as fator,

      makt.maktx,
      1                             as buzei,
      mseg.charg                    as charg,
      'VV'                          as tpsim,
      '90'                          as spart,
      kna1.ktokd
}
where
  zsdt0053.vbeln is not initial
