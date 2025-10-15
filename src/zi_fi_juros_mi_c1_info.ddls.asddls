@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Juros/Desconto Antecipação - Adiantamento - ZFI0064'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_JUROS_MI_C1_INFO
  as select from    bsad_view                      as A
    inner join      bkpf                           as BK   on  A.bukrs  = BK.bukrs
                                                           and A.belnr  = BK.belnr

    inner join      t001                                   on A.bukrs = t001.bukrs

    inner join      kna1                                   on A.kunnr = kna1.kunnr

  /* Documento de Compensação possui juros/descontos de antecipação */
    inner join      ZI_FI_CONTAB_DESC_JUROS_ANTEC  as DS   on  A.bukrs = DS.bukrs
                                                           and A.augbl  = DS.belnr

  /* Possui conta de banco nas partidas do documento */
    left outer join ZI_FI_CONTAB_CONTRA_BANCO      as CB   on  A.bukrs  = CB.bukrs
                                                           and A.belnr  = CB.belnr

  /* Documento compensa partidas de documentos que possui Conta Banco */
    left outer join ZI_FI_CONTAB_COMP_CONTRA_BANCO as CCB  on  A.bukrs  = CCB.bukrs
                                                           and A.belnr  = CCB.belnr


  /* Total dos Documentos Compensados */
    inner join      ZI_FI_COMPENSACAO_DOC_TOTAIS   as tot  on  A.bukrs   = tot.bukrs
                                                           and A.augbl   = tot.augbl
                                                           and tot.KOART = 'D' -- Cliente
                                                           and tot.umskz = 'A' -- Adiantamento
  /* Dados Complementares OV */

    inner join      ZI_SD_DADOS_COMPL_OV_INFO      as comp on A.vbel2 = comp.vbeln

{

  A.bukrs                                                                                       as bukrs,
  t001.butxt,
  A.gsber                                                                                        as gsber,
  A.kunnr,
  kna1.name1                                                                                     as ds_cliente,
  'Adiantamento'                                                                                 as Tipo,
  'Juros/Descontos'                                                                              as banco_liq,
  A.buzei,
  A.belnr,
  A.augbl,
  A.augdt,
  BK.waers,
  @Semantics.amount.currencyCode: 'waers'
  tot.DMBTR                                                                                      as total_docs_comp,

  GET_NUMERIC_VALUE ( A.dmbtr ) / GET_NUMERIC_VALUE( tot.DMBTR )                                   as percentual,

  GET_NUMERIC_VALUE ( A.dmbtr ) / GET_NUMERIC_VALUE( tot.DMBTR )  *  GET_NUMERIC_VALUE( DS.dmbtr ) as Valor_Desc_Prop_dmbtr,
  GET_NUMERIC_VALUE ( A.dmbtr ) / GET_NUMERIC_VALUE( tot.DMBTR )  *  GET_NUMERIC_VALUE( DS.dmbe2 ) as Valor_Desc_Prop_dmbe2,

  A.vbel2                                                                                        as VBEL2,
  @Semantics.amount.currencyCode: 'waers'
  A.dmbtr                                                                                          as DMBTR,
  @Semantics.amount.currencyCode: 'waers'
  A.dmbtr                                                                                          as DMBE2,
  @Semantics.amount.currencyCode: 'waers'
  DS.dmbtr                                                                                       as DESC_ANTECIPACAO_DMBTR,
  @Semantics.amount.currencyCode: 'waers'
  DS.dmbe2                                                                                       as DESC_ANTECIPACAO_DMBE2,

  comp.TPSIM,
  comp.AUART,
  comp.MATNR,
  comp.Matkl,
  comp.NRO_SOL,
  comp.TP_VENDA,
  comp.MaterialName                                                                              as maktx,
  comp.charg,
  
  case
    when COALESCE(CB.belnr,'') <> '' then
       '1'
    when COALESCE(CCB.belnr,'') <> '' then
       '2'
    end as tipo_compensacao

}

where A.umskz   =  'A'
  and A.belnr  <> A.augbl
  and A.vbel2  <> ''
  and BK.stblg  =  ''
  
