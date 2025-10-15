@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Valida se há registro na tabela'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_CONFERE_CARGA as select from zmmt0203
left outer join zmmt0202 as itens on itens.nro_cg = zmmt0203.nro_cg
left outer join  ZI_MM_QTD_NFE_DIST_ITM as itm on itm.chave_nfe = zmmt0203.chave_nfe 
                                              and itm.ebeln = itens.ebeln
                                              and itm.ebelp = itens.ebelp
left outer join  zi_mm_valida_entrada_total as itm_tot on itm.chave_nfe = zmmt0203.chave_nfe 
                                              and itm_tot.ebeln = itens.ebeln
                                              and itm_tot.ebelp = itens.ebelp                                             
{
    key zmmt0203.nro_cg,
    sum(itm.qtd) as qtd,  
    sum(itm_tot.qtd) as qtd_tot
}
group by 
zmmt0203.nro_cg

