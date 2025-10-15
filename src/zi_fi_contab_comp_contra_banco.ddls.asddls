@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Documentos que Compensam partidas Contra Banco'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_CONTAB_COMP_CONTRA_BANCO
  as select from bsad_view                 as A
    inner join   bsad_view                 as B on  A.bukrs  =  B.bukrs
                                                and A.belnr  =  B.augbl
                                                and B.belnr  <> B.augbl

    inner join   ZI_FI_CONTAB_CONTRA_BANCO as C on  B.bukrs  = C.bukrs
                                                and B.belnr  = C.belnr
{
  A.bukrs,
  A.belnr,
  A.gjahr
}
group by
  A.bukrs,
  A.belnr,
  A.gjahr
