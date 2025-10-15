@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label:'Pega sequência na Zib'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_ZFIR0124_MAXSEQ_ZIBCONTABIL
  with parameters
    p_ANO : gjahr,
    p_PROGR : sycprog
  as select from zib_contabil as A
{
max(substring(A.obj_key,9,6) ) as LAST_SEQ
}
where A.gjahr                      = $parameters.p_ANO
  and substring(A.obj_key,1,8)     = $parameters.p_PROGR

