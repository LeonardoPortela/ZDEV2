@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Junção dados Simulador com Dados das Ovs'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_OV_SIMULACAO
  as select distinct from zsdt0090 as Z0090
    left outer join       zsdt0041 as Z0041 on  Z0041.doc_simulacao = Z0090.doc_simulacao
                                            and Z0041.vbeln         = Z0090.vbeln
{
  key Z0090.doc_simulacao,

  key case coalesce(Z0041.vbeln, '')
        when ''
            then Z0090.vbelv
        else
            Z0041.vbeln
         end      as vbeln_p,
  key Z0090.vbeln as vbeln
}
where
       not(
         Z0090.categoria    =  'F'
         or Z0090.categoria =  'G'
         or Z0090.categoria =  'P'
       )
  and  Z0090.vbeln          is not initial
  and  Z0090.estorno        <> 'X'

union select distinct from zsdt0090 as Z0090
  inner join               zsdt0041 as Z0041 on  Z0041.doc_simulacao = Z0090.doc_simulacao
                                             and Z0041.vbeln         = Z0090.vbelv
{
  key Z0090.doc_simulacao,
  key Z0041.vbeln as vbeln_p,
  key Z0041.vbeln as vbeln


}
