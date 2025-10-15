@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Simuladores e OV União'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_SIMU_OV_UNI
  as select distinct from ZI_IN_DADOS_SEM_SIM
{
  key doc_simulacao,
  key vbeln_p,
  key vbeln

}

union all select distinct from ZI_IN_DADOS_ZSDT0041
{
  key doc_simulacao,
  key vbeln_p,
  key vbeln

}
union all select distinct from ZI_IN_DADOS_ZSDT0090_EX
{
  key doc_simulacao,
  key vbeln_p,
  key vbeln
}
union all select distinct from ZI_IN_DADOS_ZSDT0090
{
  key doc_simulacao,
  key vbeln_p,
  key vbeln
}
/// tentativa comentario =>>>>
//union all select distinct from ZI_IN_DADOS_COMPL
//{
//  key doc_simulacao,
//  key vbeln_p,
//  key vbeln
//}
/// tentativa comentario <<<<<<<
//// 21.05.2025 --->>>
//union all select distinct from ZI_IN_DADOS_COMPL90_41
//{
//  key doc_simulacao,
//  key vbeln_p,
//  key vbeln
//}
//// 21.05.2025 ---<<<<
//30.05.2025 -- RAMON -- 180830 CENARIO 1,2,3-->>
union all select distinct from ZI_IN_DADOS_COMP2
{
  key doc_simulacao,
  key vbeln_p,
  key vbeln
}

union all select distinct from ZI_IN_SIMU_OV_DEVOL
{
  key doc_simulacao,
  key vbeln_p,
  key vbeln
}
//30.05.2025 -- RAMON -- 180830 CENARIO 3--<<





