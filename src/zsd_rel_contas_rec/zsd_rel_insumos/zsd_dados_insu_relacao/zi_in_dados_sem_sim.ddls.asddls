@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ovs de devolução sem simulador'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_DADOS_SEM_SIM
  as select distinct from ZI_IN_DADOS_VBAK     as vbak
    left outer join       zsdt0090             as z90     on  z90.vbeln   = vbak.vbeln
                                                          and z90.estorno = ''
    left outer join       ZI_IN_DADOS_ZSDT0041 as z41     on z41.vbeln = vbak.vbeln
  //left outer join       ZI_IN_DADOS_COMPL    as compl   on compl.vbeln = vbak.vbeln // 30.06.2025
  //left outer join       ZI_IN_DADOS_COMPL90  as compl90 on compl90.vbeln = vbak.vbeln // 21.05.2025
    left outer join       ZI_IN_DADOS_COMP2    as compl2  on compl2.vbeln = vbak.vbeln // 21.05.2025
    left outer join       ZI_IN_SIMU_OV_DEVOL  as compl90 on compl90.vbeln = vbak.vbeln // 21.05.2025



{
  key '          ' as doc_simulacao,
  key vbak.vbeln   as vbeln_p,
      vbak.vbeln   as vbeln
}
where
  (
      vbak.auart            <> 'ZFNT'
  )
  and z90.vbeln             is null
  and z41.vbeln             is null
  //and   compl.doc_simulacao   is null
  and compl2.doc_simulacao  is null
  and compl90.doc_simulacao is null
