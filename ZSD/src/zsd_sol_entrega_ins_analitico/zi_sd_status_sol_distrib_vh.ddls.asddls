@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ajuda Pesquisa Status Solicitação/Distribuição Insumos'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_SD_STATUS_SOL_DISTRIB_VH
  as select from dd07t as st

{
  key st.domvalue_l as status,
      st.ddtext     as ds_status
}

where
      st.domname    = 'ZSDD019'
  and st.ddlanguage = 'P'
