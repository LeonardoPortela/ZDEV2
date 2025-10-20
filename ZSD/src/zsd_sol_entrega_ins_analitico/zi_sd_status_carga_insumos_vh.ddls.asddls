@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ajuda Pesquisa Status Carga Insumos'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_SD_STATUS_CARGA_INSUMOS_VH as 
   select from dd07t as st

{
  key st.domvalue_l as status,
      st.ddtext     as ds_status
}

where
      st.domname    = 'ZSDD_STS_CARGA'
  and st.ddlanguage = 'P'
