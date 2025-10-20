@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ajuda Pesquisa Status Distribuição Insumos'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_STATUS_DISTRIB_INSUMO_VH 
as select from dd07t as st

{
  key st.domvalue_l as status,
      st.ddtext     as ds_status
}

where
      st.domname    = 'ZSDD019'
  and st.ddlanguage = 'P'
  and ( st.domvalue_l = '2' or
        st.domvalue_l = '4' or
        st.domvalue_l = '5' ) 
