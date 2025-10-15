@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cultivar Material'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_MM_CULTIVAR_MAT_INFO
  as

  select from ZI_MM_CARACTERISTICAS_MAT_INFO as vl
{

  key vl.objek as material,
      vl.atwrt as cultivar
      
}

where
      atnam    = 'NOVA_CULTIVAR'
  and vl.klart = '001'
