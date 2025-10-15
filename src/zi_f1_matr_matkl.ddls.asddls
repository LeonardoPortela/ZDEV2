@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'MATERIAL E GRUPO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_F1_MATR_MATKL
  as select from mara   // Tabela de Materiais
    inner join makt     // Textos de Material
      on makt.matnr = mara.matnr
    inner join t023t    // Textos de Grupo de Materiais
      on  t023t.matkl = mara.matkl
      and t023t.spras = $session.system_language
{
    key mara.matnr,
    
    key makt.maktx,
    
    key mara.matkl,
    
    t023t.wgbez,
    
    mara.mtart
}
where makt.spras = $session.system_language  // Filtra pelo idioma do usuário
