@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Campo de Ajuda da tabela ZMM0023'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_HELP_TABLE_ZMM0023
as select distinct from zmmt0017
        inner join mara
         on mara.matnr = zmmt0017.matnr
         inner join t023t on mara.matkl = t023t.matkl and t023t.spras = 'P'
{
mara.matkl as matkl,
t023t.wgbez60 as wgbez60
}
where 1 = 1
