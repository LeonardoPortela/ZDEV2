@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Parceiros para envio - Endereços adicionais Safra'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_PARCEIROS_END_ADD
  as select from    ZI_SD_PARCEIROS_SAFRA_INT as Par

    inner join      zsdt0132                  as Rota on Rota.kunnr = Par.Parceiro
    left outer join tzone                     as Zona on Rota.lzone = Zona.zone1

{
  key Par.Parceiro,
      Rota.nr_rot     as NumRota,
      Rota.rot_desc   as NomeEnd,
      Rota.endereco   as Endereco,
      Rota.city1      as Cidade,
      Rota.uf         as Estado,
      Zona.zcep       as Cep,
      Zona.land1      as Pais,
      Zona.zlatitude  as Latidude,
      Zona.zlongitude as Longitude,
      Zona.z_url_localizacao
}
where
  Par.Tipo = 'C'

union select from ZI_SD_PARCEIROS_SAFRA_INT as Par

  inner join      zsdt0132                  as Rota on Rota.lifnr = Par.Parceiro
  left outer join tzone                     as Zona on Rota.lzone = Zona.zone1

{
  key Par.Parceiro,
      Rota.nr_rot     as NumRota,
      Rota.rot_desc   as NomeEnd,
      Rota.endereco   as Endereco,
      Rota.city1      as Cidade,
      Rota.uf         as Estado,
      Zona.zcep       as Cep,
      Zona.land1      as Pais,
      Zona.zlatitude  as Latidude,
      Zona.zlongitude as Longitude,
      Zona.z_url_localizacao
}
where
  Par.Tipo = 'F'
