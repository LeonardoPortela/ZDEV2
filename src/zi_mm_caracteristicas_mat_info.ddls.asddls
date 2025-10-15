@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label:'Informações Caracteristica Material'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_MM_CARACTERISTICAS_MAT_INFO
  as

  select from  ausp as VL
    inner join cabn as AT on  VL.atinn = AT.atinn
                          and VL.adzhl = AT.adzhl
{

  key VL.objek as objek,
      VL.atinn as atinn,
      VL.atzhl as atzhl,
      VL.mafid as mafid,
      VL.klart as klart,
      VL.adzhl as adzhl,
      VL.atwrt as atwrt,
      AT.atnam as atnam
}
