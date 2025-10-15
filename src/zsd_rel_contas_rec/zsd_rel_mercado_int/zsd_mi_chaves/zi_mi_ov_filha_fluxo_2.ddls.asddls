@AbapCatalog.sqlViewName: 'ZVMIOVFILHASFLX2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca as OV''s filhas no fluxo'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FILHA_FLUXO_2
  as select from vbak   as OV

    inner join   tvarvc as Spart   on  Spart.name = 'Z_SPART_REL_MERCADO_INTER'
                                   and Spart.low  = OV.spart

    inner join   vbfa   as OVFilha on OVFilha.vbelv = OV.vbeln


{
  key  OV.vbeln      as Ordem,
  key  OV.bukrs_vf   as Empresa,
  key  OVFilha.vbeln as OrdemFilha
}
where
  (
        OVFilha.vbtyp_n = 'C'
    and OVFilha.vbtyp_v = 'C'
  )
  or    OVFilha.vbtyp_n = 'L'
  or    OVFilha.vbtyp_n = 'H'
  or    OVFilha.vbtyp_n = 'K'




union select from vbak   as OV
//17.02.2025 17:10 --->
  left outer join tvarvc as tvarvc on  tvarvc.name = 'ZSDT0060_TP_OV_DEVOLUCAO'
                                   and tvarvc.low  = OV.auart
//17.02.2025 ---<

  inner join      tvarvc as Spart  on  Spart.name = 'Z_SPART_REL_MERCADO_INTER'
                                   and Spart.low  = OV.spart

{

  key OV.vbeln    as Ordem,
  key OV.bukrs_vf as Empresa,
  key OV.vbeln    as OrdemFilha

}
where
        tvarvc.low is null
  and(
        OV.auart   <> 'ZCXP' // 18.02.2025

    and OV.auart   <> 'ZCPV'

    and OV.auart   <> 'ZCFV' //04.08.2025

  ) // Exclui complementos
