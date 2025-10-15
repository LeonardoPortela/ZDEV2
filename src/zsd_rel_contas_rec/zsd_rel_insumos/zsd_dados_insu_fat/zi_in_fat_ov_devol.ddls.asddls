@AbapCatalog.sqlViewName: 'ZIINFATOVD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Faturamento Devolução'
define view ZI_IN_FAT_OV_DEVOL
  as select distinct from ZI_IN_OV_DADOS_2 as vbak
    inner join            vbfa             as vbfa  on vbfa.vbelv = vbak.vbeln
    inner join            zsdt0350_in      as z350  on  vbak.auart = z350.auart
                                                    and z350.acao  = 'E'
    inner join            ZI_IN_VBFA_VBRK  as vbfa2 on vbfa2.vbeln_va = vbak.vbeln
{
  key vbfa.vbelv      as vbeln_va,
  key vbfa.vbeln      as vbeln_vf,
      sum(vbfa.rfmng) as menge_fat,
      sum(vbfa.rfwrt) as netwr_fat


}

where
      vbak.fator_devol = -1
  and vbfa2.vbeln_va   is not null

group by
  vbfa.vbelv,
  vbfa.vbeln
