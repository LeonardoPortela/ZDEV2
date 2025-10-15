@AbapCatalog.sqlViewName: 'ZVMIOVFAT2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca faturamento das Ordens'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_2
  as select distinct from vbak     as vbak
    inner join            zsdt0350 as z350  on  vbak.auart = z350.auart
                                            and z350.acao  = 'G'
    inner join            vbfa     as vbfa  on  vbfa.vbelv   = vbak.vbeln
                                            and vbfa.vbtyp_n = z350.vbtyp_n
                                            and vbfa.vbtyp_v = z350.vbtyp_v
                                            and vbfa.stufe   = z350.stufe
    left outer join       vbrk     as vbrk  on vbrk.vbeln = vbfa.vbeln
    left outer join       vbfa     as vbfa2 on  vbfa2.vbeln   = vbfa.vbeln
                                            and vbfa2.vbtyp_n = 'M'
                                            and vbfa2.vbtyp_v = 'C'

{
  key vbak.vbeln              as Ordem,
  key case coalesce(vbfa.vbeln,'') when '' then
        vbfa2.vbeln
      else
        vbfa.vbeln
      end                     as Faturamento,
      vbfa.rfmng              as qtd,
      vbrk.netwr + vbrk.mwsbk as Valor,
      case(coalesce(vbrk.valdt,'00000000') ) when '00000000' then
        vbrk.fkdat      // <--- testar
      else
        vbrk.valdt end        as valdt,

      vbrk.zterm,

      case coalesce(vbfa2.vbeln,'') when '' then
        ''
      else
        'X' end               as fat_m_c

}
