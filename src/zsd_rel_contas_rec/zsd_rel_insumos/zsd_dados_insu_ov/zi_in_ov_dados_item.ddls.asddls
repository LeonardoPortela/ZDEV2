@AbapCatalog.sqlViewName: 'ZVINOVDADOSITEM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados de item OV'
define view ZI_IN_OV_DADOS_ITEM
  as select from    vbap          as Item
    left outer join ZI_IN_OV_VBEP as _vbep on  _vbep.vbeln = Item.vbeln
                                           and _vbep.posnr = Item.posnr
  //left outer join vbep as _vbep on  _vbep.vbeln = Item.vbeln
  //                                  and _vbep.posnr = Item.posnr
  //                                  and _vbep.etenr = '0001'
{
  key Item.vbeln,
      Item.waerk,

      sum( case coalesce(_vbep.lifsp,'') when '' then Item.kwmeng else case when _vbep.lifsp <> '12' then Item.kwmeng else 0 end end ) as kwmeng,
      sum( case coalesce(_vbep.lifsp,'') when '' then Item.zmeng else case when _vbep.lifsp <> '12' then Item.kwmeng else 0 end end )  as zmeng,
      sum( case coalesce(_vbep.lifsp,'') when '' then Item.netwr else case when _vbep.lifsp <> '12' then Item.netwr else 0 end end )   as netwr,
      sum( case coalesce(_vbep.lifsp,'') when '' then Item.mwsbp else case when _vbep.lifsp <> '12' then Item.mwsbp else 0 end end )   as mwsbp

}
group by
  Item.vbeln,
  Item.waerk
