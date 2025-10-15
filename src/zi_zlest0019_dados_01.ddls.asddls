@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BUSCA DADOS ZLEST0019'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable : true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_ZLEST0019_DADOS_01
  with parameters
    --p_matnr          : matnr,
    --p_bukrs          : bukrs,
    p_dtini_s : dats,
    p_dtfim_s : dats
    --p_dtini_e : dats,
    --p_dtfim_e : dats
    --p_werks          : werks_d,
    --p_zplaca         : zidvagao,
    --p_dcl            : zdcl
  as select from    zlest0019 as a
    left outer join zlest0019 as b on  b.dcl     = a.dcl
                                   and b.idinter = 'L3'
                                   and b.tp_reg  = '20'
    left outer join zlest0019 as c on  c.dcl     = a.dcl
                                   and c.idinter = 'L2'
                                   and c.tp_reg  = '20'
    left outer join makt      as i on  a.matnr = i.matnr
                                   and i.spras = 'P'
{
  @Search.defaultSearchElement : true
  a.bukrs,
  a.branch,
  a.matnr,
  i.maktx,
  a.nfenum,
  a.docnum,
  a.observacao,
  a.idinter,
  a.chave,
  a.dcl,
  a.seriedcl,
  a.cnpjcliente,
  a.status_duplica,
  a.cod_fornecedor,
  a.cnpjferro                       as cnpjferro_s,
  b.cnpjferro                       as cnpjferro_e,
  c.cnpjferro                       as cnpjferro_fr,
  a.tp_movi                         as tp_movi_s,
  b.tp_movi                         as tp_movi_E,
  a.tp_reg                          as tp_reg_s,
  b.tp_reg                          as tp_reg_e,
  substring( a.idvagao,1,10 )       as idvagao_s,
  substring( b.idvagao,1,10 )       as idvagao_e,
  cast( a.pesonf as abap.int8 )     as pesonf,
  cast( a.pesodvagao as abap.int8 ) as pesovagao_s,
  cast( b.pesovagao as abap.int8 )  as pesovagao_e,
  cast( c.pesovagao  as abap.int8 ) as pesovagao_t,
  a.dtadecarga                      as dtadecarga_s,
  b.dtadecarga                      as DTADECARGA_e,
  a.horadescarga                    as horadescarga_S,
  b.horadescarga                    as horadescarga_E
}
where
      1                           = 1
  and a.idinter                   = 'L2'
  and a.tp_reg                    = '30'
  and substring( b.idvagao,1,10 ) = substring( a.idvagao,1,10 )
  and substring( c.idvagao,1,10 ) = substring( a.idvagao,1,10 )
  --and a.matnr                     = $parameters.p_matnr
  --and a.bukrs                     = $parameters.p_bukrs
  --and a.branch                    = $parameters.p_werks
  and a.dtadecarga                >= $parameters.p_dtini_s
  and a.dtadecarga                <= $parameters.p_dtfim_s
--and b.dtadecarga                >= $parameters.p_dtini_e 
--and b.dtadecarga                <= $parameters.p_dtfim_e
  --and substring( a.idvagao,1,10 ) = $parameters.p_zplaca
  --and a.dcl                       = $parameters.p_dcl
