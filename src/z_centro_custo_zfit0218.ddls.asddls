@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CENTRO DE CSUTO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity Z_CENTRO_CUSTO_zfit0218 
  //with parameters
    //p_bukrs : bukrs
as select from csks as _cs
left outer join cskt as _cst  on _cs.kokrs = _cst.kokrs
                             and _cs.kostl = _cst.kostl
                             and _cs.datbi = _cst.datbi
                             and _cst.spras = $session.system_language
{
key _cs.bukrs  as Bukrs,
key _cs.gsber  as Werks,
key _cs.kostl  as Kostl,
    _cst.ktext as ktext
}
where _cs.kokrs = 'MAGI'
and _cs.datbi >= $session.system_date

