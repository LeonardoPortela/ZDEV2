@AbapCatalog.sqlViewName: 'ZPARAMCONTFRET'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Parametros para geração de ordem'
@Metadata.ignorePropagatedAnnotations: true
define view zi_param_cont_fret as select from zparam_cont_fret
{
    key bukrs as Bukrs,
    key werks as Werks,
    key tp_producao as TpProducao,
    key dco as Dco,
    key auart as Auart,
    key industrializacao as Industrializacao,
    kunnr as Kunnr,
    zieme as Zieme,
    pmein as Pmein,
    nivel as Nivel,
    cod_fp as CodFp,
    tipo_calc as TipoCalc,
    preco as Preco,
    c_decimais as CDecimais,
    zlsch as Zlsch,
    zterm as Zterm,
    qte_venc as QteVenc,
    vkaus as Vkaus,
    param_espec as ParamEspec,
    tp_venda as TpVenda,
    status as Status,
    vtweg as Vtweg,
    spart as Spart,
    multiplicador as Multiplicador,
    usnam as Usnam,
    data_atual as DataAtual,
    hora_atual as HoraAtual
}
