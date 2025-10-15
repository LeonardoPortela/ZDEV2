@AbapCatalog.sqlViewName: 'ZVINDADOSSIM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados de simulador de vendas'
define view ZI_IN_DADOS_SIM
  as select from    zsdt0040 as z40
    left outer join zsdt0038 as z38 on z38.cultura = z40.cultura

{
  key z40.doc_simulacao,
      z40.id_order_ecommerce,

      z40.safra,
      z40.meio_pago,
      z40.status,
      z38.cultura,
      z38.descricao as cultura_desc,
      z40.ecommerce as ecommerce,

      case z40.meio_pago when 'D' then
        'Deposito em Conta'
      when 'A' then
        'Acerto'
      when 'B' then
        'Boleto Bancário'
      else
        'Não Antecipado'
      end           as pgto_ant,

      z40.juros_ano as tx_juros

}
