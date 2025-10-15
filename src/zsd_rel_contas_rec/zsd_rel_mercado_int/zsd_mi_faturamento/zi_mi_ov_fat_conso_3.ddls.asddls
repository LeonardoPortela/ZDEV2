@AbapCatalog.sqlViewName: 'ZVMIOVFATCONS3'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exibição principal dos docs de fat'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_CONSO_3
  as select from ZI_MI_OV_FAT_CONSO_2
{
  key Simulador,
  key ItemSimulador,
  key Ordem,
  key Empresa,
  key OrdemDerivada,
      NumDocs,
      EscVendas,
      EqVendas,
      EqVendas_txt,
      TpOV,
      TpDevVenda,
      Cliente,
      NomeCliente,
      data_venc,
      Safra,
      Pgto_ant,
      tx_multa,
      tx_juros,
      CondPagto,
      DescCondPagto,
      DataCriacao,
      Moeda,
      Qtd,
      ValorLiq,
      ValorImp,
      ValorTotal,
      TotalRecebido,
      QtdFat,
      SaldoOV,
      SaldoOVUSD,
      SaldoJuros,
      SaldoJurosusd,
      SaldoRefUSD,
      SaldoRefBRL,
      SaldoRefFiltro,
      SaldoJurosFiltro,
            com_devolucao,
            Qtd_Devol,
            Vlr_Devol,
      
            case when TpDevVenda = 'V' then
      
                case Moeda when 'BRL' then
                  SaldoOV - Vlr_Devol
                else
                  SaldoOVUSD - Vlr_Devol
                end
      
            else
      
                case Moeda when 'BRL' then
                  SaldoOV + Vlr_Devol
                else
                  SaldoOVUSD + Vlr_Devol
                end
      
            end as Saldo_VlrDevol,
      SaldoDevolucao,
            Tolerancia,
            ToleranciaN

}
