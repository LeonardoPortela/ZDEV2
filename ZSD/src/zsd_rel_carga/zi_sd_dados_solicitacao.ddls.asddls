@AbapCatalog.sqlViewName: 'ZVRELSOLI'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Solicitação - Relatorio Carga'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_SD_DADOS_SOLICITACAO
  as select from    zsdt0082             as Sol

    inner join      I_SalesOrder         as Ordem    on Sol.vbeln = Ordem.SalesOrder
    inner join      I_SalesOrderItem     as OrdemIt  on  Sol.vbeln = OrdemIt.SalesOrder
                                                     and Sol.posnr = OrdemIt.SalesOrderItem
    inner join      I_MaterialText       as Material on  OrdemIt.Material  = Material.Material
                                                     and Material.Language = 'P'
    inner join      I_Customer           as Cliente  on Ordem.SoldToParty = Cliente.Customer

    inner join      ZI_SD_QTD_VINC_CARGA as QtdVinc  on  Sol.nro_sol       = QtdVinc.NroSol
                                                     and Sol.vbeln         = QtdVinc.Vbeln
                                                     and Sol.posnr         = QtdVinc.Posnr
                                                     and Ordem.SoldToParty = QtdVinc.Kunnr
    left outer join zsdt0132             as Rota     on Sol.nr_rot = Rota.nr_rot
{
  key Sol.nro_sol                   as NroSol,
      Sol.dt_sol                    as DtSol,
      Sol.vbeln                     as Vbeln,
      Sol.posnr                     as Posnr,
      Sol.nro_sol_origem            as NroSolOrigem,
      Ordem.SoldToParty             as Cliente,
      Cliente.CustomerName          as NomeCliente,
      Cliente.Region                as UFCliente,
      OrdemIt.Material              as Material,
      Material.MaterialName         as NomeMaterial,
      Sol.nr_rot                    as NrRota,
      Rota.uf                       as UF,
      Rota.rot_desc                 as DescRota,
      Sol.qte_sol                   as QtdSoli,
      @Semantics.quantity.unitOfMeasure: 'UM'
      QtdVinc.QtdVinc               as QtdVinc,
      QtdVinc.Um                    as UM,
      Sol.qte_sol - QtdVinc.QtdVinc as SaldoCarga,
      Sol.dt_entrega                as DataEntrega

}

where
  Sol.seq = '001'
