@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Lista cargas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZCDS_LISTA_CARGAS
  as select from    zmmt0201           as header
    left outer join zmmt0202           as itens          on  header.nro_cg = itens.nro_cg
                                                         and itens.cancel  = ''
    left outer join zsdt0391           as frete          on frete.nro_cg = header.nro_cg
    left outer join lfa1               as transportadora on transportadora.lifnr = header.cod_transportadora
    left outer join lfa1               as pc             on pc.lifnr = header.ponto_coleta
    left outer join lfa1               as le             on le.lifnr = header.local_entrega
    left outer join zmmt0196           as solic          on  solic.nro_sol = header.nro_sol
                                                         and solic.seq     = '001'
    left outer join ekko               as ekko           on ekko.ebeln = solic.ebeln
    left outer join ekpo               as pedido         on  pedido.ebeln = solic.ebeln
                                                         and pedido.ebelp = solic.ebelp

    left outer join ekpo               as pedido_sol     on  pedido_sol.ebeln = itens.ebeln
                                                         and pedido_sol.ebelp = itens.ebelp

    left outer join makt               as makt           on  makt.matnr = itens.matnr
                                                         and makt.spras = 'P'
    left outer join ZI_MM_STATUS_CARGA as status         on header.nro_cg = status.nro_cg


{
  key header.nro_cg                  as NroCg,
  key itens.item_carga               as ItemCarga,
      //key notas.chave_nfe                as ChaveNfe,
      header.viagem_id               as ViagemId,
      header.nro_sol                 as NroSol,
      header.bukrs                   as bukrs,
      header.cod_transportadora      as CodTransportadora,
      transportadora.name1           as desc_transp,
      header.cod_motorista           as CodMotorista,
      header.nome_motorista          as NomeMotorista,
      header.placa_cav               as PlacaCav,
      header.placa_car1              as PlacaCar1,
      header.placa_car2              as PlacaCar2,
      header.placa_car3              as PlacaCar3,
      header.dt_prevista_embarque    as DtPrevistaEmbarque,
      header.inco1                   as Inco1,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      header.qtd_total_kg            as QtdTotalKg,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      header.qtd_total_kg_efetivado  as QtdTotalKgEfetivado,

      case 
        when coalesce(pedido_sol.meins,'') <> '' then
           pedido_sol.meins
        else
           pedido.meins
      end                            as unidade,

      header.ponto_coleta            as PontoColeta,
      pc.name1                       as desc_pc,
      header.local_entrega           as LocalEntrega,
      le.name1                       as desc_le,
      header.transf_no_fornecedor    as TransfNoFornecedor,
      header.dt_autorizacao_embarque as DtAutorizacaoEmbarque,
      header.motivo_cancelamento     as MotivoCancelamento,
      header.cancel_parcial          as CancelParcial,
      header.user_create             as UserCreate,
      header.date_create             as DateCreate,
      header.time_create             as TimeCreate,
      header.user_change             as UserChange,
      header.date_change             as DateChange,
      header.time_change             as TimeChange,
      header.user_cancel             as UserCancel,
      header.date_cancel             as DateCancel,
      header.time_cancel             as TimeCancel,
      header.cancel                  as Cancel,
      status.status                  as status,
      itens.nro_sol                  as nrosol_item,
      itens.seq                      as Seq,
      itens.tp_saldo_vinc            as tp_saldo_vinc,
      itens.ebeln                    as Ebeln,
      itens.ebelp                    as Ebelp,
      ekko.bsart                     as tipo_pedido,
      pedido.matkl                   as matkl,
      itens.matnr                    as Matnr,
      makt.maktx                     as desc_material,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      itens.qtd_vinc_carga           as QtdVincCarga,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      itens.qtd_vinc_carga_kg        as QtdVincCargaKg,
      itens.user_create              as ItemUserCreate,
      itens.date_create              as ItemDateCreate,
      itens.time_create              as ItemTimeCreate,
      itens.user_change              as ItemUserChange,
      itens.date_change              as ItemDateChange,
      itens.time_change              as ItemTimeChange,
      itens.user_cancel              as ItemUserCancel,
      itens.date_cancel              as ItemDateCancel,
      itens.time_cancel              as ItemTimeCancel,
      itens.cancel                   as ItemCancel,
      frete.modalidade_pag_frete     as modalidade_frete,
      @Semantics.amount.currencyCode: 'moeda'
      frete.preco_total_frete        as preco_frete,
      frete.moeda                    as moeda,
      frete.dt_contratacao_frete     as dt_contrat_frete,
      frete.dt_cotacao_frete         as dt_cotacao_frete,
      frete.user_create              as freteusercreate,
      frete.date_create              as fretedatecreate,
      frete.time_create              as freatetimecreate,
      frete.user_change              as freteuserchange,
      frete.date_change              as fretedatachange,
      frete.time_change              as fretetimechange,
      frete.user_cancel              as freteusercancel,
      frete.date_cancel              as fretedatecancel,
      frete.time_cancel              as fretetimecancel,
      frete.cancel                   as fretecancel


}
where
  header.cancel = ''
