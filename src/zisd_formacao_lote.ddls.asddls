@AbapCatalog.sqlViewName: 'ZVSDFORMACAOLOTE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'União ZSDTPROD_FLOTE e ZSDTPROD_FLOTE'
@Metadata.ignorePropagatedAnnotations: true
define view ZISD_FORMACAO_LOTE(

    docnum,
    itmnum,
    werks,
    sequencial,
    grupoMercadoria,
    werksReal,
    dataEmissao,
    material,
    qtdNotaFiscal,
    saldoVinculado,
    saldoDisponivel,
    saldoNaoDisponivel,
    compraFimEspecifico,
    romaneioCompleto,
    eudr,
    entradaTransferencia,
    usuarioCriacao,
    dataCriacao,
    horaCriacao,
    manual,
    cancel,
    usuarioCancel,
    dataCancel,
    horaCancel

  )
  as select from zsdtflote_flote as _flote_1
{

  key  _flote_1.docnum               as Docnum,
  key  _flote_1.itmnum               as Itmnum,
  key  _flote_1.werks                as Werks,
  key  _flote_1.seq                  as sequencial,
       ''                            as grupoMercadoria,
       _flote_1.werks_real           as WerksReal,
       _flote_1.data_emissao         as DataEmissao,
       _flote_1.material             as Material,
       _flote_1.qtd_nf               as QtdNotaFiscal,
       _flote_1.saldo_vinc           as SaldoVinculado,
       _flote_1.saldo_disponivel     as SaldoDisponivel,
       _flote_1.saldo_nao_disponivel as SaldoNaoDisponivel,
       _flote_1.compra_fim_es        as CompraFimEspecifico,
       ''                            as RomaneioCompleto,
       _flote_1.eudr                 as Eudr,
       ''                            as EntradaTransferencia,
       _flote_1.us_criacao           as usuarioCriacao,
       _flote_1.dt_criacao           as DataCriacao,
       _flote_1.hr_criacao           as HoraCriacao,
       ''                            as Manual,
       _flote_1.cancel               as Cancel,
       _flote_1.us_cancel            as usuarioCancel,
       _flote_1.dt_cancel            as DataCancel,
       _flote_1.hr_cancel            as HoraCancel

}
union select from zsdtprod_flote as _flote_2
{

  key _flote_2.docnum               as Docnum,
  key _flote_2.itmnum               as Itmnum,
  key _flote_2.werks                as Werks,
  key _flote_2.seq                  as Sequencial,

      _flote_2.matkl                as grupoMercadoria,
      _flote_2.werks_real           as WerksReal,
      _flote_2.data_emissao         as DataEmissao,
      _flote_2.material             as Material,
      _flote_2.qtd_nf               as QtdNotafiscal,
      _flote_2.saldo_vinc           as SaldoVinculado,
      _flote_2.saldo_disponivel     as SaldoDisponivel,
      _flote_2.saldo_nao_disponivel as SaldoNaoDisponivel,
      _flote_2.compra_fim_es        as CompraFimEspecifico,
      _flote_2.romaneio_completo    as RomaneioCompleto,
      _flote_2.eudr                 as Eudr,
      _flote_2.entrada_transf       as EntradaTransferencia,
      _flote_2.us_criacao           as UsuarioCriacao,
      _flote_2.dt_criacao           as DataCriacao,
      _flote_2.hr_criacao           as HoraCriacao,
      _flote_2.manual               as Manual,
      _flote_2.cancel               as Cancel,
      _flote_2.us_cancel            as UsuarioCancel,
      _flote_2.dt_cancel            as DataCancel,
      _flote_2.hr_cancel            as HoraCancel

}
