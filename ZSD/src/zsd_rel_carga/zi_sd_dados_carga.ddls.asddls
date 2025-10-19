@AbapCatalog.sqlViewName: 'ZVRELCARGA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Solicitação - Cargas'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_SD_DADOS_CARGA
  as select distinct from zsdt0131                as Lote

    inner join            zsdt0129                as Carga      on Lote.nro_lote = Carga.nro_lote
    inner join            zsdt0133                as CabCarga   on CabCarga.nro_cg = Carga.nro_cg
    inner join            ZI_SD_STATUS_CARGA      as StatusCarg on StatusCarg.Status = CabCarga.status
    inner join            zsdt0130                as CliLote    on  Lote.nro_lote = CliLote.nro_lote
                                                                and Lote.nro_sol  = CliLote.nro_sol
                                                                and Lote.seq      = CliLote.seq

    left outer join       zsdt0134                as Rom        on  Lote.vbeln = Rom.vbeln
                                                                and Lote.posnr = Rom.posnr

    left outer join       zsdt0132                as Rota       on Rota.nr_rot = Lote.cod_loc_emb

    left outer join       ZI_SD_DADOS_ROM_NF      as NF         on  Lote.vbeln   = NF.Vbeln
                                                                and Lote.posnr   = NF.Posnr
                                                                and Carga.nro_cg = NF.NroCg
    left outer join       I_Supplier              as MotoTel    on Carga.motorista = MotoTel.Supplier

    left outer join       I_Supplier              as TranspDet  on Carga.motorista = TranspDet.Supplier

    left outer join       zsdt0410                as NfCarga    on  NfCarga.nro_cg   = Carga.nro_cg
                                                                and NfCarga.processo = '1'

    left outer join       zsdt0410                as NfCargaFo  on  NfCargaFo.nro_cg   = Carga.nro_cg
                                                                and NfCargaFo.processo = '2'

    left outer join       zib_nfe_dist_ter        as NFDist     on NFDist.chave_nfe = NfCargaFo.chave_nfe


    left outer join       zsdt0376                as CarrCD     on CarrCD.id_autorizacao_embarque = Carga.nro_cg
    left outer join       ZI_SD_ST_CG_SAI_INSUMOS as St         on St.nro_cg = Carga.nro_cg

{
  key Lote.nro_sol                         as NroSol,
  key Lote.nro_lote                        as NroLote,
  key Carga.nro_cg                         as Carga,
  key Lote.seq                             as Seq,
      //CabCarga.status                      as StatusCarga,
      //StatusCarg.Ddtext                    as DescStCarga,
      CliLote.seq_ent_cg                   as SeqEntrega,
      Lote.qtd_vinc                        as QtdVinc,
      Lote.um                              as Um,
      CabCarga.cod_transportadora          as Transp,
      TranspDet.SupplierFullName           as NomeTransp,
      Carga.motorista                      as Motorista,
      MotoTel.SupplierFullName             as NomeMotorista,
      MotoTel.PhoneNumber1                 as TelefoneMot,
      Carga.placa_cav                      as PlacaCavalo,
      Lote.local_embarq                    as LocalEmb,
      Rota.uf                              as Uf,
      ''                                   as StatusEntrega,
      ''                                   as LinkLocaliza,
      NF.Nf                                as Nf,
      CabCarga.data_atual                  as DataCarga,
      case CabCarga.integrar_carguero
        when ''
        then CabCarga.data_atual
        else CabCarga.dt_envio_cotacao end as DtCargaEnvCot,
      CabCarga.dt_frete_contratado         as DtFreteCont,
      CabCarga.dt_autorizacao_embarque     as DtAutEmb,
      NfCarga.date_create                  as DtTrocaNFFor,
      NFDist.dt_emissao                    as DtCarrFor,
      CarrCD.date_create                   as DtCarrCD,
      St.status                            as StatusCarga,
      St.ds_status                         as DescStCarga
}
where
  Lote.status <> 'X'
