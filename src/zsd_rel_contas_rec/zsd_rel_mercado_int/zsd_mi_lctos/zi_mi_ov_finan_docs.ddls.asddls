@AbapCatalog.sqlViewName: 'ZVOVFINANDOCS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Unificar todos os doc financeiros da OV'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FINAN_DOCS
  as select from    ZI_MI_OV_FAT_PROC as Fat
    inner join      ZI_MI_OV_TP_VENC  as Ordem   on Fat.Ordem = Ordem.Ordem
    left outer join      acdoca            as DocCont on  DocCont.rldnr = '0L'
                                                 and DocCont.awref = Fat.Faturamento
                                                 and DocCont.bschl = '01'
    left outer join j_1bnflin         as NfeIt   on NfeIt.refkey = Fat.Faturamento
    left outer join j_1bnfdoc         as NfeDoc  on NfeDoc.docnum = NfeIt.docnum
{
  key Ordem.Ordem,

  key Fat.Faturamento as Faturamento,

  key '0000000000'    as DocCont,
  key '0000000000'    as DocComp,


      '0000000000'    as id26,


      DocCont.rfccur  as moeda,

      DocCont.ksl     as ValorRecUSD,

      DocCont.hsl     as ValorRec,

      Fat.dataVenc    as DtVenc,

      '        '      as DtPagto,

      NfeDoc.docnum   as docnum,

      case when TipoVencimento = 'F' then
        NfeDoc.nfenum
      else
        '         '
      end             as NFE,

      case when TipoVencimento = 'F' then
        NfeIt.netwr
      else
        0
      end             as Vlr_NFE,

      0               as Ptax,

      ''              as ForPagto,

      0               as vlr_multa_calc,

      0               as vlr_juros_calc,

      0               as vlr_multa_rbdo,

      0               as vlr_juros_rbdo,

      0               as vlr_desc_mult,

      0               as vlr_desc_jros,

      ''              as observacao,

      0               as SaldoJurosusd,

      0               as SaldoJuros,

      0               as Mont_Calc,

      0 //Fat.Valor
             as ValorFatRef,

      Fat.qtd         as QtdFatRef,

      Fat.Valor       as VlrTotRef,

      0               as VlrTotRef26,

      TipoVencimento
}
