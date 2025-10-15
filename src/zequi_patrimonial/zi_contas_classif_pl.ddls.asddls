@AbapCatalog.sqlViewName: 'ZVCONTAEQPL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Contas Classificação PL'
define view ZI_CONTAS_CLASSIF_PL
  as select distinct from ska1              as Conta
    left outer join       skat              as _DesConta      on  Conta.saknr     = _DesConta.saknr
                                                              and _DesConta.ktopl = '0050'
                                                              and _DesConta.spras = $session.system_language

    inner join            ZI_COD_CLASSIF_PL as _RecContab     on Conta.saknr = _RecContab.Saknr
    left outer join       zglt039           as _DescRecContab on  _RecContab.CodClasBal  = _DescRecContab.codigo
                                                              and _RecContab.CodClasNot2 = _DescRecContab.cod_nota

{

  key Conta.saknr               as Conta,
      Conta.ktoks               as GrpContas,
      _DesConta.txt50           as DescConta,
      _RecContab.CodClasBal     as ClassBal,
      _RecContab.CodClasNot2    as ClassNota,
      _DescRecContab.descr      as DescClassBal,
      _DescRecContab.descr_nota as DescClassNota


}
where
      Conta.ktopl = '0050'
  and Conta.ktoks = 'YB02'
