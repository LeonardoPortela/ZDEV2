@AbapCatalog.sqlViewName: 'ZVMIOVTPVEC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca tipo de vencimento OV'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_TP_VENC
  as select distinct from vbak as Ordem

    inner join   vbkd as Com   on Ordem.vbeln = Com.vbeln
    inner join   t052 as TpCom on Com.zterm = TpCom.zterm

{
  key Ordem.vbeln as Ordem,
      case TpCom.zdart
      when 'B'
        then 'F' //Vencimento por fatura
      else
        'V' //Vencimento por OV
      end         as TipoVencimento
}
