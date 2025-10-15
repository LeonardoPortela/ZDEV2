@AbapCatalog.sqlViewName: 'ZVINOVDADOS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Basicos para OV'
define view ZI_IN_OV_DADOS
  as select from    vbak
    inner join      ZI_IN_OV_DADOS_ITEM        as vbap  on vbak.vbeln = vbap.vbeln
    inner join      ZI_IN_OV_VBKD              as vbkd  on vbkd.vbeln = vbak.vbeln
  //inner join      vbkd                       as vbkd  on  vbkd.vbeln = vbak.vbeln
  //                                                    and vbkd.posnr = '000000'
    inner join      I_Customer                 as kna1  on vbak.kunnr = kna1.Customer
    inner join      I_CustomerPaymentTermsText as t052u on  t052u.CustomerPaymentTerms = vbkd.zterm
                                                        and t052u.Language             = $session.system_language

    left outer join tvgrt                               on  tvgrt.vkgrp = vbak.vkgrp
                                                        and tvgrt.spras = $session.system_language

    left outer join tvarvc                     as Dev   on  Dev.name = 'ZSDT0060_TP_OV_DEVOLUCAO'
                                                        and Dev.low  = vbak.auart

{
  key vbak.vbeln,
      vbak.bukrs_vf                  as bukrs,
      vbak.vkbur,
      vbak.vkgrp,
      tvgrt.bezei                    as vkgrp_name,
      vbak.auart,
      kna1.Customer                  as kunnr,
      kna1.CustomerName              as name1,
      t052u.CustomerPaymentTerms     as zterm,
      t052u.CustomerPaymentTermsName as zterm_name,
      vbak.erdat,
      vbap.waerk,
      vbkd.kurrf,
      vbkd.valdt,
      vbak.vgbel,
      vbak.vtweg,
      vbak.spart,
      case coalesce(Dev.low,'') when '' then
        1
      else
        -1
      end                            as fator_devol,

      vbap.kwmeng,
      vbap.zmeng,
      vbap.netwr,
      vbap.mwsbp,
      (vbap.netwr + vbap.mwsbp )     as vlr_tot_ov

}
