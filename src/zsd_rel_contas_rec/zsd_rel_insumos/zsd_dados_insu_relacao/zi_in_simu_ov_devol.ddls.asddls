@AbapCatalog.sqlViewName: 'ZIINSIMUOVDEVOL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relação Devolução X Simulador'
define view ZI_IN_SIMU_OV_DEVOL
  as select distinct from ZI_IN_OV_DADOS          as vbak
    inner join            vbfa                    as vbfa  on  vbak.vgbel   = vbfa.vbeln
                                                           and vbfa.vbtyp_n = 'M'
                                                           and vbfa.vbtyp_v = 'C'
                                                           and vbfa.stufe   = '01'
    left outer join       ZI_IN_DADOS_ZSDT0041    as z41   on z41.vbeln = vbfa.vbelv
    left outer join       ZI_IN_DADOS_ZSDT0090    as z90   on z90.vbeln = vbfa.vbelv
    left outer join       ZI_IN_DADOS_ZSDT0090_EX as z90_2 on z90_2.vbeln = vbfa.vbelv
{
  key case coalesce(z41.doc_simulacao,'') when '' then


  case coalesce(z90.doc_simulacao,'') when '' then
    z90_2.doc_simulacao
  else
    z90.doc_simulacao
  end

  else

    case coalesce(z41.doc_simulacao,'') when '' then
      z90_2.doc_simulacao
    else
        z41.doc_simulacao
    end

  end as doc_simulacao,

  key case coalesce(z41.vbeln_p,'') when '' then

    case coalesce(z90.vbeln_p,'') when '' then
        z90_2.vbeln_p
    else
        z90.vbeln_p
    end
  else
    case coalesce(z41.vbeln_p,'') when '' then
       z90_2.vbeln_p
    else
        z41.vbeln_p
    end

  end as vbeln_p,

  key vbak.vbeln
}
where
  vbak.fator_devol = -1
