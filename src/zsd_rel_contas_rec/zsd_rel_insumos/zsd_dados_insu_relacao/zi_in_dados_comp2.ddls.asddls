@AbapCatalog.sqlViewName: 'ZIINDADOSCOMP2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relação Complemento X Simulador'
define view ZI_IN_DADOS_COMP2
  as select from    vbak                 as vbak
    inner join      vbfa                 as vbfa on vbfa.vbeln = vbak.vbeln
    left outer join ZI_IN_DADOS_ZSDT0041 as z41  on z41.vbeln = vbfa.vbelv
    left outer join ZI_IN_DADOS_ZSDT0090_EX as z90  on z90.vbeln = vbfa.vbelv
    left outer join ZI_IN_DADOS_ZSDT0090 as zz90  on zz90.vbeln = vbfa.vbelv
{
  key case coalesce(z41.doc_simulacao,'') when '' then
    case coalesce(z90.doc_simulacao,'') when '' then //Rubenilson - 12.08.25
    zz90.doc_simulacao                               //Rubenilson - 12.08.25
    else                                             //Rubenilson - 12.08.25
    z90.doc_simulacao                                
    end                                              //Rubenilson - 12.08.25
  else
    z41.doc_simulacao
  end as doc_simulacao,

  key case coalesce(z41.vbeln_p,'') when '' then
  case coalesce(z90.vbeln_p,'') when '' then //Rubenilson - 12.08.25
    zz90.vbeln_p                             //Rubenilson - 12.08.25
    else                                     //Rubenilson - 12.08.25
    z90.vbeln_p
    end                                      //Rubenilson - 12.08.25
  else
    z41.vbeln_p
  end as vbeln_p,

  key vbak.vbeln

}
where
        vbak.auart   = 'ZCOP'
  and(
        vbfa.vbtyp_n = 'L'
    and vbfa.vbtyp_v = 'C'
  )
