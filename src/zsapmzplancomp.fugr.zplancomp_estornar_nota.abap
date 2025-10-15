function zplancomp_estornar_nota.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_DOCNUM) TYPE  J_1BDOCNUM
*"     REFERENCE(P_ITMNUM) TYPE  J_1BITMNUM
*"     REFERENCE(P_VERIFICA_PLANEJA) TYPE  CHAR01 OPTIONAL
*"  EXCEPTIONS
*"      DOC_NAO_PLANEJADO
*"----------------------------------------------------------------------

  data: it_znom_reme_notas  type table of znom_reme_notas with header line,
        wa_znom_reme_notase type znom_reme_notase,
        wa_j_1bnfdoc        type j_1bnfdoc.

  check not p_docnum is initial.

  select single * into wa_j_1bnfdoc
    from j_1bnfdoc
   where docnum eq p_docnum.

  check sy-subrc is initial.

  select * into table it_znom_reme_notas
    from znom_reme_notas
   where docnum eq p_docnum
     and itmnum eq p_itmnum.

  if not sy-subrc is initial.
    message e051 with p_docnum raising doc_nao_planejado.
  endif.

  check p_verifica_planeja is initial.

  "Registra Estorno de documento
  loop at it_znom_reme_notas.
    move-corresponding it_znom_reme_notas to wa_znom_reme_notase.
    wa_znom_reme_notase-model  = wa_j_1bnfdoc-model.
    wa_znom_reme_notase-series = wa_j_1bnfdoc-series.
    wa_znom_reme_notase-nfnum  = wa_j_1bnfdoc-nfnum.
    wa_znom_reme_notase-parid  = wa_j_1bnfdoc-parid.
    wa_znom_reme_notase-nfe    = wa_j_1bnfdoc-nfe.
    wa_znom_reme_notase-nfenum = wa_j_1bnfdoc-nfenum.
    modify znom_reme_notase from wa_znom_reme_notase.
  endloop.

endfunction.
