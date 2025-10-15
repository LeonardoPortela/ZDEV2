function zplancomp_reenvio_nota.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_DOCNUM) TYPE  J_1BDOCNUM
*"     REFERENCE(P_NOTASE) TYPE  ZNOM_REME_NOTASE
*"----------------------------------------------------------------------

  data: it_notase    type table of znom_reme_notase with header line,
        it_notas     type table of znom_reme_notas  with header line,
        it_remessa   type table of znom_prog_reme   with header line,
        it_notas_pro type table of zdoc_nf_produtor with header line,
        wa_notas     type znom_reme_notas,
        wa_j_1bnflin type j_1bnflin.

  select * into table it_notase
    from znom_reme_notase
   where model  eq p_notase-model
     and series eq p_notase-series
     and nfnum  eq p_notase-nfnum
     and parid  eq p_notase-parid
     and nfe    eq p_notase-nfe
     and nfenum eq p_notase-nfenum.

  check sy-subrc is initial.

  select * into table it_notas
    from znom_reme_notas
     for all entries in it_notase
   where docnum eq it_notase-docnum
     and itmnum eq it_notase-itmnum.

  select single * into wa_j_1bnflin
    from j_1bnflin
   where docnum eq p_docnum.

  check sy-subrc is initial.

  loop at it_notas into wa_notas.

    update znom_reme_notas
       set docnum = wa_j_1bnflin-docnum
           itmnum = wa_j_1bnflin-itmnum
     where id_nomeacao_tran eq wa_notas-id_nomeacao_tran
       and id_empresa       eq wa_notas-id_empresa
       and id_filial        eq wa_notas-id_filial
       and id_material      eq wa_notas-id_material
       and id_remetente     eq wa_notas-id_remetente
       and docnum           eq wa_notas-docnum
       and itmnum           eq wa_notas-itmnum.

  endloop.

  select * into table it_notas_pro
    from zdoc_nf_produtor
     for all entries in it_notase
   where docnum_prod eq it_notase-docnum
     and itmnum_prod eq it_notase-itmnum.

  loop at it_notas_pro.

    update zdoc_nf_produtor
       set docnum_prod = wa_j_1bnflin-docnum
           itmnum_prod = wa_j_1bnflin-itmnum
     where docnum_prod eq it_notas_pro-docnum_prod
       and itmnum_prod eq it_notas_pro-itmnum_prod.

  endloop.

endfunction.
