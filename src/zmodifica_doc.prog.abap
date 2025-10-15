*&---------------------------------------------------------------------*
*& Report  ZMODIFICA_DOC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMODIFICA_DOC.

tables j_1bnfdoc.

data : it_j_1bnfdoc      type table of j_1bnfdoc,
       it_j_1bnfdoc_aux  type table of j_1bnfdoc,
       it_j_1bnflin      type table of j_1bnflin,
       it_J_1BNFE_ACTIVE type table of J_1BNFE_ACTIVE.

data : wa_j_1bnfdoc      type j_1bnfdoc,
       wa_j_1bnflin      type j_1bnflin,
       wa_J_1BNFE_ACTIVE type J_1BNFE_ACTIVE.


selection-screen: begin of block b1 with frame title text-001.

  select-options: p_docdat for j_1bnfdoc-docdat obligatory default '20101124' .

selection-screen: end of block b1.

start-of-selection.

  perform:  f_seleciona_dados,
            f_saida          .


end-of-selection.


form f_seleciona_dados.


  select *
    into table it_j_1bnfdoc
    from j_1bnfdoc
    where DIRECT = '1'
      and MODEL  = '57'
      and  DOCDAT in p_docdat.
      "and DOCDAT >= '20101124' .


*  select *
*    into table it_j_1bnfdoc
*    from j_1bnfdoc
*    where DIRECT = '1'
*      and MODEL  = '57'
*      and BRANCH = '0116'
*      and PARID  = '0000000116'.

  select *
    from j_1bnflin
    into table it_j_1bnflin
    for all entries in it_j_1bnfdoc
    where docnum = it_j_1bnfdoc-docnum.

  select *
    from J_1BNFE_ACTIVE
    into table it_J_1BNFE_ACTIVE
    for all entries in it_j_1bnfdoc
    where docnum = it_j_1bnfdoc-docnum.

endform.


form f_saida .
  sort: it_j_1bnfdoc by docnum,
        it_j_1bnflin by docnum.

  data parid type c length 4.

  LOOP AT it_j_1bnfdoc into wa_j_1bnfdoc.

    parid = wa_j_1bnfdoc-PARID+6(4).

    if parid = wa_j_1bnfdoc-BRANCH.

       read table it_j_1bnflin into wa_j_1bnflin with key docnum = wa_j_1bnfdoc-docnum.

       wa_j_1bnfdoc-BRANCH = wa_j_1bnflin-WERKS.

       modify j_1bnfdoc from wa_j_1bnfdoc .

*   ---------------------------------------------------------------------------------------------

       read table it_J_1BNFE_ACTIVE into wa_J_1BNFE_ACTIVE with key docnum = wa_j_1bnfdoc-docnum.

       wa_J_1BNFE_ACTIVE-BRANCH = wa_j_1bnflin-WERKS.

       modify J_1BNFE_ACTIVE from wa_J_1BNFE_ACTIVE .

    endif.

    clear : wa_j_1bnfdoc, wa_j_1bnflin.
  ENDLOOP.
  commit work.
endform.
