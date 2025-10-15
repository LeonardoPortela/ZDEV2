*&---------------------------------------------------------------------*
*& Report  ZLESR0019
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zlesr0019.


tables zlest0006.


types: begin of ty_zlest0004,
         serie_despacho type zlest0004-serie_despacho,
         nr_despacho    type zlest0004-nr_despacho,
         nr_fatura      type zlest0004-nr_fatura,
         cgc_remetente  type zlest0004-cgc_remetente,
         cgc_dest       type zlest0004-cgc_dest,
         x(2)           type c,
       end of ty_zlest0004,

       begin of ty_j_1bbranch,
         branch type j_1bbranch-branch,
         stcd1  type j_1bbranch-stcd1 ,
       end of  ty_j_1bbranch,


       begin of ty_zlest0003,
         serie_despacho type zlest0003-serie_despacho,
         nr_despacho    type zlest0003-nr_despacho,
         nr_nf          type zlest0003-nr_nf,
         cgc_remetente  type zlest0035-cnpj,
         cgc_cliente    type zlest0006-cgc_cliente ,
       end of ty_zlest0003,

       begin of ty_zlest0003_sld                     ,
         serie_despacho type zlest0003-serie_despacho,
         nr_despacho    type zlest0003-nr_despacho   ,
         nr_nf          type zlest0003-nr_nf         ,
         cgc_remetente  type zlest0004-cgc_remetente ,
         cgc_cliente    type zlest0006-cgc_cliente   ,
         nr_frete       type zlest0006-nr_frete      ,
       end of ty_zlest0003_sld                       ,

       begin of ty_zlest0035_aux             ,
         nr_nf          type zlest0035-nr_nf     ,
         cnpj           type zlest0004-cgc_remetente,
         cgc_cliente    type zlest0006-cgc_cliente  ,
       end   of ty_zlest0035_aux             .



data: ti_zlest0006      type table of zlest0006,
      ti_zlest0004      type table of ty_zlest0004,
      ti_zlest0003      type table of ty_zlest0003,
      ti_zlest0035      type table of zlest0035,
      ti_zlest0035_aux2 type table of ty_zlest0035_aux  ,
      ti_j_1bbranch     type table of ty_j_1bbranch,
      ti_zlest0035_sld  type table of zlest0035,
      ti_zlest0003_sld  type table of ty_zlest0003_sld,
      ti_zlest0019_sld  type table of zlest0019,
      tl_zlest0019      type table of zlest0019,
      ti_zlest0003_fat  type table of ty_zlest0003_sld,
      ti_zlest0019_fat  type table of zlest0019.

data: wa_zlest0006      type zlest0006,
      wa_zlest0004      type ty_zlest0004,
      wa_zlest0003      type ty_zlest0003,
      wa_zlest0035      type zlest0035,
      wa_zlest0035_aux2 type ty_zlest0035_aux  ,
      wa_j_1bbranch     type ty_j_1bbranch,
      wa_zlest0035_sld  type zlest0035,
      wa_zlest0003_sld  type ty_zlest0003_sld,
      wa_zlest0019_sld  type zlest0019,
      wa_zlest0019      type zlest0019,
      wa_zlest0003_fat  type ty_zlest0003_sld,
      wa_zlest0004_fat  type ty_zlest0004,
      wa_zlest0006_fat  type zlest0006,
      wa_zlest0019_fat  type zlest0019.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
selection-screen: begin of block b1 with frame title text-001.

select-options: p_nfall for  zlest0006-nr_nf_all no intervals no-extension  obligatory.
parameters : p_cnpj  type zlest0006-cgc_all obligatory.

selection-screen: end of block b1.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
perform : f_seleciona_dados,
          f_atualiza_saldo .


*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_seleciona_dados .

  perform z_seleciona_zlest0006.
  perform z_seleciona_zlest0004.
  perform z_seleciona_zlest0003.
  perform z_seleciona_zlest0035.
  perform z_seleciona_nfs.



endform.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0006
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_seleciona_zlest0006 .
  refresh ti_zlest0006.

  select *
    from zlest0006
    into table ti_zlest0006
  where status    eq 'L'
    and nr_nf_all in p_nfall
    and cgc_all   eq p_cnpj.

  if not sy-subrc is initial .

    message i000(z01) with 'Não há faturas para atualizar Saldo !' .
    exit.

  endif.

  sort ti_zlest0006 by nr_fatura.

endform.                    " Z_SELECIONA_ZLEST0006

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_seleciona_zlest0004 .



  data tl_zlest0006 type table of zlest0006.

  refresh ti_zlest0004.

  check not ti_zlest0006[] is initial.
  tl_zlest0006[] = ti_zlest0006[].
  delete adjacent duplicates from tl_zlest0006 comparing nr_fatura.

  select serie_despacho
         nr_despacho
         nr_fatura
         cgc_remetente
         cgc_dest
    from zlest0004
    into table ti_zlest0004
    for all entries in tl_zlest0006
  where nr_fatura     eq tl_zlest0006-nr_fatura.

  sort ti_zlest0004 by serie_despacho nr_despacho.
endform.                    " Z_SELECIONA_ZLEST0004

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_seleciona_zlest0003 .

  data tl_zlest0004 type table of ty_zlest0004.

  refresh ti_zlest0003.

  check not ti_zlest0004[] is initial.

  tl_zlest0004[] = ti_zlest0004[].

  delete adjacent duplicates from tl_zlest0004
    comparing serie_despacho nr_despacho.

  select z~serie_despacho
         z~nr_despacho
         z~nr_nf
         e~cgc_remetente
         d~cgc_cliente
    from zlest0003 as z
    inner join zlest0004 as e on z~serie_despacho eq e~serie_despacho and z~nr_despacho eq  e~nr_despacho
    inner join zlest0006 as d on d~nr_fatura eq e~nr_fatura
    into table ti_zlest0003
    for all entries in tl_zlest0004
  where z~serie_despacho eq tl_zlest0004-serie_despacho
    and z~nr_despacho    eq tl_zlest0004-nr_despacho.

endform.                    " Z_SELECIONA_ZLEST0003

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0035
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_seleciona_zlest0035 .

  data : sl_zlest0035_aux2 type ty_zlest0035_aux,
         sl_zlest0003      type ty_zlest0003_sld,
         sl_zlest0019      type zlest0019,
         vl_branch         type j_1bbranch-branch.

  refresh : ti_zlest0035, ti_zlest0003_sld, ti_zlest0019_sld, ti_zlest0035_aux2, ti_j_1bbranch.

  check not ti_zlest0003[] is initial.

  read table ti_zlest0006 index 1 into wa_zlest0006.

  select single branch
    from j_1bbranch
    into vl_branch
   where stcd1 eq wa_zlest0006-cgc_cliente.

  select *
    from zlest0035
    into table ti_zlest0035
    for all entries in ti_zlest0003
  where nr_nf eq ti_zlest0003-nr_nf
    and cnpj  eq ti_zlest0003-cgc_remetente
    and werks eq vl_branch.

  select branch stcd1
    from j_1bbranch
    into table ti_j_1bbranch
     for all entries in ti_zlest0035
   where branch eq ti_zlest0035-werks.

  sort ti_j_1bbranch by branch .

  loop at ti_zlest0035 into wa_zlest0035 .

    sl_zlest0035_aux2-nr_nf = wa_zlest0035-nr_nf.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_zlest0035-cnpj
      importing
        output = sl_zlest0035_aux2-cnpj.

    read table ti_j_1bbranch into wa_j_1bbranch with key branch = wa_zlest0035-werks binary search.

    sl_zlest0035_aux2-cgc_cliente = wa_j_1bbranch-stcd1.

    append sl_zlest0035_aux2 to ti_zlest0035_aux2.
  endloop.

  select z~serie_despacho
         z~nr_despacho
         z~nr_nf
         e~cgc_remetente
         d~cgc_cliente
         d~nr_frete
    from zlest0003 as z
    inner join zlest0004 as e on z~serie_despacho eq e~serie_despacho and z~nr_despacho eq  e~nr_despacho
    inner join zlest0006 as d on d~nr_fatura eq e~nr_fatura
    into table ti_zlest0003_sld
    for all entries in ti_zlest0035_aux2
    where z~nr_nf          eq ti_zlest0035_aux2-nr_nf
      and e~cgc_remetente  eq ti_zlest0035_aux2-cnpj
      and d~cgc_cliente    eq ti_zlest0035_aux2-cgc_cliente.

  loop at ti_zlest0003_sld into sl_zlest0003.

    sl_zlest0019-seriedcl = sl_zlest0003-serie_despacho.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = sl_zlest0003-nr_despacho
      importing
        output = sl_zlest0019-dcl.

    append sl_zlest0019 to tl_zlest0019.

    clear: sl_zlest0003, sl_zlest0019.
  endloop.

  select *
    from zlest0019
    into table ti_zlest0019_sld
    for all entries in tl_zlest0019
  where dcl      eq tl_zlest0019-dcl
    and seriedcl eq tl_zlest0019-seriedcl
    and idinter  eq 'L2'
    and tp_reg   eq '30'.



*  select *
*    from .
*
*
*   select *
*    from zlest0035
*    into table ti_zlest0035
*    for all entries in ti_zlest0003
*  where nr_nf eq ti_zlest0003-nr_nf
*    and cnpj  eq ti_zlest0003-cgc_remetente
*    and werks eq vl_branch.



endform.                    " Z_SELECIONA_ZLEST0035
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_atualiza_saldo .

  data: vl_cnpj     type zlest0004-cgc_remetente,
        vl_dcl_fat  type zlest0019-dcl,
        vl_qtd_sld  type zlest0035-qtd_cheg.

  sort: ti_zlest0035     by nr_nf serie_nf cnpj,
        ti_j_1bbranch    by branch,
        ti_zlest0003_fat by nr_nf cgc_remetente cgc_cliente,
        ti_zlest0019_fat by dcl seriedcl.

  loop at ti_zlest0035 into wa_zlest0035  .

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_zlest0035-cnpj
      importing
        output = vl_cnpj.

    read table ti_j_1bbranch into wa_j_1bbranch with key  branch = wa_zlest0035-werks binary search.

    vl_qtd_sld = wa_zlest0035-qtd_cheg.

    loop at ti_zlest0003_fat into wa_zlest0003_fat where nr_nf         = wa_zlest0035-nr_nf and
                                                         cgc_remetente = vl_cnpj and
                                                         cgc_cliente   = wa_j_1bbranch-stcd1.

      if wa_zlest0003_fat-nr_frete is not initial.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = wa_zlest0003_fat-nr_despacho
          importing
            output = vl_dcl_fat.

        loop at ti_zlest0019_fat into wa_zlest0019_fat where dcl = vl_dcl_fat and
                                                             seriedcl = wa_zlest0003_fat-serie_despacho and
                                                             nfenum = wa_zlest0003_fat-nr_nf.

          vl_qtd_sld := vl_qtd_sld - wa_zlest0019_fat-pesodvagao.

        endloop.

      endif.

    endloop.

    wa_zlest0035_sld-nr_nf      = wa_zlest0035-nr_nf   .
    wa_zlest0035_sld-serie_nf   = wa_zlest0035-serie_nf.
    wa_zlest0035_sld-cnpj       = wa_zlest0035-cnpj    .
    wa_zlest0035_sld-docnum     = wa_zlest0035-docnum  .
    wa_zlest0035_sld-werks      = wa_zlest0035-werks   .
    wa_zlest0035_sld-qtd_nf     = wa_zlest0035-qtd_nf  .
    wa_zlest0035_sld-qtd_cheg   = wa_zlest0035-qtd_cheg.
    wa_zlest0035_sld-dtachegada = wa_zlest0035-dtachegada.
    wa_zlest0035_sld-saldo      = vl_qtd_sld.

    append wa_zlest0035_sld to ti_zlest0035_sld.

    clear wa_zlest0035_sld.

  endloop.

  modify zlest0035 from table ti_zlest0035_sld.

  message i000(z01) with 'Saldo Atualizado !' .

endform.                    " F_ATUALIZA_SALDO
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_NFS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_seleciona_nfs .

  refresh : ti_zlest0003_fat.

  select z~serie_despacho
         z~nr_despacho
         z~nr_nf
         e~cgc_remetente
         d~cgc_cliente
         d~nr_frete
    from zlest0003 as z
    inner join zlest0004 as e on z~serie_despacho eq e~serie_despacho and z~nr_despacho eq  e~nr_despacho
    inner join zlest0006 as d on d~nr_fatura eq e~nr_fatura
    into table ti_zlest0003_fat
    for all entries in ti_zlest0003_sld
    where z~nr_nf          eq ti_zlest0003_sld-nr_nf
      and e~cgc_remetente  eq ti_zlest0003_sld-cgc_remetente
      and d~cgc_cliente    eq ti_zlest0003_sld-cgc_cliente.


  if sy-subrc is initial.

    refresh : tl_zlest0019,
              ti_zlest0019_fat .

    loop at ti_zlest0003_fat into wa_zlest0003_fat.

      wa_zlest0019-seriedcl = wa_zlest0003_fat-serie_despacho.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_zlest0003_fat-nr_despacho
        importing
          output = wa_zlest0019-dcl.
      append wa_zlest0019 to tl_zlest0019.

      clear: wa_zlest0003_fat,
             wa_zlest0019.
    endloop.

    select *
      from zlest0019
      into table ti_zlest0019_fat
      for all entries in tl_zlest0019
    where dcl      eq tl_zlest0019-dcl
      and seriedcl eq tl_zlest0019-seriedcl
      and idinter  eq 'L2'
      and tp_reg   eq '30'.

    delete ti_zlest0019_fat where not nfnum is initial.

  endif.


endform.                    " Z_SELECIONA_NFS
