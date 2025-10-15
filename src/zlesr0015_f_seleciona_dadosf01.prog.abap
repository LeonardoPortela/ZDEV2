
types: begin of ty_zlest0004,
         serie_despacho type zlest0004-serie_despacho,
         nr_despacho    type zlest0004-nr_despacho,
         nr_fatura      type zlest0004-nr_fatura,
         cgc_remetente  type zlest0004-cgc_remetente,
         x(2)           type c,
       end of ty_zlest0004.

*----------------------------------------------------------------------*
***INCLUDE ZLESR0015_F_SELECIONA_DADOSF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA_NR_NF  text
*      -->P_WA_SAIDA_CNPJ  text
*----------------------------------------------------------------------*
form f_seleciona_dados2  using    p_nr_nf
                                  p_cnpj
                                  p_cnpj_cliente
                                  p_docnum.



  data: tl_zlest0019 type table of zlest0019,
         sl_zlest0019 type zlest0019         ,
         sl_zlest0004 type zlest0004      .


  refresh : it_zlest0003, it_zlest0004, it_zlest0006,it_zlest0019, tg_0045, tg_0044.
  "-------------0003-----------
  select z~serie_despacho
         z~nr_despacho
         z~nr_nf
         e~cgc_remetente
         d~cgc_cliente
    from zlest0003 as z
    inner join zlest0004 as e on z~serie_despacho eq e~serie_despacho and z~nr_despacho eq  e~nr_despacho
    inner join zlest0006 as d on d~nr_fatura eq e~nr_fatura
    into table it_zlest0003
    where z~nr_nf          eq p_nr_nf
      and e~cgc_remetente  eq p_cnpj
      and d~cgc_cliente    eq p_cnpj_cliente.

  if sy-subrc is initial.
    "-------------0004-----------

    select *
      from zlest0004
      into table it_zlest0004
      for all entries in it_zlest0003
      where serie_despacho eq it_zlest0003-serie_despacho
        and nr_despacho    eq it_zlest0003-nr_despacho.

    if sy-subrc is initial.
      "-------------0006-----------
      select *
        from zlest0006
        into table it_zlest0006
        for all entries in it_zlest0004
        where nr_fatura   eq it_zlest0004-nr_fatura.


      refresh it_zlest0019.

      loop at it_zlest0004 into sl_zlest0004.

        sl_zlest0019-seriedcl = sl_zlest0004-serie_despacho.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = sl_zlest0004-nr_despacho
          importing
            output = sl_zlest0019-dcl.
        append sl_zlest0019 to tl_zlest0019.

        clear: sl_zlest0004,
               sl_zlest0019.
      endloop.

      select *
        from zlest0019
        into table it_zlest0019
        for all entries in tl_zlest0019
      where dcl      eq tl_zlest0019-dcl
        and seriedcl eq tl_zlest0019-seriedcl
        and idinter  eq 'L2'
        and tp_reg   eq '30'
        and nfenum   eq p_nr_nf.

      delete it_zlest0019_aux where not nfnum is initial.

    endif.
  endif.
  select *
    from zlest0045
    into table tg_0045
     where docnum eq p_docnum.

  if sy-subrc is initial.
    select *
      from zlest0044
      into table tg_0044
       for all entries in tg_0045
        where chave_cte eq tg_0045-chave_cte.

  endif.
*  endif.

endform.                    " F_SELECIONA_DADOS2
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_FAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_saida_fat .

  data : wdcl type zlest0004-nr_despacho.

  sort: it_zlest0006    by nr_fatura     ,
        it_zlest0004    by nr_fatura     ,
        it_zlest0003    by serie_despacho nr_despacho,
        it_zlest0035    by nr_nf cnpj    ,
        it_t001w        by werks         ,
        it_j_1bnfdoc    by docnum        ,
        it_j_1bnflin    by docnum        ,
        it_makt         by matnr         ,
        it_parceiro     by vbeln         ,
        it_lfa1         by lifnr         .


  refresh : it_saida2.
  clear: wa_saida2.
  loop at  it_zlest0003 into wa_zlest0003.

    loop at it_zlest0004 into wa_zlest0004 where  serie_despacho eq wa_zlest0003-serie_despacho and nr_despacho eq wa_zlest0003-nr_despacho.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_zlest0004-nr_despacho
        importing
          output = wdcl.

      loop at it_zlest0006 into wa_zlest0006 where nr_fatura   eq wa_zlest0004-nr_fatura.

        loop at it_zlest0019 into wa_zlest0019 where dcl = wdcl and seriedcl = wa_zlest0004-serie_despacho.

          wa_saida2-nr_nf_all      = wa_zlest0006-nr_nf_all.
          wa_saida2-nr_fatura      = wa_zlest0006-nr_fatura.
          wa_saida2-nr_trans       = wa_zlest0006-nr_trans .
          wa_saida2-nr_frete       = wa_zlest0006-nr_frete .
          wa_saida2-serie_despacho = wa_zlest0004-serie_despacho.
          wa_saida2-nr_despacho    = wa_zlest0004-nr_despacho.
          wa_saida2-idvagao        = wa_zlest0019-idvagao  .
          wa_saida2-dtaenvio       = wa_zlest0019-dtaenvio .
          wa_saida2-obs            = wa_zlest0019-obs      .
          wa_saida2-chave          = wa_zlest0019-chave    .

          if wa_zlest0006-nr_frete is not initial.
            wa_saida2-peso_vagao     = wa_zlest0019-pesodvagao.
          else  .
            wa_saida2-peso_vagao  = 0.
          endif.

          append wa_saida2 to it_saida2.

          clear: wa_saida    ,
                 wa_zlest0019.

        endloop.
        clear: wa_zlest0006.

      endloop.

      clear: wa_zlest0004.

    endloop.

    clear: wa_zlest0003.

  endloop.

  clear: wa_saida2.
  loop at tg_0045.
    read table it_zlest0035 into wa_zlest0035
      with key docnum = tg_0045-docnum.

    read table tg_0044
      with key chave_cte = tg_0045-chave_cte.

    if sy-subrc is initial.
      wa_saida2-nr_nf_all      = tg_0044-nr_cte.
      wa_saida2-nr_trans       = tg_0044-nr_trans.
      wa_saida2-nr_frete       = tg_0044-nr_frete.
*    wa_saida2-idvagao        = wa_zlest0019-idvagao  .
      wa_saida2-dtaenvio       = tg_0044-data.
      concatenate tg_0044-bukrs tg_0044-branch wa_zlest0035-nr_nf into wa_saida2-chave separated by '-'.
      wa_saida2-peso_vagao     = tg_0045-peso_rateado.
      concatenate tg_0045-nr_vagao tg_0045-tp_vagao into wa_saida2-idvagao.

      collect wa_saida2 into it_saida2.
    endif.
  endloop.
  delete it_saida2 where nr_trans is initial
                     and nr_frete is initial.
endform.                    " F_SAIDA_FAT
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_alv_fat .
  refresh  it_fcat2.
  perform alv_preenche_cat2 using:
        'IT_SAIDA2' 'NR_NF_ALL'      text-011   '40'  ' '  ' '," DACTE
        'IT_SAIDA2' 'NR_FATURA'      text-012   '40'  ' '  ' '," NR FATURA
        'IT_SAIDA2' 'SERIE_DESPACHO' text-013   '40'  ' '  ' '," Série Despacho
        'IT_SAIDA2' 'NR_DESPACHO'    text-014   '40'  ' '  ' '," Nr Despacho
        'IT_SAIDA2' 'CHAVE'          text-024   '40'  ' '  ' ',"
        'IT_SAIDA2' 'NR_TRANS'       text-025   '40'  ' '  'X',"
        'IT_SAIDA2' 'NR_FRETE'       text-026   '40'  ' '  'X',"
        'IT_SAIDA2' 'IDVAGAO'        text-020   '40'  ' '  ' ',"
        'IT_SAIDA2' 'PESO_VAGAO'     text-021   '40'  ' '  ' ',"
        'IT_SAIDA2' 'DTAENVIO'       text-022   '40'  ' '  ' ',"
        'IT_SAIDA2' 'OBS'            text-023   '40'  ' '  ' '."

endform.                    " F_ALV_FAT
*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV_FAT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module z_exibe_alv_fat output.

  s_variant2-report = sy-repid.
  if wa_cont2 is initial.

    create object wa_cont2
      exporting
        container_name              = 'CC_ALV2'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.
  endif.
  if wa_alv2 is initial and not
    wa_cont2 is initial.

    create object wa_alv2
      exporting
        i_parent          = wa_cont2
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5.
  endif.

  if wa_event2 is initial.

    create object wa_event2.
    set handler: wa_event2->zm_handle_hotspot for wa_alv2.
    set handler: wa_event2->zm_handle_toolbar for wa_alv2.
    set handler: wa_event2->zm_handle_user_command for wa_alv2.

  endif.

  call method wa_alv2->set_table_for_first_display
    exporting
      i_save                        = 'A'
      i_default                     = 'X'
      is_variant                    = s_variant2      "is_layout = s_layout
      is_layout                     = wa_layout
    changing
      it_outtab                     = it_saida2
      it_fieldcatalog               = it_fcat2
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  check not wa_alv2 is initial.

endmodule.                 " Z_EXIBE_ALV_FAT  OUTPUT




*&---------------------------------------------------------------------*
*&      Form  alv_preenche_cat2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TABNAME  text
*      -->P_CAMPO    text
*      -->P_DESC     text
*      -->P_TAM      text
*      -->P_HOT      text
*      -->P_ZERO     text
*----------------------------------------------------------------------*
form alv_preenche_cat2 using   p_tabname type dd02d-tabname
                               p_campo type c
                               p_desc  type c
                               p_tam   type c
                               p_hot   type c
                               p_zero  type c           .
  data: wl_fcat type lvc_s_fcat.

  wl_fcat-tabname   = p_tabname                         .
  wl_fcat-fieldname = p_campo                           .
  wl_fcat-scrtext_l = p_desc                            .
  wl_fcat-scrtext_m = p_desc                            .
  wl_fcat-scrtext_s = p_desc                            .
  wl_fcat-hotspot   = p_hot                             .
  wl_fcat-no_zero   = p_zero                            .

  append wl_fcat to it_fcat2.

endform.                    "alv_preenche_cat2
