*&---------------------------------------------------------------------*
*&  Include           ZFIS33FORM
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*     CLASSES
*----------------------------------------------------------------------*
class custom_screen definition.
  public section.
    methods set_screen_cadastro
      importing
        screen_a type sy-repid.
endclass.

class custom_screen implementation.
  method set_screen_cadastro.
    move screen_a to screen_cadastro.
  endmethod.
endclass.



form seleciona_dados.
  data: valida_werks  type csks-gsber.

  check p_bukrs is not initial.

  if rd_cfop eq 'X'.

    select * from zfit0136
      into table it_saida_136
      where bukrs eq p_bukrs.

    loop at it_saida_136 assigning field-symbol(<wa_saida_136>).
      replace '/' with ' ' into <wa_saida_136>-cfop.
      condense <wa_saida_136>-cfop no-gaps.
    endloop.

  elseif rd_filial_cc is not initial.

    select * from zfit0137
      into table it_saida_137
      where bukrs eq p_bukrs.

  elseif rd_filial_ctb is not initial.

    select * from zfit0151
      into table it_saida_151
      where bukrs eq p_bukrs.

  elseif rd_grupo_merc is not initial.

    select * from zfit0195
    into table it_saida_195
      where bukrs eq p_bukrs.

  endif.

endform.


form grava_dados.

  data: num_cfop       type j_1bagt-cfop,
        tipom(02)      type c,
        v_kostl(10)    type c,
        t_question(60) type c,
        ans            type c,
        l_count_table  type i,
        v_error        type c.

  perform verifica_erros changing v_error.

  check v_error eq abap_false.

  if rd_cfop eq 'X'.

    if sy-subrc eq 0.
      data: valida_barra type c.

      sort it_saida_136 by cfop.

      loop at it_saida_136 into wa_saida_136.

        num_cfop = |{ wa_saida_136-cfop+4(1) }|.
        if num_cfop ne '/'.
          clear num_cfop.
          delete it_saida_136 where cfop = wa_saida_136-cfop.

          num_cfop = |{ wa_saida_136-cfop+0(4) }/{ wa_saida_136-cfop+4(2) }|.
          clear wa_saida_136-cfop.
          wa_saida_136-cfop = num_cfop.

        endif.

        if wa_saida_136-usnam is initial.

          wa_saida_136-usnam      = sy-uname.
          wa_saida_136-data_atual = sy-datum.
          wa_saida_136-hora_atual = sy-uzeit.

        endif.

        wa_saida_136-bukrs = p_bukrs.

        append wa_saida_136 to it_aux_136.

      endloop.

      modify zfit0136 from table it_aux_136.
      message text-002 type 'S'.
      free it_aux_136.

    endif.

  elseif rd_filial_cc is not initial.

    loop at it_saida_137 into wa_saida_137 where usnam is initial.

      wa_saida_137-usnam      = sy-uname.
      wa_saida_137-data_atual = sy-datum.
      wa_saida_137-hora_atual = sy-uzeit.
      wa_saida_137-bukrs      = p_bukrs.

      modify it_saida_137 from wa_saida_137.

    endloop.

    modify zfit0137 from table it_saida_137.
    commit work.
    message text-002 type 'S'.


  elseif rd_filial_ctb is not initial.

    loop at it_saida_151 into wa_saida_151 where usnam is initial.

      wa_saida_151-usnam      = sy-uname.
      wa_saida_151-data_atual = sy-datum.
      wa_saida_151-hora_atual = sy-uzeit.

      modify it_saida_151 from wa_saida_151.

    endloop.

    modify zfit0151 from table it_saida_151.
    commit work.
    message text-002 type 'S'.

  elseif rd_grupo_merc is not initial.

    loop at it_saida_195 into wa_saida_195 where usnam is initial.

      wa_saida_195-usnam      = sy-uname.
      wa_saida_195-data_atual = sy-datum.
      wa_saida_195-hora_atual = sy-uzeit.

      modify it_saida_195 from wa_saida_195.
    endloop.

    modify zfit0195 from table it_saida_195.
    commit work.
    message text-002 type 'S'.

  endif.

  clear: wa_zfit0136, wa_saida_137, wa_saida_151 , ans, v_kostl, p_cfop, p_tp_mercado, p_werks, p_kostl, p_saknr, n_cfop, n_filial, n_centro, tipom, t_question,
          it_saida_136[], it_saida_137[], it_saida_151[].

  perform seleciona_dados.


  perform f_refresh_alv.


endform.


form deleta_dados.

  loop at tg_selectedrow into wg_selectedrow.

    if rd_cfop is not initial.

      read table it_saida_136 into wa_saida_136 index wg_selectedrow-index.
       wa_saida_136-cfop = |{ wa_saida_136-cfop+0(4) }/{ wa_saida_136-cfop+4(2) }|.
      delete zfit0136 from wa_saida_136.
*      delete from zfit0136
*        where bukrs eq wa_saida_136-bukrs
*          and cfop eq wa_saida_136-cfop.
*          and tp_mercado eq wa_saida_136-tp_mercado.

    elseif rd_filial_cc is not initial.

      read table it_saida_137 into wa_saida_137 index wg_selectedrow-index.
      delete zfit0137 from wa_saida_137.

    elseif rd_filial_ctb is not initial.

      read table it_saida_151 into wa_saida_151 index wg_selectedrow-index.
      delete zfit0151 from wa_saida_151.

    elseif rd_grupo_merc is not initial.

      read table it_saida_195 into wa_saida_195 index wg_selectedrow-index.
      if sy-subrc eq 0.
        delete from zfit0195
        where bukrs eq wa_saida_195-bukrs
          and matkl eq wa_saida_195-matkl.
      endif.
    endif.

  endloop.

  clear: wa_zfit0136, wa_saida_137, wa_saida_151, wa_saida_195, p_cfop, p_tp_mercado, p_werks, p_kostl, p_saknr, n_cfop, n_filial, n_centro,
         it_saida_136[], it_saida_137[],  it_saida_151[], it_saida_195[].

  perform seleciona_dados.

  perform f_refresh_alv.

endform.             "   DELETA_DADOS

form verifica_erros changing p_error.

  data: num_cfop    type j_1bagt-cfop,
        valida_cfop type j_1bagt-cfop,
        tl_cell     type lvc_t_cell,
        wl_linha(6).

  clear tg_msg_ret[].

  p_error = abap_false.

  if rd_cfop is not initial.
    loop at it_saida_136 into wa_saida_136.
      wl_linha = sy-tabix.

      num_cfop = |{ wa_saida_136-cfop+4(1) }|.
      if num_cfop ne '/'.
        clear num_cfop.
        num_cfop = |{ wa_saida_136-cfop+0(4) }{ wa_saida_136-cfop+4(2) }|.

        select single cfop
       from j_1bagt
       into valida_cfop
       where cfop  = num_cfop.

        if valida_cfop is initial or wa_saida_136-cfop is initial.
          clear: cfop_aux, num_cfop.

          p_error = abap_true.

          message text-005 type 'I'.
          exit.
        endif.

      endif.
      clear wa_saida_136.


    endloop.

  elseif rd_filial_cc is not initial.

    if wa_saida_137-kostl is not initial.
      select single gsber
          from csks
          into werks_aux_2
          where gsber = werks_aux and
                kostl = kostl_aux.

      if werks_aux_2 is initial.

        p_error = abap_true.

        message text-007 type 'I'.
        exit.
      endif.
    endif.

  elseif rd_filial_ctb is not initial.

    if wa_saida_151-saknr is not initial.

      select single *                  "#EC CI_DB_OPERATION_OK[2431747]
        from ska1 into @data(_wl_ska1) "#EC CI_DB_OPERATION_OK[2389136]
       where ktopl = '0050'
         and saknr = @saknr_aux.

      if sy-subrc ne 0.

        p_error = abap_true.

        message text-008 type 'I'.
        exit.
      endif.

    endif.

  endif.

endform.

form f_refresh_alv.

  if ( rd_cfop is not initial ).

    if g_grid is not initial.
      call method g_grid->refresh_table_display
        exporting
          is_stable = wa_stable.
    endif.

  elseif rd_filial_cc is not initial.

    if g_grid2 is not initial.
      call method g_grid2->refresh_table_display
        exporting
          is_stable = wa_stable.
    endif.

  elseif rd_filial_ctb is not initial.

    if g_grid3 is not initial.
      call method g_grid3->refresh_table_display
        exporting
          is_stable = wa_stable.
    endif.

  elseif rd_grupo_merc is not initial.

    if g_grid4 is not initial.
      call method g_grid4->refresh_table_display
        exporting
          is_stable = wa_stable.
    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_EMPRESA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module atribui_info_empresa input.

  if p_bukrs is not initial.
    select single butxt from t001
      into p_txt_bukrs
      where bukrs eq p_bukrs.
    if sy-subrc ne 0.
      message e024(sd) with 'Empresa informada é invalida'.
    endif.
  endif.

endmodule.
