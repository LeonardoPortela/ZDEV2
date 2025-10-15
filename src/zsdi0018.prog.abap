*&---------------------------------------------------------------------*
*& Report  ZSDI0018
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zsdi0018.

tables: zjcnd_branch,sscrfields.

types begin of ty_zjcnd_branch.
include structure zjcnd_branch.
types: name type name1.
types end of ty_zjcnd_branch.

data: it_zjcnd_branch     type table of zjcnd_branch with header line,
      it_zjcnd_branch_aux type table of zjcnd_branch with header line,
      it_j_1bbranch       type table of j_1bbranch with header line,
      it_saida            type table of ty_zjcnd_branch with header line,
      wa_saida            type ty_zjcnd_branch,
      wa_scroll_col       type lvc_s_col,
      wa_scroll_row       type lvc_s_roid,
      gs_layout           type lvc_s_layo,
      alv                 type ref to cl_gui_alv_grid,
      catalogo            type lvc_t_fcat,
      primeiro            type char01,
      editar              type char01,
      container           type ref to cl_gui_custom_container,
      ok_code             type sy-ucomm,
      ok_code_0002        type sy-ucomm,
      ok_code_0003        type sy-ucomm,
      vg_tx_bukrs         type butxt,
      vg_tx_branch        type name1,
      vg_tx_regio         type t005u-bezei.

data: lt_return type table of ddshretval,
      ls_return type ddshretval.

data: it_screen_status type table of sy-ucomm.

selection-screen: begin of block bl_001 with frame title text-010.
  select-options: s_bukrs    for zjcnd_branch-bukrs,
                  s_branch   for zjcnd_branch-branch,
                  s_emiss    for zjcnd_branch-dt_emissao.
selection-screen: end of block bl_001.

*AT SELECTION-SCREEN OUTPUT. "58979 CS2021000217 Melhoria Cadastro Certidão Negativa de Débito ICMS - PSA
*
*  it_screen_status = VALUE #( ( CONV sy-ucomm( '' ) ) ).
*
*  IF sy-dynnr = 1000.
*
*    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
*      EXPORTING
*        p_status  = 'ST1000' "SY-pfkey "'ST1000'
*        p_program = sy-repid
*      TABLES
*        p_exclude = it_screen_status.
*
*  ENDIF.


start-of-selection.

at selection-screen.
  case sy-ucomm.
    when 'FB_PARAM01'.
      call transaction 'ZSDT0218'. " Cadastro de Empresa
    when 'BACK'.
      leave to screen 0.
      leave program.
    when 'LEAV' or 'CANC'.
      leave to screen 0.
      leave program.
    when 'ONLI'.
      perform consulta.
      call screen 0001.
  endcase.



*&---------------------------------------------------------------------*
*&      Module  CRIAR_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module criar_alv output.

  constants: tabela type string value 'IT_SAIDA'.

  data: text_n001 type c length 50 value 'Empresa',
        text_n002 type c length 50 value 'Filial',
        text_n003 type c length 50 value 'Nome Filial',
        text_n004 type c length 50 value 'Dt. Emissão',
        text_n005 type c length 50 value 'Dt. Vencimento',
        text_n006 type c length 50 value 'Código CND',
        text_n007 type c length 50 value 'Código Autenticidade.'.

  if primeiro is initial.

    create object container
      exporting
        container_name = 'CTN'.

    create object alv
      exporting
        i_parent = container.

    perform z_estrutura_fieldcat tables catalogo using:
        tabela 'BUKRS'       text_n001 ' ' 01 05 space space space space space space space,
        tabela 'BRANCH'      text_n002 ' ' 02 05 space space space space space space space,
        tabela 'NAME'        text_n003 ' ' 03 30 space space space space space space space,
        tabela 'DT_EMISSAO'  text_n004 ' ' 04 12 space space space space space space space,
        tabela 'DT_VALIDADE' text_n005 ' ' 05 12 space space space space space space space,
        tabela 'CD_CND'      text_n006 ' ' 06 20 space space space space space space space,
        tabela 'COD_AUT'     text_n007 ' ' 06 20 space space space space space space space.

    clear: gs_layout.
    gs_layout-zebra      = 'X'.
    gs_layout-sel_mode   = 'A'.

    call method alv->set_table_for_first_display
      exporting
        i_default       = space
        is_layout       = gs_layout
      changing
        it_fieldcatalog = catalogo
        it_outtab       = it_saida[].

    primeiro = 'X'.
  endif.

  call method alv->refresh_table_display.

  call method alv->set_scroll_info_via_id
    exporting
      is_col_info = wa_scroll_col
      is_row_no   = wa_scroll_row.

endmodule.                 " CRIAR_ALV  OUTPUT


*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
form z_estrutura_fieldcat tables it_catalogo type lvc_t_fcat
                           using p_tab_name
                                 p_fieldname
                                 p_texto_grande
                                 p_hot
                                 p_posicao
                                 p_outputlen
                                 p_fix_column
                                 p_convexit
                                 p_do_sum
                                 p_icon
                                 p_just
                                 p_emphasize
                                 p_edit.

  data: wa_catalog type lvc_s_fcat.
  wa_catalog-tabname     = p_tab_name.
  wa_catalog-fieldname   = p_fieldname.
  wa_catalog-scrtext_l   = p_texto_grande.
  wa_catalog-scrtext_m   = p_texto_grande.
  wa_catalog-scrtext_s   = p_texto_grande.
  wa_catalog-hotspot     = p_hot.
  wa_catalog-col_pos     = p_posicao.
  wa_catalog-outputlen   = p_outputlen.
  wa_catalog-fix_column  = p_fix_column.
  wa_catalog-convexit    = p_convexit.
  wa_catalog-do_sum      = p_do_sum.
  wa_catalog-icon        = p_icon.
  wa_catalog-just        = p_just.
  wa_catalog-emphasize   = p_emphasize.
  wa_catalog-edit        = p_edit.
  append wa_catalog to it_catalogo.

endform.                    " Z_ESTRUTURA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0001 output.

  set pf-status 'PF0001'.
  set titlebar 'TL0001'.

endmodule.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0001 input.

  data: vg_sel type sy-subrc.

  clear: editar.

  case ok_code.
    when 'BACK' or 'EXIT' or 'CANCEL'.
      leave to screen 0.
    when 'NOVO'.
      clear: zjcnd_branch,vg_tx_branch, vg_tx_bukrs,vg_tx_regio.
      call screen 0002 starting at 10 10.
    when 'NOVO_MASSA'.
      clear: zjcnd_branch,vg_tx_branch, vg_tx_bukrs,vg_tx_regio.
      call screen 0003 starting at 10 10.
    when 'EDITAR'.
      perform seleciona_registro using vg_sel.
      if vg_sel is initial.
        editar = 'X'.
        move-corresponding wa_saida to zjcnd_branch.
        call screen 0002 starting at 10 10.
      endif.
    when 'EXCLUIR'.
      "PERFORM seleciona_registro USING vg_sel. "58979 CS2021000217 Melhoria Cadastro Certidão Negativa de Débito ICMS - PSA

      data: it_selected_rows type lvc_t_row,
            wa_selected_rows type lvc_s_row.
      free:it_selected_rows.
      clear: wa_saida,wa_selected_rows.

      call method alv->get_selected_rows
        importing
          et_index_rows = it_selected_rows.
      data qtd_lines type i.
      describe table it_selected_rows  lines qtd_lines .

      if qtd_lines > 0 .
        loop at it_selected_rows into wa_selected_rows.
          read table it_saida into wa_saida index wa_selected_rows-index.
          delete from zjcnd_branch where chave = wa_saida-chave and bukrs = wa_saida-bukrs and branch = wa_saida-branch.
        endloop.
        perform consulta.
      else.
        message s836(sd) with 'Selecione uma linha!'.

      endif.



  endcase.

endmodule.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_REGISTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form seleciona_registro  using p_sel type sy-subrc.

  data: it_selected_rows type lvc_t_row,
        wa_selected_rows type lvc_s_row.

  p_sel = 1.

  clear: wa_saida.

  call method alv->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  if it_selected_rows is initial.
    message s836(sd) with 'Deve ser selecionado um lançamento!'.
  endif.

  check not it_selected_rows is initial.
  read table it_selected_rows index 1 into wa_selected_rows.
  read table it_saida into wa_saida index wa_selected_rows-index.
  p_sel = 0.

endform.                    " SELECIONA_REGISTRO

*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0002 output.

  set pf-status 'PF0002'.
  set titlebar 'TL0002'.

  if not zjcnd_branch-bukrs is initial.

    select single butxt into vg_tx_bukrs
      from t001
    where bukrs eq zjcnd_branch-bukrs.

    if not zjcnd_branch-branch is initial.
      select single name into vg_tx_branch
        from j_1bbranch
       where bukrs  eq zjcnd_branch-bukrs
      and branch eq zjcnd_branch-branch.
    endif.

  else.
*    if s_bukrs-low is not initial.
*      zjcnd_branch-bukrs = s_bukrs-low.
*    endif.
  endif.



  if not editar is initial.
    loop at screen.
      case screen-name.
        when 'ZJCND_BRANCH-BUKRS'.
          screen-input = '0'.
        when 'ZJCND_BRANCH-BRANCH'.
          screen-input = '0'.
        when 'ZJCND_BRANCH-DT_EMISSAO'.
          screen-input = '0'.
        when 'ZJCND_BRANCH-DT_VALIDADE'.
          screen-input = '0'.
      endcase.
      modify screen.
    endloop.
  endif.

endmodule.                 " STATUS_0002  OUTPUT
module status_0003 output.

  set pf-status 'PF0003'.
  set titlebar 'TL0003'.

  if not zjcnd_branch-bukrs is initial.

    select single butxt into vg_tx_bukrs
      from t001
    where bukrs eq zjcnd_branch-bukrs.

    if not zjcnd_branch-branch is initial.
      select single name into vg_tx_branch
        from j_1bbranch
       where bukrs  eq zjcnd_branch-bukrs
      and branch eq zjcnd_branch-branch.
    endif.
  else.
*    if s_bukrs-low is not initial.
*      zjcnd_branch-bukrs = s_bukrs-low.
*    endif.
  endif.

  if not editar is initial.
    loop at screen.
      case screen-name.
        when 'ZJCND_BRANCH-BUKRS'.
          screen-input = '0'.
        when 'ZJCND_BRANCH-BRANCH'.
          screen-input = '0'.
        when 'ZJCND_BRANCH-DT_EMISSAO'.
          screen-input = '0'.
        when 'ZJCND_BRANCH-DT_VALIDADE'.
          screen-input = '0'.
      endcase.
      modify screen.
    endloop.
  endif.

endmodule.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0002 input.

  data: vg_chave type zjcnd_branch-chave.

  case ok_code_0002.
    when 'GRAVAR'.

      if s_bukrs is not initial.
        read table s_bukrs into data(ws_bukrs) with key low = zjcnd_branch-bukrs.
        if sy-subrc ne 0.
          message s836(sd) with 'Seleciona a empresa conforme tela de seleção' .
          exit.
        endif.
      endif.

      if zjcnd_branch-dt_validade lt zjcnd_branch-dt_emissao.
        message s836(sd) with 'Data de validade' 'deve ser maior/igual' 'data de emissão!'.
        exit.
      endif.

      if zjcnd_branch-chave is initial.
        vg_chave = 0.
        select max( chave ) into vg_chave from zjcnd_branch.
        zjcnd_branch-chave = vg_chave + 1.
      endif.
      modify zjcnd_branch.
      perform consulta.
      leave to screen 0.
  endcase.

endmodule.                 " USER_COMMAND_0002  INPUT
module user_command_0003 input. "58979 CS2021000217 Melhoria Cadastro Certidão Negativa de Débito ICMS - PSA

  "DATA: vg_chave TYPE zjcnd_branch-chave.



  case sy-ucomm.
    when 'FB_SEARCH_BUKRS'.

*      SELECT DISTINCT a~bukrs,c~butxt FROM j_1bbranch AS a
*      INNER JOIN t001w AS b ON a~bukrs = b~vkorg
*      INNER JOIN t001 AS c ON a~bukrs = c~bukrs AND b~land1 = c~land1
*      WHERE substring( a~branch, 1,1 ) IN ('0','1','2','3','4','5','6','7','8','9')
*      AND substring( a~bukrs, 1,1 ) IN ('0','1','2','3','4','5','6','7','8','9')
*        AND substring( b~werks, 1,1 ) IN ('0','1','2','3','4','5','6','7','8','9')
*      AND b~regio <> ''
*      AND b~land1 = 'BR'
*            ORDER BY a~bukrs
*            INTO TABLE @DATA(it_bukrs_filter).

*     CS2021000217 // MMSILVA - 29.10.2024
      select bukrs, butxt
        from t001
        where bukrs in @s_bukrs
        order by bukrs
        into table @data(it_bukrs_filter).

*     CS2021000217 // MMSILVA - 29.10.2024

      sort it_bukrs_filter by bukrs ascending.

      free: lt_return.
      call function 'F4IF_INT_TABLE_VALUE_REQUEST'
        exporting
          retfield        = 'BUKRS'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          value_org       = 'S'
        tables
          value_tab       = it_bukrs_filter
          return_tab      = lt_return
        exceptions
          parameter_error = 1
          no_values_found = 2
          others          = 3.

      if sy-subrc = 0.
        read table lt_return into ls_return index 1.
        if sy-subrc = 0.
          read table it_bukrs_filter with key bukrs = ls_return-fieldval assigning field-symbol(<_get_bukrs>).

*          if s_bukrs is not initial.
*            if s_bukrs-low ne <_get_bukrs>-bukrs.
*              message s836(sd) with 'Seleciona a empresa ' s_bukrs-low 'conforme tela de seleção' .
*              exit.
*            endif.
*          endif.

          zjcnd_branch-bukrs = <_get_bukrs>-bukrs.
          vg_tx_bukrs = <_get_bukrs>-butxt.
          clear:zjcnd_branch-regio,vg_tx_regio.
        endif.
      else.

      endif.

    when 'FB_SEARCH_REGIO'.

      if zjcnd_branch-bukrs is not initial.
        select distinct b~regio,d~bezei from j_1bbranch as a
        inner join t001w as b on a~bukrs = b~vkorg
        inner join t001 as c on a~bukrs = c~bukrs and b~land1 = c~land1
        inner join t005u as d on b~regio = d~bland and b~land1 = d~land1 and d~SPras = 'P'
        where substring( a~branch, 1,1 ) in ('0','1','2','3','4','5','6','7','8','9')
        and substring( a~bukrs, 1,1 ) in ('0','1','2','3','4','5','6','7','8','9')
          and substring( b~werks, 1,1 ) in ('0','1','2','3','4','5','6','7','8','9')
        and a~bukrs = @zjcnd_branch-bukrs
        and b~regio <> ''
        and b~land1 = 'BR'
                order by b~regio
                      into table @data(it_regio_filter).
      else.
        message 'Selecione a Empresa para selecionar o Estado!' type 'I'.
      endif.

      check it_regio_filter is not initial.

      sort it_regio_filter by regio ascending.

      free: lt_return.
      call function 'F4IF_INT_TABLE_VALUE_REQUEST'
        exporting
          retfield        = 'REGIO'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          value_org       = 'S'
        tables
          value_tab       = it_regio_filter
          return_tab      = lt_return
        exceptions
          parameter_error = 1
          no_values_found = 2
          others          = 3.

      if sy-subrc = 0.
        read table lt_return into ls_return index 1.
        if sy-subrc = 0.
          read table it_regio_filter with key regio = ls_return-fieldval assigning field-symbol(<_get_regio>).
          zjcnd_branch-regio = <_get_regio>-regio.
          vg_tx_regio = <_get_regio>-bezei.
        endif.
      else.

      endif.

    when 'GRAVAR'.

      type-pools: esp1.

      data: lt_tab type esp1_message_tab_type.
      data: ls_tab type esp1_message_wa_type.
      free: lt_tab.
      clear: ls_tab.

      data: _erro    type bool value abap_false,
            msg_erro type message.


      if s_bukrs is not initial.
        read table s_bukrs into ws_bukrs with key low = zjcnd_branch-bukrs.
        if sy-subrc ne 0.
*          message s836(sd) with 'Seleciona a empresa ' s_bukrs-low 'conforme tela de seleção' .
*          exit.

          ls_tab-msgty  = 'E'.
          ls_tab-msgid = 1.
          ls_tab-msgv1  = |Seleciona a empresa conforme tela de seleção|.
          ls_tab-lineno = 1.
          append ls_tab to lt_tab.
          _erro = abap_true.
        endif.
      endif.


      if zjcnd_branch-bukrs is initial.
        ls_tab-msgty  = 'E'.
        ls_tab-msgid = 1.
        ls_tab-msgv1  = 'Empresa é Obrigatório!'.
        ls_tab-lineno = 1.
        append ls_tab to lt_tab.
        _erro = abap_true.
      endif.
      if zjcnd_branch-regio is initial.
        ls_tab-msgty  = 'E'.
        ls_tab-msgid = 2.
        ls_tab-msgv1  = 'Estado é Obrigatório!'.
        ls_tab-lineno = 2.
        append ls_tab to lt_tab.
        _erro = abap_true.
      endif.
      if zjcnd_branch-cd_cnd is initial.
        ls_tab-msgty  = 'E'.
        ls_tab-msgid = 3.
        ls_tab-msgv1  = 'Cod._Certidão_Negativa_de_Débito_(ICMS) é Obrigatório!'.
        ls_tab-lineno = 3.
        "APPEND ls_tab TO lt_tab.
        _erro = abap_true.
      endif.

      if zjcnd_branch-cod_aut is initial.
        ls_tab-msgty  = 'E'.
        ls_tab-msgid = 4.
        ls_tab-msgv1  = 'Cod._Autenticidade é Obrigatório!'.
        ls_tab-lineno = 4.
        append ls_tab to lt_tab.
        _erro = abap_true.
      endif.

      if zjcnd_branch-dt_validade lt zjcnd_branch-dt_emissao.
        ls_tab-msgty  = 'E'.
        ls_tab-msgid = 5.
        ls_tab-msgv1  = 'Data de validade deve ser maior/igual data de emissão!'.
        ls_tab-lineno = 5.
        append ls_tab to lt_tab.
        _erro = abap_true.
      endif.

      if zjcnd_branch-chave is initial.
        vg_chave = 0.
        select max( chave ) into vg_chave from zjcnd_branch.
        zjcnd_branch-chave = vg_chave + 1.
      endif.

      if _erro = abap_false.

        select distinct a~Bukrs,b~werks,b~regio from j_1bbranch as a
        inner join t001w as b on a~bukrs = b~vkorg
        where substring( a~branch, 1,1 ) in ('0','1','2','3','4','5','6','7','8','9')
        and substring( a~bukrs, 1,1 ) in ('0','1','2','3','4','5','6','7','8','9')
        and substring( b~werks, 1,1 ) in ('0','1','2','3','4','5','6','7','8','9')
        and a~bukrs = @zjcnd_branch-bukrs
        and b~regio = @zjcnd_branch-regio
        and b~land1 = 'BR'
         order by b~werks
                  into table @data(it_grava).

        data: _cd_cnd      type zjcnd_branch-cd_cnd,
              _cod_aut     type zjcnd_branch-cod_aut,
              _dt_emissao  type zjcnd_branch-dt_emissao,
              _dt_validade type zjcnd_branch-dt_validade.

        clear: _cd_cnd,_cod_aut,_dt_emissao,_dt_validade.

        _cd_cnd = zjcnd_branch-cd_cnd.
        _cod_aut = zjcnd_branch-cod_aut.
        _dt_emissao = zjcnd_branch-dt_emissao.
        _dt_validade = zjcnd_branch-dt_validade.

        clear: zjcnd_branch.

        loop at it_grava assigning field-symbol(<grava>).
          clear: zjcnd_branch.
          zjcnd_branch-bukrs = <grava>-bukrs.
          zjcnd_branch-branch = <grava>-werks.
          zjcnd_branch-regio = <grava>-regio.
          zjcnd_branch-cd_cnd = _cd_cnd.
          zjcnd_branch-cod_aut = _cod_aut.
          zjcnd_branch-dt_emissao = _dt_emissao.
          zjcnd_branch-dt_validade = _dt_validade.
          modify zjcnd_branch.
          clear: zjcnd_branch.
        endloop.

        "MODIFY zjcnd_branch.
        clear: zjcnd_branch,vg_tx_branch, vg_tx_bukrs,vg_tx_regio.
        perform consulta.
        leave to screen 0.
      else.

        call function 'C14Z_MESSAGES_SHOW_AS_POPUP'
          tables
            i_message_tab = lt_tab.
      endif.

  endcase.

endmodule.

*----------------------------------------------------------------------*
module user_command_0002_exit input.
  leave to screen 0.
endmodule.                 " USER_COMMAND_0002_EXIT  INPUT
module user_command_0003_exit input. "58979 CS2021000217 Melhoria Cadastro Certidão Negativa de Débito ICMS - PSA
  leave to screen 0.
endmodule.                 " USER_COMMAND_0002_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Form  CONSULTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form consulta .

  clear: it_saida[],
         it_zjcnd_branch[],
         it_j_1bbranch[].

  select * into table it_zjcnd_branch
    from zjcnd_branch
   where bukrs      in s_bukrs
     and branch     in s_branch
  and dt_emissao in s_emiss.

  move it_zjcnd_branch[] to it_zjcnd_branch_aux[].
  sort it_zjcnd_branch_aux by bukrs branch.
  delete adjacent duplicates from it_zjcnd_branch_aux comparing bukrs branch.

  select * into table it_j_1bbranch
    from j_1bbranch
     for all entries in it_zjcnd_branch_aux
   where bukrs   eq it_zjcnd_branch_aux-bukrs
  and branch  eq it_zjcnd_branch_aux-branch.

  loop at it_zjcnd_branch.
    move-corresponding it_zjcnd_branch to it_saida.
    read table it_j_1bbranch with key bukrs  = it_zjcnd_branch-bukrs
                                      branch = it_zjcnd_branch-branch.
    it_saida-name = it_j_1bbranch-name.
    append it_saida.
  endloop.

endform.                    " CONSULTA
*&---------------------------------------------------------------------*
*&      Module  PAI_HELP_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai_help_field input.


*     CS2021000217 // MMSILVA - 29.10.2024
      free: it_bukrs_filter.
        select bukrs, butxt
        from t001
        where bukrs in @s_bukrs
        order by bukrs
        into table @it_bukrs_filter.

*     CS2021000217 // MMSILVA - 29.10.2024

      sort it_bukrs_filter by bukrs ascending.

      free: lt_return.
      call function 'F4IF_INT_TABLE_VALUE_REQUEST'
        exporting
          retfield        = 'BUKRS'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          value_org       = 'S'
        tables
          value_tab       = it_bukrs_filter
          return_tab      = lt_return
        exceptions
          parameter_error = 1
          no_values_found = 2
          others          = 3.

      if sy-subrc = 0.
        read table lt_return into ls_return index 1.
        if sy-subrc = 0.
          read table it_bukrs_filter with key bukrs = ls_return-fieldval ASSIGNING <_get_bukrs>.
          zjcnd_branch-bukrs = <_get_bukrs>-bukrs.
          vg_tx_bukrs = <_get_bukrs>-butxt.
          clear:zjcnd_branch-regio,vg_tx_regio.
        endif.
      else.

      endif.


endmodule.
