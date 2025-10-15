*----------------------------------------------------------------------*
***INCLUDE MZDRE0001_0001 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0001 output.

  clear: it_fcode.

  if tl_0001 is initial.
    tab_strip-activetab = ok_filtro.
    tl_0001             = tl_1001.
  endif.

  if tl_0001 eq tl_1002.
    wa_fcode = ok_csta.
    append wa_fcode to it_fcode.
    wa_fcode = ok_cpro.
    append wa_fcode to it_fcode.
    wa_fcode = ok_ccancpro.
    append wa_fcode to it_fcode.
    wa_fcode = ok_cexcel.
    append wa_fcode to it_fcode.
  endif.

  set pf-status 'PF0001' excluding it_fcode.
  set titlebar 'TL0001'.

  if t_empres[] is initial.
    select single *
         from usr05
         into @data(_usr05)
         where bname = @sy-uname
         and parid   = 'BUK'.
    if sy-subrc = 0.
      t_empres-sign    = 'I'.
      t_empres-option  = 'EQ'.
      t_empres-low = _usr05-parva+0(4).
      append t_empres.
    endif.
  endif.
endmodule.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0001_exit input.
  if tab_strip-activetab ne ok_result.
    leave program.
  else.
    tab_strip-activetab = ok_filtro.
    tl_0001             = tl_1001.
  endif.
endmodule.                 " USER_COMMAND_0001_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0001 input.

  data: vg_verifica_selecao type sy-subrc.

  case ok_code_0001.
    when ok_filtro or ok_back.
      "BREAK-POINT.
      "IF ( TAB_STRIP-ACTIVETAB EQ OK_FILTRO ) AND ( OK_CODE_0001 EQ OK_BACK ).
      "  LEAVE PROGRAM.
      "ENDIF.
      tab_strip-activetab = ok_filtro.
      tl_0001             = tl_1001.
    when ok_result.
      perform verifica_selecao_dre using vg_verifica_selecao.
    when ok_csta.
      perform consultar_dres.
    when ok_cpro.
      clear: wa_dre_dados_alv, ok_code_1103.
      call screen 1103 starting at 07 05 ending at 75 10.
    when ok_ccancpro.
      perform cancela_processamento.

    when ok_cexcel.
      refresh: it_excel.
      clear: wg_file_local.
      wg_file_local = 'C:\Temp\'.
      call screen 1105 starting at 07 05 ending at 75 10.
      if sy-ucomm eq ok_save.
        sy-ucomm  = ok_cexcel.
        perform verifica_selecao_dre using vg_verifica_selecao.
        perform pesquisa_dre.
        if it_excel[] is not initial.
          perform exporta_dados.
        endif.
      endif.
***     Inicio - ALX
    when c_fbxgl.
      perform fm_fbl3n_zgl060.
*      CALL SCREEN 9012.
***     Fim - ALX

    when c_zgl_tbder.
      perform fm_zgl060_tabela_derivada.

    when c_tbdr_dre.
      perform fm_tb_der_dre.
  endcase.
  clear: ok_code_0001.
endmodule.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Form  CONSULTAR_DRES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form consultar_dres .

  data: it_t001             type table of t001 with header line,
        it_zgl015_dre_est01 type table of zgl015_dre_est01 with header line,
        "IT_ZGL015_DRE_EST08 TYPE TABLE OF ZGL015_DRE_EST08 WITH HEADER LINE,
        it_dre_dados_aux    type table of zgl020_dre_dados with header line,
        it_t005             type table of t005 with header line,
        vg_status_prot      type string,
        qtd_empresas        type i.

  clear: it_dre_dados[],
         it_dre_dados_alv[],
         it_t001[],
         it_t005[].

  ranges: empresa for t001-bukrs.

  select * into table it_t001
    from t001
   where bukrs in t_empres.

  clear: empresa.

  loop at it_t001.
    empresa-sign   = 'I'.
    empresa-option = 'EQ'.
    empresa-low    = it_t001-bukrs.
    empresa-high   = it_t001-bukrs.
    authority-check object 'F_BKPF_BUK' id 'BUKRS' field it_t001-bukrs.
    if sy-subrc is initial.
      append empresa.
    endif.
  endloop.

  describe table empresa lines qtd_empresas.
  if qtd_empresas le 0.
    read table it_t001 index 1.
    message s091(8b) with it_t001-bukrs.
    exit.
  endif.

  clear: it_t001[].

  select * into table it_dre_dados
    from zgl020_dre_dados
   where bukrs in empresa
     and versn in t_estrut
     and gjahr in t_ano
     and monat in t_mes
   order by gjahr monat.

  clear: it_dre_dados_aux[].
  move it_dre_dados[] to it_dre_dados_aux[].
  sort it_dre_dados_aux by bukrs.
  delete adjacent duplicates from it_dre_dados_aux comparing bukrs.

  if not it_dre_dados_aux[] is initial.
    select * into table it_t001
      from t001
       for all entries in it_dre_dados_aux
     where bukrs eq it_dre_dados_aux-bukrs.

    if sy-subrc is initial.
      if t_moeda-low is initial.
        select *
          from t005
          into table it_t005
           for all entries in it_t001
            where land1 eq it_t001-land1.
      endif.
    endif.
  endif.

  clear: it_dre_dados_aux[].
  move it_dre_dados[] to it_dre_dados_aux[].
  sort it_dre_dados_aux by bukrs versn.
  delete adjacent duplicates from it_dre_dados_aux comparing bukrs versn.

  if not it_dre_dados_aux[] is initial.

*    SELECT * INTO TABLE IT_ZGL015_DRE_EST08
*      FROM ZGL015_DRE_EST08
*       FOR ALL ENTRIES IN IT_DRE_DADOS_AUX
*     WHERE BUKRS_B EQ IT_DRE_DADOS_AUX-BUKRS
*       AND VERSN   EQ IT_DRE_DADOS_AUX-VERSN.

    select * into table it_zgl015_dre_est01
      from zgl015_dre_est01
       for all entries in it_dre_dados_aux
     where versn eq it_dre_dados_aux-versn.

*    IF IT_ZGL015_DRE_EST08[] IS NOT INITIAL.
*      SELECT * APPENDING TABLE IT_ZGL015_DRE_EST01
*        FROM ZGL015_DRE_EST01
*         FOR ALL ENTRIES IN IT_ZGL015_DRE_EST08
*       WHERE BUKRS EQ IT_ZGL015_DRE_EST08-BUKRS
*         AND VERSN EQ IT_ZGL015_DRE_EST08-VERSN.
*    ENDIF.

  endif.

  loop at it_dre_dados.

    clear: wa_dre_dados_alv.

    move-corresponding it_dre_dados to wa_dre_dados_alv.

    read table it_t001 with key bukrs = it_dre_dados-bukrs.
    if sy-subrc is initial.
      wa_dre_dados_alv-butxt = it_t001-butxt.
    endif.

    if t_moeda-low is initial.
      read table it_t005
       with key land1 = it_t001-land1.
      if sy-subrc is initial.
        move it_t005-waers to wa_dre_dados_alv-waers.

      endif.
    else.
      move t_moeda-low to wa_dre_dados_alv-waers.
    endif.

    read table it_zgl015_dre_est01 with key versn = it_dre_dados-versn.

    if sy-subrc is initial.
      wa_dre_dados_alv-vstxt = it_zgl015_dre_est01-vstxt.
*    ELSE.
*      READ TABLE IT_ZGL015_DRE_EST08 WITH KEY BUKRS_B = IT_DRE_DADOS-BUKRS
*                                              VERSN   = IT_DRE_DADOS-VERSN.
*      IF SY-SUBRC IS INITIAL.
*        READ TABLE IT_ZGL015_DRE_EST01 WITH KEY BUKRS = IT_ZGL015_DRE_EST08-BUKRS
*                                                VERSN = IT_ZGL015_DRE_EST08-VERSN.
*        IF SY-SUBRC IS INITIAL.
*          WA_DRE_DADOS_ALV-VSTXT = IT_ZGL015_DRE_EST01-VSTXT.
*        ENDIF.
*      ENDIF.
    endif.

    case wa_dre_dados_alv-liberado.
      when space.
        wa_dre_dados_alv-icoli = icon_import_all_requests.
      when 'X'.
        if wa_dre_dados_alv-status = 3.
          wa_dre_dados_alv-icoli = icon_system_okay.
        elseif wa_dre_dados_alv-status = 1.
          wa_dre_dados_alv-icoli = icon_led_yellow.
        elseif wa_dre_dados_alv-status is initial.
          wa_dre_dados_alv-icoli = icon_system_okay.
        endif.
    endcase.

    case wa_dre_dados_alv-status.
      when space.
        wa_dre_dados_alv-icost    = icon_dummy.
        wa_dre_dados_alv-rowcolor = 'C201'.
        wa_dre_dados_alv-irepo    = icon_status.
      when '1'.
        wa_dre_dados_alv-icost    = icon_led_yellow.
        wa_dre_dados_alv-rowcolor = 'C601'.
        wa_dre_dados_alv-irepo    = icon_status.
      when '2'.
        wa_dre_dados_alv-icost    = icon_led_red.
        wa_dre_dados_alv-rowcolor = 'C610'.
        wa_dre_dados_alv-irepo    = icon_refresh.
      when '3'.
        wa_dre_dados_alv-icost    = icon_led_green.
        wa_dre_dados_alv-rowcolor = 'C501'.
        wa_dre_dados_alv-irepo    = icon_refresh.
    endcase.
    vg_status_prot = wa_dre_dados_alv-status_proc.
    concatenate vg_status_prot ' %' into wa_dre_dados_alv-status_prot.

    append wa_dre_dados_alv to it_dre_dados_alv.

  endloop.

  sort it_dre_dados_alv by gjahr monat.

endform.                    " CONSULTAR_DRES

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_DRES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_alv_dres output.

  data: wa_hints  type lvc_s_qinf.

  constants: tabela_dre type string value 'IT_DRE_DADOS_ALV'.

  data: text_n001 type c length 50,
        text_n002 type c length 50,
        text_n003 type c length 50,
        text_n004 type c length 50,
        text_n005 type c length 50,
        text_n006 type c length 50,
        text_n007 type c length 50,
        text_n008 type c length 50,
        text_n009 type c length 50,
        text_n010 type c length 50,
        text_n011 type c length 50,
        text_n012 type c length 50,
        text_n013 type c length 50,
        text_n014 type c length 50,
        text_n015 type c length 50,
        text_n016 type c length 50,
        text_n017 type c length 50,
        text_n018 type c length 50,
        text_n019 type c length 50.

  text_n001 = text-001.
  text_n002 = text-002.
  text_n003 = text-003.
  text_n004 = text-004.
  text_n005 = text-005.
  text_n006 = text-006.
  text_n007 = text-007.
  text_n008 = text-008.
  text_n009 = text-009.
  text_n010 = text-010.
  text_n011 = text-011.
  text_n012 = text-012.
  text_n013 = text-013.
  text_n014 = text-014.
  text_n015 = text-015.
  text_n016 = text-016.
  text_n017 = text-017.
  text_n018 = text-018.
  text_n019 = text-019.

  if prim_dre is initial.

    create object dre_container
      exporting
        container_name = 'CTN_DRE'.

    create object dre_alv
      exporting
        i_parent = dre_container.

    perform z_estrutura_fieldcat tables dre_catalogo using:
        tabela_dre 'ICOLI'       space     'X' 01 03 space space space 'X'   space space space,
        tabela_dre 'ICOST'       space     'X' 02 03 space space space 'X'   space space space,
        tabela_dre 'IREPO'       space     'X' 03 03 space space space 'X'   space space space,
        tabela_dre 'BUKRS'       text_n001 ' ' 04 05 space space space space space space space,
        tabela_dre 'BUTXT'       text_n002 ' ' 05 30 space space space space space space space,
        tabela_dre 'VERSN'       text_n003 ' ' 06 05 space space space space space space space,
        tabela_dre 'VSTXT'       text_n004 ' ' 07 25 space space space space space space space,
        tabela_dre 'WAERS'       text_n019 ' ' 08 05 space space space space space space space,
        tabela_dre 'GJAHR'       text_n005 ' ' 09 05 space space space space space space space,
        tabela_dre 'MONAT'       text_n006 ' ' 10 04 space space space space space space space,
        tabela_dre 'UNAME'       text_n007 ' ' 11 10 space space space space space space space,
        tabela_dre 'DATUM'       text_n008 ' ' 12 10 space space space space space space space,
        tabela_dre 'UZEIT'       text_n009 ' ' 13 10 space space space space space space space,
        tabela_dre 'UNAME_PROG'  text_n010 ' ' 14 10 space space space space space space space,
        tabela_dre 'DATUM_PROG'  text_n011 ' ' 15 10 space space space space space space space,
        tabela_dre 'UZEIT_PROG'  text_n012 ' ' 16 10 space space space space space space space,
        tabela_dre 'DATUM_INIC'  text_n013 ' ' 17 10 space space space space space space space,
        tabela_dre 'UZEIT_INIC'  text_n014 ' ' 18 10 space space space space space space space,
        tabela_dre 'DATUM_TERM'  text_n015 ' ' 19 10 space space space space space space space,
        tabela_dre 'UZEIT_TERM'  text_n016 ' ' 20 10 space space space space space space space,
        tabela_dre 'STATUS_PROT' text_n017 ' ' 21 09 space space space space space space space,
        tabela_dre 'MSGV1'       text_n018 ' ' 22 50 space space space space space space space,
        tabela_dre 'MSGV2'       text_n018 ' ' 23 50 space space space space space space space,
        tabela_dre 'MSGV3'       text_n018 ' ' 24 50 space space space space space space space,
        tabela_dre 'MSGV4'       text_n018 ' ' 25 50 space space space space space space space.

    clear: dre_gs_layout.
    dre_gs_layout-zebra      = c_x.
    dre_gs_layout-sel_mode   = 'A'.
    dre_gs_layout-info_fname = 'ROWCOLOR'.

    data: wa_sort type lvc_s_sort.
    wa_sort-spos      = 1.
    wa_sort-down      = abap_true.
    wa_sort-fieldname = 'GJAHR'.
    append wa_sort to dre_sort.
    wa_sort-spos      = 2.
    wa_sort-down      = abap_true.
    wa_sort-fieldname = 'MONAT'.
    append wa_sort to dre_sort.

    "Liberado
    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_system_okay.
    wa_hints-text      = text-020.
    wa_hints-fieldname = 'ICOLI'.
    append wa_hints to dre_except_qinfo.

    "Importar
    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_import_all_requests.
    wa_hints-text      = text-021.
    wa_hints-fieldname = 'ICOLI'.
    append wa_hints to dre_except_qinfo.

    "Processada
    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_system_okay.
    wa_hints-text      = text-022.
    wa_hints-fieldname = 'ICOLI'.
    append wa_hints to dre_except_qinfo.

    "Processando
    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_led_yellow.
    wa_hints-text      = text-023.
    wa_hints-fieldname = 'ICOLI'.
    append wa_hints to dre_except_qinfo.

    "Status nao definido DRE
*    WA_HINTS-TYPE      = CL_SALV_TOOLTIP=>C_TYPE_SYMBOL.
*    WA_HINTS-VALUE     = ICON_STATUS.
*    WA_HINTS-TEXT      = TEXT-024.
*    WA_HINTS-FIELDNAME = 'ICOST'.
*    APPEND WA_HINTS TO DRE_EXCEPT_QINFO.

    "Finalizado DRE
    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_led_green.
    wa_hints-text      = text-024.
    wa_hints-fieldname = 'ICOST'.
    append wa_hints to dre_except_qinfo.

    "Processando DRE
    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_led_yellow.
    wa_hints-text      = text-023.
    wa_hints-fieldname = 'ICOST'.
    append wa_hints to dre_except_qinfo.

    "Cancelado DRE
    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_led_red.
    wa_hints-text      = text-027.
    wa_hints-fieldname = 'ICOST'.
    append wa_hints to dre_except_qinfo.

    "Atualizar DRE
    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_refresh.
    wa_hints-text      = text-025.
    wa_hints-fieldname = 'IREPO'.
    append wa_hints to dre_except_qinfo.

    "Sem DRE Processada
    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_status.
    wa_hints-text      = text-026.
    wa_hints-fieldname = 'IREPO'.
    append wa_hints to dre_except_qinfo.

    call method dre_alv->set_table_for_first_display
      exporting
        i_default       = space
        is_layout       = dre_gs_layout
        it_except_qinfo = dre_except_qinfo
      changing
        it_fieldcatalog = dre_catalogo
        it_sort         = dre_sort
        it_outtab       = it_dre_dados_alv[].

*   Create Object for Event Handler
    create object dre_event_dre.
    set handler dre_event_dre->handle_hotspot_dre for dre_alv.

    prim_dre = c_x.
  endif.

  call method dre_alv->refresh_table_display.

  call method dre_alv->set_scroll_info_via_id
    exporting
      is_col_info = wa_scroll_col
      is_row_no   = wa_scroll_row.

endmodule.                 " CRIA_ALV_DRES  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_VERIFICA_SELECAO  text
*----------------------------------------------------------------------*
form verifica_selecao_dre  using  vg_verifica_selecao type sy-subrc.

  data: it_selected_rows type lvc_t_row,
        wa_selected_rows type lvc_s_row.

  clear: wa_dre_dados_alv.

  call method dre_alv->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  loop at it_selected_rows into wa_selected_rows.
    read table it_dre_dados_alv into wa_dre_dados_alv index wa_selected_rows-index.
  endloop.

  if not wa_dre_dados_alv is initial.

    perform seleciona_dre using wa_dre_dados_alv.
  else.
    message s004.
    vg_verifica_selecao = 1.
  endif.

endform.                    " VERIFICA_SELECAO_DRE

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DRE
*&---------------------------------------------------------------------*
form seleciona_dre  using p_dre type tp_dre_dados.

  if wa_dre_dados_alv-status eq '3'.
    vg_verifica_selecao = 0.
    if sy-ucomm ne ok_cexcel.
      tab_strip-activetab = ok_result.
      tl_0001             = tl_1002.
      vg_gerar_rel        = c_x.
    endif.
  else.
    message s003.
    vg_verifica_selecao = 1.
  endif.

endform.                    " SELECIONA_DRE

*&---------------------------------------------------------------------*
*&      Form  LIBERAR_DRE
*&---------------------------------------------------------------------*
form liberar_dre  using  p_dre type tp_dre_dados.

  move-corresponding p_dre to zgl020_dre_dados.

  clear: zgl020_dre_dados-datum_inic,
         zgl020_dre_dados-uzeit_inic,
         zgl020_dre_dados-datum_term,
         zgl020_dre_dados-uzeit_term,
         zgl020_dre_dados-status_proc,
         zgl020_dre_dados-status.

  zgl020_dre_dados-datum_prog = sy-datum.
  zgl020_dre_dados-uzeit_prog = sy-uzeit.
  zgl020_dre_dados-liberado   = 'X'.
  call screen 1104 starting at 07 05 ending at 75 12.

endform.                    " LIBERAR_DRE
*&---------------------------------------------------------------------*
*&      Form  CANCELA_PROCESSAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cancela_processamento .
  data: it_selected_rows type lvc_t_row,
        wa_selected_rows type lvc_s_row.

  call method dre_alv->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  loop at it_selected_rows into wa_selected_rows.
    read table it_dre_dados_alv into wa_dre_dados_alv index wa_selected_rows-index.
    if wa_dre_dados_alv-liberado is not initial.
      move-corresponding wa_dre_dados_alv to zgl020_dre_dados.
      zgl020_dre_dados-status = '2'.
      clear: zgl020_dre_dados-liberado.
      modify zgl020_dre_dados.
    endif.
  endloop.
  perform consultar_dres.
endform.                    " CANCELA_PROCESSAMENTO
*&---------------------------------------------------------------------*
*&      Form  EXPORTA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exporta_dados .
  data: vlr_rea(20),
        vlr_dolar(20),
        vlr_grupo(20),
        qtd_ton(20),
        v_caminho     type string,
        wl_cont       type sy-tabix,
        it_excel_aux  like table of it_excel with header line,
        tx_aux        type c length 30.

  refresh: it_file.
  loop at it_excel.
    collect it_excel into it_excel_aux.
  endloop.
  refresh: it_excel.
  it_excel[] = it_excel_aux[].

  loop at it_excel.
    if sy-tabix eq 1.
      "Empresa
      message s016 into tx_aux.
      concatenate tx_aux ';' into it_file-field.

      "Estrutura
      message s017 into tx_aux.
      concatenate it_file-field tx_aux ';' into it_file-field.

      "Mês
      message s018 into tx_aux.
      concatenate it_file-field tx_aux ';' into it_file-field.

      "Ano
      message s019 into tx_aux.
      concatenate it_file-field tx_aux ';' into it_file-field.

      "Nível
      message s020 into tx_aux.
      concatenate it_file-field tx_aux ';' into it_file-field.

      "Conta
      message s021 into tx_aux.
      concatenate it_file-field tx_aux ';' into it_file-field.

      "Descrição
      message s022 into tx_aux.
      concatenate it_file-field tx_aux ';' into it_file-field.

      "Qtd TO
      message s023 into tx_aux.
      concatenate it_file-field tx_aux ';' into it_file-field.

      "Moeda Interna
      message s024 into tx_aux.
      concatenate it_file-field tx_aux ';' into it_file-field.

      "Moeda Forte
      message s025 into tx_aux.
      concatenate it_file-field tx_aux ';' into it_file-field.

      "Moeda Grupo
      message s026 into tx_aux.
      concatenate it_file-field tx_aux ';' into it_file-field.

      "IT_FILE-FIELD = 'Empresa;Estrutura;Mês;Ano;Nivel;Conta;Descrição;qtd ton;Moeda Interna;Moeda Forte;Moeda Grupo;'.
      append it_file.
    endif.

    write it_excel-qtd_ton to qtd_ton.
    wl_cont = strlen( qtd_ton ).
    subtract 1 from wl_cont.
    if qtd_ton+wl_cont(1) eq '-'.
      translate qtd_ton using '- '.
      concatenate '-' qtd_ton into qtd_ton.
    endif.

    write it_excel-vlr_rea   to vlr_rea.
    wl_cont = strlen( vlr_rea ).
    subtract 1 from wl_cont.
    if vlr_rea+wl_cont(1) eq '-'.
      translate vlr_rea using '- '.
      concatenate '-' vlr_rea into vlr_rea.
    endif.

    write it_excel-vlr_dolar to vlr_dolar.
    wl_cont = strlen( vlr_dolar ).
    subtract 1 from wl_cont.
    if vlr_dolar+wl_cont(1) eq '-'.
      translate vlr_dolar using '- '.
      concatenate '-' vlr_dolar into vlr_dolar.
    endif.

    write it_excel-vlr_grupo to vlr_grupo.
    wl_cont = strlen( vlr_grupo ).
    subtract 1 from wl_cont.
    if vlr_grupo+wl_cont(1) eq '-'.
      translate vlr_grupo using '- '.
      concatenate '-' vlr_grupo into vlr_grupo.
    endif.

    concatenate it_excel-bukrs
                it_excel-versn
                it_excel-monat
                it_excel-gjahr
                it_excel-nivel
                it_excel-saknr
                it_excel-desc
                qtd_ton
                vlr_rea
                vlr_dolar
                vlr_grupo into it_file-field separated by ';'.

    append it_file.
*    collect it_file.
    clear: it_file, qtd_ton, vlr_rea, vlr_dolar, vlr_grupo.
  endloop.

  wl_cont = strlen( wg_file_local ).
  subtract 4 from wl_cont.
  if wg_file_local+wl_cont(4) ne '.CSV'.
    concatenate wg_file_local '.CSV' into v_caminho.
  else.
    move wg_file_local to v_caminho.
  endif.

  call function 'GUI_DOWNLOAD'
    exporting
*     bin_filesize            = numbytes
      filename                = v_caminho
      filetype                = 'ASC'
    tables
      data_tab                = it_file
    exceptions
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6.

endform.                    " EXPORTA_DADOS
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_1105 input.

  case sy-ucomm.
    when ok_save.
      leave to screen 0.
  endcase.
endmodule.                 " USER_COMMAND_1105  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_1105 output.
  set pf-status 'PF1103'.
  set titlebar 'TL1105'.

endmodule.                 " STATUS_1105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  BUSCA_FILE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module busca_file input.


  call function 'KD_GET_FILENAME_ON_F4'
    exporting
      field_name    = 'C:\'
* FC - UPGRADE ECC 6.0 - LUP xxx - Início
*     MASK          = '.txt '
* FC - UPGRADE ECC6.0 - LUP xxx - Fim
    changing
      file_name     = wg_file_local
    exceptions
      mask_too_long = 1
      others        = 2.
  case sy-subrc.
    when 1.
      message e014.
    when 2.
      message e015.
      "MESSAGE e000(zb) WITH 'Ocorreu um erro.'.
  endcase.
endmodule.                 " BUSCA_FILE  INPUT
*&---------------------------------------------------------------------*
*&      Form  FM_ZGL060_TABELA_DERIVADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_zgl060_tabela_derivada .
  "USER STORY 102736 / AOENNING
  data: it_selected_rows type lvc_t_row,
        wa_selected_rows type lvc_s_row,
        lines            type sy-tabix.

  clear: wa_selected_rows, lines, wa_dre_dados_alv, ws_x001, wa_color, ws_moedaint, ws_moedafor, ws_moedaind, wa_zgl060_tabderivada.
  free: it_zglt_dre_02[], it_zglt_dre_04[], it_param_, it_zgl060_tabderivada.
  call method dre_alv->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  describe table it_selected_rows lines lines.

  if ( lines is initial ).
    message text-e01 type 'I' display like 'E'.

  else.
    if  lines > 1.
      message text-e02 type 'I' display like 'E'.
      exit.
    endif.

    read table it_selected_rows into wa_selected_rows index 1.

    "Seleção valores transação ZGL060.
    read table it_dre_dados_alv into wa_dre_dados_alv index wa_selected_rows-index.
    if wa_dre_dados_alv is not initial.

      rg_monat = value #( ( sign = 'I' option = 'EQ' low = wa_dre_dados_alv-monat ) ).
      if wa_dre_dados_alv-monat eq '12'.
        rg_monat = value #( ( sign = 'I' option = 'BT' low = wa_dre_dados_alv-monat high = '15' ) ).
      endif.


      "Seleção valores transação ZGL060.
*      SELECT bukrs, gjahr, poper, saknr,  kostl, kosar, prctr, matnr, matkl, aufnr, vbund, SUM( vlhsl ) AS vlhsl, SUM( vlksl ) AS vlksl, SUM( vlosl ) AS vlosl
      select bukrs, gjahr, poper, saknr, sum( vlhsl ) as vlhsl, sum( vlksl ) as vlksl, sum( vlosl ) as vlosl
         from zglt_dre_04  into corresponding fields of table @it_zglt_dre_04
         where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat "BUG SOLTO 167163 / AOENNING.
        group by  bukrs, gjahr, poper, saknr. ",  kostl, kosar, prctr, matnr, matkl, aufnr, vbund.
      if sy-subrc eq 0.
        it_zgl060_tabderivada  = value #( for l in it_zglt_dre_04[] (
          bukrs  = l-bukrs
          gjahr  = l-gjahr
          monat  = l-poper
          saknr  = l-saknr
*          kostl  = l-kostl
*          kosar  = l-kosar
*          prctr  = l-prctr
*          matnr  = l-matnr
*          matkl  = l-matkl
*          aufnr  = l-aufnr
*          vbund  = l-vbund
           ) ).
      endif.

      "Seleção valores Tabela Derivada.
      select bukrs, gjahr, poper, saknr, sum( vlhsl ) as vlhsl, sum( vlksl ) as vlksl, sum( vlosl ) as vlosl
        from zglt_dre_02  into corresponding fields of table @it_zglt_dre_02
        where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat "BUG SOLTO 167163 / AOENNING.
        group by  bukrs, gjahr, poper, saknr.
      if sy-subrc eq 0.
        loop at it_zglt_dre_02 into data(ws_zglt_dre_02).
          append value #(
          bukrs = ws_zglt_dre_02-bukrs
          gjahr = ws_zglt_dre_02-gjahr
          monat = ws_zglt_dre_02-poper
          saknr = ws_zglt_dre_02-saknr
*          kostl = ws_zglt_dre_02-kostl
*          kosar = ws_zglt_dre_02-kosar
*          prctr = ws_zglt_dre_02-prctr
*          matnr = ws_zglt_dre_02-matnr
*          matkl = ws_zglt_dre_02-matkl
*          aufnr = ws_zglt_dre_02-aufnr
*          vbund = ws_zglt_dre_02-vbund
          ) to it_zgl060_tabderivada."it_param_.
        endloop.
      endif.

      if it_zgl060_tabderivada is not initial.
        sort it_zgl060_tabderivada by bukrs gjahr monat saknr. "kostl kosar prctr matnr matkl aufnr vbund.
        delete adjacent duplicates from it_zgl060_tabderivada  comparing  bukrs gjahr monat saknr. "kostl kosar prctr matnr matkl aufnr vbund.

        sort: it_zgl060_tabderivada by bukrs gjahr saknr monat, "kostl kosar prctr matnr matkl aufnr vbund,
         it_zglt_dre_04 by bukrs gjahr saknr poper, "kostl kosar prctr matnr matkl aufnr vbund,
         it_zglt_dre_02 by bukrs gjahr saknr poper. "kostl kosar prctr matnr matkl aufnr vbund.


*        LOOP AT it_param_ INTO DATA(ls_param).
        loop at it_zgl060_tabderivada assigning field-symbol(<ls_derivada>).

*          wa_zgl060_tabderivada-bukrs = ls_param-bukrs.
*          wa_zgl060_tabderivada-gjahr = ls_param-gjahr.
*          wa_zgl060_tabderivada-monat = ls_param-monat.
*          wa_zgl060_tabderivada-saknr = ls_param-saknr.
*          wa_zgl060_tabderivada-kostl = ls_param-kostl.
*          wa_zgl060_tabderivada-kosar = ls_param-kosar.
*          wa_zgl060_tabderivada-prctr = ls_param-prctr.
*          wa_zgl060_tabderivada-matnr = ls_param-matnr.
*          wa_zgl060_tabderivada-matkl = ls_param-matkl.
*          wa_zgl060_tabderivada-aufnr = ls_param-aufnr.
*          wa_zgl060_tabderivada-vbund = ls_param-vbund.

          read table it_zglt_dre_04 assigning field-symbol(<ls_dre_04>) with key bukrs = <ls_derivada>-bukrs
                                             gjahr = <ls_derivada>-gjahr
                                             saknr = <ls_derivada>-saknr
                                             poper = <ls_derivada>-monat binary search.
          if sy-subrc eq 0.
            <ls_derivada>-moedaint_zgl = <ls_dre_04>-vlhsl.
            <ls_derivada>-moedafor_zgl = <ls_dre_04>-vlksl.
            <ls_derivada>-moedaind_zgl = <ls_dre_04>-vlosl.
            <ls_dre_04>-zcheck = abap_true.
          endif.

          read table it_zglt_dre_02 assigning field-symbol(<ls_dre_02>) with key bukrs = <ls_derivada>-bukrs
                                             gjahr = <ls_derivada>-gjahr
                                             saknr = <ls_derivada>-saknr
                                             poper = <ls_derivada>-monat binary search.

          if sy-subrc eq 0.
            <ls_derivada>-moedaint_der  = <ls_dre_02>-vlhsl.
            <ls_derivada>-moedafor_der  = <ls_dre_02>-vlksl.
            <ls_derivada>-moedaind_der  = <ls_dre_02>-vlosl.
            <ls_dre_02>-zcheck = abap_true.
          endif.

          "Aplicar calculo de diferença.
*          IF wa_zgl060_tabderivada-moedaint_zgl IS NOT INITIAL AND wa_zgl060_tabderivada-moedaint_der IS NOT INITIAL.
          <ls_derivada>-dif_moedaint = <ls_derivada>-moedaint_zgl - <ls_derivada>-moedaint_der.
*          ENDIF.

*          IF wa_zgl060_tabderivada-moedafor_zgl IS NOT INITIAL AND wa_zgl060_tabderivada-moedafor_der IS NOT INITIAL.
          <ls_derivada>-dif_moedafor = <ls_derivada>-moedafor_zgl - <ls_derivada>-moedafor_der.
*          ENDIF.

*          IF wa_zgl060_tabderivada-moedaind_zgl IS NOT INITIAL AND wa_zgl060_tabderivada-moedaind_der IS NOT INITIAL.
          <ls_derivada>-dif_moedaind = <ls_derivada>-moedaind_zgl - <ls_derivada>-moedaind_der.
*          ENDIF.

          clear wa_color.
          append value #( fname = 'MOEDAINT_ZGL'  color-col = '5' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAINT_DER'  color-col = '5' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'DIF_MOEDAINT'  color-col = '5' color-int = '0' color-inv = '0') to it_color.

          append value #( fname = 'MOEDAFOR_ZGL'  color-col = '2' color-int = '1' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAFOR_DER'  color-col = '2' color-int = '1' color-inv = '0') to it_color.
          append value #( fname = 'DIF_MOEDAFOR'  color-col = '2' color-int = '1' color-inv = '0') to it_color.

          append value #( fname = 'MOEDAIND_ZGL'  color-col = '4' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAIND_DER'  color-col = '4' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'DIF_MOEDAIND'  color-col = '4' color-int = '0' color-inv = '0') to it_color.

          <ls_derivada>-cell_color[] = it_color[].

*          APPEND wa_zgl060_tabderivada TO it_zgl060_tabderivada.

          clear: wa_fbl3n_zgl060, it_zglt_dre_02, it_zglt_dre_04.
        endloop.

      endif.

      "identificar a moeda Forte e a Moeda Índice.
      clear: ws_x001.
      call function 'FI_CURRENCY_INFORMATION'
        exporting
          i_bukrs = wa_dre_dados_alv-bukrs
        importing
          e_x001  = ws_x001.


      ws_moedafor = ws_x001-hwae2.
      ws_moedaind = ws_x001-hwae3.

      "Seleção do tipo de moeda interna.
      clear: ws_moedaint.
      select single waers from t001 into ws_moedaint where bukrs eq wa_dre_dados_alv-bukrs.

      call screen 9013.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9013  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9013 output.
  set pf-status 'ST9013'.
  set titlebar 'T-9013'.

  perform fm_criar_objetos_9013.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9013  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9013 input.

  case sy-ucomm.
    when 'EXIT'.
*      CALL METHOD gob_gui_alv_9013->free.
      leave to screen 0.
    when others.
  endcase.

endmodule.
*&---------------------------------------------------------------------*
*&      Form  FM_FBL3N_ZGL060
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_fbl3n_zgl060 .
  "USER STORY 75412 / AOENNING
  data: it_contas type zct_emp_contas,
        lv_conta  type racct,
        p_ajuste  type char01.


  data: it_selected_rows type lvc_t_row,
        wa_selected_rows type lvc_s_row,
        lines            type sy-tabix.

  clear: wa_selected_rows, lines, wa_dre_dados_alv, it_contas, ws_x001, wa_color, ws_moedaint, ws_moedafor, ws_moedaind.
  free: it_fbl3n_zgl060[], it_selected_rows[], it_param[], it_zglt_dre_04[], it_saldo[], it_saldo2[], it_saldo3[], it_0300[].

  call method dre_alv->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  describe table it_selected_rows lines lines.

  if ( lines is initial ).
    message text-e01 type 'I' display like 'E'.

  else.
    if  lines > 1.
      message text-e02 type 'I' display like 'E'.
      exit.
    endif.

    read table it_selected_rows into wa_selected_rows index 1.

    "Seleção valores transação ZGL060.
    read table it_dre_dados_alv into wa_dre_dados_alv index wa_selected_rows-index.
    if wa_dre_dados_alv is not initial.

      clear: it_contas.
      it_contas = value #( ( bukrs = wa_dre_dados_alv-bukrs ) ).

      "Seleção valores que corresponde FBL3N.

      call function 'Z_FI_GL_SALDO_FAGLFLEXT'
        exporting
          ryear         = wa_dre_dados_alv-gjahr
          contas        = it_contas
          p_gerar_todas = abap_true
        tables
          it_saldos     = it_saldo
          it_saldos_2   = it_saldo2
          it_saldos_3   = it_saldo3
        exceptions
          moeda_nao_adm = 1
          erro_ledger   = 2
          others        = 3.


      if sy-subrc eq 0.

        "Ordenar.
        sort:  it_saldo[] by racct, it_saldo2[] by racct, it_saldo3[] by racct.
        lv_conta = '0000311000'.

        "Deletar contas diferente 311000.
        delete it_saldo[] where racct < lv_conta.
        delete it_saldo2[] where racct < lv_conta.
        delete it_saldo3[] where racct < lv_conta.

        if sy-subrc is not initial.
          sy-msgid = 'Não identificamos valores para conta selecionada'.
          message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          stop.
        else.
          loop at it_saldo.
            clear: it_0300.
            it_0300-bukrs	= it_saldo-rbukrs.
            it_0300-gjahr = it_saldo-ryear.
            it_0300-poper = wa_dre_dados_alv-monat.
            it_0300-saknr = it_saldo-racct.
            perform busca_valor_mes using it_saldo wa_dre_dados_alv-monat changing it_0300-vlhsl.
            read table it_saldo2 with key racct = it_saldo-racct binary search.
            perform busca_valor_mes using it_saldo2 wa_dre_dados_alv-monat changing it_0300-vlksl.
            read table it_saldo3 with key racct = it_saldo-racct binary search.
            perform busca_valor_mes using it_saldo3 wa_dre_dados_alv-monat changing it_0300-vlosl.
            append it_0300.
          endloop.
        endif.

        if it_0300[] is not initial.
          it_param  = value #( for l in it_0300[] (
          bukrs = l-bukrs
          gjahr = l-gjahr
          monat = l-poper
          saknr = l-saknr
          ) ).
        endif.

*>>>>>"BUG SOLTO 167163 / AOENNING.
        free: rg_monat.
        rg_monat = value #( ( sign = 'I' option = 'EQ' low = wa_dre_dados_alv-monat ) ).
        if wa_dre_dados_alv-monat eq '12'.
          rg_monat = value #( ( sign = 'I' option = 'BT' low = wa_dre_dados_alv-monat high = '15' ) ).
        endif.
*>>>>>"BUG SOLTO 167163 / AOENNING.

        "Seleção valores transação ZGL060.
        select bukrs, gjahr, poper, saknr, sum( vlhsl ) as vlhsl, sum( vlksl ) as vlksl, sum( vlosl ) as vlosl
        from zglt_dre_04  into corresponding fields of table @it_zglt_dre_04
        where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat "BUG SOLTO 167163 / AOENNING.
        group by  bukrs, gjahr, poper, saknr.

        if sy-subrc eq 0.
          loop at it_zglt_dre_04 into data(ws_zglt_dre_04).
            append value #( bukrs = ws_zglt_dre_04-bukrs
            gjahr = ws_zglt_dre_04-gjahr
            monat = ws_zglt_dre_04-poper
            saknr = ws_zglt_dre_04-saknr

            ) to it_param.
          endloop.
        endif.

        if it_param is not initial.
          sort it_param by bukrs gjahr saknr monat.
          delete adjacent duplicates from it_param comparing bukrs gjahr saknr monat.
        endif.

        "identificar a moeda Forte e a Moeda Índice.
        clear: ws_x001.
        call function 'FI_CURRENCY_INFORMATION'
          exporting
            i_bukrs = wa_dre_dados_alv-bukrs
          importing
            e_x001  = ws_x001.


        ws_moedafor = ws_x001-hwae2.
        ws_moedaind = ws_x001-hwae3.

        "Seleção do tipo de moeda interna.
        clear: ws_moedaint.
        select single waers from t001 into ws_moedaint where bukrs eq wa_dre_dados_alv-bukrs.

        sort: it_param by bukrs gjahr saknr monat,
              it_0300 by bukrs gjahr saknr poper,
              it_zglt_dre_04 by bukrs gjahr saknr poper.


        loop at it_param into data(ls_param).
          wa_fbl3n_zgl060-bukrs = ls_param-bukrs.
          wa_fbl3n_zgl060-gjahr = ls_param-gjahr.
          wa_fbl3n_zgl060-monat = ls_param-monat.
          wa_fbl3n_zgl060-saknr = ls_param-saknr.

          read table it_0300 with key bukrs = ls_param-bukrs gjahr = ls_param-gjahr  saknr = ls_param-saknr poper = ls_param-monat binary search.
          if sy-subrc eq 0.
            wa_fbl3n_zgl060-moedaint_fbl3n   = it_0300-vlhsl.
            wa_fbl3n_zgl060-moedafor_fbl3n = it_0300-vlksl.
            wa_fbl3n_zgl060-moedaind_fbl3n = it_0300-vlosl.
          endif.

          read table it_zglt_dre_04 with key bukrs = ls_param-bukrs gjahr = ls_param-gjahr  saknr = ls_param-saknr poper = ls_param-monat binary search.
          if sy-subrc eq 0.
            wa_fbl3n_zgl060-moedaint_zgl060 = it_zglt_dre_04-vlhsl.
            wa_fbl3n_zgl060-moedafor_zgl060 = it_zglt_dre_04-vlksl.
            wa_fbl3n_zgl060-moedaind_zgl060 = it_zglt_dre_04-vlosl.
          endif.

          "Aplicar calculo de diferença.
*          IF wa_fbl3n_zgl060-moedaint_fbl3n IS NOT INITIAL AND wa_fbl3n_zgl060-moedaint_zgl060 IS NOT INITIAL.
          wa_fbl3n_zgl060-dif_moedaint = wa_fbl3n_zgl060-moedaint_fbl3n - wa_fbl3n_zgl060-moedaint_zgl060.
*          ENDIF.

*          IF wa_fbl3n_zgl060-moedafor_fbl3n IS NOT INITIAL AND wa_fbl3n_zgl060-moedafor_zgl060 IS NOT INITIAL.
          wa_fbl3n_zgl060-dif_moedafor = wa_fbl3n_zgl060-moedafor_fbl3n - wa_fbl3n_zgl060-moedafor_zgl060.
*          ENDIF.

*          IF wa_fbl3n_zgl060-moedaind_fbl3n IS NOT INITIAL AND wa_fbl3n_zgl060-moedaind_zgl060 IS NOT INITIAL.
          wa_fbl3n_zgl060-dif_moedaind = wa_fbl3n_zgl060-moedaind_fbl3n - wa_fbl3n_zgl060-moedaind_zgl060.
*          ENDIF.

          clear wa_color.
          append value #( fname = 'MOEDAINT_FBL3N'  color-col = '5' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAINT_ZGL060' color-col = '5' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'DIF_MOEDAINT   ' color-col = '5' color-int = '0' color-inv = '0') to it_color.

          append value #( fname = 'MOEDAFOR_FBL3N'  color-col = '2' color-int = '1' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAFOR_ZGL060' color-col = '2' color-int = '1' color-inv = '0') to it_color.
          append value #( fname = 'DIF_MOEDAFOR'    color-col = '2' color-int = '1' color-inv = '0') to it_color.

          append value #( fname = 'MOEDAIND_FBL3N'  color-col = '4' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAIND_ZGL060' color-col = '4' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'DIF_MOEDAIND'    color-col = '4' color-int = '0' color-inv = '0') to it_color.

          wa_fbl3n_zgl060-cell_color[] = it_color[].

          append wa_fbl3n_zgl060 to it_fbl3n_zgl060.
          clear: wa_fbl3n_zgl060, it_zglt_dre_04, ls_param, it_0300.
        endloop.

        sort it_fbl3n_zgl060 by monat.
        data(it_fbl3n_zgl060_aux) = it_fbl3n_zgl060.
        delete it_fbl3n_zgl060 where monat > 12.
        delete it_fbl3n_zgl060_aux where monat < 13.

        sort it_fbl3n_zgl060 by bukrs gjahr saknr.
        loop at it_fbl3n_zgl060 assigning field-symbol(<ws_zgl060>) where monat > 11.
          clear: p_ajuste.
          loop at it_fbl3n_zgl060_aux assigning field-symbol(<ws_zgl060_aux>) where bukrs eq <ws_zgl060>-bukrs
                                                                                and gjahr eq <ws_zgl060>-gjahr
                                                                                and saknr eq <ws_zgl060>-saknr.

            if <ws_zgl060_aux>-monat > 12.
              <ws_zgl060>-moedaint_zgl060 = <ws_zgl060>-moedaint_zgl060 + <ws_zgl060_aux>-moedaint_zgl060.
              <ws_zgl060>-moedafor_zgl060 = <ws_zgl060>-moedafor_zgl060 + <ws_zgl060_aux>-moedafor_zgl060.
              <ws_zgl060>-moedaind_zgl060 = <ws_zgl060>-moedaind_zgl060 + <ws_zgl060_aux>-moedaind_zgl060.
              if p_ajuste is initial.
                p_ajuste = abap_true.
              endif.
            endif.
          endloop.

          if p_ajuste eq abap_true.
            <ws_zgl060>-dif_moedaint = <ws_zgl060>-moedaint_fbl3n - <ws_zgl060>-moedaint_zgl060.
            <ws_zgl060>-dif_moedafor = <ws_zgl060>-moedafor_fbl3n - <ws_zgl060>-moedafor_zgl060.
            <ws_zgl060>-dif_moedaind = <ws_zgl060>-moedaind_fbl3n - <ws_zgl060>-moedaind_zgl060.
          endif.
        endloop.


        "ALV.
        call screen 9014.

      endif.
    endif.
  endif.



endform.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9014  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9014 output.
  set pf-status 'ST9014'.
  set titlebar 'TIT9014'.

  perform fm_criar_objetos_9014.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9014  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9014 input.

  case sy-ucomm.
    when 'EXIT'.
*      CALL METHOD gob_gui_alv_9013->free.
      leave to screen 0.

    when others.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALOR_MES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SALDOS  text
*      -->P_P_MONAT_LOW  text
*      <--P_IT_0300_VLHSL  text
*----------------------------------------------------------------------*
form busca_valor_mes   using   p_saldo type zde_fi_gl_saldo_faglflext
                               p_monat type monat
                      changing p_valor type vlcur12.


  case p_monat.
    when 01.
      p_valor = p_saldo-sl01.
    when 02.
      p_valor = p_saldo-sl02.
    when 03.
      p_valor = p_saldo-sl03.
    when 04.
      p_valor = p_saldo-sl04.
    when 05.
      p_valor = p_saldo-sl05.
    when 06.
      p_valor = p_saldo-sl06.
    when 07.
      p_valor = p_saldo-sl07.
    when 08.
      p_valor = p_saldo-sl08.
    when 09.
      p_valor = p_saldo-sl09.
    when 10.
      p_valor = p_saldo-sl10.
    when 11.
      p_valor = p_saldo-sl11.
    when 12.
      p_valor = p_saldo-sl12 + p_saldo-sl13 + p_saldo-sl14 + p_saldo-sl15.
  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS_9014
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_criar_objetos_9014 .

  data: lva_data(22) type c,
        w_layout     type lvc_s_layo.


  data: objeto type ref to zcl_screen.
  create object objeto." TYPE ('zif_screen').

  free: git_fcat[].
  clear: w_layout.


  data: gs_variant  type disvariant.
  gs_variant-report      = sy-repid.

  if gob_gui_alv_9014 is initial.
    free: zcl_screen=>zif_screen~split.
  endif.

  perform fm_cria_fieldcat.

  clear: lva_data.
  concatenate sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) into lva_data.

  if objeto->zif_screen~set_criar_tela_padrao_report(
  exporting
     i_titulo  = 'Conferencia valores FBL3N x ZGL060'
     i_filtros = value zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
   changing
     alv = gob_gui_alv_9014
   )

     eq abap_true.

  endif.

*
*  CREATE OBJECT event_receiver.
*  SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.
*  SET HANDLER event_receiver->get_ucomm  FOR gob_gui_alv_grid.

  w_layout-info_fname = 'ROWCOLOR'.  "Row color
  w_layout-ctab_fname = 'CELL_COLOR'.
  w_layout-cwidth_opt = abap_true.
  w_layout-zebra      = 'X'.
  w_layout-sel_mode   = 'A'.
  w_layout-col_opt    = abap_true.


  call method gob_gui_alv_9014->set_table_for_first_display
    exporting
      is_layout                     = w_layout
      i_save                        = 'A'
      is_variant                    = gs_variant
    changing
      it_outtab                     = it_fbl3n_zgl060
      it_fieldcatalog               = git_fcat
*     IT_SORT                       =
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.


  call method gob_gui_alv_9014->refresh_table_display
    exporting
      is_stable = ls_stable_9014
    exceptions
      finished  = 1
      others    = 2.


endform.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_cria_fieldcat.

*  ws_moedaint
*  ws_moedafor
*  ws_moedaind


  types: lit_fieldcat_aux type table of lvc_s_fcat with default key.
  git_fcat = value lit_fieldcat_aux(
( fieldname ='BUKRS           '     coltext = 'Empresa                '   col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field = 'BURKS' )
( fieldname ='GJAHR           '     coltext = 'Ano                    '   col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field = 'GJAHR' )
( fieldname ='MONAT           '     coltext = 'Mês                    '   col_opt = 'X' no_zero = '' do_sum = ''  ref_table = ''            ref_field = '' )
( fieldname ='SAKNR           '     coltext = 'Conta                  '   col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field = 'SAKNR' )
( fieldname ='MOEDAINT_FBL3N  '     coltext = |{ ws_moedaint }-FBL3N|     col_opt = 'X' no_zero = '' do_sum = 'X' )
( fieldname ='MOEDAINT_ZGL060 '     coltext = |{ ws_moedaint }-ZGL060|    col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='DIF_MOEDAINT    '     coltext = |Dif-{ ws_moedaint }|       col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAFOR_FBL3N  '     coltext = |{ ws_moedafor }-FBL3N|     col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAFOR_ZGL060 '     coltext = |{ ws_moedafor }-ZGL060|    col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='DIF_MOEDAFOR    '     coltext = |Dif-{ ws_moedafor }|       col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAIND_FBL3N  '     coltext = |{ ws_moedaind }-FBL3N|     col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAIND_ZGL060 '     coltext = |{ ws_moedaind }-ZGL060|    col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='DIF_MOEDAIND    '     coltext = |Dif-{ ws_moedaind }|       col_opt = 'X' no_zero = '' do_sum = 'X')
).
endform.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS_9013
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_criar_objetos_9013 .

  data: lva_data(22) type c,
        w_layout     type lvc_s_layo.


  data: objeto type ref to zcl_screen.
  create object objeto. "TYPE ('zif_screen').

  free: git_fcat_[].
  clear: w_layout.

  if gob_gui_alv_9013 is initial.
    free: zcl_screen=>zif_screen~split.
  endif.

  data: gs_variant  type disvariant.
  gs_variant-report      = sy-repid.

  perform fm_cria_fieldcat_9013.

  clear: lva_data.
  concatenate sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) into lva_data.

  if objeto->zif_screen~set_criar_tela_padrao_report(
   exporting
    i_titulo  = 'Conferencia valores ZGL060 x Tabela Derivada'
    i_filtros = value zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
  changing
    alv = gob_gui_alv_9013
  )
     eq abap_true.
  endif.


  w_layout-info_fname = 'ROWCOLOR'.  "Row color
  w_layout-ctab_fname = 'CELL_COLOR'.
  w_layout-cwidth_opt = abap_true.
  w_layout-zebra      = 'X'.
  w_layout-sel_mode   = 'A'.
  w_layout-col_opt    = abap_true.


  call method gob_gui_alv_9013->set_table_for_first_display
    exporting
      is_layout                     = w_layout
      i_save                        = 'A'
      is_variant                    = gs_variant
    changing
      it_outtab                     = it_zgl060_tabderivada
      it_fieldcatalog               = git_fcat_
*     IT_SORT                       =
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.




  call method gob_gui_alv_9013->refresh_table_display
    exporting
      is_stable = ls_stable_9013
    exceptions
      finished  = 1
      others    = 2.



endform.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT_9013
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_cria_fieldcat_9013 .


  types: lit_fieldcat_aux type table of lvc_s_fcat with default key.
  git_fcat_ = value lit_fieldcat_aux(
( fieldname ='BUKRS '               coltext = 'Empresa                '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='BUKRS '  )
( fieldname ='GJAHR '               coltext = 'Ano                    '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='GJAHR '  )
( fieldname ='MONAT '               coltext = 'Mês                    '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='MONAT '  )
( fieldname ='SAKNR '               coltext = 'Conta                  '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='SAKNR '  )
*( fieldname ='KOSTL '               coltext = 'C.custo                '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='KOSTL '  )
*( fieldname ='KOSAR '               coltext = 'Tp.c.custo             '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='KOSAR '  )
*( fieldname ='PRCTR '               coltext = 'C.lucro                '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='PRCTR '  )
*( fieldname ='MATNR '               coltext = 'Nº do material         '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='MATNR '  )
*( fieldname ='MATKL '               coltext = 'Grp.mercadorias        '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='MATKL '  )
*( fieldname ='AUFNR '               coltext = 'Nº ordem'                   col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='AUFNR '  )
*( fieldname ='VBUND '               coltext = 'Nº sociedade'               col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='VBUND '  )
( fieldname ='MOEDAINT_ZGL    '     coltext = |{ ws_moedaint }-ZGL060|     col_opt = 'X' no_zero = '' do_sum = 'X' )
( fieldname ='MOEDAINT_DER    '     coltext = |{ ws_moedaint }-T.DERIV|    col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='DIF_MOEDAINT    '     coltext = |Dif-{ ws_moedaint }|        col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAFOR_ZGL    '     coltext = |{ ws_moedafor }-ZGL060|     col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAFOR_DER    '     coltext = |{ ws_moedafor }-T.DERIV|    col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='DIF_MOEDAFOR    '     coltext = |Dif-{ ws_moedafor }|        col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAIND_ZGL    '     coltext = |{ ws_moedaind }-ZGL060|        col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAIND_DER    '     coltext = |{ ws_moedaind }-T.DERIV|    col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='DIF_MOEDAIND    '     coltext = |Dif-{ ws_moedaind }|        col_opt = 'X' no_zero = '' do_sum = 'X')
).

endform.
*&---------------------------------------------------------------------*
*&      Form  FM_TB_DER_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_tb_der_dre .

  "USER STORY 102736 / AOENNING
  data: it_selected_rows type lvc_t_row,
        wa_selected_rows type lvc_s_row,
        lines            type sy-tabix,
        p_ajuste         type char01.

  data: r_saknr     type range of saknr,
        r_saknr_cc  type range of saknr,
        r_saknr_cl  type range of saknr,
        r_saknr_raz type range of saknr,
        r_saknr_mat type range of saknr,
        r_saknr_avu type range of saknr,
        r_saknr_dv  type range of saknr,
        r_aufnr     type range of aufnr,
        r_kostl     type range of kostl,
        r_prctr     type range of prctr.


  clear: wa_selected_rows, lines, wa_dre_dados_alv, ws_x001, wa_color, ws_moedaint, ws_moedafor, ws_moedaind, wa_zgl060_tabderivada.
  free: it_zglt_dre_02[], it_zglt_dre_04[], it_tabderivada_dre[], it_dre[],
  it_param_dre[], it_dre_02_tbdr[], it_dre_02_cust[], r_saknr_avu[], r_saknr_raz[],
  r_saknr_cc[], r_saknr_cl[], r_saknr_mat[], r_aufnr[], r_saknr[], it_total_cust_cc[].

  call method dre_alv->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  describe table it_selected_rows lines lines.

  if ( lines is initial ).
    message text-e01 type 'I' display like 'E'.

  else.
    if  lines > 1.
      message text-e02 type 'I' display like 'E'.
      exit.
    endif.

    read table it_selected_rows into wa_selected_rows index 1.

    "Seleção valores.
    read table it_dre_dados_alv into wa_dre_dados_alv index wa_selected_rows-index.

*>>>>>"BUG SOLTO 167163 / AOENNING.
    free: rg_monat.
    rg_monat = value #( ( sign = 'I' option = 'EQ' low = wa_dre_dados_alv-monat ) ).
    if wa_dre_dados_alv-monat eq '12'.
      rg_monat = value #( ( sign = 'I' option = 'BT' low = wa_dre_dados_alv-monat high = '15' ) ).
    endif.
*>>>>>"BUG SOLTO 167163 / AOENNING.

*1.1 ==============Busca das contas na Tabela Derivada============================================================
    if wa_dre_dados_alv is not initial.
      "Seleção valores Tabela Derivada.
      free: it_zglt_dre_02[].
      select * from zglt_dre_02  into table it_zglt_dre_02
        where bukrs eq wa_dre_dados_alv-bukrs
        and gjahr eq wa_dre_dados_alv-gjahr
        and poper in rg_monat.
      if sy-subrc eq 0.
        sort it_zglt_dre_02[] by saknr.
        delete adjacent duplicates from it_zglt_dre_02[] comparing saknr.
        r_saknr = value #( for ls in it_zglt_dre_02 ( sign = 'I' option = 'EQ' low = ls-saknr ) ).
      endif.

      "zglt061
      free: it_zglt061[].
      select * from zglt061 into table it_zglt061
      where bukrs eq wa_dre_dados_alv-bukrs
        and gjahr eq wa_dre_dados_alv-gjahr.

*1.2==========Classificar contas/Identificar a estrutura da DRE====================================================
      free: it_zgl015_dre_est08[].
      if it_zglt061[] is not initial.
        if wa_dre_dados_alv-bukrs ne '0001'.
          select * from zgl015_dre_est08 into table it_zgl015_dre_est08
          for all entries in it_zglt061
          where bukrs_b eq it_zglt061-bukrs
          and versn eq it_zglt061-versn.
        else.
          clear: ws_zgl015_dre_est08.
          select single * from zgl015_dre_est08 into ws_zgl015_dre_est08
          where bukrs eq wa_dre_dados_alv-bukrs
          and versn eq wa_dre_dados_alv-versn.
          if ws_zgl015_dre_est08 is not initial.
            append ws_zgl015_dre_est08 to it_zgl015_dre_est08.
          endif.
        endif.
      endif.

      "=========Contas tipo Centro de Custo=================================================================
      if r_saknr[] is not initial and it_zgl015_dre_est08[] is not initial.
        "Contas tipo Centro de Custo
        free: it_zgl015_dre_est04[].
        select * from zgl015_dre_est04 into table it_zgl015_dre_est04
          for all entries in it_zgl015_dre_est08
          where bukrs eq it_zgl015_dre_est08-bukrs
            and versn eq it_zgl015_dre_est08-versn
            and saknr in r_saknr.

        if sy-subrc eq 0.
          sort it_zgl015_dre_est04[] by saknr.
          delete adjacent duplicates from it_zgl015_dre_est04[] comparing saknr.
          r_saknr_cc = value #( for c in it_zgl015_dre_est04 ( sign = 'I' option = 'EQ' low = c-saknr ) ).
        endif.
      endif.

      "=========Contas tipo Centro de Lucro=================================================================

      if r_saknr[] is not initial and it_zgl015_dre_est08[] is not initial.
        "Contas tipo Centro de Lucro
        free: it_zgl015_dre_est05[].
        select * from zgl015_dre_est05 into table it_zgl015_dre_est05
          for all entries in it_zgl015_dre_est08
          where bukrs eq it_zgl015_dre_est08-bukrs
            and versn eq it_zgl015_dre_est08-versn
            and saknr in r_saknr.

        if sy-subrc eq 0.
          sort it_zgl015_dre_est05[] by saknr.
          delete adjacent duplicates from it_zgl015_dre_est05[] comparing saknr.
          r_saknr_cl = value #( for cl in it_zgl015_dre_est05 ( sign = 'I' option = 'EQ' low = cl-saknr ) ).
        endif.
      endif.

      "=========Contas tipo Material==========================================================================

      if r_saknr[] is not initial and it_zgl015_dre_est08[] is not initial.
        "Contas tipo Material
        free: it_zgl015_dre_est06[].
        select * from zgl015_dre_est06 into table it_zgl015_dre_est06
          for all entries in it_zgl015_dre_est08
          where bukrs eq it_zgl015_dre_est08-bukrs
            and versn eq it_zgl015_dre_est08-versn
            and saknr in r_saknr.

        if sy-subrc eq 0.
          sort it_zgl015_dre_est06[] by saknr.
          delete adjacent duplicates from it_zgl015_dre_est06[] comparing saknr.
          r_saknr_mat = value #( for mt in it_zgl015_dre_est06 ( sign = 'I' option = 'EQ' low = mt-saknr ) ).
        endif.
      endif.


      "=========Contas tipo Razão==========================================================================
      if r_saknr[] is not initial and it_zgl015_dre_est08[] is not initial.
*        IF r_saknr_cc[] IS NOT INITIAL.
        "Contas tipo Razão
        free: it_zgl015_dre_est03[].
        select * from zgl015_dre_est03 into table it_zgl015_dre_est03
          for all entries in it_zgl015_dre_est08
          where bukrs eq it_zgl015_dre_est08-bukrs
            and versn eq it_zgl015_dre_est08-versn.

        if sy-subrc eq 0.
          sort it_zgl015_dre_est03 by saknr.
          if r_saknr_cl[] is not initial.
            delete it_zgl015_dre_est03[] where saknr in r_saknr_cl.
          endif.
          if r_saknr_mat[] is not initial.
            delete it_zgl015_dre_est03[] where saknr in r_saknr_mat.
          endif.
          if r_saknr_cc[] is not initial.
            delete it_zgl015_dre_est03[] where saknr in r_saknr_cc.
          endif.

          if it_zgl015_dre_est03[] is not initial.
            sort it_zgl015_dre_est03[] by saknr.
            delete adjacent duplicates from it_zgl015_dre_est03[] comparing saknr.
            r_saknr_raz = value #( for rz in it_zgl015_dre_est03 ( sign = 'I' option = 'EQ' low = rz-saknr ) ).
          endif.
        endif.
*        ENDIF.
      endif.


      "=========Contas não vinculada na estrutura da DRE===================================================================================

      "Contas não vinculada na estrutura da DRE
      if it_zgl015_dre_est08[] is not initial.
*      IF r_saknr[] IS NOT INITIAL.
        free: it_zglt_dre_02_aux[].
        select * from zglt_dre_02  into table it_zglt_dre_02_aux
        where bukrs eq wa_dre_dados_alv-bukrs
        and gjahr eq wa_dre_dados_alv-gjahr
        and poper in rg_monat.
        sort it_zglt_dre_02_aux by saknr.
        delete adjacent duplicates from it_zglt_dre_02_aux comparing saknr.

        free: it_zgl015_dre_est03_aux[].
        select * from zgl015_dre_est03 into table it_zgl015_dre_est03_aux
          for all entries in it_zgl015_dre_est08
          where bukrs eq it_zgl015_dre_est08-bukrs
            and versn eq it_zgl015_dre_est08-versn.
        sort it_zgl015_dre_est03_aux[] by saknr.
        delete adjacent duplicates from it_zgl015_dre_est03_aux[] comparing saknr.

        "Verificar.
        free: r_saknr_avu[].
        loop at it_zglt_dre_02_aux.
          read table it_zgl015_dre_est03_aux with key saknr = it_zglt_dre_02_aux-saknr.
          if sy-subrc ne 0.
            append value #( sign = 'I' option = 'EQ' low = it_zglt_dre_02_aux-saknr ) to r_saknr_avu[].
          endif.
        endloop.
      endif.

*      ENDIF.


*1.3============Totalizar valores por c.custo================================================================================================
      if r_saknr_cc[] is not initial.
        free: it_dre_02_cc[].
        select  bukrs, gjahr, poper, saknr, kostl, sum( vlhsl ) as vlhsl, sum( vlksl ) as vlksl, sum( vlosl ) as vlosl, sum( qtmsl ) as qtmsl
        from zglt_dre_02  into corresponding fields of table @it_dre_02_cc
        where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat
*        AND kostl NE @space
        and saknr in @r_saknr_cc
        group by  bukrs, gjahr, poper, saknr, kostl.
        if sy-subrc eq 0.
          append lines of it_dre_02_cc[] to it_dre_02_tbdr[].
        endif.
      endif.


      "============Totalizar valores por c.lucro==================================================================================
      if r_saknr_cl[] is not initial.
        free: it_dre_02_cl[].
        select  bukrs, gjahr, poper, saknr, prctr, sum( vlhsl ) as vlhsl, sum( vlksl ) as vlksl, sum( vlosl ) as vlosl, sum( qtmsl ) as qtmsl
        from zglt_dre_02  into corresponding fields of table @it_dre_02_cl
        where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat
        and prctr ne @space
        and saknr in @r_saknr_cl
        and matkl eq @space
        group by  bukrs, gjahr, poper, saknr,  prctr.
        if sy-subrc eq 0.
          append lines of it_dre_02_cl[] to it_dre_02_tbdr[].
        endif.
      endif.


      "============Totalizar valores por material==================================================================================
      if r_saknr_mat[] is not initial.
*        FREE: it_dre_02_mat[].
*        SELECT  bukrs, gjahr, poper, saknr, matkl, SUM( vlhsl ) AS vlhsl, SUM( vlksl ) AS vlksl, SUM( vlosl ) AS vlosl, SUM( qtmsl ) AS qtmsl
*        FROM zglt_dre_02  INTO CORRESPONDING FIELDS OF TABLE @it_dre_02_mat
*        WHERE bukrs EQ @wa_dre_dados_alv-bukrs
*        AND gjahr EQ @wa_dre_dados_alv-gjahr
*        AND poper EQ @wa_dre_dados_alv-monat
*        AND matkl NE @space
*        AND saknr IN @r_saknr_mat
*        GROUP BY  bukrs, gjahr, poper, saknr, matkl.

        free: it_dre_02_mat[].
        select  *
        from zglt_dre_02  into corresponding fields of table @it_dre_02_mat
        where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat
        and matkl ne @space
        and saknr in @r_saknr_mat.


        select  *
        from zglt_dre_02 appending corresponding fields of table @it_dre_02_mat
        where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat
        and matkl eq @space
        and kostl ne @space
        and kosar eq 'O'
        and saknr in @r_saknr_mat.


        if it_dre_02_mat[] is not initial.
          sort it_dre_02_mat[] by bukrs gjahr poper saknr matkl.

          free: it_zgl015_dre_cvend.
          select * from zgl015_dre_cvend into table it_zgl015_dre_cvend
            for all entries in it_dre_02_mat[]
            where bukrs eq it_dre_02_mat-bukrs
              and hkont eq it_dre_02_mat-saknr
              and kostl eq it_dre_02_mat-kostl.

          "Ajuste na grupo de material, identificar e setar o grupo correto.
          loop at it_dre_02_mat assigning field-symbol(<ls_grmat>).
            if <ls_grmat>-kosar eq 'O'.
              read table it_zgl015_dre_cvend with key bukrs = <ls_grmat>-bukrs
                                                      hkont = <ls_grmat>-saknr
                                                      kostl = <ls_grmat>-kostl.

              if sy-subrc eq 0.
                <ls_grmat>-matkl = it_zgl015_dre_cvend-matkl. "Recebendo grupo material.
              endif.
            endif.
          endloop.

          free: it_dre_02_mat_aux.
          it_dre_02_mat_aux[] = it_dre_02_mat[].

          sort it_dre_02_mat[] by bukrs gjahr poper saknr matkl.
          delete adjacent duplicates from it_dre_02_mat[] comparing  bukrs gjahr poper saknr matkl.

          "Totalizar valores por grupo material
          loop at it_dre_02_mat assigning field-symbol(<ls_mat>).
            <ls_mat>-vlhsl = 0.
            <ls_mat>-vlksl = 0.
            <ls_mat>-vlosl = 0.

            <ls_mat>-kostl = space.

            loop at it_dre_02_mat_aux where bukrs eq <ls_mat>-bukrs
                                        and gjahr eq <ls_mat>-gjahr
                                        and poper eq <ls_mat>-poper
                                        and saknr eq <ls_mat>-saknr
                                        and matkl eq <ls_mat>-matkl.




              add it_dre_02_mat_aux-vlhsl to <ls_mat>-vlhsl.
              add it_dre_02_mat_aux-vlksl to <ls_mat>-vlksl.
              add it_dre_02_mat_aux-vlosl to <ls_mat>-vlosl.
            endloop.
          endloop.
        endif.

        if it_dre_02_mat[] is not initial.
          append lines of it_dre_02_mat[] to it_dre_02_tbdr[].
        endif.
      endif.



      "============Totalizar valores por tipo razão==================================================================================
      if r_saknr_raz[] is not initial.
        free: it_dre_02_raz[].
        select  bukrs, gjahr, poper, saknr, sum( vlhsl ) as vlhsl, sum( vlksl ) as vlksl, sum( vlosl ) as vlosl, sum( qtmsl ) as qtmsl
        from zglt_dre_02  into corresponding fields of table @it_dre_02_raz
        where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat
        and saknr in @r_saknr_raz
        group by  bukrs, gjahr, poper, saknr.
        if sy-subrc eq 0.
          append lines of it_dre_02_raz[] to it_dre_02_tbdr[].
        endif.
      endif.
      free: it_dre_02_raz[].

      "============Totalizar Contas não vinculada na estrutura da DRE==================================================================================
      if r_saknr_avu[] is not initial.
        free: it_dre_02_avu[].
        select  bukrs, gjahr, poper, saknr, sum( vlhsl ) as vlhsl, sum( vlksl ) as vlksl, sum( vlosl ) as vlosl, sum( qtmsl ) as qtmsl
        from zglt_dre_02  into corresponding fields of table @it_dre_02_avu
        where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat
        and saknr in @r_saknr_avu
        group by  bukrs, gjahr, poper, saknr.
        if sy-subrc eq 0.
          append lines of it_dre_02_avu[] to it_dre_02_tbdr[].
        endif.
      endif.



      "============Passando os dados para estrutura parametro principal==================================================================================
      loop at it_dre_02_tbdr.
        append value #( bukrs = it_dre_02_tbdr-bukrs
        gjahr = it_dre_02_tbdr-gjahr
        monat = it_dre_02_tbdr-poper
        saknr = it_dre_02_tbdr-saknr
        kostl = it_dre_02_tbdr-kostl
        prctr = it_dre_02_tbdr-prctr
        matkl = it_dre_02_tbdr-matkl
        ) to it_param_dre.
      endloop.

      free: it_zglt_dre_02[], it_dre_02_avu[],  it_zglt_dre_02_aux[], it_zgl015_dre_est03[],
      it_zgl015_dre_est05[], it_zgl015_dre_est06[], it_zgl015_dre_est08[], it_zgl015_dre_est04[], it_dre_02_mat[], it_dre_02_cl[].



*1.4============Valores de Custeio e Ordens==================================================================================


      "============ Contas tipo centro de custos e que possuam ordem diferente de ZSTA e ZSIN.===============================
*      IF r_saknr_cc[] IS NOT INITIAL.
*        FREE: it_dre_02_cust_cc_1[].
*        SELECT bukrs, gjahr, poper, saknr, kostl, SUM( vlhsl ) AS vlhsl, SUM( vlksl ) AS vlksl, SUM( vlosl ) AS vlosl
*        FROM zglt_dre_02 AS a  INTO CORRESPONDING FIELDS OF TABLE @it_dre_02_cust_cc_1
*        WHERE bukrs EQ @wa_dre_dados_alv-bukrs
*        AND gjahr EQ @wa_dre_dados_alv-gjahr
*        AND poper EQ @wa_dre_dados_alv-monat
*        AND saknr IN @r_saknr_cc
*        AND aufnr NE @space
*        AND EXISTS ( SELECT * FROM coas WHERE aufnr = a~aufnr AND auart NOT IN ( 'ZSTA', 'ZSIN' ) )
*        GROUP BY  bukrs, gjahr, poper, saknr, kostl.
*
*        IF sy-subrc EQ 0.
*
*          IF it_dre_02_cust_cc_1[] IS NOT INITIAL.
*            APPEND LINES OF it_dre_02_cust_cc_1[] TO it_dre_02_cust[].
*          ENDIF.
*        ENDIF.
*      ENDIF.

      free: it_dre_02_cust_cc_1.
      if r_saknr_cc[] is not initial.
        select *
         from zglt_dre_02 as a  into corresponding fields of table @it_dre_02_cust_cc_1
         where bukrs eq @wa_dre_dados_alv-bukrs
         and gjahr eq @wa_dre_dados_alv-gjahr
         and poper in @rg_monat
         and saknr in @r_saknr_cc
         and aufnr ne @space.
*         AND EXISTS ( SELECT * FROM coas WHERE aufnr = a~aufnr AND auart NOT IN ( 'ZSTA', 'ZSIN' ) ).
        if it_dre_02_cust_cc_1[] is not initial.

          free: it_coas[].
          select * from coas into table it_coas
            for all entries in it_dre_02_cust_cc_1[]
              where aufnr eq it_dre_02_cust_cc_1-aufnr.


          loop at it_dre_02_cust_cc_1.
            read table it_coas with key aufnr = it_dre_02_cust_cc_1-aufnr.
            if sy-subrc eq 0.

              if it_coas-auart eq 'ZSTA' or it_coas-auart eq 'ZSIN'. " OR it_coas-auart EQ 'ZOAN'.
                if it_dre_02_cust_cc_1-kosar eq 'F'.
*                  APPEND it_dre_02_cust_cc_1 TO it_total_cust_cc[].

                  append value #(
                                bukrs = it_dre_02_cust_cc_1-bukrs
                                gjahr = it_dre_02_cust_cc_1-gjahr
                                poper = it_dre_02_cust_cc_1-poper
                                saknr = it_dre_02_cust_cc_1-saknr
                                kostl = it_dre_02_cust_cc_1-kostl
                                vlhsl = it_dre_02_cust_cc_1-vlhsl
                                vlksl = it_dre_02_cust_cc_1-vlksl
                                vlosl = it_dre_02_cust_cc_1-vlosl


                  ) to it_total_cust_cc.
                endif.
              else.
*                APPEND it_dre_02_cust_cc_1 TO it_total_cust_cc[].
                append value #(
                bukrs = it_dre_02_cust_cc_1-bukrs
                gjahr = it_dre_02_cust_cc_1-gjahr
                poper = it_dre_02_cust_cc_1-poper
                saknr = it_dre_02_cust_cc_1-saknr
                kostl = it_dre_02_cust_cc_1-kostl
                vlhsl = it_dre_02_cust_cc_1-vlhsl
                vlksl = it_dre_02_cust_cc_1-vlksl
                vlosl = it_dre_02_cust_cc_1-vlosl
                ) to it_total_cust_cc.
              endif.
            endif.

            clear: it_coas, it_dre_02_cust_cc_1.
          endloop.
        endif.
      endif.


      "============ Contas tipo centro de custos que não possuem centro de custo e nem ordem (liquidação)===============================
*     IF r_saknr_cc[] IS NOT INITIAL.
*      FREE: it_dre_02_cust_cc_2[].
*      SELECT bukrs, gjahr, poper, saknr, kostl, SUM( vlhsl ) AS vlhsl, SUM( vlksl ) AS vlksl, SUM( vlosl ) AS vlosl
*      FROM zglt_dre_02  INTO CORRESPONDING FIELDS OF TABLE @it_dre_02_cust_cc_2
*      WHERE bukrs EQ @wa_dre_dados_alv-bukrs
*      AND gjahr EQ @wa_dre_dados_alv-gjahr
*      AND poper EQ @wa_dre_dados_alv-monat
*      AND saknr IN @r_saknr_cc
*      AND kostl EQ @space
*      AND aufnr EQ @space
*      GROUP BY  bukrs, gjahr, poper, saknr, kostl.
*      IF it_dre_02_cust_cc_2[] IS NOT INITIAL.
*        APPEND LINES OF it_dre_02_cust_cc_2[] TO it_dre_02_cust[].
*      ENDIF.
*    ENDIF.
      free: it_dre_02_cust_cc_2[].
      if r_saknr_cc[] is not initial.
        select *
        from zglt_dre_02  into corresponding fields of table @it_dre_02_cust_cc_2
        where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat
        and saknr in @r_saknr_cc
        and kostl eq @space
        and aufnr eq @space.

        if it_dre_02_cust_cc_2[] is not initial.
*          APPEND LINES OF it_dre_02_cust_cc_2[] TO it_total_cust_cc[].
          loop at it_dre_02_cust_cc_2.
            append value #(
            bukrs = it_dre_02_cust_cc_2-bukrs
            gjahr = it_dre_02_cust_cc_2-gjahr
            poper = it_dre_02_cust_cc_2-poper
            saknr = it_dre_02_cust_cc_2-saknr
            kostl = it_dre_02_cust_cc_2-kostl
            vlhsl = it_dre_02_cust_cc_2-vlhsl
            vlksl = it_dre_02_cust_cc_2-vlksl
            vlosl = it_dre_02_cust_cc_2-vlosl
            ) to it_total_cust_cc.
            clear: it_dre_02_cust_cc_2.
          endloop.
        endif.
      endif.
      free: it_dre_02_cust_cc_2[].



      "============ Contas tipo centro de custos, especificamente ‘F’ que não possuem ordem===============================
*      IF r_saknr_cc[] IS NOT INITIAL.
*        FREE: it_dre_02_cust_cc_3[].
*        SELECT bukrs, gjahr, poper, saknr, kostl, SUM( vlhsl ) AS vlhsl, SUM( vlksl ) AS vlksl, SUM( vlosl ) AS vlosl
*        FROM zglt_dre_02  INTO CORRESPONDING FIELDS OF TABLE @it_dre_02_cust_cc_3
*        WHERE bukrs EQ @wa_dre_dados_alv-bukrs
*        AND gjahr EQ @wa_dre_dados_alv-gjahr
*        AND poper EQ @wa_dre_dados_alv-monat
*        AND saknr IN @r_saknr_cc
*        AND aufnr EQ @space
*        AND kosar EQ 'F'
*        GROUP BY  bukrs, gjahr, poper, saknr, kostl.
*
*        IF it_dre_02_cust_cc_3[] IS NOT INITIAL.
*          APPEND LINES OF it_dre_02_cust_cc_3[] TO it_dre_02_cust[].
*        ENDIF.
*      ENDIF.

      if r_saknr_cc[] is not initial.
        free: it_dre_02_cust_cc_3[].
        select *
        from zglt_dre_02  into corresponding fields of table @it_dre_02_cust_cc_3
        where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat
        and saknr in @r_saknr_cc
        and aufnr eq @space
        and kosar eq 'F' .
      endif.

      if it_dre_02_cust_cc_3[] is not initial.
*        APPEND LINES OF it_dre_02_cust_cc_3[] TO it_total_cust_cc[].
        loop at it_dre_02_cust_cc_3.
          append value #(
          bukrs = it_dre_02_cust_cc_3-bukrs
          gjahr = it_dre_02_cust_cc_3-gjahr
          poper = it_dre_02_cust_cc_3-poper
          saknr = it_dre_02_cust_cc_3-saknr
          kostl = it_dre_02_cust_cc_3-kostl
          vlhsl = it_dre_02_cust_cc_3-vlhsl
          vlksl = it_dre_02_cust_cc_3-vlksl
          vlosl = it_dre_02_cust_cc_3-vlosl
          ) to it_total_cust_cc.
          clear: it_dre_02_cust_cc_3.
        endloop.
      endif.
      free: it_dre_02_cust_cc_3[].

      if it_total_cust_cc[] is not initial.
        sort it_total_cust_cc[] by bukrs  gjahr poper saknr kostl.
        free: it_total_cust_cc_aux[].
        it_total_cust_cc_aux[] = it_total_cust_cc[].
        delete adjacent duplicates from it_total_cust_cc comparing bukrs  gjahr  poper saknr kostl.
      endif.

      "Totalizar valores conta c.custo.
      loop at it_total_cust_cc assigning field-symbol(<ws_total_cc>).

        <ws_total_cc>-vlhsl = 0.
        <ws_total_cc>-vlksl = 0.
        <ws_total_cc>-vlosl = 0.

        loop at it_total_cust_cc_aux where bukrs eq <ws_total_cc>-bukrs
                                      and gjahr eq <ws_total_cc>-gjahr
                                     and  poper eq <ws_total_cc>-poper
                                      and saknr eq <ws_total_cc>-saknr
                                      and kostl eq <ws_total_cc>-kostl.


          add it_total_cust_cc_aux-vlhsl to <ws_total_cc>-vlhsl.
          add it_total_cust_cc_aux-vlksl to <ws_total_cc>-vlksl.
          add it_total_cust_cc_aux-vlosl to <ws_total_cc>-vlosl.

        endloop.
*
*        LOOP AT it_dre_02_cust_cc_1 WHERE bukrs EQ <ws_total_cc>-bukrs
*                                      AND gjahr EQ <ws_total_cc>-gjahr
*                                     AND  poper EQ <ws_total_cc>-poper
*                                      AND saknr EQ <ws_total_cc>-saknr
*                                      AND kostl EQ <ws_total_cc>-kostl.
*
*          ADD it_dre_02_cust_cc_1-vlhsl TO <ws_total_cc>-vlhsl.
*          ADD it_dre_02_cust_cc_1-vlksl TO <ws_total_cc>-vlksl.
*          ADD it_dre_02_cust_cc_1-vlosl TO <ws_total_cc>-vlosl.
*        ENDLOOP.
*
*        LOOP AT it_dre_02_cust_cc_2 WHERE bukrs EQ <ws_total_cc>-bukrs
*                                      AND gjahr EQ <ws_total_cc>-gjahr
*                                     AND  poper EQ <ws_total_cc>-poper
*                                      AND saknr EQ <ws_total_cc>-saknr
*                                      AND kostl EQ <ws_total_cc>-kostl.
*
*          ADD it_dre_02_cust_cc_2-vlhsl TO <ws_total_cc>-vlhsl.
*          ADD it_dre_02_cust_cc_2-vlksl TO <ws_total_cc>-vlksl.
*          ADD it_dre_02_cust_cc_2-vlosl TO <ws_total_cc>-vlosl.
*        ENDLOOP.
*
*        LOOP AT it_dre_02_cust_cc_3 WHERE bukrs EQ <ws_total_cc>-bukrs
*                                      AND gjahr EQ <ws_total_cc>-gjahr
*                                     AND  poper EQ <ws_total_cc>-poper
*                                      AND saknr EQ <ws_total_cc>-saknr
*                                      AND kostl EQ <ws_total_cc>-kostl.
*
*          ADD it_dre_02_cust_cc_3-vlhsl TO <ws_total_cc>-vlhsl.
*          ADD it_dre_02_cust_cc_3-vlksl TO <ws_total_cc>-vlksl.
*          ADD it_dre_02_cust_cc_3-vlosl TO <ws_total_cc>-vlosl.
*        ENDLOOP.
      endloop.

      if it_total_cust_cc[] is not initial.
        append lines of it_total_cust_cc[] to it_dre_02_cust[].
      endif.
      free: it_total_cust_cc[].


      "============ Contas não inclusas na estrutura da DRE===============================
      if r_saknr_avu[] is not initial.
        free: it_dre_02_cust_avu[].
        select bukrs, gjahr, poper, saknr, sum( vlhsl ) as vlhsl, sum( vlksl ) as vlksl, sum( vlosl ) as vlosl
        from zglt_dre_02  into corresponding fields of table @it_dre_02_cust_avu
        where bukrs eq @wa_dre_dados_alv-bukrs
        and gjahr eq @wa_dre_dados_alv-gjahr
        and poper in @rg_monat
        and saknr in @r_saknr_avu
        group by  bukrs, gjahr, poper, saknr.
        if it_dre_02_cust_avu[] is not initial.
          append lines of it_dre_02_cust_avu[] to it_dre_02_cust[].
        endif.
      endif.
      free: it_dre_02_cust_avu[].


      loop at it_dre_02_cust.
        append value #(
        bukrs = it_dre_02_cust-bukrs
        gjahr = it_dre_02_cust-gjahr
        monat = it_dre_02_cust-poper
        saknr = it_dre_02_cust-saknr
        kostl = it_dre_02_cust-kostl
        prctr = it_dre_02_cust-prctr
        matkl = it_dre_02_cust-matkl
        ) to it_param_dre.
      endloop.


*1.5============Valores de DRE Processada==================================================================================
      "============ Conta razão - Valores DRE Processada===============================
*      IF it_zglt061[] IS NOT INITIAL.
      free: it_zgl021_dre_dados[].
      select * from zgl021_dre_dados into table it_zgl021_dre_dados
*          FOR ALL ENTRIES IN it_zglt061
        where versn eq wa_dre_dados_alv-versn
          and bukrs eq wa_dre_dados_alv-bukrs
          and gjahr eq wa_dre_dados_alv-gjahr
          and monat in rg_monat.
*      ENDIF.

      loop at it_zgl021_dre_dados.
        append value #(
        bukrs = it_zgl021_dre_dados-bukrs
        gjahr = it_zgl021_dre_dados-gjahr
        poper = it_zgl021_dre_dados-monat
        saknr = it_zgl021_dre_dados-saknr
        kostl = ''
        prctr = ''
        matkl = ''
        vlhsl = it_zgl021_dre_dados-vlr_rea
        vlksl = it_zgl021_dre_dados-vlr_dolar
        vlosl = it_zgl021_dre_dados-vlr_grupo
        nivel = it_zgl021_dre_dados-nivel
        qtmsl = ''
        ) to it_dre.
      endloop.
      free: it_zgl021_dre_dados[].

      "============  Conta centro de custo - Valores DRE Processada===============================
*      IF it_zglt061[] IS NOT INITIAL.
      free: it_zgl022_dre_dados[].
      select * from zgl022_dre_dados into table it_zgl022_dre_dados
*          FOR ALL ENTRIES IN it_zglt061
        where versn eq wa_dre_dados_alv-versn
          and bukrs eq wa_dre_dados_alv-bukrs
          and gjahr eq wa_dre_dados_alv-gjahr
          and monat in rg_monat.
*      ENDIF.

      loop at it_zgl022_dre_dados.
        append value #(
        bukrs = it_zgl022_dre_dados-bukrs
        gjahr = it_zgl022_dre_dados-gjahr
        poper = it_zgl022_dre_dados-monat
        saknr = it_zgl022_dre_dados-saknr
        kostl = it_zgl022_dre_dados-kostl
        prctr = ''
        matkl = ''
        vlhsl = it_zgl022_dre_dados-vlr_rea
        vlksl = it_zgl022_dre_dados-vlr_dolar
        vlosl = it_zgl022_dre_dados-vlr_grupo
        nivel = it_zgl022_dre_dados-nivel
        qtmsl = ''
        ) to it_dre.
      endloop.
      free: it_zgl022_dre_dados[].

      "============  Conta centro de lucro - Valores DRE Processada===============================
*      IF it_zglt061[] IS NOT INITIAL.
      free: it_zgl023_dre_dados[].
      select * from zgl023_dre_dados into table it_zgl023_dre_dados
*          FOR ALL ENTRIES IN it_zglt061
        where versn eq wa_dre_dados_alv-versn
          and bukrs eq wa_dre_dados_alv-bukrs
          and gjahr eq wa_dre_dados_alv-gjahr
          and monat in rg_monat.


      loop at it_zgl023_dre_dados.
        append value #(
        bukrs = it_zgl023_dre_dados-bukrs
        gjahr = it_zgl023_dre_dados-gjahr
        poper = it_zgl023_dre_dados-monat
        saknr = it_zgl023_dre_dados-saknr
        kostl = ''
        prctr = it_zgl023_dre_dados-prctr
        matkl = ''
        vlhsl = it_zgl023_dre_dados-vlr_rea
        vlksl = it_zgl023_dre_dados-vlr_dolar
        vlosl = it_zgl023_dre_dados-vlr_grupo
        nivel = it_zgl023_dre_dados-nivel
        qtmsl = ''
        ) to it_dre.
      endloop.
      free: it_zgl023_dre_dados[].
*      ENDIF.

      "============  Conta de material - Valores DRE Processada===============================
*      FREE: it_zgl024_dre_dados[].
*      IF it_zglt061[] IS NOT INITIAL.
      select * from zgl024_dre_dados into table it_zgl024_dre_dados
*          FOR ALL ENTRIES IN it_zglt061
        where versn eq wa_dre_dados_alv-versn
          and bukrs eq wa_dre_dados_alv-bukrs
          and gjahr eq wa_dre_dados_alv-gjahr
          and monat in rg_monat.


      loop at it_zgl024_dre_dados.
        append value #(
        bukrs = it_zgl024_dre_dados-bukrs
        gjahr = it_zgl024_dre_dados-gjahr
        poper = it_zgl024_dre_dados-monat
        saknr = it_zgl024_dre_dados-saknr
        kostl = ''
        prctr = ''
        matkl = it_zgl024_dre_dados-matkl
        vlhsl = it_zgl024_dre_dados-vlr_rea
        vlksl = it_zgl024_dre_dados-vlr_dolar
        vlosl = it_zgl024_dre_dados-vlr_grupo
        nivel = it_zgl024_dre_dados-nivel
        qtmsl = it_zgl024_dre_dados-qtd_ton
        ) to it_dre.
      endloop.

      free: it_zgl024_dre_dados[], it_zglt_dre_02[], r_saknr[], r_saknr_cc[], r_saknr_cl[],  r_saknr_raz[], r_saknr_mat[], r_saknr_avu[], r_saknr_dv[],
       r_aufnr[], r_kostl[], r_prctr[].

*      ENDIF.

      loop at it_dre.
        append value #(
        bukrs = it_dre-bukrs
        monat = it_dre-poper
        gjahr = it_dre-gjahr
        saknr = it_dre-saknr
        matkl = it_dre-matkl
        prctr = it_dre-prctr
        kostl = it_dre-kostl
        ) to it_param_dre.
      endloop.

*===============================Totalizar valores dos periodos maior que 12============================================================.


      sort it_dre_02_tbdr by poper.
      it_dre_02_tbdr_aux[] = it_dre_02_tbdr[].

      delete it_dre_02_tbdr[] where poper > 12.
      delete it_dre_02_tbdr_aux[] where poper < 13.
      sort it_dre_02_tbdr[] by bukrs gjahr poper saknr kostl prctr matkl.
      sort it_dre_02_tbdr_aux[] by bukrs gjahr poper saknr kostl prctr matkl.

      loop at it_dre_02_tbdr assigning field-symbol(<ws_dre_02_tbdr>) where poper > 11.
        loop at it_dre_02_tbdr_aux assigning field-symbol(<ws_dre_02_tbdr_aux>)  where bukrs = <ws_dre_02_tbdr>-bukrs
                                                                           and gjahr = <ws_dre_02_tbdr>-gjahr
                                                                           and saknr = <ws_dre_02_tbdr>-saknr
*                                                                           and poper = <ws_dre_02_tbdr>-poper
                                                                           and prctr = <ws_dre_02_tbdr>-prctr
                                                                           and kostl = <ws_dre_02_tbdr>-kostl
                                                                           and matkl = <ws_dre_02_tbdr>-matkl.

          <ws_dre_02_tbdr>-vltsl = <ws_dre_02_tbdr>-vltsl + <ws_dre_02_tbdr_aux>-vltsl.
          <ws_dre_02_tbdr>-vlhsl = <ws_dre_02_tbdr>-vlhsl + <ws_dre_02_tbdr_aux>-vlhsl.
          <ws_dre_02_tbdr>-vlksl = <ws_dre_02_tbdr>-vlksl + <ws_dre_02_tbdr_aux>-vlksl.
          <ws_dre_02_tbdr>-vlosl = <ws_dre_02_tbdr>-vlosl + <ws_dre_02_tbdr_aux>-vlosl.
          <ws_dre_02_tbdr>-qtmsl = <ws_dre_02_tbdr>-qtmsl + <ws_dre_02_tbdr_aux>-qtmsl.
        endloop.
      endloop.


      sort it_dre_02_cust by poper.
      it_dre_02_cust_aux[] = it_dre_02_cust[].

      delete it_dre_02_cust[] where poper > 12.
      delete it_dre_02_cust_aux[] where poper < 13.
      sort it_dre_02_cust by bukrs gjahr poper saknr kostl prctr matkl.
      sort it_dre_02_cust_aux[] by bukrs gjahr poper saknr kostl prctr matkl.

      loop at it_dre_02_cust assigning field-symbol(<ws_dre_02_cust>) where poper > 11.
        loop at it_dre_02_cust_aux assigning field-symbol(<ws_dre_02_cust_aux>)  where bukrs = <ws_dre_02_tbdr>-bukrs
                                                                           and gjahr = <ws_dre_02_cust>-gjahr
                                                                           and saknr = <ws_dre_02_cust>-saknr
*                                                                           and poper = <ws_dre_02_cust>-poper
                                                                           and prctr = <ws_dre_02_cust>-prctr
                                                                           and kostl = <ws_dre_02_cust>-kostl
                                                                           and matkl = <ws_dre_02_cust>-matkl.

          <ws_dre_02_cust>-vltsl = <ws_dre_02_cust>-vltsl + <ws_dre_02_cust_aux>-vltsl.
          <ws_dre_02_cust>-vlhsl = <ws_dre_02_cust>-vlhsl + <ws_dre_02_cust_aux>-vlhsl.
          <ws_dre_02_cust>-vlksl = <ws_dre_02_cust>-vlksl + <ws_dre_02_cust_aux>-vlksl.
          <ws_dre_02_cust>-vlosl = <ws_dre_02_cust>-vlosl + <ws_dre_02_cust_aux>-vlosl.
          <ws_dre_02_cust>-qtmsl = <ws_dre_02_cust>-qtmsl + <ws_dre_02_cust_aux>-qtmsl.
        endloop.
      endloop.



*===============================Organiza dados============================================================.
      if it_param_dre[] is not initial.

        sort: it_param_dre[] by bukrs gjahr monat saknr kostl prctr matkl,
              it_dre_02_tbdr by bukrs gjahr poper saknr kostl prctr matkl,
              it_dre_02_cust by bukrs gjahr poper saknr kostl prctr matkl,
              it_dre         by bukrs gjahr poper saknr kostl prctr matkl.

        delete adjacent duplicates from it_param_dre[] comparing bukrs gjahr monat saknr kostl prctr matkl.

        sort it_param_dre[] by monat.
        delete it_param_dre[] where monat > 12.
        sort it_param_dre[] by bukrs gjahr monat saknr kostl prctr matkl.

        loop at it_param_dre into data(ls_param).
          wa_tabderivada_dre-bukrs = ls_param-bukrs.
          wa_tabderivada_dre-gjahr = ls_param-gjahr.
          wa_tabderivada_dre-monat = ls_param-monat.
          wa_tabderivada_dre-saknr = ls_param-saknr.
          wa_tabderivada_dre-kostl = ls_param-kostl.
          wa_tabderivada_dre-prctr = ls_param-prctr.
          wa_tabderivada_dre-matkl = ls_param-matkl.


          read table it_dre_02_tbdr with key bukrs = ls_param-bukrs
                                             gjahr = ls_param-gjahr
                                             saknr = ls_param-saknr
                                             poper = ls_param-monat
                                             prctr = ls_param-prctr
                                             kostl = ls_param-kostl
                                             matkl = ls_param-matkl binary search.

          if sy-subrc eq 0.
            if ls_param-bukrs eq '0101'. "Convertendo o valor moeda interna empresa Paraguay
              wa_tabderivada_dre-moedaint_tbdr = ( it_dre_02_tbdr-vlhsl * 100 ).
            else.
              wa_tabderivada_dre-moedaint_tbdr = it_dre_02_tbdr-vlhsl.
            endif.

            wa_tabderivada_dre-moedafor_tbdr = it_dre_02_tbdr-vlksl.
            wa_tabderivada_dre-moedaind_tbdr = it_dre_02_tbdr-vlosl.
            wa_tabderivada_dre-qtde_tbdr     = it_dre_02_tbdr-qtmsl.
          endif.

          read table it_dre_02_cust with key bukrs = ls_param-bukrs
                                             gjahr = ls_param-gjahr
                                             saknr = ls_param-saknr
                                             poper = ls_param-monat
                                             prctr = ls_param-prctr
                                             kostl = ls_param-kostl
                                             matkl = ls_param-matkl binary search.



          if sy-subrc eq 0.
            wa_tabderivada_dre-moedaint_cust  = it_dre_02_cust-vlhsl.
            wa_tabderivada_dre-moedafor_cust  = it_dre_02_cust-vlksl.
            wa_tabderivada_dre-moedaind_cust  = it_dre_02_cust-vlosl.
          endif.

          read table it_dre with key bukrs = ls_param-bukrs
                                             gjahr = ls_param-gjahr
                                             saknr = ls_param-saknr
                                             poper = ls_param-monat
                                             kostl = ls_param-kostl
                                             prctr = ls_param-prctr
                                             matkl = ls_param-matkl binary search.



          if sy-subrc eq 0.
            wa_tabderivada_dre-moedaint_dre  = it_dre-vlhsl.
            wa_tabderivada_dre-moedafor_dre  = it_dre-vlksl.
            wa_tabderivada_dre-moedaind_dre  = it_dre-vlosl.
            wa_tabderivada_dre-nivel_dre     = it_dre-nivel.
            wa_tabderivada_dre-qtde_dre      = it_dre-qtmsl.
          endif.

          if wa_tabderivada_dre-moedaint_tbdr is not initial and wa_tabderivada_dre-moedaint_cust is not initial.
            wa_tabderivada_dre-moedaint_res = ( wa_tabderivada_dre-moedaint_tbdr - wa_tabderivada_dre-moedaint_cust ).
          elseif wa_tabderivada_dre-moedaint_tbdr is initial and wa_tabderivada_dre-moedaint_cust is not initial.
            wa_tabderivada_dre-moedaint_res = wa_tabderivada_dre-moedaint_cust.
          elseif wa_tabderivada_dre-moedaint_tbdr is not initial and wa_tabderivada_dre-moedaint_cust is initial.
            wa_tabderivada_dre-moedaint_res = wa_tabderivada_dre-moedaint_tbdr.
          endif.

          if wa_tabderivada_dre-moedafor_tbdr is not initial and wa_tabderivada_dre-moedafor_cust is not initial.
            wa_tabderivada_dre-moedafor_res = ( wa_tabderivada_dre-moedafor_tbdr - wa_tabderivada_dre-moedafor_cust ).
          elseif wa_tabderivada_dre-moedafor_tbdr is initial and wa_tabderivada_dre-moedafor_cust is not initial.
            wa_tabderivada_dre-moedafor_res = wa_tabderivada_dre-moedafor_cust.
          elseif wa_tabderivada_dre-moedafor_tbdr is not initial and wa_tabderivada_dre-moedafor_cust is initial.
            wa_tabderivada_dre-moedafor_res = wa_tabderivada_dre-moedafor_tbdr.
          endif.

          if wa_tabderivada_dre-moedaind_tbdr is not initial and wa_tabderivada_dre-moedaind_cust is not initial.
            wa_tabderivada_dre-moedaind_res = ( wa_tabderivada_dre-moedaind_tbdr - wa_tabderivada_dre-moedaind_cust ).
          elseif wa_tabderivada_dre-moedaind_tbdr is initial and wa_tabderivada_dre-moedaind_cust is not initial.
            wa_tabderivada_dre-moedaind_res = wa_tabderivada_dre-moedaind_cust.
          elseif wa_tabderivada_dre-moedaind_tbdr is not initial and wa_tabderivada_dre-moedaind_cust is initial.
            wa_tabderivada_dre-moedaind_res = wa_tabderivada_dre-moedaind_tbdr.
          endif.



*          Aplicar calculo de diferença.
          if wa_tabderivada_dre-moedaint_res < 0 and wa_tabderivada_dre-moedaint_dre > 0.
            wa_tabderivada_dre-moedaint_res = abs( wa_tabderivada_dre-moedaint_res ).

            wa_tabderivada_dre-dif_moedaint = wa_tabderivada_dre-moedaint_res - wa_tabderivada_dre-moedaint_dre.
          else.
            wa_tabderivada_dre-dif_moedaint = wa_tabderivada_dre-moedaint_res - wa_tabderivada_dre-moedaint_dre.
          endif.

*          Aplicar calculo de diferença.
*          IF wa_tabderivada_dre-moedafor_res IS NOT INITIAL AND wa_tabderivada_dre-moedafor_dre IS NOT INITIAL.
          wa_tabderivada_dre-dif_moedafor = wa_tabderivada_dre-moedafor_res - wa_tabderivada_dre-moedafor_dre.
*          ENDIF.

*           Aplicar calculo de diferença.
*          IF wa_tabderivada_dre-moedaind_res IS NOT INITIAL AND wa_tabderivada_dre-moedaind_dre IS NOT INITIAL.
          wa_tabderivada_dre-dif_moedaind = wa_tabderivada_dre-moedaind_res - wa_tabderivada_dre-moedaind_dre.
*          ENDIF.


          clear wa_color.
          append value #( fname = 'MOEDAINT_TBDR' color-col = '5' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAINT_DRE'  color-col = '5' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAINT_CUST' color-col = '5' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAINT_RES'  color-col = '5' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'DIF_MOEDAINT'  color-col = '5' color-int = '0' color-inv = '0') to it_color.

          append value #( fname = 'MOEDAFOR_TBDR' color-col = '2' color-int = '1' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAFOR_DRE'  color-col = '2' color-int = '1' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAFOR_CUST' color-col = '2' color-int = '1' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAFOR_RES'  color-col = '2' color-int = '1' color-inv = '0') to it_color.
          append value #( fname = 'DIF_MOEDAFOR'  color-col = '2' color-int = '1' color-inv = '0') to it_color.

          append value #( fname = 'MOEDAIND_TBDR' color-col = '4' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAIND_DRE'  color-col = '4' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAIND_CUST' color-col = '4' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'MOEDAIND_RES'  color-col = '4' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'DIF_MOEDAIND'  color-col = '4' color-int = '0' color-inv = '0') to it_color.

          append value #( fname = 'QTDE_DRE   '  color-col = '2' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'QTDE_TBDR  '  color-col = '2' color-int = '0' color-inv = '0') to it_color.
          append value #( fname = 'NIVE_DRE   '  color-col = '2' color-int = '0' color-inv = '0') to it_color.


          wa_tabderivada_dre-cell_color[] = it_color[].

          append wa_tabderivada_dre to it_tabderivada_dre.
          clear: wa_tabderivada_dre, it_dre_02_cust, it_dre_02_tbdr, it_dre, ls_param.
          refresh it_color.
        endloop.


        "identificar a moeda Forte e a Moeda Índice.
        clear: ws_x001.
        call function 'FI_CURRENCY_INFORMATION'
          exporting
            i_bukrs = wa_dre_dados_alv-bukrs
          importing
            e_x001  = ws_x001.


        ws_moedafor = ws_x001-hwae2.
        ws_moedaind = ws_x001-hwae3.

        "Seleção do tipo de moeda interna.
        clear: ws_moedaint.
        select single waers from t001 into ws_moedaint where bukrs eq wa_dre_dados_alv-bukrs.


        call screen 9015.

      endif.
    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9015  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9015 output.
  set pf-status 'ST9015'.
  set titlebar 'xxx'.

  perform fm_criar_objetos_9015.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9015  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9015 input.
  case sy-ucomm.
    when 'EXIT'.
*      CALL METHOD gob_gui_alv_9013->free.
      leave to screen 0.
    when others.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS_9015
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_criar_objetos_9015 .

  data: lva_data(22) type c,
        w_layout     type lvc_s_layo.

  data: objeto type ref to zcl_screen.
  create object objeto. "TYPE zcl_screen.

  free: git_fcat_9015[].
  clear: w_layout.

  if gob_gui_alv_9015 is initial.
    free: zcl_screen=>zif_screen~split.
  endif.

  data: gs_variant  type disvariant.
  gs_variant-report      = sy-repid.

  perform fm_cria_fieldcat_9015.

  clear: lva_data.
  concatenate sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) into lva_data.

  if objeto->zif_screen~set_criar_tela_padrao_report(
    exporting
       i_titulo  = 'Conferencia valores Tabela Derivada X DRE'
       i_filtros = value zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
     changing
       alv = gob_gui_alv_9015
     )
     eq abap_true.
  endif.

  w_layout-info_fname = 'ROWCOLOR'.  "Row color
  w_layout-ctab_fname = 'CELL_COLOR'.
  w_layout-cwidth_opt = abap_true.
  w_layout-zebra      = 'X'.
  w_layout-sel_mode   = 'A'.
  w_layout-col_opt    = abap_true.


  call method gob_gui_alv_9015->set_table_for_first_display
    exporting
      is_layout                     = w_layout
      i_save                        = 'A'
      is_variant                    = gs_variant
    changing
      it_outtab                     = it_tabderivada_dre
      it_fieldcatalog               = git_fcat_9015
*     IT_SORT                       =
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.


  call method gob_gui_alv_9015->refresh_table_display
    exporting
      is_stable = ls_stable_9015
    exceptions
      finished  = 1
      others    = 2.

endform.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT_9015
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_cria_fieldcat_9015 .

  types: lit_fieldcat_aux type table of lvc_s_fcat with default key.
  git_fcat_9015 = value lit_fieldcat_aux(
( fieldname ='BUKRS          '               coltext = 'Empresa                '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='BUKRS '  )
( fieldname ='GJAHR          '               coltext = 'Ano                    '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='GJAHR '  )
( fieldname ='MONAT          '               coltext = 'Mês                    '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='MONAT '  )
( fieldname ='SAKNR          '               coltext = 'Conta                  '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='SAKNR '  )
( fieldname ='KOSTL          '               coltext = 'C.custo                '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='KOSTL '  )
( fieldname ='PRCTR          '               coltext = 'C.lucro                '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='PRCTR '  )
( fieldname ='MATKL          '               coltext = 'Grp.mercadorias        '    col_opt = 'X' no_zero = '' do_sum = ''  ref_table = 'ZGLT_DRE_04' ref_field ='MATKL '  )
( fieldname ='MOEDAINT_TBDR  '               coltext = |{ ws_moedaint }-T.DER|      col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAINT_CUST  '               coltext = |{ ws_moedaint }-CUSTO|      col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAINT_RES   '               coltext = |{ ws_moedaint }-RESULT|     col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAINT_DRE   '               coltext = |{ ws_moedaint }-DRE|        col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='DIF_MOEDAINT   '               coltext = |Dif-{ ws_moedaint }|        col_opt = 'X' no_zero = '' do_sum = 'X')

( fieldname ='MOEDAFOR_TBDR  '               coltext = |{ ws_moedafor }-T.DER|      col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAFOR_CUST  '               coltext = |{ ws_moedafor }-CUSTO|      col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAFOR_RES   '               coltext = |{ ws_moedafor }-RESULT|     col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAFOR_DRE   '               coltext = |{ ws_moedafor }-DRE|        col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='DIF_MOEDAFOR   '               coltext = |Dif-{ ws_moedafor }|        col_opt = 'X' no_zero = '' do_sum = 'X')

( fieldname ='MOEDAIND_TBDR  '               coltext = |{ ws_moedaind }-T.DER|      col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAIND_CUST  '               coltext = |{ ws_moedaind }-CUSTO|      col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAIND_RES   '               coltext = |{ ws_moedaind }-RESULT|     col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='MOEDAIND_DRE   '               coltext = |{ ws_moedaind }-DRE|        col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='DIF_MOEDAIND   '               coltext = |Dif-{ ws_moedaind }|        col_opt = 'X' no_zero = '' do_sum = 'X')

( fieldname ='QTDE_TBDR      '               coltext = |Qtde T.DER|                 col_opt = 'X' no_zero = '' do_sum = 'X')
*( fieldname ='QTDE_RES       '               coltext = |Qtde RES|                   col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='QTDE_DRE       '               coltext = |Qtde DRE|                   col_opt = 'X' no_zero = '' do_sum = 'X')
( fieldname ='NIVEL_DRE      '               coltext = |Nivel DRE|                  col_opt = 'X' no_zero = '' do_sum = ' ')
).

endform.
