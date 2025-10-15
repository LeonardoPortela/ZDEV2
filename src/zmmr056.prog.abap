*&---------------------------------------------------------------------*
*& Report  ZMMR056
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zmmr056.


tables: pa0001, resb, rkpf, mara.


types: begin of ty_saida,
         werks      type resb-werks,
         kostl      type csks-kostl,
         pernr      type pa0001-pernr,
         ename      type pa0001-ename,
         matnr      type resb-matnr,
         maktx      type makt-maktx,
         quantidade type bdmng,
         vlr_total  type mseg-dmbtr,
         bwart      type rkpf-bwart,
         rsdat      type zmmt0087-data,
         dtvenc     type zmmt0087-data,
         mblnr      type zmmt0087-mblnr,
         lgort      type zmmt0087-lgort,
         color      type   kkblo_specialcol occurs 0,
       end of ty_saida.



data: it_zmmt0087       type table of zmmt0087,
* Inicio - RRIBEIRO - IR216036 - 27.06.2025 - STEFANINI
      it_zmmt0087_aux   type table of zmmt0087,
      it_zmmt0087_na    type table of zmmt0087,
      it_zmmt0087_d     type table of zmmt0087,
      it_zmmt0087_e     type table of zmmt0087,
      it_zmmt0087_final type table of zmmt0087,
      wa_zmmt0087_aux   type zmmt0087,
      wa_zmmt0087_na    type zmmt0087,
* FIM - RRIBEIRO - IR216036 - 27.06.2025 - STEFANINI
      wa_zmmt0087       type zmmt0087,
      it_resb           type table of resb,
      wa_resb           type resb,
      it_rkpf           type table of rkpf,
      wa_rkpf           type rkpf,
      it_makt           type table of makt,
      wa_makt           type makt,
      it_pa0001         type table of pa0001,
      wa_pa0001         type pa0001,
      it_mseg           type table of mseg,
      wa_mseg           type mseg,
*** Stefanini - IR201779 - 25/10/2024 - LAZAROSR - Início de Alteração
      it_mseg_xloek     type table of mseg,
*** Stefanini - IR201779 - 25/10/2024 - LAZAROSR - Fim de Alteração
      it_saida          type table of ty_saida,
      wa_saida          type ty_saida.

data: it_rsparams type table of rsparams,
      wa_rsparams type rsparams.

data: g_custom_container type ref to cl_gui_custom_container,
      g_grid             type ref to cl_gui_alv_grid,
      it_fieldcatalog    type lvc_t_fcat,
      wa_fieldcatalog    type lvc_s_fcat,
      tl_function        type ui_functions,
      wl_function        like tl_function  with header line,
      gs_layout          type lvc_s_layo,
      gs_variant         type disvariant,
      gt_estilo          type lvc_t_styl,
      wa_stable          type lvc_s_stbl value 'XX'.

* Inicio - RRIBEIRO - IR216036 - 27.06.2025 - STEFANINI

types: begin of ty_chave,
         rsnum type zmmt0087-rsnum,
         rspos type zmmt0087-rspos,
         matnr type zmmt0087-matnr,
         mblnr type zmmt0087-mblnr,
       end of ty_chave.

data: lt_cheve_ex type sorted table of ty_chave with unique key rsnum rspos matnr mblnr,
      ls_chave    type ty_chave.

* Fim - RRIBEIRO - IR216036 - 27.06.2025 - STEFANINI



selection-screen begin of block b1 with frame title text-001.
  select-options: p_pernr   for pa0001-pernr,
                  p_werks   for resb-werks,
                  p_lgort   for resb-lgort,
                  p_kostl   for rkpf-kostl,
                  p_tp_mat  for mara-mtart,
                  p_date    for rkpf-rsdat obligatory.
selection-screen end of block b1.


start-of-selection.


  perform z_busca_dados.
  perform z_tratar_dados.


*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_busca_dados .

  select *
    from zmmt0087 as a
    inner join resb as b  on b~rsnum = a~rsnum and a~rspos = b~rspos
    inner join mkpf as c  on c~mblnr = a~mblnr
    inner join rkpf as c2 on c2~rsnum = a~rsnum
    inner join mara as m  on m~matnr = b~matnr
       into corresponding fields of table it_zmmt0087
   where a~pernr in p_pernr
     and a~operacao = 'E'
     and b~werks in p_werks
     and b~xwaok eq abap_true
     and b~lgort in p_lgort
     and c2~kostl in p_kostl
     and m~mtart in p_tp_mat
     and c~budat in p_date.

* Inicio - RRIBEIRO - IR216036 - 27.06.2025 - STEFANINI

  select *
    from zmmt0087
    into table it_zmmt0087_aux
    for all entries in it_zmmt0087
    where rsnum = it_zmmt0087-rsnum
    and   rspos = it_zmmt0087-rspos
    and   matnr = it_zmmt0087-matnr
    and   mblnr = it_zmmt0087-mblnr
    and   operacao = 'D'.

  if it_zmmt0087[] is not initial.
    select *
      from zmmt0087
      into table it_zmmt0087_na
      for all entries in it_zmmt0087
      where pernr = it_zmmt0087-PERNR.
  endif.

  loop at it_zmmt0087_na assigning field-symbol(<fs_dado>).
    if <fs_dado>-operacao = 'D'.
      append <fs_dado> to it_zmmt0087_d.
    elseif <fs_dado>-operacao = 'E'.
      append <fs_dado> to it_zmmt0087_e.
    endif.
  endloop.

  " Verificar quais D não têm correspondente E
  loop at it_zmmt0087_d assigning field-symbol(<fs_d>).
    data(lv_encontrou_e) = abap_false.

    read table it_zmmt0087_e transporting no fields
         with key rsnum = <fs_d>-rsnum.

    if sy-subrc <> 0. " Não encontrou E correspondente
      <fs_d>-operador = 'Nao'.
*      <fs_d>-quant = <fs_d>-quant * -1.
      append <fs_d> to it_zmmt0087_final.
    endif.
  endloop.

  append lines of it_zmmt0087_final to it_zmmt0087.


* Fim - RRIBEIRO - IR216036 - 27.06.2025 - STEFANINI

  if it_zmmt0087[] is not initial.

    select *
      from resb into table it_resb
       for all entries in it_zmmt0087
     where rsnum eq it_zmmt0087-rsnum
      and  rspos eq it_zmmt0087-rspos.

    select *
      from makt into table it_makt
     for all entries in it_zmmt0087
     where matnr eq it_zmmt0087-matnr.

    select *
      from pa0001 into table it_pa0001
      for all entries in it_zmmt0087
    where pernr eq it_zmmt0087-pernr.

    select *
      from mseg into table it_mseg
     for all entries in it_zmmt0087
     where mblnr eq it_zmmt0087-mblnr
      and  matnr eq it_zmmt0087-matnr.
    if sy-subrc is initial.
      sort it_mseg by mblnr matnr.
    endif.

    check it_resb[] is not initial.

    select *
      from rkpf into table it_rkpf
     for all entries in it_resb
     where rsnum eq it_resb-rsnum.

  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  Z_TRATAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_tratar_dados .
  data  wl_color    type kkblo_specialcol.

*** Stefanini - IR201779 - 25/10/2024 - LAZAROSR - Início de Alteração
  it_mseg_xloek = it_mseg.
  sort it_mseg_xloek by mblnr mjahr rsnum rspos.
*** Stefanini - IR201779 - 25/10/2024 - LAZAROSR - Fim de Alteração

* Inicio - RRIBEIRO - IR216036 - 30.06.2025 - STEFANINI
  sort it_zmmt0087 by rsnum rspos matnr mblnr.
* Fim - RRIBEIRO - IR216036 - 30.06.2025 - STEFANINI

  loop at  it_zmmt0087 into wa_zmmt0087.

    wa_saida-mblnr = wa_zmmt0087-mblnr.
    wa_saida-rsdat = wa_zmmt0087-data.

    read table it_mseg assigning field-symbol(<fs_mseg>)
    with key mblnr = wa_zmmt0087-mblnr
             matnr = wa_zmmt0087-matnr
    binary search.
    if sy-subrc is initial.
      wa_saida-lgort = <fs_mseg>-lgort.
    endif.

    read table it_resb into wa_resb with key  rsnum = wa_zmmt0087-rsnum
                                              rspos = wa_zmmt0087-rspos.
    if sy-subrc = 0.
      wa_saida-werks      = wa_resb-werks.
      wa_saida-matnr      = wa_resb-matnr.
      wa_saida-quantidade = wa_resb-bdmng.


*** Stefanini - IR201779 - 25/10/2024 - LAZAROSR - Início de Alteração
      if wa_resb-xloek is not initial. " Item marcado como eliminado

        perform z_ajustar_qtd changing wa_saida-quantidade.

      endif.
*** Stefanini - IR201779 - 25/10/2024 - LAZAROSR - Fim de Alteração

    endif.

    read table it_rkpf into wa_rkpf with key rsnum = wa_resb-rsnum.
    if sy-subrc = 0.
      wa_saida-bwart = wa_rkpf-bwart.
      wa_saida-kostl = wa_rkpf-kostl.
      if wa_rkpf-kostl is initial and wa_rkpf-aufnr is not initial.
        select single kostl
          into wa_saida-kostl
          from aufk
          where aufnr = wa_rkpf-aufnr.
      endif.
    endif.

    read table it_pa0001 into wa_pa0001 with key pernr = wa_zmmt0087-pernr.
    if sy-subrc = 0.
      wa_saida-pernr = wa_pa0001-pernr.
      wa_saida-ename = wa_pa0001-ename.
    endif.

    read table it_makt into wa_makt with key matnr = wa_zmmt0087-matnr.
    if sy-subrc = 0.
      wa_saida-maktx = wa_makt-maktx.
    endif.

    loop at it_mseg into wa_mseg
      where mblnr eq wa_zmmt0087-mblnr
      and   mjahr eq wa_zmmt0087-data+0(4)
      and   matnr eq wa_zmmt0087-matnr.

      wa_saida-vlr_total = wa_saida-vlr_total + wa_mseg-dmbtr.

      clear wa_mseg.
    endloop.


    data: valuesnum type standard table of bapi1003_alloc_values_num initial size 0.
    data: valueschar type standard table of bapi1003_alloc_values_char initial size 0.
    data: valuescurr type standard table of bapi1003_alloc_values_curr initial size 0.
    data: return type standard table of bapiret2 initial size 0.
    data: qtd type i.
    data: qtd_data type i. ""AHSS Ajuste periodicidade de troca =1 ##145578

    "CS2024000013 ZMM0158 - Nova coluna de data PSA
    call function 'BAPI_OBJCL_GETDETAIL' "#EC CI_USAGE_OK[2438131]
      exporting
        objectkey       = conv objnum( wa_saida-matnr )
        objecttable     = 'MARA'
        classnum        = 'MATEPI'
        classtype       = '023'
      tables
        allocvaluesnum  = valuesnum
        allocvalueschar = valueschar
        allocvaluescurr = valuescurr
        return          = return.

    loop at valuesnum assigning field-symbol(<get_valor>) where charact = 'ZEPI_PERI'.
      clear: qtd.
      clear: qtd_data.""AHSS Ajuste periodicidade de troca =1 ##145578

      qtd = <get_valor>-value_from.
      qtd_data = trunc( wa_saida-quantidade ). ""AHSS Ajuste periodicidade de troca =1 ##145578

      ""AHSS Ajuste periodicidade de troca =1 ##145578
      if qtd eq 1.
        wa_saida-dtvenc = wa_saida-rsdat + qtd_data.
      else.
        wa_saida-dtvenc = wa_saida-rsdat + qtd.
      endif.
      ""AHSS Ajuste periodicidade de troca =1 ##145578

      if wa_saida-dtvenc lt sy-datum.
        clear: wl_color.
        wl_color-fieldname = 'DTVENC'.
        wl_color-color-col = 6.
        wl_color-color-inv = 6.
        append wl_color to wa_saida-color.
      endif.
    endloop.

* Inicio - RRIBEIRO - IR216036 - 30.06.2025 - STEFANINI

    sort it_zmmt0087_aux by rsnum rspos matnr mblnr.

    read table it_zmmt0087_aux with key rsnum = wa_zmmt0087-rsnum
                                        rspos = wa_zmmt0087-rspos
                                        matnr = wa_zmmt0087-matnr
                                        mblnr = wa_zmmt0087-mblnr transporting no fields.

    select single smbln
      into @data(lv_mblnr)
      from mseg
      where rsnum eq @wa_zmmt0087-rsnum
      and   ( bwart eq '202' or bwart eq '262' or bwart eq '201' or bwart eq '261' )
      and   mblnr is not null.

    if sy-subrc is not initial or wa_zmmt0087-operador eq 'Nao'.
      if wa_zmmt0087-operador eq 'Nao'.
        wa_saida-quantidade = wa_saida-quantidade * -1.
        clear: wl_color.
        wl_color-fieldname = ''.
        wl_color-color-col = 6.
        wl_color-color-int = 1.
        wl_color-color-inv = 0.
        append wl_color to wa_saida-color.
      endif.
      append wa_saida to it_saida.
    elseif lv_mblnr is initial.
      append wa_saida to it_saida.
    endif.

    clear: wa_saida, wa_resb, wa_rkpf,  wa_pa0001, wa_makt, wa_mseg, lv_mblnr.

*    READ TABLE it_mseg INTO wa_mseg WITH KEY SMBLN = wa_zmmt0087-MBLNR.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      APPEND wa_saida TO it_saida.
*    ENDIF.
*
*    CLEAR: wa_saida, wa_resb, wa_rkpf,  wa_pa0001, wa_makt, wa_mseg.

* Fim - RRIBEIRO - IR216036 - 30.06.2025 - STEFANINI

  endloop.


  call screen 0100.

endform.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status 'ST_0100'.
  set titlebar 'TL_0100'.

  perform  z_alv.

  if g_custom_container is initial.

    create object g_custom_container
      exporting
        container_name              = 'CONTAINER'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.

    if g_grid is initial and g_custom_container is not  initial.
      create object g_grid
        exporting
          i_parent          = g_custom_container
        exceptions
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          others            = 5.
    endif.

    wl_function  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_check.
    append wl_function to tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_refresh.
    append wl_function to tl_function.


    gs_layout-stylefname = 'CELLTAB'.
    gs_layout-ctab_fname = 'COLOR'.


    call method g_grid->set_table_for_first_display
      exporting
        is_variant           = gs_variant
        is_layout            = gs_layout
        it_toolbar_excluding = tl_function
      changing
        it_outtab            = it_saida
        it_fieldcatalog      = it_fieldcatalog.


    call method g_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method g_grid->set_ready_for_input
      exporting
        i_ready_for_input = 1.

  else.

    call method g_grid->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = it_fieldcatalog.

    call method g_grid->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.
    when 'EXCEL'.
      perform z_export_excel.

  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Form  Z_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_alv .

  refresh it_fieldcatalog.

  perform preenche_cat using:
        'WERKS'           'Centro'                 '10'     ''     ''     ''     ''   ''      ''    ''   '',
        'KOSTL'           'Centro de Custo'        '14'     ''     ''     ''     ''   ''      ''    ''   '',
        'PERNR'           'Matricula'              '10'     ''     ''     ''     ''   ''      ''    ''   '',
        'ENAME'           'Nome do Colaborador'    '35'     ''     ''     ''     ''   ''      ''    ''   '',
        'MATNR'           'Material'               '10'     'X'     ''     ''     ''   ''      ''    ''   '',
        'MAKTX'           'Descrição Material'     '30'     ''     ''     ''     ''   ''      ''    ''   '',
        'QUANTIDADE'      'Qtd.Movimentada'        '12'     ''     ''     ''     ''   ''      ''    ''   '',
        'VLR_TOTAL'       'Valor Total'            '12'     ''     ''     ''     ''   ''      ''    ''   '',
        'BWART'           'Tipo Mov.'              '10'     ''     ''     ''     ''   ''      ''    ''   '',
        'RSDAT'           'Data Mov.'              '10'     ''     ''     ''     ''   ''      ''    ''   '',
        'DTVENC'          'Periodicidade EPI'      '10'     ''     ''     ''     ''   ''      ''    ''   '', "PSA
        'MBLNR'           'Numero Mov.'            '12'     ''     ''     ''     ''   ''      ''    ''   '',
        'LGORT'           'Depósito'               '10'     ''     ''     ''     ''   ''      ''    ''   ''.

endform.

form preenche_cat using value(p_campo)
                          value(p_desc)
                          value(p_tam)
                          value(p_zero)
                          value(p_hot)
                          value(p_sum)
                          value(p_just)
                          value(p_edit)
                          value(p_table)
                          value(p_fieldname)
                          value(p_f4).

  wa_fieldcatalog-fieldname   = p_campo.
  wa_fieldcatalog-coltext     = p_desc.
  wa_fieldcatalog-scrtext_l   = p_desc.
  wa_fieldcatalog-scrtext_m   = p_desc.
  wa_fieldcatalog-scrtext_s   = p_desc.
  wa_fieldcatalog-outputlen   = p_tam.
  wa_fieldcatalog-hotspot     = p_hot.
  wa_fieldcatalog-no_zero     = p_zero.
  wa_fieldcatalog-do_sum      = p_sum.
  wa_fieldcatalog-just        = p_just.
  wa_fieldcatalog-edit        = p_edit.
  wa_fieldcatalog-ref_table   = p_table.
  wa_fieldcatalog-ref_field   = p_fieldname.
  wa_fieldcatalog-f4availabl  = p_f4.

  append wa_fieldcatalog to it_fieldcatalog.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_EXPORT_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_export_excel .

  types: begin of ty_fieldnames,
           name type char20,
         end of ty_fieldnames.

  types:  t_fieldcat          type table of lvc_s_fcat with default key.
  types:  t_fieldnames_excel  type table of ty_fieldnames with default key.
  data   path(250).

  check it_saida[] is not initial.

  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = ' '
      def_path         = 'C:\'
      mask             = ',*.XLS,'
      mode             = 'S'
      title            = 'Local de Gravação'
    importing
      filename         = path
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.

  check ( sy-subrc = 0 ).

  data(p_local) = |{ path }.XLS|.

  data(t_excel_names) = value t_fieldnames_excel(
    ( name = 'Centro' )               ( name = 'Centro de Custo' )
    ( name = 'Matricula'  )           ( name = 'Nome do Colaborador')
    ( name = 'Material')              ( name = 'Descrição Material')
    ( name = 'Qntd.Retirada')         ( name = 'Valor Total')
    ( name = 'Tipo Mov.')             ( name = 'Data Mov.')
    ( name = 'Numero Mov.' )  ).

  call function 'GUI_DOWNLOAD'
    exporting
      filename            = p_local
      filetype            = 'DAT'
    tables
      data_tab            = it_saida[]
      fieldnames          = t_excel_names[]
    exceptions
      file_open_error     = 1
      file_write_error    = 2
      invalid_filesize    = 3
      invalid_table_width = 4
      invalid_type        = 5
      no_batch            = 6
      unknown_error       = 7
      others              = 8.

  if sy-subrc = 0.
    message 'Arquivos gerados com sucesso' type 'S'.
  else.
    message 'Arquivo processado com erro' type 'E'.
  endif.
endform.

*** Stefanini - IR201779 - 25/10/2024 - LAZAROSR - Início de Alteração
form z_ajustar_qtd changing c_v_quantidade type bdmng.

  read table it_mseg_xloek into wa_mseg with key mblnr = wa_zmmt0087-mblnr
                                                 mjahr = wa_zmmt0087-data+0(4)
                                                 rsnum = wa_zmmt0087-rsnum
                                                 rspos = wa_zmmt0087-rspos
                                                             binary search.

  if sy-subrc is initial.

    c_v_quantidade = wa_mseg-menge.
    clear wa_mseg.

  endif.

endform.
*** Stefanini - IR201779 - 25/10/2024 - LAZAROSR - Fim de Alteração
