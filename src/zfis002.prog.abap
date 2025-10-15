*&---------------------------------------------------------------------*
*& Report  ZFIS002
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zfis002.
types:
  ty_lines   like tline,
* structure to define text table
  begin of source,
   line like tline-tdline,
  end of source,
* ABAP source code table type definition
  source_table type source occurs 0.

data: pm_bukrs   like t001-bukrs,
      pm_bupla   like bsis-bupla,
      pm_spmon   like s001-spmon.
data: it_values  like zst_zfis002 occurs 0 with header line,
      it_month   like t247        occurs 0 with header line,
      it_lines   like tline       occurs 0 with header line,
      wa_values  like zst_zfis002.

data: wa_sadr    like sadr,
      wa_branch  like j_1bbranch,
      wg_cgc     like j_1bwfield-cgc_number.
data: wg_fm_name type rs38l_fnam,
      wg_month   like sy-tabix,
      wg_month_txt(30).
data:  g_txtcontainer type ref to cl_gui_custom_container.
data:  wg_text        type ref to cl_gui_textedit.
data:  it_text        type source_table.
data: wg_dat_ini      like sy-datum,
      wg_dat_fim      like sy-datum,
      wg_data(10),
      wg_cfop(7).

ranges: rg_hkont      for bsis-hkont,
        rg_taxtyp     for j_1bnfstx-taxtyp.

constants: c_hkont_01_01   like bsis-hkont value '0000213009',
           c_hkont_03_01   like bsis-hkont value '0000213007',
           c_hkont_07_01   like bsis-hkont value '0000213005',
           c_hkont_07_02   like bsis-hkont value '0000213001',
           c_hkont_09_01   like bsis-hkont value '0000213005'.

data: begin of it_bsis occurs 0,
        hkont    like bsis-hkont,
        shkzg    like bsis-shkzg,
        dmbtr    like bsis-dmbtr,
      end of it_bsis.
data: begin of it_tax occurs 0,
       docnum   like j_1bnfstx-docnum,
       taxval   like j_1bnfstx-taxval,
       taxtyp   like j_1bnfstx-taxtyp,
       cfop     like j_1bnflin-cfop,
       direct   like j_1bnfdoc-direct,
      end of it_tax.


start-of-selection.
*  perform f_get_bukrs.
*  perform f_get_month.
*  perform f_fake_data.
*  perform f_call_ssf.
*&---------------------------------------------------------------------*
*&      Form  F_GET_BUKRS
*&---------------------------------------------------------------------*
form f_get_bukrs .
  clear: wa_sadr, wg_cgc, wa_branch.
  call function 'J_1B_BRANCH_READ'
    exporting
      branch        = pm_bupla
      company       = pm_bukrs
    importing
      address       = wa_sadr
      branch_record = wa_branch
      cgc_number    = wg_cgc
    exceptions
      others        = 4.
*  check sy-subrc is initial.
  if not sy-subrc is initial.
    message e782(8z) with pm_bupla pm_bukrs.
  endif.
endform.                    " F_GET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  f_fake_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_fake_data .

  do 10 times.
    unpack sy-index to it_values-key.
    it_values-debit  = sy-index * 1209654673.
    it_values-credit = sy-index * 3275677562.
    it_values-waers = 'BRL'.
    concatenate 'Descrição do item' it_values-key
           into it_values-bezei
           separated by space.
    if sy-index = 1 or sy-index = 2 or sy-index = 3.
      it_values-type = 'C'.
    endif.
    if sy-index = 4 or sy-index = 5 or sy-index = 6 or sy-index = 7.
      it_values-type = 'D'.
    endif.
    if sy-index = 8 or sy-index = 9 or sy-index = 10.
      it_values-type = 'T'.
    endif.
    append it_values.
  enddo.

endform.                    " f_fake_data
*&---------------------------------------------------------------------*
*&      Form  f_call_ssf
*&---------------------------------------------------------------------*
form f_call_ssf .
  call function 'SSF_FUNCTION_MODULE_NAME'
    exporting
      formname = 'ZFIS002'
    importing
      fm_name  = wg_fm_name
    exceptions
      others   = 3.
  check sy-subrc is initial.
  call function wg_fm_name
    exporting
      sadr   = wa_sadr
      cgc    = wg_cgc
      branch = wa_branch
      month  = wg_month_txt
    tables
      resumo = it_values
      texto  = it_lines
    exceptions
      others = 5.
  check sy-subrc is initial.

endform.                    " f_call_ssf
*&---------------------------------------------------------------------*
*&      Form  f_get_month
*&---------------------------------------------------------------------*
form f_get_month .
  if pm_spmon is initial.
    message e140(12).
  endif.
  wg_month = pm_spmon+4.
  call function 'MONTH_NAMES_GET'
    tables
      month_names = it_month
    exceptions
      others      = 2.
  check sy-subrc is initial.
  read table it_month index wg_month.
  check sy-subrc is initial.
  concatenate it_month-ltx
              'DE'(001)
              pm_spmon(4)
         into wg_month_txt separated by space.
  translate wg_month_txt to upper case.
  wg_dat_ini = pm_spmon.
  wg_dat_ini+6 = '01'.
  wg_dat_fim = wg_dat_ini.
  add 50 to wg_dat_fim.
  wg_dat_fim+6 = '01'.
  subtract 1 from wg_dat_fim.

endform.                    " f_get_month

include zfis002_modules_100.
*&---------------------------------------------------------------------*
*&      Form  f_call_form
*&---------------------------------------------------------------------*
form f_call_form .
  perform f_get_bukrs.
  perform f_get_month.
  perform f_get_data.
  perform f_build_summary.
  perform f_call_ssf.

endform.                    " f_call_form

*&---------------------------------------------------------------------*
*&      Form  f_create_text
*&---------------------------------------------------------------------*
form f_create_text  using pi_container_name
                          pi_selected_text type source_table
                 changing pr_txteditor type ref to cl_gui_textedit.
  data: wl_modo type i.
*  IF wg_block IS INITIAL.
*    MOVE 0 TO wl_modo.
*  ELSE.
*    MOVE 1 TO wl_modo.
*  ENDIF.
  if pr_txteditor is initial.
    create object g_txtcontainer
      exporting
        container_name = pi_container_name.
    create object pr_txteditor
      exporting
        wordwrap_mode              = 2
        wordwrap_position          = 72
        wordwrap_to_linebreak_mode = 1
        parent                     = g_txtcontainer.
    call method pr_txteditor->set_statusbar_mode
      exporting
        statusbar_mode = 0.
    call method pr_txteditor->set_toolbar_mode
      exporting
        toolbar_mode = 0.
  endif.
  if pi_container_name = 'CN_ROUT'.
    wl_modo = 1.
  else.
    wl_modo = 0.
  endif.
  if pi_container_name = 'CN_BLCK'.
    wl_modo = 1.
  endif.

  call method pr_txteditor->set_readonly_mode
    exporting
      readonly_mode = wl_modo.

  call method pr_txteditor->set_text_as_r3table
    exporting
      table = pi_selected_text.
endform.                    " f_cria_texto

*&---------------------------------------------------------------------*
*&      Form  f_get_text
*&---------------------------------------------------------------------*
form f_get_text using pi_selected_text      type        source_table
                      pi_g_txteditor_name   type ref to cl_gui_textedit.
*--- Text aus OCX
  clear pi_selected_text.
  free pi_selected_text.

  call method pi_g_txteditor_name->get_text_as_r3table
    importing
      table = pi_selected_text.
*  CALL METHOD pi_g_txteditor_name->free.
endform.                    " f_get_text
*&---------------------------------------------------------------------*
*&      Form  f_get_data
*&---------------------------------------------------------------------*
form f_get_data .
  perform f_def_ranges.
  select hkont shkzg dmbtr
    from bsis
    into table it_bsis
   where bukrs eq pm_bukrs
     and bupla eq pm_bupla
     and budat between wg_dat_ini and wg_dat_fim.

  select j_1bnfstx~docnum j_1bnfstx~taxval j_1bnfstx~taxtyp
         j_1bnflin~cfop
         j_1bnfdoc~direct
    from j_1bnfstx
             inner join j_1bnfdoc on j_1bnfstx~docnum eq j_1bnfdoc~docnum
             inner join j_1bnflin on j_1bnfstx~docnum eq j_1bnflin~docnum
                                 and j_1bnfstx~itmnum eq j_1bnflin~itmnum
    into table it_tax
   where j_1bnfdoc~bukrs  eq pm_bukrs
     and j_1bnfdoc~branch eq pm_bupla
     and j_1bnfdoc~pstdat between wg_dat_ini and wg_dat_fim
     and j_1bnfstx~taxtyp in rg_taxtyp
     and j_1bnfdoc~doctyp eq '1'
     and j_1bnfdoc~cancel eq space
     and j_1bnfstx~stattx eq space.

endform.                    " f_get_data
*&---------------------------------------------------------------------*
*&      Form  f_def_ranges
*&---------------------------------------------------------------------*
form f_def_ranges .
  refresh: rg_taxtyp, rg_hkont.
  rg_hkont-sign = 'EQ'.
  rg_hkont-option = 'I'.
  rg_hkont-low = c_hkont_01_01.
  append rg_hkont.
  rg_hkont-low = c_hkont_03_01.
  append rg_hkont.
  rg_hkont-low = c_hkont_07_01.
  append rg_hkont.
  rg_hkont-low = c_hkont_07_02.
  append rg_hkont.
  append 'IEQICM0'  to rg_taxtyp.
  append 'IEQICM1'  to rg_taxtyp.
  append 'IEQICM2'  to rg_taxtyp.
  append 'IEQICM3'  to rg_taxtyp.
  append 'IEQICMF'  to rg_taxtyp.
  append 'IEQIFR1'  to rg_taxtyp.
  append 'IEQIC1O'  to rg_taxtyp.
  append 'IEQICM4'  to rg_taxtyp.
  append 'IEQICMN'  to rg_taxtyp.
  append 'IEQICMO'  to rg_taxtyp.
  append 'IEQICMX'  to rg_taxtyp.

endform.                    " f_def_ranges

**********************************************************************
* FORM    :  f_build_summary
* Created :  04.03.2008 00:39:49
**********************************************************************
form f_build_summary.
  refresh it_values.
  append '01' to it_values.
  append '02' to it_values.
  append '03' to it_values.
  append '04' to it_values.
  append '05' to it_values.
  append '06' to it_values.
  append '07' to it_values.
  append '08' to it_values.
  append '09' to it_values.
  append '10' to it_values.
  loop at it_bsis.
    clear it_values.
    case it_bsis-hkont .
      when c_hkont_01_01.
        check it_bsis-shkzg = 'H'.
        it_values-key = '01'.
        it_values-credit = it_bsis-dmbtr.
        perform f_collect.
      when c_hkont_03_01.
        check it_bsis-shkzg = 'H'.
        it_values-key = '03'.
        it_values-credit = it_bsis-dmbtr.
        perform f_collect.
      when c_hkont_07_01.
        check it_bsis-shkzg = 'S'.
        it_values-key = '07'.
        it_values-debit = it_bsis-dmbtr.
        perform f_collect.
        it_values-key = '09'.
        it_values-debit = it_bsis-dmbtr.
        perform f_collect.
      when c_hkont_07_02.
        check it_bsis-shkzg = 'S'.
        it_values-key = '07'.
        it_values-debit = it_bsis-dmbtr.
        perform f_collect.
    endcase.
  endloop.
  loop at it_tax .
    clear it_values.
    case it_tax-direct .
      when '1'.
        it_values-credit = it_tax-taxval.
        it_values-key    = '02'.
        perform f_collect.
        continue.
      when '2'.     it_values-debit  = it_tax-taxval.
      when others.  continue.
    endcase.
    write it_tax-cfop to wg_cfop.
    case wg_cfop(1).
      when '5'.    it_values-key = '04'.
      when '6'.    it_values-key = '05'.
      when '7'.    it_values-key = '06'.
      when others. continue.
    endcase.
    perform f_collect.
  endloop.

* Campos Calculados
  clear wa_values.
  read table it_values index 8 into wa_values.
  read table it_values index 9.
  it_values-credit = wa_values-credit - wa_values-debit - it_values-debit.
  modify it_values index 9.
  wa_values = it_values.
  read table it_values index 10.
  it_values-debit = wa_values-debit.
  modify it_values index 10.

  loop at it_values.
    case sy-tabix.
      when 1.
        it_values-bezei = 'Saldo Credor Anterior'.
        it_values-type  = 'C'.
      when 2.
        it_values-bezei = 'Entradas com Crédito'.
        it_values-type  = 'C'.
      when 3.
        it_values-bezei = 'Outros Créditos (energia, fretes, etc)'.
        it_values-type  = 'C'.
      when 4.
        it_values-bezei = 'Saídas Internas'.
        it_values-type  = 'D'.
      when 5.
        it_values-bezei = 'Saídas Interestaduais'.
        it_values-type  = 'D'.
      when 6.
        it_values-bezei = 'Saídas para Exportação'.
        it_values-type  = 'D'.
      when 7.
        it_values-bezei = 'Outros Débitos (Estornos, Diferecial de Alíq, etc)'.
        it_values-type  = 'D'.
      when 8.
        it_values-bezei = 'Totais'.
        it_values-type  = 'T'.
      when 9.
        it_values-bezei = 'Saldo Credor / Imposto a Recolher'.
        it_values-type  = 'T'.
      when 10.
        write wg_dat_fim to wg_data.
        concatenate 'Data do Recolhimento:' wg_data
               into it_values-bezei
               separated by space.
        it_values-type  = ''.
    endcase.
    it_values-waers = 'BRL'.
    modify it_values.
  endloop.

endform. "f_build_summary

*&---------------------------------------------------------------------*
*&      Form  f_collect
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_collect.
  collect it_values.
  check it_values-key lt '08'.
  it_values-key = '08'.
  collect it_values.
endform.                    "f_collect
