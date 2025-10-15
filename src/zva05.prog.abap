*&---------------------------------------------------------------------*
*& Report ZVA05
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zva05.


data: it_saida type table of zsd_va05,
      wa_saida type zsd_va05,
      lt_fcat  type lvc_t_fcat.

data: lr_data            type ref to data,
      lr_data_line       type ref to data,
      lr_data_descr      type ref to cl_abap_datadescr,
      lr_data_line_descr type ref to cl_abap_datadescr.

cl_salv_bs_runtime_info=>set(
  exporting display  = abap_false
            metadata = abap_false
            data     = abap_true ).


field-symbols: <lt_data>      type any table,
               <lt_data_line> type any table,
               <ls_data>      type any,
               <ls_data_line> type any.



include sd_sales_document_data_i01.
data: gv_cust_func1 type salv_de_function,
      gv_cust_func2 type salv_de_function,
      gv_cust_func3 type salv_de_function,
      gv_cust_func4 type salv_de_function,
      gv_cust_func5 type salv_de_function,
      gv_cust_func6 type salv_de_function.
include sd_sales_document_main_sel_i02.
include rfm_psst_main_sel.
include rfm_main_sel.
include sd_sales_document_supp_sel_i03.
include sd_sales_document_stat_sel.

initialization.

start-of-selection.

  " Monta catálogo LVC a partir da estrutura DDIC
  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name = 'ZSD_VA05'
    changing
      ct_fieldcat      = lt_fcat.

  loop at lt_fcat assigning field-symbol(<fs_cat>).
    if <fs_cat>-fieldname = 'ORT01F'.
      <fs_cat>-reptext = 'Cidade Filial'.
      <fs_cat>-scrtext_l = 'Cidade Filial'.
      <fs_cat>-scrtext_m = 'Cidade Filial'.
      <fs_cat>-scrtext_s = 'Cidade Filial'.
    endif.
    if <fs_cat>-fieldname = 'PSTLZF'.
      <fs_cat>-reptext = 'CEP Filial'.
      <fs_cat>-scrtext_l = 'CEP Filial'.
      <fs_cat>-scrtext_m = 'CEP Filial'.
      <fs_cat>-scrtext_s = 'CEP Filial'.
    endif.
    if <fs_cat>-fieldname = 'REGIOF'.
      <fs_cat>-reptext = 'Região Filial'.
      <fs_cat>-scrtext_l = 'Região Filial'.
      <fs_cat>-scrtext_m = 'Região Filial'.
      <fs_cat>-scrtext_s = 'Região Filial'.
    endif.
    if <fs_cat>-fieldname = 'TXJCDF'.
      <fs_cat>-reptext = 'Domicilio Filial'.
      <fs_cat>-scrtext_l = 'Domicilio Filial'.
      <fs_cat>-scrtext_m = 'Domicilio Filial'.
      <fs_cat>-scrtext_s = 'Domicilio Filial'.
    endif.

  endloop.

  submit sd_sales_document_view
          with svbeln  in svbeln
          with sauart  in sauart
          with skunnr  in skunnr
          with saudat  in saudat
          with smatnr  in smatnr
          with serdat  in serdat
          with spartner  in spartner
          with sempl   in sempl
          with sernam  in sernam
          with svkorg  in svkorg
          with svtweg  in svtweg
          with sspart  in sspart
          with svkbur  in svkbur
          with svkgrp  in svkgrp
          with st_psst  in st_psst
          with st_seayr in st_seayr
          with st_sea  in st_sea
          with st_col  in st_col
          with st_thm  in st_thm
          exporting list to memory
          and return.
  try.
      cl_salv_bs_runtime_info=>get_data_ref(
          importing r_data_descr      = lr_data_descr
                    r_data_line_descr = lr_data_line_descr ).

      if lr_data_descr is not initial.
        create data lr_data type handle lr_data_descr.


        assign lr_data->* to <lt_data>.

        cl_salv_bs_runtime_info=>get_data(
          importing t_data      = <lt_data> ).
      endif.

    catch cx_salv_bs_sc_runtime_info.
*      MESSAGE 'Não é possível recuperar os dados ALV' TYPE 'E'.
  endtry.

  cl_salv_bs_runtime_info=>clear_all( ).
  if lr_data_descr is initial.
    exit.
  endif.
  assign lr_data->* to <ls_data>.

  loop at <lt_data> assigning <ls_data>.
    move-corresponding <ls_data> to wa_saida.
    append wa_saida to it_saida.
  endloop.
  if it_saida[] is not initial.
    select *
      from kna1
      into table @data(it_kna1)
      for all entries in @it_saida
    where kunnr = @it_saida-kunnr.

    sort it_kna1 by kunnr.

    select vbap~vbeln,vbap~posnr, t001w~werks, t001w~ort01,t001w~pstlz,t001w~regio,t001w~txjcd
     from vbap
     inner join t001w on t001w~werks = vbap~werks
     into table @data(it_t001w)
     for all entries in @it_saida
    where vbeln = @it_saida-vbeln
    and   posnr = @it_saida-posnr.

    sort it_t001w by vbeln posnr.

    loop at it_saida assigning field-symbol(<fs_saida>).
      read table it_kna1 into data(wa_kna1) with key kunnr = <fs_saida>-kunnr binary search.
      if sy-subrc = 0.
        move-corresponding wa_kna1 to <fs_saida>.
      endif.
      read table it_t001w into data(wa_t001w) with key vbeln = <fs_saida>-vbeln
                                                       posnr = <fs_saida>-posnr binary search.
      if sy-subrc = 0.
        <fs_saida>-werks  = wa_t001w-werks.
        <fs_saida>-ort01f = wa_t001w-ort01.
        <fs_saida>-pstlzf = wa_t001w-pstlz.
        <fs_saida>-regiof = wa_t001w-regio.
        <fs_saida>-txjcdf = wa_t001w-txjcd.
      endif.
    endloop.
  endif.
  " Exibe usando REUSE_ALV_GRID_DISPLAY
  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program = sy-repid
      i_save = 'A'
      is_layout_lvc      = value lvc_s_layo( zebra = abap_true cwidth_opt = abap_true )
      it_fieldcat_lvc    = lt_fcat
    tables
      t_outtab           = it_saida.

end-of-selection.
