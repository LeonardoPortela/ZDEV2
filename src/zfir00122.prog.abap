*&---------------------------------------------------------------------*
*& REPORT ZFIR00122
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zfir00122.

include zfir00122_form.

tables: t001,vbak, bsad.

data : type_augdt  type dats,
       lt_seltab   type table of rsparams,
       ls_seltab   like line of lt_seltab,
       p_ovpedj(1),
       p_dcsimj(1).

selection-screen begin of block frame1 with frame title text-000.

  parameters: lboxtipo(1) as listbox visible length 15 default 'C' .
  parameters: lboxupar(6) as listbox visible length 20 default 'OVPEDJ'.
  parameters: lboxtdoc(2) as listbox visible length 15 default 'AB'.
  select-options: p_bukrs for t001-bukrs no intervals, "OBLIGATORY
  p_augdt for type_augdt no intervals no-extension,
  p_spart for vbak-spart no intervals,
  p_parid for bsad-kunnr.

selection-screen end of block frame1.

at selection-screen output.

  perform lboxtipo.
  perform lboxupar.
  perform lboxtdoc.

initialization.


at selection-screen.

  case sy-ucomm.
    when 'ONLI'.

      if lboxtipo is initial.
        message 'O CAMPO SELEÇÃO CLIENTE/FORNECEDOR É OBRIGATÓRIO!' type 'I' display like 'I'.
        exit.
      else.

        case lboxtipo.
          when 'C'.
            clear: ls_seltab.
            ls_seltab-selname = 'RB_CLI'.
            ls_seltab-kind    = 'P'.
            ls_seltab-sign    = 'I'.
            ls_seltab-option  = 'EQ'.
            ls_seltab-low     = 'X'.
            append ls_seltab to lt_seltab.
          when 'F'.
            clear: ls_seltab.
            ls_seltab-selname = 'RB_FORN'.
            ls_seltab-kind    = 'P'.
            ls_seltab-sign    = 'I'.
            ls_seltab-option  = 'EQ'.
            ls_seltab-low     = 'X'.
            append ls_seltab to lt_seltab.
          when others.
        endcase.

      endif.

      if lboxupar is initial.
        message 'O CAMPO UNIFICAÇÃO PARTIDAS É OBRIGATÓRIO!' type 'I' display like 'I'.
        exit.
      else.
        case lboxupar.
          when 'OVPEDJ'.
            p_ovpedj = 'X'.
            p_dcsimj = ' '.
          when 'DCSIMJ'.
            p_ovpedj = ' '.
            p_dcsimj = 'X'.
          when others.
        endcase.
      endif.

      if lboxtdoc is initial.
        message 'O CAMPO  TIPO DE DOCUMENTO É OBRIGATÓRIO!' type 'I' display like 'I'.
        exit.
      else.
        clear: ls_seltab.
        ls_seltab-selname = 'P_BLT_CP'.
        ls_seltab-kind    = 'P'.
        ls_seltab-sign    = 'I'.
        ls_seltab-option  = 'EQ'.
        ls_seltab-low     = lboxtdoc.
        append ls_seltab to lt_seltab.
      endif.

      if p_bukrs[] is initial.
        message 'O CAMPO EMPRESA É OBRIGATÓRIO!' type 'I' display like 'I'.
        exit.
      else.

        loop at p_bukrs[] assigning field-symbol(<bukrs>).
          clear: ls_seltab.
          ls_seltab-selname = 'P_BUKRS'.
          ls_seltab-kind    = 'S'.
          ls_seltab-sign    = 'I'.
          ls_seltab-option  = 'EQ'.
          ls_seltab-low     = <bukrs>-low.
          ls_seltab-high    = <bukrs>-low.
          append ls_seltab to lt_seltab.
        endloop.

      endif.

      loop at p_parid assigning field-symbol(<parid>).
        clear: ls_seltab.
        ls_seltab-selname = 'P_PARID'.
        ls_seltab-kind    = 'S'.
        ls_seltab-sign    = 'I'.
        ls_seltab-option  = 'EQ'.
        ls_seltab-low     = <parid>-low.
        ls_seltab-high    = <parid>-low.
        append ls_seltab to lt_seltab.
      endloop.

      if p_augdt is initial.
        message 'O CAMPO DATA DE COMPENSAÇÃO É OBRIGATÓRIO!' type 'I' display like 'I'.
        exit.
      else.

        loop at p_augdt[] assigning field-symbol(<augdt>).
          clear: ls_seltab.
          ls_seltab-selname = 'P_AUGDT'.
          ls_seltab-kind    = 'S'.
          ls_seltab-sign    = 'I'.
          ls_seltab-option  = 'EQ'.
          ls_seltab-low     = <augdt>-low.
          ls_seltab-high    = <augdt>-low.
          append ls_seltab to lt_seltab.
        endloop.

      endif.

      if p_spart is initial.
*    MESSAGE 'O CAMPO SETOR DE ATIVIDADE É OBRIGATÓRIO!' TYPE 'I' DISPLAY LIKE 'I'.
*    EXIT.

        clear: ls_seltab.
        ls_seltab-selname = 'P_SPART'.
        ls_seltab-kind    = 'S'.
        ls_seltab-sign    = 'I'.
        ls_seltab-option  = 'EQ'.
        ls_seltab-low     = '02'.
        append ls_seltab to lt_seltab.

        clear: ls_seltab.
        ls_seltab-selname = 'P_SPART'.
        ls_seltab-kind    = 'S'.
        ls_seltab-sign    = 'I'.
        ls_seltab-option  = 'EQ'.
        ls_seltab-low     = '03'.
        append ls_seltab to lt_seltab.

        clear: ls_seltab.
        ls_seltab-selname = 'P_SPART'.
        ls_seltab-kind    = 'S'.
        ls_seltab-sign    = 'I'.
        ls_seltab-option  = 'EQ'.
        ls_seltab-low     = '04'.
        append ls_seltab to lt_seltab.

      else.

        loop at p_spart[] assigning field-symbol(<spart>).
          clear: ls_seltab.
          ls_seltab-selname = 'P_SPART'.
          ls_seltab-kind    = 'S'.
          ls_seltab-sign    = 'I'.
          ls_seltab-option  = 'EQ'.
          ls_seltab-low     = <spart>-low.
          ls_seltab-high    = <spart>-low.
          append ls_seltab to lt_seltab.
        endloop.

      endif.

      "Autoriza fazer compensação Total
      clear: ls_seltab.
      ls_seltab-selname = 'P_CPLIB'.
      ls_seltab-kind    = 'P'.
      ls_seltab-sign    = 'I'.
      ls_seltab-option  = 'EQ'.
      ls_seltab-low     = 'X'.
      append ls_seltab to lt_seltab.

    when others.
  endcase.

start-of-selection.

  if sy-batch is not initial.

  else.

    cl_salv_bs_runtime_info=>set(
      exporting
        display  = abap_false
        metadata = abap_false
        data     = abap_true ).

    submit zfir058
    with selection-table lt_seltab
    with p_ovpedj = p_ovpedj
    with p_dcsimj = p_dcsimj
    and return
    exporting list to memory.

    data: lr_data            type ref to data,
          lr_data_line       type ref to data,
          lr_data_descr      type ref to cl_abap_datadescr,
          lr_data_line_descr type ref to cl_abap_datadescr.

    field-symbols: <lt_data>      type any table,
                   <lt_data_line> type any table,
                   <ls_data>      type any,
                   <ls_data_line> type any.

    data: lo_dynamic_line  type ref to data.

    try.
        cl_salv_bs_runtime_info=>get_data_ref(
          importing
            r_data_descr      = lr_data_descr
            r_data_line_descr = lr_data_line_descr ).

        if lr_data_descr is not initial.
          create data lr_data type handle lr_data_descr.
          assign lr_data->* to <lt_data>.
          cl_salv_bs_runtime_info=>get_data(
            importing
              t_data = <lt_data> ).
        endif.
      catch cx_salv_bs_sc_runtime_info.
*      MESSAGE 'Não é possível recuperar os dados ALV' TYPE 'E'.
    endtry.

    cl_salv_bs_runtime_info=>clear_all( ).
    if lr_data_descr is initial.
      exit.
    endif.
    assign lr_data->* to <ls_data>.

    break-point.

    sort <lt_data> ascending.
    loop at <lt_data> assigning <ls_data>.

    endloop.

  endif.
