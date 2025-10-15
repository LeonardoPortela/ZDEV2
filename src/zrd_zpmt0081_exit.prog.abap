*&---------------------------------------------------------------------*
*& Report  ZRD_zpmt0075_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zrd_zpmt0081_exit.


form f_exit_zpmt0081_0001 using p_registro_manter type any.


  data: wl_zpmt0081 type zpme0081.

  clear: wl_zpmt0081.

  move-corresponding p_registro_manter to wl_zpmt0081.

  if wl_zpmt0081-us_criacao is initial.
    wl_zpmt0081-dt_criacao      = sy-datum.
    wl_zpmt0081-hr_criacao      = sy-uzeit.
    wl_zpmt0081-us_criacao      = sy-uname.

    wl_zpmt0081-dt_modif      = sy-datum.
    wl_zpmt0081-hr_modif      = sy-uzeit.
    wl_zpmt0081-us_modif      = sy-uname.
  endif.

  move-corresponding wl_zpmt0081 to p_registro_manter.

endform.

form f_exit_zpmt0081_0002 using p_registro_manter type any
                       changing p_error.

  data: wl_zpmt0081 type zpme0081.

  clear: wl_zpmt0081.

  move-corresponding p_registro_manter to wl_zpmt0081.

  if wl_zpmt0081-eqtyp is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha a categoria de equipamento!'.
    exit.
  else.
    select single * from t370U into @data(ls_t370U) where eqtyp eq @wl_zpmt0081-eqtyp and spras eq @sy-langu.
    if sy-subrc ne 0.
      message i024(sd) with 'categoria de equipamento não existe !'.
      p_error = abap_true.
      exit.
    endif.
  endif.

  if wl_zpmt0081-eqart is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o tipo Objeto!'.
    exit.
  else.
    select single * from t370k_t into @data(ls_T370K_T) where eqart eq @wl_zpmt0081-eqart and spras eq @sy-langu.
    if sy-subrc ne 0.
      message i024(sd) with 'O tipo Objeto informando não existe!'.
      p_error = abap_true.
      exit.
    endif.
  endif.

  if wl_zpmt0081-kostl is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o centro de custo!'.
    exit.
  else.
    select single * from cskt into @data(ls_CSKT) where kostl eq @wl_zpmt0081-kostl and spras eq @sy-langu.
    if sy-subrc ne 0.
      message i024(sd) with 'O centro de custo informando não existe!'.
      p_error = abap_true.
      exit.
    endif.
  endif.


  move-corresponding wl_zpmt0081 to p_registro_manter.

endform.


form f_exit_zpme0081_0005 changing p_saida type any.

  data: wl_zpme0081 type zpme0081.

  clear: wl_zpme0081.

  move-corresponding p_saida to wl_zpme0081.

  if wl_zpme0081-us_criacao is initial.
    wl_zpme0081-dt_criacao      = sy-datum.
    wl_zpme0081-hr_criacao      = sy-uzeit.
    wl_zpme0081-us_criacao      = sy-uname.
  else.
    wl_zpme0081-dt_modif      = sy-datum.
    wl_zpme0081-hr_modif      = sy-uzeit.
    wl_zpme0081-us_modif      = sy-uname.
  endif.

  move-corresponding wl_zpme0081 to p_saida.


endform.

form f_exit_zpmt0081_0004 using p_registro_manter type any.

*
  data: wl_zpmt0081 type zpme0081.

  move-corresponding p_registro_manter to wl_zpmt0081.

  if wl_zpmt0081-eqart is not initial.
    select single eartx from t370k_t into wl_zpmt0081-eartx where eqart eq wl_zpmt0081-eqart and spras eq sy-langu.
  endif.

  if wl_zpmt0081-kostl is not initial.
    select single ktext from cskt into wl_zpmt0081-ktext where kostl eq wl_zpmt0081-kostl and spras eq sy-langu.
  endif.



  move-corresponding wl_zpmt0081 to p_registro_manter.

endform.

form  f_exit_zpmt0081_0016 using p_ucomm  type sy-ucomm changing p_registro_manter type any p_saida type any.


  data: wl_zpmt0081 type zpme0081.
*
  clear: wl_zpmt0081.
*
  move-corresponding p_registro_manter to wl_zpmt0081.


*  if wl_zpmt0081-wptxt is not initial.
*    translate wl_zpmt0075-wptxt to upper case.
*  endif.


  move-corresponding wl_zpmt0081 to p_registro_manter.


endform.

form f_exit_zpmt0081_0017 using p_tipo.





  call function 'SAPGUI_SET_FUNCTIONCODE'
    exporting
      functioncode           = '=ENTER' "ENTER
    exceptions
      function_not_supported = 1
      others                 = 2.

  if p_tipo = '0001'.
    perform f4_value using '<FS_WA_REGISTRO_MANTER>-EQTYP' p_tipo.
  endif.

  if p_tipo = '0002'.
    perform f4_value using '<FS_WA_REGISTRO_MANTER>-EQART' p_tipo.

  endif.

  if p_tipo = '0003'.
    perform f4_value using '<FS_WA_REGISTRO_MANTER>-KOSTL' p_tipo.

  endif.


endform.
*&---------------------------------------------------------------------*
*& Form f4_value
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_TIPO
*&---------------------------------------------------------------------*
form f4_value  using p_campo type help_info-dynprofld
                     p_tipo.

  data: zde_FIELDNAME    type dfies-fieldname,
        zde_window_title type c,
        value_tab        type value_tab.

  data: t_return  type standard table of ddshretval,
        t_mapping type standard table of dselc.


  case p_tipo.
    when '0001'. "Categoria

      clear t_return.

      select eqtyp, typtx
        from t370u
        into table @data(lt_T370U)
        where spras eq @sy-langu.
      if sy-subrc = 0.
        call function 'F4IF_INT_TABLE_VALUE_REQUEST'
          exporting
            retfield        = 'EQTYP'
            dynpprog        = sy-cprog
            dynpnr          = sy-dynnr
            dynprofield     = p_campo
            window_title    = 'Categoria de equipamento'
            value_org       = 'S'
          tables
            value_tab       = lt_T370U
            return_tab      = t_return
            dynpfld_mapping = t_mapping
          exceptions
            parameter_error = 1
            no_values_found = 2
            others          = 3.
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.
      endif.

    when '0002'. "Tipo

      clear t_return.

      select eqart, eartx
       from t370k_t
       into table @data(lt_t370k_t)
       where spras eq @sy-langu.
      if sy-subrc = 0.
        call function 'F4IF_INT_TABLE_VALUE_REQUEST'
          exporting
            retfield        = 'EQART'
            dynpprog        = sy-cprog
            dynpnr          = sy-dynnr
            dynprofield     = p_campo
            window_title    = 'Tipo do objeto técnico'
            value_org       = 'S'
          tables
            value_tab       = lt_t370k_t
            return_tab      = t_return
            dynpfld_mapping = t_mapping
          exceptions
            parameter_error = 1
            no_values_found = 2
            others          = 3.
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.
      endif.

    when '0003'. "C.custo

      clear t_return.

      select kostl, ktext, ltext
       from cskt
       into table @data(lt_cskt)
       where spras eq @sy-langu.
      if sy-subrc = 0.
        call function 'F4IF_INT_TABLE_VALUE_REQUEST'
          exporting
            retfield        = 'KOSTL'
            dynpprog        = sy-cprog
            dynpnr          = sy-dynnr
            dynprofield     = p_campo
            window_title    = 'Tipo do objeto técnico'
            value_org       = 'S'
          tables
            value_tab       = lt_cskt
            return_tab      = t_return
            dynpfld_mapping = t_mapping
          exceptions
            parameter_error = 1
            no_values_found = 2
            others          = 3.
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.
      endif.
  endcase.

  call function 'SAPGUI_SET_FUNCTIONCODE'
    exporting
      functioncode           = '=ENTER' "ENTER
    exceptions
      function_not_supported = 1
      others                 = 2.

endform.
