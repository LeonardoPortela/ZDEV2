*&---------------------------------------------------------------------*
*& Report  ZRD_zpmt0078_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zrd_zpmt0078_exit.

data: t_return  type standard table of ddshretval,
      v_emp     type bukrs,
      acesso(1) type c.

form f_exit_zpmt0078_0001 changing p_registro_manter type any.

  data: wl_zpmt0078 type zpmt0078.

  clear: wl_zpmt0078.

  wl_zpmt0078-data_registro = sy-datum.
  wl_zpmt0078-hora_registro = sy-uzeit.
  wl_zpmt0078-usuario = sy-uname.

  move-corresponding wl_zpmt0078 to p_registro_manter.

endform.

form f_exit_zpmt0078_0002 using p_registro_manter type any
changing p_error.

  CASE sy-ucomm.
    WHEN 'CHANGE'.
    WHEN 'OPT1'.
    WHEN OTHERS.
  ENDCASE.

  data: wl_zpmt0078 type zpmt0078.

  clear: wl_zpmt0078.

  move-corresponding p_registro_manter to wl_zpmt0078.

  clear: p_error.
  "142670 Efetuar ajustes na tela de lançamento de contrato PSA

  if  wl_zpmt0078-ini_contrato is not initial and
  wl_zpmt0078-fim_contrato is not initial
    AND sy-ucomm <> 'CHANGE'.

    shift wl_zpmt0078-equip left deleting leading '0'.

    select single *
    from zpmt0078
    where equip = @wl_zpmt0078-equip
    and centro = @wl_zpmt0078-centro
    and centro_custo = @wl_zpmt0078-centro_custo
    and emp_locacao = @wl_zpmt0078-emp_locacao
    and ini_contrato >= @wl_zpmt0078-ini_contrato
    and fim_contrato <= @wl_zpmt0078-fim_contrato
    into @data(wa_consulta_zpmt0078).

    if wa_consulta_zpmt0078-contrato_corp is not initial or wa_consulta_zpmt0078-contrato_spot is not initial."wl_zpmt0078-fim_contrato < wl_zpmt0078-ini_contrato .

      p_error = abap_true.
      message s024(sd) with 'Periodo do contrato inválido.'
      display like 'E'.
      exit.

    endif.
  endif.

  if  wl_zpmt0078-emp_locacao is initial or
  wl_zpmt0078-valor_locacao is initial or
  wl_zpmt0078-ini_contrato is initial or
  wl_zpmt0078-fim_contrato is initial or
  wl_zpmt0078-equip is initial or
  wl_zpmt0078-emp_locacao_desc is initial or
    ( wl_zpmt0078-contrato_corp is not initial and wl_zpmt0078-contrato_corp_desc is initial ) or ( wl_zpmt0078-contrato_spot is not initial and wl_zpmt0078-contrato_spot_desc is initial )."142670 Efetuar ajustes na tela de lançamento de contrato PSA

    p_error = abap_true.
    message s024(sd) with 'Todos os campos são obrigatórios.'
    display like 'E'.
    exit.

  endif.


  if  wl_zpmt0078-contrato_corp is not initial and wl_zpmt0078-contrato_spot is not initial.

    p_error = abap_true.
    message s024(sd) with 'Preencha apenas um contrato.'
    display like 'E'.
    exit.

  endif.

  if  wl_zpmt0078-contrato_corp is initial and wl_zpmt0078-contrato_spot is initial.

    p_error = abap_true.
    message s024(sd) with 'Preencha pelo menos um contrato.'
    display like 'E'.
    exit.

  endif.


  if  wl_zpmt0078-equip is not initial.

    unpack wl_zpmt0078-equip  to wl_zpmt0078-equip .

    select single equnr
    from equi
    into @data(lv_dummy)
          where equnr = @wl_zpmt0078-equip
          and eqtyp = 2.
    "142670 Efetuar ajustes na tela de lançamento de contrato PSA
    if lv_dummy is not initial.


      types: begin of ty_status.
               include type bapi_itob_status.
      types: end of ty_status.


      data: it_sysstatus type standard table of ty_status,
            it_usrstatus type standard table of ty_status,
            l_sysstatus  type  j_stext,
            l_userstatus type  asttx,
            lt_return    like  bapiret2.


      call function 'BAPI_EQUI_GETSTATUS'
        exporting
          equipment     = lv_dummy
*         LANGUAGE      = SY-LANGU
*         LANGUAGE_ISO  =
        importing
          systemstatus  = l_sysstatus
          userstatus    = l_userstatus
          return        = lt_return
        tables
          system_status = it_sysstatus
          user_status   = it_usrstatus.

      if it_sysstatus is not initial.
        loop at it_sysstatus assigning field-symbol(<_del>).
          if <_del>-text = 'MREL' or <_del>-text = 'INAT'.
            data(_stop) = 'X'.
          endif.
        endloop.
      endif.


      if _stop = 'X'."sy-subrc <> 0.
        "142670 Efetuar ajustes na tela de lançamento de contrato PSA
        clear: wl_zpmt0078.
        move-corresponding wl_zpmt0078 to p_registro_manter.
        p_error = abap_true.
        message s024(sd) with 'É permitido apenas equip. tipo "Agro Locado" '
        display like 'E'.
        exit.

      else.


      endif.
    else.
      "142670 Efetuar ajustes na tela de lançamento de contrato PSA
      clear: wl_zpmt0078.
      move-corresponding wl_zpmt0078 to p_registro_manter.
      p_error = abap_true.
      message s024(sd) with 'Insira um equipamento válido!" '
      display like 'E'.
      exit.
    endif.

  endif.


  if  wl_zpmt0078-emp_locacao is not initial AND sy-ucomm <> 'CHANGE'.

    unpack wl_zpmt0078-emp_locacao to wl_zpmt0078-emp_locacao.

    select single a~lifnr
    from lfa1 as a inner join lfb1 as b
    on a~lifnr = b~lifnr
    into @data(lv_dummy_emp_loc)
          where b~bukrs = @v_emp
          and a~lifnr = @wl_zpmt0078-emp_locacao
          and a~loevm <> 'X'    "Marcado para eliminação
          and a~sperr <> 'X'    "Bloqueado
          and a~ktokk = 'ZFNJ'. "Forn.Nacional Pessoa Juridica

    if sy-subrc <> 0.

      p_error = abap_true.
      message s024(sd) with 'Empresa de locação inválida!'
      display like 'E'.
      exit.

    endif.

  endif.


  if  wl_zpmt0078-ini_contrato is not initial and
  wl_zpmt0078-fim_contrato is not initial and
  wl_zpmt0078-equip is not initial AND sy-ucomm <> 'CHANGE'.

    select single * from zpmt0078
    into @data(lv_dummy_check)
          where equip         = @wl_zpmt0078-equip
          and contrato_corp = @wl_zpmt0078-contrato_corp
          and contrato_spot = @wl_zpmt0078-contrato_spot
          and ini_contrato  =  @wl_zpmt0078-ini_contrato
          and fim_contrato  =  @wl_zpmt0078-fim_contrato.

    if sy-subrc = 0.

      p_error = abap_true.
      message s024(sd) with 'Já existe contrato ativo para o equipamento'
      display like 'E'.
      exit.

    endif.


  endif.

endform.

form f_exit_zpmt0078_0003 changing p_registro_manter type any.

  data: wl_zpmt0078 type zpmt0078.

  clear: wl_zpmt0078.

  move-corresponding p_registro_manter to wl_zpmt0078.

  wl_zpmt0078-data_registro = sy-datum.
  wl_zpmt0078-hora_registro = sy-uzeit.
  wl_zpmt0078-usuario = sy-uname.

  move-corresponding wl_zpmt0078 to p_registro_manter.

endform.

form f_exit_zpmt0078_0004 changing p_saida type any.

  data: wl_zpmt0078_out type zpmt0078_out.

  clear: wl_zpmt0078_out.
  move-corresponding p_saida to wl_zpmt0078_out.

endform.

form f_exit_zpmt0078_0009 tables it_excl_toolbar
using p_db_tab.

*  IF p_db_tab = 'zpmt0078'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.

endform.

form f_exit_zpmt0078_0014 using p_saida changing p_break.

  perform get_acesso.
  if acesso is not initial.
    case acesso.
      when 'E'.
        message 'Seu usuário não tem permissão de Deletar, favor consultar o Gestor ou Abrir uma SA!' type 'S' display like 'E'.
        p_break = abap_true.
      when 'F'.
      when others.
    endcase.
  else.
    message 'Seu usuário não tem permissão, favor consultar o Gestor ou Abrir uma SA!' type 'S' display like 'E'.
     p_break = abap_true.
  endif.

endform.

form f_exit_zpmt0078_0016 using p_ucomm type sy-ucomm changing p_registro_manter type any p_saida type any.

  data: wl_zpmt0078 type zpmt0078.

  clear: wl_zpmt0078.

  move-corresponding p_registro_manter to wl_zpmt0078.

  if wl_zpmt0078-equip is not initial.

    data lv_equip type equnr.

    lv_equip  = |{ wl_zpmt0078-equip alpha = in }|.

    select single equnr
    from equi
    into @data(lv_dummy)
          where equnr = @lv_equip
          and eqtyp = 2.
    "142670 Efetuar ajustes na tela de lançamento de contrato PSA


    if lv_dummy is not initial.
      "142670 Efetuar ajustes na tela de lançamento de contrato PSA

      types: begin of ty_status.
               include type bapi_itob_status.
      types: end of ty_status.


      data: it_sysstatus type standard table of ty_status,
            it_usrstatus type standard table of ty_status,
            l_sysstatus  type  j_stext,
            l_userstatus type  asttx,
            lt_return    like  bapiret2.


      call function 'BAPI_EQUI_GETSTATUS'
        exporting
          equipment     = lv_equip
*         LANGUAGE      = SY-LANGU
*         LANGUAGE_ISO  =
        importing
          systemstatus  = l_sysstatus
          userstatus    = l_userstatus
          return        = lt_return
        tables
          system_status = it_sysstatus
          user_status   = it_usrstatus.

      if it_sysstatus is not initial.
        loop at it_sysstatus assigning field-symbol(<_del>).
          if <_del>-text = 'MREL' or <_del>-text = 'INAT'.
            data(_stop) = 'X'.
          endif.
        endloop.
      endif.


      if _stop = 'X'."sy-subrc <> 0.
        "142670 Efetuar ajustes na tela de lançamento de contrato PSA
        clear: wl_zpmt0078-equip,
        wl_zpmt0078-equip_desc,
        wl_zpmt0078-centro,
        wl_zpmt0078-centro_desc,
        wl_zpmt0078-centro_custo,
        wl_zpmt0078-centro_custo_desc,
        wl_zpmt0078-emp,
        wl_zpmt0078-emp_desc.

        move-corresponding wl_zpmt0078 to p_registro_manter.

        message 'É permitido apenas equip. válido!' type 'S' display like 'E'.

        exit.

      endif.


      select single eqktx iwerk kostl bukrs
      from V_equi
      into ( wl_zpmt0078-equip_desc,
      wl_zpmt0078-centro,
      wl_zpmt0078-centro_custo,
      wl_zpmt0078-emp )
      where equnr = lv_equip.

      if sy-subrc = 0.
        v_emp = wl_zpmt0078-emp.

      else.

        clear: wl_zpmt0078-equip,
        wl_zpmt0078-equip_desc,
        wl_zpmt0078-centro,
        wl_zpmt0078-centro_desc,
        wl_zpmt0078-centro_custo,
        wl_zpmt0078-centro_custo_desc,
        wl_zpmt0078-emp,
        wl_zpmt0078-emp_desc.
        move-corresponding wl_zpmt0078 to p_registro_manter.
        message 'É permitido apenas equip. válido!' type 'S' display like 'E'.

        exit.
      endif.

    else.

      clear: wl_zpmt0078-equip,
      wl_zpmt0078-equip_desc,
      wl_zpmt0078-centro,
      wl_zpmt0078-centro_desc,
      wl_zpmt0078-centro_custo,
      wl_zpmt0078-centro_custo_desc,
      wl_zpmt0078-emp,
      wl_zpmt0078-emp_desc.
      move-corresponding wl_zpmt0078 to p_registro_manter.
      message 'É permitido apenas equip. válido!' type 'S' display like 'E'.

      exit.
    endif.

  else.

    clear: wl_zpmt0078-equip,
    wl_zpmt0078-equip_desc,
    wl_zpmt0078-centro,
    wl_zpmt0078-centro_desc,
    wl_zpmt0078-centro_custo,
    wl_zpmt0078-centro_custo_desc,
    wl_zpmt0078-emp,
    wl_zpmt0078-emp_desc.

    move-corresponding wl_zpmt0078 to p_registro_manter.
    message 'É permitido apenas equip. válido!' type 'S' display like 'E'.

    exit.

  endif.

  "142670 Efetuar ajustes na tela de lançamento de contrato PSA
  if wl_zpmt0078-centro is not initial.
    select single name1 from t001W where werks = @wl_zpmt0078-centro into @wl_zpmt0078-centro_desc.
  endif.


  if wl_zpmt0078-centro_custo is not initial.

    select single ltext
    from cskt
    into wl_zpmt0078-centro_custo_desc
    where kostl = wl_zpmt0078-centro_custo.

    if sy-subrc <> 0.
      clear wl_zpmt0078-centro_custo_desc.
    endif.
  else.

    clear wl_zpmt0078-centro_custo_desc.

  endif.

  if wl_zpmt0078-emp is not initial.

    select single butxt
    from t001
    into wl_zpmt0078-emp_desc
    where bukrs = wl_zpmt0078-emp.

    if sy-subrc <> 0.
      clear wl_zpmt0078-emp_desc.
    endif.

  else.

    clear wl_zpmt0078-emp_desc.

  endif.

  if  wl_zpmt0078-contrato_corp is not initial.

    unpack wl_zpmt0078-contrato_corp to wl_zpmt0078-contrato_corp.

    select single txz01
    into wl_zpmt0078-contrato_corp_desc
    from ekpo
    where ebeln = wl_zpmt0078-contrato_corp
    and bstyp = 'F'.

    if sy-subrc <> 0.
      clear wl_zpmt0078-contrato_corp_desc.
    endif.

  else.

    clear wl_zpmt0078-contrato_corp_desc.
  endif.

  if  wl_zpmt0078-contrato_spot is not initial.

    unpack wl_zpmt0078-contrato_spot to wl_zpmt0078-contrato_spot.

    select single txz01
    into wl_zpmt0078-contrato_spot_desc
    from ekpo
    where ebeln = wl_zpmt0078-contrato_spot
    and bstyp = 'F'.

    if sy-subrc <> 0.
      clear wl_zpmt0078-contrato_spot_desc.
    endif.

  else.

    clear wl_zpmt0078-contrato_spot_desc.
  endif.

  if  wl_zpmt0078-emp_locacao is not initial.

    unpack wl_zpmt0078-emp_locacao to wl_zpmt0078-emp_locacao.


    select single a~name1
    from lfa1 as a
    inner join lfb1 as b on a~lifnr = b~lifnr
    where a~lifnr = @wl_zpmt0078-emp_locacao
    and a~land1 = 'BR'
    and a~loevm is initial
    and a~sperr is initial
    and a~ktokk = 'ZFNJ'
    and b~bukrs = @v_EMP
    into @wl_zpmt0078-emp_locacao_desc.

    if sy-subrc <> 0.
      clear wl_zpmt0078-emp_locacao_desc.
    endif.

  else.

    clear wl_zpmt0078-emp_locacao_desc.
  endif.


  move-corresponding wl_zpmt0078 to p_saida.
  move-corresponding wl_zpmt0078 to p_registro_manter.

endform.
form f_exit_zpmt0078_0017 using p_tipo.

  if p_tipo = '0001'.
    perform f4_val_equip using '<FS_WA_REGISTRO_MANTER>-EQUIP'
          '<FS_WA_REGISTRO_MANTER>-EQUIP_DESC'.
  endif.

  if p_tipo = '0002'.
    perform f4_val_emp_loc using '<FS_WA_REGISTRO_MANTER>-EMP_LOCACAO'
          '<FS_WA_REGISTRO_MANTER>-EMP_LOCACAO_DESC'.
  endif.

  if p_tipo = '0003'.
    perform f4_val_contrato using '<FS_WA_REGISTRO_MANTER>-CONTRATO_SPOT'
          '<FS_WA_REGISTRO_MANTER>-CONTRATO_SPOT_DESC'
          'CONTRATO_SPOT'.
  endif.

  if p_tipo = '0004'.
    perform f4_val_contrato using '<FS_WA_REGISTRO_MANTER>-CONTRATO_CORP'
          '<FS_WA_REGISTRO_MANTER>-CONTRATO_CORP_DESC'
          'CONTRATO_CORP'.
  endif.


endform.


*&---------------------------------------------------------------------*
*&      Form  F4_VAL
*&---------------------------------------------------------------------*
form f4_val_equip using p_cod type help_info-dynprofld
      p_desc type help_info-dynprofld.

  clear t_return.
  data: t_mapping type standard table of dselc.

*====>  Work Area
  data: s_return  type ddshretval.
  data: s_mapping type dselc.

  select equnr as equip, eqktx as equip_desc
  from v_equi
  into table @data(t_equip)
        where eqtyp = '2'.

  sort t_equip by equip ascending.
  delete adjacent duplicates from t_equip comparing equip.

  "142670 Efetuar ajustes na tela de lançamento de contrato PSA

  types: begin of ty_status.
           include type bapi_itob_status.
  types: end of ty_status.


  data: it_sysstatus type standard table of ty_status,
        it_usrstatus type standard table of ty_status,
        l_sysstatus  type  j_stext,
        l_userstatus type  asttx,
        lt_return    like  bapiret2.

  loop at t_equip assigning field-symbol(<_remove_itens>).
    call function 'BAPI_EQUI_GETSTATUS'
      exporting
        equipment     = <_remove_itens>-equip
*       LANGUAGE      = SY-LANGU
*       LANGUAGE_ISO  =
      importing
        systemstatus  = l_sysstatus
        userstatus    = l_userstatus
        return        = lt_return
      tables
        system_status = it_sysstatus
        user_status   = it_usrstatus.

    if it_sysstatus is not initial.
      loop at it_sysstatus assigning field-symbol(<_del>).
        if <_del>-text = 'MREL' or <_del>-text = 'INAT'.
          data(_delete) = 'X'.
        endif.
      endloop.
    endif.

    if _delete = 'X'.
      delete t_equip where equip = <_remove_itens>-equip.
    endif.


  endloop.

  if sy-sysid = 'DEV'.
    append initial line to t_equip assigning field-symbol(<fs>).
    <fs>-equip = '1234'.
    <fs>-equip_desc = 'teste 1'.
    sy-subrc = 0.
  endif.

  if sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    append s_mapping to t_mapping.
    clear s_mapping.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_desc.
    append s_mapping to t_mapping.
    clear s_mapping.

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'EQUIP'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Equipamento'
        value_org       = 'S'
      tables
        value_tab       = t_equip
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

  call function 'SAPGUI_SET_FUNCTIONCODE'
    exporting
      functioncode           = '=ENTER' "ENTER
    exceptions
      function_not_supported = 1
      others                 = 2.

endform.

form f4_val_emp_loc using p_cod type help_info-dynprofld
      p_desc type help_info-dynprofld.

  clear t_return.
  data: t_mapping type standard table of dselc.

*====>  Work Area
  data: s_return  type ddshretval.
  data: s_mapping type dselc.

  select a~lifnr as emp_locacao, a~name1 as emp_locacao_desc, a~loevm, a~sperr, a~ktokk, b~bukrs
  from lfa1 as a
  inner join lfb1 as b on a~lifnr = b~lifnr
  into table @data(t_emp_loc)
        where a~land1 = 'BR'.

  if sy-sysid = 'DEV'.

    select lifnr name1
    from lfa1 into table t_emp_loc
    where land1 = 'BR'.

  else.

    delete t_emp_loc where loevm = 'X'. "Marcado para eliminação
    delete t_emp_loc where sperr = 'X'. "Bloqueado
    delete t_emp_loc where ktokk <> 'ZFNJ'."Forn.Nacional Pessoa Juridica
    delete t_emp_loc where bukrs <> v_emp.

  endif.


  if sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    append s_mapping to t_mapping.
    clear s_mapping.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_desc.
    append s_mapping to t_mapping.
    clear s_mapping.

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'EMP_LOCACAO'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Empresa Locação'
        value_org       = 'S'
      tables
        value_tab       = t_emp_loc
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

  call function 'SAPGUI_SET_FUNCTIONCODE'
    exporting
      functioncode           = '=ENTER' "ENTER
    exceptions
      function_not_supported = 1
      others                 = 2.



endform.

form f4_val_contrato using p_cod type help_info-dynprofld
      p_desc type help_info-dynprofld
      p_campo type dfies-fieldname.

  clear t_return.
  data: t_mapping type standard table of dselc.

*====>  Work Area
  data: s_return  type ddshretval.
  data: s_mapping type dselc.

  select k~ebeln, p~txz01 up to 50 rows
  into table @data(t_contrato)
        from ekko as k
        inner join ekpo as p
        on k~ebeln = p~ebeln
        where k~bstyp = 'F'.

  if sy-sysid = 'DEV'.

    append initial line to t_contrato assigning field-symbol(<fs_contrato>).
    <fs_contrato>-ebeln = '123456'.
    <fs_contrato>-txz01 = 'Teste contrato'.
    sy-subrc = 0.

  else.


  endif.


  if sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    append s_mapping to t_mapping.
    clear s_mapping.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_desc.
    append s_mapping to t_mapping.
    clear s_mapping.

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = p_campo
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Contrato'
        value_org       = 'S'
      tables
        value_tab       = t_contrato
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

  call function 'SAPGUI_SET_FUNCTIONCODE'
    exporting
      functioncode           = '=ENTER' "ENTER
    exceptions
      function_not_supported = 1
      others                 = 2.



endform.

form get_acesso.

  data: it_param type  ustyp_t_parameters.

  call function 'SUSR_USER_PARAMETERS_GET'
    exporting
      user_name           = sy-uname
    tables
      user_parameters     = it_param
    exceptions
      user_name_not_exist = 1
      others              = 2.


  if it_param is not initial.
    loop at it_param assigning field-symbol(<_get>) where parva = 'X' and parid+0(7) = 'ZPM0104'.
      clear: acesso.
      case <_get>-parid.
        when 'ZPM0104_EDIT'.
          acesso = 'E'.
        when 'ZPM0104_FULL'.
          acesso = 'F'.
        when others.
      endcase.
    endloop.
  else.
  endif.

endform.
