*&---------------------------------------------------------------------*
*&  Include           ZPPR0028_FORM
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  ZAPONTAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zapontamento .

  if obj_main->t_aufk is not initial.
    clear zaponta.

    zaponta-aufnr                = v_ordem.
    zaponta-activity             = v_vornr.
    zaponta-ktext1               = v_ktext.
    if zaponta-duration_normal_unit is initial.
      zaponta-duration_normal_unit = 'H'.
    endif.
    call screen 0200 starting at 5 5 ending at 86 24.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.

  get cursor field w_cursor_field.
  case sy-ucomm.
    when 'VOLTAR'.
      leave to screen 0.
    when 'EXIT'.
      clear sy-ucomm.
      leave to screen 0.
    when 'BACK'.
      clear sy-ucomm.
      leave to screen 0.
    when 'CLEAR'.
      clear sy-ucomm.
      leave to screen 0.
    when 'V_CANCELAR'.
      perform clear_tela.
      leave to screen 0.
    when 'V_CONFIRMA'.
      perform validar_dados.
    when 'ENTER'.
      perform check_operacao.
    when 'V_NOVO'.
      perform clear_tela.
    when 'ZINPUT_CAL_HR'.
      perform set_calc_hr.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Form  SELEC_OPERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selec_operacao .
  free it_aufk[].
  select a~aufnr a~vaplz b~aufpl c~vornr c~ltxa1 a~werks e~ktext
  from aufk as a
  inner join afko as b on b~aufnr = a~aufnr
  inner join afvc as c on c~aufpl = b~aufpl
  inner join crhd as d on d~arbpl = a~vaplz and d~werks = a~werks
  inner join crtx as e on e~objid = d~objid
  into corresponding fields of table it_aufk
  where a~aufnr eq zaponta-aufnr
    and c~vornr eq zaponta-activity.


  if it_aufk[] is initial.
    message i000(o0) with 'A operação' zaponta-activity 'não existe na ordem' zaponta-aufnr display like 'E'.
  else.
    read table it_aufk with key aufnr = zaponta-aufnr
                                vornr = zaponta-activity.

    if sy-subrc = 0.
      zaponta-description          = it_aufk-ltxa1.
      zaponta-werks                = it_aufk-werks.

      if zaponta-duration_normal_unit is initial.
        zaponta-duration_normal_unit = 'H'.
      endif.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  SELEC_EMPREGADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selec_empregado .
  data wa_empregado type ty_hrp1001.
  data: cont_ctrab type p.
  data: hinic type sy-uzeit.
  data: hfim  type sy-uzeit.
  data: hpaus type char8.

  if zaponta-pernr is not initial.
    free it_empregado[].
    select distinct a~objid b~kapid c~sobid d~pernr d~sname a~arbpl a~werks f~ktext
    from crhd as a
    inner join crca    as b on b~objid = a~objid
    inner join hrp1001 as c on c~objid = b~kapid
    inner join pa0001  as d on d~pernr = c~sobid
    inner join aufk    as e on e~werks = a~werks
    inner join crtx    as f on f~objid = a~objid
    into corresponding fields of table it_empregado
    where c~sobid eq zaponta-pernr
      and e~aufnr eq zaponta-aufnr
      and a~werks eq zaponta-werks
     and  c~otype eq 'KA'.
    sort it_empregado ascending by pernr.
    delete adjacent duplicates from it_empregado comparing pernr objid.

    if it_empregado[] is not initial.
      clear cont_ctrab.
      loop at it_empregado.
        add 1 to cont_ctrab.
      endloop.

      read table it_empregado with key  pernr = zaponta-pernr
                                        werks = zaponta-werks.

      if zaponta-sname <> it_empregado-sname.
        if cont_ctrab > 1.
          call screen 0300 starting at 5 5 ending at 70 20. "Tela de selação centro de trabalho.
        else.
          read table it_empregado with key  pernr = zaponta-pernr
                                            werks = zaponta-werks.
          if sy-subrc = 0.
            zaponta-sname     = it_empregado-sname.
            zaponta-work_cntr = it_empregado-arbpl.
            zaponta-ktext     = it_empregado-ktext.
          endif.
        endif.
      endif.

    else.
*      MESSAGE TEXT-003 TYPE 'I' DISPLAY LIKE 'E'.
      message i000(o0) with 'Código do empregado' zaponta-pernr 'não cadastrado para um centro de trabalho'.
      clear zaponta-sname.
      clear it_empregado-sname.
      clear it_empregado-ktext.
      clear zaponta-ktext.
      clear it_empregado-arbpl.
      clear zaponta-work_cntr.
      clear zaponta-begzt.
      clear zaponta-endzt.
      clear zaponta-pause.
      clear zaponta-einzt.
      clear zaponta-ngrad.
      clear zaponta-ueberlast.
      clear zaponta-v_sobrcarg.
      clear zaponta-einzh.
    endif.
  endif.

  free it_kako[].
  if zaponta-work_cntr is not initial.
    select c~kapid a~werks c~begzt c~endzt c~pause c~ueberlast c~ngrad
  from crhd as a
  inner join crca as b on b~objid = a~objid
  inner join kako as c on c~kapid = b~kapid
  into corresponding fields of table it_kako
  where a~arbpl eq zaponta-work_cntr
    and a~werks eq zaponta-werks.

    if it_kako[] is not initial.
      loop at it_kako.

        zaponta-begzt     =  it_kako-begzt.
        zaponta-endzt     =  it_kako-endzt.
        zaponta-pause     =  it_kako-pause.

        it_kako-begzt = ( it_kako-begzt / 60 ) / 60.
        it_kako-endzt = ( it_kako-endzt / 60 ) / 60.
        it_kako-pause = ( it_kako-pause / 60 ) / 60.

        zaponta-einzt     = ( it_kako-endzt - it_kako-begzt ) - it_kako-pause.
        zaponta-ngrad     = ( it_kako-ngrad / 100 ) * 100.
        zaponta-ueberlast = ( it_kako-ueberlast / 100 ) * 100.

        zaponta-einzt      = ( zaponta-einzt * zaponta-ngrad ) / 100.
        zaponta-v_sobrcarg = ( zaponta-einzt * zaponta-ueberlast ) / 100.
        zaponta-einzh      = ( zaponta-einzt + zaponta-v_sobrcarg ).
      endloop.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  SELEC_DESC_PARADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selec_desc_parada .

  if zaponta-grund is not initial.
    select *
    from trugt
    into corresponding fields of table it_selec_parada
    where grund eq zaponta-grund
      and werks eq zaponta-werks
      and spras eq 'P'.

    if it_selec_parada[] is not initial.
      loop at it_selec_parada.
        clear zaponta-grdtx.
        zaponta-grdtx = it_selec_parada-grdtx.
      endloop.
    else.
      message i000(o0) with 'Causa desvio' zaponta-grund 'não existe para o centro' zaponta-werks display like 'E'.
      clear it_selec_parada-grdtx.
      clear zaponta-grdtx.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  CLEAR_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form clear_tela .
  clear: zaponta-description,
         zaponta-work_cntr,
         zaponta-pernr,
         zaponta-sname,
         zaponta-grund,
         zaponta-grdtx,
         zaponta-afrud,
         zaponta-duration_normal_unit,
         zaponta-isdd ,
         zaponta-isdz ,
         zaponta-iedd ,
         zaponta-iedz ,
         zaponta-begzt,
         zaponta-endzt,
         zaponta-pause,
         zaponta-einzt,
         zaponta-ngrad,
         zaponta-ueberlast,
         zaponta-v_sobrcarg,
         zaponta-einzh,
         zaponta-ktext,
         zaponta-fin_conf,
         zaponta-duration_normal_unit,
         soma_total,
         zaponta-ltxa1. "BUG #175277 - MMSILVA - 30.04.2025
*         ZAPONTA-ACTIVITY.
*
*  IF V_ORDEM IS NOT INITIAL.
*    CLEAR ZAPONTA-AUFNR.
*    ZAPONTA-AUFNR = V_ORDEM.
*  ENDIF.
endform.
*&---------------------------------------------------------------------*
*&      Form  CHECK_OPERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_operacao .
  free it_aufk[].
  if zaponta-activity is initial.
    message i000(o0) with 'É necessário atribuir a ordem' zaponta-aufnr 'a um nº de operação'.
  else.
    if zaponta-activity is not initial.
      perform selec_operacao.
    endif.

    if zaponta-pernr is not initial.
      if it_aufk-ltxa1 is not initial.
        perform selec_empregado.
      else.
        message i000(o0) with 'A operação' zaponta-activity 'não existe na ordem' zaponta-aufnr display like 'E'.
      endif.
    else.
      clear zaponta-sname.
      clear it_empregado-ktext.
      clear zaponta-ktext.
      clear it_empregado-arbpl.
      clear zaponta-work_cntr.
      clear zaponta-begzt.
      clear zaponta-endzt.
      clear zaponta-pause.
      clear zaponta-einzt.
      clear zaponta-ngrad.
      clear zaponta-ueberlast.
      clear zaponta-v_sobrcarg.
      clear zaponta-duration_normal_unit.
      clear zaponta-einzh.
    endif.

    if zaponta-isdd is not initial.
      if it_empregado-sname is not initial.

      else.
*        MESSAGE TEXT-003 TYPE 'I' DISPLAY LIKE 'E'.
*        MESSAGE I000(O0) WITH 'Código do empregado' ZAPONTA-PERNR 'não cadastrado para o centro de trabalho' ZAPONTA-WORK_CNTR DISPLAY LIKE 'E'.
        clear zaponta-sname.
        clear it_empregado-ktext.
        clear zaponta-ktext.
        clear it_empregado-arbpl.
        clear zaponta-work_cntr.
        clear zaponta-begzt.
        clear zaponta-endzt.
        clear zaponta-pause.
        clear zaponta-einzt.
        clear zaponta-ngrad.
        clear zaponta-ueberlast.
        clear zaponta-v_sobrcarg.
        clear zaponta-duration_normal_unit.
        clear zaponta-einzh.
      endif.
    endif.

    if zaponta-grund is not initial.
      perform selec_desc_parada.
    endif.

    if zaponta-isdd is not initial.
      zaponta-budat = zaponta-isdd.
    else.
      clear zaponta-budat.
    endif.

    case abap_false.
      when zaponta-isdd or zaponta-isdz or zaponta-iedd or zaponta-iedz.
        exit.
    endcase.

    if zaponta-isdd is not initial and zaponta-iedd is not initial.

      if zaponta-isdd > zaponta-iedd.
        message 'Data limite ultrapassada para lançamento.' type 'I' display like 'E'.
      else.
        if  zaponta-isdd is not initial
          and zaponta-isdz is not initial
            and zaponta-iedd is not initial
              and zaponta-iedz is not initial.

          if zaponta-isdd > sy-datum or zaponta-iedd > sy-datum or zaponta-isdd = sy-datum and zaponta-isdz > sy-uzeit or zaponta-iedd = sy-datum and zaponta-iedz > sy-uzeit.
            message i209(ru) display like 'E'.
          else.
            if zaponta-isdd = zaponta-iedd
               and zaponta-isdz > zaponta-iedz.
              message 'Data limite ultrapassada para lançamento.' type 'I' display like 'E'.
            else.

              data(ld_beg_da) = zaponta-isdd.
              data(ld_end_da) = zaponta-iedd.

              clear: ld_no_day, ld_no_month, ld_no_year, ld_no_cal_day, soma_data, soma_hora, soma_total.

              "Calculando total de dias.
              call function 'HR_AUPBS_MONTH_DAY'
                exporting
                  beg_da     = ld_beg_da
                  end_da     = ld_end_da
                importing
                  no_day     = ld_no_day
                  no_month   = ld_no_month
                  no_year    = ld_no_year
                  no_cal_day = ld_no_cal_day.

              "Calculando total de horas.
              soma_data     = ( ld_no_cal_day - 1 ) * 24.
              soma_hora     = ( zaponta-iedz  - zaponta-isdz  ) / 60.
              soma_hora     = ( soma_hora / 60 ).
              soma_total    = ( soma_data + soma_hora ).


              if soma_total > 0.
                if soma_total > zaponta-einzh."IT_KAKO-V_EINZH.
                  message i000(o0) with 'Carga horaria informada' soma_total 'é maior que tempo de utilização cadastrado' zaponta-einzh display like 'E'.
                else.

                  if zaponta-afrud is not initial and zaponta-duration_normal_unit is not initial.
                    if zaponta-duration_normal_unit = 'H' or  zaponta-duration_normal_unit = 'MIN'.

                      case zaponta-duration_normal_unit.
                        when 'MIN'.
                          zaponta-afrud = ( zaponta-afrud / 60 ).
                          zaponta-duration_normal_unit = 'H'.
                      endcase.


                      if zaponta-afrud > soma_total.
                        message i000(o0) with 'Valor informado é maior que 'soma_total zaponta-duration_normal_unit display like 'E'.
                      endif.

                    else.
                      message e011 with zaponta-duration_normal_unit.

                    endif.
                  endif.
                endif.
              else.
                message 'Data limite ultrapassada para lançamento.' type 'I' display like 'E'.
                exit.
              endif.
            endif.
          endif.
        endif.
      endif.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  SELEC_INF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selec_inf .

  data: zretun    type p decimals 2,
        v_ponta   type p decimals 2,
        cont_erro type p decimals 2,
        wa_apont  type ty_zaponta.

  if zaponta is not initial.
    free t_afru[].
    free tl_afru[].
*    Check se ja existe lançamento para empregado no mesmo periodo para mesma ordem ou ordens diferente.
    select *
    from afru
    into corresponding fields of table t_afru
    where pernr eq zaponta-pernr
      and stokz ne abap_true
      and stzhl eq ' '.

    sort t_afru ascending by isdd isdz.
    clear zretun.
    clear v_ponta.


    loop at t_afru where pernr = zaponta-pernr
                    and   isdd = zaponta-isdd or iedd = zaponta-iedd.

      if sy-subrc = 0.
        if zaponta-pernr = t_afru-pernr and zaponta-isdd between t_afru-isdd and t_afru-iedd.
          if zaponta-pernr = t_afru-pernr and zaponta-isdz between t_afru-isdz and t_afru-iedz.

            add 1 to zretun.

            move-corresponding t_afru to tl_afru.
            append tl_afru.
          endif.
        endif.
      endif.
      clear t_afru.
    endloop.


    loop at it_aponta into wa_apont where pernr = zaponta-pernr
                                     and   isdd = zaponta-isdd or iedd = zaponta-iedd.

      if zaponta-pernr = wa_apont-pernr and zaponta-isdd between wa_apont-isdd and wa_apont-iedd.
        if zaponta-pernr = wa_apont-pernr and zaponta-isdz between wa_apont-isdz and wa_apont-iedz.
          add 1 to zretun.
        endif.

      endif.
      clear wa_apont.
    endloop.

    loop at it_aponta assigning field-symbol(<l_apont>) where aufnr = zaponta-aufnr
                                                       and activity = zaponta-activity.
      if <l_apont>-fin_conf is not initial.
        message i000 with 'Confimarção final ja foi selecionada'display like 'E'.
        add 1 to cont_erro.
        continue.
      endif.
    endloop.


    if zretun = 0.
*      Check calculo total de horas digitado.
      loop at t_afru  assigning field-symbol(<w_afru>) where pernr = zaponta-pernr
                                                          and isdd = zaponta-isdd.


        if <w_afru>-isdd = zaponta-isdd and <w_afru>-pernr is not initial.
          if <w_afru>-ismne = 'MIN'.
            <w_afru>-ismnw = ( <w_afru>-ismnw / 60 ).
            add <w_afru>-ismnw to v_ponta.
          else.

            add <w_afru>-ismnw to v_ponta.
          endif.
        endif.
      endloop.


      loop at it_aponta assigning field-symbol(<w_apont>) where pernr = zaponta-pernr
                                                            and isdd  = zaponta-isdd.

        if <w_apont>-isdd  = zaponta-isdd and <w_apont>-pernr is not initial.
          if <w_apont>-duration_normal_unit = 'MIN'.
            <w_apont>-afrud = ( <w_apont>-afrud / 60 ).

            add <w_apont>-afrud to v_ponta.

          else.
            add <w_apont>-afrud to v_ponta.
          endif.
        endif.
      endloop.

*      IF ZAPONTA-DURATION_NORMAL_UNIT = 'MIN'.
*        ZAPONTA-AFRUD = ( ZAPONTA-AFRUD / 60 ).
*      ENDIF.

      v_ponta = ( v_ponta + zaponta-afrud ).

      if v_ponta > zaponta-einzh.
        message i008 with v_ponta zaponta-einzh display like 'E'.
      else.
        if cont_erro is initial.
          append zaponta to it_aponta.
          move-corresponding it_aponta to obj_main->it_opera.
          sort it_aponta ascending by pernr isdd isdz.
          sort obj_main->it_opera ascending by pernr isdd isdz.
          clear cont_erro.
        else.
          clear cont_erro.
        endif.
      endif.
    else.
      message i009 with zaponta-pernr zaponta-sname display like 'E'.
      if tl_afru[] is not initial.
        call screen 0400 starting at 5 5 ending at 110 20.
      endif.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form validar_dados .
  if zaponta-work_cntr              is initial
  or zaponta-pernr                  is initial
  or zaponta-isdd                   is initial
  or zaponta-isdz                   is initial
  or zaponta-iedd                   is initial
  or zaponta-iedz                   is initial
  or zaponta-afrud                  is initial
  or zaponta-budat                  is initial.
    message text-005 type 'I' display like 'E'.
  else.
    perform selec_inf.
    perform clear_tela.
    obj_main->alv1->refresh_table_display( is_stable = obj_main->stable )."STABLE
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_EMPREGADO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module busca_empregado input.

  select a~werks a~arbpl c~sobid d~pernr d~sname a~objid b~kapid f~ktext
  from crhd as a
  inner join crca    as b on b~objid = a~objid
  inner join hrp1001 as c on c~objid = b~kapid
  inner join pa0001  as d on d~pernr = c~sobid
  inner join aufk    as e on e~werks = a~werks
  inner join crtx    as f on f~objid = a~objid
    into corresponding fields of table it_hrp1001
    where e~aufnr eq zaponta-aufnr
      and c~otype eq 'KA'.
  sort it_hrp1001 ascending by pernr.
  delete adjacent duplicates from it_hrp1001 comparing pernr objid.


  check it_hrp1001[] is not initial.
  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'PERNR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'PERNR'
      value_org       = 'S'
    tables
      value_tab       = it_hrp1001
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_INTERVALO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module busca_intervalo input.
  data: t_trugt type table of trugt.

  free t_trugt.
  select *
  from trugt
  into table t_trugt
  where werks eq zaponta-werks
   and  spras eq 'PT'.

  check t_trugt is not initial.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'GRUND'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'GRUND'
      value_org       = 'S'
    tables
      value_tab       = t_trugt
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0300 output.

  data:
 lst_layout type lvc_s_layo.

  data: url(255)                type c,
        p_text                  type sdydo_text_element,
        p_text_2                type sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            type sdydo_text_table,
        p_text_table_2          type sdydo_text_table,
        vl_cont                 type i,
        vl_butxt                type t001-butxt,
        vl_dates1               type char10,
        vl_dates2               type char10.

  set pf-status 'T0300'.
  set titlebar 'T0301'.

*  FREE G_CUSTOM_CONTAINER.
* Adicionando Logo Marca no Cabeçalho
  if g_custom_container is initial.

    create object g_custom_container
      exporting
        container_name              = 'CONTAINER'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

*    IF  IT_EMPREGADO[] IS NOT INITIAL.

    perform fill_it_fieldcatalog using:
          1  'WERKS'  'IT_EMPREGADO '  '20'   ' '  ' '  ' '  'Centro            '  ''  ''  'CRHD     '  ' ',
          2  'ARBPL'  'IT_EMPREGADO '  '20'   ' '  ' '  ' '  'C.trabalho        '  ''  ''  'CRHD     '  ' ',
          3  'PERNR'  'IT_EMPREGADO '  '20'   ' '  ' '  ' '  'Cod empregado     '  ''  ''  'PA0001   '  ' ',
          4  'SNAME'  'IT_EMPREGADO '  '20'   ' '  ' '  ' '  'Nome              '  ''  ''  'PA0001   '  ' '.


    gs_layout-sel_mode   = 'A'.
    gs_layout-cwidth_opt = 'X'.
    clear: it_exclude_fcode, it_exclude_fcode[].

    create object ctl_alv
      exporting
        i_parent = g_custom_container.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_excl_all.
    append wa_exclude_fcode to it_exclude_fcode.

    call method ctl_alv->set_table_for_first_display
      exporting
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      changing
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_empregado[]
        it_sort              = it_sort.

    set handler: lcl_event_handler=>on_double_click for ctl_alv.

  else.

    ls_stable-row = 'X'.
    ls_stable-col = 'X'.

    call method ctl_alv->refresh_table_display
      exporting
        is_stable = ls_stable
      exceptions
        finished  = 1
        others    = 2.

    if sy-subrc <> 0.
    endif.

  endif.


endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0300 input.
  case sy-ucomm.
    when 'EXIT'.
      message i000(o0) with 'Selecione centro de trabalho com double click'.
*      LEAVE TO SCREEN 0.
  endcase.


endmodule.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_7098   text
*      -->P_7099   text
*      -->P_7100   text
*      -->P_7101   text
*      -->P_7102   text
*      -->P_7103   text
*      -->P_7104   text
*      -->P_7105   text
*      -->P_7106   text
*      -->P_7107   text
*      -->P_7108   text
*----------------------------------------------------------------------*
form fill_it_fieldcatalog using value(p_colnum)
                                value(p_fieldname)
                                value(p_tabname)
                                value(p_len)
                                value(p_edit)
                                value(p_icon)
                                value(p_do_sum)
                                value(p_header)
                                value(p_emphasize)
                                value(p_hotspot)
                                value(p_ref_table)
                                value(p_ref_field).

  data:  wa_fieldcatalog  type lvc_s_fcat.
  wa_fieldcatalog-col_pos     = p_colnum.
  wa_fieldcatalog-fieldname   = p_fieldname.
  wa_fieldcatalog-tabname     = p_tabname.
  wa_fieldcatalog-outputlen   = p_len.
  wa_fieldcatalog-edit        = p_edit.
  wa_fieldcatalog-icon        = p_icon.
  wa_fieldcatalog-do_sum      = p_do_sum.
  wa_fieldcatalog-coltext     = p_header.
  wa_fieldcatalog-emphasize   = p_emphasize.
  wa_fieldcatalog-hotspot     = p_hotspot.
  wa_fieldcatalog-ref_table   = p_ref_table.
  wa_fieldcatalog-ref_table   = p_ref_field.
*  WA_FIELDCATALOG-CHECKTABLE  = P_CHECKTABLE.



  wa_fieldcatalog-hotspot     = p_ref_field.


*  WA_FIELDCATALOG-EXCP_CONDS  = P_EXCP_CONDS.

  gs_layout-excp_conds    = 'X'.
  gs_layout-zebra         = 'X'.
  gs_layout-sel_mode      = 'A'.
  gs_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout-totals_bef    = ''.

  append wa_fieldcatalog to it_fieldcatalog.


endform.                    " F_DEFINE_CONTAINER_HEADER

class lcl_event_handler implementation.
  method on_double_click.
    data: wa_afvc type ty_aufk.
    data  wa_aufk type ty_aufk.

    if it_empregado is not initial.
      data: wa_empregado type ty_hrp1001.
      read table it_empregado into wa_empregado index e_row.

      check sy-subrc is initial.
      zaponta-sname     = wa_empregado-sname.
      zaponta-work_cntr = wa_empregado-arbpl.
      zaponta-ktext     = wa_empregado-ktext.
      clear wa_empregado.
      leave to screen 0.
    endif.

    if it_afvc[] is not initial.
      read table it_afvc into wa_afvc index e_row.
      v_vornr = wa_afvc-vornr.
      leave to screen 0.
    endif.
  endmethod.
endclass.                    "LCL_EVENT_HANDLER IMPLEMENTATION

form fill_it_fieldcatalog_1 using value(p_colnum)
                                value(p_fieldname)
                                value(p_tabname)
                                value(p_len)
                                value(p_edit)
                                value(p_icon)
                                value(p_do_sum)
                                value(p_header)
                                value(p_emphasize)
                                value(p_hotspot)
                                value(p_ref_table)
                                value(p_ref_field).

  data:  wa_fieldcatalog_1  type lvc_s_fcat.
  wa_fieldcatalog_1-col_pos     = p_colnum.
  wa_fieldcatalog_1-fieldname   = p_fieldname.
  wa_fieldcatalog_1-tabname     = p_tabname.
  wa_fieldcatalog_1-outputlen   = p_len.
  wa_fieldcatalog_1-edit        = p_edit.
  wa_fieldcatalog_1-icon        = p_icon.
  wa_fieldcatalog_1-do_sum      = p_do_sum.
  wa_fieldcatalog_1-coltext     = p_header.
  wa_fieldcatalog_1-emphasize   = p_emphasize.
  wa_fieldcatalog_1-hotspot     = p_hotspot.
  wa_fieldcatalog_1-ref_table   = p_ref_table.
  wa_fieldcatalog_1-ref_table   = p_ref_field.
*  WA_FIELDCATALOG-CHECKTABLE  = P_CHECKTABLE.
  wa_fieldcatalog_1-hotspot     = p_ref_field.


*  WA_FIELDCATALOG-EXCP_CONDS  = P_EXCP_CONDS.

  gs_layout_1-excp_conds    = 'X'.
  gs_layout_1-zebra         = 'X'.
  gs_layout_1-sel_mode      = 'A'.
  gs_layout_1-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout_1-totals_bef    = ''.

  append wa_fieldcatalog_1 to it_fieldcatalog_1.


endform.                    " F_DEFINE_CONTAINER_HEADER

form fill_it_fieldcatalog_2 using value(p_colnum)
                                value(p_fieldname)
                                value(p_tabname)
                                value(p_len)
                                value(p_edit)
                                value(p_icon)
                                value(p_do_sum)
                                value(p_header)
                                value(p_emphasize)
                                value(p_hotspot)
                                value(p_ref_table)
                                value(p_ref_field).

  data:  wa_fieldcatalog_2  type lvc_s_fcat.
  wa_fieldcatalog_2-col_pos     = p_colnum.
  wa_fieldcatalog_2-fieldname   = p_fieldname.
  wa_fieldcatalog_2-tabname     = p_tabname.
  wa_fieldcatalog_2-outputlen   = p_len.
  wa_fieldcatalog_2-edit        = p_edit.
  wa_fieldcatalog_2-icon        = p_icon.
  wa_fieldcatalog_2-do_sum      = p_do_sum.
  wa_fieldcatalog_2-coltext     = p_header.
  wa_fieldcatalog_2-emphasize   = p_emphasize.
  wa_fieldcatalog_2-hotspot     = p_hotspot.
  wa_fieldcatalog_2-ref_table   = p_ref_table.
  wa_fieldcatalog_2-ref_table   = p_ref_field.
*  WA_FIELDCATALOG-CHECKTABLE  = P_CHECKTABLE.
  wa_fieldcatalog_2-hotspot     = p_ref_field.


*  WA_FIELDCATALOG-EXCP_CONDS  = P_EXCP_CONDS.

  gs_layout_2-excp_conds    = 'X'.
  gs_layout_2-zebra         = 'X'.
  gs_layout_2-sel_mode      = 'A'.
  gs_layout_2-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout_2-totals_bef    = ''.

  append wa_fieldcatalog_2 to it_fieldcatalog_2.


endform.                    " F_DEFINE_CONTAINER_HEADER

class lcl_event_handler_nota implementation.
  method on_double_click.
    data: wa_empregado type ty_hrp1001.
    read table it_empregado into wa_empregado index e_row.

    check sy-subrc is initial.
    zaponta-sname     = wa_empregado-sname.
    zaponta-work_cntr = wa_empregado-arbpl.
    zaponta-ktext     = wa_empregado-ktext.
    clear wa_empregado.
    leave to screen 0.
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_SAIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form confirm_sair .
  data: p_respo type c.

  call function 'POPUP_TO_CONFIRM'
    exporting        "TITLEBAR = 'Confirmar'
      text_question         = 'Sair da transação sem salvar as alterações?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      display_cancel_button = ' '
    importing
      answer                = p_respo.

  if p_respo = 1.
    leave to screen 0.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  EXIBIR_ORDEM_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exibir_ordem_nota .
  get cursor field w_cursor_field.
  case w_cursor_field.
    when 'V_ORDEM'."Ordem Manuteção

      if sy-subrc = 0.
        call function 'POPUP_TO_CONFIRM'
          exporting        "TITLEBAR = 'Confirmar'
            text_question         = 'Deseja acessar qual visão?'
            text_button_1         = 'Exibir ordem'
            text_button_2         = 'Exibir apto.hrs'
            display_cancel_button = 'X' "VALUE(DISPLAY_CANCEL_BUTTON) DEFAULT 'X'
          importing
            answer                = p_resp.

        case p_resp.
          when '1'.
            if v_ordem is not initial.
              set parameter id 'ANR' field v_ordem.
              call transaction 'IW33' and skip first screen .
            endif.

          when '2'.
            break-point.
            clear ti_bdcdata.
            perform f_bdc_data using:
'        '      '0000'  'T'   '            '      'BS AA X   F',
'RIAFRU20'      '1000'  'X'   '            '        '           ',
'        '      '0000'  ' '   'BDC_CURSOR  '      'VARIANT    ',
'        '      '0000'  ' '   'BDC_OKCODE  '      '=ONLI      ',
'        '      '0000'  ' '   'DY_IAR      '      'X          ',
'        '      '0000'  ' '   'DY_ABG      '      'X          ',
'        '      '0000'  ' '   'AUFNR_O-LOW '      v_ordem      ,
'        '      '0000'  ' '   'ERSDA_C-LOW '      '           ',
'        '      '0000'  ' '   'ERSDA_C-HIGH'      '           ',
*'        '      '0000'  ' '   'ISDZ_C-LOW  '      '00:00:00   ',
*'        '      '0000'  ' '   'ISDZ_C-HIGH '      '00:00:00   ',
*'        '      '0000'  ' '   'IEDZ_C-LOW  '      '00:00:00   ',
*'        '      '0000'  ' '   'IEDZ_C-HIGH '      '00:00:00   ',
'        '      '0000'  ' '   'VARIANT     '      '/APONT. M.O',
'SAPMSSY0'      '0120'  'X'   '            '        '           ',
*'        '      '0000'  ' '   'BDC_CURSOR  '      '04/03      ',
'        '      '0000'  ' '   'BDC_OKCODE  '      '=BACK      ',
'RIAFRU20'      '1000'  'X'   '            '        '           ',
'        '      '0000'  ' '   'BDC_OKCODE  '      '/EE        ',
'        '      '0000'  ' '   'BDC_CURSOR  '      'SELSCHEM   '.


            perform zf_call_transaction using 'IW47' changing p_erro.
            if p_erro is not initial.
              message text-007 type 'S' display like 'E'.
            endif.


          when others.
            leave to screen 0100.
        endcase.
      endif.



    when 'V_NOTA'.
      if v_nota is not initial.
        set parameter id 'IQM' field v_nota.
        call transaction 'IW22' and skip first screen .

        wait up to 5 seconds.

        v_ordem = |{ v_ordem alpha = in }|.
*        V_ORDEM = '000011001000'.
        free obj_main->header.
        call function 'BAPI_ALM_ORDER_GET_DETAIL' "#EC CI_USAGE_OK[2438131]
          exporting                                "#EC CI_USAGE_OK[2669857]
            number    = v_ordem
          importing
            es_header = obj_main->header
          tables
            return    = obj_main->it_return.

        data: catalog_profile type bapi10011e,
              return1         type bapireturn,
              codes           type table of  bapi10011t.

        call function 'BAPI_SERVNOT_GETCATALOGPROFIL'
          exporting
            number          = obj_main->header-notif_no
            language        = sy-langu
          importing
            catalog_profile = catalog_profile
            return          = return1
          tables
            codes           = codes.

        check obj_main->get_item( obj_main->header-notif_no ) is initial.

        obj_main->set_item( ).

*        OBJ_MAIN->alv1->refresh_table_display( is_stable = OBJ_MAIN->stable ).
        obj_main->alv2->refresh_table_display( exporting is_stable = obj_main->stable ).
        obj_main->alv3->refresh_table_display( exporting is_stable = obj_main->stable ).
        obj_main->alv4->refresh_table_display( exporting is_stable = obj_main->stable ).
        obj_main->alv5->refresh_table_display( exporting is_stable = obj_main->stable ).
        obj_main->alv6->refresh_table_display( exporting is_stable = obj_main->stable ).
      endif.
  endcase.
endform.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0400 input.
  case sy-ucomm.
    when 'EXIT'.
      clear sy-ucomm.
      leave to screen 0.
    when others.
  endcase.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0400 output.
  set pf-status 'T004'.
  set titlebar 'T005'.

*  FREE G_CUSTOM_CONTAINER_1.
* Adicionando Logo Marca no Cabeçalho
  if g_custom_container_1 is initial.

    create object g_custom_container_1
      exporting
        container_name              = 'CONTAINER_2'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    perform fill_it_fieldcatalog_1 using:
        2  'PERNR'  'TL_AFRU'  '10'   ' '  ' '  ' '  'Empregado               '  ''  ''  'AFRU '  ' ',
        1  'WERKS'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Centro                  '  ''  ''  'AFRU '  ' ',
        3  'BUDAT'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Data lançamento         '  ''  ''  'AFRU '  ' ',
        4  'ISDD '  'TL_AFRU'  '20'   ' '  ' '  ' '  'Data Inic               '  ''  ''  'AFRU '  ' ',
        5  'ISDZ '  'TL_AFRU'  '20'   ' '  ' '  ' '  'Hora Inic               '  ''  ''  'AFRU '  ' ',
        6  'IEDD '  'TL_AFRU'  '20'   ' '  ' '  ' '  'Data Fim                '  ''  ''  'AFRU '  ' ',
        7  'IEDZ '  'TL_AFRU'  '20'   ' '  ' '  ' '  'Hora Fim                '  ''  ''  'AFRU '  ' ',
        8  'AUFNR'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Ordem                   '  ''  ''  'AFRU '  ' ',
        9  'VORNR'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Operaçao                '  ''  ''  'AFRU '  ' ',
       10  'ISMNW'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Trabalho Real           '  ''  ''  'AFRU '  ' ',
       11  'ISMNE'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Unidade de trabalho     '  ''  ''  'AFRU '  ' '.


    gs_layout_1-sel_mode   = 'A'.
    gs_layout_1-cwidth_opt = 'X'.
    clear: it_exclude_fcode_1, it_exclude_fcode_1[].

    create object ctl_alv_1
      exporting
        i_parent = g_custom_container_1.

    wa_exclude_fcode_1 = cl_gui_alv_grid=>mc_fc_excl_all.
    append wa_exclude_fcode_1 to it_exclude_fcode_1.

    call method ctl_alv_1->set_table_for_first_display
      exporting
        is_layout            = gs_layout_1
        is_variant           = gs_variant_1
        it_toolbar_excluding = it_exclude_fcode_1
        i_save               = 'A'
      changing
        it_fieldcatalog      = it_fieldcatalog_1
        it_outtab            = tl_afru[]
*       IT_OUTTAB            = T_AFRU[]
        it_sort              = it_sort_1.

  else.

    ls_stable_1-row = 'X'.
    ls_stable_1-col = 'X'.

    call method ctl_alv_1->refresh_table_display
      exporting
        is_stable = ls_stable_1
      exceptions
        finished  = 1
        others    = 2.

    if sy-subrc <> 0.
    endif.

  endif.




endmodule.
*&---------------------------------------------------------------------*
*&      Form  HAB_ENC_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form hab_enc_ordem .

  data: p_respo type c.

  if v_conf_enc is not initial.
    call function 'POPUP_TO_CONFIRM'
      exporting        "TITLEBAR = 'Confirmar'
        text_question         = 'Deseja desmarcar encerramento da ordem?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        display_cancel_button = ' '
      importing
        answer                = p_respo.

    if p_respo = 1.
      clear v_conf_enc.
    endif.
  else.
    v_conf_enc = '@01@'."ICON_CHECKED.
    modify screen.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  HAB_ENC_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form hab_enc_nota .
  data: p_respo type c.
  if zaponta-aufnr is not initial.
    if v_conf_nota is not initial.
      call function 'POPUP_TO_CONFIRM'
        exporting        "TITLEBAR = 'Confirmar'
          text_question         = 'Deseja desmarcar encerramento da nota?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          display_cancel_button = ' '
        importing
          answer                = p_respo.

      if p_respo = 1.
        clear v_conf_nota.
      endif.
    else.
      loop at screen.
        case screen-name.
          when 'V_ENC_NOTA'.
*            SCREEN-ACTIVE = 0.
*        SCREEN-INPUT = 0.
            v_conf_nota = '@01@'."ICON_CHECKED.
            modify screen.
        endcase.
      endloop.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  ELIM_LINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form elim_linha.

  data: w_opera type ty_zaponta.
  data: p_resp,
  lv_msg type bapi_msg.


  clear: it_select_rows[], wa_select_rows, w_opera.

  if it_aponta is not initial.
    call method obj_main->alv1->get_selected_rows
      importing
        et_index_rows = it_select_rows.


    if it_select_rows[] is not initial.

      call function 'POPUP_TO_CONFIRM'
        exporting        "TITLEBAR = 'Confirmar'
          text_question         = 'Deseja realmente excluir a linha?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          display_cancel_button = ' '
        importing
          answer                = p_resp.

      if p_resp =< 1.

        loop at it_select_rows into wa_select_rows.
          loop at it_aponta assigning field-symbol(<faponta>).
            if sy-tabix = wa_select_rows-index.
              <faponta>-marc = 'X'.
            endif.
          endloop.
        endloop.

        delete it_aponta where marc eq 'X'.
        if sy-subrc = 0.
          message s000(o0) with 'Informação excluida com sucesso' display like 'S'.
        endif.

        obj_main->alv1->refresh_table_display( is_stable = obj_main->stable ).

      endif.

    else.
      message i026(sv)." WITH 'Selecione uma linha para excluir'.
    endif.
  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  MODIF_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form modif_screen .

  loop at screen.
    case  screen-name.
      when 'V_ENC_ORDEM'.
        if it_aponta is initial.
          screen-input = 0.
          modify screen.
        endif.

      when 'V_ENC_NOTA'.
        if obj_main->header-notif_no is initial.
          screen-input = 0.
          modify screen.
        endif.

      when 'V_ORDEM'.
        if it_aponta is not initial.
          screen-input = 0.
          modify screen.
        endif.

*      WHEN 'V_VORNR'.
*        IF it_aponta IS NOT INITIAL.
*          screen-input = 0.
*          MODIFY SCREEN.
*        ENDIF.
    endcase.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0500 input.
  case sy-ucomm.
    when 'EXIT'.
*      MESSAGE I000(O0) WITH 'Selecione uma operação com double click'.
      leave to screen 0.

    when 'OKAY'.

      call method ctl_alv_2->get_selected_rows
        importing
          et_index_rows = it_selected_rows.

      describe table it_selected_rows lines lines.

      if ( lines is initial ).
        message text-e01 type 'I' display like 'E'.

      else.

        data: wa_afvc type ty_aufk.
        data  wa_aufk type ty_aufk.

        if it_empregado is not initial.
          data: wa_empregado type ty_hrp1001.

          read table it_selected_rows into wa_selected_rows index 1.
          read table it_empregado into wa_empregado index wa_selected_rows-index.
          check sy-subrc is initial.
          zaponta-sname     = wa_empregado-sname.
          zaponta-work_cntr = wa_empregado-arbpl.
          zaponta-ktext     = wa_empregado-ktext.
          clear wa_empregado.
          leave to screen 0.
        endif.

        if it_afvc[] is not initial.

          read table it_selected_rows into wa_selected_rows index 1.
          read table it_afvc into wa_afvc index wa_selected_rows-index.
          check sy-subrc is initial.
          v_vornr = wa_afvc-vornr.
          leave to screen 0.
      endif.

    endif.
endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0500 output.
  set pf-status 'T005'.
  set titlebar 'T006'.

  if obj_main->t_aufk is not initial.
    v_aufnr = obj_main->header-orderid.

    loop at obj_main->t_aufk assigning field-symbol(<w_aufk>).
      v_ktext = <w_aufk>-ktext.
    endloop.

    free it_afvc[].
*   BUG #175277 - MMSILVA - 29.04.2025 - Inicio
    SORT obj_main->t_aufk BY vornr.
*   BUG #175277 - MMSILVA - 29.04.2025 - Fim
    move-corresponding obj_main->t_aufk to it_afvc[].

    if g_custom_container_2 is initial.

      create object g_custom_container_2
        exporting
          container_name              = 'CONTAINER_3'
        exceptions
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      perform fill_it_fieldcatalog_2 using:
          2  'VORNR'  'T_AUFK '  '05'   ' '  ' '  ' '  'Operação          '  ''  ''  'AFVC '  ' ',
          1  'VAPLZ'  'T_AUFK '  '20'   ' '  ' '  ' '  'Cen Trab          '  ''  ''  'AUFK '  ' ',
          3  'WERKS'  'T_AUFK '  '10'   ' '  ' '  ' '  'Centro            '  ''  ''  'AUFK '  ' ',
          4  'LTXA1'  'T_AUFK '  '40'   ' '  ' '  ' '  'Txt.breve operação'  ''  ''  'AFVC '  ' '.

      gs_layout_2-sel_mode   = 'A'.
      gs_layout_2-cwidth_opt = 'X'.
      clear: it_exclude_fcode_2, it_exclude_fcode_2[].

      create object ctl_alv_2
        exporting
          i_parent = g_custom_container_2.

      wa_exclude_fcode_2 = cl_gui_alv_grid=>mc_fc_excl_all.
      append wa_exclude_fcode_2 to it_exclude_fcode_2.



      call method ctl_alv_2->set_table_for_first_display
        exporting
          is_layout            = gs_layout_2
          is_variant           = gs_variant_2
          it_toolbar_excluding = it_exclude_fcode_2
          i_save               = 'A'
        changing
          it_fieldcatalog      = it_fieldcatalog_2
          it_outtab            = it_afvc[]
          it_sort              = it_sort_2.

      set handler: lcl_event_handler=>on_double_click for ctl_alv_2.

    else.

      ls_stable_2-row = 'X'.
      ls_stable_2-col = 'X'.

      call method ctl_alv_2->refresh_table_display
        exporting
          is_stable = ls_stable_2
        exceptions
          finished  = 1
          others    = 2.

      if sy-subrc <> 0.
      endif.
    endif.
  endif.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  V_ORDEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module v_ordem input.
*  V_VORNR = OBJ_MAIN->HEADER-ORDERID.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  V_KTEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module v_ktext input.
*  LOOP AT OBJ_MAIN->T_AUFK ASSIGNING FIELD-SYMBOL(<W_AUFK>).
*    V_KTEXT = <W_AUFK>-KTEXT.
*  ENDLOOP.
endmodule.
*&---------------------------------------------------------------------*
*&      Form  CHECK_REQ_AND_PED_PENDENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HEARDER_ORDERID  text
*----------------------------------------------------------------------*
form check_req_and_ped_pendente  using p_header-orderid.

  if sy-tcode = 'ZPM0047'."'IW32' OR
* AND SY-UCOMM = 'ARCH'.
    types: begin of ty_ebkn,          " Classificação contábil da requisição de compra
             banfn type ebkn-banfn,  " Nº requisição de compra
             bnfpo type ebkn-bnfpo,  " Nº do item da requisição de compra
             aufnr type ebkn-aufnr,  " Nº ordem
           end of ty_ebkn,

           begin of ty_eban,         " Requisições de compras da ordem
             banfn type eban-banfn,  " Numero da requisição
             bnfpo type eban-bnfpo,  " Item da requisiçãp
             statu type eban-statu,  " Status do item
             frgkz type eban-frgkz,  " Status da aprovação
             matnr type eban-matnr,  " Numero do material
             arsnr type eban-arsnr,  " Numero da reserva
             arsps type eban-arsps,  " Item da reserva
             ebakz type eban-ebakz,  " Requisição de compra concluída
             loekz type eban-loekz,  " Código de eliminação no documento de compras
             wepos type eban-wepos,  " Código de entrada de mercadorias
             ebeln type eban-ebeln,  " Nº pedido
             ebelp type eban-ebelp,  " Nº item do pedido
             pstyp type eban-pstyp,  " Ctg.item no documento compra
             frgst type eban-frgst,  " Estratégia de liberação na requisição de compra
             menge type eban-menge,  " Quantidade da requisição de compra
             bsmng type eban-bsmng,  " Quantidade pedida da ReqC
             ztipo type c,           " Tipo de requisição ( M - Material, S - Serviço )
           end of ty_eban,

           begin of ty_ekpo,         " Itens pedido de compra.
             ebeln  type ekpo-ebeln,  " Nº do documento de compras
             ebelp  type ekpo-ebelp,  " Nº item do documento de compra
             loekz  type ekpo-loekz,  " Código de eliminação
             elikz  type ekpo-elikz,  " Código de remessa final
             pstyp  type ekpo-pstyp,  " Ctg.item no documento compra
             packno type ekpo-packno, " Nº pacote
           end of ty_ekpo,

           begin of ty_esll,          " Linhas do pacote de serviços
             packno     type esll-packno,     " Nº pacote
             sub_packno type esll-sub_packno, " Nº do subpacote
             menge      type esll-menge,      " Qtd.com símbolo +/-
             act_menge  type esll-act_menge,  " Pedido: qtd.registrada
           end of ty_esll.



    data: t_eban      type table of ty_eban,
          t_ebkn      type table of ty_ebkn,
          t_esll      type table of ty_esll,
          t_esll_sub  type table of ty_esll,
          w_eban      type ty_eban,
          w_ebkn      type ty_ebkn,
          w_ekpo      type ty_ekpo,
          w_esll      type ty_esll,
          w_esll_sub  type ty_esll,
          l_resul     type eket-menge,
          l_len       type i,
          l_msg(100)  type c,
          l_banfn(82) type c,
          l_ebeln(82) type c.

    clear: t_req_pend, t_ped_pend.
    refresh: t_req_pend[], t_ped_pend[].

* Verifica se existem requisições vinculadas a ordem.
    select banfn bnfpo aufnr into table t_ebkn
      from ebkn
      where aufnr = p_header-orderid.

      "Se não existir requisição vinculada a ordem - SAIR.
      if sy-subrc ne 0.
        exit.
      endif.

      break fabap04.
* Verificar se existe requisição aprovada ou item eliminado.
      select banfn bnfpo statu frgkz matnr arsnr arsps ebakz loekz wepos
             ebeln ebelp pstyp frgst menge bsmng
        into corresponding fields of table t_eban
        from eban
        for all entries in t_ebkn
        where banfn eq t_ebkn-banfn
        and   bnfpo eq t_ebkn-bnfpo.

        if t_eban is initial.
          exit.     " Não existe requisição .
        endif.

        loop at t_eban into w_eban.

          if w_eban-frgst is initial.
            continue.
          endif.
* se requisição aprovada e item não eliminado
          if w_eban-frgkz eq 'X'.              " Requisição aprovada.
            continue.
          elseif w_eban-loekz is not initial.  " Item não eliminado.
            continue.
          endif.

* se Requisição pendente - EBAN-STATU – Status de processamento.
* Observar as seguintes regras:
* Para “N” – Não processado, ou “A” Sol. Cotação criada
* bloquear o encerramento técnico.
          if w_eban-statu = 'N' or         " Não processada.
             w_eban-statu = 'A'.           " Sol. cotação
            t_req_pend-banfn = w_eban-banfn.
            append t_req_pend .
            continue.
          endif.

* Para EBAN-STATU = “B” – Pedido criado verificar pedido.
          if w_eban-statu ne 'B'.          " Pedido não criado
            continue.
          endif.

* Se qtde solicitada ReqC menos Qtde pedida na ReqC diferente de zero
* enviar msg de Requisição Pendente.
          l_resul = w_eban-menge - w_eban-bsmng.

          if l_resul ne 0.
            t_req_pend-banfn = w_eban-banfn.
            append t_req_pend .
            clear l_resul.
            continue.
          endif.

* Pedido criado:
* verificar na tabela EKPO as seguintes condições:
* 1 - EKPO-LOEKZ – Código de eliminação = branco
* 2 - EKET-MENGE – Qtde. da divisão – EKET-WEMNG – Fornecido  # de 0, envia msg.
          select single ebeln ebelp loekz elikz pstyp packno into w_ekpo
            from ekpo
            where ebeln eq w_eban-ebeln
            and   ebelp eq w_eban-ebelp.

            if w_ekpo-loekz is not initial.
              continue.
            endif.

            if w_ekpo-pstyp ne '9'.

*    Se ekpo-elikz for branco.
*    envia mensagem de pedido pendente.
              if w_ekpo-elikz is initial.
                t_ped_pend-ebeln = w_ekpo-ebeln.
                append t_ped_pend .
                continue.
              endif.
            else.
              select packno sub_packno menge act_menge
                from esll
                into table t_esll
                where packno = w_ekpo-packno.
                if sy-subrc ne 0.
                  continue.
                endif.

                loop at t_esll into w_esll.
                  select packno sub_packno menge act_menge
                  from esll
                  into table t_esll_sub
                  where packno = w_esll-sub_packno.
                    if sy-subrc ne 0.
                      continue.
                    endif.
                    loop at t_esll_sub into w_esll_sub.
                      l_resul = w_esll_sub-menge - w_esll_sub-act_menge.
                      if l_resul ne 0.
                        t_ped_pend-ebeln = w_ekpo-ebeln.
                        append t_ped_pend .
                      endif.
                    endloop.
                  endloop.
                endif.
              endloop.

*  select single ebeln ebelp etenr menge wemng
*    from eket
*    into w_eket
*    where ebeln  = w_ekpo-ebeln
*    and   ebelp  = w_ekpo-ebelp.
* Se EKET-MENGE – EKET-WEMNG é diferente de zero
* envia mensagem de pedido pendente.
*    l_resul = w_eket-menge - w_eket-wemng.
*    if l_resul ne 0.
*      t_ped_pend-ebeln = w_eket-ebeln.
*      append t_ped_pend .
*      continue.
*    endif.
*  endif.
*endloop.

              if t_req_pend is not initial.
                sort t_req_pend by banfn.
                delete adjacent duplicates from t_req_pend comparing all fields.
                loop at t_req_pend.
                  concatenate t_req_pend-banfn ',*' l_banfn into l_banfn.
                endloop.
                l_len = strlen( l_banfn ).
                l_len = l_len - 2.
                translate l_banfn using '* '.
                translate l_banfn+l_len using ',.'.
                concatenate '>>> Requisições Pendentes' l_banfn into l_msg
                      separated by space.
                message l_msg type 'I'.
              endif.

              if t_ped_pend is not initial.
                sort t_ped_pend by ebeln.
                delete adjacent duplicates from t_ped_pend comparing ebeln.
                loop at t_ped_pend.
                  concatenate t_ped_pend-ebeln ',*' l_ebeln into l_ebeln.
                endloop.
                l_len = strlen( l_ebeln ).
                l_len = l_len - 2.
                translate l_ebeln using '* '.
                translate l_ebeln+l_len using ',.'.
                concatenate '>>> Pedido(s) Pendente(s)' l_ebeln into l_msg
                          separated by space.
                message l_msg type 'I'.
              endif.

*    IF T_REQ_PEND IS NOT INITIAL OR
*       T_PED_PEND IS NOT INITIAL.
*      MESSAGE I007 WITH P_HEADER-ORDERID DISPLAY LIKE 'E'.
*    ENDIF.
            endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  CHECK_BLOQ_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_bloq_ordem using v_ordem changing sy-subrc.

  data: tl_enq     type table of seqg3 with header line,
        wl_num_enq type sy-tabix,
        wl_arg     type seqg3-garg,
        coruf      type coruf,
        tco01      type tco01,
        aufnr      type seqg3-garg.

  clear sy-subrc.
  wl_arg  = |{ sy-mandt }{ v_ordem }|.
  v_ordem = |{ v_ordem alpha = in }|.
  tco01-autyp = '30'.
  coruf-aufnr = v_ordem.

  call function 'CO_RU_ORDER_LOCK'
    exporting
      aufnr_imp            = coruf-aufnr
      autyp_imp            = tco01-autyp
    exceptions
      order_already_locked = 1.

  case sy-subrc.
    when 1.
      clear sy-subrc.
      sy-subrc = 1.
      message e469(co) with coruf-aufnr sy-msgv2
                       raising order_already_locked.
  endcase.
endform.
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WHEN  text
*      -->P_OTHERS  text
*----------------------------------------------------------------------*
form f_bdc_data  using p_program p_dynpro p_start p_fnam p_fval.

  append value #(
                program   = p_program
                dynpro    = p_dynpro
                dynbegin  = p_start
                fnam      = p_fnam
                fval      = p_fval
  ) to ti_bdcdata.
endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2527   text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
form zf_call_transaction using p_trans changing p_erro.

  constants: c_msgid like it_msg-msgid value 'F5',
             c_msgnr like it_msg-msgnr value '312',
             c_msgne like it_msg-msgnr value '539'.

  data: wl_cont    type sy-tabix,
        wl_mode(1).

*  FREE IT_MSG .
  clear wl_mode.
  clear wl_cont.
  clear it_msg.
  wl_mode = 'E'.

  call transaction p_trans using ti_bdcdata
                           mode wl_mode
                           messages into it_msg.
*  COMMIT WORK.
*  WAIT UP TO 10 SECONDS.

  clear: wl_cont.

  if line_exists( it_msg[ msgtyp = 'A' ] ).
    p_erro = abap_true.
  else.
    if line_exists( it_msg[ msgtyp = 'E' ] ).
      p_erro = abap_true.
    endif.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Module  SET_CALC_HR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form set_calc_hr.

  perform check_operacao.


endform.
*&---------------------------------------------------------------------*
*&      Module  SET_CALC_HR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_calc_hr input.


endmodule.
