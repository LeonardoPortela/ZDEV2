*&---------------------------------------------------------------------*
*& Report  ZRD_zpmt0075_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zrd_zpmt0075_exit.


form f_exit_zpmt0075_0001 using p_registro_manter type any.


  data: wl_zpmt0075 type zpmt0075.

  clear: wl_zpmt0075.

  move-corresponding p_registro_manter to wl_zpmt0075.

  if wl_zpmt0075-us_criacao is initial.
    wl_zpmt0075-dt_criacao      = sy-datum.
    wl_zpmt0075-hr_criacao      = sy-uzeit.
    wl_zpmt0075-us_criacao      = sy-uname.
  endif.



  move-corresponding wl_zpmt0075 to p_registro_manter.

endform.

form f_exit_zpmt0075_0002 using p_registro_manter type any
                       changing p_error.

  data: wl_zpmt0075 type zpmt0075.

  clear: wl_zpmt0075.

  move-corresponding p_registro_manter to wl_zpmt0075.

  if wl_zpmt0075-typbz is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha a tipo de veiculo!'.
    exit.
  else.
    select single * from zpmr0001 into @data(ls_zpmr0001) where typbz eq @wl_zpmt0075-typbz.
    if sy-subrc ne 0.
      message i024(sd) with 'Tipo de veiculo não existe !'.
      p_error = abap_true.
      exit.
    endif.
  endif.

  if wl_zpmt0075-mptyp is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha a Ctg.plano de manutenção!'.
    exit.
  else.
    select single * from t399w_t into @data(ls_t399w_t) where mptyp eq @wl_zpmt0075-mptyp.
    if sy-subrc ne 0.
      message i024(sd) with 'Ctg.plano de manutenção invalida !'.
      p_error = abap_true.
      exit.
    endif.
  endif.
  clear: ls_t399w_t.



  if wl_zpmt0075-wptxt is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o Texto do plano de manutenção!'.
    exit.
  endif.

  if wl_zpmt0075-zykl1 is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o ciclo!'.
    exit.
  endif.

  if wl_zpmt0075-zeieh is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha a unidade do ciclo!'.
    exit.
  else.
    select single * from t006a into @data(ls_t006a) where msehi eq @wl_zpmt0075-zeieh and spras eq @sy-langu.
    if sy-subrc ne 0.
      message i024(sd) with 'unidade do ciclo invalida !'.
      p_error = abap_true.
      exit.
    endif.
  endif.
  clear: ls_t006a.

  if wl_zpmt0075-pak_text is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o texto para ciclo!'.
    exit.
  endif.

  if wl_zpmt0075-locas is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o conjunto!'.
    exit.
  else.
    select single * from mara into @data(ls_mara) where matnr eq @wl_zpmt0075-locas.
    if sy-subrc ne 0.
      message i024(sd) with 'Conjunto invalido!'.
      p_error = abap_true.
      exit.
    endif.

    "Verificar se esta cadastrado os parametro ponto de medição.
    select single * from zpmt0074 into @data(ls_zpmt0074)
    where locas eq @wl_zpmt0075-locas
      and typbz eq @wl_zpmt0075-typbz.
    if sy-subrc ne 0.
      p_error = abap_true.
      message i024(sd) with |Não existe ponto de medição|
                            |{ wl_zpmt0075-locas } para|
                            |sistema na transação (ZPM0093)|.
      exit.
    endif.
  endif.
  clear: ls_mara.

  if wl_zpmt0075-auart is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o tipo de ordem!'.
    exit.
  else.
    select single * from t003o into @data(ls_t003o) where auart eq @wl_zpmt0075-auart.
    if sy-subrc ne 0.
      message i024(sd) with 'Tipo de ordem invalida!'.
      p_error = abap_true.
      exit.
    endif.
  endif.
  clear: ls_t003o.

  if wl_zpmt0075-ilart is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o tipo de atividade!'.
    exit.
  else.
    select single * from t353i into @data(ls_t353i) where ilart eq @wl_zpmt0075-ilart.
    if sy-subrc ne 0.
      message i024(sd) with 'Tipo de atividade invalida !'.
      p_error = abap_true.
      exit.
    endif.
  endif.
  clear: ls_t353i.

  if wl_zpmt0075-gewerk is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o CenTrab respon.!'.
    exit.
  else.
    select single * from m_cramv into @data(ls_m_cramv) where arbpl eq @wl_zpmt0075-gewerk.
    if sy-subrc ne 0.
      message i024(sd) with 'CenTrab respon. invalida !'.
      p_error = abap_true.
      exit.
    endif.
  endif.
  clear: ls_m_cramv.


  if wl_zpmt0075-wpgrp is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o Grp.plnj.PM!'.
    exit.
  else.
    select single * from t024i into @data(ls_t024i) where ingrp eq @wl_zpmt0075-wpgrp.
    if sy-subrc ne 0.
      message i024(sd) with 'Grp.plnj.PM invalida !'.
      p_error = abap_true.
      exit.
    endif.
  endif.
  clear: ls_t024i.

  if wl_zpmt0075-priok is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha a Prioridade!'.
    exit.
  else.
    select single * from t356 into @data(ls_t356) where priok eq @wl_zpmt0075-priok.
    if sy-subrc ne 0.
      message i024(sd) with 'Prioridade invalida !'.
      p_error = abap_true.
      exit.
    endif.
  endif.
  clear: ls_t356.

  if wl_zpmt0075-plnty is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o tipo de roteiro!'.
    exit.
  endif.

  if wl_zpmt0075-plnnr is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o GrpLisTarefa.!'.
    exit.
  else.
    "Verifica se a lista é diferente do conjunto.
    if wl_zpmt0075-plnnr ne wl_zpmt0075-locas.
      p_error = abap_true.
      message i024(sd) with 'Lista de tarefas é diferente do conjunto!'.
      exit.
    else.
      "Validar se esta cadastrada.
      select single plnal from plko into @data(ws_plko) where plnnr eq @wl_zpmt0075-plnnr.
      if sy-subrc ne 0.
        p_error = abap_true.
        message i024(sd) with 'Lista de tarefas não cadastrada!'.
        exit.
      endif.
    endif.
  endif.
  clear: ws_plko.


  if wl_zpmt0075-vspos is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o fator deslocamento na confirmação atrasada!'.
    exit.
  endif.

  if wl_zpmt0075-topos is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha a tolerância no caso de confirmação atrasada (%)!'.
    exit.
  endif.

  if wl_zpmt0075-vsneg is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha o fator deslocamento na confirm.antecipada!'.
    exit.
  endif.

  if wl_zpmt0075-toneg is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha a tolerância no caso de confirmação antecipada (%)!'.
    exit.
  endif.

  if wl_zpmt0075-toneg is initial.
    p_error = abap_true.
    message i024(sd) with 'Preencha a tolerância no caso de confirmação antecipada (%)!'.
    exit.
  endif.



  if wl_zpmt0075-wptxt is not initial.
    translate wl_zpmt0075-wptxt to upper case.
  endif.
*
  if wl_zpmt0075-zykl1 is not initial.
    translate wl_zpmt0075-zykl1 to upper case.
  endif.
*
  if wl_zpmt0075-pak_text is not initial.
    translate wl_zpmt0075-pak_text to upper case.
  endif.


  move-corresponding wl_zpmt0075 to p_registro_manter.

endform.


form f_exit_zpmt0075_0005 changing p_saida type any.

  data: wl_zpmt0075 type zpmt0075.

  clear: wl_zpmt0075.

  move-corresponding p_saida to wl_zpmt0075.

  if wl_zpmt0075-us_criacao is initial.
    wl_zpmt0075-dt_criacao      = sy-datum.
    wl_zpmt0075-hr_criacao      = sy-uzeit.
    wl_zpmt0075-us_criacao      = sy-uname.
  else.
    wl_zpmt0075-dt_modif      = sy-datum.
    wl_zpmt0075-hr_modif      = sy-uzeit.
    wl_zpmt0075-us_modif      = sy-uname.
  endif.

  move-corresponding wl_zpmt0075 to p_saida.


endform.

form f_exit_zpmt0074_0004 using p_registro_manter type any.

*
*  DATA: wl_zpmt0072 TYPE zpmt0073.
*
*  CLEAR: wl_zpmt0072.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0072.
*
*  IF wl_zpmt0072-us_criacao IS INITIAL.
*    wl_zpmt0072-dt_criacao      = sy-datum.
*    wl_zpmt0072-hr_criacao      = sy-uzeit.
*    wl_zpmt0072-us_criacao      = sy-uname.
*  ELSE.
*    wl_zpmt0072-dt_modif      = sy-datum.
*    wl_zpmt0072-hr_modif      = sy-uzeit.
*    wl_zpmt0072-us_modif      = sy-uname.
*  ENDIF.
*
*
*
*  MOVE-CORRESPONDING wl_zpmt0072 TO p_registro_manter.

endform.

form  f_exit_zpmt0075_0016 using p_ucomm  type sy-ucomm changing p_registro_manter type any p_saida type any.


  data: wl_zpmt0075 type zpmt0075.
*
  clear: wl_zpmt0075.
*
  move-corresponding p_registro_manter to wl_zpmt0075.


  if wl_zpmt0075-wptxt is not initial.
    translate wl_zpmt0075-wptxt to upper case.
  endif.
*
  if wl_zpmt0075-zykl1 is not initial.
    translate wl_zpmt0075-zykl1 to upper case.
  endif.
*
  if wl_zpmt0075-pak_text is not initial.
    translate wl_zpmt0075-pak_text to upper case.
  endif.

  move-corresponding wl_zpmt0075 to p_registro_manter.


endform.
