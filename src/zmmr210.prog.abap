*&---------------------------------------------------------------------*
*& Report ZMMR210
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zmmr210.

data: p_name       type usnam,
      p_sp01       type checkbox,
      p_sp02       type checkbox,
      p_sp03       type checkbox,
      p_sp04       type checkbox,
      p_sp05       type checkbox,
      p_sp06       type checkbox,
      p_sp07       type checkbox,
      p_sp08       type checkbox,
      p_sp09       type checkbox,
      p_sp10       type checkbox,
      p_sp11       type checkbox,
      p_sp12       type checkbox,
      p_sp13       type checkbox,
      p_sp14       type checkbox,
      p_sp15       type checkbox,
      p_sp16       type checkbox,
      p_sp17       type checkbox,
      p_sp18       type checkbox,
      p_sp19       type checkbox,
      p_sp20       type checkbox,
      p_sp21       type checkbox,
      p_sp22       type checkbox,
      p_sp23       type checkbox,
      p_sp24       type checkbox,
      p_sp25       type checkbox,
      p_sp26       type checkbox,
      p_sp27       type checkbox,
      p_sp28       type checkbox,
      p_sp29       type checkbox,
      p_sp30       type checkbox,
      p_sp31       type checkbox,
      p_sp32       type checkbox,
      p_sp33       type checkbox,
      p_sp34       type checkbox,
      p_sp35       type checkbox,
      it_zmmt0212  type table of zmmt0212,
      wa_zmmt0212  type zmmt0212,
      lv_fieldname type string,
      lv_index     type c length 2,
      resposta     type c.

field-symbols: <fs_checkbox> type abap_bool.

initialization.
  call screen 100.


form trata_dados.
  if p_name is initial.
    message 'Preencher com o nome do usuário.' type 'S' display like 'E'.
    exit.
  endif.

  clear: wa_zmmt0212, it_zmmt0212.

  "Verifica se o usuário SAP existe
  select single bname
    from usr02
    into @data(wl_user_exists_usr02)
    where bname eq @p_name.
  if sy-subrc is not initial.
    message 'Usuário informado não existe.' type 'S' display like 'E'.
    exit.
  endif.

  "Pop-up de confirmação
  clear resposta.
  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar              = 'Confirmar'
      text_question         = 'Deseja confirmar a alteração?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '2'
      display_cancel_button = ''
    importing
      answer                = resposta
    exceptions
      text_not_found        = 1
      others                = 2.
  if resposta = '2'.
    exit.
  else.
    "Deleta na tabela ZMMT0212 caso o usuário SAP já tenha algum acesso
    select single user_sap
      from zmmt0212
      into @data(wl_user_exists_zmmt0212)
      where user_sap eq @p_name.
    if sy-subrc is initial.
      delete from zmmt0212 where user_sap eq p_name.
    endif.

    "Realiza o preenchimento da tabela IT_ZMMT0212 caso tenha algum checkbox marcado.
    do 35 times.
      lv_index = cond string(
                     when sy-index < 10 then |0{ sy-index }|
                     else |{ sy-index }|
                   ).

      lv_fieldname = |P_SP{ lv_index }|.

      assign (lv_fieldname) to <fs_checkbox>.
      if sy-subrc is initial and <fs_checkbox> = abap_true.
        wa_zmmt0212-user_sap   = p_name.
        wa_zmmt0212-aba        = lv_fieldname+2.
        wa_zmmt0212-data_atual = sy-datum.
        wa_zmmt0212-hora_atual = sy-uzeit.
        wa_zmmt0212-usuario    = sy-uname.
        append wa_zmmt0212 to it_zmmt0212.
      endif.
    enddo.

    "Insere na tabela transparente
    if it_zmmt0212 is not initial.
      insert zmmt0212 from table it_zmmt0212.
      commit work.

      message 'Modificado com sucesso.' type 'S'.
    endif.
  endif.


endform.

*&---------------------------------------------------------------------*
*& Module STATUS_100 OUTPUT
*&---------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'PF0100'.
  set titlebar 'TB0100'.

*  PERFORM f_init_alv.
endmodule.


*&---------------------------------------------------------------------*
*&  Module  USER_COMMAND_100  INPUT
*&---------------------------------------------------------------------*
module user_command_0100 input.
  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.

    when 'EXIT' or 'CANCEL'.
      leave program.

    when 'SAVE'.
      perform trata_dados.

    when 'ATUAL'.
      perform exibe_dados.

  endcase.
endmodule.
*&---------------------------------------------------------------------*
*& Form EXIBE_DADOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form exibe_dados .
  select *
   from zmmt0212
   into table @data(tb_user_exists_zmmt0212)
   where user_sap eq @p_name.


  do 35 times.
    lv_index = cond string(
                   when sy-index < 10 then |0{ sy-index }|
                   else |{ sy-index }|
                 ).

    lv_fieldname = |P_SP{ lv_index }|.
    assign (lv_fieldname) to <fs_checkbox>.
    if  (
        lv_index = 3 or
        lv_index = 8 or
         lv_index = 21 or
        lv_index = 22 ) and tb_user_exists_zmmt0212[] is INITIAL.
      if <fs_checkbox> is assigned.
        <fs_checkbox> = 'X'.
      endif.
    else.
      if <fs_checkbox> is assigned.
        clear <fs_checkbox>.
      endif.
    endif.

  enddo.

  "Realiza o preenchimento da tabela IT_ZMMT0212 caso tenha algum checkbox marcado.
  loop at tb_user_exists_zmmt0212 into data(wa_user_exists_zmmt0212).
    lv_fieldname = |P_{ wa_user_exists_zmmt0212-aba }|.

    assign (lv_fieldname) to <fs_checkbox>.
    if <fs_checkbox> is assigned.
      <fs_checkbox> = abap_true.
    endif.
  endloop.

endform.
