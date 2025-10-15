function z_zw_estrategia_executar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) TYPE  SY-UNAME
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_ESTRA STRUCTURE  ZFI_ESTRATEGIA_ZNFW
*"      T_OPERACOES STRUCTURE  ZFI_OPERACOES_ZNFW
*"----------------------------------------------------------------------
*{   INSERT         DEVK9A297X                                        1

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  data: wl_input_estra type  zfiwrt0034,
        flag_undo(1),
        wl_erro(1),
        linha_estra    type sy-tabix,
        ult_linha      type sy-tabix,
        e_row_id       type sy-tabix.

  sort t_estra by nivel aprovador.

  loop at t_estra into data(wl_estra) where aprovador = v_usuario.

    if e_row_id is not initial and sy-tabix ne e_row_id + 1.
      exit.
    elseif e_row_id is not initial and sy-tabix eq e_row_id + 1.
      if wl_estra-opcoes ne icon_system_undo.
        move icon_set_state to wl_estra-opcoes.
      endif.
      e_row_id = sy-tabix.
    else.
      e_row_id = sy-tabix.
    endif.
    "
    if wl_estra-opcoes ne icon_set_state and wl_estra-opcoes ne icon_system_undo and wl_estra-opcoes ne icon_reject .
      msg =  'Opção inválida para processamento!'.
      exit.
    endif.

    if v_usuario ne wl_estra-aprovador.
      msg =  'Usuário não é o aprovador deste nível!'.
      exit.
    endif.

    if  wl_estra-opcoes = icon_set_state.
      flag_undo = 'S'.
      linha_estra =  e_row_id.
      loop at t_estra into data(wl_estra2).
        ult_linha = sy-tabix.
        if wl_estra2-opcoes = icon_set_state and sy-tabix lt e_row_id.
          flag_undo = 'N'.
        endif.
      endloop.
      if flag_undo = 'S'.
        wl_input_estra-mandt       = sy-mandt.
        wl_input_estra-operacao    = wl_estra-operacao.
        wl_input_estra-dep_resp    = wl_estra-dep_resp.
        wl_input_estra-nivel       = wl_estra-nivel.
        wl_input_estra-aprovador   = wl_estra-aprovador.

        wl_input_estra-data_atual  = sy-datum.
        wl_input_estra-hora_atual  = sy-uzeit.
        wl_input_estra-usuario     = sy-uname.
        modify zfiwrt0034 from wl_input_estra.
        clear wl_input_estra.
        if ult_linha = linha_estra.
          "Aprova mesmo com erro
          update zfiwrt0001
          set status_aprov = 'A'
           where operacao = wl_estra-operacao.
          commit work.

          loop at t_estra into wl_estra.
            wl_estra-opcoes = ' ' .
            wl_estra-estado = icon_checked .
            modify t_estra from wl_estra index sy-tabix transporting opcoes estado.
          endloop.
          msg = 'Processamento concluído com sucesso'.
        else.
          wl_estra-opcoes = icon_system_undo .
          wl_estra-estado = icon_checked .
          modify t_estra from wl_estra index e_row_id.

          msg = 'Processamento concluído com sucesso'.
        endif.

      else.
        msg = 'Devem ser aprovadas as estratégias anteriores'.
      endif.
    elseif  wl_estra-opcoes = icon_system_undo .
      flag_undo = 'S'.
      linha_estra =  e_row_id.
      loop at t_estra into wl_estra2.
        if wl_estra2-opcoes = icon_system_undo and sy-tabix gt e_row_id
          and wl_estra2-aprovador ne wl_estra-aprovador.                        "/Modificação CS2016000820/
          flag_undo = 'N'.
        endif.
        if wl_estra2-opcoes = icon_message_critical.
          msg = 'Operação totalmente liberada'.
          flag_undo = 'N'.
          exit.
        endif.
      endloop.
      if flag_undo = 'S'.
        delete  from zfiwrt0034
          where operacao   = wl_estra-operacao
          and   nivel      = wl_estra-nivel
          and   aprovador  = wl_estra-aprovador.
        commit work.
        wl_estra-estado = icon_led_yellow  .
        wl_estra-opcoes = icon_set_state.
        modify t_estra from wl_estra index e_row_id.
      else.
        msg = 'Devem ser reiniciadas as estratégias posteriores'.
      endif.
    elseif wl_estra-opcoes = icon_reject.
      update zfiwrt0001 set status_aprov  = 'R' where operacao = wl_estra-operacao.
      commit work.
    endif.

  endloop.

*
endfunction.
