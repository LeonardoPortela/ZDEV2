function z_iv_estrategia_executar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) TYPE  SY-UNAME
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"     VALUE(OK) TYPE  CHAR01
*"  TABLES
*"      T_LOTES STRUCTURE  ZIV_LOTES_IMP
*"      T_ESTRA STRUCTURE  ZFI_ESTRATEGIA_IMP
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  type-pools: icon.

  types: begin of ty_estra ,
           bukrs     type zimp_lotes_aprov-bukrs,
           lote      type zimp_lotes_aprov-lote,
           valor_de  type zimp_aprovador-valor_de,
           valor_ate type zimp_aprovador-valor_ate,
           aprovador type zimp_aprovador-aprovador,
           nivel     type zimp_aprovador-nivel,
           estado(4),
           opcoes(4),
         end of ty_estra,

         begin of ty_cadlote,
           empresa(30) type c,
           lote(50)    type c,
           usuario(20) type c,
           total       type zimp_lanc_imp_ct-valor_imp,
           dep_resp(2),
           data(10),
         end of ty_cadlote.

  data: tg_estra   type table of ty_estra,
        wg_cadlote type ty_cadlote.

  data: wl_estra       like line of tg_estra,
        wl_estra2      like line of tg_estra,
        w_estra        type          zfi_estrategia_imp,
        wl_input_estra type zinv_lotes_aprov,
        flag_undo(1),
        linha_estra    type sy-tabix,
        ult_linha      type sy-tabix,
        e_row_id       type sy-tabix.

  ok = abap_false.

  loop at t_estra.
    move-corresponding t_estra to wl_estra.
    append wl_estra to tg_estra.
  endloop.

  sort tg_estra by nivel aprovador.
  sort t_estra by nivel aprovador.

  loop at tg_estra into wl_estra where aprovador = v_usuario.                 "/Modificação CS2016000820/
    "READ TABLE TG_ESTRA INTO WL_ESTRA WITH KEY  APROVADOR = V_USUARIO.       "/Modificação CS2016000820/
    if sy-subrc = 0.
      if e_row_id is not initial and sy-tabix ne e_row_id + 1.                "/Modificação CS2016000820/
        exit.                                                                 "/Modificação CS2016000820/
      elseif e_row_id is not initial and sy-tabix eq e_row_id + 1.            "/Modificação CS2016000820/
        if wl_estra-opcoes ne icon_system_undo.                               "/Modificação CS2016000820/
          move icon_set_state to wl_estra-opcoes.                             "/Modificação CS2016000820/
        endif.
        e_row_id = sy-tabix.                                                  "/Modificação CS2016000820/
      else.
        e_row_id = sy-tabix.
      endif.
      read table t_lotes index 1.
      wg_cadlote-empresa  = t_lotes-empresa.
      concatenate  t_lotes-lote '-'  into wg_cadlote-lote.
      wg_cadlote-usuario  = v_usuario.
      wg_cadlote-total    = t_lotes-total.
      wg_cadlote-data     = t_lotes-dt_venc.
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
        loop at tg_estra into wl_estra2.
          ult_linha = sy-tabix.
          if wl_estra2-opcoes = icon_set_state and sy-tabix lt e_row_id.
            flag_undo = 'N'.
          endif.
        endloop.
        if wl_estra2-valor_ate lt t_lotes-total.
          msg =  'Falta nível para aprovação total!- Ver Compliance'.
          exit.
        endif.

        if flag_undo = 'S'.
          wl_input_estra-mandt       = sy-mandt.
          wl_input_estra-bukrs       = wl_estra-bukrs.
          wl_input_estra-lote        = wl_estra-lote.
          wl_input_estra-nivel       = wl_estra-nivel.
          wl_input_estra-aprovador   = wl_estra-aprovador.
          wl_input_estra-valor_de    = wl_estra-valor_de.
          wl_input_estra-valor_ate   = wl_estra-valor_ate.
          wl_input_estra-data_atual  = sy-datum.
          wl_input_estra-hora_atual  = sy-uzeit.
          wl_input_estra-usuario     = sy-uname.
          modify zinv_lotes_aprov from wl_input_estra.
          clear wl_input_estra.
          if ult_linha = linha_estra.
            update zfit0036 set status = 'L'
             where lote = wl_estra-lote
             and   status = 'A'.
            commit work.
            "MSG =  'Última liberação, lote liberado'..
            loop at tg_estra into wl_estra.
              wl_estra-opcoes = ' ' .
              wl_estra-estado = icon_checked .
              modify tg_estra from wl_estra index sy-tabix transporting opcoes estado.
            endloop.
            msg = 'Processamento concluído com sucesso'.
            ok = abap_true.
          else.
            wl_estra-opcoes = icon_system_undo .
            wl_estra-estado = icon_checked .
            modify tg_estra from wl_estra index e_row_id.
            perform envia_email(zfir0027) tables tg_estra using wg_cadlote e_row_id .
            msg = 'Processamento concluído com sucesso'.
            ok = abap_true.
          endif.

        else.
          msg = 'Devem ser aprovadas as estratégias anteriores'.
        endif.
      elseif  wl_estra-opcoes = icon_system_undo .
        flag_undo = 'S'.
        linha_estra =  e_row_id.
        loop at tg_estra into wl_estra2.
          if wl_estra2-opcoes = icon_system_undo and sy-tabix gt e_row_id
            and wl_estra2-aprovador ne wl_estra-aprovador.                        "/Modificação CS2016000820/
            flag_undo = 'N'.
          endif.
          if wl_estra2-opcoes = icon_message_critical.
            msg = 'Lote totalmente liberado'.
            flag_undo = 'N'.
            exit.
          endif.
        endloop.
        if flag_undo = 'S'.
          delete  from zinv_lotes_aprov
             where bukrs      = wl_estra-bukrs
             and   lote       = wl_estra-lote
             and   nivel      = wl_estra-nivel
             and   aprovador  = wl_estra-aprovador.
          wl_estra-estado = icon_led_yellow  .
          wl_estra-opcoes = icon_set_state.
          modify tg_estra from wl_estra index e_row_id.
        else.
          msg = 'Devem ser reiniciadas as estratégias posteriores'.
        endif.
      elseif wl_estra-opcoes = icon_reject.
        update zfit0036 set status = 'L'
          where lote = wl_estra-lote
          and   status = 'A'.
        commit work.
        msg = 'Processamento concluído com sucesso'.
        ok = abap_true.
      endif.
      loop at tg_estra into wl_estra.
        if e_row_id = sy-tabix.
          move-corresponding wl_estra to w_estra.
          modify t_estra from w_estra index sy-tabix transporting opcoes estado.
          exit.
        endif.
      endloop.
    endif.
  endloop.

endfunction.
