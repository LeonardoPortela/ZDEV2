function z_ad_estrategia_executar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(V_USUARIO) LIKE  SY-UNAME
*"  EXPORTING
*"     REFERENCE(MSG) TYPE  CHAR50
*"     REFERENCE(OK) TYPE  CHAR01
*"  TABLES
*"      T_LOTES STRUCTURE  ZAD_LOTES_IMP
*"      T_ESTRA STRUCTURE  ZFI_ESTRATEGIA_IMP
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  type-pools: icon.


  types: begin of ty_estra ,
           bukrs     type zadt_sol_aprov-bukrs,
           nro_sol   type zadt_sol_aprov-nro_sol,
           valor_de  type zadto_aprovador-valor_de,
           valor_ate type zadto_aprovador-valor_ate,
           aprovador type zadto_aprovador-aprovador,
           nivel     type zadto_aprovador-nivel,
           estado(4),
           opcoes(4),
         end of ty_estra,

         begin of ty_cadlote,
           empresa(30) type c,
           nro_sol     type zfit0045-nro_sol,
           usuario(20) type c,
           total(30), "       TYPE ZFIT0046-VLR_ADIANTAMENTO,
           dep_resp(2),
           data(10),
         end of ty_cadlote.

  data: tg_estra    type table of ty_estra,
        wg_cadlote  type ty_cadlote,
        wa_zfit0045 type zfit0045,
        w_bsart     type ekko-bsart.


  data: wl_estra         like line of tg_estra,
        wl_estra2        like line of tg_estra,
        w_estra          type          zfi_estrategia_imp,
        wl_input_estra   type  zadt_sol_aprov,
        flag_undo(1),
        wl_erro(1),
        wg_documento(10),
        linha_estra      type sy-tabix,
        ult_linha        type sy-tabix,
        e_row_id         type sy-tabix,
        vdata(10).

  ok  = abap_false.

  loop at t_estra.
    move-corresponding t_estra to wl_estra.
    move t_estra-lote to wl_estra-nro_sol.
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
      " Bug 186300 28.07.25
      move t_lotes-dt_venc to vdata.
      if strlen( vdata ) = 10.
        concatenate vdata+6(4) vdata+3(2) vdata+0(2) into t_lotes-dt_venc.
      endif.

      wg_cadlote-empresa  = t_lotes-empresa.
      concatenate  t_lotes-nro_sol '-'  into wg_cadlote-nro_sol.
      wg_cadlote-usuario  = v_usuario .
      wg_cadlote-total    = t_lotes-total.
      wg_cadlote-dep_resp = t_lotes-dep_resp+0(2).
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
        if flag_undo = 'S'.
          wl_input_estra-mandt       = sy-mandt.
          wl_input_estra-bukrs       = wl_estra-bukrs.
          wl_input_estra-nro_sol     = wl_estra-nro_sol.
          wl_input_estra-nivel       = wl_estra-nivel.
          wl_input_estra-aprovador   = wl_estra-aprovador.
          wl_input_estra-valor_de    = wl_estra-valor_de.
          wl_input_estra-valor_ate   = wl_estra-valor_ate.
          wl_input_estra-data_atual  = sy-datum.
          wl_input_estra-hora_atual  = sy-uzeit.
          wl_input_estra-usuario     = sy-uname.
          modify zadt_sol_aprov from wl_input_estra.
          clear wl_input_estra.
          if ult_linha = linha_estra.
            "Aprova mesmo com erro
            update zfit0045
            set status = 'A'
                dt_pgto = t_lotes-dt_venc "User Story 144133 / aoenning.
             where nro_sol =  wl_estra-nro_sol.
            commit work.
            "Aprovacao 14.01.2016
            clear wl_erro.
            select single ekko~bsart
              from zfit0045
              inner join ekko on ekko~ebeln = zfit0045~ebeln
              into w_bsart
             where nro_sol = wl_estra-nro_sol.
*            IF NOT ( 'PCEF_PSEF_YCEF_YSEF_ZEFI_ZGEF_ZEF' CS w_bsart ). "Entrega futura não gera doc contabil
*              PERFORM f_shdb(zfir0032) USING wa_zfit0045-nro_sol CHANGING wl_erro.
            perform f_shdb(zfir0032) using wl_estra-nro_sol changing wl_erro.
*            ENDIF.
            if wl_erro ne 'X' .
              "MSG =  'Última liberação, lote liberado'..
              loop at tg_estra into wl_estra.
                wl_estra-opcoes = ' ' .
                wl_estra-estado = icon_checked .
                modify tg_estra from wl_estra index sy-tabix transporting opcoes estado.
              endloop.
              msg = 'Processamento concluído com sucesso'.
              ok  = abap_true.
            else.
              msg = 'Processamento concluído com Erro'.
            endif.

          else.
            wl_estra-opcoes = icon_system_undo .
            wl_estra-estado = icon_checked .
            modify tg_estra from wl_estra index e_row_id.
            perform envia_email(zfir0034) tables tg_estra using wg_cadlote e_row_id .
            msg = 'Processamento concluído com sucesso'.
            ok  = abap_true.
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
            msg = 'Solicitação totalmente liberada'.
            flag_undo = 'N'.
            exit.
          endif.
        endloop.
        if flag_undo = 'S'.
          delete  from zadt_sol_aprov
            where bukrs      = wl_estra-bukrs
            and   nro_sol    = wl_estra-nro_sol
            and   nivel      = wl_estra-nivel
            and   aprovador  = wl_estra-aprovador.
          wl_estra-estado = icon_led_yellow  .
          wl_estra-opcoes = icon_set_state.
          modify tg_estra from wl_estra index e_row_id.
        else.
          msg = 'Devem ser reiniciadas as estratégias posteriores'.
        endif.
      elseif wl_estra-opcoes = icon_reject.
        update zfit0045 set status = 'R' where nro_sol = wl_estra-nro_sol.
        commit work.
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
