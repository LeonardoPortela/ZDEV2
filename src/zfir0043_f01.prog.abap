*----------------------------------------------------------------------*
***INCLUDE ZFIR0043_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ELIMINAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form eliminar_data .

  "Mercado Financeiro – Projeção BM&F
  delete from zfit0060 where  dt_fechamento = wg_cadpro-dt_fechamento.

  "Financeiro interpolação
  delete from zfit0062 where bukrs         = wg_cadpro-bukrs
                       and   dt_fechamento = wg_cadpro-dt_fechamento.

  "MTM - Financeiro Calculo NDF
  delete from zfit0064 where bukrs         = wg_cadpro-bukrs
                       and   dt_fechamento = wg_cadpro-dt_fechamento.

  "MTM – Financeiro Calculo SWAP
  delete from zfit0067 where bukrs         = wg_cadpro-bukrs
                       and   dt_fechamento = wg_cadpro-dt_fechamento.

  "MTM – Financeiro – SWAP VANILA
  delete from zfit0069 where bukrs         = wg_cadpro-bukrs
                       and   dt_fechamento = wg_cadpro-dt_fechamento.

  "MTM – Financeiro Calculo SWAP Com CDI
  delete from zfit0070 where bukrs         = wg_cadpro-bukrs
                       and   dt_fechamento = wg_cadpro-dt_fechamento.


  message s836(sd) with 'Cáculo excluído!'.

endform.                    " ELIMINAR_DATA
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_dados .
  data: tl_zfit0060 type table of zfit0060 with header line,
        tl_zfit0062 type table of zfit0062 with header line,
        wl_zfit0061 type zfit0061.


  clear:  xcod_oper_nav, xcalculo_ctb, xadd.
  if wg_cadpro-bukrs is not initial.
    select single *
      from t001
      into wl_t001
      where bukrs = wg_cadpro-bukrs.

    if sy-subrc = 0.
      wg_cadpro-butxt = wl_t001-butxt.
    else.
      clear wg_cadpro-butxt.
    endif.
  else.
    clear wg_cadpro-butxt.
  endif.

  if wg_cadpro-cod_oper is not initial.
    select single *
    from zfit0063
    into wl_zfit0063
    where cod_oper = wg_cadpro-cod_oper.

    if sy-subrc = 0.
      wg_cadpro-descr_oper = wl_zfit0063-descr.
    else.
      clear wg_cadpro-descr_oper.
    endif.
  else.
    clear wg_cadpro-descr_oper.
  endif.


  if wg_cadpro-bukrs is not initial
    and wg_cadpro-dt_fechamento is not initial
    and ( wg_acao eq c_displa or g_tab_tela-subtela   = '0210' ).

    refresh: it_zfit0060,it_zfit0062.

    select *
      from zfit0060
      into table tl_zfit0060
      where dt_fechamento = wg_cadpro-dt_fechamento.

    refresh: tg_itens.
    loop at tl_zfit0060.
      move-corresponding tl_zfit0060 to tg_itens.
      append tg_itens.
      clear: tg_itens.
      move-corresponding tl_zfit0060 to wa_zfit0060.
      append wa_zfit0060 to it_zfit0060.
      xadd = 'X'.
    endloop.

    select single *
      from zfit0061
      into wl_zfit0061
      where dt_fechamento = wg_cadpro-dt_fechamento
      and   cod_oper      = wg_cadpro-cod_oper
      and   tipo = '1'.

    refresh: tg_editor.
    clear: wl_cont_aux2, wl_cont_aux, wl_cont.
    wl_cont = strlen( wl_zfit0061-observacao ).
    wl_cont_aux = wl_cont / 72.

    do.
      move: wl_zfit0061-observacao+wl_cont_aux2 to wg_editor-line.
      add 72 to wl_cont_aux2.
      append wg_editor to tg_editor.

      if wl_cont_aux2 gt wl_cont.
        exit.
      endif.
    enddo.

    " Exibe descrição primeira Tela
    call method obg_descbox->set_text_as_r3table
      exporting
        table = tg_editor.
    call method obg_descbox->set_readonly_mode
      exporting
        readonly_mode = 0.

    " Interpolação
    select *
      from zfit0062
      into table tl_zfit0062
      where bukrs         = wg_cadpro-bukrs
      and   dt_fechamento = wg_cadpro-dt_fechamento.

    refresh: tg_itens2.
    loop at tl_zfit0062.
      move-corresponding tl_zfit0062 to tg_itens2.
      append tg_itens2.
      clear: tg_itens2.
      move-corresponding tl_zfit0062 to wa_zfit0062.
      append wa_zfit0062 to it_zfit0062.
      xadd = 'X'.
    endloop.

    select single *
         from zfit0061
         into wl_zfit0061
         where bukrs         = wg_cadpro-bukrs
         and   dt_fechamento = wg_cadpro-dt_fechamento
         and   cod_oper      = wg_cadpro-cod_oper
         and   tipo = '2'.

    refresh: tg_editor1.
    clear: wl_cont_aux2, wl_cont_aux, wl_cont.
    wl_cont = strlen( wl_zfit0061-observacao ).
    wl_cont_aux = wl_cont / 72.

    do.
      move: wl_zfit0061-observacao+wl_cont_aux2 to wg_editor-line.
      add 72 to wl_cont_aux2.
      append wg_editor to tg_editor1.

      if wl_cont_aux2 gt wl_cont.
        exit.

      endif.
    enddo.


    select single *
      from zfit0061
      into wl_zfit0061
      where dt_fechamento = wg_cadpro-dt_fechamento
      and   cod_oper      in ('S','N')
      and   tipo = '3'.

    refresh: tg_editor2.
    clear: wl_cont_aux2, wl_cont_aux, wl_cont.
    wl_cont = strlen( wl_zfit0061-observacao ).
    wl_cont_aux = wl_cont / 72.

    do.
      move: wl_zfit0061-observacao+wl_cont_aux2 to wg_editor-line.
      add 72 to wl_cont_aux2.
      append wg_editor to tg_editor2.

      if wl_cont_aux2 gt wl_cont.
        exit.

      endif.
    enddo.

    perform f_busca_contabil.

    if wg_acao ne c_add.
      wg_acao = c_displa.
    endif.

    refresh: tg_fields.

    perform trata_campos using space
                              'GR1'
                               c_0       "INPUT 1     NO INPUT 0
                               c_0.      "INVISIBLE 1 VISIBLE 0
  endif.



endform.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form verifica_erros .
  data: wl_linha(6)  ,
        wl_itens    like line of tg_itens,
        v_qtde      type i value 0.
  refresh: tg_msg_ret.
  clear: tg_msg_ret.

  " Calcula Ultimo dia do mês.
  data: data_ini type sy-datum,
        data_pro type sy-datum,
        vmes(2),
        vano(4),
        nmes     type i,
        nano     type i.

  if g_tab_tela-subtela = '0211'.
    if wg_cadpro-bukrs is initial .
      move: text-e01                  to tg_msg_ret-msg,
            'WG_CADPRO-BUKRS'         to tg_msg_ret-field.
      concatenate  tg_msg_ret-msg 'Empresa' into tg_msg_ret-msg separated by space.
      append tg_msg_ret.
      clear: tg_msg_ret.
    else.

    endif.
  endif.

  if wg_cadpro-dt_fechamento is initial .
    move: text-e01                  to tg_msg_ret-msg,
          'WG_CADPRO-DT_FECHAMENTO' to tg_msg_ret-field.
    concatenate  tg_msg_ret-msg 'Data Interpolação' into tg_msg_ret-msg separated by space.
    append tg_msg_ret.
    clear: tg_msg_ret.
  else.
    nmes =  wg_cadpro-dt_fechamento+4(2).
    nano =  wg_cadpro-dt_fechamento+0(4).
    if nmes = '12'.
      nmes = '01'.
      nano = nano + 1.
    else.
      add 1 to nmes.
    endif.
    vmes = nmes.
    vano = nano.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = vmes
      importing
        output = vmes.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = vano
      importing
        output = vano.

    concatenate vano vmes '01' into data_ini.

    data_pro = data_ini.

    subtract 1 from data_ini.

    call function 'Z_RET_DATA_MES_ABERTO'
      exporting
        p_data_ent  = data_ini
        p_bukrs     = wg_cadpro-bukrs
      importing
        p_data_val  = p_data_val
      exceptions
        sem_periodo = 1
        others      = 2.

*    IF NOT SY-SUBRC IS INITIAL.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
*      EXPORTING
*        P_DATA_ENT     = DATA_INI
*        P_BUKRS        = WG_CADPRO-BUKRS
*        P_VAL_FI       = 'X'
*        P_VAL_MM       = 'X'
*      IMPORTING
*        P_DATA_VAL     = P_DATA_VAL
*      EXCEPTIONS
*        DATA_FI_MM_NAO = 1
*        OTHERS         = 2.
**    IF SY-SUBRC <> 0.
**      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**    ENDIF.

    if data_ini+0(6) ne p_data_val+0(6).
      move: text-e04                  to tg_msg_ret-msg,
                 'WG_CADPRO-DT_FECHAMENTO'         to tg_msg_ret-field.
      concatenate  tg_msg_ret-msg data_ini+4(2) '/' data_ini+0(4) into tg_msg_ret-msg separated by space.
      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.

    call function 'Z_RET_DATA_MES_ABERTO'
      exporting
        p_data_ent  = data_pro
        p_bukrs     = wg_cadpro-bukrs
      importing
        p_data_val  = p_data_val
      exceptions
        sem_periodo = 1
        others      = 2.
*    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
*      EXPORTING
*        P_DATA_ENT     = DATA_PRO
*        P_BUKRS        = WG_CADPRO-BUKRS
*        P_VAL_FI       = 'X'
*        P_VAL_MM       = 'X'
*      IMPORTING
*        P_DATA_VAL     = P_DATA_VAL
*      EXCEPTIONS
*        DATA_FI_MM_NAO = 1
*        OTHERS         = 2.
**    IF SY-SUBRC <> 0.
**      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**    ENDIF.

    if data_pro+0(6) ne p_data_val+0(6).
      move: text-e04                  to tg_msg_ret-msg,
                 'WG_CADPRO-DT_FECHAMENTO'         to tg_msg_ret-field.
      concatenate  tg_msg_ret-msg data_pro+4(2) '/' data_pro+0(4) into tg_msg_ret-msg separated by space.
      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.

  endif.

  if g_tab_tela-subtela = '0210'.
    clear v_qtde.
    loop at tg_itens into wl_itens.
      wl_linha = sy-tabix.
      add 1 to v_qtde.

      if wl_itens-seq is initial.
        move:  '' to tg_msg_ret-aba.
        concatenate text-e02 'Seq.    LINHA: ' wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.

      if wl_itens-dias_corridos is initial.
        move:  '' to tg_msg_ret-aba.
        concatenate text-e02 'Dias Corridos LINHA: ' wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.

      if wl_itens-tx_proj_ano_ban is initial or wl_itens-tx_proj_ano_ban le 0.
        move:  '' to tg_msg_ret-aba.
        concatenate text-e02 'Tx.  Bancária LINHA: ' wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.

      if wl_itens-tx_proj_ano_com is initial or wl_itens-tx_proj_ano_com le 0.
        move:  '' to tg_msg_ret-aba.
        concatenate text-e02 'Tx. Comercial LINHA: ' wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.

      if wl_itens-tx_par_dolar is initial or wl_itens-tx_par_dolar le 0.
        move:  '' to tg_msg_ret-aba.
        concatenate text-e02 'Tx. Paridade LINHA: ' wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.

      if wl_itens-dt_base_bmf is initial .
        move:  '' to tg_msg_ret-aba.
        concatenate text-e02 'Dt. Base – dias Corridos  LINHA: ' wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.

    endloop.

    if v_qtde eq 0.
      move: 'Informe a Sequência'   to tg_msg_ret-msg,
          'WG_CADPRO-DT_FECHAMENTO' to tg_msg_ret-field.
      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.
  elseif g_tab_tela-subtela = '0211'.
    if it_zfit0062[] is  initial.
      move: 'Cálculo não realizado'   to tg_msg_ret-msg,
           'WG_CADPRO-DT_FECHAMENTO' to tg_msg_ret-field.
      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.

  endif.

*  IF 'H_V' CS WG_CADPRO-COD_OPER AND WG_CADPRO-PTAX IS INITIAL.
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*          'WG_CADPRO-PTAX' TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'PTAX' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

endform.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grava_dados .

  data: tl_input_zfit0060 type table of zfit0060 with header line,
        wl_input_zfit0061 type zfit0061.


  "Observação BMF
  loop at tg_editor into wg_editor.
    if sy-tabix eq 1.
      wl_input_zfit0061-observacao = wg_editor-line.

    elseif sy-tabix ge 2.
      concatenate wl_input_zfit0061-observacao  wg_editor-line into wl_input_zfit0061-observacao.

    endif.
  endloop.
  move: sy-mandt                 to wl_input_zfit0061-mandt,
        wg_cadpro-dt_fechamento  to wl_input_zfit0061-dt_fechamento,
        wg_cadpro-cod_oper       to wl_input_zfit0061-cod_oper,
        '1'                      to wl_input_zfit0061-tipo.
  modify zfit0061 from       wl_input_zfit0061.

  "Observação Interpolação
  loop at tg_editor1 into wg_editor.
    if sy-tabix eq 1.
      wl_input_zfit0061-observacao = wg_editor-line.

    elseif sy-tabix ge 2.
      concatenate wl_input_zfit0061-observacao  wg_editor-line into wl_input_zfit0061-observacao.

    endif.
  endloop.

  move: sy-mandt                 to wl_input_zfit0061-mandt,
        wg_cadpro-bukrs          to wl_input_zfit0061-bukrs,
        wg_cadpro-dt_fechamento  to wl_input_zfit0061-dt_fechamento,
        wg_cadpro-cod_oper       to wl_input_zfit0061-cod_oper,
        '2'                      to wl_input_zfit0061-tipo.
  modify zfit0061 from       wl_input_zfit0061.

  "Observação NDF / SWAP S
  loop at tg_editor2 into wg_editor.
    if sy-tabix eq 1.
      wl_input_zfit0061-observacao = wg_editor-line.

    elseif sy-tabix ge 2.
      concatenate wl_input_zfit0061-observacao  wg_editor-line into wl_input_zfit0061-observacao.

    endif.
  endloop.

  move: sy-mandt                 to wl_input_zfit0061-mandt,
        wg_cadpro-dt_fechamento  to wl_input_zfit0061-dt_fechamento,
        wg_cadpro-cod_oper       to wl_input_zfit0061-cod_oper,
        '3'                      to wl_input_zfit0061-tipo.
  modify zfit0061 from       wl_input_zfit0061.

  "Grava BMF
  delete tg_itens where  seq is initial.
  loop at tg_itens.
    move: sy-mandt                 to tl_input_zfit0060-mandt,
          wg_cadpro-dt_fechamento  to tl_input_zfit0060-dt_fechamento,
          tg_itens-seq             to tl_input_zfit0060-seq,
          tg_itens-dias_corridos   to tl_input_zfit0060-dias_corridos,
          tg_itens-tx_proj_ano_ban to tl_input_zfit0060-tx_proj_ano_ban,
          tg_itens-tx_proj_ano_com to tl_input_zfit0060-tx_proj_ano_com,
          tg_itens-tx_par_dolar    to tl_input_zfit0060-tx_par_dolar,
          tg_itens-dt_base_bmf     to tl_input_zfit0060-dt_base_bmf,
          tg_itens-dias_uteis      to tl_input_zfit0060-dias_uteis,
          tg_itens-tx_cupom_camb   to tl_input_zfit0060-tx_cupom_camb,
          sy-uname                 to tl_input_zfit0060-usnam,
          sy-datum                 to tl_input_zfit0060-data_atual,
          sy-uzeit                 to tl_input_zfit0060-hora_atual.
    append tl_input_zfit0060.
  endloop.
  "
  delete from zfit0060 where dt_fechamento = wg_cadpro-dt_fechamento.

  modify zfit0060 from table tl_input_zfit0060.
  modify zfit0061 from       wl_input_zfit0061.

  " grava calculo 1 - Financeiro interpolação
  delete from zfit0062 where bukrs         eq wg_cadpro-bukrs
                       and   dt_fechamento eq wg_cadpro-dt_fechamento.
  modify zfit0062 from table it_zfit0062.
  clear wa_zfit0062.
  refresh it_zfit0062.

  clear xadd.

  message s836(sd) with 'Cálculo'
                       ', criado/modificado com sucesso!'.


endform.                    " GRAVA_DADOS

*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD      text
*      -->P_GROUP1     text
*      -->P_VALUE      text
*      -->P_INVISIBLE  text
*----------------------------------------------------------------------*
form trata_campos  using    p_field
                            p_group1
                            p_value
                            p_invisible.

  tg_fields-campo     = p_field.
  tg_fields-group1    = p_group1.
  tg_fields-value     = p_value.
  tg_fields-invisible = p_invisible.
  append tg_fields.

endform.                    " TRATA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form limpa_campos .
  clear: wg_mensagem,x_field,wa_zfit0062.
  refresh: tg_itens,tg_itens2,tg_itens3,tg_itens4,tg_itens5,tg_itens6,tg_itens7,tg_editor,tg_editor1,tg_editor2,
           it_zfit0060,it_zfit0062, it_zfit0064,it_zfit0065,it_zfit0066,it_zfit0067,it_zfit0069,it_zfit0070.
endform.                    " LIMPA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  CALC_dias_uteis
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form calc_dias_uteis using dodia atedia changing v_dias_uteis.
  data : hol_id(2)  type c  value 'ZT',        " holiday calender id
         fact_id(2) type c  value 'ZT',        " factory calender id
         fromdate   type sydatum ,   " from date
         enddate    type sydatum,    " end date
         v_days     type i.


  data: begin of holiday occurs 0.
          include structure iscal_day.
data end of holiday.

  data: v_yrfrm(4)  type n,              "year valid from
        v_yrvlto(4) type n.             "year valid to


  fromdate   = dodia.
  enddate    = atedia.
  call function 'HOLIDAY_GET'
    exporting
      holiday_calendar           = hol_id
      factory_calendar           = fact_id
      date_from                  = fromdate
      date_to                    = enddate
    importing
      year_of_valid_from         = v_yrfrm
      year_of_valid_to           = v_yrvlto
*     RETURNCODE                 =
    tables
      holidays                   = holiday
    exceptions
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      date_has_invalid_format    = 3
      date_inconsistency         = 4
      others                     = 5.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  endif.

  describe table holiday lines v_dias_uteis.


  v_days       = enddate - fromdate.
  v_dias_uteis = v_days - v_dias_uteis.

  "SUBTRACT 1 FROM V_DIAS_UTEIS.


endform.                    "CALC_dias_uteis

*&---------------------------------------------------------------------*
*&      Form  F_CALCULO_SWAP_V
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_calculo_swap_h. "Swap Vanila
  data: vtx_proj_ano_bana type zfit0060-tx_proj_ano_ban,
        vtx_proj_ano_band type zfit0060-tx_proj_ano_ban,
        vtx_cupom_camba   type zfit0060-tx_cupom_camb,
        vtx_cupom_cambd   type zfit0060-tx_cupom_camb,
        vtx_com_interp2   type ztx_bpm,
        vtx_com_interp3   type ztx_bpm,
        vseqa             type zfit0062-seq,
        vseqd             type zfit0062-seq.

  loop at it_zfit0069 into wa_zfit0069.
    if wa_zfit0069-obj_key is not initial.
      vobj_key = wa_zfit0069-obj_key.
    endif.
  endloop.
  " Verifica se já foi contabilizado, neste caso não contabiliza este calculo - fazer estorno antes
  select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

  if sy-subrc eq 0.
    exit.
  endif.

  refresh it_zfit0069.

  select *
     from zfit0059
     into table it_zfit0059
     where mdo_tipo  = 'H'
     and   data_vencimento gt  wg_cadpro-dt_fechamento
     and   data_realizacao le  wg_cadpro-dt_fechamento
     and   pfj_codigo      eq wg_cadpro-bukrs
     and   regra           ne ''
     and   valor_operacao  ne 0.

*  SELECT *
*    FROM zfit0059
*    INTO TABLE it_zfit0059
*    WHERE mdo_tipo  = 'H'
*    AND   data_vencimento GT  wg_cadpro-dt_fechamento
*    AND   data_realizacao LE  wg_cadpro-dt_fechamento
*    AND   pfj_codigo      EQ wg_cadpro-bukrs.

  loop at it_zfit0059 into wa_zfit0059.

    "US 150369 operações FREE_SCHED não tem restrição porque vira do XRT como CTF ao inves de VANILA
    translate wa_zfit0059-mdo_codigo to upper case.
*    if  wa_zfit0059-mdo_codigo eq 'FREE SCHED'.
*       AND wa_zfit0059-opr_numero NE '00039711'
*       AND wa_zfit0059-opr_numero NE '00034333'
*       AND wa_zfit0059-opr_numero NE '00036410'
*       AND wa_zfit0059-opr_numero NE '00036412'
*       AND wa_zfit0059-opr_numero NE '00036530'
*       AND wa_zfit0059-opr_numero NE '00037608'
*       AND wa_zfit0059-opr_numero NE '00040612'.
*      continue.
*    endif.
    "US 150369 operações FREE_SCHED não tem restrição porque vira do XRT como CTF ao inves de VANILA


    delete it_zfit0059 where posicao = 'XX'.
    wa_zfit0069-dt_fechamento    = wg_cadpro-dt_fechamento.
    wa_zfit0069-bukrs            = wa_zfit0059-pfj_codigo.
    wa_zfit0069-nro_cto          = wa_zfit0059-opr_numero.
    wa_zfit0069-cod_oper         = wa_zfit0059-mdo_tipo.
    wa_zfit0069-banco            = wa_zfit0059-pfj_agente.
    wa_zfit0069-dt_inicio_cto	   = wa_zfit0059-data_realizacao.
    wa_zfit0069-dt_fim_cto       = wa_zfit0059-data_vencimento.
    wa_zfit0069-tx_data_base     = wg_cadpro-ptax.
    wa_zfit0069-vlr_operacao     = wa_zfit0059-valor_operacao.
    wa_zfit0069-vlr_operacao_int  = wa_zfit0059-valor_operacao * wa_zfit0059-tx_dolar_in_op.
    wa_zfit0069-tx_cambio_in_op	 = wa_zfit0059-tx_dolar_in_op.
    wa_zfit0069-index_ativo      = wa_zfit0059-moeda.
    wa_zfit0069-tx_index_passivo = wa_zfit0059-index_passivo.
    wa_zfit0069-tx_al_index_ativ = wa_zfit0059-tx_ind_ativo.
    wa_zfit0069-tx_al_index_pass = wa_zfit0059-tx_ind_passivo.

    if wa_zfit0069-tx_al_index_pass = 0.
      wa_zfit0069-tx_index_ativo   = 100.
      wa_zfit0069-index_passivo    = 'CDI'.
      wa_zfit0069-tx_al_index_pass =  0.
      wa_zfit0069-tp_taxa_ativo    = '2'.
      wa_zfit0069-tp_taxa_passivo	 = '1'.
    else.
      wa_zfit0069-index_passivo    = 'PRE'.
    endif.

    wa_zfit0069-dias_corridos_01 = wa_zfit0069-dt_fim_cto - wa_zfit0069-dt_inicio_cto.
    perform calc_dias_uteis using wa_zfit0069-dt_inicio_cto wa_zfit0069-dt_fim_cto changing v_dias_uteis.
    wa_zfit0069-dias_uteis_01 = v_dias_uteis.

    wa_zfit0069-dias_corridos_02  = wg_cadpro-dt_fechamento - wa_zfit0069-dt_inicio_cto.
    if wa_zfit0069-dias_corridos_02 gt 0.
      perform calc_dias_uteis using wa_zfit0069-dt_inicio_cto wg_cadpro-dt_fechamento changing v_dias_uteis.
    else.
      perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0069-dt_inicio_cto changing v_dias_uteis.
      v_dias_uteis = v_dias_uteis * -1.
    endif.
    wa_zfit0069-dias_uteis_02 = v_dias_uteis.

    wa_zfit0069-dias_corridos_03  = wa_zfit0069-dt_fim_cto - wg_cadpro-dt_fechamento.
    perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0069-dt_fim_cto  changing v_dias_uteis.
    wa_zfit0069-dias_uteis_03 = v_dias_uteis.

    wa_zfit0069-tx_comp_dolar_at = wa_zfit0069-tx_data_base /  wa_zfit0069-tx_cambio_in_op.

    wa_zfit0069-ft_atual_ativa   = ( ( ( wa_zfit0069-tx_al_index_ativ / 100 ) / 360 ) * wa_zfit0069-dias_corridos_02 ) + 1.

    wa_zfit0069-vlr_curva_ativa  = wa_zfit0069-vlr_operacao_int * wa_zfit0069-tx_comp_dolar_at * wa_zfit0069-ft_atual_ativa.

    perform f_calculo_ft_acum using wa_zfit0069-dt_inicio_cto wa_zfit0069-tx_index_passivo   changing v_ft_acum_indev.

    wa_zfit0069-ft_acum_indev = v_ft_acum_indev.

    wa_zfit0069-ft_multp_passiva = ( 1 + ( wa_zfit0069-tx_al_index_pass / 100 ) ) ** ( wa_zfit0069-dias_uteis_02 / 252 ).

    wa_zfit0069-vlr_curva_passiv = wa_zfit0069-vlr_operacao_int * wa_zfit0069-ft_acum_indev * wa_zfit0069-ft_multp_passiva.

    if wa_zfit0069-dt_inicio_cto ne ''.
      read table it_zfit0062 into wa_zfit0062
       with key bukrs         = wg_cadpro-bukrs
                dt_fechamento = wg_cadpro-dt_fechamento
                dt_vcto_cto   = wa_zfit0069-dt_fim_cto.
      if sy-subrc = 0.
        wa_zfit0069-tx_proj_interp   = wa_zfit0062-tx_cb_interp.
        wa_zfit0069-tx_inter_cdi_ati = wa_zfit0062-tx_com_interp.
        wa_zfit0069-proj_cdi_inter   = wa_zfit0062-tx_bco_interp.
        wa_zfit0069-tx_inter_cdi_pas = wa_zfit0062-tx_com_interp.
      endif.
    endif.

    if wa_zfit0069-tx_al_index_pass = 0. "CDI
      wa_zfit0069-tx_inter_dolar_a = wa_zfit0069-tx_proj_interp / wa_zfit0069-tx_cambio_in_op.
      wa_zfit0069-ft_multp_ativa = ( ( wa_zfit0069-tx_al_index_ativ / 100 ) / 360 * wa_zfit0069-dias_corridos_01 ) + 1.
      wa_zfit0069-vlr_fut_ativa = wa_zfit0069-vlr_operacao_int * wa_zfit0069-tx_inter_dolar_a * wa_zfit0069-ft_multp_ativa.
      wa_zfit0069-vlr_mtm_p_ativ = wa_zfit0069-vlr_fut_ativa / ( 1 + ( wa_zfit0069-tx_inter_cdi_ati / 100 ) ). "Por CDI
    else.
      "3.	INDEXADOR EM PRÉ BRL
      clear: vtx_com_interp2,vtx_com_interp3 .
      if wa_zfit0069-dt_inicio_cto ne ''.
        read table it_zfit0062 into wa_zfit0062
         with key bukrs         = wg_cadpro-bukrs
                  dt_fechamento = wg_cadpro-dt_fechamento
                  dt_vcto_cto   = wa_zfit0069-dt_fim_cto.
        if sy-subrc = 0.
          vseqa = wa_zfit0062-seq - 1.
          vseqd = wa_zfit0062-seq + 1.
          "
          clear wa_zfit0060.
          read table it_zfit0060 into wa_zfit0060 with key seq = vseqa.
          vtx_proj_ano_bana = wa_zfit0060-tx_proj_ano_ban.

          read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062-seq.
          vtx_cupom_camba   = wa_zfit0060-tx_cupom_camb.

          "
          clear wa_zfit0060.
          read table it_zfit0060 into wa_zfit0060 with key seq = vseqd.
          vtx_proj_ano_band = wa_zfit0060-tx_proj_ano_ban.
          vtx_cupom_cambd   = wa_zfit0060-tx_cupom_camb.
          "
          if vtx_proj_ano_bana = vtx_proj_ano_band.
            vtx_com_interp2 = vtx_proj_ano_band.
          else.
            subtract 1 from  wa_zfit0062-dias_u_pri_inter.
            vtx_com_interp2 = vtx_proj_ano_bana + ( ( wa_zfit0062-dias_u_mtm_cto - wa_zfit0062-dias_u_pri_inter ) / ( ( wa_zfit0062-dias_u_seq_inter - wa_zfit0062-dias_u_pri_inter ) / ( vtx_proj_ano_band - vtx_proj_ano_bana ) ) ) .
          endif.

          if ( vtx_cupom_cambd gt 0 and vtx_cupom_camba  gt 0 ) and ( vtx_cupom_cambd ne vtx_cupom_camba ). "ALRS 26.04.2023
            vtx_com_interp3 = vtx_cupom_camba + ( ( wa_zfit0062-dias_c_mtm_cto - wa_zfit0062-dias_c_pri_inter ) /  ( ( wa_zfit0062-dias_c_seq_inter - wa_zfit0062-dias_c_pri_inter )  / ( vtx_cupom_cambd - vtx_cupom_camba ) ) ).
          else.
            vtx_com_interp3 = 0.
          endif.


        endif.
      endif.
      if vtx_com_interp2 = 0.
        read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062-seq.
        vtx_com_interp2  = wa_zfit0060-tx_proj_ano_ban.
      endif.

      wa_zfit0069-vlr_mtm_p_ativ = wa_zfit0069-vlr_operacao_int * ( 1 + ( wa_zfit0069-tx_al_index_pass / 100 ) ) ** ( wa_zfit0069-dias_uteis_01 / 252 ) /
                                   ( 1 +  ( vtx_com_interp2 / 100 ) ) ** ( wa_zfit0069-dias_uteis_03 / 252 ) . "novo calculo - Taxa pré
    endif.

    if wa_zfit0069-tx_al_index_pass = 0. "CDI
      "1.	INDEXADOR % DE CDI
      wa_zfit0069-ind_proj_final = ( 1 + ( ( wa_zfit0069-proj_cdi_inter * wa_zfit0069-tx_index_passivo ) / 10000 ) ).

      wa_zfit0069-ftr_multp_passiv = 1 + ( wa_zfit0069-tx_al_index_pass / 100 ) ** ( wa_zfit0069-dias_uteis_03 / 252 ).

      wa_zfit0069-vlr_fut_passiva = wa_zfit0069-vlr_curva_passiv * wa_zfit0069-ind_proj_final * wa_zfit0069-ftr_multp_passiv.

      wa_zfit0069-vlr_mtm_p_passiv  = wa_zfit0069-vlr_fut_passiva / ( 1 + ( wa_zfit0069-tx_inter_cdi_pas / 100 ) ).
    else.
      "2.	INDEXADOR EM PRÉ USD + VARIAÇÃO CAMBIAL USDBRL
      if vtx_com_interp3 = 0.
        read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062-seq.
        vtx_com_interp3  = wa_zfit0060-tx_cupom_camb.
      endif.
      wa_zfit0069-vlr_mtm_p_passiv = ( wa_zfit0059-valor_operacao * wa_zfit0069-tx_data_base ) *
                                     ( 1 +  ( ( wa_zfit0069-tx_al_index_ativ / 100 )  * wa_zfit0069-dias_corridos_01 / 360  ) )
                                     / ( 1 + ( vtx_com_interp3 / 100 )  *  wa_zfit0069-dias_corridos_03 / 360 ). "novo calculo - Taxa pré
    endif.

    wa_zfit0069-vlr_aj_merc	      = ( wa_zfit0069-vlr_mtm_p_ativ - wa_zfit0069-vlr_mtm_p_passiv ).

    if wa_zfit0069-vlr_aj_merc > 0.
      wa_zfit0069-tp_ajuste = 'ATIVO'.
    else.
      wa_zfit0069-tp_ajuste = 'PASSIVO'.
    endif.

*** PBI - 73761 - Inicio - CBRAND
    if wa_zfit0069-dias_corridos_03  > 365 and wa_zfit0069-vlr_aj_merc gt 0 .
      wa_zfit0069-tp_ajuste = 'ATIVOLP'.
    else.
      if  wa_zfit0069-dias_corridos_03 > 365 and wa_zfit0069-vlr_aj_merc  le 0 .
        wa_zfit0069-tp_ajuste = 'PASSIVOLP'.
      endif.
    endif.
*** PBI - 73761 - Fim - CBRAND


    move:
    sy-uname                 to wa_zfit0069-usnam,
    sy-datum                 to wa_zfit0069-data_atual,
    sy-uzeit                 to wa_zfit0069-hora_atual.
    append wa_zfit0069 to it_zfit0069.
    clear wa_zfit0069.

  endloop.

  refresh tg_itens6.
  loop at it_zfit0069 into wa_zfit0069.
    move-corresponding wa_zfit0069 to tg_itens6.
    tg_itens6-ano = tg_itens6-dt_fim_cto+0(4).
    append tg_itens6.
  endloop.

  if it_zfit0069[] is initial.
    message s000(zwrm001) display like 'E' with 'Não há dados SWAP Vanila!'.
  else.
    message s836(sd) with 'Cálculo SWAP Vanila'
                       ',Realizado com sucesso!'.
  endif.

endform.                    "F_CALCULO_SWAP_H

form f_calculo_swap_h_novo. "Swap Vanila
  data: vtx_proj_ano_bana  type zfit0060-tx_proj_ano_ban,
        vtx_proj_ano_band  type zfit0060-tx_proj_ano_ban,
        vtx_cupom_camba    type zfit0060-tx_cupom_camb,
        vtx_cupom_cambd    type zfit0060-tx_cupom_camb,
        vtx_com_interp2    type ztx_bpm,
        vtx_com_interp3    type ztx_bpm,
        vseqa              type zfit0062-seq,
        vseqd              type zfit0062-seq,
        vponta_ativa(10),
        vponta_passiva(10),
        vg_ftr_multp_ativa type zfator_desc,
        zlv_offset         type i,
        vg_vlr_aj_merc     type zfit0069-vlr_aj_merc.


  loop at it_zfit0069 into wa_zfit0069.
    if wa_zfit0069-obj_key is not initial.
      vobj_key = wa_zfit0069-obj_key.
    endif.
  endloop.
  " Verifica se já foi contabilizado, neste caso não contabiliza este calculo - fazer estorno antes
  select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

  if sy-subrc eq 0.
    exit.
  endif.

  refresh it_zfit0069.

  select *
    from zfit0059
    into table it_zfit0059
    where mdo_tipo  = 'H'
    and   data_vencimento gt  wg_cadpro-dt_fechamento
    and   data_realizacao le  wg_cadpro-dt_fechamento
    and   pfj_codigo      eq wg_cadpro-bukrs
    and   regra           ne ''
    and   valor_operacao  ne 0.

  loop at it_zfit0059 into wa_zfit0059.
    "US 150369 operações FREE_SCHED não tem restrição porque vira do XRT como CTF ao inves de VANILA
    translate wa_zfit0059-mdo_codigo to upper case.

    delete it_zfit0059 where posicao = 'XX'.
    wa_zfit0069-dt_fechamento    = wg_cadpro-dt_fechamento.
    wa_zfit0069-bukrs            = wa_zfit0059-pfj_codigo.
    wa_zfit0069-nro_cto          = wa_zfit0059-opr_numero.
    wa_zfit0069-cod_oper         = wa_zfit0059-mdo_tipo.
    wa_zfit0069-banco            = wa_zfit0059-pfj_agente.
    wa_zfit0069-dt_inicio_cto	   = wa_zfit0059-data_realizacao.
    wa_zfit0069-dt_fim_cto       = wa_zfit0059-data_vencimento.
    wa_zfit0069-tx_data_base     = wg_cadpro-ptax.
    wa_zfit0069-vlr_operacao     = wa_zfit0059-valor_operacao.
    wa_zfit0069-vlr_operacao_int  = wa_zfit0059-valor_operacao * wa_zfit0059-tx_dolar_in_op.
    wa_zfit0069-tx_cambio_in_op	 = wa_zfit0059-tx_dolar_in_op.

    wa_zfit0069-index_ativo      = wa_zfit0059-forma_taxa_ativo.
    wa_zfit0069-index_passivo    = wa_zfit0059-forma_taxa_passivo.

    wa_zfit0069-tx_index_ativo   = wa_zfit0059-index_ativo.
    wa_zfit0069-tx_index_passivo = wa_zfit0059-index_passivo.

    wa_zfit0069-tx_al_index_ativ = wa_zfit0059-tx_ind_ativo.
    wa_zfit0069-tx_al_index_pass = wa_zfit0059-tx_ind_passivo.


    clear : vponta_ativa,vponta_passiva.
    if wa_zfit0059-ind_codigo_ativo = 'IPCA_15_M-2'.
      vponta_ativa = 'IPCA'.
      wa_zfit0069-index_ativo      = 'IPCA'.
    elseif wa_zfit0059-ind_codigo_ativo = 'USD_VENDA'.
      vponta_ativa = 'PRE_USD+VC'.
      wa_zfit0069-index_ativo      = 'USD'.
    elseif  wa_zfit0059-ind_codigo_ativo = 'CDI'.
      vponta_ativa = 'CDI'.
      wa_zfit0069-index_ativo      = 'CDI'.
    else.
      vponta_ativa = 'PRE_BRL'.
      wa_zfit0069-index_ativo      = 'BRL'.
    endif.

    if wa_zfit0059-ind_codigo_passivo = 'USD_VENDA'.
      vponta_passiva = 'PRE_USD+VC'.
      wa_zfit0069-index_passivo      = 'USD'.
    elseif  wa_zfit0059-ind_codigo_passivo = 'CDI'.
      vponta_passiva = 'CDI'.
      wa_zfit0069-index_passivo      = 'CDI'.
    else.
      vponta_passiva = 'PRE_BRL'.
      wa_zfit0069-index_passivo      = 'BRL'.
    endif.


    wa_zfit0069-dias_corridos_01 = wa_zfit0069-dt_fim_cto - wa_zfit0069-dt_inicio_cto.
    perform calc_dias_uteis using wa_zfit0069-dt_inicio_cto wa_zfit0069-dt_fim_cto changing v_dias_uteis.
    wa_zfit0069-dias_uteis_01 = v_dias_uteis.

    wa_zfit0069-dias_corridos_02  = wg_cadpro-dt_fechamento - wa_zfit0069-dt_inicio_cto.
    if wa_zfit0069-dias_corridos_02 gt 0.
      perform calc_dias_uteis using wa_zfit0069-dt_inicio_cto wg_cadpro-dt_fechamento changing v_dias_uteis.
    else.
      perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0069-dt_inicio_cto changing v_dias_uteis.
      v_dias_uteis = v_dias_uteis * -1.
    endif.
    wa_zfit0069-dias_uteis_02 = v_dias_uteis.

    wa_zfit0069-dias_corridos_03  = wa_zfit0069-dt_fim_cto - wg_cadpro-dt_fechamento.
    perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0069-dt_fim_cto  changing v_dias_uteis.
    wa_zfit0069-dias_uteis_03 = v_dias_uteis.


    if vponta_passiva = 'CDI'.
      perform f_calculo_ft_acum using wa_zfit0069-dt_inicio_cto wa_zfit0069-tx_index_passivo   changing v_ft_acum_indev.
    elseif  vponta_ativa = 'CDI'.
      perform f_calculo_ft_acum using wa_zfit0069-dt_inicio_cto wa_zfit0069-tx_index_ativo   changing v_ft_acum_indev.
    endif.

    wa_zfit0069-ft_acum_indev = v_ft_acum_indev.

    if wa_zfit0069-dt_inicio_cto ne ''.
      read table it_zfit0062 into wa_zfit0062
       with key bukrs         = wg_cadpro-bukrs
                dt_fechamento = wg_cadpro-dt_fechamento
                dt_vcto_cto   = wa_zfit0069-dt_fim_cto.
      if sy-subrc = 0.
        wa_zfit0069-tx_proj_interp   = wa_zfit0062-tx_cb_interp.
        wa_zfit0069-tx_inter_cdi_ati = wa_zfit0062-tx_com_interp.
        wa_zfit0069-proj_cdi_inter   = wa_zfit0062-tx_bco_interp.
        wa_zfit0069-tx_inter_cdi_pas = wa_zfit0062-tx_com_interp.
      endif.
    endif.

*    WA_ZFIT0069-TX_COMP_DOLAR_AT = WA_ZFIT0069-TX_DATA_BASE /  WA_ZFIT0069-TX_CAMBIO_IN_OP.
*    WA_ZFIT0069-FT_ATUAL_ATIVA   = ( ( ( WA_ZFIT0069-TX_AL_INDEX_ATIV / 100 ) / 360 ) * WA_ZFIT0069-DIAS_CORRIDOS_02 ) + 1.
*    WA_ZFIT0069-VLR_CURVA_ATIVA  = WA_ZFIT0069-VLR_OPERACAO_INT * WA_ZFIT0069-TX_COMP_DOLAR_AT * WA_ZFIT0069-FT_ATUAL_ATIVA.
*    WA_ZFIT0069-TX_INTER_DOLAR_A = WA_ZFIT0069-TX_PROJ_INTERP / WA_ZFIT0069-TX_CAMBIO_IN_OP.
*    WA_ZFIT0069-FT_MULTP_ATIVA = ( ( WA_ZFIT0069-TX_AL_INDEX_ATIV / 100 ) / 360 * WA_ZFIT0069-DIAS_CORRIDOS_01 ) + 1.
*    WA_ZFIT0069-VLR_FUT_ATIVA = WA_ZFIT0069-VLR_OPERACAO_INT * WA_ZFIT0069-TX_INTER_DOLAR_A * WA_ZFIT0069-FT_MULTP_ATIVA.
*    WA_ZFIT0069-VLR_MTM_P_ATIV = WA_ZFIT0069-VLR_FUT_ATIVA / ( 1 + ( WA_ZFIT0069-TX_INTER_CDI_ATI / 100 ) ).

    clear: vtx_com_interp2,vtx_com_interp3 .
    if wa_zfit0069-dt_inicio_cto ne ''.
      read table it_zfit0062 into wa_zfit0062
       with key bukrs         = wg_cadpro-bukrs
                dt_fechamento = wg_cadpro-dt_fechamento
                dt_vcto_cto   = wa_zfit0069-dt_fim_cto.
      if sy-subrc = 0.
        vseqa = wa_zfit0062-seq - 1.
        vseqd = wa_zfit0062-seq + 1.
        "
        clear wa_zfit0060.
        read table it_zfit0060 into wa_zfit0060 with key seq = vseqa.
        vtx_proj_ano_bana = wa_zfit0060-tx_proj_ano_ban.

        read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062-seq.
        vtx_cupom_camba   = wa_zfit0060-tx_cupom_camb.

        "
        clear wa_zfit0060.
        read table it_zfit0060 into wa_zfit0060 with key seq = vseqd.
        vtx_proj_ano_band = wa_zfit0060-tx_proj_ano_ban.
        vtx_cupom_cambd   = wa_zfit0060-tx_cupom_camb.
        "
        if vtx_proj_ano_bana = vtx_proj_ano_band.
          vtx_com_interp2 = vtx_proj_ano_band.
        else.
          subtract 1 from  wa_zfit0062-dias_u_pri_inter.
          vtx_com_interp2 = vtx_proj_ano_bana + ( ( wa_zfit0062-dias_u_mtm_cto - wa_zfit0062-dias_u_pri_inter ) / ( ( wa_zfit0062-dias_u_seq_inter - wa_zfit0062-dias_u_pri_inter ) / ( vtx_proj_ano_band - vtx_proj_ano_bana ) ) ) .
        endif.

        if vtx_cupom_cambd gt 0 and vtx_cupom_camba  gt 0 and vtx_cupom_cambd ne vtx_cupom_camba.
          vtx_com_interp3 = vtx_cupom_camba + ( ( wa_zfit0062-dias_c_mtm_cto - wa_zfit0062-dias_c_pri_inter ) /  ( ( wa_zfit0062-dias_c_seq_inter - wa_zfit0062-dias_c_pri_inter )  / ( vtx_cupom_cambd - vtx_cupom_camba ) ) ).
        else.
          vtx_com_interp3 = 0.
        endif.

      endif.
    endif.
    if vtx_com_interp2 = 0.
      read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062-seq.
      vtx_com_interp2  = wa_zfit0060-tx_proj_ano_ban.
    endif.


    if vponta_ativa = 'IPCA'.
      wa_zfit0069-tx_comp_dolar_at = wa_zfit0069-tx_data_base /  wa_zfit0069-tx_cambio_in_op.
      wa_zfit0069-ft_atual_ativa   = ( ( ( wa_zfit0069-tx_al_index_ativ / 100 ) / 360 ) * wa_zfit0069-dias_corridos_02 ) + 1.
      wa_zfit0069-vlr_curva_ativa  = wa_zfit0069-vlr_operacao_int * wa_zfit0069-tx_comp_dolar_at * wa_zfit0069-ft_atual_ativa.
      wa_zfit0069-tx_inter_dolar_a = wa_zfit0069-tx_proj_interp / wa_zfit0069-tx_cambio_in_op.
      wa_zfit0069-ft_multp_ativa = ( ( wa_zfit0069-tx_al_index_ativ / 100 ) / 360 * wa_zfit0069-dias_corridos_01 ) + 1.
      wa_zfit0069-vlr_fut_ativa = wa_zfit0069-vlr_operacao_int * wa_zfit0069-tx_inter_dolar_a * wa_zfit0069-ft_multp_ativa.
      wa_zfit0069-vlr_mtm_p_ativ = wa_zfit0069-vlr_fut_ativa / ( 1 + ( wa_zfit0069-tx_inter_cdi_ati / 100 ) ).
    endif.
    "
    "1.	INDEXADOR % DE CDI
    if vponta_ativa = 'CDI'.
      wa_zfit0069-ft_multp_ativa = ( 1 + ( wa_zfit0069-tx_al_index_ativ / 100 ) ) ** ( wa_zfit0069-dias_uteis_02 / 252 ).
      wa_zfit0069-vlr_curva_ativa = wa_zfit0069-vlr_operacao_int * wa_zfit0069-ft_acum_indev * wa_zfit0069-ft_multp_ativa.
      wa_zfit0069-ind_proj_final = ( 1 + ( ( wa_zfit0069-proj_cdi_inter * wa_zfit0069-tx_index_ativo ) / 10000 ) ).
      vg_ftr_multp_ativa = 1 + ( wa_zfit0069-tx_al_index_ativ / 100 ) ** ( wa_zfit0069-dias_uteis_03 / 252 ).
      wa_zfit0069-vlr_fut_ativa  = wa_zfit0069-vlr_curva_ativa * wa_zfit0069-ind_proj_final * vg_ftr_multp_ativa .
      wa_zfit0069-vlr_mtm_p_ativ = wa_zfit0069-vlr_fut_ativa / ( 1 + ( wa_zfit0069-tx_inter_cdi_pas / 100 ) ).
    endif.

    if vponta_passiva = 'CDI'.
*      wa_zfit0069-ft_multp_passiva = ( 1 + ( wa_zfit0069-tx_al_index_pass / 100 ) ) ** ( wa_zfit0069-dias_uteis_02 / 252 ).
*      wa_zfit0069-vlr_curva_passiv = wa_zfit0069-vlr_operacao_int * wa_zfit0069-ft_acum_indev * wa_zfit0069-ft_multp_passiva.
*      wa_zfit0069-ind_proj_final = ( 1 + ( ( wa_zfit0069-proj_cdi_inter * wa_zfit0069-tx_index_passivo ) / 10000 ) ).
*      wa_zfit0069-ftr_multp_passiv = 1 + ( wa_zfit0069-tx_al_index_pass / 100 ) ** ( wa_zfit0069-dias_uteis_03 / 252 ).
*      wa_zfit0069-vlr_fut_passiva = wa_zfit0069-vlr_curva_passiv * wa_zfit0069-ind_proj_final * wa_zfit0069-ftr_multp_passiv.
*      wa_zfit0069-vlr_mtm_p_passiv  = wa_zfit0069-vlr_fut_passiva / ( 1 + ( wa_zfit0069-tx_inter_cdi_ati / 100 ) ).
      wa_zfit0069-ft_multp_passiva = ( 1 + ( wa_zfit0069-tx_al_index_pass / 100 ) ) ** ( wa_zfit0069-dias_uteis_02 / 252 ).
      wa_zfit0069-vlr_curva_passiv = wa_zfit0069-vlr_operacao_int * wa_zfit0069-ft_acum_indev * wa_zfit0069-ft_multp_passiva.
      wa_zfit0069-ind_proj_final = ( 1 + ( ( wa_zfit0069-proj_cdi_inter * wa_zfit0069-tx_index_passivo ) / 10000 ) ).
      wa_zfit0069-ftr_multp_passiv = 1 + ( wa_zfit0069-tx_al_index_pass / 100 ) ** ( wa_zfit0069-dias_uteis_03 / 252 ).
      wa_zfit0069-vlr_fut_passiva = wa_zfit0069-vlr_curva_passiv * wa_zfit0069-ind_proj_final * wa_zfit0069-ftr_multp_passiv.
      wa_zfit0069-vlr_mtm_p_passiv  = wa_zfit0069-vlr_fut_passiva / ( 1 + ( wa_zfit0069-tx_inter_cdi_pas / 100 ) ).
    endif.

    "2.	INDEXADOR EM PRÉ USD + VARIAÇÃO CAMBIAL USDBRL
    if vtx_com_interp3 = 0.
      read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062-seq.
      vtx_com_interp3  = wa_zfit0060-tx_cupom_camb.
    endif.

    translate wa_zfit0059-pfj_agente to upper case.

    if vponta_ativa = 'PRE_USD+VC'.
      wa_zfit0069-tx_comp_dolar_at = wa_zfit0069-tx_data_base /  wa_zfit0069-tx_cambio_in_op.
      wa_zfit0069-ft_atual_ativa   = ( ( ( wa_zfit0069-tx_al_index_ativ / 100 ) / 360 ) * wa_zfit0069-dias_corridos_02 ) + 1.
      wa_zfit0069-vlr_curva_ativa  = wa_zfit0069-vlr_operacao_int * wa_zfit0069-tx_comp_dolar_at * wa_zfit0069-ft_atual_ativa.
      wa_zfit0069-tx_inter_dolar_a = wa_zfit0069-tx_proj_interp / wa_zfit0069-tx_cambio_in_op.
      wa_zfit0069-ft_multp_ativa = ( ( wa_zfit0069-tx_al_index_ativ / 100 ) / 360 * wa_zfit0069-dias_corridos_01 ) + 1.
      wa_zfit0069-vlr_fut_ativa = wa_zfit0069-vlr_operacao_int * wa_zfit0069-tx_inter_dolar_a * wa_zfit0069-ft_multp_ativa.
      if wa_zfit0059-pfj_agente+0(8) = 'RABOBANK'.
        wa_zfit0069-vlr_mtm_p_ativ = ( wa_zfit0059-valor_operacao * wa_zfit0069-tx_data_base ) *
                                       ( 1 +  ( ( wa_zfit0069-tx_al_index_ativ / 100 )  * wa_zfit0069-dias_corridos_01 / 360  ) )
                                       / ( 1 + ( vtx_com_interp3 / 100 )  *  wa_zfit0069-dias_corridos_03 / 360 ). "novo calculo - Taxa pré
      else.
        wa_zfit0069-vlr_mtm_p_ativ = wa_zfit0069-vlr_fut_ativa / ( 1 + ( wa_zfit0069-tx_inter_cdi_ati / 100 ) ).
      endif.
    endif.
    if vponta_passiva = 'PRE_USD+VC'.
      wa_zfit0069-tx_comp_dolar_at = wa_zfit0069-tx_data_base /  wa_zfit0069-tx_cambio_in_op.
      wa_zfit0069-ft_atual_ativa   = ( ( ( wa_zfit0069-tx_al_index_pass / 100 ) / 360 ) * wa_zfit0069-dias_corridos_02 ) + 1.
      wa_zfit0069-vlr_curva_passiv  = wa_zfit0069-vlr_operacao_int * wa_zfit0069-tx_comp_dolar_at * wa_zfit0069-ft_atual_ativa.
      wa_zfit0069-tx_inter_dolar_p = wa_zfit0069-tx_proj_interp / wa_zfit0069-tx_cambio_in_op.
      wa_zfit0069-ft_multp_passiva = ( ( wa_zfit0069-tx_al_index_pass / 100 ) / 360 * wa_zfit0069-dias_corridos_01 ) + 1.
      wa_zfit0069-vlr_fut_passiva  = wa_zfit0069-vlr_operacao_int * wa_zfit0069-tx_inter_dolar_p * wa_zfit0069-ft_multp_passiva.
      if wa_zfit0059-pfj_agente+0(8) = 'RABOBANK'.
        wa_zfit0069-vlr_mtm_p_passiv = ( wa_zfit0059-valor_operacao * wa_zfit0069-tx_data_base ) *
                                       ( 1 +  ( ( wa_zfit0069-tx_al_index_pass / 100 )  * wa_zfit0069-dias_corridos_01 / 360  ) )
                                       / ( 1 + ( vtx_com_interp3 / 100 )  *  wa_zfit0069-dias_corridos_03 / 360 ). "novo calculo - Taxa pré
      else.
        wa_zfit0069-vlr_mtm_p_passiv = wa_zfit0069-vlr_fut_passiva / ( 1 + ( wa_zfit0069-tx_inter_cdi_ati / 100 ) ).
      endif.
    endif.

    "3.	INDEXADOR EM PRÉ BRL
    if vponta_ativa = 'PRE_BRL'.
*      wa_zfit0069-vlr_mtm_p_ativ = wa_zfit0069-vlr_operacao_int * ( 1 + ( wa_zfit0069-tx_al_index_ativ / 100 ) ) ** ( wa_zfit0069-dias_uteis_01 / 252 ) /
*                                ( 1 +  ( vtx_com_interp2 / 100 ) ) ** ( wa_zfit0069-dias_uteis_03 / 252 ) .

      wa_zfit0069-tx_inter_dolar_a = wa_zfit0069-tx_proj_interp / wa_zfit0069-tx_cambio_in_op.
      wa_zfit0069-ft_multp_ativa = ( ( wa_zfit0069-tx_al_index_ativ / 100 ) / 360 * wa_zfit0069-dias_corridos_01 ) + 1.
      wa_zfit0069-vlr_fut_ativa = wa_zfit0069-vlr_operacao_int * wa_zfit0069-tx_inter_dolar_a * wa_zfit0069-ft_multp_ativa.
      wa_zfit0069-vlr_mtm_p_ativ = wa_zfit0069-vlr_fut_ativa / ( 1 + ( wa_zfit0069-tx_inter_cdi_ati / 100 ) ). "Por CDI


    endif.


    if vponta_passiva = 'PRE_BRL'.
      wa_zfit0069-tx_inter_dolar_p = wa_zfit0069-tx_proj_interp / wa_zfit0069-tx_cambio_in_op.
      wa_zfit0069-ft_atual_ativa   = ( ( ( wa_zfit0069-tx_al_index_pass / 100 ) / 360 ) * wa_zfit0069-dias_corridos_01 ) + 1.
      wa_zfit0069-vlr_fut_passiva  = wa_zfit0069-vlr_operacao_int * wa_zfit0069-tx_inter_dolar_p * wa_zfit0069-ft_atual_ativa.
      wa_zfit0069-vlr_mtm_p_passiv = wa_zfit0069-vlr_fut_passiva / ( 1 + ( wa_zfit0069-tx_inter_cdi_ati / 100 ) ).

    endif.

    wa_zfit0069-vlr_aj_merc	      = ( wa_zfit0069-vlr_mtm_p_ativ - wa_zfit0069-vlr_mtm_p_passiv ).

    vg_vlr_aj_merc  = wa_zfit0069-vlr_aj_merc.

    clear wa_zfit0069-vlr_extrato_bco.
    select single vlr_extrato_bco
         from zfit0214
         into wa_zfit0069-vlr_extrato_bco
         where dt_fechamento = wg_cadpro-dt_fechamento
         and   bukrs         = wg_cadpro-bukrs
         and   nro_cto       = wa_zfit0069-nro_cto
         and   dt_fim_cto    = wa_zfit0069-dt_fim_cto
         and   cod_oper      = 'H'.

    if sy-subrc = 0.
      wa_zfit0069-vlr_aj_merc = wa_zfit0069-vlr_extrato_bco.
    endif.

    if wa_zfit0069-vlr_aj_merc > 0.
      wa_zfit0069-tp_ajuste = 'ATIVO'.
    else.
      wa_zfit0069-tp_ajuste = 'PASSIVO'.
    endif.

*** PBI - 73761 - Inicio - CBRAND
    if wa_zfit0069-dias_corridos_03  > 365 and wa_zfit0069-vlr_aj_merc gt 0 .
      wa_zfit0069-tp_ajuste = 'ATIVOLP'.
    else.
      if  wa_zfit0069-dias_corridos_03 > 365 and wa_zfit0069-vlr_aj_merc  le 0 .
        wa_zfit0069-tp_ajuste = 'PASSIVOLP'.
      endif.
    endif.
*** PBI - 73761 - Fim - CBRAND

    if vponta_ativa  = vponta_passiva.
      wa_zfit0069-vlr_aj_merc = 0.
      if wa_zfit0069-dias_corridos_03  > 365.
        wa_zfit0069-tp_ajuste = 'ATIVOLP'.
      else.
        if  wa_zfit0069-dias_corridos_03 > 365.
          wa_zfit0069-tp_ajuste = 'PASSIVOLP'.
        endif.
      endif.
      "  wa_zfit0069-tp_ajuste  = 'ERRO FORMULA'.
    endif.

    wa_zfit0069-vlr_aj_merc = vg_vlr_aj_merc.

    move:
    sy-uname                 to wa_zfit0069-usnam,
    sy-datum                 to wa_zfit0069-data_atual,
    sy-uzeit                 to wa_zfit0069-hora_atual.
    append wa_zfit0069 to it_zfit0069.
    clear wa_zfit0069.

  endloop.

  refresh tg_itens6.
  loop at it_zfit0069 into wa_zfit0069.
    move-corresponding wa_zfit0069 to tg_itens6.
    clear: wl_color.
    refresh tg_itens6-color.
    wl_color-fieldname = 'VLR_EXTRATO_BCO'.
    wl_color-color-col = 3.
    wl_color-color-inv = 3.
    append wl_color to tg_itens6-color.
    tg_itens6-ano = tg_itens6-dt_fim_cto+0(4).
    append tg_itens6.
  endloop.

  if it_zfit0069[] is initial.
    message s000(zwrm001) display like 'E' with 'Não há dados SWAP Vanila!'.
  else.
    message s836(sd) with 'Cálculo SWAP Vanila'
                       ',Realizado com sucesso!'.
  endif.

endform.                    "F_CALCULO_SWAP_H

form f_calculo_swap_h_atu. "Swap Vanila alrs
  data:
*        vtx_proj_ano_com   type zfator_desc,
*        vtx_proj_ano_ban   type zfator_desc,
    vponta_ativa(10),
    vponta_passiva(10),
    vg_ftr_multp_ativa type zfator_desc,
    zlv_offset         type i,
    vg_vlr_aj_merc     type zfit0069-vlr_aj_merc.

  data: vtx_proj_ano_com  type ztx_par_dolar,
        vtx_proj_ano_como type ztx_par_dolar,
        vtx_proj_ano_coma type ztx_par_dolar,
        vtx_proj_ano_ban  type ztx_par_dolar,
        vtx_proj_ano_bano type ztx_par_dolar,
        vtx_proj_ano_bana type ztx_par_dolar,
        vg_dt_fim_cto     type zfit0067-dt_fim_cto,
        vg_seqa           type zfit0062-seq.

  loop at it_zfit0069 into wa_zfit0069.
    if wa_zfit0069-obj_key is not initial.
      vobj_key = wa_zfit0069-obj_key.
    endif.
  endloop.
  " Verifica se já foi contabilizado, neste caso não contabiliza este calculo - fazer estorno antes
  select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

  if sy-subrc eq 0.
    exit.
  endif.

  refresh it_zfit0069.

  select *
    from zfit0059
    into table it_zfit0059
    where mdo_tipo  = 'H'
    and   data_vencimento gt  wg_cadpro-dt_fechamento
    and   data_realizacao le  wg_cadpro-dt_fechamento
    and   pfj_codigo      eq wg_cadpro-bukrs
    and   regra           ne ''
    and   valor_operacao  ne 0.

  loop at it_zfit0059 into wa_zfit0059.
    "US 150369 operações FREE_SCHED não tem restrição porque vira do XRT como CTF ao inves de VANILA
    translate wa_zfit0059-mdo_codigo to upper case.

    delete it_zfit0059 where posicao = 'XX'.
    wa_zfit0069-dt_fechamento    = wg_cadpro-dt_fechamento.
    wa_zfit0069-bukrs            = wa_zfit0059-pfj_codigo.
    wa_zfit0069-nro_cto          = wa_zfit0059-opr_numero.
    wa_zfit0069-cod_oper         = wa_zfit0059-mdo_tipo.
    wa_zfit0069-banco            = wa_zfit0059-pfj_agente.
    wa_zfit0069-dt_inicio_cto	   = wa_zfit0059-data_realizacao.
    wa_zfit0069-dt_fim_cto       = wa_zfit0059-data_vencimento.
    wa_zfit0069-tx_data_base     = wg_cadpro-ptax.
    wa_zfit0069-vlr_operacao     = wa_zfit0059-valor_operacao.
    wa_zfit0069-vlr_operacao_int  = wa_zfit0059-valor_operacao * wa_zfit0059-tx_dolar_in_op.
    wa_zfit0069-tx_cambio_in_op	 = wa_zfit0059-tx_dolar_in_op.

    wa_zfit0069-index_ativo      = wa_zfit0059-ind_codigo_ativo.
    wa_zfit0069-index_passivo    = wa_zfit0059-ind_codigo_passivo.

    wa_zfit0069-tx_index_ativo   = wa_zfit0059-index_ativo.
    wa_zfit0069-tx_index_passivo = wa_zfit0059-index_passivo.

    wa_zfit0069-tx_al_index_ativ = wa_zfit0059-tx_ind_ativo.
    wa_zfit0069-tx_al_index_pass = wa_zfit0059-tx_ind_passivo.


    clear : vponta_ativa,vponta_passiva.
    if wa_zfit0059-ind_codigo_ativo = 'IPCA_15_M-2'.
      vponta_ativa = 'IPCA'.
*      wa_zfit0069-index_ativo      = 'IPCA'.
    elseif wa_zfit0059-ind_codigo_ativo = 'USD_VENDA'.
      vponta_ativa = 'PRE_USD+VC'.
*      wa_zfit0069-index_ativo      = 'USD'.
    elseif  wa_zfit0059-ind_codigo_ativo = 'CDI'.
      vponta_ativa = 'CDI'.
*      wa_zfit0069-index_ativo      = 'CDI'.
    else.
      vponta_ativa = 'PRE_BRL'.
*      wa_zfit0069-index_ativo      = 'BRL'.
    endif.

    if wa_zfit0059-ind_codigo_passivo = 'USD_VENDA'.
      vponta_passiva = 'PRE_USD+VC'.
*      wa_zfit0069-index_passivo      = 'USD'.
    elseif  wa_zfit0059-ind_codigo_passivo = 'CDI'.
      vponta_passiva = 'CDI'.
*      wa_zfit0069-index_passivo      = 'CDI'.
    else.
      vponta_passiva = 'PRE_BRL'.
*      wa_zfit0069-index_passivo      = 'BRL'.
    endif.


    wa_zfit0069-dias_corridos_01 = wa_zfit0069-dt_fim_cto - wa_zfit0069-dt_inicio_cto.
    perform calc_dias_uteis using wa_zfit0069-dt_inicio_cto wa_zfit0069-dt_fim_cto changing v_dias_uteis.
    wa_zfit0069-dias_uteis_01 = v_dias_uteis.

    wa_zfit0069-dias_corridos_02  = wg_cadpro-dt_fechamento - wa_zfit0069-dt_inicio_cto.
    if wa_zfit0069-dias_corridos_02 gt 0.
      perform calc_dias_uteis using wa_zfit0069-dt_inicio_cto wg_cadpro-dt_fechamento changing v_dias_uteis.
    else.
      perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0069-dt_inicio_cto changing v_dias_uteis.
      v_dias_uteis = v_dias_uteis * -1.
    endif.
    wa_zfit0069-dias_uteis_02 = v_dias_uteis.

    wa_zfit0069-dias_corridos_03  = wa_zfit0069-dt_fim_cto - wg_cadpro-dt_fechamento.
    perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0069-dt_fim_cto  changing v_dias_uteis.
    wa_zfit0069-dias_uteis_03 = v_dias_uteis.


    if vponta_passiva = 'CDI'.
      perform f_calculo_ft_acum using wa_zfit0069-dt_inicio_cto wa_zfit0069-tx_index_passivo   changing v_ft_acum_indev.
    elseif  vponta_ativa = 'CDI'.
      perform f_calculo_ft_acum using wa_zfit0069-dt_inicio_cto wa_zfit0069-tx_index_ativo   changing v_ft_acum_indev.
    endif.

    wa_zfit0069-ft_acum_indev = v_ft_acum_indev.

    if wa_zfit0069-dt_inicio_cto ne ''.
      read table it_zfit0062 into wa_zfit0062
       with key bukrs         = wg_cadpro-bukrs
                dt_fechamento = wg_cadpro-dt_fechamento
                dt_vcto_cto   = wa_zfit0069-dt_fim_cto.
      if sy-subrc = 0.
        wa_zfit0069-tx_proj_interp   = wa_zfit0062-tx_cb_interp.
        wa_zfit0069-tx_inter_cdi_ati = wa_zfit0062-tx_com_interp.
        wa_zfit0069-proj_cdi_inter   = wa_zfit0062-tx_bco_interp.
        wa_zfit0069-tx_inter_cdi_pas = wa_zfit0062-tx_com_interp.
      endif.
    endif.

    clear: vtx_proj_ano_ban, vtx_proj_ano_com..
    if wa_zfit0069-dt_inicio_cto ne ''.
      read table it_zfit0062 into wa_zfit0062
       with key bukrs         = wg_cadpro-bukrs
                dt_fechamento = wg_cadpro-dt_fechamento
                dt_vcto_cto   = wa_zfit0069-dt_fim_cto.
      if sy-subrc = 0.
        "
*        clear wa_zfit0060.
*        read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062-seq.
*        vtx_proj_ano_com = wa_zfit0060-tx_proj_ano_com.
*        vtx_proj_ano_ban = wa_zfit0060-tx_proj_ano_ban.

        clear wa_zfit0060.
        read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062-seq.
        vtx_proj_ano_bano = wa_zfit0060-tx_proj_ano_ban.
        vtx_proj_ano_como = wa_zfit0060-tx_proj_ano_com.

        read table it_zfit0060 into wa_zfit0060 with key dias_corridos = wa_zfit0062-dias_c_mtm_cto.
        if sy-subrc = 0.
          vtx_proj_ano_com = wa_zfit0060-tx_proj_ano_com.
          vtx_proj_ano_ban = wa_zfit0060-tx_proj_ano_ban.
        else.
          vg_seqa = wa_zfit0062-seq + 1.
          read table it_zfit0060 into wa_zfit0060 with key seq = vg_seqa.
          vtx_proj_ano_com = wa_zfit0060-tx_proj_ano_com.
          vtx_proj_ano_ban = wa_zfit0060-tx_proj_ano_ban.
        endif.
        if vtx_proj_ano_bano ne vtx_proj_ano_ban.
          vtx_proj_ano_bana = ( ( vtx_proj_ano_bano / 100 ) + ( ( ( vtx_proj_ano_ban / 100 ) - ( vtx_proj_ano_bano / 100 ) ) * ( ( wa_zfit0062-dias_u_mtm_cto - wa_zfit0062-dias_u_pri_inter ) /
                             ( wa_zfit0062-dias_u_seq_inter - wa_zfit0062-dias_u_pri_inter ) ) ) ) * 100.
          vtx_proj_ano_ban = vtx_proj_ano_bana.
          clear vtx_proj_ano_bana.
        endif.

        if vtx_proj_ano_como ne vtx_proj_ano_com.
          vtx_proj_ano_coma = ( ( vtx_proj_ano_como / 100 ) + ( ( ( vtx_proj_ano_com / 100 ) - ( vtx_proj_ano_como / 100 ) ) * ( ( wa_zfit0062-dias_c_mtm_cto - wa_zfit0062-dias_c_pri_inter ) /
                             ( wa_zfit0062-dias_c_seq_inter - wa_zfit0062-dias_c_pri_inter ) ) ) ) * 100.
          vtx_proj_ano_com = vtx_proj_ano_coma.
          clear vtx_proj_ano_coma.
        endif.

      endif.


    endif.

    "BULLET
    wa_zfit0069-moeda_comparada =  ( wa_zfit0069-tx_proj_interp / wa_zfit0059-tx_dolar_in_op ).
    wa_zfit0069-saldo_divida    = wa_zfit0069-vlr_operacao_int * wa_zfit0069-moeda_comparada.
    translate wa_zfit0059-contrato to upper case.
    if wa_zfit0059-contrato = 'APLICACAO' or
       wa_zfit0059-contrato = 'VAN_COLCHAO'.
      "ativa
      wa_zfit0069-tipo_opera = 'APLICACAO'.
      wa_zfit0069-ft_multp_ativa = ( ( ( wa_zfit0069-tx_al_index_ativ / 100 ) / 360 ) * wa_zfit0069-dias_corridos_01 ) + 1.

      wa_zfit0069-ind_proj_final =  ( ( ( 1 + ( vtx_proj_ano_com / 100 ) ) ** ( wa_zfit0069-dias_corridos_03 / 360 ) ) - 1 ) * 100.
      wa_zfit0069-vlr_fut_ativa  = wa_zfit0069-vlr_operacao_int * ( wa_zfit0069-tx_proj_interp / wa_zfit0059-tx_dolar_in_op ) * wa_zfit0069-ft_multp_ativa.
      wa_zfit0069-vlr_mtm_p_ativ = wa_zfit0069-vlr_fut_ativa / ( 1 + ( wa_zfit0069-ind_proj_final / 100 ) ).
      "passiva
      wa_zfit0069-ft_multp_passiva = ( ( ( 1 + ( wa_zfit0069-tx_al_index_pass ) / 100 ) ) ** ( wa_zfit0069-dias_corridos_01 / 360 ) ).
      wa_zfit0069-vlr_curva_passiv = wa_zfit0069-vlr_operacao_int * wa_zfit0069-ft_acum_indev.
      wa_zfit0069-p_cdi_interp     = ( vtx_proj_ano_ban / 100 ) * ( wa_zfit0069-tx_index_passivo / 100 ).
      wa_zfit0069-p_ind_final      = ( 1 + wa_zfit0069-p_cdi_interp ) ** ( wa_zfit0062-dias_u_mtm_cto / 252 ).
      wa_zfit0069-vlr_fut_passiva  = wa_zfit0069-vlr_curva_passiv  *  wa_zfit0069-p_ind_final.
      wa_zfit0069-vlr_mtm_p_passiv = wa_zfit0069-vlr_fut_passiva / ( 1 + ( wa_zfit0069-ind_proj_final / 100 ) ).
    endif.

    if wa_zfit0059-contrato = 'VAN_FINANC.'. "BULLET
      wa_zfit0069-tipo_opera = 'DIVIDA'.
      "passiva
      wa_zfit0069-ft_multp_passiva = ( ( ( wa_zfit0069-tx_al_index_pass / 100 ) / 360 ) * wa_zfit0069-dias_corridos_01 ) + 1.
      wa_zfit0069-ind_proj_final   = ( ( ( 1 + ( vtx_proj_ano_com / 100 ) ) ** ( wa_zfit0069-dias_corridos_03 / 360 ) ) - 1 ) * 100.
      wa_zfit0069-vlr_fut_passiva  = wa_zfit0069-vlr_operacao_int * ( wa_zfit0069-tx_proj_interp / wa_zfit0059-tx_dolar_in_op ) * wa_zfit0069-ft_multp_passiva.
      wa_zfit0069-vlr_mtm_p_passiv = wa_zfit0069-vlr_fut_passiva / ( 1 + ( wa_zfit0069-ind_proj_final / 100 ) ).
      "ativa
      wa_zfit0069-ft_multp_ativa = ( 1 + ( vtx_proj_ano_ban / 100 ) ) ** ( wa_zfit0069-dias_uteis_03 / 252 ).
      wa_zfit0069-vlr_curva_ativa = wa_zfit0069-vlr_operacao_int * wa_zfit0069-ft_acum_indev.
*      wa_zfit0069-ind_proj_final =  vtx_proj_ano_com * ( wa_zfit0069-dias_corridos_03 / 360 ).
      wa_zfit0069-tx_inter_cdi_ati = vtx_proj_ano_ban.

      wa_zfit0069-vlr_fut_ativa = wa_zfit0069-vlr_curva_ativa * wa_zfit0069-ft_multp_ativa.
      wa_zfit0069-vlr_mtm_p_ativ  = wa_zfit0069-vlr_fut_ativa / ( 1 + ( wa_zfit0069-ind_proj_final  / 100 ) ).

    endif.

    translate wa_zfit0059-pfj_agente to upper case.

    wa_zfit0069-vlr_aj_merc	      = ( wa_zfit0069-vlr_mtm_p_ativ - wa_zfit0069-vlr_mtm_p_passiv ).

    vg_vlr_aj_merc  = wa_zfit0069-vlr_aj_merc.

    clear wa_zfit0069-vlr_extrato_bco.
    select single vlr_extrato_bco
         from zfit0214
         into wa_zfit0069-vlr_extrato_bco
         where dt_fechamento = wg_cadpro-dt_fechamento
         and   bukrs         = wg_cadpro-bukrs
         and   nro_cto       = wa_zfit0069-nro_cto
         and   dt_fim_cto    = wa_zfit0069-dt_fim_cto
         and   cod_oper      = 'H'.

    if sy-subrc = 0.
      wa_zfit0069-vlr_aj_merc = wa_zfit0069-vlr_extrato_bco.
    endif.

    if wa_zfit0069-vlr_aj_merc > 0.
      wa_zfit0069-tp_ajuste = 'ATIVO'.
    else.
      wa_zfit0069-tp_ajuste = 'PASSIVO'.
    endif.

*** PBI - 73761 - Inicio - CBRAND
    if wa_zfit0069-dias_corridos_03  > 365 and wa_zfit0069-vlr_aj_merc gt 0 .
      wa_zfit0069-tp_ajuste = 'ATIVOLP'.
    else.
      if  wa_zfit0069-dias_corridos_03 > 365 and wa_zfit0069-vlr_aj_merc  le 0 .
        wa_zfit0069-tp_ajuste = 'PASSIVOLP'.
      endif.
    endif.
*** PBI - 73761 - Fim - CBRAND

    if vponta_ativa  = vponta_passiva.
      wa_zfit0069-vlr_aj_merc = 0.
      if wa_zfit0069-dias_corridos_03  > 365.
        wa_zfit0069-tp_ajuste = 'ATIVOLP'.
      else.
        if  wa_zfit0069-dias_corridos_03 > 365.
          wa_zfit0069-tp_ajuste = 'PASSIVOLP'.
        endif.
      endif.
      "  wa_zfit0069-tp_ajuste  = 'ERRO FORMULA'.
    endif.

    wa_zfit0069-vlr_aj_merc = vg_vlr_aj_merc.

    move:
    sy-uname                 to wa_zfit0069-usnam,
    sy-datum                 to wa_zfit0069-data_atual,
    sy-uzeit                 to wa_zfit0069-hora_atual.
    append wa_zfit0069 to it_zfit0069.
    clear wa_zfit0069.

  endloop.
  select *
    from zfit0066
    into table it_zfit0066
    where  cod_oper      eq 'H'.
  sort it_zfit0066 by tp_operacao tp_ajuste.

  refresh tg_itens6.
  loop at it_zfit0069 into wa_zfit0069.
    move-corresponding wa_zfit0069 to tg_itens6.
    tg_itens6-ind_proj_final2 = tg_itens6-ind_proj_final.
    clear: wl_color.
    refresh tg_itens6-color.
    wl_color-fieldname = 'VLR_EXTRATO_BCO'.
    wl_color-color-col = 3.
    wl_color-color-inv = 3.
    append wl_color to tg_itens6-color.
    tg_itens6-ano = tg_itens6-dt_fim_cto+0(4).
    loop at it_zfit0066 into wa_zfit0066 where tp_ajuste   = wa_zfit0069-tp_ajuste.
      if wa_zfit0066-bschl = '40'.
        tg_itens6-hkont_d = wa_zfit0066-hkont.
      else.
        tg_itens6-hkont_c = wa_zfit0066-hkont.
      endif.
    endloop.
    append tg_itens6.
  endloop.

  tg_itens6_aux[] = tg_itens6[].

  if it_zfit0069[] is initial.
    message s000(zwrm001) display like 'E' with 'Não há dados SWAP Vanila!'.
  else.
    message s836(sd) with 'Cálculo SWAP Vanila'
                       ',Realizado com sucesso!'.
  endif.

endform.                    "F_CALCULO_SWAP_H

*&---------------------------------------------------------------------*
*&      Form  F_CALCULO_SWAP_V
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_calculo_swap_v.

  data: vg_vlr_aj_merc type  zfit0070-vlr_aj_merc.

  loop at it_zfit0070 into wa_zfit0070.
    if wa_zfit0070-obj_key is not initial.
      vobj_key = wa_zfit0070-obj_key.
    endif.
  endloop.
  " Verifica se já foi contabilizado, neste caso não contabiliza este calculo - fazer estorno antes
  select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

  if sy-subrc eq 0.
    exit.
  endif.

  refresh it_zfit0070.

  select *
    from zfit0065
    into table it_zfit0065
    where cod_oper   =  'V' " SWAP V
    and   dt_fim_cto gt wg_cadpro-dt_fechamento
    "AND   DT_INICIO_CTO LE WG_CADPRO-DT_FECHAMENTO
    and   bukrs      eq wg_cadpro-bukrs.


  loop at it_zfit0065 into wa_zfit0065.
    wa_zfit0070-dt_fechamento     = wg_cadpro-dt_fechamento.
    wa_zfit0070-bukrs             = wa_zfit0065-bukrs.
    wa_zfit0070-nro_cto           = wa_zfit0065-nro_cto.
    wa_zfit0070-nro_par           = wa_zfit0065-nro_par.
    wa_zfit0070-cod_oper          = wa_zfit0065-cod_oper.
    wa_zfit0070-banco             = wa_zfit0065-banco.
    wa_zfit0070-dt_inicio_cto	    =	wa_zfit0065-dt_inicio_cto.
    wa_zfit0070-dt_fim_cto        = wa_zfit0065-dt_fim_cto.
    wa_zfit0070-vlr_operacao_int  = wa_zfit0065-vlr_cto_r.
    wa_zfit0070-vlr_operacao      = wa_zfit0065-vlr_cto_us.
    wa_zfit0070-vlr_base_vc       = wa_zfit0065-vlr_parc.
    wa_zfit0070-moeda             = wa_zfit0065-moeda.
    wa_zfit0070-tp_tx_ativa       = wa_zfit0065-tp_tx_ativa.
    wa_zfit0070-tx_cambio_in_op	  =	wa_zfit0065-tx_cambio_in_op.
    wa_zfit0070-tp_tx_passiva     = wa_zfit0065-tp_tx_passiva.
    wa_zfit0070-tx_jros_passiva	  =	wa_zfit0065-tx_jros_passiva.
    wa_zfit0070-tx_jrs_alem_cdi   = wa_zfit0065-tx_acima_ind.
    wa_zfit0070-index_ativo       = wa_zfit0065-index_ativo.
    wa_zfit0070-tx_perct_cdi      = wa_zfit0065-tx_index_ativo.
    wa_zfit0070-tx_data_base      = wg_cadpro-ptax.


    if wa_zfit0070-vlr_operacao_int eq 0.
      wa_zfit0070-vlr_operacao_int  = ( wa_zfit0070-vlr_operacao * wa_zfit0070-tx_cambio_in_op ).
    endif.

    wa_zfit0070-dias_corridos_01 = wa_zfit0070-dt_fim_cto - wa_zfit0070-dt_inicio_cto.
    perform calc_dias_uteis using wa_zfit0070-dt_inicio_cto wa_zfit0070-dt_fim_cto changing v_dias_uteis.
    wa_zfit0070-dias_uteis_01 = v_dias_uteis.

    wa_zfit0070-dias_corridos_02  = wg_cadpro-dt_fechamento - wa_zfit0070-dt_inicio_cto.
    if wa_zfit0070-dias_corridos_02 gt 0.
      perform calc_dias_uteis using wa_zfit0070-dt_inicio_cto wg_cadpro-dt_fechamento changing v_dias_uteis.
    else.
      perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0070-dt_inicio_cto changing v_dias_uteis.
      v_dias_uteis = v_dias_uteis * -1.
    endif.
    wa_zfit0070-dias_uteis_02 = v_dias_uteis.

    wa_zfit0070-dias_corridos_03  = wa_zfit0070-dt_fim_cto - wg_cadpro-dt_fechamento.
    perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0070-dt_fim_cto  changing v_dias_uteis.
    wa_zfit0070-dias_uteis_03 = v_dias_uteis.

    perform f_calculo_ft_acum using wa_zfit0070-dt_inicio_cto wa_zfit0070-tx_perct_cdi   changing v_ft_acum_indev.

    wa_zfit0070-ft_acum_indev = v_ft_acum_indev.

    wa_zfit0070-ft_multp_ativa = ( 1 + ( wa_zfit0070-tx_jrs_alem_cdi / 100 ) ) ** ( wa_zfit0070-dias_uteis_02 / 252 ).

    wa_zfit0070-vlr_curva_ativa	= wa_zfit0070-vlr_operacao_int * wa_zfit0070-ft_acum_indev * wa_zfit0070-ft_multp_ativa.

    wa_zfit0070-tx_comp_dolar_pa  = wa_zfit0070-tx_data_base / wa_zfit0070-tx_cambio_in_op.

    wa_zfit0070-ft_atual_passiva  = ( ( wa_zfit0070-tx_jros_passiva / 100 ) * wa_zfit0070-dias_corridos_02 / 360 ) + 1.

    wa_zfit0070-vlr_curva_passiv  = wa_zfit0070-vlr_operacao_int * wa_zfit0070-tx_comp_dolar_pa  * wa_zfit0070-ft_atual_passiva.

    if wa_zfit0070-dt_inicio_cto ne ''.
      read table it_zfit0062 into wa_zfit0062
       with key bukrs         = wg_cadpro-bukrs
                dt_fechamento = wg_cadpro-dt_fechamento
                dt_vcto_cto   = wa_zfit0070-dt_fim_cto.
      if sy-subrc = 0.
        wa_zfit0070-proj_cdi_inter   = wa_zfit0062-tx_bco_interp.
        wa_zfit0070-tx_desc_cdi	     = wa_zfit0062-tx_bco_interp.
        wa_zfit0070-tx_proj_interp   = wa_zfit0062-tx_cb_interp.
      endif.
    endif.

    wa_zfit0070-ind_proj_cdi_int 	= ( 1 + ( wa_zfit0070-tx_jrs_alem_cdi / 100 ) ) ** ( wa_zfit0070-dias_uteis_03 / 252 ).

    wa_zfit0070-vlr_proj_jros	  = wa_zfit0070-vlr_operacao_int * ( wa_zfit0070-proj_cdi_inter / 100 ) * wa_zfit0070-ind_proj_cdi_int.

    if ( wa_zfit0070-vlr_curva_ativa ne 0 ).
      wa_zfit0070-vlr_proj_jros   = wa_zfit0070-vlr_proj_jros + ( wa_zfit0070-vlr_curva_ativa -  wa_zfit0070-vlr_operacao_int ).
    endif.

    wa_zfit0070-vlr_mtm_p_ativ  = wa_zfit0070-vlr_proj_jros / ( 1 + ( wa_zfit0070-tx_desc_cdi / 100 ) ).

    wa_zfit0070-vlr_var_proj    = wa_zfit0070-tx_proj_interp / wa_zfit0070-tx_cambio_in_op.

    wa_zfit0070-vlr_var_cambial	= wa_zfit0070-vlr_base_vc * ( wa_zfit0070-tx_proj_interp - wa_zfit0070-tx_cambio_in_op ).

    wa_zfit0070-vlr_juros_fut	  = wa_zfit0070-vlr_operacao * wa_zfit0070-tx_proj_interp * ( wa_zfit0070-tx_jros_passiva / 100 ) * wa_zfit0070-dias_corridos_01 / 360.

    wa_zfit0070-tx_desc	= wa_zfit0070-tx_desc_cdi.

    wa_zfit0070-vlr_mtm_p_passiv  = ( wa_zfit0070-vlr_var_cambial + wa_zfit0070-vlr_juros_fut ) / ( 1 + ( wa_zfit0070-tx_desc / 100 ) ).

    wa_zfit0070-vlr_aj_merc	= wa_zfit0070-vlr_mtm_p_ativ - wa_zfit0070-vlr_mtm_p_passiv.

    vg_vlr_aj_merc = wa_zfit0070-vlr_aj_merc.

    clear wa_zfit0070-vlr_extrato_bco.
    select single vlr_extrato_bco
        from zfit0214
        into wa_zfit0070-vlr_extrato_bco
        where dt_fechamento = wg_cadpro-dt_fechamento
        and   bukrs         = wg_cadpro-bukrs
        and   nro_cto       = wa_zfit0070-nro_cto
        and   nro_par       = wa_zfit0070-nro_par
        and   cod_oper      = 'V'.

    if sy-subrc = 0.
      wa_zfit0070-vlr_aj_merc = wa_zfit0070-vlr_extrato_bco.
    endif.

    if wa_zfit0070-vlr_aj_merc > 0.
      wa_zfit0070-tp_ajuste = 'ATIVO'.
    else.
      wa_zfit0070-tp_ajuste = 'PASSIVO'.
    endif.

    if wa_zfit0070-dias_corridos_03  > 365 and wa_zfit0070-vlr_aj_merc gt 0 .
      wa_zfit0070-tp_ajuste = 'ATIVOLP'.
    else.
      if wa_zfit0070-dias_corridos_03 > 365 and wa_zfit0070-vlr_aj_merc le 0 .
        wa_zfit0070-tp_ajuste = 'PASSIVOLP'.
      endif.
    endif.


    wa_zfit0070-vlr_aj_merc = vg_vlr_aj_merc.
    move:
    sy-uname                 to wa_zfit0070-usnam,
    sy-datum                 to wa_zfit0070-data_atual,
    sy-uzeit                 to wa_zfit0070-hora_atual.
    append wa_zfit0070 to it_zfit0070.
    clear wa_zfit0070.

  endloop.

  refresh tg_itens7.
  loop at it_zfit0070 into wa_zfit0070.
    refresh tg_itens7-color.
    wl_color-fieldname = 'VLR_EXTRATO_BCO'.
    wl_color-color-col = 3.
    wl_color-color-inv = 3.
    append wl_color to tg_itens7-color.
    move-corresponding wa_zfit0070 to tg_itens7.
    tg_itens7-ano = tg_itens7-dt_fim_cto+0(4).
    append tg_itens7.
  endloop.

  if it_zfit0070[] is initial.
    message s000(zwrm001) display like 'E' with 'Não há dados SWAP Fluxo C. T.V.!'.
  else.
    message s836(sd) with 'Cálculo SWAP V'
                       ',Realizado com sucesso!'.
  endif.

endform.                    "F_CALCULO_SWAP_V

form f_calculo_swap_v_novo.
  data: vtx_proj_ano_bana type zfit0060-tx_proj_ano_ban,
        vtx_proj_ano_band type zfit0060-tx_proj_ano_ban,
        vtx_cupom_camba   type zfit0060-tx_cupom_camb,
        vtx_cupom_cambd   type zfit0060-tx_cupom_camb,

        vtx_com_interp2   type ztx_bpm,
        vtx_com_interp3   type ztx_bpm,
        vseqa             type zfit0062-seq,
        vseqd             type zfit0062-seq.

  loop at it_zfit0070 into wa_zfit0070.
    if wa_zfit0070-obj_key is not initial.
      vobj_key = wa_zfit0070-obj_key.
    endif.
  endloop.
  " Verifica se já foi contabilizado, neste caso não contabiliza este calculo - fazer estorno antes
  select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

  if sy-subrc eq 0.
    exit.
  endif.

  refresh it_zfit0070.

  select *
    from zfit0065
    into table it_zfit0065
    where cod_oper   =  'V' " SWAP V
    and   dt_fim_cto gt wg_cadpro-dt_fechamento
    "AND   DT_INICIO_CTO LE WG_CADPRO-DT_FECHAMENTO
    and   bukrs      eq wg_cadpro-bukrs.


  loop at it_zfit0065 into wa_zfit0065.
    wa_zfit0070-dt_fechamento     = wg_cadpro-dt_fechamento.
    wa_zfit0070-bukrs             = wa_zfit0065-bukrs.
    wa_zfit0070-nro_cto           = wa_zfit0065-nro_cto.
    wa_zfit0070-nro_par           = wa_zfit0065-nro_par.
    wa_zfit0070-cod_oper          = wa_zfit0065-cod_oper.
    wa_zfit0070-banco             = wa_zfit0065-banco.
    wa_zfit0070-dt_inicio_cto	    =	wa_zfit0065-dt_inicio_cto.
    wa_zfit0070-dt_fim_cto        = wa_zfit0065-dt_fim_cto.
    wa_zfit0070-vlr_operacao_int  = wa_zfit0065-vlr_cto_r.
    wa_zfit0070-vlr_operacao      = wa_zfit0065-vlr_cto_us.
    wa_zfit0070-vlr_base_vc       = wa_zfit0065-vlr_parc.
    wa_zfit0070-moeda             = wa_zfit0065-moeda.
    wa_zfit0070-tp_tx_ativa       = wa_zfit0065-tp_tx_ativa.
    wa_zfit0070-tx_cambio_in_op	  =	wa_zfit0065-tx_cambio_in_op.
    wa_zfit0070-tp_tx_passiva     = wa_zfit0065-tp_tx_passiva.
    wa_zfit0070-tx_jros_passiva	  =	wa_zfit0065-tx_jros_passiva.
    wa_zfit0070-tx_jrs_alem_cdi   = wa_zfit0065-tx_acima_ind.
    wa_zfit0070-index_ativo       = wa_zfit0065-index_ativo.
    wa_zfit0070-tx_perct_cdi      = wa_zfit0065-tx_index_ativo.
    wa_zfit0070-tx_data_base      = wg_cadpro-ptax.

    if wa_zfit0070-vlr_operacao_int eq 0.
      wa_zfit0070-vlr_operacao_int  = ( wa_zfit0070-vlr_operacao * wa_zfit0070-tx_cambio_in_op ).
    endif.

    wa_zfit0070-dias_corridos_01 = wa_zfit0070-dt_fim_cto - wa_zfit0070-dt_inicio_cto.
    perform calc_dias_uteis using wa_zfit0070-dt_inicio_cto wa_zfit0070-dt_fim_cto changing v_dias_uteis.
    wa_zfit0070-dias_uteis_01 = v_dias_uteis.

    wa_zfit0070-dias_corridos_02  = wg_cadpro-dt_fechamento - wa_zfit0070-dt_inicio_cto.
    if wa_zfit0070-dias_corridos_02 gt 0.
      perform calc_dias_uteis using wa_zfit0070-dt_inicio_cto wg_cadpro-dt_fechamento changing v_dias_uteis.
    else.
      perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0070-dt_inicio_cto changing v_dias_uteis.
      v_dias_uteis = v_dias_uteis * -1.
    endif.
    wa_zfit0070-dias_uteis_02 = v_dias_uteis.

    wa_zfit0070-dias_corridos_03  = wa_zfit0070-dt_fim_cto - wg_cadpro-dt_fechamento.
    perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0070-dt_fim_cto  changing v_dias_uteis.
    wa_zfit0070-dias_uteis_03 = v_dias_uteis.

    perform f_calculo_ft_acum using wa_zfit0070-dt_inicio_cto wa_zfit0070-tx_perct_cdi   changing v_ft_acum_indev.

    wa_zfit0070-ft_acum_indev = v_ft_acum_indev.
    "
    "interpolação base 252
    if wa_zfit0070-dt_inicio_cto ne ''.
      read table it_zfit0062 into wa_zfit0062
       with key bukrs         = wg_cadpro-bukrs
                dt_fechamento = wg_cadpro-dt_fechamento
                dt_vcto_cto   = wa_zfit0070-dt_fim_cto.
      if sy-subrc = 0.
        vseqa = wa_zfit0062-seq - 1.
        vseqd = wa_zfit0062-seq + 1.
        "
        read table it_zfit0060 into wa_zfit0060 with key seq = vseqa.
        if sy-subrc ne 0.
          message 'Não encontrou sequencia anterior' type 'I'.
          exit.
        endif.

        vtx_proj_ano_bana = wa_zfit0060-tx_proj_ano_ban.

        read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062-seq.
        vtx_cupom_camba   = wa_zfit0060-tx_cupom_camb.
        "
        read table it_zfit0060 into wa_zfit0060 with key seq = vseqd.
        if sy-subrc ne 0.
          message 'Não encontrou sequencia posterior' type 'I'.
          exit.
        endif.
        vtx_proj_ano_band = wa_zfit0060-tx_proj_ano_ban.
        vtx_cupom_cambd   = wa_zfit0060-tx_cupom_camb.
        "
        if vtx_proj_ano_bana = vtx_proj_ano_band.
          vtx_com_interp2 = vtx_proj_ano_band.
        else.
          subtract 1 from  wa_zfit0062-dias_u_pri_inter.
          vtx_com_interp2 = vtx_proj_ano_bana + ( ( wa_zfit0062-dias_u_mtm_cto - wa_zfit0062-dias_u_pri_inter ) / ( ( wa_zfit0062-dias_u_seq_inter - wa_zfit0062-dias_u_pri_inter ) / ( vtx_proj_ano_band - vtx_proj_ano_bana ) ) ) .
        endif.

        if vtx_cupom_cambd gt 0 and vtx_cupom_camba  gt 0.
          try .
              vtx_com_interp3 = vtx_cupom_camba + ( ( wa_zfit0062-dias_c_mtm_cto - wa_zfit0062-dias_c_pri_inter ) /  ( ( wa_zfit0062-dias_c_seq_inter - wa_zfit0062-dias_c_pri_inter )  / ( vtx_cupom_cambd - vtx_cupom_camba ) ) ).
            catch cx_sy_zerodivide.
          endtry.
        else.
          vtx_com_interp3 = 0.
        endif.
      endif.
    endif.

    wa_zfit0070-ft_multp_ativa = ( 1 + ( wa_zfit0070-tx_jrs_alem_cdi / 100 ) ) ** ( wa_zfit0070-dias_uteis_01 / 252 ). "ALRS
    wa_zfit0070-ft_multp_ativa  = wa_zfit0070-ft_multp_ativa  / ( ( 1 +  ( vtx_com_interp2 / 100 ) ) ** ( wa_zfit0070-dias_uteis_03 / 252 ) ).

    wa_zfit0070-vlr_curva_ativa	= wa_zfit0070-vlr_operacao_int * wa_zfit0070-ft_multp_ativa.
    "

    wa_zfit0070-tx_comp_dolar_pa  = wa_zfit0070-tx_data_base / wa_zfit0070-tx_cambio_in_op.

    wa_zfit0070-ft_atual_passiva  = ( ( wa_zfit0070-tx_jros_passiva / 100 ) * wa_zfit0070-dias_corridos_02 / 360 ) + 1.


    wa_zfit0070-vlr_curva_passiv  = wa_zfit0070-vlr_operacao * wg_cadpro-ptax  * ( 1 + ( wa_zfit0070-tx_jros_passiva / 100 ) * ( wa_zfit0070-dias_corridos_01 / 360 ) ) / ( 1 + ( vtx_com_interp3 / 100 ) * wa_zfit0070-dias_corridos_03 / 360  ).

    if wa_zfit0070-dt_inicio_cto ne ''.
      read table it_zfit0062 into wa_zfit0062
       with key bukrs         = wg_cadpro-bukrs
                dt_fechamento = wg_cadpro-dt_fechamento
                dt_vcto_cto   = wa_zfit0070-dt_fim_cto.
      if sy-subrc = 0.
        wa_zfit0070-proj_cdi_inter   = wa_zfit0062-tx_bco_interp.
        wa_zfit0070-tx_desc_cdi	     = wa_zfit0062-tx_bco_interp.
        wa_zfit0070-tx_proj_interp   = wa_zfit0062-tx_cb_interp.
      endif.
    endif.

    wa_zfit0070-ind_proj_cdi_int 	= ( 1 + ( wa_zfit0070-tx_jrs_alem_cdi / 100 ) ) ** ( wa_zfit0070-dias_uteis_03 / 252 ).

    wa_zfit0070-vlr_proj_jros	  = wa_zfit0070-vlr_operacao_int * ( wa_zfit0070-proj_cdi_inter / 100 ) * wa_zfit0070-ind_proj_cdi_int.

    if ( wa_zfit0070-vlr_curva_ativa ne 0 ).
      wa_zfit0070-vlr_proj_jros   = wa_zfit0070-vlr_proj_jros + ( wa_zfit0070-vlr_curva_ativa -  wa_zfit0070-vlr_operacao_int ).
    endif.

    wa_zfit0070-vlr_mtm_p_ativ  = wa_zfit0070-vlr_proj_jros / ( 1 + ( wa_zfit0070-tx_desc_cdi / 100 ) ).

    wa_zfit0070-vlr_var_proj    = wa_zfit0070-tx_proj_interp / wa_zfit0070-tx_cambio_in_op.

    wa_zfit0070-vlr_var_cambial	= wa_zfit0070-vlr_base_vc * ( wa_zfit0070-tx_proj_interp - wa_zfit0070-tx_cambio_in_op ).

    wa_zfit0070-vlr_juros_fut	  = wa_zfit0070-vlr_operacao * wa_zfit0070-tx_proj_interp * ( wa_zfit0070-tx_jros_passiva / 100 ) * wa_zfit0070-dias_corridos_01 / 360.

    wa_zfit0070-tx_desc	= wa_zfit0070-tx_desc_cdi.

    wa_zfit0070-vlr_mtm_p_passiv  = ( wa_zfit0070-vlr_var_cambial + wa_zfit0070-vlr_juros_fut ) / ( 1 + ( wa_zfit0070-tx_desc / 100 ) ).

*    WA_ZFIT0070-VLR_AJ_MERC  = WA_ZFIT0070-VLR_MTM_P_ATIV - WA_ZFIT0070-VLR_MTM_P_PASSIV.
    wa_zfit0070-vlr_aj_merc  = wa_zfit0070-vlr_curva_ativa - wa_zfit0070-vlr_curva_passiv.

    if wa_zfit0070-vlr_aj_merc > 0.
      wa_zfit0070-tp_ajuste = 'ATIVO'.
    else.
      wa_zfit0070-tp_ajuste = 'PASSIVO'.
    endif.

*** PBI - 73761 - Inicio - CBRAND
    if wa_zfit0070-dias_corridos_03  > 365 and wa_zfit0070-vlr_aj_merc gt 0 .
      wa_zfit0070-tp_ajuste = 'ATIVOLP'.
    else.
      if wa_zfit0070-dias_corridos_03 > 365 and wa_zfit0070-vlr_aj_merc le 0 .
        wa_zfit0070-tp_ajuste = 'PASSIVOLP'.
      endif.
    endif.
*** PBI - 73761 - Inicio - CBRAND

    move:
    sy-uname                 to wa_zfit0070-usnam,
    sy-datum                 to wa_zfit0070-data_atual,
    sy-uzeit                 to wa_zfit0070-hora_atual.
    append wa_zfit0070 to it_zfit0070.
    clear wa_zfit0070.

  endloop.

  refresh tg_itens7.
  loop at it_zfit0070 into wa_zfit0070.
    move-corresponding wa_zfit0070 to tg_itens7.
    tg_itens7-ano = tg_itens7-dt_fim_cto+0(4).
    append tg_itens7.
  endloop.

  if it_zfit0070[] is initial.
    message s000(zwrm001) display like 'E' with 'Não há dados SWAP Fluxo C. T.V.!'.
  else.
    message s836(sd) with 'Cálculo SWAP V'
                       ',Realizado com sucesso!'.
  endif.

endform.                    "F_CALCULO_SWAP_V

form f_calculo_swap_v_atu.
  data: vtx_proj_ano_bana  type zfit0060-tx_proj_ano_ban,
        vtx_proj_ano_band  type zfit0060-tx_proj_ano_ban,
        vtx_cupom_camba    type zfit0060-tx_cupom_camb,
        vtx_cupom_cambd    type zfit0060-tx_cupom_camb,
        it_zfit0059_aux    type table of zfit0059,
        vnro_par           type zfit0070-nro_par,
        vg_data_realizacao type zfit0059-data_realizacao,

        vtx_com_interp2    type ztx_bpm,
        vtx_com_interp3    type ztx_bpm,
        vseqa              type zfit0062-seq,
        vseqd              type zfit0062-seq.

  loop at it_zfit0070 into wa_zfit0070.
    if wa_zfit0070-obj_key is not initial.
      vobj_key = wa_zfit0070-obj_key.
    endif.
  endloop.
  " Verifica se já foi contabilizado, neste caso não contabiliza este calculo - fazer estorno antes
  select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

  if sy-subrc eq 0.
    exit.
  endif.

  refresh: it_zfit0065, it_zfit0059.

  select *
    from zfit0059
    into table it_zfit0059
    where mdo_tipo  = 'V' " SWAP S -
    and   data_vencimento gt wg_cadpro-dt_fechamento
    and   pfj_codigo      eq wg_cadpro-bukrs
    and   regra           ne ''
    and   tx_dolar_in_op  gt 0. "ver com carlos


  it_zfit0059_aux[] = it_zfit0059[].
  sort it_zfit0059_aux by opr_numero .
  sort it_zfit0059     by opr_numero data_vencimento.
  delete adjacent duplicates from it_zfit0059_aux comparing opr_numero.
  loop at it_zfit0059_aux  into data(wa_zfit0059_aux).
    vnro_par = 0.
    clear vg_data_realizacao.
    loop at it_zfit0059  into wa_zfit0059 where opr_numero = wa_zfit0059_aux-opr_numero.
      add 1 to vnro_par.
      if vg_data_realizacao is initial.
        call function 'CCM_GO_BACK_MONTHS'
          exporting
            currdate   = wa_zfit0059-data_realizacao
            backmonths = 1
          importing
            newdate    = vg_data_realizacao.
      endif.
      wa_zfit0065-bukrs               = wa_zfit0059-pfj_codigo.
      wa_zfit0065-nro_cto             = wa_zfit0059-opr_numero.
      wa_zfit0065-nro_par             = vnro_par.
      wa_zfit0065-cod_oper            = 'S'.
      wa_zfit0065-banco               = wa_zfit0059-pfj_agente.
      if wa_zfit0059-tipo_operacao       = 'JUR'.
        wa_zfit0065-posicao    = 'J'.
      else.
        wa_zfit0065-posicao    = 'F'.
      endif.
      wa_zfit0065-natureza_cto        = ''.
      wa_zfit0065-dt_inicio_cto       = wa_zfit0059-data_realizacao.
      wa_zfit0065-dt_fim_cto          = wa_zfit0059-data_vencimento.
      wa_zfit0065-vlr_cto_r           = wa_zfit0059-valor_operacao * wa_zfit0059-tx_dolar_in_op.
      wa_zfit0065-vlr_cto_us          = wa_zfit0059-valor_operacao .
      wa_zfit0065-vlr_parc            = wa_zfit0059-valor_operacao.
      wa_zfit0065-moeda               = wa_zfit0059-moeda.
      wa_zfit0065-tx_cambio_fut       = 0.                               "Não encontrado
      wa_zfit0065-tp_tx_ativa         = '2'.
      wa_zfit0065-tx_cambio_in_op     = wa_zfit0059-tx_dolar_in_op.
      wa_zfit0065-tx_jros_ativa       = wa_zfit0059-index_ativo.
      wa_zfit0065-tp_tx_passiva       = '2'.
      wa_zfit0065-tx_jros_passiva     = wa_zfit0059-index_passivo.
      wa_zfit0065-tx_acima_ind        = 0.                              "Não encontrado
      wa_zfit0065-index_passivo       = wa_zfit0059-ind_codigo_passivo.
      wa_zfit0065-tx_index_passivo    = wa_zfit0059-tx_ind_passivo.
      wa_zfit0065-tx_ac_index_pass    = 0.                              "Não encontrado
      wa_zfit0065-index_ativo         = wa_zfit0059-ind_codigo_ativo.
      wa_zfit0065-tx_index_ativo      = wa_zfit0059-tx_ind_ativo.
      wa_zfit0065-taxa_inicial        = wa_zfit0059-tx_dolar_in_op.
      append wa_zfit0065 to it_zfit0065.
      clear wa_zfit0065.
      vg_data_realizacao = wa_zfit0059-data_realizacao.
    endloop.
  endloop.

  loop at it_zfit0065 into wa_zfit0065.
    wa_zfit0070-dt_fechamento     = wg_cadpro-dt_fechamento.
    wa_zfit0070-bukrs             = wa_zfit0065-bukrs.
    wa_zfit0070-nro_cto           = wa_zfit0065-nro_cto.
    wa_zfit0070-nro_par           = wa_zfit0065-nro_par.
    wa_zfit0070-cod_oper          = 'O'.
    wa_zfit0070-banco             = wa_zfit0065-banco.
    wa_zfit0070-dt_inicio_cto	    =	wa_zfit0065-dt_inicio_cto.
    wa_zfit0070-dt_fim_cto        = wa_zfit0065-dt_fim_cto.
    wa_zfit0070-vlr_operacao_int  = wa_zfit0065-vlr_cto_r.
    wa_zfit0070-vlr_operacao      = wa_zfit0065-vlr_cto_us.
    wa_zfit0070-vlr_base_vc       = wa_zfit0065-vlr_parc.
    wa_zfit0070-moeda             = wa_zfit0065-moeda.
    wa_zfit0070-tp_tx_ativa       = wa_zfit0065-tp_tx_ativa.
    wa_zfit0070-tx_cambio_in_op	  =	wa_zfit0065-tx_cambio_in_op.
    wa_zfit0070-tp_tx_passiva     = wa_zfit0065-tp_tx_passiva.
    wa_zfit0070-tx_jros_passiva	  =	wa_zfit0065-tx_jros_passiva.
    wa_zfit0070-tx_jrs_alem_cdi   = wa_zfit0065-tx_acima_ind.
    wa_zfit0070-index_ativo       = wa_zfit0065-index_ativo.
    wa_zfit0070-tx_perct_cdi      = wa_zfit0065-tx_index_ativo.
    wa_zfit0070-tx_data_base      = wg_cadpro-ptax.

    if wa_zfit0070-vlr_operacao_int eq 0.
      wa_zfit0070-vlr_operacao_int  = ( wa_zfit0070-vlr_operacao * wa_zfit0070-tx_cambio_in_op ).
    endif.

    wa_zfit0070-dias_corridos_01 = wa_zfit0070-dt_fim_cto - wa_zfit0070-dt_inicio_cto.
    perform calc_dias_uteis using wa_zfit0070-dt_inicio_cto wa_zfit0070-dt_fim_cto changing v_dias_uteis.
    wa_zfit0070-dias_uteis_01 = v_dias_uteis.

    wa_zfit0070-dias_corridos_02  = wg_cadpro-dt_fechamento - wa_zfit0070-dt_inicio_cto.
    if wa_zfit0070-dias_corridos_02 gt 0.
      perform calc_dias_uteis using wa_zfit0070-dt_inicio_cto wg_cadpro-dt_fechamento changing v_dias_uteis.
    else.
      perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0070-dt_inicio_cto changing v_dias_uteis.
      v_dias_uteis = v_dias_uteis * -1.
    endif.
    wa_zfit0070-dias_uteis_02 = v_dias_uteis.

    wa_zfit0070-dias_corridos_03  = wa_zfit0070-dt_fim_cto - wg_cadpro-dt_fechamento.
    perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0070-dt_fim_cto  changing v_dias_uteis.
    wa_zfit0070-dias_uteis_03 = v_dias_uteis.

    perform f_calculo_ft_acum using wa_zfit0070-dt_inicio_cto wa_zfit0070-tx_perct_cdi   changing v_ft_acum_indev.

    wa_zfit0070-ft_acum_indev = v_ft_acum_indev.
    "

    data: v_moeda_comparada  type zfator_desc,
          v_ft_multp_passiva type zfator_desc,
          v_desc_cdi         type zfator_desc.

    if wa_zfit0070-dt_inicio_cto ne ''.
      read table it_zfit0062 into wa_zfit0062
       with key bukrs         = wg_cadpro-bukrs
                dt_fechamento = wg_cadpro-dt_fechamento
                dt_vcto_cto   = wa_zfit0070-dt_fim_cto.
      if sy-subrc = 0.
        wa_zfit0070-tx_desc_cdi = wa_zfit0062-tx_com_t1m.
      endif.
    endif.

    "ponta ativa
    wa_zfit0070-ft_multp_ativa  = ( ( ( 1 + ( wa_zfit0070-tx_perct_cdi / 100 ) ) ** ( wa_zfit0070-dias_corridos_03 / 360 ) ) - 1 ) + 1. "Taxa no período

    wa_zfit0070-vlr_curva_ativa	= wa_zfit0070-vlr_operacao_int * wa_zfit0070-ft_multp_ativa.

    wa_zfit0070-vlr_mtm_p_ativ  = wa_zfit0070-vlr_curva_ativa  / ( 1 +  ( wa_zfit0070-tx_desc_cdi / 100 )  ).
    "
    "ponta passiva
    v_moeda_comparada = wa_zfit0062-tx_cb_interp / wa_zfit0065-taxa_inicial.
    wa_zfit0070-p_moeda_comp = wa_zfit0062-tx_cb_interp / wa_zfit0065-taxa_inicial.

    v_ft_multp_passiva            = ( ( ( ( wa_zfit0065-tx_index_passivo / 100 ) / 360 ) * wa_zfit0070-dias_corridos_03 ) + 1 ). "tx Adicional
    wa_zfit0070-tx_index_passivo  = wa_zfit0065-tx_index_passivo.
    wa_zfit0070-ft_atual_passiva  = v_ft_multp_passiva.


    wa_zfit0070-vlr_curva_passiv  = wa_zfit0070-vlr_operacao_int * v_ft_multp_passiva * v_moeda_comparada.

    wa_zfit0070-tx_desc	= wa_zfit0070-tx_desc_cdi.

    wa_zfit0070-vlr_mtm_p_passiv  = wa_zfit0070-vlr_curva_passiv  / ( 1 + ( wa_zfit0070-tx_desc_cdi / 100 ) ).

    wa_zfit0070-vlr_aj_merc  = wa_zfit0070-vlr_mtm_p_ativ  - wa_zfit0070-vlr_mtm_p_passiv.

    if wa_zfit0070-vlr_aj_merc > 0.
      wa_zfit0070-tp_ajuste = 'ATIVO'.
    else.
      wa_zfit0070-tp_ajuste = 'PASSIVO'.
    endif.

*** PBI - 73761 - Inicio - CBRAND
    if wa_zfit0070-dias_corridos_03  > 365 and wa_zfit0070-vlr_aj_merc gt 0 .
      wa_zfit0070-tp_ajuste = 'ATIVOLP'.
    else.
      if wa_zfit0070-dias_corridos_03 > 365 and wa_zfit0070-vlr_aj_merc le 0 .
        wa_zfit0070-tp_ajuste = 'PASSIVOLP'.
      endif.
    endif.
*** PBI - 73761 - Inicio - CBRAND

    move:
    sy-uname                 to wa_zfit0070-usnam,
    sy-datum                 to wa_zfit0070-data_atual,
    sy-uzeit                 to wa_zfit0070-hora_atual.
    append wa_zfit0070 to it_zfit0070.
    clear wa_zfit0070.

  endloop.

  select *
       from zfit0066
       into table it_zfit0066
       where  cod_oper      eq 'V'.
  sort it_zfit0066 by tp_operacao tp_ajuste.

  refresh tg_itens7.
  loop at it_zfit0070 into wa_zfit0070.
    move-corresponding wa_zfit0070 to tg_itens7.
    tg_itens7-ano = tg_itens7-dt_fim_cto+0(4).
    loop at it_zfit0066 into wa_zfit0066 where tp_ajuste   = wa_zfit0070-tp_ajuste.
      if wa_zfit0066-bschl = '40'.
        tg_itens7-hkont_d = wa_zfit0066-hkont.
      else.
        tg_itens7-hkont_c = wa_zfit0066-hkont.
      endif.
    endloop.
    append tg_itens7.
  endloop.

  if it_zfit0070[] is initial.
    message s000(zwrm001) display like 'E' with 'Não há dados SWAP Fluxo C. T.V.!'.
  else.
    message s836(sd) with 'Cálculo SWAP V'
                       ',Realizado com sucesso!'.
  endif.

endform.                    "F_CALCULO_SWAP_V


*&---------------------------------------------------------------------*
*&      Form  f_calculo_SWAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_calculo_swap_s.

  data: vnro_par        type zfit0065-nro_par,
        vvlr_cto_us     type zfit0065-vlr_cto_us,
        vvlr_cto_r      type zfit0065-vlr_cto_r,
        it_zfit0059_aux type table of zfit0059,
        vg_vlr_aj_merc  type zfit0067-vlr_aj_merc.

  loop at it_zfit0067 into wa_zfit0067.
    if wa_zfit0067-obj_key is not initial.
      vobj_key = wa_zfit0067-obj_key.
    endif.
  endloop.
  " Verifica se já foi contabilizado, neste caso não contabiliza este calculo - fazer estorno antes
  select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

  if sy-subrc eq 0.
    exit.
  endif.

  refresh it_zfit0067.

  select *
    from zfit0065
    into table it_zfit0065
    where cod_oper   =  'S' " SWAP S
    and   dt_fim_cto gt wg_cadpro-dt_fechamento
    "AND   DT_INICIO_CTO LE WG_CADPRO-DT_FECHAMENTO
    and   bukrs      eq wg_cadpro-bukrs.

  "converter FREE_SCH para CTF
  "US 150369 operações FREE_SCHED não tem restrição porque vira do XRT como CTF ao inves de VANILA
  refresh it_zfit0059.
  select *
   from zfit0059
   into table it_zfit0059
   where mdo_tipo  = 'S'
   and   data_vencimento gt  wg_cadpro-dt_fechamento
   and   data_realizacao le  wg_cadpro-dt_fechamento
   and   pfj_codigo      eq wg_cadpro-bukrs
   and   regra           ne ''
   and   valor_operacao  ne 0.

  it_zfit0059_aux[] = it_zfit0059[].
  sort it_zfit0059_aux by opr_numero.
  delete adjacent duplicates from it_zfit0059_aux comparing opr_numero.
  loop at it_zfit0059_aux  into data(wa_zfit0059_aux).
    vnro_par = 0.
    vvlr_cto_us = 0.
    vvlr_cto_r = 0.
    loop at it_zfit0059  into wa_zfit0059 where opr_numero = wa_zfit0059_aux-opr_numero.
      add wa_zfit0059-valor_operacao  to vvlr_cto_us .
      wa_zfit0065-vlr_cto_r          = wa_zfit0059-valor_operacao * wa_zfit0059-tx_dolar_in_op.
      add wa_zfit0065-vlr_cto_r       to vvlr_cto_r.
    endloop.
    loop at it_zfit0059  into wa_zfit0059 where opr_numero = wa_zfit0059_aux-opr_numero.
      add 1 to vnro_par.
      wa_zfit0065-bukrs               = wa_zfit0059-pfj_codigo.
      wa_zfit0065-nro_cto             = wa_zfit0059-opr_numero.
      wa_zfit0065-nro_par             = vnro_par.
      wa_zfit0065-cod_oper            = 'S'.
      wa_zfit0065-banco               = wa_zfit0059-pfj_agente.
      wa_zfit0065-posicao             = 'F'.
      if wa_zfit0065-posicao = 'F'.
        wa_zfit0067-tp_posicao    = 'FINANCEIRO'.
      else.
        wa_zfit0067-tp_posicao    = 'OPERACIONAL'.
      endif.
      wa_zfit0065-natureza_cto        = ''.
      wa_zfit0065-posicao = 'O'.
      wa_zfit0065-dt_inicio_cto       = wa_zfit0059-data_realizacao.
      wa_zfit0065-dt_fim_cto          = wa_zfit0059-data_vencimento.

      if wa_zfit0059-moeda = 'BRL'.
        wa_zfit0065-vlr_cto_r           = wa_zfit0059-valor_operacao. "vvlr_cto_r.
      else.
        wa_zfit0065-vlr_cto_us          = wa_zfit0059-valor_operacao. "vvlr_cto_us.
      endif.
      wa_zfit0065-vlr_parc            = wa_zfit0059-valor_operacao.

      wa_zfit0065-moeda               = wa_zfit0059-moeda.
      wa_zfit0065-tx_cambio_fut       = 0.                               "Não encontrado
      wa_zfit0065-tp_tx_ativa         = '2'.
      wa_zfit0065-tx_cambio_in_op     = wa_zfit0059-tx_dolar_in_op.
      wa_zfit0065-tx_jros_ativa       = wa_zfit0059-index_ativo.
      wa_zfit0065-tp_tx_passiva       = '2'.
      wa_zfit0065-tx_jros_passiva     = wa_zfit0059-index_passivo.
      wa_zfit0065-tx_acima_ind        = 0.                              "Não encontrado
      wa_zfit0065-index_passivo       = wa_zfit0059-ind_codigo_passivo.
      wa_zfit0065-tx_index_passivo    = wa_zfit0059-tx_ind_passivo.
      wa_zfit0065-tx_ac_index_pass    = 0.                              "Não encontrado
      wa_zfit0065-index_ativo         = wa_zfit0059-ind_codigo_ativo.
      wa_zfit0065-tx_index_ativo      = wa_zfit0059-tx_ind_ativo.
      append wa_zfit0065 to it_zfit0065.
      clear wa_zfit0065.
    endloop.


  endloop.
  "converter FREE_SCH para CTF
  "US 150369 operações FREE_SCHED não tem restrição porque vira do XRT como CTF ao inves de VANILA

  loop at it_zfit0065 into wa_zfit0065.
    wa_zfit0067-dt_fechamento     = wg_cadpro-dt_fechamento.
    wa_zfit0067-bukrs             = wa_zfit0065-bukrs.
    wa_zfit0067-nro_cto           = wa_zfit0065-nro_cto.
    wa_zfit0067-nro_par           = wa_zfit0065-nro_par.
    wa_zfit0067-cod_oper          = wa_zfit0065-cod_oper.
    wa_zfit0067-banco             = wa_zfit0065-banco.
    wa_zfit0067-tp_posicao        = wa_zfit0065-posicao.
    wa_zfit0067-natureza_cto      = wa_zfit0065-natureza_cto.
    if wa_zfit0065-posicao = 'F'.
      wa_zfit0067-tp_posicao    = 'FINANCEIRO'.
    else.
      wa_zfit0067-tp_posicao    = 'OPERACIONAL'.
    endif.
    wa_zfit0067-dt_inicio_cto	    =	wa_zfit0065-dt_inicio_cto.
    wa_zfit0067-dt_fim_cto        = wa_zfit0065-dt_fim_cto.
    wa_zfit0067-vlr_operacao_int  = wa_zfit0065-vlr_cto_r.
    wa_zfit0067-vlr_operacao      = wa_zfit0065-vlr_cto_us.
    wa_zfit0067-vlr_parc          = wa_zfit0065-vlr_parc.
    wa_zfit0067-moeda             = wa_zfit0065-moeda.
    wa_zfit0067-tp_tx_ativa       = wa_zfit0065-tp_tx_ativa.
    wa_zfit0067-tx_cambio_in_op	  =	wa_zfit0065-tx_cambio_in_op.
    wa_zfit0067-tx_jros_ativa     = wa_zfit0065-tx_jros_ativa.
    wa_zfit0067-tp_tx_passiva     = wa_zfit0065-tp_tx_passiva.
    wa_zfit0067-tx_jros_passiva	  = wa_zfit0065-tx_jros_passiva.

    if wa_zfit0067-vlr_operacao_int eq 0.
      wa_zfit0067-vlr_operacao_int  = ( wa_zfit0067-vlr_operacao * wa_zfit0067-tx_cambio_in_op ).
    endif.

    wa_zfit0067-dias_corridos_01  = wa_zfit0067-dt_fim_cto - wa_zfit0067-dt_inicio_cto.
    perform calc_dias_uteis using wa_zfit0067-dt_inicio_cto wa_zfit0067-dt_fim_cto changing v_dias_uteis.
    wa_zfit0067-dias_uteis_01 = v_dias_uteis.

    wa_zfit0067-dias_corridos_02  = wg_cadpro-dt_fechamento - wa_zfit0067-dt_inicio_cto.
    if wa_zfit0067-dias_corridos_02 gt 0.
      perform calc_dias_uteis using wa_zfit0067-dt_inicio_cto wg_cadpro-dt_fechamento changing v_dias_uteis.
    else.
      perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0067-dt_inicio_cto changing v_dias_uteis.
      v_dias_uteis = v_dias_uteis * -1.
    endif.
    wa_zfit0067-dias_uteis_02 = v_dias_uteis.

    wa_zfit0067-dias_corridos_03  = wa_zfit0067-dt_fim_cto - wg_cadpro-dt_fechamento.
    perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0067-dt_fim_cto  changing v_dias_uteis.
    wa_zfit0067-dias_uteis_03 = v_dias_uteis.

    if wa_zfit0067-tp_tx_ativa = 1.
      wa_zfit0067-vlr_aux_op_tp1 = ( ( wa_zfit0067-vlr_operacao_int * ( 1 + ( wa_zfit0067-tx_jros_ativa / 100 ) ) ) ** ( wa_zfit0067-dias_uteis_01 / 252 ) ) - wa_zfit0067-vlr_operacao_int.
      wa_zfit0067-vlr_proj_jros = wa_zfit0067-vlr_aux_op_tp1.
    endif.

    if wa_zfit0067-tp_tx_ativa = 2.
      wa_zfit0067-vlr_aux_op_tp2 = ( ( ( wa_zfit0067-vlr_operacao_int * ( wa_zfit0067-tx_jros_ativa / 100 ) ) * wa_zfit0067-dias_corridos_01 ) / 360 ).
      wa_zfit0067-vlr_proj_jros = wa_zfit0067-vlr_aux_op_tp2.
    elseif wa_zfit0067-tp_tx_ativa = 4.
      wa_zfit0067-vlr_aux_op_tp2 = ( ( ( wa_zfit0067-vlr_operacao_int * ( wa_zfit0067-tx_jros_ativa / 100 ) ) * wa_zfit0067-dias_corridos_01 ) / 365 ).
      wa_zfit0067-vlr_proj_jros = wa_zfit0067-vlr_aux_op_tp2.
    endif.

    if wa_zfit0067-tp_tx_ativa = 3.
      wa_zfit0067-vlr_aux_op_tp3 =  ( wa_zfit0067-vlr_operacao_int * ( 1 + ( wa_zfit0067-tx_jros_ativa / 100 ) ) ** ( wa_zfit0067-dias_corridos_01 / 360 ) )  - wa_zfit0067-vlr_operacao_int .
      wa_zfit0067-vlr_proj_jros = wa_zfit0067-vlr_aux_op_tp3 .
    elseif wa_zfit0067-tp_tx_ativa = 5.
      wa_zfit0067-vlr_aux_op_tp3 =  ( wa_zfit0067-vlr_operacao_int * ( 1 + ( wa_zfit0067-tx_jros_ativa / 100 ) ) ** ( wa_zfit0067-dias_corridos_01 / 365 ) )  - wa_zfit0067-vlr_operacao_int .
      wa_zfit0067-vlr_proj_jros = wa_zfit0067-vlr_aux_op_tp3 .
    endif.

    if wa_zfit0067-dt_inicio_cto ne ''.
      read table it_zfit0062 into wa_zfit0062
        with key bukrs      = wg_cadpro-bukrs
                 dt_fechamento = wg_cadpro-dt_fechamento
                 dt_vcto_cto   = wa_zfit0067-dt_fim_cto.
      if sy-subrc = 0.
        wa_zfit0067-tx_desc_cdi    = wa_zfit0062-tx_com_interp.
        wa_zfit0067-tx_proj_interp = wa_zfit0062-tx_cb_interp.
      endif.
    endif.
    wa_zfit0067-vlr_mtm_p_ativ = wa_zfit0067-vlr_proj_jros / ( 1 + ( wa_zfit0067-tx_desc_cdi / 100 ) ).


    wa_zfit0067-vlr_var_proj = wa_zfit0067-tx_proj_interp / wa_zfit0067-tx_cambio_in_op.

    wa_zfit0067-vlr_var_cambial = wa_zfit0067-vlr_parc * ( wa_zfit0067-tx_proj_interp - wa_zfit0067-tx_cambio_in_op ).

    if wa_zfit0067-tp_tx_passiva = 1 .
      "WA_ZFIT0067-VLR_JUROS_FUT = ( WA_ZFIT0067-VLR_OPERACAO * WA_ZFIT0067-TX_PROJ_INTERP   * ( WA_ZFIT0067-TX_JROS_PASSIVA / 100 ) * WA_ZFIT0067-DIAS_UTEIS_01 ) / 252.
      wa_zfit0067-vlr_juros_fut =  ( wa_zfit0067-vlr_operacao * wa_zfit0067-tx_proj_interp ) * ( ( 1 +  wa_zfit0067-tx_jros_passiva / 100 ) ** ( wa_zfit0067-dias_uteis_01 / 252 ) ) - ( wa_zfit0067-vlr_operacao * wa_zfit0067-tx_proj_interp ).

    else.
      wa_zfit0067-vlr_juros_fut = ( wa_zfit0067-vlr_operacao * wa_zfit0067-tx_proj_interp * ( wa_zfit0067-tx_jros_passiva / 100 ) * wa_zfit0067-dias_corridos_01 ) / 360.
    endif.

    wa_zfit0067-tx_desc = wa_zfit0067-tx_desc_cdi.

    wa_zfit0067-vlr_mtm_p_passiv = ( wa_zfit0067-vlr_var_cambial + wa_zfit0067-vlr_juros_fut ) / ( 1 + ( wa_zfit0067-tx_desc / 100 ) ).

    if wa_zfit0067-vlr_mtm_p_ativ ne ''.
      wa_zfit0067-vlr_aj_merc = ( wa_zfit0067-vlr_mtm_p_ativ - wa_zfit0067-vlr_mtm_p_passiv ).
    endif.

    vg_vlr_aj_merc = wa_zfit0067-vlr_aj_merc.

    clear wa_zfit0067-vlr_extrato_bco.
    select single vlr_extrato_bco
         from zfit0214
         into wa_zfit0067-vlr_extrato_bco
         where dt_fechamento = wg_cadpro-dt_fechamento
         and   bukrs         = wg_cadpro-bukrs
         and   nro_cto       = wa_zfit0067-nro_cto
         and   nro_par       = wa_zfit0067-nro_par
         and   cod_oper      = 'S'.

    if sy-subrc = 0.
      wa_zfit0067-vlr_aj_merc = wa_zfit0067-vlr_extrato_bco.
    endif.

    if wa_zfit0067-vlr_aj_merc > 0.
      wa_zfit0067-tp_ajuste = 'ATIVO'.
    else.
      wa_zfit0067-tp_ajuste = 'PASSIVO'.
    endif.

*** PBI - 73761 - Inicio - CBRAND
    if wa_zfit0067-dias_corridos_03  > 365 and wa_zfit0067-vlr_aj_merc gt 0 .
      wa_zfit0067-tp_ajuste = 'ATIVOLP'.
    else.
      if  wa_zfit0067-dias_corridos_03 > 365 and wa_zfit0067-vlr_aj_merc le 0 .
        wa_zfit0067-tp_ajuste = 'PASSIVOLP'.
      endif.
    endif.
*** PBI - 73761 - Fim - CBRAND

    wa_zfit0067-vlr_aj_merc = vg_vlr_aj_merc.

    move:
    sy-uname                 to wa_zfit0067-usnam,
    sy-datum                 to wa_zfit0067-data_atual,
    sy-uzeit                 to wa_zfit0067-hora_atual.

    append wa_zfit0067 to it_zfit0067.
    clear wa_zfit0067.

  endloop.


  refresh tg_itens5.
  loop at it_zfit0067 into wa_zfit0067.
    refresh tg_itens5-color.
    wl_color-fieldname = 'VLR_EXTRATO_BCO'.
    wl_color-color-col = 3.
    wl_color-color-inv = 3.
    append wl_color to tg_itens5-color.
    move-corresponding wa_zfit0067 to tg_itens5.
    tg_itens5-ano = tg_itens5-dt_fim_cto+0(4).
    append tg_itens5.
  endloop.

  if it_zfit0067[] is initial.
    message s000(zwrm001) display like 'E' with 'Não há dados SWAP Fluxo caixa!'.
  else.
    message s836(sd) with 'Cálculo SWAP S'
                       ',Realizado com sucesso!'.
  endif.


endform.                    "f_calculo_SWAP

form f_calculo_swap_s_atu.

  data: vnro_par           type zfit0065-nro_par,
        vvlr_cto_us        type zfit0065-vlr_cto_us,
        vvlr_cto_r         type zfit0065-vlr_cto_r,
        it_zfit0059_aux    type table of zfit0059,
        vg_vlr_aj_merc     type zfit0067-vlr_aj_merc,
        vg_data_realizacao type zfit0059-data_realizacao,
        vg_meses           type vtbbewe-atage,
        vg_backmonths      type  numc3.

  data: vtx_proj_ano_com  type ztx_par_dolar,
        vtx_proj_ano_como type ztx_par_dolar,
        vtx_proj_ano_coma type ztx_par_dolar,
        vtx_proj_ano_ban  type ztx_par_dolar,
        vtx_proj_ano_bano type ztx_par_dolar,
        vtx_proj_ano_bana type ztx_par_dolar,
        vg_dt_fim_cto     type zfit0067-dt_fim_cto,
        vg_seqa           type zfit0062-seq.

  loop at it_zfit0067 into wa_zfit0067.
    if wa_zfit0067-obj_key is not initial.
      vobj_key = wa_zfit0067-obj_key.
    endif.
  endloop.
  " Verifica se já foi contabilizado, neste caso não contabiliza este calculo - fazer estorno antes
  select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

  if sy-subrc eq 0.
    exit.
  endif.

  select *
      from zfit0066
      into table it_zfit0066
      where  cod_oper      eq 'S'.
  sort it_zfit0066 by tp_operacao tp_ajuste .

  refresh: it_zfit0059, it_zfit0065, it_zfit0067 .
  select *
   from zfit0059
   into table it_zfit0059
   where mdo_tipo  = 'S'
   and   data_vencimento gt  wg_cadpro-dt_fechamento
*   and   data_realizacao le  wg_cadpro-dt_fechamento
   and   pfj_codigo      eq wg_cadpro-bukrs
   and   regra           ne ''.

  it_zfit0059_aux[] = it_zfit0059[].
  sort it_zfit0059_aux by opr_numero.
  sort it_zfit0059     by opr_numero data_vencimento par_tipo.
  delete adjacent duplicates from it_zfit0059_aux comparing opr_numero.
  loop at it_zfit0059_aux  into data(wa_zfit0059_aux).
    vnro_par = 0.
    vvlr_cto_us = 0.
    vvlr_cto_r = 0.
    clear vg_data_realizacao.
    loop at it_zfit0059  into wa_zfit0059 where opr_numero = wa_zfit0059_aux-opr_numero.
      add wa_zfit0059-valor_operacao  to vvlr_cto_us .
      wa_zfit0065-vlr_cto_r          = wa_zfit0059-valor_operacao * wa_zfit0059-tx_dolar_in_op.
      add wa_zfit0065-vlr_cto_r       to vvlr_cto_r.
    endloop.
    loop at it_zfit0059  into wa_zfit0059 where opr_numero = wa_zfit0059_aux-opr_numero.
      if wa_zfit0059-par_tipo = 'JUR'.
        read table it_zfit0059  into data(wa_59) with key opr_numero       = wa_zfit0059-opr_numero
                                                          data_vencimento  = wa_zfit0059-data_vencimento
                                                          par_tipo         = 'PRI' binary search.
        if sy-subrc = 0.
          continue.
        endif.
      endif.

      add 1 to vnro_par.
      if vnro_par = 1.
        vg_data_realizacao = wa_zfit0059-data_realizacao.
      endif.
      wa_zfit0065-bukrs               = wa_zfit0059-pfj_codigo.
      wa_zfit0065-nro_cto             = wa_zfit0059-opr_numero.
      wa_zfit0065-nro_par             = vnro_par.
      wa_zfit0065-cod_oper            = 'S'.
      wa_zfit0065-banco               = wa_zfit0059-pfj_agente.
      if wa_zfit0059-tipo_operacao       = 'JUR'.
        wa_zfit0065-posicao    = 'J'.
      else.
        wa_zfit0065-posicao    = 'F'.
      endif.
      wa_zfit0065-natureza_cto        = ''.
      wa_zfit0065-dt_inicio_cto       = vg_data_realizacao.
      wa_zfit0065-dt_fim_cto          = wa_zfit0059-data_vencimento.
      wa_zfit0065-vlr_cto_r           = vvlr_cto_r.
      wa_zfit0065-vlr_cto_us          = vvlr_cto_us.
      wa_zfit0065-vlr_parc            = wa_zfit0059-valor_operacao.
      wa_zfit0065-moeda               = wa_zfit0059-moeda.
      wa_zfit0065-tx_cambio_fut       = 0.                               "Não encontrado
      wa_zfit0065-tp_tx_ativa         = '2'.
      wa_zfit0065-tx_cambio_in_op     = wa_zfit0059-tx_dolar_in_op.
      wa_zfit0065-tx_jros_ativa       = wa_zfit0059-index_ativo.
      wa_zfit0065-tp_tx_passiva       = '2'.
      wa_zfit0065-tx_jros_passiva     = wa_zfit0059-index_passivo.
      wa_zfit0065-tx_acima_ind        = 0.                              "Não encontrado
      wa_zfit0065-index_passivo       = wa_zfit0059-ind_codigo_passivo.
      wa_zfit0065-tx_index_passivo    = wa_zfit0059-tx_ind_passivo.
      wa_zfit0065-tx_ac_index_pass    = 0.                              "Não encontrado
      wa_zfit0065-index_ativo         = wa_zfit0059-ind_codigo_ativo.
      wa_zfit0065-tx_index_ativo      = wa_zfit0059-tx_ind_ativo.
      wa_zfit0065-taxa_inicial        = wa_zfit0059-tx_dolar_in_op.
      append wa_zfit0065 to it_zfit0065.
      clear wa_zfit0065.
      vg_data_realizacao = wa_zfit0059-data_vencimento.
    endloop.

  endloop.

  sort it_zfit0065 by nro_cto nro_par.

  loop at it_zfit0065 into wa_zfit0065.
    wa_zfit0067-dt_fechamento     = wg_cadpro-dt_fechamento.
    wa_zfit0067-bukrs             = wa_zfit0065-bukrs.
    wa_zfit0067-nro_cto           = wa_zfit0065-nro_cto.
    wa_zfit0067-nro_par           = wa_zfit0065-nro_par.
    wa_zfit0067-cod_oper          = wa_zfit0065-cod_oper.
    wa_zfit0067-banco             = wa_zfit0065-banco.
    wa_zfit0067-tp_posicao        = wa_zfit0065-posicao.
    wa_zfit0067-natureza_cto      = wa_zfit0065-natureza_cto.
    if wa_zfit0065-posicao = 'F'.
      wa_zfit0067-tp_posicao    = 'FINANCEIRO'.
    else.
      wa_zfit0067-tp_posicao    = 'JUROS'.
    endif.
    wa_zfit0067-dt_inicio_cto	    =	wa_zfit0065-dt_inicio_cto.
    wa_zfit0067-dt_fim_cto        = wa_zfit0065-dt_fim_cto.
    wa_zfit0067-vlr_operacao_int  = wa_zfit0065-vlr_cto_r.
    wa_zfit0067-vlr_operacao      = wa_zfit0065-vlr_cto_us.
    wa_zfit0067-vlr_parc          = wa_zfit0065-vlr_parc.
    wa_zfit0067-moeda             = wa_zfit0065-moeda.
    wa_zfit0067-tp_tx_ativa       = wa_zfit0065-tp_tx_ativa.
    wa_zfit0067-tx_index_ativo    = wa_zfit0065-tx_index_ativo.
    wa_zfit0067-tx_cambio_in_op	  =	wa_zfit0065-tx_cambio_in_op.
    wa_zfit0067-tx_jros_ativa     = wa_zfit0065-tx_jros_ativa.
    wa_zfit0067-tp_tx_passiva     = wa_zfit0065-tp_tx_passiva.
    wa_zfit0067-tx_index_passivo  = wa_zfit0065-tx_index_passivo.
    wa_zfit0067-tx_jros_passiva	  = wa_zfit0065-tx_jros_passiva.

    if wa_zfit0067-vlr_operacao_int eq 0.
      wa_zfit0067-vlr_operacao_int  = ( wa_zfit0067-vlr_operacao * wa_zfit0067-tx_cambio_in_op ).
    endif.

    wa_zfit0067-dias_corridos_01  = wa_zfit0067-dt_fim_cto - wa_zfit0067-dt_inicio_cto.
    perform calc_dias_uteis using wa_zfit0067-dt_inicio_cto wa_zfit0067-dt_fim_cto changing v_dias_uteis.
    wa_zfit0067-dias_uteis_01 = v_dias_uteis.

    wa_zfit0067-dias_corridos_02  = wg_cadpro-dt_fechamento - wa_zfit0067-dt_inicio_cto.
    if wa_zfit0067-dias_corridos_02 gt 0.
      perform calc_dias_uteis using wa_zfit0067-dt_inicio_cto wg_cadpro-dt_fechamento changing v_dias_uteis.
    else.
      perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0067-dt_inicio_cto changing v_dias_uteis.
      v_dias_uteis = v_dias_uteis * -1.
    endif.
    wa_zfit0067-dias_uteis_02 = v_dias_uteis.

    wa_zfit0067-dias_corridos_03  = wa_zfit0067-dt_fim_cto - wg_cadpro-dt_fechamento.
    perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0067-dt_fim_cto  changing v_dias_uteis.
    wa_zfit0067-dias_uteis_03 = v_dias_uteis.


    clear v_ft_acum_indev.
    if wa_zfit0065-index_passivo = 'CDI'.
*      perform f_calculo_ft_acum using wa_zfit0067-dt_inicio_cto wa_zfit0065-tx_index_passivo   changing v_ft_acum_indev.
      perform f_calculo_ft_acum using wa_zfit0067-dt_inicio_cto wa_zfit0065-tx_jros_passiva   changing v_ft_acum_indev.
    elseif   wa_zfit0065-index_ativo = 'CDI'.
*      perform f_calculo_ft_acum using wa_zfit0067-dt_inicio_cto wa_zfit0065-tx_index_ativo   changing v_ft_acum_indev.
      perform f_calculo_ft_acum using wa_zfit0067-dt_inicio_cto wa_zfit0065-tx_jros_ativa   changing v_ft_acum_indev.
    endif.

    data: v_fator         type zfator_desc,
          v_desc_cdi      type zfator_desc,
          v_amortiz       type zfit0067-vlr_juros_fut,
          v_amortiz_total type zfit0067-vlr_juros_fut.

    clear: v_amortiz,v_amortiz_total.
    loop at it_zfit0065 into data(_zfit0065) where nro_cto = wa_zfit0067-nro_cto.
      if _zfit0065-nro_par lt wa_zfit0067-nro_par.
        v_amortiz = _zfit0065-vlr_parc * _zfit0065-tx_cambio_in_op.
        vg_dt_fim_cto = _zfit0065-dt_fim_cto.
        add v_amortiz to v_amortiz_total.
      elseif  _zfit0065-nro_par eq wa_zfit0067-nro_par.
        v_amortiz = _zfit0065-vlr_parc * wa_zfit0065-tx_cambio_in_op.
        exit.
      endif.
    endloop.
    subtract v_amortiz_total from wa_zfit0067-vlr_operacao_int.
    wa_zfit0067-vlr_operacao  = ( wa_zfit0067-vlr_operacao_int / wa_zfit0067-tx_cambio_in_op ).

    if wa_zfit0067-dt_inicio_cto ne ''.
      read table it_zfit0062 into wa_zfit0062
        with key bukrs      = wg_cadpro-bukrs
                 dt_fechamento = wg_cadpro-dt_fechamento
                 dt_vcto_cto   = wa_zfit0067-dt_fim_cto.
      if sy-subrc = 0.
        clear wa_zfit0060.
        read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062-seq.
        vtx_proj_ano_bano = wa_zfit0060-tx_proj_ano_ban.
        vtx_proj_ano_como = wa_zfit0060-tx_proj_ano_com.

        read table it_zfit0060 into wa_zfit0060 with key dias_corridos = wa_zfit0062-dias_c_mtm_cto.
        if sy-subrc = 0.
          vtx_proj_ano_com = wa_zfit0060-tx_proj_ano_com.
          vtx_proj_ano_ban = wa_zfit0060-tx_proj_ano_ban.
        else.
          vg_seqa = wa_zfit0062-seq + 1.
          read table it_zfit0060 into wa_zfit0060 with key seq = vg_seqa.
          vtx_proj_ano_com = wa_zfit0060-tx_proj_ano_com.
          vtx_proj_ano_ban = wa_zfit0060-tx_proj_ano_ban.
        endif.
        if vtx_proj_ano_bano ne vtx_proj_ano_ban.
          vtx_proj_ano_bana = ( ( vtx_proj_ano_bano / 100 ) + ( ( ( vtx_proj_ano_ban / 100 ) - ( vtx_proj_ano_bano / 100 ) ) * ( ( wa_zfit0062-dias_u_mtm_cto - wa_zfit0062-dias_u_pri_inter ) /
                             ( wa_zfit0062-dias_u_seq_inter - wa_zfit0062-dias_u_pri_inter ) ) ) ) * 100.
          vtx_proj_ano_ban = vtx_proj_ano_bana.
          clear vtx_proj_ano_bana.
        endif.

        if vtx_proj_ano_como ne vtx_proj_ano_com.
          vtx_proj_ano_coma = ( ( vtx_proj_ano_como / 100 ) + ( ( ( vtx_proj_ano_com / 100 ) - ( vtx_proj_ano_como / 100 ) ) * ( ( wa_zfit0062-dias_c_mtm_cto - wa_zfit0062-dias_c_pri_inter ) /
                             ( wa_zfit0062-dias_c_seq_inter - wa_zfit0062-dias_c_pri_inter ) ) ) ) * 100.
          vtx_proj_ano_com = vtx_proj_ano_coma.
          clear vtx_proj_ano_coma.
        endif.

        v_desc_cdi = ( ( 1 + ( vtx_proj_ano_com / 100 ) )  ** ( wa_zfit0062-dias_c_mtm_cto / 360 ) ) - 1.

        "
        wa_zfit0067-tx_desc_cdi    = wa_zfit0062-tx_com_interp.
        wa_zfit0067-tx_proj_interp = wa_zfit0062-tx_cb_interp.
        "
        if wa_zfit0067-nro_par gt 1.
          read table it_zfit0062 into wa_zfit0062a
             with key bukrs         = wg_cadpro-bukrs
                      dt_fechamento = wg_cadpro-dt_fechamento
                      dt_vcto_cto   = vg_dt_fim_cto.
          if sy-subrc = 0.
            clear wa_zfit0060.
            read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062a-seq.
            vtx_proj_ano_bano = wa_zfit0060-tx_proj_ano_ban.

            read table it_zfit0060 into wa_zfit0060 with key dias_corridos = vg_dt_fim_cto.
            if sy-subrc = 0.
              vtx_proj_ano_bana = wa_zfit0060-tx_proj_ano_ban.
            else.
              vg_seqa = wa_zfit0062a-seq + 1.
              read table it_zfit0060 into wa_zfit0060 with key seq = vg_seqa.
              vtx_proj_ano_bana = wa_zfit0060-tx_proj_ano_ban.
            endif.
            if vtx_proj_ano_bano ne vtx_proj_ano_ban.
              vtx_proj_ano_bana = ( ( vtx_proj_ano_bano / 100 ) + ( ( ( vtx_proj_ano_bana / 100 ) - ( vtx_proj_ano_bano / 100 ) ) * ( ( wa_zfit0062a-dias_u_mtm_cto - wa_zfit0062a-dias_u_pri_inter ) /
                                 ( wa_zfit0062a-dias_u_seq_inter - wa_zfit0062a-dias_u_pri_inter ) ) ) ) * 100.
            endif.

            vtx_proj_ano_ban = ( ( ( ( 1 + ( vtx_proj_ano_ban  / 100 )  ) ** ( wa_zfit0062-dias_u_mtm_cto  / 252 ) ) /
                                 ( ( 1 + ( vtx_proj_ano_bana / 100 ) ) ** ( wa_zfit0062a-dias_u_mtm_cto / 252 ) ) ) ** ( 252 / ( wa_zfit0062-dias_u_mtm_cto - wa_zfit0062a-dias_u_mtm_cto ) ) - 1 ) * 100.
          endif.
        endif.
      endif.
    endif.

    clear vg_vlr_aj_merc .
    if wa_zfit0065-index_ativo = 'CDI'.
      "ponta ativa
      wa_zfit0067-a_ind_fix_db = wa_zfit0065-tx_index_ativo.
      wa_zfit0067-a_cdi_interp = vtx_proj_ano_ban.


      clear: v_fator, wa_zfit0067-vlr_proj_jros.
      if wa_zfit0067-dt_inicio_cto le wg_cadpro-dt_fechamento.
        v_fator = ( 1 + ( wa_zfit0065-tx_index_ativo  / 100 ) )  ** ( wa_zfit0067-dias_uteis_02 / 252 ) .
        wa_zfit0067-a_ind_var_db  = v_ft_acum_indev.
        wa_zfit0067-a_pa_curva    = ( v_ft_acum_indev * v_fator * wa_zfit0067-vlr_operacao_int ) - wa_zfit0067-vlr_operacao_int . "PA na Curva
        wa_zfit0067-a_ind_final   = (  ( ( 1 + ( vtx_proj_ano_ban / 100 ) ) ** ( wa_zfit0067-dias_uteis_03 / 252 )   ) * ( 1 + ( wa_zfit0065-tx_index_ativo  / 100 ) ) ** ( wa_zfit0067-dias_uteis_03 / 252 ) ) - 1.
      else.
        wa_zfit0067-a_ind_final   = (  ( ( 1 + ( vtx_proj_ano_ban / 100 ) ) ** ( wa_zfit0067-dias_uteis_01 / 252 )   ) * ( 1 + ( wa_zfit0065-tx_index_ativo  / 100 ) ) ** ( wa_zfit0067-dias_uteis_01 / 252 ) ) - 1.
      endif.

      wa_zfit0067-a_vf_pa    = ( wa_zfit0067-a_ind_final * ( wa_zfit0067-vlr_operacao_int + wa_zfit0067-a_pa_curva ) ). "valor futuro PA
      wa_zfit0067-a_vf_pa_ar = wa_zfit0067-a_vf_pa + wa_zfit0067-a_pa_curva + v_amortiz. "VF PA após Amort.

      wa_zfit0067-a_desc_cdi     = v_desc_cdi.
      wa_zfit0067-vlr_mtm_p_ativ = wa_zfit0067-a_vf_pa_ar / ( 1 + v_desc_cdi ).

      "ponta passiva
      wa_zfit0067-p_moeda_comp     = wa_zfit0067-tx_proj_interp / wa_zfit0065-taxa_inicial.
      v_amortiz = v_amortiz * wa_zfit0067-p_moeda_comp.
      wa_zfit0067-p_fator_adic     = ( ( ( wa_zfit0065-tx_index_passivo / 100 ) / 360 ) * wa_zfit0067-dias_corridos_01 ). "tx Adicional
      wa_zfit0067-p_fut_pp         = wa_zfit0067-p_fator_adic * ( wa_zfit0067-vlr_operacao_int * wa_zfit0067-p_moeda_comp ) . "Futuro da PP
      wa_zfit0067-p_fut_pp_ar      = wa_zfit0067-p_fut_pp + v_amortiz . "VF PP após Amort.
      wa_zfit0067-p_desc_cdi       = v_desc_cdi.
      wa_zfit0067-vlr_mtm_p_passiv = wa_zfit0067-p_fut_pp_ar / ( 1 + v_desc_cdi ). "MTM da PP
      wa_zfit0067-vlr_amortiz      = v_amortiz.

      wa_zfit0067-vlr_aj_merc = ( wa_zfit0067-vlr_mtm_p_ativ - wa_zfit0067-vlr_mtm_p_passiv ).


      vg_vlr_aj_merc = wa_zfit0067-vlr_aj_merc.
    endif.

    clear wa_zfit0067-vlr_extrato_bco.
    select single vlr_extrato_bco
         from zfit0214
         into wa_zfit0067-vlr_extrato_bco
         where dt_fechamento = wg_cadpro-dt_fechamento
         and   bukrs         = wg_cadpro-bukrs
         and   nro_cto       = wa_zfit0067-nro_cto
         and   nro_par       = wa_zfit0067-nro_par
         and   cod_oper      = 'S'.

    if sy-subrc = 0.
      wa_zfit0067-vlr_aj_merc = wa_zfit0067-vlr_extrato_bco.
    endif.

    if wa_zfit0067-vlr_aj_merc > 0.
      wa_zfit0067-tp_ajuste = 'ATIVO'.
    else.
      wa_zfit0067-tp_ajuste = 'PASSIVO'.
    endif.

*** PBI - 73761 - Inicio - CBRAND
    if wa_zfit0067-dias_corridos_03  > 365 and wa_zfit0067-vlr_aj_merc gt 0 .
      wa_zfit0067-tp_ajuste = 'ATIVOLP'.
    else.
      if  wa_zfit0067-dias_corridos_03 > 365 and wa_zfit0067-vlr_aj_merc le 0 .
        wa_zfit0067-tp_ajuste = 'PASSIVOLP'.
      endif.
    endif.
*** PBI - 73761 - Fim - CBRAND

    wa_zfit0067-vlr_aj_merc = vg_vlr_aj_merc.

    move:
    sy-uname                 to wa_zfit0067-usnam,
    sy-datum                 to wa_zfit0067-data_atual,
    sy-uzeit                 to wa_zfit0067-hora_atual.

    append wa_zfit0067 to it_zfit0067.
    clear wa_zfit0067.

  endloop.


  refresh tg_itens5.
  loop at it_zfit0067 into wa_zfit0067.
    refresh tg_itens5-color.
    wl_color-fieldname = 'VLR_EXTRATO_BCO'.
    wl_color-color-col = 3.
    wl_color-color-inv = 3.
    append wl_color to tg_itens5-color.
    move-corresponding wa_zfit0067 to tg_itens5.
    tg_itens5-ano = tg_itens5-dt_fim_cto+0(4).
    loop at it_zfit0066 into wa_zfit0066 where tp_ajuste   = wa_zfit0067-tp_ajuste.
      if wa_zfit0066-bschl = '40'.
        tg_itens5-hkont_d = wa_zfit0066-hkont.
      else.
        tg_itens5-hkont_c = wa_zfit0066-hkont.
      endif.

    endloop.
    tg_itens5-csd_div_usd = tg_itens5-vlr_operacao_int * tg_itens5-p_moeda_comp.
    tg_itens5-vlr_parc_brl = tg_itens5-vlr_parc * tg_itens5-tx_cambio_in_op.
    append tg_itens5.
  endloop.

  if it_zfit0067[] is initial.
    message s000(zwrm001) display like 'E' with 'Não há dados SWAP Fluxo caixa!'.
  else.
    message s836(sd) with 'Cálculo SWAP S'
                       ',Realizado com sucesso!'.
  endif.


endform.                    "f_calculo_SWAP

*&---------------------------------------------------------------------*
*&      Form  F_CALCULO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_calculo .

  data: xtx(14)    type p decimals 6,
        xdias(14)  type p decimals 6,
        xres(14)   type p decimals 6,

        xtx2(14)   type p decimals 6,
        xdias2(14) type p decimals 6,
        xres2(14)  type p decimals 6,

        xtx3(14)   type p decimals 6,
        xdias3(14) type p decimals 6,
        xres3(14)  type p decimals 6,

        xtx4(14)   type p decimals 6,
        xdias4(14) type p decimals 6,
        xres4(14)  type p decimals 6,

        xtxb(14)   type p decimals 7,
        xtxc(14)   type p decimals 7,

        vlines     type sy-tabix,
        tabix      type sy-tabix,

        werro(1).

  create object obj_sel_del.

  data:  tl_input_zfit0060 type table of zfit0060 with header line.

  clear werro.
  refresh it_zfit0060.
  loop at tg_itens.
    move: sy-mandt                 to tl_input_zfit0060-mandt,
          wg_cadpro-dt_fechamento  to tl_input_zfit0060-dt_fechamento,
          tg_itens-seq             to tl_input_zfit0060-seq,
          tg_itens-dias_corridos   to tl_input_zfit0060-dias_corridos,
          tg_itens-tx_proj_ano_ban to tl_input_zfit0060-tx_proj_ano_ban,
          tg_itens-tx_proj_ano_com to tl_input_zfit0060-tx_proj_ano_com,
          tg_itens-tx_par_dolar    to tl_input_zfit0060-tx_par_dolar,
          tg_itens-dt_base_bmf     to tl_input_zfit0060-dt_base_bmf,
          tg_itens-dias_uteis      to tl_input_zfit0060-dias_uteis,
          tg_itens-tx_cupom_camb   to tl_input_zfit0060-tx_cupom_camb,
          sy-uname                 to tl_input_zfit0060-usnam,
          sy-datum                 to tl_input_zfit0060-data_atual,
          sy-uzeit                 to tl_input_zfit0060-hora_atual.
    append tl_input_zfit0060.
  endloop.


  loop at tl_input_zfit0060.
    move-corresponding tl_input_zfit0060 to wa_zfit0060.
    append wa_zfit0060 to it_zfit0060.
  endloop.

  if it_zfit0060[] is initial.
    message s000(zwrm001) display like 'E' with 'Não há projeção BM&F cadastrada!'.
    exit.
  endif.

  sort: it_zfit0060 by dias_corridos.

* Set para empresas que utilizam a forma antiga
  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class         = '0000'
      setnr         = 'ZFI0043_EMPRESA'
    tables
      set_values    = t_empresa
    exceptions
      set_not_found = 1
      others        = 2.

  sort t_empresa by from.
  clear check_emp.
*  em caso de emcontrar a empresa no Set é alimentado o CHECK_EMP com X
  read table t_empresa with key from = wg_cadpro-bukrs.
  if sy-subrc is initial.
    move 'X' to check_emp.
  endif.

  if wg_cadpro-cod_oper ne 'T'.

    select *
      from zfit0065
      into table it_zfit0065
      where bukrs         = wg_cadpro-bukrs
      and   dt_fim_cto gt wg_cadpro-dt_fechamento
      and   cod_oper      = wg_cadpro-cod_oper.

    case check_emp.
      when 'X'.
        obj_sel_del->sel_dados_59( ).

      when others.

        case wg_cadpro-cod_oper.
          when 'N'.
*          Tipo de Operação NDF Verifica a tabela 83
            obj_sel_del->sel_dados_83( ).
            obj_sel_del->del_estornados( ).
          when others.
            obj_sel_del->sel_dados_59( ).
        endcase.
    endcase.

  else.

    select *
      from zfit0065
      into table it_zfit0065
      where bukrs         = wg_cadpro-bukrs
      and   dt_fim_cto gt wg_cadpro-dt_fechamento.

    select *
      from zfit0059
      into table it_zfit0059
      where data_vencimento gt  wg_cadpro-dt_fechamento
      and   pfj_codigo      eq wg_cadpro-bukrs.
*      AND   UPPER( MDO_CODIGO ) NE 'FREE SCHED'.

    "US 150369 operações FREE_SCHED não tem restrição porque vira do XRT como CTF ao inves de VANILA
*    loop at it_zfit0059 into wa_zfit0059.
*      data(tabix2) = sy-tabix.
*      translate wa_zfit0059-mdo_codigo to upper case.
*      if  wa_zfit0059-mdo_codigo eq 'FREE SCHED'.
*       AND wa_zfit0059-opr_numero NE '00039711'
*       AND wa_zfit0059-opr_numero NE '00034333'
*       AND wa_zfit0059-opr_numero NE '00036410'
*       AND wa_zfit0059-opr_numero NE '00036412'
*       AND wa_zfit0059-opr_numero NE '00036530'
*       AND wa_zfit0059-opr_numero NE '00037608'
*       AND wa_zfit0059-opr_numero NE '00040612'.
*        wa_zfit0059-posicao = 'XX'.
*        modify it_zfit0059 from  wa_zfit0059 index tabix2 transporting posicao.
*      endif.
*    endloop.
*    delete it_zfit0059 where posicao = 'XX'.

    "US 150369 operações FREE_SCHED não tem restrição porque vira do XRT como CTF ao inves de VANILA

    obj_sel_del->sel_dados_83( ).
    obj_sel_del->del_estornados( ).

  endif.

  if not check_emp is initial.

    sort it_zfit0059 by data_vencimento.
    delete adjacent duplicates from it_zfit0059  comparing data_vencimento.
    clear wa_zfit0065.
    loop at it_zfit0059 into wa_zfit0059.
      wa_zfit0065-dt_fim_cto = wa_zfit0059-data_vencimento.
      append wa_zfit0065 to it_zfit0065.
    endloop.

  else.

    case wg_cadpro-cod_oper.
      when 'N'.
        sort it_zfit0083 by date_period_1.
        delete adjacent duplicates from it_zfit0083  comparing date_period_1.
        clear wa_zfit0065.
        loop at it_zfit0083 into wa_zfit0083.
          wa_zfit0065-dt_fim_cto = wa_zfit0083-date_period_1.
          append wa_zfit0065 to it_zfit0065.
        endloop.

      when others.

        sort it_zfit0059 by data_vencimento.
        delete adjacent duplicates from it_zfit0059  comparing data_vencimento.
        clear wa_zfit0065.
        loop at it_zfit0059 into wa_zfit0059.
          wa_zfit0065-dt_fim_cto = wa_zfit0059-data_vencimento.
          append wa_zfit0065 to it_zfit0065.
        endloop.

        if wg_cadpro-cod_oper eq 'T'.

          sort it_zfit0083 by date_period_1.
          delete adjacent duplicates from it_zfit0083  comparing date_period_1.
          clear wa_zfit0065.
          loop at it_zfit0083 into wa_zfit0083.
            wa_zfit0065-dt_fim_cto = wa_zfit0083-date_period_1.
            append wa_zfit0065 to it_zfit0065.
          endloop.

        endif.

    endcase.

  endif.

  if it_zfit0065[] is initial.
    message s000(zwrm001) display like 'E' with 'Não há contratos em aberto, para esta empresa/operação!'.
    exit.
  endif.

  sort it_zfit0065 by dt_fim_cto.
  delete adjacent duplicates from it_zfit0065  comparing dt_fim_cto.

  refresh it_zfit0062.
  loop at it_zfit0065 into wa_zfit0065.
    wa_zfit0062-bukrs             = wg_cadpro-bukrs.
    wa_zfit0062-dt_fechamento     = wg_cadpro-dt_fechamento.
    wa_zfit0062-dt_vcto_cto       = wa_zfit0065-dt_fim_cto.
    wa_zfit0062-dias_c_mtm_cto    = wa_zfit0062-dt_vcto_cto - wg_cadpro-dt_fechamento.

    clear wa_zfit0060_2.
    loop at it_zfit0060 into wa_zfit0060.
      if wa_zfit0060-dias_corridos = wa_zfit0062-dias_c_mtm_cto.
        move-corresponding wa_zfit0060 to wa_zfit0060_2.
        exit.
      endif.
    endloop.
    if wa_zfit0060_2-seq is not initial.
      wa_zfit0062-seq                = wa_zfit0060_2-seq.
    elseif it_zfit0060[] is not initial.
      clear wa_zfit0060_2.
      loop at it_zfit0060 into wa_zfit0060.
        wa_zfit0060-dif  = wa_zfit0062-dias_c_mtm_cto - wa_zfit0060-dias_corridos.
        modify it_zfit0060 from wa_zfit0060 index sy-tabix transporting dif.
      endloop.
      sort it_zfit0060 by  dif.

      clear wa_zfit0060.
      loop at it_zfit0060 into wa_zfit0060.
        if wa_zfit0060-dif ge 0.
          exit.
        endif.
      endloop.
      move-corresponding wa_zfit0060 to wa_zfit0060_2.
      wa_zfit0062-seq                = wa_zfit0060-seq.
    endif.

    if wa_zfit0060_2-seq is initial.
      message s000(zwrm001) display like 'E' with 'Sequencia1 não encontrada!'.
      exit.
    endif.
    wa_zfit0062-dias_c_pri_inter  = wa_zfit0060-dias_corridos.

    sort it_zfit0060 by seq.
    read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0060_2-seq.
    tabix = sy-tabix + 1.
    describe table it_zfit0060 lines vlines.
    if tabix gt vlines.
      message s000(zwrm001) display like 'E' with 'Sequencia2 não encontrada!'.
      exit.
    endif.

    read table it_zfit0060 into wa_zfit0060 index tabix.
    if sy-subrc = 0.
      wa_zfit0062-dias_c_seq_inter = wa_zfit0060-dias_corridos.
      wa_zfit0062-dias_u_seq_inter = wa_zfit0060-dias_uteis.
      xtx  = ( wa_zfit0060-tx_proj_ano_ban / 100 ) + 1.
      xdias = wa_zfit0060-dias_uteis / 252.
      xres = ( ( xtx ** xdias ) - 1 ) * 100 .
      wa_zfit0062-tx_bco_tp        = xres.

      xtx3   = ( wa_zfit0060-tx_proj_ano_com / 100 ) + 1.
      xdias3 = wa_zfit0060-dias_corridos / 360.
      xres3  = ( ( xtx3 ** xdias3 ) - 1 ) * 100 .

      wa_zfit0062-tx_cb_tp          = wa_zfit0060-tx_par_dolar.
    else.
      wa_zfit0062-dias_c_seq_inter = 0.
    endif.

    perform calc_dias_uteis using wg_cadpro-dt_fechamento wa_zfit0062-dt_vcto_cto changing v_dias_uteis.
    wa_zfit0062-dias_u_mtm_cto     = v_dias_uteis.

    read table it_zfit0060 into wa_zfit0060 with key seq = wa_zfit0062-seq.
    if sy-subrc = 0.
      subtract 1 from wa_zfit0060-dias_uteis. "ALRS DUMP
      wa_zfit0062-dias_u_pri_inter   = wa_zfit0060-dias_uteis. "ALRS TESTE GISELE.
      xtx2  = ( wa_zfit0060-tx_proj_ano_ban / 100 ) + 1.
      xdias2 = wa_zfit0060-dias_uteis / 252.
      xres2 = ( ( xtx2 ** xdias2 ) - 1 ) * 100 .
      wa_zfit0062-tx_bco_tm        = xres2.

      xtx4  = ( wa_zfit0060-tx_proj_ano_com / 100 ) + 1.
      xdias4 = wa_zfit0060-dias_corridos / 360.
      xres4 = ( ( xtx4 ** xdias4 ) - 1 ) * 100 .

      wa_zfit0062-tx_cb_tm  = wa_zfit0060-tx_par_dolar.
    else.
      wa_zfit0062-dias_u_pri_inter   = 0.
    endif.

    xtxb =  wa_zfit0062-tx_bco_tm +  ( ( wa_zfit0062-dias_u_mtm_cto - wa_zfit0062-dias_u_pri_inter ) /  ( ( wa_zfit0062-dias_u_seq_inter - wa_zfit0062-dias_u_pri_inter  ) / ( wa_zfit0062-tx_bco_tp - wa_zfit0062-tx_bco_tm ) ) ) .

    wa_zfit0062-tx_bco_interp   = xtxb .
    wa_zfit0062-tx_com_t1p      = xres3.
    wa_zfit0062-tx_com_t1m      = xres4.

    xtxc =  wa_zfit0062-tx_com_t1m +  ( ( wa_zfit0062-dias_c_mtm_cto - wa_zfit0062-dias_c_pri_inter ) / ( ( wa_zfit0062-dias_c_seq_inter - wa_zfit0062-dias_c_pri_inter ) / ( wa_zfit0062-tx_com_t1p - wa_zfit0062-tx_com_t1m ) ) ).
    wa_zfit0062-tx_com_interp   = xtxc.


    xtxc =  wa_zfit0062-tx_cb_tm  +  ( ( wa_zfit0062-dias_c_mtm_cto - wa_zfit0062-dias_c_pri_inter ) / ( ( wa_zfit0062-dias_c_seq_inter - wa_zfit0062-dias_c_pri_inter ) / ( wa_zfit0062-tx_cb_tp   - wa_zfit0062-tx_cb_tm ) ) ).

    wa_zfit0062-tx_cb_interp        = xtxc.
    wa_zfit0062-usnam               = sy-uname.
    wa_zfit0062-data_atual          = sy-datum.
    wa_zfit0062-hora_atual          = sy-uzeit.

    append wa_zfit0062 to it_zfit0062.

  endloop.

  refresh tg_itens2.
  loop at it_zfit0062 into wa_zfit0062.
    move-corresponding wa_zfit0062 to tg_itens2.
    append tg_itens2.
  endloop.

  call method grid1->refresh_table_display
    exporting
      is_stable = wa_stable.

endform.                    " F_CALCULO
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout3 .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 'ZFIT0064'       'BUKRS'               'TG_ITENS3' 'BUKRS'            'Empresa'          '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'NRO_CTO'             'TG_ITENS3' 'NRO_CTO'          'Contrato'         '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'COD_OPER'            'TG_ITENS3' 'COD_OPER'         'Operação'         '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'BANCO'               'TG_ITENS3' 'BANCO'            'Banco'            '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'DT_INICIO_CTO'       'TG_ITENS3' 'DT_INICIO_CTO'    'Dt_Inicio_Cto'    '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'DT_FIM_CTO'          'TG_ITENS3' 'DT_FIM_CTO'       'Dt_Fim_Cto'       '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'TP_POSICAO'          'TG_ITENS3' 'TP_POSICAO'       'Posição'          '10' ' ' ' ' 'X',
*        2 'ZFIT0064'       'NATUREZA_CTO'        'TG_ITENS3' 'NATUREZA_CTO'     'Natureza Cto'     '10' ' ' ' ' 'X',
*        2 'ZFIT0064'       'TP_OPERACAO'         'TG_ITENS3' 'TP_OPERACAO'      'Tp.Operação'      '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'VLR_OPERACAO'        'TG_ITENS3' 'VLR_OPERACAO'     'Vlr.Operação'     '10' ' ' 'X' 'X',
        2 'ZFIT0064'       'MOEDA'               'TG_ITENS3' 'MOEDA'            'Moeda'            '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'TX_CAMBIO_FUT'       'TG_ITENS3' 'TX_CAMBIO_FUT'    'Tx_Cambio_Fut'    '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'QTE_DIAS_DT_BASE'    'TG_ITENS3' 'QTE_DIAS_DT_BASE' 'Qte_Dias_Dt_Base' '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'VLR_BASE_CALC'       'TG_ITENS3' 'VLR_BASE_CALC'    'Vlr_Base_Calc'    '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'TX_CAMB_PROJ'        'TG_ITENS3' 'TX_CAMB_PROJ'     'Tx_Camb_Proj'     '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'VLR_FUT_PROJ'        'TG_ITENS3' 'VLR_FUT_PROJ'     'Vlr_Fut_Proj'     '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'TX_DESC'             'TG_ITENS3' 'TX_DESC'          'Tx_Desc'          '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'FATOR_DESC'          'TG_ITENS3' 'FATOR_DESC'       'Fator_Desc'       '10' ' ' ' ' 'X',
        2 'ZFIT0064'       'VLR_EXTRATO_BCO'     'TG_ITENS3' 'VLR_EXTRATO_BCO'  'Vlr_Extr_BCO'     '10' 'X' 'X' 'X',
        2 'ZFIT0064'       'VLR_PRES_MTM'        'TG_ITENS3' 'VLR_PRES_MTM'     'Vlr_Pres_MTM'     '10' ' ' 'X' 'X',
        2 'ZFIT0064'       'TP_AJUSTE'           'TG_ITENS3' 'TP_AJUSTE'        'Tp_Ajuste'        '10' ' ' ' ' 'X',
        2 'ZFIT0066'       'HKONT_D'             'TG_ITENS5' 'HKONT_D'          'Débito'           '10' ' ' ' ' 'X',
        2 'ZFIT0066'       'HKONT_C'             'TG_ITENS5' 'HKONT_C'          'Crédito'          '10' ' ' ' ' 'X',
        2 ' '              ' '                   'TG_ITENS3' 'ANO'              'Ano_Enc_Ct'       '10' ' ' ' ' 'X'.

endform.                    " MONTAR_LAYOUT3

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form montar_layout4 .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 'ZFIT0063'       'DESCR'                   'TG_ITENS4_AUX' 'DESCR'                'Operação'           '10' ' ' ' ' 'X',
*        1 'ZFIT0064'       'TP_OPERACAO'             'TG_ITENS4_AUX' 'TP_OPERACAO'          'Tp.Operação'        '10' ' ' ' ' 'X',
        1 'ZFIT0066'       'TP_AJUSTE'               'TG_ITENS4_AUX' 'TP_AJUSTE'            'Tp.Ajuste'          '10' ' ' ' ' 'X',
        1 'ZFIT0066'       'BSCHL'                   'TG_ITENS4_AUX' 'BSCHL'                'Chv.Lcto'           '10' ' ' ' ' 'X',
        1 ' '              ' '                       'TG_ITENS4_AUX' 'HKONT'                'Conta'              '20' ' ' ' ' 'X',
        1 ' '              ' '                       'TG_ITENS4_AUX' 'TXT50'                'Descrição'          '20' ' ' ' ' 'X',
        1 'ZFIT0066'       'SHKZG'                   'TG_ITENS4_AUX' 'SHKZG'                'D/C'                '10' ' ' ' ' 'X',
        1 'ANLP'           'NAFAZ'                   'TG_ITENS4_AUX' 'WRBTR'                'Vlr.Moeda Interna'  '20' ' ' 'X' 'X'.

endform.                    " MONTAR_LAYOUT4


*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form montar_layout5.
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 'ZFIT0067'       'BUKRS'               'TG_ITENS5' 'BUKRS'            'Empresa'          '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'NRO_CTO'             'TG_ITENS5' 'NRO_CTO'          'Contrato'         '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'NRO_PAR'             'TG_ITENS5' 'NRO_PAR'          'Parcela'          '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'COD_OPER'            'TG_ITENS5' 'COD_OPER'         'Operação'         '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'BANCO'               'TG_ITENS5' 'BANCO'            'Banco'            '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'DT_INICIO_CTO'       'TG_ITENS5' 'DT_INICIO_CTO'    'Dt_Inicio_Cto'    '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'DT_FIM_CTO'          'TG_ITENS5' 'DT_FIM_CTO'       'Dt_Fim_Cto'       '10' ' ' ' ' 'X',

        2 'ZFIT0067'       'TX_CAMBIO_IN_OP'     'TG_ITENS5' 'TX_CAMBIO_IN_OP'  'Tx_Cambio_In_Op'  '10' ' ' ' ' 'X',

        2 'ZFIT0067'       'TX_PROJ_INTERP'      'TG_ITENS5'  'TX_PROJ_INTERP'  'USD Projetado'    '10' ' ' ' ' 'X',


        2 'ZFIT0067'       'TP_POSICAO'          'TG_ITENS5' 'TP_POSICAO'       'Posição'          '10' ' ' ' ' 'X',
*        2 'ZFIT0067'       'TP_OPERACAO'         'TG_ITENS5' 'TP_OPERACAO'      'Tp.Operação'      '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'VLR_OPERACAO'        'TG_ITENS5' 'VLR_OPERACAO'     'Vlr.Operação USD' '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'VLR_OPERACAO_INT'    'TG_ITENS5' 'VLR_OPERACAO_INT' 'Vlr.Operação Int' '10' ' ' ' ' 'X',

        2 'ZFIT0067'       'VLR_OPERACAO_INT'    'TG_ITENS5' 'CSD_DIV_USD'      'Saldo Dívida (USD)' '10' ' ' ' ' 'X',

        2 'ZFIT0067'       'VLR_PARC'            'TG_ITENS5' 'VLR_PARC'         'Vlr.Parc USD'     '10' ' ' 'X' 'X',

        2 'ZFIT0067'       'VLR_PARC'            'TG_ITENS5' 'VLR_PARC_BRL'     'Vlr.Parc BRL'     '10' ' ' 'X' 'X',
        2 'ZFIT0067'       'VLR_PARC'            'TG_ITENS5' 'VLR_AMORTIZ'      'Amortização'      '10' ' ' 'X' 'X',
        2 'ZFIT0067'       'TX_INDEX_ATIVO'      'TG_ITENS5' 'TX_INDEX_ATIVO'   'Tp_Tx_Ativa'      '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'TX_JROS_ATIVA'       'TG_ITENS5' 'TX_JROS_ATIVA'    'Tx_Jros_Ativa'    '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'MOEDA'               'TG_ITENS5' 'MOEDA'            'Moeda'            '10' ' ' ' ' 'X',

        2 'ZFIT0067'       'TX_INDEX_PASSIVO'    'TG_ITENS5' 'TX_INDEX_PASSIVO' 'TP_TX_PASSIVA'    '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'TX_JROS_PASSIVA'     'TG_ITENS5' 'TX_JROS_PASSIVA'  'TX_JROS_PASSIVA'  '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'DIAS_CORRIDOS_01'    'TG_ITENS5' 'DIAS_CORRIDOS_01' 'DIAS_CORRIDOS_01' '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'DIAS_UTEIS_01'       'TG_ITENS5' 'DIAS_UTEIS_01'    'DIAS_UTEIS_01'    '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'DIAS_CORRIDOS_02'    'TG_ITENS5' 'DIAS_CORRIDOS_02' 'DIAS_CORRIDOS_02' '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'DIAS_UTEIS_02'       'TG_ITENS5' 'DIAS_UTEIS_02'    'DIAS_UTEIS_02'    '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'DIAS_CORRIDOS_03'    'TG_ITENS5' 'DIAS_CORRIDOS_03' 'DIAS_CORRIDOS_03' '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'DIAS_UTEIS_03'       'TG_ITENS5' 'DIAS_UTEIS_03'    'DIAS_UTEIS_03'    '10' ' ' ' ' 'X',

        2 'ZFIT0067'       'A_IND_VAR_DB'        'TG_ITENS5' 'A_IND_VAR_DB'     'Index Var até DB' '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'A_IND_FIX_DB'        'TG_ITENS5' 'A_IND_FIX_DB'     'Index Fixo até DB' '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'A_PA_CURVA'          'TG_ITENS5' 'A_PA_CURVA'       'PA na Curva'      '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'A_CDI_INTERP'        'TG_ITENS5' 'A_CDI_INTERP'     'CDI Interp. Fut'  '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'A_IND_FINAL'         'TG_ITENS5' 'A_IND_FINAL'      'Indexador Final'  '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'A_VF_PA'             'TG_ITENS5' 'A_VF_PA'          'VF PA'            '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'A_VF_PA_AR'          'TG_ITENS5' 'A_VF_PA_AR'       'VF PA Amortiz'    '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'A_DESC_CDI'          'TG_ITENS5' 'A_DESC_CDI'       'Desconto CDI'     '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'VLR_MTM_P_ATIV'      'TG_ITENS5' 'VLR_MTM_P_ATIV'   'MTM P.A.'         '10' ' ' ' ' 'X',

        2 'ZFIT0067'       'P_MOEDA_COMP'        'TG_ITENS5' 'P_MOEDA_COMP'     'Moeda Comparada'  '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'P_FATOR_ADIC'        'TG_ITENS5' 'P_FATOR_ADIC'     'Fator c/ Tx Adic' '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'P_FUT_PP'            'TG_ITENS5' 'P_FUT_PP'         'Futuro da PP'     '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'P_FUT_PP_AR'         'TG_ITENS5' 'P_FUT_PP_AR'     'Futuro da PP Amor' '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'P_DESC_CDI'          'TG_ITENS5' 'P_DESC_CDI'       'DEsconto CDI'     '10' ' ' ' ' 'X',
        2 'ZFIT0067'       'VLR_MTM_P_PASSIV'    'TG_ITENS5' 'VLR_MTM_P_PASSIV' 'MTM P.P'          '10' ' ' ' ' 'X',

        2 'ZFIT0067'       'VLR_EXTRATO_BCO'     'TG_ITENS5' 'VLR_EXTRATO_BCO'    'VLR_EXTR_BCO'     '10' 'X' 'X' 'X',
        2 'ZFIT0067'       'VLR_AJ_MERC'         'TG_ITENS5' 'VLR_AJ_MERC'        'VLR_AJ_MERC'      '10' ' ' 'X' 'X',
        2 'ZFIT0067'       'TP_AJUSTE'           'TG_ITENS5' 'TP_AJUSTE'          'TP_AJUSTE'        '10' ' ' ' ' 'X',
        2 'ZFIT0066'       'HKONT_D'             'TG_ITENS5' 'HKONT_D'            'Débito'           '10' ' ' ' ' 'X',
        2 'ZFIT0066'       'HKONT_C'             'TG_ITENS5' 'HKONT_C'            'Crédito'          '10' ' ' ' ' 'X',
        2 ' '              ' '                   'TG_ITENS3' 'ANO'              'Ano_Enc_Ct'        '10' ' ' ' ' 'X'.

endform.                    "MONTAR_LAYOUT5
form montar_layout6_div.
  refresh t_fieldcatalog.
  perform montar_estrutura using:
  2 'ZFIT0069'      'NRO_CTO'           'TG_ITENS6'   'NRO_CTO'              'NRO_CTO'           '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_DATA_BASE'      'TG_ITENS6'   'TX_DATA_BASE'         'TX_DATA_BASE'      '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'COD_OPER'          'TG_ITENS6'   'COD_OPER'             'COD_OPER'          '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'BANCO'             'TG_ITENS6'   'BANCO'                'BANCO'             '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DT_INICIO_CTO'     'TG_ITENS6'   'DT_INICIO_CTO'        'DT_INICIO_CTO'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DT_FIM_CTO'        'TG_ITENS6'   'DT_FIM_CTO'           'DT_FIM_CTO'        '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_OPERACAO'      'TG_ITENS6'   'VLR_OPERACAO'         'VLR_OPERACAO USD'  '10' ' ' 'X' 'X',
  2 'ZFIT0069'      'VLR_OPERACAO_INT'  'TG_ITENS6'   'VLR_OPERACAO_INT'     'VLR_OPERACAO_INT'  '10' ' ' 'X' 'X',
  2 'ZFIT0069'      'MOEDA_COMPARADA'   'TG_ITENS6'   'MOEDA_COMPARADA'      'Moeda Comparada'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'SALDO_DIVIDA'      'TG_ITENS6'   'SALDO_DIVIDA'         'Saldo Divida USD'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_CAMBIO_IN_OP'   'TG_ITENS6'   'TX_CAMBIO_IN_OP'      'TX_CAMBIO_IN_OP'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_PROJ_INTERP'    'TG_ITENS6'   'TX_PROJ_INTERP'       'USD PROJETADO'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'INDEX_ATIVO'       'TG_ITENS6'   'INDEX_ATIVO'          'INDEX_ATIVO'       '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_INDEX_ATIVO'    'TG_ITENS6'   'TX_INDEX_ATIVO'       'TX_INDEX_ATIVO'    '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_AL_INDEX_ATIV'  'TG_ITENS6'   'TX_AL_INDEX_ATIV'     'TX_AL_INDEX_ATIV'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TP_TAXA_ATIVO'     'TG_ITENS6'   'TP_TAXA_ATIVO'        'TP_TAXA_ATIVO'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'INDEX_PASSIVO'     'TG_ITENS6'   'INDEX_PASSIVO'        'INDEX_PASSIVO'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_INDEX_PASSIVO'  'TG_ITENS6'   'TX_INDEX_PASSIVO'     'TX_INDEX_PASSIVO'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_AL_INDEX_PASS'  'TG_ITENS6'   'TX_AL_INDEX_PASS'     'TX_AL_INDEX_PASS'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TP_TAXA_PASSIVO'   'TG_ITENS6'   'TP_TAXA_PASSIVO'      'TP_TAXA_PASSIVO'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_CORRIDOS_01'  'TG_ITENS6'   'DIAS_CORRIDOS_01'     'DIAS_CORRIDOS_01'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_UTEIS_01'     'TG_ITENS6'   'DIAS_UTEIS_01'        'DIAS_UTEIS_01'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_CORRIDOS_02'  'TG_ITENS6'   'DIAS_CORRIDOS_02'     'DIAS_CORRIDOS_02'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_UTEIS_02'     'TG_ITENS6'   'DIAS_UTEIS_02'        'DIAS_UTEIS_02'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_CORRIDOS_03'  'TG_ITENS6'   'DIAS_CORRIDOS_03'     'DIAS_CORRIDOS_03'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_UTEIS_03'     'TG_ITENS6'   'DIAS_UTEIS_03'        'DIAS_UTEIS_03'     '10' ' ' ' ' 'X',

  "Ponta Ativa
  2 'ZFIT0069'      'FT_ACUM_INDEV'     'TG_ITENS6'   'FT_ACUM_INDEV'        'Indexador ate DB'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_CURVA_ATIVA'   'TG_ITENS6'   'VLR_CURVA_ATIVA'      'PA na Curva'       '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_INTER_CDI_ATI'  'TG_ITENS6'   'TX_INTER_CDI_ATI'     'CDI interpolado'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'FT_MULTP_ATIVA'    'TG_ITENS6'   'FT_MULTP_ATIVA'       'Indexador Final'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_FUT_ATIVVA'    'TG_ITENS6'   'VLR_FUT_ATIVA'        'VLR_FUT ATIVA'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'IND_PROJ_FINAL'    'TG_ITENS6'   'IND_PROJ_FINAL'       'Desconto CDI PA'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_MTM_P_ATIV'    'TG_ITENS6'   'VLR_MTM_P_ATIV'       'MTM PA'            '10' ' ' ' ' 'X',

  "Ponta Passiva
  2 'ZFIT0069'      'FT_MULTP_PASSIVA'  'TG_ITENS6'   'FT_MULTP_PASSIVA'     'Fator c/tx. Adic.' '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_FUT_PASSIVA'   'TG_ITENS6'   'VLR_FUT_PASSIVA'      'Futuro da PP'      '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'IND_PROJ_FINAL'    'TG_ITENS6'   'IND_PROJ_FINAL2'      'Desconto CDI PP'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_MTM_P_PASSIV'  'TG_ITENS6'   'VLR_MTM_P_PASSIV'     'MTM PP'            '10' ' ' ' ' 'X',

  2 'ZFIT0069'      'VLR_EXTRATO_BCO'   'TG_ITENS6'   'VLR_EXTRATO_BCO'      'VLR_EXTR_BCO'      '10' 'X' 'X' 'X',
  2 'ZFIT0069'      'VLR_AJ_MERC'       'TG_ITENS6'   'VLR_AJ_MERC'          'VLR_AJ_MERC'       '10' ' ' 'X' 'X',
  2 'ZFIT0069'      'TP_AJUSTE'         'TG_ITENS6'   'TP_AJUSTE'            'TP_AJUSTE'         '10' ' ' ' ' 'X',
  2 'ZFIT0066'      'HKONT_D'           'TG_ITENS5'   'HKONT_D'              'Débito'            '10' ' ' ' ' 'X',
  2 'ZFIT0066'      'HKONT_C'           'TG_ITENS5'   'HKONT_C'              'Crédito'           '10' ' ' ' ' 'X',
  2 ' '              ' '                'TG_ITENS3'   'ANO'                  'Ano_Enc_Ct'        '10' ' ' ' ' 'X'.

endform.                    "MONTAR_LAYOUT6
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form montar_layout6.
  refresh t_fieldcatalog.
  perform montar_estrutura using:
  2 'ZFIT0069'      'NRO_CTO'           'TG_ITENS6'   'NRO_CTO'              'NRO_CTO'           '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_DATA_BASE'      'TG_ITENS6'   'TX_DATA_BASE'         'TX_DATA_BASE'      '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'COD_OPER'          'TG_ITENS6'   'COD_OPER'             'COD_OPER'          '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'BANCO'             'TG_ITENS6'   'BANCO'                'BANCO'             '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DT_INICIO_CTO'     'TG_ITENS6'   'DT_INICIO_CTO'        'DT_INICIO_CTO'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DT_FIM_CTO'        'TG_ITENS6'   'DT_FIM_CTO'           'DT_FIM_CTO'        '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_OPERACAO'      'TG_ITENS6'   'VLR_OPERACAO'         'VLR_OPERACAO USD'  '10' ' ' 'X' 'X',
  2 'ZFIT0069'      'VLR_OPERACAO_INT'  'TG_ITENS6'   'VLR_OPERACAO_INT'     'VLR_OPERACAO_INT'  '10' ' ' 'X' 'X',
  2 'ZFIT0069'      'MOEDA_COMPARADA'   'TG_ITENS6'   'MOEDA_COMPARADA'      'Moeda Comparada'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'SALDO_DIVIDA'      'TG_ITENS6'   'SALDO_DIVIDA'         'Saldo Divida USD'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_CAMBIO_IN_OP'   'TG_ITENS6'   'TX_CAMBIO_IN_OP'      'TX_CAMBIO_IN_OP'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_PROJ_INTERP'    'TG_ITENS6'   'TX_PROJ_INTERP'       'USD PROJETADO'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'INDEX_ATIVO'       'TG_ITENS6'   'INDEX_ATIVO'          'INDEX_ATIVO'       '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_INDEX_ATIVO'    'TG_ITENS6'   'TX_INDEX_ATIVO'       'TX_INDEX_ATIVO'    '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_AL_INDEX_ATIV'  'TG_ITENS6'   'TX_AL_INDEX_ATIV'     'TX_AL_INDEX_ATIV'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TP_TAXA_ATIVO'     'TG_ITENS6'   'TP_TAXA_ATIVO'        'TP_TAXA_ATIVO'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'INDEX_PASSIVO'     'TG_ITENS6'   'INDEX_PASSIVO'        'INDEX_PASSIVO'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_INDEX_PASSIVO'  'TG_ITENS6'   'TX_INDEX_PASSIVO'     'TX_INDEX_PASSIVO'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TX_AL_INDEX_PASS'  'TG_ITENS6'   'TX_AL_INDEX_PASS'     'TX_AL_INDEX_PASS'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'TP_TAXA_PASSIVO'   'TG_ITENS6'   'TP_TAXA_PASSIVO'      'TP_TAXA_PASSIVO'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_CORRIDOS_01'  'TG_ITENS6'   'DIAS_CORRIDOS_01'     'DIAS_CORRIDOS_01'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_UTEIS_01'     'TG_ITENS6'   'DIAS_UTEIS_01'        'DIAS_UTEIS_01'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_CORRIDOS_02'  'TG_ITENS6'   'DIAS_CORRIDOS_02'     'DIAS_CORRIDOS_02'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_UTEIS_02'     'TG_ITENS6'   'DIAS_UTEIS_02'        'DIAS_UTEIS_02'     '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_CORRIDOS_03'  'TG_ITENS6'   'DIAS_CORRIDOS_03'     'DIAS_CORRIDOS_03'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'DIAS_UTEIS_03'     'TG_ITENS6'   'DIAS_UTEIS_03'        'DIAS_UTEIS_03'     '10' ' ' ' ' 'X',
*  2 'ZFIT0069'      'TX_COMP_DOLAR_AT'  'TG_ITENS6'   'TX_COMP_DOLAR_AT'     'TX_COMP_DOLAR_AT'  '10' ' ' ' ' 'X',

  2 'ZFIT0069'      'FT_MULTP_ATIVA'    'TG_ITENS6'   'FT_MULTP_ATIVA'       'Fator c/tx. Adic.' '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_FUT_ATIVA'     'TG_ITENS6'   'VLR_FUT_ATIVA'        'Futuro da PA'      '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'IND_PROJ_FINAL'    'TG_ITENS6'   'IND_PROJ_FINAL'       'Desconto CDI PA'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_MTM_P_ATIV'    'TG_ITENS6'   'VLR_MTM_P_ATIV'       'MTM PA'            '10' ' ' ' ' 'X',

  2 'ZFIT0069'      'FT_ACUM_INDEV'     'TG_ITENS6'   'FT_ACUM_INDEV'        'Indexador ate DB'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_CURVA_PASSIV'  'TG_ITENS6'   'VLR_CURVA_PASSIV'     'PP na Curva'       '10' ' ' ' ' 'X',
*  2 'ZFIT0069'      'TX_INTER_CDI_PAS'  'TG_ITENS6'   'TX_INTER_CDI_PAS'     'CDI interpolado'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'P_CDI_INTERP'      'TG_ITENS6'   'P_CDI_INTERP'         'Curva CDI Interp'  '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'P_IND_FINAL'       'TG_ITENS6'   'P_IND_FINAL'          'Indexador Final'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_FUT_PASSIVA'   'TG_ITENS6'   'VLR_FUT_PASSIVA'      'VLR_FUT PP'        '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'IND_PROJ_FINAL'    'TG_ITENS6'   'IND_PROJ_FINAL2'      'Desconto CDI PP'   '10' ' ' ' ' 'X',
  2 'ZFIT0069'      'VLR_MTM_P_PASSIV'  'TG_ITENS6'   'VLR_MTM_P_PASSIV'     'MTM PP'            '10' ' ' ' ' 'X',

  2 'ZFIT0069'      'VLR_EXTRATO_BCO'   'TG_ITENS6'   'VLR_EXTRATO_BCO'      'VLR_EXTR_BCO'      '10' 'X' 'X' 'X',
  2 'ZFIT0069'      'VLR_AJ_MERC'       'TG_ITENS6'   'VLR_AJ_MERC'          'VLR_AJ_MERC'       '10' ' ' 'X' 'X',
  2 'ZFIT0069'      'TP_AJUSTE'         'TG_ITENS6'   'TP_AJUSTE'            'TP_AJUSTE'         '10' ' ' ' ' 'X',
  2 'ZFIT0066'      'HKONT_D'           'TG_ITENS5'   'HKONT_D'              'Débito'            '10' ' ' ' ' 'X',
  2 'ZFIT0066'      'HKONT_C'           'TG_ITENS5'   'HKONT_C'              'Crédito'           '10' ' ' ' ' 'X',
  2 ' '              ' '                'TG_ITENS3'   'ANO'                  'Ano_Enc_Ct'        '10' ' ' ' ' 'X'.

endform.                    "MONTAR_LAYOUT6

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT7
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form montar_layout7.
  refresh t_fieldcatalog.
  perform montar_estrutura using:
   2 'ZFIT0070'      'BUKRS'              'TG_ITENS7'   'BUKRS'               'BUKRS'             '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'NRO_CTO'            'TG_ITENS7'   'NRO_CTO'             'NRO_CTO'           '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'NRO_PAR'            'TG_ITENS7'   'NRO_PAR'             'NRO_PAR'           '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'TX_DATA_BASE'       'TG_ITENS7'   'TX_DATA_BASE'        'TX_DATA_BASE'      '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'COD_OPER'           'TG_ITENS7'   'COD_OPER'            'COD_OPER'          '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'BANCO'              'TG_ITENS7'   'BANCO'               'BANCO'             '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'DT_INICIO_CTO'      'TG_ITENS7'   'DT_INICIO_CTO'       'DT_INICIO_CTO'     '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'DT_FIM_CTO'         'TG_ITENS7'   'DT_FIM_CTO'          'DT_FIM_CTO'        '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'TP_TX_ATIVA'        'TG_ITENS7'   'TP_TX_ATIVA'         'TP_TX_ATIVA'       '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'INDEX_ATIVO'        'TG_ITENS7'   'INDEX_ATIVO'         'INDEX_ATIVO'       '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'TX_CAMBIO_IN_OP'    'TG_ITENS7'   'TX_CAMBIO_IN_OP'     'TX_CAMBIO_IN_OP'   '10' ' ' ' ' 'X',
*   2 'ZFIT0070'      'TX_PERCT_CDI'       'TG_ITENS7'   'TX_PERCT_CDI'        'TX_PERCT_CDI'      '10' ' ' ' ' 'X',
*   2 'ZFIT0070'      'TX_JRS_ALEM_CDI'    'TG_ITENS7'   'TX_JRS_ALEM_CDI'     'TX_JRS_ALEM_CDI'   '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'VLR_OPERACAO'       'TG_ITENS7'   'VLR_OPERACAO'        'VLR_OPERACAO'      '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'VLR_OPERACAO_INT'   'TG_ITENS7'   'VLR_OPERACAO_INT'    'VLR_OPERACAO_INT'  '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'MOEDA'              'TG_ITENS7'   'MOEDA'               'MOEDA'             '10' ' ' ' ' 'X',
*   2 'ZFIT0070'      'TP_TX_PASSIVA'      'TG_ITENS7'   'TP_TX_PASSIVA'       'TP_TX_PASSIVA'     '10' ' ' ' ' 'X',
*   2 'ZFIT0070'      'VLR_BASE_VC'        'TG_ITENS7'   'VLR_BASE_VC'         'VLR_BASE_VC'       '10' ' ' 'X' 'X',
*   2 'ZFIT0070'      'TX_JROS_PASSIVA'    'TG_ITENS7'   'TX_JROS_PASSIVA'     'TX_JROS_PASSIVA'   '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'DIAS_CORRIDOS_01'   'TG_ITENS7'   'DIAS_CORRIDOS_01'    'DIAS_CORRIDOS_01'  '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'DIAS_UTEIS_01'      'TG_ITENS7'   'DIAS_UTEIS_01'       'DIAS_UTEIS_01'     '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'DIAS_CORRIDOS_02'   'TG_ITENS7'   'DIAS_CORRIDOS_02'    'DIAS_CORRIDOS_02'  '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'DIAS_UTEIS_02'      'TG_ITENS7'   'DIAS_UTEIS_02'       'DIAS_UTEIS_02'     '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'DIAS_CORRIDOS_03'   'TG_ITENS7'   'DIAS_CORRIDOS_03'    'DIAS_CORRIDOS_03'  '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'DIAS_UTEIS_03'      'TG_ITENS7'   'DIAS_UTEIS_03'       'DIAS_UTEIS_03'     '10' ' ' ' ' 'X',

*   2 'ZFIT0070'      'FT_ACUM_INDEV'      'TG_ITENS7'   'FT_ACUM_INDEV'       'FT_ACUM_INDEV'    '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'TX_PERCT_CDI'       'TG_ITENS7'   'TX_PERCT_CDI'        'Taxa Ativo'        '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'FT_MULTP_ATIVA'     'TG_ITENS7'   'FT_MULTP_ATIVA'      'Fator'             '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'VLR_CURVA_ATIVA'    'TG_ITENS7'   'VLR_CURVA_ATIVA'     'Futuro da PA'      '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'TX_DESC_CDI'        'TG_ITENS7'   'TX_DESC_CDI'         'Desconto CDI'      '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'VLR_MTM_P_ATIV'     'TG_ITENS7'   'VLR_MTM_P_ATIV'      'MTM P.A.'          '10' ' ' ' ' 'X',


   2 'ZFIT0070'      'TX_INDEX_PASSIVO'   'TG_ITENS7'   'TX_INDEX_PASSIVO'    'Taxa Adicional PP' '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'P_MOEDA_COMP'       'TG_ITENS7'   'P_MOEDA_COMP'        'Moeda Comparada'   '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'FT_ATUAL_PASSIVA'   'TG_ITENS7'   'FT_ATUAL_PASSIVA'    'Fator Tx Adic'     '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'VLR_CURVA_PASSIV'   'TG_ITENS7'   'VLR_CURVA_PASSIV'    'Futuro da PP'      '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'TX_DESC'            'TG_ITENS7'   'TX_DESC'             'Desconto CDI'      '10' ' ' ' ' 'X',
   2 'ZFIT0070'      'VLR_MTM_P_PASSIV'   'TG_ITENS7'   'VLR_MTM_P_PASSIV'    'MTM P.P.'          '10' ' ' ' ' 'X',

   2 'ZFIT0070'      'VLR_EXTRATO_BCO'    'TG_ITENS7'   'VLR_EXTRATO_BCO'     'VLR_EXTR_BCO'      '10' 'X' 'X' 'X',
   2 'ZFIT0070'      'VLR_AJ_MERC'        'TG_ITENS7'   'VLR_AJ_MERC'         'VLR_AJ_MERC'       '10' ' ' 'X' 'X',
   2 'ZFIT0070'      'TP_AJUSTE'          'TG_ITENS7'   'TP_AJUSTE'           'TP_AJUSTE'         '10' ' ' ' ' 'X',
   2 'ZFIT0066'      'HKONT_D'            'TG_ITENS5'   'HKONT_D'             'Débito'            '10' ' ' ' ' 'X',
   2 'ZFIT0066'      'HKONT_C'            'TG_ITENS5'   'HKONT_C'             'Crédito'           '10' ' ' ' ' 'X',
   2 ' '              ' '                 'TG_ITENS7'    'ANO'                'Ano_Enc_Ct'        '10' ' ' ' ' 'X'.

endform.                    "MONTAR_LAYOUT7

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ERR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form montar_layout_err .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 'ZIB_CONTABIL_ERR'         'OBJ_KEY'        'IT_ZIB_CONTABIL_ERR' 'OBJ_KEY'         ' '   '20' ' ' ' ' ' ',
        1 'ZIB_CONTABIL_ERR'         'NR_ITEM'        'IT_ZIB_CONTABIL_ERR' 'NR_ITEM'         ' '   '10' ' ' ' ' ' ',
        2 'ZIB_CONTABIL_ERR'         'INTERFACE'      'IT_ZIB_CONTABIL_ERR' 'INTERFACE'       ' '   '15' ' ' ' ' ' ',
        3 'ZIB_CONTABIL_ERR'         'DT_ATUALIZACAO' 'IT_ZIB_CONTABIL_ERR' 'DT_ATUALIZACAO'  ' '   '15' ' ' ' ' ' ',
        4 'ZIB_CONTABIL_ERR'         'HR_ATUALIZACAO' 'IT_ZIB_CONTABIL_ERR' 'HR_ATUALIZACAO'  ' '   '15' ' ' ' ' ' ',
        5 'ZIB_CONTABIL_ERR'         'TYPE'           'IT_ZIB_CONTABIL_ERR' 'TYPE'            ' '   '08' ' ' ' ' ' ',
        6 'ZIB_CONTABIL_ERR'         'ID'             'IT_ZIB_CONTABIL_ERR' 'ID'              ' '   '10' ' ' ' ' ' ',
        7 'ZIB_CONTABIL_ERR'         'NUM'            'IT_ZIB_CONTABIL_ERR' 'NUM'             ' '   '10' ' ' ' ' ' ',
        "8 'ZIB_CONTABIL_ERR'         'MESSAGE'        'IT_ZIB_CONTABIL_ERR' 'MESSAGE'         ' '   '20' ' ' ' ' ' ',
        8 ' '                        ' '              'IT_ZIB_CONTABIL_ERR' 'MESSAGE'         'Mensagem de Erro '   '100' ' ' ' ' ' ',
        9 'ZIB_CONTABIL_ERR'         'MESSAGE_V1'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V1'      ' '   '50' ' ' ' ' ' ',
       10 'ZIB_CONTABIL_ERR'         'MESSAGE_V2'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V2'      ' '   '30' ' ' ' ' ' ',
       11 'ZIB_CONTABIL_ERR'         'MESSAGE_V3'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V3'      ' '   '30' ' ' ' ' ' ',
       12 'ZIB_CONTABIL_ERR'         'MESSAGE_V4'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V4'      ' '   '30' ' ' ' ' ' '.
endform.                    " MONTAR_LAYOUT_ERR

*&---------------------------------------------------------------------*
*&      Form  F_CALCULO_NDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_calculo_ndf .

  data: wl_zfit0062     type zfit0062,
        tabix           type sy-tabix,
        t_operacao      type standard table of  rgsb4 with header line,
        vg_vlr_pres_mtm type zfit0064-vlr_pres_mtm.

  data: text1(30).

  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class         = '0000'
      setnr         = 'MAGGI_ZFI0043'
    tables
      set_values    = t_operacao
    exceptions
      set_not_found = 1
      others        = 2.
  if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
  sort t_operacao by from.

  loop at it_zfit0064 into wa_zfit0064.
    if wa_zfit0064-obj_key is not initial.
      vobj_key = wa_zfit0064-obj_key.
    endif.
  endloop.
  " Verifica se já foi contabilizado, neste caso não contabiliza este calculo - fazer estorno antes
  select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

  if sy-subrc eq 0.
    exit.
  endif.

  sort it_zfit0062 by bukrs  dt_vcto_cto.
  free: it_zfit0064, it_zfit0083, wa_zfit0083.

  case check_emp.
    when 'X'.

      select *
        from zfit0059
        into table it_zfit0059
        where mdo_tipo  = 'N'
        and   data_vencimento gt  wg_cadpro-dt_fechamento
        and   data_realizacao le  wg_cadpro-dt_fechamento
        and   pfj_codigo      eq wg_cadpro-bukrs.

      loop at it_zfit0059 into wa_zfit0059.
        wa_zfit0064-dt_fechamento = wg_cadpro-dt_fechamento.
        wa_zfit0064-nro_cto       = wa_zfit0059-opr_numero.
        wa_zfit0064-bukrs         = wa_zfit0059-pfj_codigo.
        wa_zfit0064-cod_oper      = wa_zfit0059-mdo_tipo.
        wa_zfit0064-banco         = wa_zfit0059-pfj_agente.
        wa_zfit0064-dt_inicio_cto = wa_zfit0059-data_realizacao.
        wa_zfit0064-dt_fim_cto    = wa_zfit0059-data_vencimento.
        wa_zfit0064-tp_posicao    = wa_zfit0059-posicao.
        wa_zfit0064-natureza_cto  = wa_zfit0059-mdo_codigo.

        read table t_operacao with key from = wa_zfit0064-natureza_cto.
        if sy-subrc eq 0.
          wa_zfit0064-tp_operacao	  =	'FINANCEIRO'.
        else.
          wa_zfit0064-tp_operacao	  =	'OPERACIONAL'.
        endif.

        wa_zfit0064-vlr_operacao  = wa_zfit0059-valor_operacao.
        wa_zfit0064-moeda         = wa_zfit0059-moeda.
        wa_zfit0064-tx_cambio_fut = wa_zfit0059-tx_dolar_in_op.

        move:
        sy-uname                 to wa_zfit0064-usnam,
        sy-datum                 to wa_zfit0064-data_atual,
        sy-uzeit                 to wa_zfit0064-hora_atual.
        append wa_zfit0064 to it_zfit0064.
        clear wa_zfit0064.

      endloop.

    when others.

*      SELECT *
*        FROM ZFIT0083
*        INTO TABLE IT_ZFIT0083
*        WHERE DEAL_TYPE EQ '4'
*        AND   BUKRS         EQ WG_CADPRO-BUKRS
*        AND   DATE_OF_DEAL  GT  WG_CADPRO-DT_FECHAMENTO
*        AND   DATE_PERIOD_1 LE  WG_CADPRO-DT_FECHAMENTO.

      obj_sel_del->sel_dados_83( ).
      obj_sel_del->del_estornados( ).

      loop at it_zfit0083 into wa_zfit0083.

        move:
            wg_cadpro-dt_fechamento      to wa_zfit0064-dt_fechamento,
            wa_zfit0083-trade_id         to wa_zfit0064-nro_cto,
            wa_zfit0083-bukrs            to wa_zfit0064-bukrs,
            'N'                          to wa_zfit0064-cod_oper,
            wa_zfit0083-cont_part_deal_c to wa_zfit0064-banco,
            wa_zfit0083-date_of_deal     to wa_zfit0064-dt_inicio_cto,
            wa_zfit0083-date_period_1    to wa_zfit0064-dt_fim_cto.

        case wa_zfit0083-side.
          when 'B'.
            move text-001 to wa_zfit0064-tp_posicao.
          when 'S'.
            move text-002 to wa_zfit0064-tp_posicao.
        endcase.

        concatenate wa_zfit0083-note_text_1 wa_zfit0083-note_text_2 into text1.

        clear wa_zfit0095.
        select single *
          from zfit0095
            into wa_zfit0095
              where bukrs eq wg_cadpro-bukrs
             and natureza eq text1.

        concatenate wa_zfit0083-note_text_1 wa_zfit0083-note_text_2 into wa_zfit0064-natureza_cto.

        case wa_zfit0095-operacao.
          when 'O'.
            move text-003 to wa_zfit0064-tp_operacao.
          when 'F'.
            move text-004 to wa_zfit0064-tp_operacao.
          when 'C'.
            move text-005 to wa_zfit0064-tp_operacao.
        endcase.

        move:
            wa_zfit0083-amount_dealt     to wa_zfit0064-vlr_operacao,
            wa_zfit0083-dealt_currency   to wa_zfit0064-moeda,
            wa_zfit0083-exch_rat_period1 to wa_zfit0064-tx_cambio_fut,
            sy-uname                     to wa_zfit0064-usnam,
            sy-datum                     to wa_zfit0064-data_atual,
            sy-uzeit                     to wa_zfit0064-hora_atual.

        append wa_zfit0064 to it_zfit0064.
        clear wa_zfit0064.

      endloop.

  endcase.

  select *
    from zfit0065
    into table it_zfit0065
    where cod_oper   = 'N'
    and   dt_fim_cto gt wg_cadpro-dt_fechamento
    and   dt_inicio_cto le wg_cadpro-dt_fechamento
    and   bukrs      eq wg_cadpro-bukrs.

  loop at it_zfit0065 into wa_zfit0065.
    wa_zfit0064-dt_fechamento = wg_cadpro-dt_fechamento.
    wa_zfit0064-nro_cto       = wa_zfit0065-nro_cto.
    wa_zfit0064-bukrs         = wa_zfit0065-bukrs.
    wa_zfit0064-cod_oper      = wa_zfit0065-cod_oper.
    wa_zfit0064-banco         = wa_zfit0065-banco.
    wa_zfit0064-dt_inicio_cto = wa_zfit0065-dt_inicio_cto.
    wa_zfit0064-dt_fim_cto    = wa_zfit0065-dt_fim_cto.
    if wa_zfit0065-posicao = 'C'.
      wa_zfit0064-tp_posicao    = 'Compra'.
    else.
      wa_zfit0064-tp_posicao    = 'Venda'.
    endif.
    wa_zfit0064-natureza_cto  = wa_zfit0065-natureza_cto.
    wa_zfit0064-tp_operacao	  =	wa_zfit0065-tp_operacao.
    if wa_zfit0065-tp_operacao = 'F'.
      wa_zfit0064-tp_operacao    = 'FINANCEIRO'.
    else.
      wa_zfit0064-tp_operacao   = 'OPERACIONAL'.
    endif.
    if wa_zfit0065-moeda = 'USD'.
      wa_zfit0064-vlr_operacao  = wa_zfit0065-vlr_cto_us.
    else.
      wa_zfit0064-vlr_operacao  = wa_zfit0065-vlr_cto_r.
    endif.
    wa_zfit0064-moeda         = wa_zfit0065-moeda.
    wa_zfit0064-tx_cambio_fut = wa_zfit0065-tx_cambio_fut.

    move:
      sy-uname                 to wa_zfit0064-usnam,
      sy-datum                 to wa_zfit0064-data_atual,
      sy-uzeit                 to wa_zfit0064-hora_atual.

    append wa_zfit0064 to it_zfit0064.
    clear wa_zfit0064.

  endloop.


  loop at it_zfit0064 into wa_zfit0064.
    tabix = sy-tabix.
    "Calculos
    wa_zfit0064-qte_dias_dt_base  = wa_zfit0064-dt_fim_cto - wg_cadpro-dt_fechamento.
    if wa_zfit0064-tp_posicao+0(1) = 'V'. "'VENDIDA'.
      wa_zfit0064-vlr_base_calc	=  wa_zfit0064-vlr_operacao * -1.
    else.
      wa_zfit0064-vlr_base_calc  = wa_zfit0064-vlr_operacao.
    endif.

    read table  it_zfit0062 into wl_zfit0062
        with key  bukrs       = wa_zfit0064-bukrs
                  dt_vcto_cto = wa_zfit0064-dt_fim_cto.

    if sy-subrc = 0.
      wa_zfit0064-tx_camb_proj  = wl_zfit0062-tx_cb_interp.
      wa_zfit0064-tx_desc   = wl_zfit0062-tx_com_interp.
    else.
      clear: wa_zfit0064-tx_camb_proj, wa_zfit0064-tx_desc.
    endif.



    wa_zfit0064-vlr_fut_proj  = ( wa_zfit0064-tx_camb_proj - wa_zfit0064-tx_cambio_fut ) * wa_zfit0064-vlr_base_calc.
    wa_zfit0064-fator_desc    = ( 1 + ( wa_zfit0064-tx_desc / 100 ) ) ** ( wa_zfit0064-qte_dias_dt_base / 360 ).
    wa_zfit0064-vlr_pres_mtm  = wa_zfit0064-vlr_fut_proj / ( 1 + ( wa_zfit0064-tx_desc / 100 ) ).
    "
    vg_vlr_pres_mtm = wa_zfit0064-vlr_pres_mtm.

    clear wa_zfit0064-vlr_extrato_bco.
    select single vlr_extrato_bco
        from zfit0214
        into wa_zfit0064-vlr_extrato_bco
        where dt_fechamento = wg_cadpro-dt_fechamento
        and   bukrs         = wg_cadpro-bukrs
        and   nro_cto       = wa_zfit0064-nro_cto
        and   cod_oper      = 'N'.
    if sy-subrc = 0.
      wa_zfit0064-vlr_pres_mtm  = wa_zfit0064-vlr_extrato_bco.
    endif.

    if wa_zfit0064-vlr_pres_mtm gt 0 .
      wa_zfit0064-tp_ajuste = 'ATIVO'.
    else.
      wa_zfit0064-tp_ajuste = 'PASSIVO'.
    endif.
*** PBI - 73761 - Inicio - CBRAND
    if wa_zfit0064-qte_dias_dt_base  > 365 and wa_zfit0064-vlr_pres_mtm gt 0 .
      wa_zfit0064-tp_ajuste = 'ATIVOLP'.
    else.
      if wa_zfit0064-qte_dias_dt_base  > 365 and  wa_zfit0064-vlr_pres_mtm le 0 .
        wa_zfit0064-tp_ajuste = 'PASSIVOLP'.
      endif.
    endif.

    wa_zfit0064-vlr_pres_mtm  = vg_vlr_pres_mtm.
*** PBI - 73761 - Fim - CBRAND
    modify it_zfit0064 from wa_zfit0064 index tabix transporting qte_dias_dt_base vlr_base_calc tx_camb_proj tx_desc vlr_fut_proj fator_desc vlr_pres_mtm tp_ajuste.
  endloop.

  delete it_zfit0064 where dt_inicio_cto gt wg_cadpro-dt_fechamento.

  select *
     from zfit0066
     into table it_zfit0066
     where  cod_oper      eq 'N'.
  sort it_zfit0066 by tp_operacao tp_ajuste.

  refresh tg_itens3.
  loop at it_zfit0064 into wa_zfit0064.
    move-corresponding wa_zfit0064 to tg_itens3.
    tg_itens3-ano = tg_itens3-dt_fim_cto+0(4).
    clear: wl_color.
    refresh tg_itens3-color.
    wl_color-fieldname = 'VLR_EXTRATO_BCO'.
    wl_color-color-col = 3.
    wl_color-color-inv = 3.
    append wl_color to tg_itens3-color.
    loop at it_zfit0066 into wa_zfit0066 where tp_ajuste   = wa_zfit0064-tp_ajuste.
      if wa_zfit0066-bschl = '40'.
        tg_itens3-hkont_d = wa_zfit0066-hkont.
      else.
        tg_itens3-hkont_c = wa_zfit0066-hkont.
      endif.

    endloop.

    append tg_itens3.
  endloop.

  if it_zfit0064[] is initial.
    message s000(zwrm001) display like 'E' with 'Não há dados  NDF!'.
  else.
    message s836(sd) with 'Cálculo NDF'
                       ',Realizado com sucesso!'.
  endif.

endform.                    " F_CALCULO_NDF

*&---------------------------------------------------------------------*
*&      Form  F_CALCULO_CTB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_calculo_ctb.

  data:
    wl_skat         type skat,
    wl_tbsl         type tbsl,
    wa_zfit0064_aux type zfit0064,
    it_zfit0064_aux type table of zfit0064,
    wa_zfit0067_aux type zfit0067,
    it_zfit0067_aux type table of zfit0067,
    wa_zfit0069_aux type zfit0069,
    it_zfit0069_aux type table of zfit0069,
    wa_zfit0070_aux type zfit0070,
    it_zfit0070_aux type table of zfit0070,
    tl_zfit0064     type table of zfit0064 with header line,
    tl_zfit0067     type table of zfit0067 with header line,
    tl_zfit0069     type table of zfit0069 with header line,
    tl_zfit0070     type table of zfit0070 with header line.

  clear vg_extrato.
  refresh tg_itens4_aux. "guardar os já contabilizados OK somente para exibição
*  LOOP AT TG_ITENS4.
*    IF TG_ITENS4-GRAV = 'X'.
*      MOVE-CORRESPONDING TG_ITENS4 TO TG_ITENS4_AUX.
*      APPEND TG_ITENS4_AUX.
*    ENDIF.
*  ENDLOOP.

  refresh tg_itens4. "Limpa contabilização
  clear tg_itens4.

  clear: xestorno, xestorno_botao, wg_cadpro-belnr, wg_cadpro-belnr2. " Limpa
  " Contabiliza NDF
  if xcod_oper_nav = 'N' or wg_cadpro-cod_oper  = 'T'.
    it_zfit0064_aux[] = it_zfit0064[].
    sort: it_zfit0064     by tp_operacao tp_ajuste,
          it_zfit0064_aux by tp_operacao tp_ajuste.

    delete adjacent duplicates from it_zfit0064_aux comparing tp_operacao tp_ajuste.

    loop at it_zfit0064_aux into wa_zfit0064.
      wa_zfit0064-vlr_pres_mtm = 0.
      wa_zfit0064-vlr_extrato_bco = 0.
      modify it_zfit0064_aux from wa_zfit0064 index sy-tabix transporting vlr_pres_mtm.
    endloop.

    loop at it_zfit0064_aux into wa_zfit0064_aux.
      tabix = sy-tabix.

      loop at it_zfit0064 into wa_zfit0064 where tp_operacao = wa_zfit0064_aux-tp_operacao
                                           and   tp_ajuste   = wa_zfit0064_aux-tp_ajuste.
        if wa_zfit0064-vlr_extrato_bco ne 0.
          add wa_zfit0064-vlr_extrato_bco to wa_zfit0064_aux-vlr_pres_mtm.
        else.
          add wa_zfit0064-vlr_pres_mtm    to wa_zfit0064_aux-vlr_pres_mtm.
        endif.
      endloop.
      if wa_zfit0064_aux-vlr_pres_mtm lt 0.
        wa_zfit0064_aux-vlr_pres_mtm = wa_zfit0064_aux-vlr_pres_mtm * -1.
      endif.
      modify it_zfit0064_aux from wa_zfit0064_aux index tabix transporting vlr_pres_mtm .
    endloop.

    clear vobj_key.
    " NDF
    select *
      from zfit0064
      into table tl_zfit0064
      where dt_fechamento = wg_cadpro-dt_fechamento
      and   bukrs         = wg_cadpro-bukrs
      and   cod_oper      = 'N'.

    loop at tl_zfit0064.
      if tl_zfit0064-obj_key is not initial.
        vobj_key = tl_zfit0064-obj_key.
        robj_key = tl_zfit0064-obj_key_est.
      endif.
    endloop.


    " Reinicializa Erros
    select single *
      from zib_contabil_err
      into wa_zib_contabil_err
      where obj_key = vobj_key.

    if sy-subrc = 0.
      delete from zib_contabil_err where obj_key = vobj_key.
      delete from zib_contabil     where obj_key = vobj_key.
      delete from zib_contabil_err where obj_key = robj_key.
      delete from zib_contabil     where obj_key = robj_key.
    endif.

    select *
      from zfit0066
      into table it_zfit0066
      where  cod_oper      eq 'N'.

    sort it_zfit0066 by tp_operacao tp_ajuste .

    " Verifica se já foi contabilizado, neste caso não contabiliza este calculo - fazer estorno antes
    select  single *
      from zib_contabil
      into wa_zib_contabil
      where obj_key = vobj_key
      and   bktxt   = 'MTM-FINANCEIRO'.

    if sy-subrc ne 0.
      loop at it_zfit0064_aux into wa_zfit0064_aux.
        translate wa_zfit0064_aux-tp_ajuste to upper case.
        loop at it_zfit0066 into wa_zfit0066 where tp_operacao = wa_zfit0064_aux-tp_operacao+0(1)
                                             and   tp_ajuste   = wa_zfit0064_aux-tp_ajuste.
          move-corresponding wa_zfit0066 to tg_itens4.
*          IF WA_ZFIT0064_AUX-TP_OPERACAO+0(1) NE WA_ZFIT0066-TP_OPERACAO.
*            CONTINUE.
*          ENDIF.
          tg_itens4-tp_operacao = wa_zfit0064_aux-tp_operacao.

          select single *
             from skat
             into wl_skat
             where spras eq sy-langu and ktopl eq '0050'
             and   saknr = tg_itens4-hkont.
          tg_itens4-txt50 =  wl_skat-txt50.

          select single * from tbsl
            into wl_tbsl
            where koart = 'S'
            and   bschl = tg_itens4-bschl.

          if wa_zfit0064_aux-vlr_extrato_bco = 0.
            if wa_zfit0066-tx_imp = 0 .
              tg_itens4-wrbtr  = wa_zfit0064_aux-vlr_pres_mtm.
            else.
              tg_itens4-wrbtr  = wa_zfit0064_aux-vlr_pres_mtm * ( wa_zfit0066-tx_imp / 100 ).
            endif.
          else.
            if wa_zfit0066-tx_imp = 0 .
              tg_itens4-wrbtr  = wa_zfit0064_aux-vlr_extrato_bco.
            else.
              tg_itens4-wrbtr  = wa_zfit0064_aux-vlr_extrato_bco * ( wa_zfit0066-tx_imp / 100 ).
            endif.
          endif.

          if wl_tbsl-shkzg = 'H'.
            move 'C' to tg_itens4-shkzg.
            tg_itens4-wrbtr  = tg_itens4-wrbtr  * -1.
          else.
            move 'D' to tg_itens4-shkzg.
          endif.

          select single *
            from zfit0063
            into wl_zfit0063
            where cod_oper = 'N'.

          if sy-subrc = 0.
            tg_itens4-descr = wl_zfit0063-descr.
          endif.

          append tg_itens4.
          clear tg_itens4.
        endloop.

      endloop.
    else.
      message 'Cáculo NDF, já contabilizado, estornar se necessário recalcular' type 'I'.
    endif.
  endif.

  " Contabiliza SWAP Fluxo de Caixa T.F.
  if xcod_oper_nav = 'S' or wg_cadpro-cod_oper  = 'T'.
    it_zfit0067_aux[] = it_zfit0067[].
    sort: it_zfit0067     by tp_ajuste,
          it_zfit0067_aux by tp_ajuste.

    delete adjacent duplicates from it_zfit0067_aux comparing tp_ajuste.

    loop at it_zfit0067_aux into wa_zfit0067.
      wa_zfit0067-vlr_aj_merc = 0.
      wa_zfit0067-vlr_extrato_bco = 0.
      modify it_zfit0067_aux from wa_zfit0067 index sy-tabix transporting vlr_aj_merc vlr_extrato_bco.
    endloop.

    loop at it_zfit0067_aux into wa_zfit0067_aux.
      tabix = sy-tabix.

      loop at it_zfit0067 into wa_zfit0067 where tp_ajuste = wa_zfit0067_aux-tp_ajuste.
        if wa_zfit0067-vlr_extrato_bco ne 0.
          add wa_zfit0067-vlr_extrato_bco to wa_zfit0067_aux-vlr_aj_merc.
        else.
          add wa_zfit0067-vlr_aj_merc     to wa_zfit0067_aux-vlr_aj_merc.
        endif.
      endloop.
      if wa_zfit0067_aux-vlr_aj_merc lt 0.
        wa_zfit0067_aux-vlr_aj_merc = wa_zfit0067_aux-vlr_aj_merc * -1.
      endif.
      modify it_zfit0067_aux from wa_zfit0067_aux index tabix transporting vlr_aj_merc .
    endloop.

    clear vobj_key.
    " SWAP S
    select *
      from zfit0067
      into table tl_zfit0067
      where dt_fechamento = wg_cadpro-dt_fechamento
      and   bukrs         = wg_cadpro-bukrs
      and   cod_oper      = 'S'.

    loop at tl_zfit0067.
      if tl_zfit0067-obj_key is not initial.
        vobj_key = tl_zfit0067-obj_key.
        robj_key = tl_zfit0067-obj_key_est.
      endif.
    endloop.

    " Reinicializa Erros
    select single *
      from zib_contabil_err
      into wa_zib_contabil_err
      where obj_key = vobj_key.

    if sy-subrc = 0.
      delete from zib_contabil_err where obj_key = vobj_key.
      delete from zib_contabil     where obj_key = vobj_key.
      delete from zib_contabil_err where obj_key = robj_key.
      delete from zib_contabil     where obj_key = robj_key.
    endif.

    select *
      from zfit0066
      into table it_zfit0066
      where  cod_oper      eq 'S'.

    sort it_zfit0066 by tp_ajuste .

    select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

    if sy-subrc ne 0.
      loop at it_zfit0067_aux into wa_zfit0067_aux.
        translate wa_zfit0067_aux-tp_ajuste to upper case.
        loop at it_zfit0066 into wa_zfit0066 where tp_ajuste = wa_zfit0067_aux-tp_ajuste.
          move-corresponding wa_zfit0066 to tg_itens4.

          select single *
             from skat
             into wl_skat
             where spras eq sy-langu and ktopl eq '0050'
             and   saknr = tg_itens4-hkont.
          tg_itens4-txt50 =  wl_skat-txt50.

          select single * from tbsl
            into wl_tbsl
            where koart = 'S'
            and   bschl = tg_itens4-bschl.

          if wa_zfit0067_aux-vlr_extrato_bco = 0.
            if wa_zfit0066-tx_imp = 0 .
              tg_itens4-wrbtr  = wa_zfit0067_aux-vlr_aj_merc.
            else.
              tg_itens4-wrbtr  = wa_zfit0067_aux-vlr_aj_merc * ( wa_zfit0066-tx_imp / 100 ).
            endif.
          else.
            if wa_zfit0066-tx_imp = 0 .
              tg_itens4-wrbtr  = wa_zfit0067_aux-vlr_extrato_bco.
            else.
              tg_itens4-wrbtr  = wa_zfit0067_aux-vlr_extrato_bco * ( wa_zfit0066-tx_imp / 100 ).
            endif.
          endif.

          if wl_tbsl-shkzg = 'H'.
            move 'C' to tg_itens4-shkzg.
            tg_itens4-wrbtr  = tg_itens4-wrbtr  * -1.
          else.
            move 'D' to tg_itens4-shkzg.
          endif.

          select single *
                 from zfit0063
                 into wl_zfit0063
                 where cod_oper = 'S'.

          if sy-subrc = 0.
            tg_itens4-descr = wl_zfit0063-descr.
          endif.

          append tg_itens4.
          clear tg_itens4.
        endloop.

      endloop.
    else.
      message 'Cáculo SWAP Fluxo T.F., já contabilizado, estornar se necessário recalcular' type 'I'.
    endif.
  endif.

  " Contabiliza SWAP Fluxo de Caixa T.V.
  if xcod_oper_nav = 'V' or wg_cadpro-cod_oper  = 'T'.
    it_zfit0070_aux[] = it_zfit0070[].
    sort: it_zfit0070     by tp_ajuste,
          it_zfit0070_aux by tp_ajuste.

    delete adjacent duplicates from it_zfit0070_aux comparing tp_ajuste.

    loop at it_zfit0070_aux into wa_zfit0070.
      wa_zfit0070-vlr_aj_merc = 0.
      wa_zfit0070-vlr_extrato_bco = 0.
      modify it_zfit0070_aux from wa_zfit0070 index sy-tabix transporting vlr_aj_merc vlr_extrato_bco.
    endloop.

    loop at it_zfit0070_aux into wa_zfit0070_aux.
      tabix = sy-tabix.

      loop at it_zfit0070 into wa_zfit0070 where tp_ajuste = wa_zfit0070_aux-tp_ajuste.
        if wa_zfit0070-vlr_extrato_bco ne 0.
          add wa_zfit0070-vlr_extrato_bco to wa_zfit0070_aux-vlr_aj_merc.
        else.
          add wa_zfit0070-vlr_aj_merc     to wa_zfit0070_aux-vlr_aj_merc.
        endif.
      endloop.
      if wa_zfit0070_aux-vlr_aj_merc lt 0.
        wa_zfit0070_aux-vlr_aj_merc = wa_zfit0070_aux-vlr_aj_merc * -1.
      endif.
      modify it_zfit0070_aux from wa_zfit0070_aux index tabix transporting vlr_aj_merc .
    endloop.

    clear vobj_key.
    " SWAP V
    select *
      from zfit0070
      into table tl_zfit0070
      where dt_fechamento = wg_cadpro-dt_fechamento
      and   bukrs         = wg_cadpro-bukrs
      and   cod_oper      = 'V'.

    loop at tl_zfit0070.
      if tl_zfit0070-obj_key is not initial.
        vobj_key = tl_zfit0070-obj_key.
        robj_key = tl_zfit0070-obj_key_est.
      endif.
    endloop.

    " Reinicializa Erros
    select single *
      from zib_contabil_err
      into wa_zib_contabil_err
      where obj_key = vobj_key.

    if sy-subrc = 0.
      delete from zib_contabil_err where obj_key = vobj_key.
      delete from zib_contabil     where obj_key = vobj_key.
      delete from zib_contabil_err where obj_key = robj_key.
      delete from zib_contabil     where obj_key = robj_key.
    endif.

    select *
      from zfit0066
      into table it_zfit0066
      where  cod_oper      eq 'V'.

    sort it_zfit0066 by tp_ajuste .

    select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

    if sy-subrc ne 0.
      loop at it_zfit0070_aux into wa_zfit0070_aux.
        translate wa_zfit0070_aux-tp_ajuste to upper case.
        loop at it_zfit0066 into wa_zfit0066 where tp_ajuste = wa_zfit0070_aux-tp_ajuste.
          move-corresponding wa_zfit0066 to tg_itens4.

          select single *
             from skat
             into wl_skat
             where spras eq sy-langu and ktopl eq '0050'
             and   saknr = tg_itens4-hkont.
          tg_itens4-txt50 =  wl_skat-txt50.

          select single * from tbsl
            into wl_tbsl
            where koart = 'S'
            and   bschl = tg_itens4-bschl.

          if wa_zfit0070_aux-vlr_extrato_bco = 0.
            if wa_zfit0066-tx_imp = 0 .
              tg_itens4-wrbtr  = wa_zfit0070_aux-vlr_aj_merc.
            else.
              tg_itens4-wrbtr  = wa_zfit0070_aux-vlr_aj_merc * ( wa_zfit0066-tx_imp / 100 ).
            endif.
          else.
            if wa_zfit0066-tx_imp = 0 .
              tg_itens4-wrbtr  = wa_zfit0070_aux-vlr_extrato_bco.
            else.
              tg_itens4-wrbtr  = wa_zfit0070_aux-vlr_extrato_bco * ( wa_zfit0066-tx_imp / 100 ).
            endif.
          endif.

          if wl_tbsl-shkzg = 'H'.
            move 'C' to tg_itens4-shkzg.
            tg_itens4-wrbtr  = tg_itens4-wrbtr  * -1.
          else.
            move 'D' to tg_itens4-shkzg.
          endif.

          select single *
                 from zfit0063
                 into wl_zfit0063
                 where cod_oper = 'S'.

          if sy-subrc = 0.
            tg_itens4-descr = wl_zfit0063-descr.
          endif.

          append tg_itens4.
          clear tg_itens4.
        endloop.

      endloop.
    else.
      message 'Cáculo SWAP Fluxo T.V., já contabilizado, estornar se necessário recalcular' type 'I'.
    endif.
  endif.

  " Contabiliza SWAP Vanila
  if xcod_oper_nav = 'H' or wg_cadpro-cod_oper  = 'T'.
    it_zfit0069_aux[] = it_zfit0069[].
    sort: it_zfit0069     by tp_ajuste,
          it_zfit0069_aux by tp_ajuste.

    delete adjacent duplicates from it_zfit0069_aux comparing tp_ajuste.

    loop at it_zfit0069_aux into wa_zfit0069.
      wa_zfit0069-vlr_aj_merc = 0.
      wa_zfit0069-vlr_extrato_bco = 0.
      modify it_zfit0069_aux from wa_zfit0069 index sy-tabix transporting vlr_aj_merc vlr_extrato_bco.
    endloop.

    loop at it_zfit0069_aux into wa_zfit0069_aux.
      tabix = sy-tabix.

      loop at it_zfit0069 into wa_zfit0069 where tp_ajuste = wa_zfit0069_aux-tp_ajuste.
        if wa_zfit0069-vlr_extrato_bco ne 0.
          add wa_zfit0069-vlr_extrato_bco to wa_zfit0069_aux-vlr_aj_merc.
        else.
          add wa_zfit0069-vlr_aj_merc     to wa_zfit0069_aux-vlr_aj_merc.
        endif.
      endloop.
      if wa_zfit0069_aux-vlr_aj_merc lt 0.
        wa_zfit0069_aux-vlr_aj_merc = wa_zfit0069_aux-vlr_aj_merc * -1.
      endif.
      if wa_zfit0069_aux-vlr_extrato_bco lt 0.
        wa_zfit0069_aux-vlr_extrato_bco = wa_zfit0069_aux-vlr_extrato_bco * -1.
      endif.
      modify it_zfit0069_aux from wa_zfit0069_aux index tabix transporting vlr_aj_merc vlr_extrato_bco.
    endloop.

    clear vobj_key.
    " SWAP Vanila
    select *
      from zfit0069
      into table tl_zfit0069
      where dt_fechamento = wg_cadpro-dt_fechamento
      and   bukrs         = wg_cadpro-bukrs
      and   cod_oper      = 'H'.

    loop at tl_zfit0069.
      if tl_zfit0069-obj_key is not initial.
        vobj_key = tl_zfit0069-obj_key.
        robj_key = tl_zfit0069-obj_key_est.
      endif.
    endloop.

    " Reinicializa Erros
    select single *
      from zib_contabil_err
      into wa_zib_contabil_err
      where obj_key = vobj_key.

    if sy-subrc = 0.
      delete from zib_contabil_err where obj_key = vobj_key.
      delete from zib_contabil     where obj_key = vobj_key.
      delete from zib_contabil_err where obj_key = robj_key.
      delete from zib_contabil     where obj_key = robj_key.
    endif.

    select *
      from zfit0066
      into table it_zfit0066
      where  cod_oper      eq 'H'.

    sort it_zfit0066 by tp_ajuste .

    select  single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key = vobj_key
    and   bktxt   = 'MTM-FINANCEIRO'.

    if sy-subrc ne 0.
      loop at it_zfit0069_aux into wa_zfit0069_aux.
        translate wa_zfit0069_aux-tp_ajuste to upper case.
        loop at it_zfit0066 into wa_zfit0066 where tp_ajuste = wa_zfit0069_aux-tp_ajuste.
          move-corresponding wa_zfit0066 to tg_itens4.

          select single *
             from skat
             into wl_skat
             where spras eq sy-langu and ktopl eq '0050'
             and   saknr = tg_itens4-hkont.
          tg_itens4-txt50 =  wl_skat-txt50.

          select single * from tbsl
            into wl_tbsl
            where koart = 'S'
            and   bschl = tg_itens4-bschl.

          if wa_zfit0069_aux-vlr_extrato_bco = 0.
            if wa_zfit0066-tx_imp = 0 .
              tg_itens4-wrbtr  = wa_zfit0069_aux-vlr_aj_merc.
            else.
              tg_itens4-wrbtr  = wa_zfit0069_aux-vlr_aj_merc * ( wa_zfit0066-tx_imp / 100 ).
            endif.
          else.
            if wa_zfit0066-tx_imp = 0 .
              tg_itens4-wrbtr  = wa_zfit0069_aux-vlr_extrato_bco.
            else.
              tg_itens4-wrbtr  = wa_zfit0069_aux-vlr_extrato_bco  * ( wa_zfit0066-tx_imp / 100 ).
            endif.
          endif.

          if wl_tbsl-shkzg = 'H'.
            move 'C' to tg_itens4-shkzg.
            tg_itens4-wrbtr  = tg_itens4-wrbtr  * -1.
          else.
            move 'D' to tg_itens4-shkzg.
          endif.

          select single *
                 from zfit0063
                 into wl_zfit0063
                 where cod_oper = 'H'.

          if sy-subrc = 0.
            tg_itens4-descr = wl_zfit0063-descr.
          endif.

          append tg_itens4.
          clear tg_itens4.
        endloop.

      endloop.
    else.
      message 'Cáculo SWAP Vanila, já contabilizado, estornar se necessário recalcular' type 'I'.
    endif.
  endif.

  "Grava os já OK, para simples exibição em TODOS
  loop at tg_itens4_aux.
    if tg_itens4_aux-grav = 'X'.
      move-corresponding tg_itens4_aux to tg_itens4.
      append tg_itens4.
    endif.
  endloop.

  if sy-ucomm = 'CALCULAR'. "CS2022001172 contabiliza individualmente (ZIB)
    xcalculo_ctb = 'X'.
  endif.

endform.                    "F_CALCULO_CTB

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
form f_pega_imagem  using    nome_logo
                    changing url.

  refresh graphic_table.
  call method cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    exporting
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    receiving
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  while l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    append graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  endwhile.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  append graphic_table.
  call function 'DP_CREATE_URL'
    exporting
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    tables
      data     = graphic_table
    changing
      url      = url.
endform.                    " F_PEGA_IMAGEM

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_ZIB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_grava_zib .

  data: vseq(10) type p,
        vnum(10) type c.

  data: tg_itens4_aux2 type table of ty_itens4,
        wg_itens4_aux2 type          ty_itens4.

  nmes =  wg_cadpro-dt_fechamento+4(2).
  nano =  wg_cadpro-dt_fechamento+0(4).
  if nmes = '12'.
    nmes = '01'.
    nano = nano + 1.
  else.
    add 1 to nmes.
  endif.
  vmes = nmes.
  vano = nano.

  "busca taxa de cambio dia 01 mes seguinte
  perform busca_tx_cambio.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = vmes
    importing
      output = vmes.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = vano
    importing
      output = vano.

  concatenate vano vmes '01' into data_ini.

  subtract 1 from data_ini.

  loop at tg_itens4.
    if tg_itens4-grav ne 'S'. " os já gravados ignora
      move-corresponding tg_itens4 to wg_itens4_aux2.
      append wg_itens4_aux2 to tg_itens4_aux2.
    endif.
  endloop.

  sort tg_itens4_aux2 by cod_oper.

  delete tg_itens4_aux2 where cod_oper ne xcod_oper_nav. "CS2022001172 contabiliza individualmente (ZIB)

  delete adjacent duplicates from tg_itens4_aux2 comparing cod_oper.
  sort tg_itens4 by cod_oper.
  loop at tg_itens4_aux2 into wg_itens4_aux2.
    " Gera numero do lote
    call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr = '01'
        object      = 'ZID_MTMF'
      importing
        number      = vseq.

    vnum = vseq.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = vnum
      importing
        output = vnum.

    refresh: it_zib_contabil.
    clear: wa_zib_contabil.

    loop at tg_itens4 where cod_oper = wg_itens4_aux2-cod_oper.

      concatenate 'ZMTMF' vnum data_ini+0(4) into  wa_zib_contabil-obj_key.

      wa_zib_contabil-seqitem        = sy-tabix.
      wa_zib_contabil-bschl          = tg_itens4-bschl.

      concatenate   wg_cadpro-bukrs+2(2) '01' into wa_zib_contabil-gsber.

      wa_zib_contabil-bukrs          = wg_cadpro-bukrs.
      wa_zib_contabil-interface      = 35.
      wa_zib_contabil-bktxt          = 'MTM-FINANCEIRO'.

      concatenate data_ini+6(2) '.' data_ini+4(2) '.' data_ini+0(4)   into wa_zib_contabil-bldat.
      concatenate data_ini+6(2) '.' data_ini+4(2) '.' data_ini+0(4)   into wa_zib_contabil-budat.

      wa_zib_contabil-gjahr          = data_ini+0(4).
      wa_zib_contabil-monat          = data_ini+4(2).
      wa_zib_contabil-blart          = 'AB'.

      concatenate data_ini+4(2) '/' data_ini+0(4) into wa_zib_contabil-xblnr.

      wa_zib_contabil-hkont          = tg_itens4-hkont.
      if tg_itens4-wrbtr lt 0.
        wa_zib_contabil-wrbtr          = tg_itens4-wrbtr * -1.
      else.
        wa_zib_contabil-wrbtr          = tg_itens4-wrbtr.
      endif.
      wa_zib_contabil-waers          = 'BRL'.

      wa_zib_contabil-waers_f   = 'USD'.
      wa_zib_contabil-dmbe2     = wa_zib_contabil-wrbtr / xtx_usd .

      concatenate   wg_cadpro-bukrs+2(2) '01' into wa_zib_contabil-bupla.

      wa_zib_contabil-xblnr          = tg_itens4-tp_ajuste.
      wa_zib_contabil-xref3          = tg_itens4-tp_operacao.
      wa_zib_contabil-rg_atualizado  = 'N'.

      insert into  zib_contabil values wa_zib_contabil.
      if sy-subrc ne 0.
        rollback work.
      else.
        commit work.
      endif.
      clear: wa_zib_contabil.

    endloop.

    "Grava estorno dia 01
    concatenate 'ZMTMF' vnum data_ini+0(4) into  vobj_key.
    perform f_estorno_ctb_01 using vobj_key changing robj_key.


    if wg_itens4_aux2-cod_oper = 'N'.
      concatenate 'ZMTMF' vnum wg_cadpro-dt_fechamento+0(4) into  vobj_key.
      loop at it_zfit0064 into wa_zfit0064.
        if  wa_zfit0064-bukrs         eq wg_cadpro-bukrs
        and wa_zfit0064-dt_fechamento eq wg_cadpro-dt_fechamento.
          wa_zfit0064-obj_key     = vobj_key.
          wa_zfit0064-obj_key_est = robj_key.
          modify it_zfit0064 from wa_zfit0064 index sy-tabix transporting obj_key obj_key_est.
        endif.
      endloop.
      "MTM - Financeiro Calculo NDF
      delete from zfit0064 where bukrs         = wg_cadpro-bukrs
                           and   dt_fechamento = wg_cadpro-dt_fechamento.
      modify zfit0064 from table it_zfit0064.
      commit work.

    elseif wg_itens4_aux2-cod_oper = 'S'.
      concatenate 'ZMTMF' vnum wg_cadpro-dt_fechamento+0(4) into  vobj_key.
      loop at it_zfit0067 into wa_zfit0067.
        if  wa_zfit0067-bukrs         eq wg_cadpro-bukrs
        and wa_zfit0067-dt_fechamento eq wg_cadpro-dt_fechamento.
          wa_zfit0067-obj_key = vobj_key.
          wa_zfit0067-obj_key_est = robj_key.
          modify it_zfit0067 from wa_zfit0067 index sy-tabix transporting obj_key obj_key_est.
        endif.
      endloop.
      "MTM – Financeiro Calculo SWAP
      delete from zfit0067 where bukrs         = wg_cadpro-bukrs
                           and   dt_fechamento = wg_cadpro-dt_fechamento.
      modify zfit0067 from table it_zfit0067.
      commit work.
    elseif wg_itens4_aux2-cod_oper = 'H'.
      concatenate 'ZMTMF' vnum wg_cadpro-dt_fechamento+0(4) into  vobj_key.
      loop at it_zfit0069 into wa_zfit0069.
        if  wa_zfit0069-bukrs         eq wg_cadpro-bukrs
        and wa_zfit0069-dt_fechamento eq wg_cadpro-dt_fechamento.
          wa_zfit0069-obj_key = vobj_key.
          wa_zfit0069-obj_key_est = robj_key.
          modify it_zfit0069 from wa_zfit0069 index sy-tabix transporting obj_key obj_key_est.
        endif.
      endloop.

      "MTM – Financeiro – SWAP VANILA
      delete from zfit0069 where bukrs         = wg_cadpro-bukrs
                           and   dt_fechamento = wg_cadpro-dt_fechamento.
      modify zfit0069 from table it_zfit0069.
      commit work.
    elseif wg_itens4_aux2-cod_oper = 'V'.
      concatenate 'ZMTMF' vnum wg_cadpro-dt_fechamento+0(4) into  vobj_key.
      loop at it_zfit0070 into wa_zfit0070.
        if  wa_zfit0070-bukrs         eq wg_cadpro-bukrs
        and wa_zfit0070-dt_fechamento eq wg_cadpro-dt_fechamento.
          wa_zfit0070-obj_key = vobj_key.
          wa_zfit0070-obj_key_est = robj_key.
          modify it_zfit0070 from wa_zfit0070 index sy-tabix transporting obj_key obj_key_est.
        endif.
      endloop.
      "MTM – Financeiro Calculo SWAP Com CDI
      delete from zfit0070 where bukrs         = wg_cadpro-bukrs
                           and   dt_fechamento = wg_cadpro-dt_fechamento.
      modify zfit0070 from table it_zfit0070.
      commit work.
    endif.

    "CS2022001172 contabiliza individualmente (ZIB)
    if wg_cadpro-cod_oper = 'T'. " Todos
      "MTM - Financeiro Calculo NDF
      delete from zfit0064 where bukrs         = wg_cadpro-bukrs
                           and   dt_fechamento = wg_cadpro-dt_fechamento
                           and   obj_key       = ''.
*      IF sy-subrc = 0.
      modify zfit0064 from table it_zfit0064.
*      ENDIF.
      "
      "MTM – Financeiro Calculo SWAP
      delete from zfit0067 where bukrs         = wg_cadpro-bukrs
                           and   dt_fechamento = wg_cadpro-dt_fechamento
                           and   obj_key       = ''.
*      IF sy-subrc = 0.
      modify zfit0067 from table it_zfit0067.
*      ENDIF.
      "
      "MTM – Financeiro – SWAP VANILA
      delete from zfit0069 where bukrs         = wg_cadpro-bukrs
                           and   dt_fechamento = wg_cadpro-dt_fechamento
                           and   obj_key       = ''.
*      IF sy-subrc = 0.
      modify zfit0069 from table it_zfit0069.
*      ENDIF.
      "
      "MTM – Financeiro Calculo SWAP Com CDI
      delete from zfit0070 where bukrs         = wg_cadpro-bukrs
                           and   dt_fechamento = wg_cadpro-dt_fechamento
                           and   obj_key       = ''.
*      IF sy-subrc = 0.
      modify zfit0070 from table it_zfit0070.
*      ENDIF.
      commit work.
    endif.
    "CS2022001172 contabiliza individualmente (ZIB)

  endloop.

  if tg_itens4[] is not initial.
    clear xcalculo_ctb.
    xbloqueio = 'X'.
    xcalculo = 'X'.
    clear xestorno_botao.
    clear: wg_cadpro-belnr, wg_cadpro-belnr2.
    if xgravado ne 'X'. " NÃO GRAVADO CALCULOS ANTERIORES
      perform grava_dados.
      xgravado = 'X'.
    endif.
    message s836(sd) with 'Contabilização'
                      ',Realizada com sucesso!'.
  endif.
endform.                    " F_GRAVA_ZIB
*&---------------------------------------------------------------------*
*&      Form  F_CRIA_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_cria_objetos.
  data: event       type cntl_simple_event,
        events      type cntl_simple_events,
        tl_filter   type lvc_t_filt,
        wl_filter   type lvc_s_filt,
        tl_function type ui_functions,
        wl_function like tl_function with header line,
        lt_f4       type lvc_t_f4    with header line.

  if xcod_oper_nav is initial.
    if wg_cadpro-cod_oper = 'T'.
      xcod_oper_nav = 'N'.
    else.
      xcod_oper_nav = wg_cadpro-cod_oper.
    endif.
  endif.

  if container is initial.
    "Header
    create object obj_dyndoc_id
      exporting
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
        no_margins = 'X'.

    perform zf_alv_header .

    if editcontainer is initial .
      create object editcontainer
        exporting
          container_name = 'HEADER'.
    endif .

    call method obj_dyndoc_id->merge_document.

    call method obj_dyndoc_id->display_document
      exporting
        reuse_control      = 'X'
        parent             = editcontainer
      exceptions
        html_display_error = 1.

    "grafico 1
    call method cl_gui_cfw=>flush.
    create object:
      container exporting container_name = 'CC_IMG',
      picture exporting parent = container.

    perform f_pega_imagem using 'LOGO_MAGGI_M' changing url.

    call method picture->load_picture_from_url
      exporting
        url = url.

    call method picture->set_display_mode
      exporting
        display_mode = picture->display_mode_fit_center.

    wa_event_picture-eventid = cl_gui_picture=>eventid_picture_click.
    wa_event_picture-appl_event = 'X'.
    append wa_event_picture to it_event_picture.
    call method picture->set_registered_events
      exporting
        events                    = it_event_picture
      exceptions
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        others                    = 4.

*    CREATE OBJECT OBJ_CL_PICTURE_CLASS.
*    SET HANDLER OBJ_CL_PICTURE_CLASS->PICTURE_YCLICK FOR PICTURE.

    "grafico 2
    call method cl_gui_cfw=>flush.
    create object:
      container1 exporting container_name = 'CC_IMG1',
      picture1 exporting parent = container1.

    perform f_pega_imagem using 'LOGO_MAGGI_M' changing url.

    call method picture1->load_picture_from_url
      exporting
        url = url.
    call method picture1->set_display_mode
      exporting
        display_mode = picture1->display_mode_fit_center.

*    WA_EVENT_PICTURE-EVENTID = CL_GUI_PICTURE=>EVENTID_PICTURE_CLICK.
*    WA_EVENT_PICTURE-APPL_EVENT = 'X'.
*    APPEND WA_EVENT_PICTURE TO IT_EVENT_PICTURE.
    call method picture1->set_registered_events
      exporting
        events                    = it_event_picture
      exceptions
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        others                    = 4.

*    CREATE OBJECT OBJ_CL_PICTURE_CLASS.
*    SET HANDLER OBJ_CL_PICTURE_CLASS->PICTURE_YCLICK1 FOR PICTURE1.

    "grafico 3
    call method cl_gui_cfw=>flush.
    create object:
      container2 exporting container_name = 'CC_IMG2',
      picture2 exporting parent = container2.

    perform f_pega_imagem using 'LOGO_MAGGI_M' changing url.

    call method picture2->load_picture_from_url
      exporting
        url = url.
    call method picture2->set_display_mode
      exporting
        display_mode = picture2->display_mode_fit_center.

*    WA_EVENT_PICTURE-EVENTID = CL_GUI_PICTURE=>EVENTID_PICTURE_CLICK.
*    WA_EVENT_PICTURE-APPL_EVENT = 'X'.
*    APPEND WA_EVENT_PICTURE TO IT_EVENT_PICTURE.
    call method picture2->set_registered_events
      exporting
        events                    = it_event_picture
      exceptions
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        others                    = 4.

*    CREATE OBJECT OBJ_CL_PICTURE_CLASS.
*    SET HANDLER OBJ_CL_PICTURE_CLASS->PICTURE_YCLICK1 FOR PICTURE1.

    "grafico 4
    call method cl_gui_cfw=>flush.
    create object:
      container3 exporting container_name = 'CC_IMG3',
      picture3 exporting parent = container3.

    perform f_pega_imagem using 'LOGO_MAGGI_M' changing url.

    call method picture3->load_picture_from_url
      exporting
        url = url.
    call method picture3->set_display_mode
      exporting
        display_mode = picture3->display_mode_fit_center.

*    WA_EVENT_PICTURE-EVENTID = CL_GUI_PICTURE=>EVENTID_PICTURE_CLICK.
*    WA_EVENT_PICTURE-APPL_EVENT = 'X'.
*    APPEND WA_EVENT_PICTURE TO IT_EVENT_PICTURE.
    call method picture3->set_registered_events
      exporting
        events                    = it_event_picture
      exceptions
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        others                    = 4.

*    CREATE OBJECT OBJ_CL_PICTURE_CLASS.
*    SET HANDLER OBJ_CL_PICTURE_CLASS->PICTURE_YCLICK1 FOR PICTURE1.
  else.
    perform zf_alv_header .
    call method obj_dyndoc_id->display_document
      exporting
        reuse_control      = 'X'
        parent             = editcontainer
      exceptions
        html_display_error = 1.

  endif.

  if g_tab_tela-subtela = '0214'. " Tela Final
    exit.
  endif.

  if g_custom_container is initial.
*    WA_LAYOUT-CWIDTH_OPT = C_X.
    wa_layout-zebra      = c_x.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
    wa_layout-no_rowmark = c_x.
*    WA_LAYOUT-COL_OPT    = C_X.
    wa_stable-row        = c_x.
    wa_layout-sel_mode   = 'A'.
    wa_layout-cwidth_opt   = 'X'.
    wa_layout-box_fname    = 'MARK'.
    clear wa_layout-ctab_fname.

    gs_variant_c-report      = sy-repid.

    create object g_custom_container
      exporting
        container_name = g_container.

    create object splitter
      exporting
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    call method splitter->get_container
      exporting
        row       = 2
        column    = 1
      receiving
        container = container_1.

    create object grid1
      exporting
        i_parent = container_1.


    if g_tab_tela-subtela = '0210'.
      perform montar_layout.
    elseif g_tab_tela-subtela = '0211'.
      perform montar_layout2.
    elseif g_tab_tela-subtela = '0212'.
      if xcod_oper_nav = 'N'.
        wa_layout-ctab_fname = 'COLOR'.
        perform montar_layout3.
      elseif xcod_oper_nav = 'S'. " Swap Fluxo Caixa T.F.
        wa_layout-ctab_fname = 'COLOR'.
        perform montar_layout5.
      elseif xcod_oper_nav = 'H'. " Swap Vanila
        wa_layout-ctab_fname = 'COLOR'.
        tg_itens6[] = tg_itens6_aux[].
        if wg_cadpro-p_divida = 'X'.
          delete  tg_itens6 where tipo_opera ne 'DIVIDA'.
          perform montar_layout6_div.
        else.
          delete  tg_itens6 where tipo_opera eq 'DIVIDA'.
          perform montar_layout6.
        endif.
      elseif xcod_oper_nav = 'V'. " Swap Fluxo Caixa T.V.
        wa_layout-ctab_fname = 'COLOR'.
        perform montar_layout7.
      else.
        tg_itens6[] = tg_itens6_aux[].
        if wg_cadpro-p_divida = 'X'.
          delete  tg_itens6 where tipo_opera ne 'DIVIDA'.
          perform montar_layout6_div.
        else.
          delete  tg_itens6 where tipo_opera eq 'DIVIDA'.
          perform montar_layout6.
        endif.
      endif.
    elseif g_tab_tela-subtela = '0213'.
      if ok-code = 'PICK'.
        perform montar_layout_err.
      else.
        perform montar_layout4.
        if vg_extrato =  'X' and xcalculo_ctb ne 'X'.
          refresh tg_itens4.
          perform  f_busca_contabil.
        elseif  vg_extrato =  'X'.
          perform f_calculo_ctb.
        endif.
      endif.
    endif.


    create object obg_toolbar
      exporting
        io_alv_grid = grid1.

    if  g_tab_tela-subtela = '0210'.
*      * Register event handler
      set handler obg_toolbar->on_toolbar for grid1.
      set handler obg_toolbar->handle_user_command for grid1.
    endif.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    append wl_function to tl_function.

    wa_layout-no_toolbar = ''.
    if g_tab_tela-subtela = '0210'.
      call method grid1->set_table_for_first_display
        exporting
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
        changing
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = tg_itens[].

      call method grid1->register_edit_event
        exporting
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      call method grid1->register_edit_event
        exporting
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      set handler:
                lcl_event_handler=>on_data_changed_finished for grid1,
                lcl_event_handler=>on_data_changed for grid1.
    elseif  g_tab_tela-subtela = '0211'.
      call method grid1->set_table_for_first_display
        exporting
          it_toolbar_excluding = tl_function
          is_variant           = gs_variant_c
          is_layout            = wa_layout
          i_save               = 'X'
          i_default            = 'X'
        changing
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = tg_itens2[].
    elseif g_tab_tela-subtela = '0212'. "Cálculos NDF SWAP
      if xcod_oper_nav = 'N'.
        refresh t_sort.
        call method grid1->set_table_for_first_display
          exporting
            it_toolbar_excluding = tl_function
            is_variant           = gs_variant_c
            is_layout            = wa_layout
            i_save               = 'X'
            i_default            = 'X'
          changing
            it_fieldcatalog      = t_fieldcatalog[]
            it_sort              = t_sort[]
            it_outtab            = tg_itens3[].
        set handler:
               lcl_event_handler=>on_data_changed3 for grid1.
      elseif xcod_oper_nav = 'S'. " Swap Fluxo de Caixa T.F.
        call method grid1->set_table_for_first_display
          exporting
            it_toolbar_excluding = tl_function
            is_variant           = gs_variant_c
            is_layout            = wa_layout
            i_save               = 'X'
            i_default            = 'X'
          changing
            it_fieldcatalog      = t_fieldcatalog[]
            it_outtab            = tg_itens5[].
        set handler:
                 lcl_event_handler=>on_data_changed5 for grid1.

      elseif xcod_oper_nav = 'V'. " Swap Fluxo de Caixa T.V.
        call method grid1->set_table_for_first_display
          exporting
            it_toolbar_excluding = tl_function
            is_variant           = gs_variant_c
            is_layout            = wa_layout
            i_save               = 'X'
            i_default            = 'X'
          changing
            it_fieldcatalog      = t_fieldcatalog[]
            it_outtab            = tg_itens7[].
        set handler:
               lcl_event_handler=>on_data_changed7 for grid1.

      elseif xcod_oper_nav = 'H'. " Swap Vanila
        call method grid1->set_table_for_first_display
          exporting
            it_toolbar_excluding = tl_function
            is_variant           = gs_variant_c
            is_layout            = wa_layout
            i_save               = 'X'
            i_default            = 'X'
          changing
            it_fieldcatalog      = t_fieldcatalog[]
            it_outtab            = tg_itens6[].
        set handler:
               lcl_event_handler=>on_data_changed6 for grid1.
      else.
        call method grid1->set_table_for_first_display
          exporting
            it_toolbar_excluding = tl_function
            is_layout            = wa_layout
          changing
            it_fieldcatalog      = t_fieldcatalog[]
            it_outtab            = tg_itens6[].
        set handler:
       lcl_event_handler=>on_data_changed for grid1.
      endif.
    elseif g_tab_tela-subtela = '0213'.
      if ok-code = 'PICK'.
        call method grid1->set_table_for_first_display
          exporting
            it_toolbar_excluding = tl_function
            is_variant           = gs_variant_c
            is_layout            = wa_layout
            i_save               = 'X'
            i_default            = 'X'
          changing
            it_fieldcatalog      = t_fieldcatalog[]
            it_outtab            = it_zib_contabil_err[].
      else.
        clear vg_i.
        refresh t_sort.

        mc_preenche_class:   'DESCR'       '' 'X' 'X',
*                             'TP_OPERACAO' '' 'X' ' ',
                             'TP_AJUSTE'   '' 'X' ' '.

        refresh tg_itens4_aux.
        loop at tg_itens4.
          if xcod_oper_nav = tg_itens4-cod_oper.
            move-corresponding tg_itens4 to tg_itens4_aux.
            append tg_itens4_aux.
          endif.
        endloop.

        call method grid1->set_table_for_first_display
          exporting
            it_toolbar_excluding = tl_function
            is_variant           = gs_variant_c
            is_layout            = wa_layout
            i_save               = 'X'
            i_default            = 'X'
          changing
            it_fieldcatalog      = t_fieldcatalog[]
            it_sort              = t_sort[]
            it_outtab            = tg_itens4_aux[].
      endif.
    endif.

*    posiciona spliter na altura x
    call method splitter->set_row_height
      exporting
        id     = 2
        height = 100.
  else.
    if g_tab_tela-subtela = '0213'.
      refresh tg_itens4_aux.
      loop at tg_itens4.
        if xcod_oper_nav = tg_itens4-cod_oper.
          move-corresponding tg_itens4 to tg_itens4_aux.
          append tg_itens4_aux.
        endif.
      endloop.
    endif.

    if xcod_oper_nav = 'H' and g_tab_tela-subtela = '0212'.
      tg_itens6[] = tg_itens6_aux[].
      if wg_cadpro-p_divida = 'X'.
        delete  tg_itens6 where tipo_opera ne 'DIVIDA'.
        perform montar_layout6_div.
      else.
        delete  tg_itens6 where tipo_opera eq 'DIVIDA'.
        perform montar_layout6.
      endif.
      call method grid1->set_frontend_fieldcatalog
        exporting
          it_fieldcatalog = t_fieldcatalog[].
    else.
      call method grid1->set_frontend_fieldcatalog
        exporting
          it_fieldcatalog = t_fieldcatalog[].
    endif.

    call method grid1->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.

  if obg_descbox is initial and g_tab_tela-subtela ne '0213'.
    call method splitter->get_container
      exporting
        row       = 1
        column    = 1
      receiving
        container = container_2.

    create object obg_descbox
      exporting
        parent            = container_2
        wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position = 72
        max_number_chars  = 350.

    call method obg_descbox->set_toolbar_mode
      exporting
        toolbar_mode = '0'.

    call method obg_descbox->set_readonly_mode
      exporting
        readonly_mode = 0.

    if g_tab_tela-subtela = '0210'.
      call method obg_descbox->set_text_as_r3table
        exporting
          table = tg_editor.
    elseif g_tab_tela-subtela = '0211'.
      call method obg_descbox->set_text_as_r3table
        exporting
          table = tg_editor1.
    elseif g_tab_tela-subtela = '0212'.
      call method obg_descbox->set_text_as_r3table
        exporting
          table = tg_editor2.
    endif.
  endif.

endform.                    " F_CRIA_OBJETOS
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_alv_header .
  data:   wl_data(10),
                 wl_hora(8),
                 wl_linha(60),
                 wl_text type sdydo_text_element.
  wl_text = wg_cadpro-mensagem.

  call method obj_dyndoc_id->initialize_document.

  call method obj_dyndoc_id->add_text
    exporting
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.
endform.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_CTB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_estorno_ctb using pdata pobj_key .
  data: vseq(10) type p,
        vnum(10) type c.

  " Gera numero do lote
  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr = '01'
      object      = 'ZID_MTMF'
    importing
      number      = vseq.

  vnum = vseq.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = vnum
    importing
      output = vnum.

  refresh: it_zib_contabil.
  clear: wa_zib_contabil.
  select *
    from zib_contabil
    into table it_zib_contabil_est
    where obj_key = pobj_key.

  " novo obj_key.
  concatenate 'ZMTMF' vnum pdata+0(4) into  vobj_key.
  loop at it_zib_contabil_est into wa_zib_contabil_est.
    wa_zib_contabil_est-obj_key = vobj_key.
    if wa_zib_contabil_est-bschl = '40'.
      wa_zib_contabil_est-bschl = '50'.
    else.
      wa_zib_contabil_est-bschl = '40'.
    endif.
    wa_zib_contabil_est-bktxt          = 'ESTORNO MTM-FINANCEIRO'.
    wa_zib_contabil_est-rg_atualizado  = 'N'.
    modify it_zib_contabil_est from wa_zib_contabil_est index sy-tabix transporting bschl obj_key bktxt rg_atualizado.
  endloop.

  if xcod_oper_nav = 'N'. "NDF
    loop at it_zfit0064 into wa_zfit0064.
      if  wa_zfit0064-bukrs         eq wg_cadpro-bukrs
      and wa_zfit0064-dt_fechamento eq wg_cadpro-dt_fechamento.
        if pdata+6(2) = '01'. " Estorno
          wa_zfit0064-obj_key_est = vobj_key.
          modify it_zfit0064 from wa_zfit0064 index sy-tabix transporting obj_key_est.
        else.
          wa_zfit0064-obj_key = vobj_key.
          modify it_zfit0064 from wa_zfit0064 index sy-tabix transporting obj_key.
        endif.
      endif.
    endloop.
    modify zfit0064 from table it_zfit0064.
  elseif xcod_oper_nav = 'S'. "SWAP Fluxo de caixa T.F.
    loop at it_zfit0067 into wa_zfit0067.
      if  wa_zfit0067-bukrs         eq wg_cadpro-bukrs
      and wa_zfit0067-dt_fechamento eq wg_cadpro-dt_fechamento.
        if pdata+6(2) = '01'. " Estorno
          wa_zfit0067-obj_key_est = vobj_key.
          modify it_zfit0067 from wa_zfit0067 index sy-tabix transporting obj_key_est.
        else.
          wa_zfit0067-obj_key = vobj_key.
          modify it_zfit0067 from wa_zfit0067 index sy-tabix transporting obj_key.
        endif.
      endif.
    endloop.
    modify zfit0067 from table it_zfit0067.
  elseif xcod_oper_nav = 'H'. " SWAP Vanila
    loop at it_zfit0069 into wa_zfit0069.
      if  wa_zfit0069-bukrs         eq wg_cadpro-bukrs
      and wa_zfit0069-dt_fechamento eq wg_cadpro-dt_fechamento.
        if pdata+6(2) = '01'. " Estorno
          wa_zfit0069-obj_key_est = vobj_key.
          modify it_zfit0069 from wa_zfit0069 index sy-tabix transporting obj_key_est.
        else.
          wa_zfit0069-obj_key = vobj_key.
          modify it_zfit0069 from wa_zfit0069 index sy-tabix transporting obj_key.
        endif.
      endif.
    endloop.
    modify zfit0069 from table it_zfit0069.
  elseif xcod_oper_nav = 'V'. "SWAP Fluxo de caixa T.F.
    loop at it_zfit0070 into wa_zfit0070.
      if  wa_zfit0070-bukrs         eq wg_cadpro-bukrs
      and wa_zfit0070-dt_fechamento eq wg_cadpro-dt_fechamento.
        if pdata+6(2) = '01'. " Estorno
          wa_zfit0070-obj_key_est = vobj_key.
          modify it_zfit0070 from wa_zfit0070 index sy-tabix transporting obj_key_est.
        else.
          wa_zfit0070-obj_key = vobj_key.
          modify it_zfit0070 from wa_zfit0070 index sy-tabix transporting obj_key.
        endif.
      endif.
    endloop.
    modify zfit0070 from table it_zfit0070.
  endif.

  modify zib_contabil from table it_zib_contabil_est.
  commit work.

  refresh: it_zib_contabil_est, tg_itens4.

  xestorno = 'X'.
  xestorno_botao = 'X'.

  wg_cadpro-belnr  =  icon_alert.
  wg_cadpro-belnr2 =  icon_alert.
  message s836(sd) with 'Estorno'
                      ',Realizado com sucesso!'.
endform.                    " F_ESTORNO_CTB

*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_CTB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_estorno_ctb_01 using p_obj_key changing robj_key.
  data: vseq(10)  type p,
        vnum(10)  type c,
        vdata01   type sy-datum,
        tl_tcurr  type table of tcurr,
        wl_tcurr  type tcurr,
        vdatax    type sy-datum,
        vdata     type tcurr-gdatu,
        vdata_ini type tcurr-gdatu.

  data: chdat(8)   type c,
        houtput(8) type n.


  vdata01 = data_ini + 1.
  " Gera numero do lote
  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr = '01'
      object      = 'ZID_MTMF'
    importing
      number      = vseq.

  vnum = vseq.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = vnum
    importing
      output = vnum.

  refresh: it_zib_contabil.
  clear: wa_zib_contabil.
  select *
    from zib_contabil
    into table it_zib_contabil_est
    where obj_key = p_obj_key.

  " novo obj_key.
  concatenate 'ZMTMF' vnum vdata01+0(4) into  robj_key.
  loop at it_zib_contabil_est into wa_zib_contabil_est.
    wa_zib_contabil_est-obj_key = robj_key.
    if wa_zib_contabil_est-bschl = '40'.
      wa_zib_contabil_est-bschl = '50'.
    else.
      wa_zib_contabil_est-bschl = '40'.
    endif.
    wa_zib_contabil_est-bktxt          = 'MTM-FINANCEIRO ESTORNO'.
    concatenate vdata01+6(2) '.' vdata01+4(2) '.' vdata01+0(4) into wa_zib_contabil_est-bldat.
    concatenate vdata01+6(2) '.' vdata01+4(2) '.' vdata01+0(4) into wa_zib_contabil_est-budat.

    wa_zib_contabil_est-gjahr          = vdata01+0(4).
    wa_zib_contabil_est-monat          = vdata01+4(2).

*    IF WL_TCURR-UKURS GT 0.
*      WA_ZIB_CONTABIL_EST-WAERS_F = 'USD'.
*      WA_ZIB_CONTABIL_EST-DMBE2 =  WA_ZIB_CONTABIL_EST-WRBTR / WL_TCURR-UKURS.
*    ENDIF.

    concatenate vdata01+4(2) '/' vdata01+0(4) into wa_zib_contabil_est-xblnr.

    wa_zib_contabil_est-rg_atualizado  = 'N'.
    modify it_zib_contabil_est from wa_zib_contabil_est index sy-tabix transporting bschl obj_key bktxt bldat budat gjahr monat rg_atualizado .
  endloop.

  modify zib_contabil from table it_zib_contabil_est.
  commit work.

  refresh: it_zib_contabil_est.

endform.                    " F_ESTORNO_CTB

*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_busca_contabil .

  data: tl_zfit0064 type table of zfit0064 with header line,
        tl_zfit0066 type table of zfit0066 with header line,
        tl_zfit0067 type table of zfit0067 with header line,
        tl_zfit0069 type table of zfit0069 with header line,
        tl_zfit0070 type table of zfit0070 with header line,
        wl_skat     type skat,
        wl_tbsl     type tbsl.

  clear: xgravado, vobj_key, robj_key.
  if xcalculo_ctb = 'X'. "Não traz informações já gravada se foi feito calculo, pois as tabelas foram modificadas
    exit.
  endif.

  if xcod_oper_nav is initial.
    if wg_cadpro-cod_oper = 'T'.
      xcod_oper_nav = 'N'.
    else.
      xcod_oper_nav = wg_cadpro-cod_oper.
    endif.
  endif.


  if xcod_oper_nav = 'N' .
    " NDF
    select *
      from zfit0064
      into table tl_zfit0064
      where dt_fechamento = wg_cadpro-dt_fechamento
      and   bukrs         = wg_cadpro-bukrs
      and   cod_oper      = 'N'.

    if tl_zfit0064[] is not initial.
      refresh: tg_itens3, it_zfit0064.
      loop at tl_zfit0064.
        clear tl_zfit0064-vlr_extrato_bco.
        select single vlr_extrato_bco
          from zfit0214
          into tl_zfit0064-vlr_extrato_bco
          where dt_fechamento = wg_cadpro-dt_fechamento
          and   bukrs         = wg_cadpro-bukrs
          and   nro_cto       = tl_zfit0064-nro_cto
          and   cod_oper      = 'N'.
        "
        if tl_zfit0064-vlr_extrato_bco ne 0.
          if tl_zfit0064-vlr_extrato_bco gt 0.
            if tl_zfit0064-tp_ajuste = 'ATIVO' or tl_zfit0064-tp_ajuste = 'PASSIVO'.
              tl_zfit0064-tp_ajuste = 'ATIVO'.
            else.
              tl_zfit0064-tp_ajuste = 'ATIVOLP'.
            endif.
          else.
            if tl_zfit0064-tp_ajuste = 'ATIVO' or tl_zfit0064-tp_ajuste = 'PASSIVO'.
              tl_zfit0064-tp_ajuste = 'PASSIVO'.
            else.
              tl_zfit0064-tp_ajuste = 'PASSIVOLP'.
            endif.
          endif.
        elseif tl_zfit0064-vlr_pres_mtm ne 0.
          if tl_zfit0064-vlr_pres_mtm gt 0.
            if tl_zfit0064-tp_ajuste = 'ATIVO' or tl_zfit0064-tp_ajuste = 'PASSIVO'.
              tl_zfit0064-tp_ajuste = 'ATIVO'.
            else.
              tl_zfit0064-tp_ajuste = 'ATIVOLP'.
            endif.
          else.
            if tl_zfit0064-tp_ajuste = 'ATIVO' or tl_zfit0064-tp_ajuste = 'PASSIVO'.
              tl_zfit0064-tp_ajuste = 'PASSIVO'.
            else.
              tl_zfit0064-tp_ajuste = 'PASSIVOLP'.
            endif.
          endif.
        endif.
        "
        move-corresponding tl_zfit0064 to tg_itens3.
        tg_itens3-ano = tg_itens3-dt_fim_cto+0(4).
        clear: wl_color.
        refresh tg_itens3-color.
        wl_color-fieldname = 'VLR_EXTRATO_BCO'.
        wl_color-color-col = 3.
        wl_color-color-inv = 3.
        append wl_color to tg_itens3-color.
        append tg_itens3.
        clear: tg_itens3.
        move-corresponding tl_zfit0064 to wa_zfit0064.
        if tl_zfit0064-obj_key is not initial.
          vobj_key = tl_zfit0064-obj_key.
          robj_key = tl_zfit0064-obj_key_est.
        endif.
        append wa_zfit0064 to it_zfit0064.
        xgravado = 'X'.
      endloop.
    endif.
  endif.

  if xcod_oper_nav = 'S' .
    " SWAP S
    select *
      from zfit0067
      into table tl_zfit0067
      where dt_fechamento = wg_cadpro-dt_fechamento
      and   bukrs         = wg_cadpro-bukrs
      and   cod_oper      = 'S'.

    if tl_zfit0067[] is not initial.
      refresh: tg_itens5,it_zfit0067.
      loop at tl_zfit0067.
        clear tl_zfit0067-vlr_extrato_bco.
        select single vlr_extrato_bco
          from zfit0214
          into tl_zfit0067-vlr_extrato_bco
          where dt_fechamento = wg_cadpro-dt_fechamento
          and   bukrs         = wg_cadpro-bukrs
          and   nro_cto       = tl_zfit0067-nro_cto
          and   nro_par       = tl_zfit0067-nro_par
          and   cod_oper      = 'S'.
        "
        if tl_zfit0067-vlr_extrato_bco ne 0.
          if tl_zfit0067-vlr_extrato_bco gt 0.
            if tl_zfit0067-tp_ajuste = 'ATIVO' or tl_zfit0067-tp_ajuste = 'PASSIVO'.
              tl_zfit0067-tp_ajuste = 'ATIVO'.
            else.
              tl_zfit0067-tp_ajuste = 'ATIVOLP'.
            endif.
          else.
            if tl_zfit0067-tp_ajuste = 'ATIVO' or tl_zfit0067-tp_ajuste = 'PASSIVO'.
              tl_zfit0067-tp_ajuste = 'PASSIVO'.
            else.
              tl_zfit0067-tp_ajuste = 'PASSIVOLP'.
            endif.
          endif.
        elseif tl_zfit0067-vlr_aj_merc ne 0.
          if tl_zfit0067-vlr_aj_merc gt 0.
            if tl_zfit0067-tp_ajuste = 'ATIVO' or tl_zfit0067-tp_ajuste = 'PASSIVO'.
              tl_zfit0067-tp_ajuste = 'ATIVO'.
            else.
              tl_zfit0067-tp_ajuste = 'ATIVOLP'.
            endif.
          else.
            if tl_zfit0067-tp_ajuste = 'ATIVO' or tl_zfit0067-tp_ajuste = 'PASSIVO'.
              tl_zfit0067-tp_ajuste = 'PASSIVO'.
            else.
              tl_zfit0067-tp_ajuste = 'PASSIVOLP'.
            endif.
          endif.
        endif.
        "
        "
        clear: wl_color.
        refresh tg_itens5-color.
        wl_color-fieldname = 'VLR_EXTRATO_BCO'.
        wl_color-color-col = 3.
        wl_color-color-inv = 3.
        append wl_color to tg_itens5-color.
        move-corresponding tl_zfit0067 to tg_itens5.
        tg_itens5-ano = tg_itens5-dt_fim_cto+0(4).
        append tg_itens5.
        clear: tg_itens5.
        move-corresponding tl_zfit0067 to wa_zfit0067.
        if tl_zfit0067-obj_key is not initial.
          vobj_key = tl_zfit0067-obj_key.
          robj_key = tl_zfit0067-obj_key_est.
        endif.
        append wa_zfit0067 to it_zfit0067.
        xgravado = 'X'.
      endloop.
    endif.
  endif.

  if xcod_oper_nav = 'V' .
    " SWAP V
    select *
      from zfit0070
      into table tl_zfit0070
      where dt_fechamento = wg_cadpro-dt_fechamento
      and   bukrs         = wg_cadpro-bukrs
      and   cod_oper      = 'V'.

    if tl_zfit0070[] is not initial.
      refresh: tg_itens7,it_zfit0070.
      loop at tl_zfit0070.
        clear tl_zfit0070-vlr_extrato_bco.
        select single vlr_extrato_bco
         from zfit0214
         into tl_zfit0070-vlr_extrato_bco
         where dt_fechamento = wg_cadpro-dt_fechamento
         and   bukrs         = wg_cadpro-bukrs
         and   nro_cto       = tl_zfit0070-nro_cto
         and   nro_par       = tl_zfit0070-nro_par
         and   cod_oper      = 'V'.
        "
        if tl_zfit0070-vlr_extrato_bco ne 0.
          if tl_zfit0070-vlr_extrato_bco gt 0.
            if tl_zfit0070-tp_ajuste = 'ATIVO' or tl_zfit0070-tp_ajuste = 'PASSIVO'.
              tl_zfit0070-tp_ajuste = 'ATIVO'.
            else.
              tl_zfit0070-tp_ajuste = 'ATIVOLP'.
            endif.
          else.
            if tl_zfit0070-tp_ajuste = 'ATIVO' or tl_zfit0070-tp_ajuste = 'PASSIVO'.
              tl_zfit0070-tp_ajuste = 'PASSIVO'.
            else.
              tl_zfit0070-tp_ajuste = 'PASSIVOLP'.
            endif.
          endif.
        elseif tl_zfit0070-vlr_aj_merc ne 0.
          if tl_zfit0070-vlr_aj_merc gt 0.
            if tl_zfit0070-tp_ajuste = 'ATIVO' or tl_zfit0070-tp_ajuste = 'PASSIVO'.
              tl_zfit0070-tp_ajuste = 'ATIVO'.
            else.
              tl_zfit0070-tp_ajuste = 'ATIVOLP'.
            endif.
          else.
            if tl_zfit0070-tp_ajuste = 'ATIVO' or tl_zfit0070-tp_ajuste = 'PASSIVO'.
              tl_zfit0070-tp_ajuste = 'PASSIVO'.
            else.
              tl_zfit0070-tp_ajuste = 'PASSIVOLP'.
            endif.
          endif.
        endif.
        "
        refresh tg_itens7-color.
        wl_color-fieldname = 'VLR_EXTRATO_BCO'.
        wl_color-color-col = 3.
        wl_color-color-inv = 3.
        append wl_color to tg_itens7-color.
        move-corresponding tl_zfit0070 to tg_itens7.
        tg_itens7-ano = tg_itens7-dt_fim_cto+0(4).
        append tg_itens7.
        clear: tg_itens7.
        move-corresponding tl_zfit0070 to wa_zfit0070.
        if tl_zfit0070-obj_key is not initial.
          vobj_key = tl_zfit0070-obj_key.
          robj_key = tl_zfit0070-obj_key_est.
        endif.
        append wa_zfit0070 to it_zfit0070.
        xgravado = 'X'.
      endloop.
    endif.
  endif.

  if xcod_oper_nav = 'H' .
    " SWAP Vanila
    select *
      from zfit0069
      into table tl_zfit0069
      where dt_fechamento = wg_cadpro-dt_fechamento
      and   bukrs         = wg_cadpro-bukrs
      and   cod_oper      = 'H'.

    if tl_zfit0069[] is not initial.
      refresh: tg_itens6,it_zfit0069.
      loop at tl_zfit0069.
        clear tl_zfit0069-vlr_extrato_bco.
        select single vlr_extrato_bco
          from zfit0214
          into tl_zfit0069-vlr_extrato_bco
          where dt_fechamento = wg_cadpro-dt_fechamento
          and   bukrs         = wg_cadpro-bukrs
          and   nro_cto       = tl_zfit0069-nro_cto
          and   dt_fim_cto    = tl_zfit0069-dt_fim_cto
          and   cod_oper      = 'H'.

        if tl_zfit0069-vlr_extrato_bco ne 0.
          if tl_zfit0069-vlr_extrato_bco > 0.
            tl_zfit0069-tp_ajuste = 'ATIVO'.
          else.
            tl_zfit0069-tp_ajuste = 'PASSIVO'.
          endif.

          if tl_zfit0069-dias_corridos_03  > 365 and tl_zfit0069-vlr_extrato_bco gt 0 .
            tl_zfit0069-tp_ajuste = 'ATIVOLP'.
          else.
            if  tl_zfit0069-dias_corridos_03 > 365 and tl_zfit0069-vlr_extrato_bco  le 0 .
              tl_zfit0069-tp_ajuste = 'PASSIVOLP'.
            endif.
          endif.

        elseif tl_zfit0069-tp_ajuste = 'ERRO FORMULA'.
          tl_zfit0069-tp_ajuste = 'ATIVO'.
          if tl_zfit0069-dias_corridos_03  > 365.
            tl_zfit0069-tp_ajuste = 'ATIVOLP'.
          endif.
        endif.
        "
        refresh tg_itens6-color.
        wl_color-fieldname = 'VLR_EXTRATO_BCO'.
        wl_color-color-col = 3.
        wl_color-color-inv = 3.
        append wl_color to tg_itens6-color.
        move-corresponding tl_zfit0069 to tg_itens6.
        tg_itens6-ano = tg_itens6-dt_fim_cto+0(4).
        append tg_itens6.
        clear: tg_itens6.
        move-corresponding tl_zfit0069 to wa_zfit0069.
        if tl_zfit0069-obj_key is not initial.
          vobj_key = tl_zfit0069-obj_key.
          robj_key = tl_zfit0069-obj_key_est.
        endif.
        append wa_zfit0069 to it_zfit0069.
        xgravado = 'X'.
      endloop.
    endif.
  endif.


  wg_cadpro-belnr  = icon_message_warning_small.
  wg_cadpro-belnr2 = icon_message_warning_small.

  clear: xestorno_botao, xestorno.
  refresh it_zib_contabil.

  "Estorno
  if robj_key is not initial.
    select  *
      from zib_contabil
      into table it_zib_contabil
      where obj_key = robj_key
      and  ( bktxt   = 'MTM-FINANCEIRO ESTORNO' or bktxt   = 'MTM-FINANCEIRO ESTORNO DI' ).
    if it_zib_contabil[] is not initial.
      xbloqueio = 'X'.
      select single *
        from zib_contabil_chv
        into wa_zib_contabil_chv
        where obj_key = robj_key.
      if sy-subrc = 0.
        wg_cadpro-belnr2 = wa_zib_contabil_chv-belnr.
      else.
        select single *
          from zib_contabil_err
          into wa_zib_contabil_err
          where obj_key = robj_key.
        if sy-subrc = 0.
          wg_cadpro-belnr2 = icon_incomplete.
        endif.
      endif.
    else.
      select  single *
       from zib_contabil
       into wa_zib_contabil
       where obj_key = robj_key
       and   bktxt   = 'ESTORNO MTM-FINANCEIRO'.
      if sy-subrc = 0.
        xestorno = 'X'.
        xestorno_botao = 'X'.
        select single *
         from zib_contabil_chv
         into wa_zib_contabil_chv
         where obj_key = robj_key.
        if sy-subrc = 0.
          wg_cadpro-belnr2 = wa_zib_contabil_chv-belnr. "Estorno
          clear xestorno.
        else.
          wg_cadpro-belnr2 = icon_alert."Estorno não concluido
        endif.
      endif.
      clear xbloqueio.
    endif.
  endif.
  " Normal
  if vobj_key is not initial.
    select  *
      from zib_contabil
      into table it_zib_contabil
      where obj_key = vobj_key
      and   bktxt   = 'MTM-FINANCEIRO'.
    if it_zib_contabil[] is not initial.
      xbloqueio = 'X'.
      select single *
        from zib_contabil_chv
        into wa_zib_contabil_chv
        where obj_key = vobj_key.
      if sy-subrc = 0.
        wg_cadpro-belnr = wa_zib_contabil_chv-belnr.
      else.
        select single *
          from zib_contabil_err
          into wa_zib_contabil_err
          where obj_key = vobj_key.
        if sy-subrc = 0.
          wg_cadpro-belnr = icon_incomplete.
          clear xbloqueio.
        endif.
      endif.
    else.
      select  single *
       from zib_contabil
       into wa_zib_contabil
       where obj_key = vobj_key
       and   bktxt   = 'ESTORNO MTM-FINANCEIRO'.
      if sy-subrc = 0.
        xestorno = 'X'.
        xestorno_botao = 'X'.
        select single *
         from zib_contabil_chv
         into wa_zib_contabil_chv
         where obj_key = vobj_key.
        if sy-subrc = 0.
          wg_cadpro-belnr = wa_zib_contabil_chv-belnr. "Estorno
          clear xestorno.
        else.
          wg_cadpro-belnr = icon_alert."Estorno não concluido
        endif.
      endif.
      clear xbloqueio.
    endif.
  else.
    clear xbloqueio.
  endif.

  refresh tg_itens4.
  clear xcalculo.

  select *
    from zfit0066
      into table tl_zfit0066
        where cod_oper eq 'N'.

  loop at it_zib_contabil into wa_zib_contabil.

    tg_itens4-tp_operacao = wa_zib_contabil-xref3.

    xcalculo = 'X'.
    move-corresponding wa_zib_contabil to tg_itens4.
    select single *
       from skat
       into wl_skat
       where spras eq sy-langu and ktopl eq '0050'
       and   saknr = tg_itens4-hkont.
    tg_itens4-txt50 =  wl_skat-txt50.

    select single * from tbsl
      into wl_tbsl
      where koart = 'S'
      and   bschl = tg_itens4-bschl.

    tg_itens4-wrbtr  = wa_zib_contabil-wrbtr.
    if wl_tbsl-shkzg = 'H'.
      move 'C' to tg_itens4-shkzg.
      tg_itens4-wrbtr  = wa_zib_contabil-wrbtr * -1.
    else.
      move 'D' to tg_itens4-shkzg.
    endif.

    tg_itens4-tp_ajuste = wa_zib_contabil-xblnr.
    tg_itens4-grav      = 'S'.

    select single *
        from zfit0063
        into wl_zfit0063
        where cod_oper = xcod_oper_nav.

    if sy-subrc = 0.
      tg_itens4-cod_oper = xcod_oper_nav.
      tg_itens4-descr    = wl_zfit0063-descr.
    endif.
    append tg_itens4.
  endloop.
  "CS2022001172 contabiliza individualmente (ZIB)
  if tg_itens4[] is initial.
    perform f_calculo_ctb.
  endif.
  "CS2022001172 contabiliza individualmente (ZIB)


endform.                    " F_BUSCA_CONTABIL
*&---------------------------------------------------------------------*
*&      Form  F_CALCULO_FT_ACUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZFIT0069_DT_INICIO_CTO  text
*      <--P_V_FT_ACUM_INDEV  text
*----------------------------------------------------------------------*
form f_calculo_ft_acum  using    p_data
                                 p_tx_index_passivo
                        changing p_v_ft_acum_indev.

  data   tabix      type sy-tabix.
  data : hol_id(2)  type c  value 'ZT',        " holiday calender id
         fact_id(2) type c  value 'ZT',        " factory calender id
         fromdate   type sydatum ,   " from date
         enddate    type sydatum,    " end date
         v_days     type i.


  data: begin of holiday occurs 0.
          include structure iscal_day.
data end of holiday.

  data: v_yrfrm(4)  type n,              "year valid from
        v_yrvlto(4) type n.             "year valid to



  refresh it_zfit0071.
  p_v_ft_acum_indev = 0.

  select *
   from zfit0071
   into table it_zfit0071
   where data ge p_data
   and   data le wg_cadpro-dt_fechamento
   and   tx_cdi_diaria gt 0
   order by data descending.

  if it_zfit0071 is not initial.                            "IR088181

    loop at it_zfit0071 into wa_zfit0071.
      if sy-tabix = 1.
        enddate    = wa_zfit0071-data.
      endif.
      fromdate   = wa_zfit0071-data.
    endloop.

    call function 'HOLIDAY_GET'
      exporting
        holiday_calendar           = hol_id
        factory_calendar           = fact_id
        date_from                  = fromdate
        date_to                    = enddate
      importing
        year_of_valid_from         = v_yrfrm
        year_of_valid_to           = v_yrvlto
*       RETURNCODE                 =
      tables
        holidays                   = holiday
      exceptions
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        others                     = 5.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    endif.



    "P_V_FT_ACUM_INDEV = 0.  "IR088181

    tabix = 0.
    loop at it_zfit0071 into wa_zfit0071.
      read table holiday with key date = wa_zfit0071-data.
      if sy-subrc = 0.
        continue.
      endif.
      add 1 to tabix.
      if tabix = 1.
*        p_v_ft_acum_indev = ( 1 + (  ( p_tx_index_passivo *  wa_zfit0071-tx_cdi_diaria ) / 10000  ) ) * 1.
        p_v_ft_acum_indev = 1.
      else.
        p_v_ft_acum_indev = ( 1 + (  ( p_tx_index_passivo *  wa_zfit0071-tx_cdi_diaria ) / 10000  ) ) * p_v_ft_acum_indev.
      endif.
    endloop.

  endif.                                                    "IR088181
  v_ft_acum_indev = p_v_ft_acum_indev.
endform.                    " F_CALCULO_FT_ACUM
*&---------------------------------------------------------------------*
*&      Form  BUSCA_TX_CAMBIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_tx_cambio .
  types :
    begin of ty_tcurr,
      kurst type tcurr-kurst,
      fcurr type tcurr-fcurr,
      tcurr type tcurr-tcurr,
      gdatu type tcurr-gdatu,
      ukurs type tcurr-ukurs,
    end of ty_tcurr,

    begin of ty_t001,
      bukrs type t001-bukrs,
      land1 type t001-land1,
    end of   ty_t001,

    begin of ty_t005,
      land1 type t005-land1,
      waers type t005-waers,
    end of   ty_t005.

  data: t_tcurr  type table of ty_tcurr,
        wa_tcurr type ty_tcurr,
        wa_t001  type ty_t001,
        wa_t005  type ty_t005.

  ranges: r_gdatu for tcurr-gdatu,
            r_fcurr for tcurr-fcurr.
  data: wl_date_aux  type datum,
        wl_input(10).

  move 'IEQ' to r_gdatu.

  wl_date_aux = wg_cadpro-dt_fechamento.
  add 1 to wl_date_aux.
  write wl_date_aux to wl_input.

  call function 'CONVERSION_EXIT_INVDT_INPUT'
    exporting
      input  = wl_input
    importing
      output = r_gdatu-low.
  append r_gdatu.

  move 'IEQ' to r_fcurr.
  r_fcurr-low = c_eur. append r_fcurr.
  r_fcurr-low = c_usd. append r_fcurr.

  select single bukrs land1
     from t001
     into wa_t001
     where bukrs = wg_cadpro-bukrs.

  select single land1 waers
    from t005
    into wa_t005
    where land1 = wa_t001-land1.

*BUSCA TAXA EM EUR
  select kurst fcurr tcurr gdatu ukurs
      from tcurr
      into table t_tcurr
      where kurst = 'B'
      and   fcurr in r_fcurr
      and   tcurr eq wa_t005-waers
      and   gdatu in r_gdatu.
  if sy-subrc = 0.
*Como o campo é de data invertida para pegar oa mais atual a data GDATU tem que ser ordenada do menor pro maior
    if sy-tcode ne 'ZFI0043'.
      sort t_tcurr by gdatu ascending fcurr ascending .
      read table t_tcurr with key fcurr = c_eur
                into wa_tcurr
                binary search.
      if sy-subrc = 0.
        xtx_eur = wa_tcurr-ukurs.
      else.
        message e398(00) with 'Erro ao encontrar taxa de conversão'
                              'do Euro'.
      endif.

    endif.

    read table t_tcurr with key fcurr = c_usd
              into wa_tcurr
              binary search.
    if sy-subrc = 0.
      xtx_usd = wa_tcurr-ukurs.
    else.
      message e398(00) with 'Erro ao encontrar taxa de conversão'
                            'do Dólar'.
    endif.

  else.
    message e398(00) with 'Erro ao encontrar taxa de conversão'
                      'do Dólar'.
  endif.

endform.                    " BUSCA_TX_CAMBIO
