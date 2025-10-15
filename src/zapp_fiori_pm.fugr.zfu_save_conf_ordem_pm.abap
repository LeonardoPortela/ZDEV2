function zfu_save_conf_ordem_pm.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_MSG TYPE  BAPIRET2_TAB OPTIONAL
*"      T_DADOS TYPE  ZPMT0084_T OPTIONAL
*"----------------------------------------------------------------------

  data: it_timetickets type table of bapi_alm_timeconfirmation,
        wa_return      type bapiret2,
        dt_inicio      type sydatum,
        dt_fim         type sydatum,
        hr_inicio      type syuzeit,
        hr_fim         type syuzeit,
        tg_return      type table of bapi_alm_return.

  data: soma_data  type p decimals 2,
        soma_hora  type p decimals 2,
        soma_total type p decimals 2.

  data:ld_no_day     type i,
       ld_no_month   type i,
       ld_no_year    type i,
       ld_no_cal_day type i.

  data: input_text type catsxt_longtext_itab,
        wa_text    type txline.

  data: wa_header type thead,
        vl_texto  type zchar30000.

  data: ls_line type line of tywf_tline,
        ls_docu type line of rsfbtltab,
        lt_docu type rsfbtltab.



  free: it_timetickets, tg_return.
  clear: dt_inicio, dt_fim, hr_inicio, hr_fim.

  check t_dados[] is not initial.

  loop at t_dados assigning field-symbol(<ws_dados>).

    "Verifica status.
    case <ws_dados>-status.
      when 'I'. "Iniciado
        select single * from zpmt0084
        into @data(ws_zpmt0084)
        where pernr eq @<ws_dados>-pernr
          and aufnr eq @<ws_dados>-aufnr
          and vornr eq @<ws_dados>-vornr
          and Isdd  eq @<ws_dados>-isdd
          and Isdz eq @<ws_dados>-Isdz.
        if sy-subrc eq 0.
          ws_zpmt0084-status = 'I'.
          modify zpmt0084 from ws_zpmt0084.
        else.
          modify zpmt0084 from <ws_dados>.

        endif.
        commit work.
        append value #( type = 'I'
                                id = ''
                            number = ''
                           message = 'Início registrado'
                            log_no = ''
                        log_msg_no = ''
                        message_v1 = '' ) to et_msg.


      when 'P'. "Pausado

        select single * from zpmt0084
        into ws_zpmt0084
        where pernr eq <ws_dados>-pernr
          and aufnr eq <ws_dados>-aufnr
          and vornr eq <ws_dados>-vornr
          and Isdd  eq <ws_dados>-isdd
          and Isdz eq <ws_dados>-Isdz.
        if sy-subrc eq 0.
          ws_zpmt0084-status = 'P'.
          modify zpmt0084 from ws_zpmt0084.
        else.
          modify zpmt0084 from <ws_dados>.

        endif.
        commit work.
        append value #( type = 'I'
                                id = ''
                            number = ''
                           message = 'Pausa registrado'
                            log_no = ''
                        log_msg_no = ''
                        message_v1 = '' ) to et_msg.

      when 'F'. "Finalizado

        clear: soma_data,
               soma_hora,
               soma_hora,
               soma_total,
               ld_no_day,
               ld_no_month,
               ld_no_year,
               ld_no_cal_day.

        dt_inicio = <ws_dados>-Isdd.
        hr_inicio = <ws_dados>-Isdz.
        dt_fim = <ws_dados>-Iedd.
        hr_fim = <ws_dados>-Iedz.




        if dt_inicio > sy-datum.

          append value #( type = 'E'
                            id = ''
                        number = ''
                       message = 'Data inicio não pode ser maior que data atual'
                        log_no = ''
                    log_msg_no = ''
                    message_v1 = '' ) to et_msg.
          continue.
        endif.

        if dt_fim > sy-datum.

          append value #( type = 'E'
                            id = ''
                        number = ''
                       message = 'Data fim não pode ser maior que data atual'
                        log_no = ''
                    log_msg_no = ''
                    message_v1 = '' ) to et_msg.
          continue.
        endif.




        if dt_fim < dt_inicio.

          append value #( type = 'E'
                            id = ''
                        number = ''
                       message = 'Data fim não pode ser menor que data inicio'
                        log_no = ''
                    log_msg_no = ''
                    message_v1 = '' ) to et_msg.
          continue.
        endif.

        if dt_inicio eq dt_fim and hr_inicio eq hr_fim.

          append value #( type = 'E'
                            id = ''
                        number = ''
                       message = 'Informar uma data e hora fim diferente'
                        log_no = ''
                    log_msg_no = ''
                    message_v1 = '' ) to et_msg.
          continue.
        endif.

        if dt_inicio ne dt_fim.

          append value #( type = 'E'
                            id = ''
                        number = ''
                       message = 'Data inicio não pode ser diferente da data fim'
                        log_no = ''
                    log_msg_no = ''
                    message_v1 = '' ) to et_msg.
          continue.
        endif.

        select *
      from afru
      into table @data(t_afru)
      where pernr = @<ws_dados>-pernr
        and stokz <> @abap_true
        and stzhl = ' '
        and isdd <= @dt_fim   " início existente é antes ou igual ao fim informado
        and iedd >= @dt_inicio " fim existente é depois ou igual ao início informado
        and isdz <= @hr_fim
        and iedz >= @hr_inicio.
        if sy-subrc eq 0.

          append value #( type = 'E'
                            id = ''
                        number = ''
                       message = |Existem lançamento no periodo para o colaborador: { <ws_dados>-pernr }|
*                   message_v1 = |para o colaborador': { <ws_dados>-pernr }|
                        log_no = ''
                    log_msg_no = ''
                    message_v2 = '' ) to et_msg.
          continue.
        endif.


        "Calculando total de dias.
        call function 'HR_AUPBS_MONTH_DAY'
          exporting
            beg_da     = dt_inicio
            end_da     = dt_fim
          importing
            no_day     = ld_no_day
            no_month   = ld_no_month
            no_year    = ld_no_year
            no_cal_day = ld_no_cal_day.

        "Calculando total de horas.
        soma_data     = ( ld_no_cal_day - 1 ) * 24.
        soma_hora     = ( hr_fim - hr_inicio  ) / 60.
        soma_hora     = ( soma_hora / 60 ).
        soma_total    = ( soma_data + soma_hora ).


        if soma_total > 14.
          append value #( type = 'E'
                            id = 'ZPMMSG'
                        number = '009'
                       message = |Total horas trab. incompativél com a jornada|
                      message_v1 = | de trabalho ordem: { <ws_dados>-aufnr }|
                        log_no = ''
                    log_msg_no = ''
                    message_v2 = '' ) to et_msg.
          continue.
        endif.

        "Seta o texto maior que 72 caractere.
        vl_texto = <ws_dados>-ltxa1.
        append value #(  orderid         = <ws_dados>-aufnr
                         postg_date      = sy-datum
                         operation       = <ws_dados>-vornr
*                    sub_oper        = ls-sub_activity
*                    work_cntr       = ls-work_cntr
                         pers_no         = <ws_dados>-pernr
                         exec_start_date = dt_inicio
                         exec_start_time = hr_inicio
                         exec_fin_date   = dt_fim
                         exec_fin_time   = hr_fim
                         fin_conf        = <ws_dados>-fin_conf
                         act_work        = soma_total
                         un_work         = 'H'
                         dev_reason      = <ws_dados>-grund
                         conf_text       = <ws_dados>-ltxa1+0(40)
*                    CLEAR_RES       = LS-CLEAR_RES
*                    act_type        = ls-acttype
                         ) to it_timetickets.


        data(zvar_pernr) = <ws_dados>-pernr.
      when others.
    endcase.
  endloop.



  if it_timetickets is not initial.
    call function 'BAPI_ALM_CONF_CREATE'
      importing
        return        = wa_return
      tables
        timetickets   = it_timetickets
        detail_return = tg_return.

    read table tg_return into data(ls_retorn) with key type = 'E'.
    if sy-subrc = 0.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
      move-corresponding ls_retorn to wa_return.
      append wa_return to et_msg.
*    append wa_return to it_return.
    else.

      read table tg_return into ls_retorn index 1.

      append value #( type = 'S'
                          id = 'ZPMMSG'
                      number = '000'
                     message = 'Confirmação realizada com sucesso!'
                      log_no = ''
                  log_msg_no = ''
                  message_v1 = '' ) to et_msg.

      data(qtd_caractere) = strlen( vl_texto ).
      if qtd_caractere > 40.
*** Quebra texto em linha de 62 caracteres
        call function 'RKD_WORD_WRAP'
          exporting
            textline            = vl_texto
            delimiter           = ' '
            outputlen           = 72
          tables
            out_lines           = input_text
          exceptions
            outputlen_too_large = 1
            others              = 2.

        clear wa_header.

        "//TÃ­tulo do Objeto
        wa_header-tdobject = 'AUFK'.
        wa_header-tdname = |{ sy-mandt }{ ls_retorn-conf_no }{ ls_retorn-conf_cnt }|.
        "//ID de Texto
        wa_header-tdid = 'RMEL'.
        "//Idioma
        wa_header-tdspras = sy-langu.

        loop at input_text into wa_text.
          ls_docu-tdformat = '*'.
          ls_docu-tdline = wa_text.
          append ls_docu to lt_docu.
        endloop.

        if lt_docu is not initial.
          "//Salva o texto
          call function 'SAVE_TEXT'
            exporting
              client   = sy-mandt
              header   = wa_header
            tables
              lines    = lt_docu
            exceptions
              id       = 1
              language = 2
              name     = 3
              object   = 4
              others   = 5.

          call function 'COMMIT_TEXT'
            exporting
              object          = wa_header-tdobject
              name            = wa_header-tdname
              id              = wa_header-tdid
              language        = wa_header-tdspras
              savemode_direct = abap_true.

        endif.
      endif.

      select * from zpmt0084
        into table @data(it_zpmt0084)
        for all entries in @t_dados
        where pernr eq @t_dados-pernr
          and aufnr eq @t_dados-aufnr
          and vornr eq @t_dados-vornr
          and Isdd  eq @t_dados-isdd
          and Isdz  eq @t_dados-Isdz.
      if sy-subrc eq 0.
        loop at it_zpmt0084 assigning field-symbol(<ls_zpmt0084>).
          <ls_zpmt0084>-status = 'F'.
        endloop.
        modify zpmt0084 from table it_zpmt0084.
      endif.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
    endif.
  endif.
endfunction.
