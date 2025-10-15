*&---------------------------------------------------------------------*
*&  Include           ZPPR0028_PAI
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai_0100 input.
  get cursor field w_cursor_field.
  case sy-ucomm.
    when 'DEL'.
      perform elim_linha.
    when 'V_ENC_NOTA'.
      perform hab_enc_nota.
    when 'V_ENC_ORDEM'.
      perform hab_enc_ordem.
    when 'REFRESH'.
      leave to current transaction.
    when 'BACK'.
      clear obj_main->acao.
      perform confirm_sair.
    when 'CLEAR'.
      clear obj_main->acao.
      perform confirm_sair.
    when 'EXIT'.
      clear obj_main->acao.
      perform confirm_sair.
    when 'APONTAR' or 'SAVE'.
      obj_main->set_apontar( ).
    when 'ENCORD'.
      obj_main->set_encerra_ordem( ).
    when 'ENCONO'.
      obj_main->set_encerra_ordem_nota( ).
    when 'EDIT'.
      obj_main->acao = sy-ucomm.
      obj_main->set_edit_nota( ).
    when 'SAVE'.
      clear obj_main->acao.
      obj_main->set_save_nota( ).
    when 'EXE'.
      get cursor field w_cursor_field.
      perform check_bloq_ordem using v_ordem changing sy-subrc.
      if sy-subrc = 0.
        v_ordem = |{ v_ordem alpha = in }|.
        obj_main->set_estrutura( v_ordem ).
      endif.
    when 'ENTER'.
      perform check_bloq_ordem using v_ordem changing sy-subrc.
      if sy-subrc = 0.
        v_ordem = |{ v_ordem alpha = in }|.
        obj_main->set_estrutura( v_ordem ).
      endif.
    when 'DOUBLE'.
      perform exibir_ordem_nota.
  endcase.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  GET_ORDEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module get_ordem input.
  v_ordem = |{ v_ordem alpha = in }|.
  obj_main->set_estrutura( v_ordem ).
  if sy-ucomm <> 'VOLTAR'.
  endif.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0200 output.
  set pf-status 'TL001'.
  set titlebar 'TL002'.

  if it_aufk[] is not initial.
    if sy-ucomm eq 'V_NOVO'.
*      SET CURSOR FIELD 'ZAPONTA-ACTIVITY'.
      set cursor field 'ZAPONTA-PERNR'.
    elseif sy-ucomm eq 'V_CONFIRMA'.
*      SET CURSOR FIELD 'ZAPONTA-ACTIVITY'.
      set cursor field 'ZAPONTA-PERNR'.
    elseif sy-ucomm eq 'ENTER'.
      if zaponta-pernr is not initial.
        if it_empregado-sname is initial.
          set cursor field 'ZAPONTA-PERNR'.
        elseif zaponta-grund is not initial.
          if it_selec_parada-grdtx is initial.
            set cursor field 'ZAPONTA-GRUND'.
          else.
            case w_cursor_field.
              when 'ZAPONTA-AUFNR'.
*                SET CURSOR FIELD 'ZAPONTA-ACTIVITY'.
*              WHEN 'ZAPONTA-ACTIVITY'.
                set cursor field 'ZAPONTA-PERNR'.
              when 'ZAPONTA-PERNR'.
                set cursor field 'ZAPONTA-ISDD'.
              when 'ZAPONTA-ISDD'.
                if zaponta-isdd > sy-datum.
                  set cursor field 'ZAPONTA-ISDD'.
                else.
                  set cursor field 'ZAPONTA-ISDZ'.
                endif.
              when 'ZAPONTA-ISDZ'.
                if zaponta-isdd = sy-datum and zaponta-isdz > sy-uzeit.
                  set cursor field 'ZAPONTA-ISDZ'.
                else.
                  set cursor field 'ZAPONTA-IEDD'.
                endif.
              when 'ZAPONTA-IEDD'.
                if zaponta-iedd > sy-datum.
                  set cursor field 'ZAPONTA-IEDD'.
                else.
                  set cursor field 'ZAPONTA-IEDZ'.
                endif.
              when 'ZAPONTA-IEDZ'.
                if soma_total > zaponta-einzh or zaponta-isdd = sy-datum and zaponta-isdz > sy-uzeit or zaponta-iedd = sy-datum and zaponta-iedz > sy-uzeit.
                  set cursor field 'ZAPONTA-IEDZ'.
                else.
                  set cursor field 'ZAPONTA-GRUND'.
                  if soma_total is not initial.
                    zaponta-afrud = soma_total.
                  endif.
                endif.
              when 'ZAPONTA-GRUND'.
                set cursor field 'ZAPONTA-AFRUD'.
              when 'ZAPONTA-AFRUD'.
                set cursor field 'ZAPONTA-DURATION_NORMAL_UNIT'.
              when 'ZAPONTA-DURATION_NORMAL_UNIT'.
                set cursor field 'V_CONFIRMA'.
                perform validar_dados.
            endcase.
          endif.
        else.
          case w_cursor_field.
*            WHEN 'ZAPONTA-ACTIVITY'.
              set cursor field 'ZAPONTA-PERNR'.
            when 'ZAPONTA-PERNR'.
              set cursor field 'ZAPONTA-ISDD'.
            when 'ZAPONTA-ISDD'.
              if zaponta-isdd > sy-datum.
                set cursor field 'ZAPONTA-ISDD'.
              else.
                set cursor field 'ZAPONTA-ISDZ'.
              endif.
            when 'ZAPONTA-ISDZ'.
              if zaponta-isdd = sy-datum and zaponta-isdz > sy-uzeit.
                set cursor field 'ZAPONTA-ISDZ'.
              else.
                set cursor field 'ZAPONTA-IEDD'.
              endif.
            when 'ZAPONTA-IEDD'.
              if zaponta-iedd > sy-datum.
                set cursor field 'ZAPONTA-IEDD'.
              else.
                set cursor field 'ZAPONTA-IEDZ'.
              endif.
            when 'ZAPONTA-IEDZ'.
              if soma_total > zaponta-einzh or zaponta-isdd = sy-datum and zaponta-isdz > sy-uzeit or zaponta-iedd = sy-datum and zaponta-iedz > sy-uzeit.
                set cursor field 'ZAPONTA-IEDZ'.
              else.
                set cursor field 'ZAPONTA-GRUND'.
                if soma_total is not initial.
                  zaponta-afrud = soma_total.
                endif.
              endif.
            when 'ZAPONTA-GRUND'.
              set cursor field 'ZAPONTA-AFRUD'.
            when 'ZAPONTA-AFRUD'.
              set cursor field 'ZAPONTA-DURATION_NORMAL_UNIT'.
            when 'ZAPONTA-DURATION_NORMAL_UNIT'.
              set cursor field 'V_CONFIRMA'.
              perform validar_dados.
          endcase.
        endif.
      else.
        case w_cursor_field.
*          WHEN 'ZAPONTA-ACTIVITY'.
            set cursor field 'ZAPONTA-PERNR'.
          when 'ZAPONTA-PERNR'.
            set cursor field 'ZAPONTA-ISDD'.
          when 'ZAPONTA-ISDD'.
            if zaponta-isdd > sy-datum.
              set cursor field 'ZAPONTA-ISDD'.
            else.
              set cursor field 'ZAPONTA-ISDZ'.
            endif.
          when 'ZAPONTA-ISDZ'.
            if zaponta-isdd = sy-datum and zaponta-isdz > sy-uzeit.
              set cursor field 'ZAPONTA-ISDZ'.
            else.
              set cursor field 'ZAPONTA-IEDD'.
            endif.
          when 'ZAPONTA-IEDD'.
            if zaponta-iedd > sy-datum.
              set cursor field 'ZAPONTA-IEDD'.
            else.
              set cursor field 'ZAPONTA-IEDZ'.
            endif.
          when 'ZAPONTA-IEDZ'.
            if soma_total > zaponta-einzh or zaponta-isdd = sy-datum and zaponta-isdz > sy-uzeit or zaponta-iedd = sy-datum and zaponta-iedz > sy-uzeit.
              set cursor field 'ZAPONTA-IEDZ'.
            else.
              set cursor field 'ZAPONTA-GRUND'.
              if soma_total is not initial.
                zaponta-afrud = soma_total.
              endif.
            endif.
          when 'ZAPONTA-GRUND'.
            set cursor field 'ZAPONTA-AFRUD'.
          when 'ZAPONTA-AFRUD'.
            set cursor field 'ZAPONTA-DURATION_NORMAL_UNIT'.
          when 'ZAPONTA-DURATION_NORMAL_UNIT'.
            set cursor field 'V_CONFIRMA'.
            perform validar_dados.
        endcase.
      endif.
    elseif sy-ucomm eq 'BACK'.
      leave to screen 0.
    elseif sy-ucomm eq 'CLEAR'.
      leave to screen 0.
    "BUG #175277 - MMSILVA - 29.04.2025 - Inicio
    elseif sy-ucomm eq 'ZINPUT_CAL_HR'.
      set cursor field 'ZAPONTA-IEDZ'.
      if soma_total is not initial.
        zaponta-afrud = soma_total.
      endif.
    "BUG #175277 - MMSILVA - 29.04.2025 - Fim
    else.
      case w_cursor_field.
*        WHEN 'ZAPONTA-ACTIVITY'.
          set cursor field 'ZAPONTA-PERNR'.
        when 'ZAPONTA-PERNR'.
          set cursor field 'ZAPONTA-ISDD'.
        when 'ZAPONTA-ISDD'.
          if zaponta-isdd > sy-datum.
            set cursor field 'ZAPONTA-ISDD'.
          else.
            set cursor field 'ZAPONTA-ISDZ'.
          endif.
        when 'ZAPONTA-ISDZ'.
          if zaponta-isdd = sy-datum and zaponta-isdz > sy-uzeit.
            set cursor field 'ZAPONTA-ISDZ'.
          else.
            set cursor field 'ZAPONTA-IEDD'.
          endif.
        when 'ZAPONTA-IEDD'.
          if zaponta-iedd > sy-datum.
            set cursor field 'ZAPONTA-IEDD'.
          else.
            set cursor field 'ZAPONTA-IEDZ'.
          endif.
        when 'ZAPONTA-IEDZ'.
          if soma_total > zaponta-einzh or zaponta-isdd = sy-datum and zaponta-isdz > sy-uzeit or zaponta-iedd = sy-datum and zaponta-iedz > sy-uzeit.
            set cursor field 'ZAPONTA-IEDZ'.
          else.
            set cursor field 'ZAPONTA-GRUND'.
            "BUG #175277 - MMSILVA - 29.04.2025 - Inicio - Comentado devido ajuste
*            if soma_total is not initial.
*              zaponta-afrud = soma_total.
*            endif.
            "BUG #175277 - MMSILVA - 29.04.2025 - Fim - Comentado devido ajuste
          endif.
        when 'ZAPONTA-GRUND'.
          set cursor field 'ZAPONTA-AFRUD'.
        when 'ZAPONTA-AFRUD'.
          set cursor field 'ZAPONTA-DURATION_NORMAL_UNIT'.
        when 'ZAPONTA-DURATION_NORMAL_UNIT'.
          set cursor field 'V_CONFIRMA'.
          perform validar_dados.
      endcase.
    endif.
    if zaponta-work_cntr is not initial.
      if zaponta-ktext is initial.
        set cursor field 'ZAPONTA-WORK_CNTR'.
      endif.
    endif.

    if zaponta-afrud is not initial.
      if zaponta-afrud > soma_total.
        set cursor field 'ZAPONTA-AFRUD'.
      endif.
    endif.

    if zaponta-isdd is not initial and zaponta-iedd is not initial.
      if zaponta-isdd > zaponta-iedd.
        set cursor field 'ZAPONTA-ISDD'.
      endif.
    endif.

    if zaponta-isdd = zaponta-iedd and zaponta-iedz < zaponta-isdz.
      set cursor field 'ZAPONTA-IEDZ'.
    endif.

  else.
    set cursor field 'ZAPONTA-PERNR'.
  endif.

*   perform set_calc_hr.
endmodule.
