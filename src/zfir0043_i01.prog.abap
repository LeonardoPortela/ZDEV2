*----------------------------------------------------------------------*
***INCLUDE ZFIR0043_I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  data w_answer.

  data: cursorfield(30) type c,
        cursorline(30)  type c,
        cursorvalue(30) type c.

  case ok-code.
    when 'PICK'.
      get cursor field cursorfield line cursorline value cursorvalue.
      if  cursorvalue = icon_incomplete and ( cursorfield = 'WG_CADPRO-BELNR' or cursorfield = 'WG_CADPRO-BELNR2' ).
        if grid1 is not initial.
          call method grid1->free.
          if splitter is not initial.
            call method splitter->free.
          endif.
          if g_custom_container is not initial.
            call method g_custom_container->free.
          endif.

          if obg_descbox is not initial.
            call method obg_descbox->free.
          endif.

          free: container_1,container_2, grid1,obg_toolbar,obg_descbox.
          free: g_custom_container,splitter.

          if cursorfield = 'WG_CADPRO-BELNR'.
            select  obj_key nr_item interface dt_atualizacao hr_atualizacao type id num message message_v1 message_v2 message_v3  message_v4
              from zib_contabil_err
              into table it_zib_contabil_err
              where obj_key eq vobj_key.
          else.
            select  obj_key nr_item interface dt_atualizacao hr_atualizacao type id num message message_v1 message_v2 message_v3  message_v4
             from zib_contabil_err
             into table it_zib_contabil_err
             where obj_key eq robj_key.

          endif.

          perform f_cria_objetos.
        endif.
      elseif not wa_zib_contabil_chv is initial
            and  not cursorvalue is initial
            and ( cursorfield = 'WG_CADPRO-BELNR' or cursorfield = 'WG_CADPRO-BELNR2' ).
        set parameter id 'BLN' field cursorvalue.
        set parameter id 'BUK' field wa_zib_contabil_chv-bukrs.
        set parameter id 'GJR' field wa_zib_contabil_chv-gjahr.
        call transaction 'FB03' and skip first screen.
      endif.


    when 'PCLCA'.
      if xcod_oper_nav is initial.
        xcod_oper_nav = wg_cadpro-cod_oper.
      endif.

      if xcod_oper_nav = 'V'. "SWAP Fluxo caixa T.V.
        xcod_oper_nav = 'S'.
      elseif xcod_oper_nav = 'S'. "SWAP Fluxo caixa T.F.
        xcod_oper_nav = 'H'.  "SWAP Vanila
      elseif xcod_oper_nav = 'H'.
        xcod_oper_nav = 'N'.  "NDF
      else.
        exit.
      endif.

      if grid1 is not initial.
        call method grid1->free.
      endif.

      if splitter is not initial.
        call method splitter->free.
      endif.
      if g_custom_container is not initial.
        call method g_custom_container->free.
      endif.

      if obg_descbox is not initial.
        call method obg_descbox->free.
      endif.

      free: container_1,container_2, grid1,obg_toolbar,obg_descbox.
      free: g_custom_container,splitter.
      perform f_busca_contabil.
      perform f_cria_objetos.

    when 'PCLCP'.
      if xcod_oper_nav is initial.
        xcod_oper_nav = wg_cadpro-cod_oper.
      endif.

      if xcod_oper_nav = 'N'. "NDF
        xcod_oper_nav = 'H'. "SWAP Vanila
      elseif xcod_oper_nav = 'H'.
        xcod_oper_nav = 'S'. "SWAP Fluxo Caixa T.F.
      elseif xcod_oper_nav = 'S'.
        xcod_oper_nav = 'V'. "SWAP Fluxo Caixa T.V.
      else.
        exit.
      endif.

      if grid1 is not initial.
        call method grid1->free.
      endif.

      if splitter is not initial.
        call method splitter->free.
      endif.
      if g_custom_container is not initial.
        call method g_custom_container->free.
      endif.

      if obg_descbox is not initial.
        call method obg_descbox->free.
      endif.

      free: container_1,container_2, grid1,obg_toolbar,obg_descbox.
      free: g_custom_container,splitter.

      perform f_busca_contabil.
      perform f_cria_objetos.
    when 'ANT'.
      vsubtela =  g_tab_tela-subtela.
      if vsubtela gt 210.
        wg_colaps =  '@K2@'.
        if g_tab_tela-subtela = '0210'.
          call method obg_descbox->get_text_as_r3table
            importing
              table = tg_editor.
        elseif g_tab_tela-subtela = '0211'.
          call method obg_descbox->get_text_as_r3table
            importing
              table = tg_editor1.
        elseif g_tab_tela-subtela = '0212'.
          call method obg_descbox->get_text_as_r3table
            importing
              table = tg_editor2.
        endif.
        subtract 1 from vsubtela.
        g_tab_tela-subtela = vsubtela.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = g_tab_tela-subtela
          importing
            output = g_tab_tela-subtela.

        if g_tab_tela-subtela = '0210'.
          perform f_pega_imagem using 'LOGO_MAGGI_M' changing url.
          call method picture->load_picture_from_url
            exporting
              url = url.
          call method picture->set_display_mode
            exporting
              display_mode = picture->display_mode_fit_center.
        elseif g_tab_tela-subtela = '0211'.
          perform f_pega_imagem using 'LOGO_MAGGI_M' changing url.
          call method picture1->load_picture_from_url
            exporting
              url = url.
          call method picture1->set_display_mode
            exporting
              display_mode = picture1->display_mode_fit_center.
        elseif g_tab_tela-subtela = '0212'.
          perform f_pega_imagem using 'LOGO_MAGGI_M' changing url.
          call method picture2->load_picture_from_url
            exporting
              url = url.
          call method picture2->set_display_mode
            exporting
              display_mode = picture2->display_mode_fit_center.
        elseif g_tab_tela-subtela = '0213'.
          perform f_pega_imagem using 'LOGO_MAGGI_M' changing url.
          call method picture3->load_picture_from_url
            exporting
              url = url.
          call method picture3->set_display_mode
            exporting
              display_mode = picture3->display_mode_fit_center.
        endif.

        if grid1 is not initial.
          call method grid1->free.
        endif.

        if splitter is not initial.
          call method splitter->free.
        endif.
        if g_custom_container is not initial.
          call method g_custom_container->free.
        endif.

        if obg_descbox is not initial.
          call method obg_descbox->free.
        endif.

        free: container_1,container_2, grid1,obg_toolbar,obg_descbox.
        free: g_custom_container,splitter.
      endif.
    when 'PROX'.

      vsubtela =  g_tab_tela-subtela.
      if vsubtela lt 214.
        wg_colaps =  '@K2@'.
        if g_tab_tela-subtela = '0210'.
          data(werro) = ' '.
          loop at  tg_itens.
            if tg_itens-tx_cupom_camb eq 0.
              werro = 'X'.
            endif.
          endloop.
          if werro = 'X'.
            message  'Preencha o campo Cupom Cambial se NOVO CALCULO'  type 'I'.
*            EXIT.
          endif.
          if tg_itens[] is initial.
            message 'Finalizar esta etapa' type 'I'.
            exit.
          endif.
          call method obg_descbox->get_text_as_r3table
            importing
              table = tg_editor.

*          PERFORM F_PEGA_IMAGEM USING 'LOGO_MAGGI_M' CHANGING URL.
          perform f_pega_imagem using 'LOGO_MAGGI_MARCAD' changing url.
          call method picture->load_picture_from_url
            exporting
              url = url.
          call method picture->set_display_mode
            exporting
              display_mode = picture->display_mode_fit_center.
        elseif g_tab_tela-subtela = '0211'.
          if tg_itens2[] is initial.
            message 'Finalizar esta etapa' type 'I'.
            exit.
          endif.
          call method obg_descbox->get_text_as_r3table
            importing
              table = tg_editor1.

*          PERFORM F_PEGA_IMAGEM USING 'LOGO_MAGGI_M' CHANGING URL.
          perform f_pega_imagem using 'LOGO_MAGGI_MARCAD' changing url.
          call method picture1->load_picture_from_url
            exporting
              url = url.
          call method picture1->set_display_mode
            exporting
              display_mode = picture1->display_mode_fit_center.

        elseif g_tab_tela-subtela = '0212'.

*          IF 'N_T' CS WG_CADPRO-COD_OPER.
*            IF NOT TG_ITENS3[] IS INITIAL.
*
*              READ TABLE TG_ITENS3 TRANSPORTING NO FIELDS WITH KEY TP_OPERACAO = ''.
*              IF SY-SUBRC IS INITIAL.
*                MESSAGE 'Existem Contratos sem Classificação TP_OPERAÇÃO! ' TYPE 'I'.
*                EXIT.
*              ENDIF.
*            ELSE.
*            MESSAGE 'Finalizar esta etapa' TYPE 'I'.
*            EXIT.
*            ENDIF.
*          ENDIF.

          call method obg_descbox->get_text_as_r3table
            importing
              table = tg_editor2.

*          PERFORM F_PEGA_IMAGEM USING 'LOGO_MAGGI_M' CHANGING URL.
          perform f_pega_imagem using 'LOGO_MAGGI_MARCAD' changing url.
          call method picture2->load_picture_from_url
            exporting
              url = url.
          call method picture2->set_display_mode
            exporting
              display_mode = picture2->display_mode_fit_center.
        elseif g_tab_tela-subtela = '0213'.
*          PERFORM F_PEGA_IMAGEM USING 'LOGO_MAGGI_M' CHANGING URL.
          perform f_pega_imagem using 'LOGO_MAGGI_MARCAD' changing url.
          call method picture3->load_picture_from_url
            exporting
              url = url.
          call method picture3->set_display_mode
            exporting
              display_mode = picture3->display_mode_fit_center.
        endif.
        add 1 to vsubtela.
        g_tab_tela-subtela = vsubtela.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = g_tab_tela-subtela
          importing
            output = g_tab_tela-subtela.

        if grid1 is not initial.
          call method grid1->free.
        endif.

        if splitter is not initial.
          call method splitter->free.
        endif.
        if g_custom_container is not initial.
          call method g_custom_container->free.
        endif.

        if obg_descbox is not initial.
          call method obg_descbox->free.
        endif.

        free: container_1,container_2, grid1,obg_toolbar,obg_descbox.
        free: g_custom_container,splitter.

      endif.

    when 'ZIB'.
      if btn_rei = 'Reinicializar'.
        wg_cadpro-belnr  = icon_message_warning_small.
        wg_cadpro-belnr2 = icon_message_warning_small.
        clear xbloqueio.
        perform f_calculo_ctb.
        xcalculo = 'X'.
        call method grid1->refresh_table_display
          exporting
            is_stable = wa_stable.
      else.
        if wg_cadpro-belnr is not initial.
          call function 'POPUP_TO_CONFIRM'
            exporting
*             TITLEBAR              = ' '
*             DIAGNOSE_OBJECT       = ' '
              text_question         = 'Confirma o Estorno?'
              text_button_1         = 'Sim'
              icon_button_1         = 'ICON_OKAY '
              text_button_2         = 'Não'
              icon_button_2         = 'ICON_CANCEL'
              default_button        = '1'
              display_cancel_button = ' '
*             USERDEFINED_F1_HELP   = ' '
              start_column          = 25
              start_row             = 6
*             POPUP_TYPE            =
*             IV_QUICKINFO_BUTTON_1 = ' '
*             IV_QUICKINFO_BUTTON_2 = ' '
            importing
              answer                = w_answer
*               TABLES
*             PARAMETER             =
            exceptions
              text_not_found        = 1
              others                = 2.

          if sy-subrc <> 0.
            message id sy-msgid type sy-msgty number sy-msgno
                    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          endif.
          if w_answer = '1'.
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

            subtract 1 from data_ini.
            vdata01 = data_ini + 1.
            perform f_estorno_ctb using data_ini vobj_key.
            perform f_estorno_ctb using vdata01  robj_key.
            clear:  xbloqueio, xcalculo.
            call method grid1->refresh_table_display
              exporting
                is_stable = wa_stable.
          endif.
        else.
          message 'Documento contabil não gerado ainda!' type 'I'.
        endif.
      endif.
    when 'GERAR'.
      perform verifica_erros.
      if tg_msg_ret[] is initial.
        if tg_itens4[] is not initial.
          perform f_grava_zib.
        else.
          message 'Não foi gerado o cálculo' type 'I'.
        endif.
      else.
        call function 'Z_DOC_CHECK_NEW'
          exporting
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          importing
            e_messagem    = wg_mensagem
          tables
            it_msgs       = tg_msg_ret.
      endif.

    when 'ATUALIZAR'.
      clear: wg_cadpro-belnr, vobj_key, robj_key.
      data: tl_zfit0064 type table of zfit0064 with header line,
            tl_zfit0067 type table of zfit0067 with header line,
            tl_zfit0069 type table of zfit0069 with header line,
            tl_zfit0070 type table of zfit0070 with header line.
      " NDF
      if xcod_oper_nav = 'N'.
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
      endif.
      if xcod_oper_nav = 'S' .
        " SWAP Fluxo de caixa
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
      endif.

      if xcod_oper_nav = 'H' .
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
      endif.

      if xcod_oper_nav = 'V' .
        " SWAP Fluxo de caixa T.V.
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
      endif.

      select single *
        from zib_contabil_chv
        into wa_zib_contabil_chv
        where obj_key = robj_key.
      if sy-subrc = 0.
        select  single *
             from zib_contabil
             into wa_zib_contabil
             where obj_key = robj_key
             and   bktxt   = 'ESTORNO MTM-FINANCEIRO'.
        if sy-subrc = 0 and tg_itens4_aux[] is not initial.
          xestorno = 'X'.
        else.
          wg_cadpro-belnr2 = wa_zib_contabil_chv-belnr.
          clear xestorno.
        endif.

      else.
        select  obj_key nr_item interface dt_atualizacao hr_atualizacao type id num message message_v1 message_v2 message_v3  message_v4
         from zib_contabil_err
         into table it_zib_contabil_err
         where obj_key eq robj_key.

        if it_zib_contabil_err[] is not initial.
          wg_cadpro-belnr2 = icon_incomplete.
        endif.
      endif.

      select single *
       from zib_contabil_chv
       into wa_zib_contabil_chv
       where obj_key = vobj_key.
      if sy-subrc = 0.
        select  single *
             from zib_contabil
             into wa_zib_contabil
             where obj_key = vobj_key
             and   bktxt   = 'ESTORNO MTM-FINANCEIRO'.
        if sy-subrc = 0 and tg_itens4_aux[] is not initial.
          xestorno = 'X'.
        else.
          wg_cadpro-belnr = wa_zib_contabil_chv-belnr.
          clear xestorno.
        endif.

      else.
        select  obj_key nr_item interface dt_atualizacao hr_atualizacao type id num message message_v1 message_v2 message_v3  message_v4
         from zib_contabil_err
         into table it_zib_contabil_err
         where obj_key eq vobj_key.

        if it_zib_contabil_err[] is not initial.
          wg_cadpro-belnr = icon_incomplete.
        endif.
      endif.
    when 'CALCULAR'.
      if  g_tab_tela-subtela = '0211'.
        if wg_cadpro-bukrs is initial.
          message 'Informe a Empresa' type 'I'.
          exit.
        endif.
      endif.
      if wg_cadpro-dt_fechamento is initial.
        message 'Informe a data de fechamento' type 'I'.
        exit.
      endif.

      if 'H_V_T' cs wg_cadpro-cod_oper.
        if wg_cadpro-ptax is initial or wg_cadpro-ptax eq 0.
          message 'Informe o PTAX' type 'I'.
          exit.
        endif.
      endif.

      perform f_calculo.

      if wg_cadpro-cod_oper = 'T'. " Todos
        perform f_calculo_ndf.
        if 1 = 1.
          perform f_calculo_swap_s_atu.
          perform f_calculo_swap_v_atu.
        else.
          perform f_calculo_swap_s.
          perform f_calculo_swap_v_novo.
        endif.


        if 1 = 1.
          perform f_calculo_swap_h_atu.
        else.
          perform f_calculo_swap_h_novo.
        endif.
      elseif xcod_oper_nav = 'N'. " NDF
        perform f_calculo_ndf.
      elseif xcod_oper_nav = 'S'. " SWAP Fluxo de caixa T.F.
        if 1 = 1.
          perform f_calculo_swap_s_atu.
        else.
          perform f_calculo_swap_s.
        endif.
      elseif xcod_oper_nav = 'V'. " SWAP Fluxo de caixa T.V.
        if 1 = 1.
          perform f_calculo_swap_v_atu.
        else.
          perform f_calculo_swap_v_novo.
        endif.
      elseif xcod_oper_nav = 'H'. " SWAP Vanila
        if 1 = 1.
          perform f_calculo_swap_h_atu.
        else.
          perform f_calculo_swap_h_novo.
        endif.
      else.
        message 'Modalidade de cálculo não prevista' type 'I'.
        exit.
      endif.
      perform f_calculo_ctb.
    when 'IMPORTAR'.
      if wg_cadpro-dt_fechamento is initial.
        message 'Informe a data de Interpolação' type 'I'.
        exit.
      endif.
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

      subtract 1 from data_ini.

*      IF WG_CADPRO-DT_FECHAMENTO NE DATA_INI.
*        MESSAGE 'Data deve ser o último dia do mês' TYPE 'I'.
*        EXIT.
*      ENDIF.

      call screen 0110 starting at 040 2
              ending   at 180 5.

    when c_col_exp.
      if wg_colaps eq '@K1@'.
        wg_colaps = '@K2@'.
        call method splitter->set_row_height
          exporting
            id     = 2
            height = 100.
      else.
        wg_colaps = '@K1@'.
        call method splitter->set_row_height
          exporting
            id     = 2
            height = 70.
      endif.
    when c_deldoc.
      call function 'POPUP_TO_CONFIRM'
        exporting
*         TITLEBAR              = ' '
*         DIAGNOSE_OBJECT       = ' '
          text_question         = 'Confirma a exclusão desta Data?'
          text_button_1         = 'Sim'(001)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'Não'(002)
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
*         USERDEFINED_F1_HELP   = ' '
          start_column          = 25
          start_row             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        importing
          answer                = w_answer
*       TABLES
*         PARAMETER             =
        exceptions
          text_not_found        = 1
          others                = 2.

      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
      if w_answer = '1'.
        perform eliminar_data.
      endif.
    when c_search.
      if g_tab_tela-subtela   = '0210'.
        perform busca_dados.
      endif.
    when c_save.
      perform verifica_erros.
      if tg_msg_ret[] is initial.
        clear wg_acao.
        perform grava_dados.
        refresh: tg_fields.
        perform trata_campos using space
                                  'GR1'
                                   c_1       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0
      else.
        message s000(zwrm001) display like 'E' with 'Há erro no documento.'.
        call function 'Z_DOC_CHECK_NEW'
          exporting
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          importing
            e_messagem    = wg_mensagem
          tables
            it_msgs       = tg_msg_ret.
      endif.

    when c_back.
      clear wg_acao.
    when c_displa.
      g_tab_tela-subtela = '0210'.
      wg_acao = c_displa.
      perform limpa_campos.
      refresh: tg_fields.
      perform trata_campos using space
                                'GR1'
                                 c_1       "INPUT 1     NO INPUT 0
                                 c_0.      "INVISIBLE 1 VISIBLE 0
    when c_add.
      if wg_cadpro-descr_oper is initial or wg_cadpro-cod_oper  is initial ..
        message s000(zwrm001) display like 'E' with 'Informe a Operação'.
        exit.
      endif.
      if wg_cadpro-butxt is initial or wg_cadpro-bukrs  is initial .
        message s000(zwrm001) display like 'E' with 'Informe a empresa'.
        exit.
      endif.

      if wg_cadpro-dt_fechamento is initial.
        message s000(zwrm001) display like 'E' with 'Informe a data de fechamento'.
        exit.
      endif.
      g_tab_tela-subtela = '0210'.
      if wg_acao ne c_displa.
        clear xmodif.
        wg_acao = c_modif.
        perform limpa_campos.
        refresh: tg_fields.
        perform trata_campos using space
                                  'GR1'
                                   c_0       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0

      elseif wg_acao = c_displa.
        wg_acao = c_add.
        perform limpa_campos.
        refresh: tg_fields.
        perform trata_campos using space
                                  'GR1'
                                   c_1       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0
      endif.
    when c_cancel.
      clear wg_acao.
    when c_atuali.

    when c_modif.
      if wg_acao = c_modif.
        clear wg_acao.
        refresh: tg_fields.
        perform trata_campos using space
                                  'GR1'
                                   c_1       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0

      else.
        wg_acao = c_modif.
        perform trata_campos using space
                                  'GR1'
                                   c_0       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0

      endif.
    when c_show_msgre.
      "CLEAR wg_acao.
      perform verifica_erros.
      if tg_msg_ret[] is not initial.
        call function 'Z_DOC_CHECK_NEW'
          exporting
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          importing
            e_messagem    = wg_mensagem
          tables
            it_msgs       = tg_msg_ret.
      endif.

    when c_exit.

      leave program.
  endcase.

endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_exit input.
  case ok-code.
    when c_back.
      set screen 0.

    when c_exit.
      leave program.
  endcase.
endmodule.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0110 input.
  case ok-code.
    when 'IMPORTAR'.
      if wg_cadpro-path is not initial.
        data: t_excel         like alsmex_tabline occurs 0 with header line,
              t_excel2        like alsmex_tabline occurs 0 with header line,
              v_dias_corridos type zfit0060-dias_corridos,
              v_dt_base_bmf   type zfit0060-dt_base_bmf,
              p_file          type rlgrap-filename,
              wrbtr(16)       type c,
              vmsg(50).

        move wg_cadpro-path to p_file.
        clear t_excel.
        refresh: t_excel, tg_itens.

        call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
          exporting
            filename                = p_file
            i_begin_col             = 1
            i_begin_row             = 2
            i_end_col               = 6
            i_end_row               = 10000
          tables
            intern                  = t_excel
          exceptions
            inconsistent_parameters = 1
            upload_ole              = 2
            others                  = 3.

        call function 'SAPGUI_PROGRESS_INDICATOR'
          exporting
            text = 'Atualizando Dados'.

        if t_excel[] is not initial.
          t_excel2[] = t_excel[].
          sort t_excel2 by row col.

          loop at t_excel.
            if t_excel-row = t_excel2-row.
              continue.
            endif.
            loop at t_excel2 where row = t_excel-row.
              wrbtr = t_excel2-value.

              replace ',' with ';' into wrbtr.
              replace '.' with ' ' into wrbtr.
              replace ';' with '.' into wrbtr.

              condense wrbtr no-gaps.

              case t_excel2-col.
                when 1.
                  wa_taxas-seq              = t_excel2-value.
                when 2.
                  wa_taxas-dias_corridos    = t_excel2-value.
                when 3.
                  wa_taxas-tx_proj_ano_ban  = wrbtr.
                when 4.
                  wa_taxas-tx_proj_ano_com  =  wrbtr.
                when 5.
                  wa_taxas-tx_par_dolar     =  wrbtr.
                when 6.
                  wa_taxas-tx_cupom_camb    =  wrbtr.
              endcase.
            endloop.

            concatenate 'Linha ' t_excel-row into vmsg separated by space.
            call function 'SAPGUI_PROGRESS_INDICATOR'
              exporting
                text = vmsg.

            move-corresponding wa_taxas to tg_itens.

            v_dt_base_bmf   = wg_cadpro-dt_fechamento.
            v_dias_corridos = wa_taxas-dias_corridos.
            add v_dias_corridos to v_dt_base_bmf.

            perform calc_dias_uteis using wg_cadpro-dt_fechamento v_dt_base_bmf changing v_dias_uteis.

            move  v_dt_base_bmf to tg_itens-dt_base_bmf.
            move  v_dias_uteis  to tg_itens-dias_uteis.
            append tg_itens.
          endloop.
          call method grid1->refresh_table_display
            exporting
              is_stable = wa_stable.
          set screen 0.
        endif.
      endif.
    when 'SAIR'.
      set screen 0.
  endcase.

endmodule.                 " USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_PATH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_path input.
  data: l_dynpfields like dynpread occurs 0 with header line.
  refresh l_dynpfields.
  clear   l_dynpfields.

  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = ' '
      def_path         = 'C:\'
      mask             = '*.XLS'
      mode             = 'O' "'S'
      title            = 'Busca de Arquivo'
    importing
      filename         = wg_cadpro-path
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.


endmodule.                 " SEARCH_PATH  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.
  case ok-code.
    when 'ANT'.
      vsubtela =  g_tab_tela-subtela.
      if vsubtela gt 210.
        subtract 1 from vsubtela.
        g_tab_tela-subtela = vsubtela.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = g_tab_tela-subtela
          importing
            output = g_tab_tela-subtela.

        "OBG_TOOLBAR

        if grid1 is not initial.
          call method grid1->free.
        endif.
        if splitter is not initial.
          call method splitter->free.
        endif.
        if g_custom_container is not initial.
          call method g_custom_container->free.
        endif.

        free: container_1,container_2, grid1.
        free: g_custom_container,splitter.
      endif.

    when 'PROX'.
      vsubtela =  g_tab_tela-subtela.
      if vsubtela lt 212.
        add 1 to vsubtela.
        g_tab_tela-subtela = vsubtela.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = g_tab_tela-subtela
          importing
            output = g_tab_tela-subtela.

        if grid1 is not initial.
          call method grid1->free.
        endif.
        if splitter is not initial.
          call method splitter->free.
        endif.
        if g_custom_container is not initial.
          call method g_custom_container->free.
        endif.

        free: container_1,container_2, grid1.
        free: g_custom_container,splitter.
      endif.
    when others.
  endcase.
endmodule.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_oper input.
  data: tl_return_tab type table of ddshretval with header line,
        tl_dselc      type table of dselc      with header line.

  refresh: tl_return_tab,tl_dselc.

  data: begin of tl_oper occurs 0,
          cod_oper type zfit0063-cod_oper,
          descr    type zfit0063-descr,
        end of tl_oper.

  select cod_oper descr
    from zfit0063
    into table tl_oper.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'COD_OPER'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIT0063-COD_OPER'
      value_org       = 'S'
    tables
      value_tab       = tl_oper
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

  if tl_return_tab is not initial.
    read table tl_return_tab index 1.
    read table tl_oper with key cod_oper = tl_return_tab-fieldval.
    if sy-subrc = 0.
      wg_cadpro-descr_oper = tl_oper-descr.
    endif.
  endif.
endmodule.                 " SEARCH_OPER  INPUT
