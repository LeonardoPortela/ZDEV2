*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 22/09/2025                                              &*
*& Descrição: Serviço de Frete de Terceiros                           &*
*& Transação: ZLES0223 (Prest. Serv. Frete Terceiros)                 &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2T37 |22/09/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*
*&---------------------------------------------------------------------*
*& Form zf_param_grp_mercadoria
*&---------------------------------------------------------------------*
*& Chama a parametrização do Grupo de Mercadoria
*&---------------------------------------------------------------------*
FORM zf_param_grp_mercadoria.

  gv_db_tab = gc_db_tab.
  gv_stcnam = gc_stcnam.
  gv_scmant = gc_scmant.
  gv_title  = TEXT-005. "Parâmetro - Grupo de Mercadoria
  gv_act_01 = TEXT-004. "Log Modificação
* Executa o programa de Cadastro de Dados.
  SUBMIT zregister_data WITH p_db_tab = gv_db_tab
                        WITH p_stcnam = gv_stcnam
                        WITH p_scmant = gv_scmant
                        WITH p_title  = gv_title
                        WITH p_act_01 = gv_act_01
                    AND RETURN.

  CLEAR: gv_db_tab, gv_stcnam, gv_scmant, gv_title, gv_act_01.

ENDFORM.
*&---------------------------------------------------------------------*
**& Form  zf_setting_verify_field
*&---------------------------------------------------------------------*
*       Configuração e validação do respectivo campo acionado
*----------------------------------------------------------------------*
*    --> P_VALUE Valor do respectivo campo para validar o preenchimento
*    --> P_NAMEF Nome do respectivo campo para validar o preenchimento
*    --> P_DYNNR Valor da tela a qual o campo pertence
*----------------------------------------------------------------------*
FORM zf_setting_verify_field USING uv_value
                                   uv_namef
                                   uv_dynnr TYPE sydynnr.
  DATA: lv_check TYPE c.

  CONSTANTS: lc_pesq TYPE char4 VALUE 'PESQ'.

  IF uv_dynnr EQ sy-dynnr.

    CASE uv_dynnr.
      WHEN '0101'. "Tela de Pesquisa
        CHECK sy-ucomm EQ lc_pesq.
* Valida  o dos campos ordin rios.
        IF uv_namef EQ 'P_BUKRS'.
          lv_check = abap_on.

        ENDIF.

      WHEN OTHERS.
*     Do Nothing.
    ENDCASE.

    IF     uv_value IS INITIAL AND
       NOT lv_check IS INITIAL.
* Preencher todos os campos obrigat rios
      MESSAGE e055(00).

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_show_data_frete
*&---------------------------------------------------------------------*
*& Exibe os dados selecionado da pesquisa
*&---------------------------------------------------------------------*
FORM zf_show_data_frete .

  DATA: tl_function TYPE ui_functions.

  DATA: el_layout  TYPE lvc_s_layo,
        el_variant TYPE disvariant.

  IF gcl_custom_container IS INITIAL.
    CREATE OBJECT gcl_custom_container
      EXPORTING
        container_name = gc_container.

    CREATE OBJECT gcl_grid1
      EXPORTING
        i_parent = gcl_custom_container.
* Criar o Catalogo de de campos do ALV de Frete.
    PERFORM zf_filcatalog_alv_frete.
* Exclui botões da Barra de tarefa do ALV Frete.
    PERFORM zf_exclud_btn_tbar_alv TABLES tl_function.
* Carrega a estrutura de Layout.
    el_layout-zebra      = abap_on.
    el_layout-cwidth_opt = abap_on.
    el_variant-report    = sy-repid.
* Carrega as configurações e dados para o ALV Frete
    CALL METHOD gcl_grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = el_layout
        i_save               = 'A'
        is_variant           = el_variant
      CHANGING
        it_fieldcatalog      = tg_fcat
        it_outtab            = tg_frete.

  ELSE.
    CALL METHOD gcl_grid1->refresh_table_display.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_filcatalog_alv_frete
*&---------------------------------------------------------------------*
*& Criar o Catalogo de de campos do ALV de Frete
*&---------------------------------------------------------------------*
FORM zf_filcatalog_alv_frete.

  DATA: vl_texto TYPE scrtext_l.

* Função de de catalogo de campos conforme estrutura definida
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZLESE_ALV_FRETE_TERC'
    CHANGING
      ct_fieldcat      = tg_fcat.

  LOOP AT tg_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN 'ID_CLIENTE'.
        vl_texto = 'Cliente cód'.

      WHEN 'NAME1'.
        vl_texto = 'Cliente descrição'.

      WHEN 'CLIENTE_IE'.
        vl_texto = 'Cliente Inscr.'.

      WHEN 'GR_MERCADORIA'.
        vl_texto = 'Gr mercadoria'.

      WHEN 'WGBEZ60'.
        vl_texto = 'Gr Descrição'.

      WHEN 'QUANTIDADE'.
        vl_texto = 'Qtdade'.

      WHEN 'TARIFA'.
        vl_texto = 'Tarifa'.

      WHEN 'BRANCH'.
        vl_texto = 'Filial Transp'.

      WHEN 'ID_LOTE_FRETE'.
        vl_texto = 'ID lote'.

      WHEN OTHERS.
        CONTINUE.

    ENDCASE.

    <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = <fs_fcat>-reptext = vl_texto.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_exclud_btn_tbar_alv
*&---------------------------------------------------------------------*
*& Exclui botões da Barra de tarefa do ALV Frete
*&---------------------------------------------------------------------*
*&      --> TL_FUNCTION TI de botões a sere excluídos
*&---------------------------------------------------------------------*
FORM zf_exclud_btn_tbar_alv  TABLES pt_function TYPE ui_functions.

  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO pt_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO pt_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO pt_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO pt_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO pt_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO pt_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO pt_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO pt_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO pt_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO pt_function.
  APPEND cl_gui_alv_grid=>mc_fc_check             TO pt_function.
  APPEND cl_gui_alv_grid=>mc_fc_refresh           TO pt_function.

ENDFORM.
*&----------------------------------------------------------------------------------*
*& Form  zf_limit_select_option
*&----------------------------------------------------------------------------------*
*  Restringe as opções da tela de seleção
*-----------------------------------------------------------------------------------*
FORM zf_limit_select_option.

  TYPE-POOLS sscr. "Tipo da tela de selação
* Restringe os dados do parâmetro da tela de seleção
  DATA: tl_screen TYPE sscr_restrict. "Tabelda tela de seleção
* Estruturas para preencher a tab. t_screen
  DATA: el_opts  TYPE sscr_opt_list, "Estrutura da restrição da lista de opções
        el_assoc TYPE sscr_ass.      "Estrutura da lista do nome da variável restringida
  CONSTANTS: cl_objectkey1(10) TYPE c VALUE 'OBJECTKEY1'.
  CONSTANTS: cl_objectkey2(10) TYPE c VALUE 'OBJECTKEY2'.
  CONSTANTS: cl_objectkey3(10) TYPE c VALUE 'OBJECTKEY3'.
  CONSTANTS: cl_objectkey4(10) TYPE c VALUE 'OBJECTKEY4'.

* Restringe o campo "Modificado em"  selection para somente EQ.
* Filial Transportadora
  el_opts-name       = cl_objectkey1.
  el_opts-options-eq = sy-abcde+23(1). "X
  APPEND el_opts TO tl_screen-opt_list_tab.
  el_assoc-kind      = sy-abcde+18(1). "S
  el_assoc-name      = 'S_BRANCH'.
  el_assoc-sg_main   = '*'.
  el_assoc-sg_addy   = space.
  el_assoc-op_main   = cl_objectkey1.
  APPEND el_assoc TO tl_screen-ass_tab.
* Ano Negociação
  el_opts-name       = cl_objectkey2.
  APPEND el_opts TO tl_screen-opt_list_tab.
  el_assoc-name      = 'S_ANO'.
  el_assoc-op_main   = cl_objectkey2.
  APPEND el_assoc TO tl_screen-ass_tab.
* Cliente
  el_opts-name       = cl_objectkey3.
  APPEND el_opts TO tl_screen-opt_list_tab.
  el_assoc-name      = 'S_CLIENT'.
  el_assoc-op_main   = cl_objectkey3.
  APPEND el_assoc TO tl_screen-ass_tab.
* ID Negociação
  el_opts-name       = cl_objectkey4.
  APPEND el_opts TO tl_screen-opt_list_tab.
  el_assoc-name      = 'S_ID_NEG'.
  el_assoc-op_main   = cl_objectkey4.
  APPEND el_assoc TO tl_screen-ass_tab.
* Função para restringir Selection  Option
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = tl_screen
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.
* Verifica de função executou com erro.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_select_data_frete
*&---------------------------------------------------------------------*
*& Busca os dados da pesquisa do frete de terceiro
*&---------------------------------------------------------------------*
FORM zf_select_data_frete.

  IF s_ano[]    IS INITIAL AND
     s_client[] IS INITIAL.
    MESSAGE |Campos Ano Negociação e Clinete vazios. Informar um deles.| TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.

  ENDIF.

  SELECT a~bukrs, id_neg, status, id_cliente, name1, cliente_cnpj, cliente_cpf, cliente_ie,
         gr_mercadoria, wgbez60, quantidade, tarifa, a~branch, id_lote_frete, lfimg AS qtd_fatur
    FROM zlest0255 AS a
    INNER JOIN kna1 AS b
     ON id_cliente EQ b~kunnr
    INNER JOIN zlest0256 AS c
     ON gr_mercadoria EQ c~matkl AND
        a~matnr       EQ c~matnr
    LEFT JOIN zlest0181 AS d
     ON a~vbeln EQ d~vbeln AND
        d~vbeln NE @space
    LEFT JOIN lips AS e
     ON d~vbeln EQ e~vbeln AND
        d~posnr EQ e~posnr
    INTO TABLE @tg_frete
  WHERE a~bukrs      EQ @p_bukrs
    AND a~branch     IN @s_branch
    AND a~ano        IN @s_ano
    AND a~id_neg     IN @s_id_neg
    AND a~id_cliente IN @s_client.

  IF NOT sy-subrc IS INITIAL.
* Não encontrados dados para esta seleção
    MESSAGE s114(pt) DISPLAY LIKE sy-abcde+4(1). "E
    RETURN.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_tela_edit_observacao
*&---------------------------------------------------------------------*
*& Cria o editor da Observação do Frete
*&---------------------------------------------------------------------*
FORM zf_tela_edit_observacao.

  IF gcl_cc_desc IS INITIAL.
    CREATE OBJECT gcl_cc_desc
      EXPORTING
        container_name = gc_descbox.

    IF gcl_cc_desc IS NOT INITIAL.
      CREATE OBJECT gcl_observacao
        EXPORTING
          parent                     = gcl_cc_desc
          wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position          = 163
          max_number_chars           = 350
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
* Inibe a Barra de Tarefa do Editor.
      CALL METHOD gcl_observacao->set_toolbar_mode
        EXPORTING
          toolbar_mode = cl_gui_textedit=>false.

      IF sy-ucomm EQ 'NEW' OR
         sy-ucomm EQ 'EDIT'.
* Exibe o Editor para edição.
        CALL METHOD gcl_observacao->set_readonly_mode
          EXPORTING
            readonly_mode = cl_gui_textedit=>false.

      ELSE.
* Exibe o Editor somente leitura.
        CALL METHOD gcl_observacao->set_readonly_mode
          EXPORTING
            readonly_mode = cl_gui_textedit=>true.

      ENDIF.

    ENDIF.

  ELSE.
    IF   sy-ucomm EQ 'NEW'    OR   "Novo Frete
         sy-ucomm EQ 'EDIT'   OR   "Editar Frete
       ( sy-ucomm EQ 'ENTE'   AND  "Enter na tela
         gv_acao  EQ 'EDIT' ) OR   "Editar Frete
       ( sy-ucomm EQ 'ENTE'   AND  "Enter na tela
         gv_acao  EQ 'NEW' )  OR   "Frete Novo
       ( sy-ucomm EQ 'SAVE'   AND  "Salvar Frete
         gv_acao  EQ 'EDIT' ).     "Editar Frete
* Exibe o Editor para edição.
      CALL METHOD gcl_observacao->set_readonly_mode
        EXPORTING
          readonly_mode = cl_gui_textedit=>false.
* Verifica se é Novo Frete.
      IF sy-ucomm EQ 'NEW'. "Novo Frete
* Carrega o texto Observação do Frete lido da função READ_TEXT.
        CALL METHOD gcl_observacao->set_text_as_r3table
          EXPORTING
            table = tg_editor.

      ENDIF.

    ELSE.
* Exibe o Editor somente leitura.
      CALL METHOD gcl_observacao->set_readonly_mode
        EXPORTING
          readonly_mode = cl_gui_textedit=>true.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_chek_fields_scr_0200
*&---------------------------------------------------------------------*
*& Verifica campos da tela de Manutenção do Frete
*&---------------------------------------------------------------------*
FORM zf_chek_fields_scr_0200.

  DATA: vl_tdlnr TYPE tdlnr.

* Verifica se a Estrutura da tela 200 está preenchida para validação.
  IF NOT *zlest0255 IS INITIAL.
* Verifica se não foi acionado a Exibir Frete.
    IF     sy-ucomm EQ 'SHOW'   OR
           sy-ucomm EQ 'EDIT'   OR
       ( ( gv_acao  EQ 'SHOW'   OR
           gv_acao  EQ 'EDIT' ) AND
           sy-ucomm EQ 'ENTE' ).
*** Dados Empresa Prestadora de Serviço
      SELECT SINGLE butxt FROM t001 INTO gv_butxt WHERE bukrs EQ *zlest0255-bukrs.
*** Dados do Cliente
      SELECT SINGLE * FROM kna1 INTO *kna1 WHERE kunnr EQ *zlest0255-id_cliente.

      SELECT SINGLE ktext FROM t151t INTO gv_ktext WHERE spras EQ 'P'
                                                     AND kdgrp EQ *zlest0255-kdgrp.
*** Dados da Condição
      SELECT SINGLE wgbez60 maktx FROM zlest0256 INTO ( gv_wgbez60, gv_maktx ) WHERE matkl EQ *zlest0255-gr_mercadoria
                                                                                 AND matnr EQ *zlest0255-matnr.

      SELECT SINGLE text1 FROM t052u INTO gv_text1 WHERE zterm EQ *zlest0255-zterm AND spras EQ sy-langu.
*** Parceiros
* Ponto Coleta.
      SELECT SINGLE lzone FROM lfa1 INTO gv_zone_pc WHERE lifnr EQ *zlest0255-pc_codigo.
* Local Entrega.
      SELECT SINGLE lzone FROM kna1 INTO gv_zone_lr WHERE kunnr EQ *zlest0255-lr_codigo.
* Verifica se a ordem de venda foi cirada.
      IF NOT *zlest0255-vbeln IS INITIAL.
* verifica se a ordem de venda existe.
        SELECT  SINGLE * FROM vbak INTO @DATA(el_vbak) WHERE vbeln EQ @*zlest0255-vbeln.

        IF NOT sy-subrc IS INITIAL.
          CLEAR *zlest0255-vbeln.
* Atualiza a tabela de Prestação de Serviço de Frete.
          UPDATE zlest0255
             SET vbeln = @*zlest0255-vbeln
          WHERE bukrs  EQ @*zlest0255-bukrs
            AND branch EQ @*zlest0255-branch
            AND ano    EQ @*zlest0255-ano
            AND id_neg EQ @*zlest0255-id_neg.

          IF sy-subrc IS INITIAL.
            COMMIT WORK.

          ENDIF.

        ENDIF.

      ENDIF.

      EXIT.

    ENDIF.
* Verifica se ocorreu erro na validação dos dados da tela 200.
    IF NOT gv_erro IS INITIAL.
      CLEAR gv_erro.
      EXIT.

    ENDIF.
*** Dados Empresa Prestadora de Serviço
    SELECT SINGLE butxt FROM t001 INTO gv_butxt WHERE bukrs EQ *zlest0255-bukrs.

    IF NOT sy-subrc IS INITIAL.
* Empresa &1 não existe
      MESSAGE s045(em) WITH *zlest0255-bukrs DISPLAY LIKE 'E'.
      gv_erro = abap_on.
      RETURN.

    ENDIF.
* Verifica o ano informado.
    CALL FUNCTION 'CONVERSION_EXIT_GJAHR_INPUT'
      EXPORTING
        input       = *zlest0255-ano
      IMPORTING
        output      = *zlest0255-ano
      EXCEPTIONS
        wrong_input = 1
        OTHERS      = 2.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      gv_erro = abap_on.
      RETURN.

    ENDIF.

    vl_tdlnr = |{ *zlest0255-branch ALPHA = IN WIDTH = 10 }|.

    SELECT SINGLE * FROM zlest0207 INTO @DATA(el_t0207) WHERE tdlnr EQ @vl_tdlnr.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE |Filial { *zlest0255-branch } não é emissora de Ct-e.| TYPE 'S' DISPLAY LIKE 'E'.
      gv_erro = abap_on.
      RETURN.

    ENDIF.
*** Dados do Cliente
    SELECT SINGLE * FROM kna1 INTO *kna1 WHERE kunnr EQ *zlest0255-id_cliente.

    IF sy-subrc IS INITIAL.
      *zlest0255-cliente_cnpj = *kna1-stcd1.
      *zlest0255-cliente_cpf  = *kna1-stcd2.
      *zlest0255-cliente_ie   = *kna1-stcd3.

      SELECT SINGLE * FROM j_1bbranch INTO @DATA(el_bbranch) WHERE branch EQ @*zlest0255-branch
                                                               AND bukrs  EQ @*zlest0255-bukrs.

      IF sy-subrc IS INITIAL.
        *zlest0255-emit_cnpj = el_bbranch-stcd1.
        *zlest0255-emit_ie   = el_bbranch-state_insc.

        SELECT SINGLE * FROM adrc INTO @DATA(el_adrc) WHERE addrnumber EQ @el_bbranch-adrnr.

        IF sy-subrc IS INITIAL.
          *zlest0255-emit_rsocial  = el_adrc-name1.
          *zlest0255-emit_endereco = el_adrc-street.
          *zlest0255-emit_local    = el_adrc-city1.
          *zlest0255-emit_uf       = el_adrc-region.

        ENDIF.

      ENDIF.

      IF sy-ucomm EQ 'SAVE' OR "Salvar Frete
         sy-ucomm EQ 'ORVD'.   "Gerar Ordem de Venda
        SELECT SINGLE * FROM knb1 INTO @DATA(el_knb1) WHERE kunnr EQ @*kna1-kunnr.
        IF sy-subrc IS INITIAL.
* Valida se o Cliente está bloqueado.
          IF *kna1-sperr    EQ abap_on OR
              el_knb1-sperr EQ abap_on.
            MESSAGE |Cliente { *kna1-kunnr } bloqueado. Solicite revisão a central de cadastro.| TYPE 'S' DISPLAY LIKE 'E'.
            gv_erro = abap_on.
            RETURN.

          ENDIF.
* Validar se cliente está expandido para a organização de venda, canal e setor.
          IF *kna1-sperr    IS INITIAL AND
              el_knb1-sperr IS INITIAL.
            SELECT SINGLE * FROM knvv
              INTO @DATA(el_knvv)
            WHERE kunnr EQ @*kna1-kunnr
              AND vkorg EQ @*zlest0255-bukrs
              AND vtweg EQ '10'     "Mercado Interno
              AND spart EQ '08'.    "Serviços

            IF sy-subrc IS INITIAL.
              IF el_knvv-kdgrp IS INITIAL.
                MESSAGE |Solicitar a classificação do cliente { *kna1-kunnr }, Org. Venda: { el_knvv-vkorg }, Canal: 10, Setor : 08 | TYPE 'S' DISPLAY LIKE 'E'.
                gv_erro = abap_on.
                RETURN.

              ELSE.
                *zlest0255-kdgrp = el_knvv-kdgrp.

                SELECT SINGLE * FROM t151t
                  INTO @DATA(el_t151t)
                WHERE spras EQ 'P'
                  AND kdgrp EQ @el_knvv-kdgrp.

                IF sy-subrc IS INITIAL.
                  gv_ktext = el_t151t-ktext.

                ENDIF.

              ENDIF.

            ELSE.
              MESSAGE |Cliente { *kna1-kunnr } não expandido para Org. Venda: { el_knvv-vkorg }, Canal: 10, Setor : 08 | TYPE 'S' DISPLAY LIKE 'E'.
              gv_erro = abap_on.
              RETURN.

            ENDIF.

          ENDIF.

        ENDIF.

      ELSE.
        SELECT SINGLE * FROM knvv
          INTO el_knvv
        WHERE kunnr EQ *kna1-kunnr
          AND vkorg EQ p_bukrs
          AND vtweg EQ '10'     "Mercado Interno
          AND spart EQ '08'.    "Serviços

        IF sy-subrc IS INITIAL.
          *zlest0255-kdgrp = el_knvv-kdgrp.

          SELECT SINGLE * FROM t151t
            INTO el_t151t
          WHERE spras EQ 'P'
            AND kdgrp EQ el_knvv-kdgrp.

          IF sy-subrc IS INITIAL.
            gv_ktext = el_t151t-ktext.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSE.
* Cliente &1 não existe
      MESSAGE s074(e7) WITH *zlest0255-id_cliente DISPLAY LIKE 'E'.
      gv_erro = abap_on.
      RETURN.

    ENDIF.
*** Dados da Condição
    IF NOT *zlest0255-gr_mercadoria IS INITIAL AND
       NOT *zlest0255-matnr         IS INITIAL.

      SELECT SINGLE wgbez60 matnr maktx
        FROM zlest0256
        INTO ( gv_wgbez60, *zlest0255-matnr, gv_maktx )
      WHERE matkl EQ *zlest0255-gr_mercadoria
        AND matnr EQ *zlest0255-matnr.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE |Não existe o Gr Mercadoria para o Material cadastrado na tabela de parâmetros. Verifcar!| TYPE 'S' DISPLAY LIKE 'E'.
        gv_erro = abap_on.
        RETURN.

      ENDIF.

    ELSE.
      SELECT matkl, wgbez60, matnr, maktx
        FROM zlest0256
        INTO TABLE @DATA(tl_t0256)
      WHERE matkl EQ @*zlest0255-gr_mercadoria.

      IF sy-subrc IS INITIAL.
        DATA(vl_qtlin) = lines( tl_t0256 ).

        IF vl_qtlin EQ 1.
          READ TABLE tl_t0256 INTO DATA(el_t0256) INDEX 1.
          gv_wgbez60       = el_t0256-wgbez60.
          *zlest0255-matnr = el_t0256-matnr.
          gv_maktx         = el_t0256-maktx.

        ELSE.
          MESSAGE |Há mais de um Material cadastrado para o mesmo Gr Mercadoria. Selecione um pela Ajuda de pesquisa e em seguida dê enter.| TYPE 'I' DISPLAY LIKE 'W'.
          RETURN.

        ENDIF.

      ELSE.
        MESSAGE |Não existe o Gr Mercadoria cadastrado na tabela de parâmetros. Verifcar!| TYPE 'S' DISPLAY LIKE 'E'.
        gv_erro = abap_on.
        RETURN.

      ENDIF.

    ENDIF.

    *zlest0255-waerk = 'BRL'.

    SELECT SINGLE text1 FROM t052u INTO gv_text1 WHERE zterm EQ *zlest0255-zterm AND spras EQ sy-langu.

*** Dados dos Parceiros.
* Remetente Mercadoria.
    SELECT SINGLE name1 stcd1 stcd2 stcd3 stras ort01 regio
      FROM lfa1
      INTO ( *zlest0255-reme_rsocial, *zlest0255-reme_cnpj, *zlest0255-reme_cpf, *zlest0255-reme_ie,
             *zlest0255-reme_endereco, *zlest0255-reme_local, *zlest0255-reme_uf )
    WHERE lifnr EQ *zlest0255-reme_codigo.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE |Remetente Mercadoria { *zlest0255-reme_codigo } não existe.| TYPE 'S' DISPLAY LIKE 'E'.
      gv_erro = abap_on.
      RETURN.

    ENDIF.
* Destinatário.
    SELECT SINGLE name1 stcd1 stcd2 stcd3 stras ort01 regio
      FROM kna1
      INTO ( *zlest0255-dest_rsocial, *zlest0255-dest_cnpj, *zlest0255-dest_cpf, *zlest0255-dest_ie,
             *zlest0255-dest_endereco, *zlest0255-dest_local, *zlest0255-dest_uf )
    WHERE kunnr EQ *zlest0255-dest_codigo.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE |Destinatário { *zlest0255-dest_codigo } não existe.| TYPE 'S' DISPLAY LIKE 'E'.
      gv_erro = abap_on.
      RETURN.

    ENDIF.
* Ponto Coleta.
    SELECT SINGLE name1 stcd1 stcd2 stcd3 stras ort01 regio lzone
      FROM lfa1
      INTO ( *zlest0255-pc_rsocial, *zlest0255-pc_cnpj, *zlest0255-pc_cpf, *zlest0255-pc_ie,
             *zlest0255-pc_endereco, *zlest0255-pc_local, *zlest0255-pc_uf, gv_zone_pc )
    WHERE lifnr EQ *zlest0255-pc_codigo.

    IF sy-subrc IS INITIAL.
      IF sy-ucomm EQ 'SAVE' OR "Salvar Frete
         sy-ucomm EQ 'ORVD'.   "Gerar Ordem de Venda
* Verifica a Zona de Transporte do Ponto de Coleta.
        IF gv_zone_pc IS INITIAL.
          MESSAGE |Zona de transporte não definida para o Ponto Coleta. Verificar!| TYPE 'S' DISPLAY LIKE 'E'.
          gv_erro = abap_on.
          RETURN.

        ENDIF.

      ENDIF.

    ELSE.
      MESSAGE |Ponto Coleta { *zlest0255-pc_codigo } não existe.| TYPE 'S' DISPLAY LIKE 'E'.
      gv_erro = abap_on.
      RETURN.

    ENDIF.
* Local Entrega.
    SELECT SINGLE name1 stcd1 stcd2 stcd3 stras ort01 regio lzone
      FROM kna1
      INTO ( *zlest0255-lr_rsocial, *zlest0255-lr_cnpj, *zlest0255-lr_cpf, *zlest0255-lr_ie,
             *zlest0255-lr_endereco, *zlest0255-lr_local, *zlest0255-lr_uf, gv_zone_lr )
    WHERE kunnr EQ *zlest0255-lr_codigo.

    IF sy-subrc IS INITIAL.
      IF sy-ucomm EQ 'SAVE' OR "Salvar Frete
         sy-ucomm EQ 'ORVD'.   "Gerar Ordem de Venda
* Verifica a Zona de Transporte do Local Entrega.
        IF gv_zone_lr IS INITIAL.
          MESSAGE |Zona de transporte não definida para o Local Entrega. Verificar!| TYPE 'S' DISPLAY LIKE 'E'.
          gv_erro = abap_on.
          RETURN.

        ENDIF.

      ENDIF.

    ELSE.
      MESSAGE |Local entrega { *zlest0255-dest_codigo } não existe.| TYPE 'S' DISPLAY LIKE 'E'.
      gv_erro = abap_on.
      RETURN.

    ENDIF.

    IF sy-ucomm EQ 'SAVE' OR "Salvar Frete
       sy-ucomm EQ 'ORVD'.   "Gerar Ordem de Venda
* Validação do Intinerário.
      SELECT SINGLE * FROM trolz
        INTO @DATA(el_trolz)
      WHERE aland EQ 'BR'
        AND azone EQ @gv_zone_pc
        AND lland EQ 'BR'
        AND lzone EQ @gv_zone_lr.

      IF el_trolz-route IS INITIAL.
        MESSAGE |Não existe Itinerário para TROLZ-AZONE x TROLZ-LZONE. Solicite a criação!| TYPE 'S' DISPLAY LIKE 'E'.
        gv_erro = abap_on.
        RETURN.

      ENDIF.

    ENDIF.
* Tomador Serviço.
    SELECT SINGLE name1 stcd1 stcd2 stcd3 stras ort01 regio
      FROM kna1
      INTO ( *zlest0255-toma_rsocial, *zlest0255-toma_cnpj, *zlest0255-toma_cpf, *zlest0255-toma_ie,
             *zlest0255-toma_endereco, *zlest0255-toma_local, *zlest0255-toma_uf )
    WHERE kunnr EQ *zlest0255-toma_codigo.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE |Tomador Serviço { *zlest0255-toma_codigo } não existe.| TYPE 'S' DISPLAY LIKE 'E'.
      gv_erro = abap_on.
      RETURN.

    ENDIF.
*** Preparação para salvar informação do Frete Novo/Editado.
    IF (   sy-ucomm EQ 'SAVE'     AND "Salvar Frete
         ( gv_acao  EQ 'NEW'      OR  "Frete Novo
           gv_acao  EQ 'EDIT' ) ) OR  "Edita Frete
           sy-ucomm EQ 'ORVD'.        "Gerar Ordem de Venda
* Númeração de uma nova negociação de frete.
      IF gv_acao EQ 'NEW'. "Frete Novo
        *zlest0255-status = '001'.
* Gera numeração do Id negociação da Prestação de Serviço de Frete.
        PERFORM zf_numerador_id_negociacao USING    *zlest0255-ano
                                           CHANGING *zlest0255-id_neg.
      ENDIF.
* Salvar dados de Frete Novo/Modificado.
      PERFORM zf_salva_frete_terceiro USING sy-ucomm
                                            gv_acao
                                            *zlest0255.

    ENDIF.

  ELSE.
    IF sy-ucomm EQ 'NEW'. "Novo Frete.
      *zlest0255-waerk = 'BRL'.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_numerador_id_negociacao
*&---------------------------------------------------------------------*
*& Gera numeração do Id negociação da Prestação de Serviço de Frete
*&---------------------------------------------------------------------*
*&   -->UV_ANO    Ano da Negociação de Frete de Terceiros
*&   <--CV_ID_NEG Id negociação
*&---------------------------------------------------------------------*
FORM zf_numerador_id_negociacao USING    uv_ano    TYPE zde_ano
                                CHANGING cv_id_neg TYPE zlesed_id_neg.

  DATA: vl_seq TYPE zlesed_id_neg.

  FREE cv_id_neg.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZLES_T0225'
    IMPORTING
      number      = vl_seq.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    gv_erro = abap_on.
    RETURN.

  ELSE.
    cv_id_neg = |{ uv_ano+2(2) }{ vl_seq+2 ALPHA = IN WIDTH = 8 }|.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_salva_frete_terceiro
*&---------------------------------------------------------------------*
*& Salvar dados de Frete Novo/Modificado
*&---------------------------------------------------------------------*
*&      --> UV_UCOMM      Comando acionado pelo Usuário
*&      --> UV_ACAO       Ação acionada pelo Usuário
*&      --> UV_ZLEST0255  Prestação de Serviço de Frete - Negociação
*&---------------------------------------------------------------------*
FORM zf_salva_frete_terceiro USING uv_ucomm     TYPE syucomm
                                   uv_acao      TYPE syucomm
                                   uv_zlest0255 TYPE zlest0255.


  IF uv_ucomm EQ 'SAVE' AND "Salvar Frete
     uv_acao  EQ 'NEW'.     "Frete Novo
    uv_zlest0255-user_create = sy-uname.
    uv_zlest0255-date_create = sy-datlo.
    uv_zlest0255-time_create = sy-timlo.

  ENDIF.

  MODIFY zlest0255 FROM uv_zlest0255.

  IF sy-subrc IS INITIAL.
* Salva texto observação da prestação de servoço de frete.
    PERFORM zf_salva_texto_observacao USING uv_zlest0255-id_neg.
    COMMIT WORK.
* Registros salvos com sucesso!
    MESSAGE s155(zehs).

  ELSE.
    ROLLBACK WORK.
* Erro ao salvar os dados
    MESSAGE s155(zlfm) DISPLAY LIKE 'E'.
    gv_erro = abap_on.
    RETURN.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_salva_texto_observacao
*&---------------------------------------------------------------------*
*& Salva texto observação da prestação de serviço de frete
*&---------------------------------------------------------------------*
*&      --> UV_ID_NEG Numerados do ID de Negociação
*&---------------------------------------------------------------------*
FORM zf_salva_texto_observacao USING uv_id_neg TYPE zlesed_id_neg.

  DATA: tl_lines TYPE tlinet,
        tl_lnspl TYPE trtexts.

  DATA: el_header TYPE thead,
        el_lines  TYPE tline.

  DATA: vl_obsrv TYPE c LENGTH 400,
        vl_fst_p TYPE c LENGTH 80,
        vl_tail  TYPE c LENGTH 80.

  CONSTANTS: cl_frtt      TYPE tdid     VALUE 'FRTT',
             cl_pt        TYPE spras    VALUE 'P',
             cl_zlesr0002 TYPE tdobject VALUE 'ZLESR0002'.

* Leitura do texto da Janel da Observação do Frete.
  CALL METHOD gcl_observacao->get_text_as_r3table
    IMPORTING
      table = tg_editor.
* Preparação do texto para salvar.
  LOOP AT tg_editor INTO DATA(el_editor).
    IF vl_obsrv IS INITIAL.
      vl_obsrv = el_editor-line.

    ELSE.
      vl_obsrv = |{ vl_obsrv }/LF{ el_editor-line }|.

    ENDIF.

  ENDLOOP.
* Quebra o texto em blocos de 72 caracteres.
  CALL FUNCTION 'TR_SPLIT_TEXT'
    EXPORTING
      iv_text  = vl_obsrv
      iv_len   = 72
    IMPORTING
      et_lines = tl_lnspl.

  IF NOT tl_lnspl[] IS INITIAL.
    LOOP AT tl_lnspl INTO DATA(el_lnspl).
      DO.
        SPLIT el_lnspl AT '/' INTO vl_fst_p vl_tail.
        IF vl_tail IS INITIAL.
          el_lines-tdline = el_lnspl.
          EXIT.

        ELSE.
          el_lines-tdline = vl_fst_p.
          APPEND el_lines TO tl_lines.
          CLEAR  el_lines.
          el_lines-tdformat = vl_tail(2).
          el_lnspl = vl_tail+2.
          CLEAR: vl_fst_p, vl_tail.

        ENDIF.

      ENDDO.

      APPEND el_lines TO tl_lines.
      CLEAR  el_lines.

    ENDLOOP.

    el_header-tdobject = cl_zlesr0002.
    el_header-tdid     = cl_frtt.
    el_header-tdspras  = cl_pt.
    el_header-tdname   = uv_id_neg.
* Salva o texto de observação da prestação de servoço de frete.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = el_header
        savemode_direct = abap_on
      TABLES
        lines           = tl_lines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.

    IF sy-subrc IS INITIAL.
* Confirma a gravação do texto de observação.
      CALL FUNCTION 'COMMIT_TEXT'
        EXPORTING
          object   = el_header-tdobject
          name     = el_header-tdname
          id       = el_header-tdid
          language = el_header-tdspras.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDIF.

  CLEAR: tl_lines, el_lines, vl_obsrv, el_header, el_editor, tl_lnspl, el_lnspl.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_show_data_frete_scr
*&---------------------------------------------------------------------*
*& Exibir os dados da pesquisa do frete de terceiro em tela
*&---------------------------------------------------------------------*
*&      --> UV_UCOMM Código de função. Valor do botão acionado pelo Users
*&---------------------------------------------------------------------*
FORM zf_show_data_frete_scr USING uv_ucomm TYPE syucomm.

  DATA: tl_index_rows TYPE lvc_t_row.

* Verifica se foi selecionada uma linha da Grid do ALV.
  PERFORM zf_check_selected_row TABLES tl_index_rows.

  CHECK NOT tl_index_rows[] IS INITIAL.

  READ TABLE tl_index_rows INTO DATA(el_index_rows) INDEX 1.
  READ TABLE tg_frete INTO DATA(el_frete) INDEX el_index_rows-index.

  SELECT SINGLE * FROM *zlest0255 WHERE bukrs  EQ el_frete-bukrs
                                    AND branch EQ el_frete-branch
                                    AND ano    EQ s_ano-low
                                    AND id_neg EQ el_frete-id_neg.

  IF uv_ucomm NE 'CANCEL' AND
     uv_ucomm NE 'FINAL'  AND
     uv_ucomm NE 'MTVO'   AND
     uv_ucomm NE 'QUANT'.

    IF uv_ucomm           EQ 'EDIT' AND
          el_frete-status NE '001'.
      EXIT.

    ENDIF.
* Lê o texto observação da prestação de serviço de frete.
    PERFORM zf_ler_texto_observacao USING *zlest0255-id_neg.
* Chama a Tela de Criação e Manutenção do frete.
    CALL SCREEN '0200'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_ler_texto_observacao
*&---------------------------------------------------------------------*
*& Lê o texto observação da prestação de serviço de frete
*&---------------------------------------------------------------------*
*&      --> UV_ID_NEG Numerados do ID de Negociação
*&---------------------------------------------------------------------*
FORM zf_ler_texto_observacao USING uv_id_neg TYPE zlesed_id_neg.

  DATA: tl_lines TYPE tlinet,
        tl_lnspl TYPE trtexts.

  DATA: el_lines  TYPE tline.

  DATA: vl_name  TYPE tdobname.

  CONSTANTS: cl_frtt      TYPE tdid     VALUE 'FRTT',
             cl_pt        TYPE spras    VALUE 'P',
             cl_zlesr0002 TYPE tdobject VALUE 'ZLESR0002'.

  vl_name = uv_id_neg.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = cl_frtt
      language                = cl_pt
      name                    = vl_name
      object                  = cl_zlesr0002
    TABLES
      lines                   = tl_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc IS INITIAL.
    LOOP AT tl_lines INTO el_lines.
      AT FIRST.
        APPEND INITIAL LINE TO tg_editor ASSIGNING FIELD-SYMBOL(<fs_editor>).

      ENDAT.

      IF el_lines-tdformat EQ 'LF'.
        APPEND INITIAL LINE TO tg_editor ASSIGNING <fs_editor>.
        <fs_editor>-line = el_lines-tdline.

      ELSE.
        IF <fs_editor>-line IS INITIAL.
          <fs_editor>-line = el_lines-tdline.

        ELSE.
          <fs_editor>-line = |{ <fs_editor>-line } { el_lines-tdline }|.

        ENDIF.

      ENDIF.

    ENDLOOP.
* Cria o editor da Observação do Frete
    PERFORM zf_tela_edit_observacao.
* Carrega o texto Observação do Frete lido da função READ_TEXT.
    CALL METHOD gcl_observacao->set_text_as_r3table
      EXPORTING
        table = tg_editor.

  ENDIF.

  CLEAR: tl_lines, el_lines, <fs_editor>.

  IF <fs_editor> IS ASSIGNED.
    UNASSIGN <fs_editor>.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_check_selected_row
*&---------------------------------------------------------------------*
*& Verifica se foi selecionada uma linha da Grid do ALV
*&---------------------------------------------------------------------*
*&      --> PT_INDEX_ROWS Controle VLA: linhas de tabela
*&---------------------------------------------------------------------*
FORM zf_check_selected_row TABLES pt_index_rows STRUCTURE lvc_s_row.

  CALL METHOD gcl_grid1->get_selected_rows
    IMPORTING
      et_index_rows = pt_index_rows[].

  IF pt_index_rows[] IS INITIAL.
* Selecione uma linha
    MESSAGE s059(wrf_prc_ctr) DISPLAY LIKE 'W'.
    LEAVE SCREEN.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_edit_data_frete_scr
*&---------------------------------------------------------------------*
*& Edita os dados da pesquisa do frete de terceiro em tela
*&---------------------------------------------------------------------*
*&      --> UV_UCOMM Código de função. Valor do botão acionado pelo User
*&---------------------------------------------------------------------*
FORM zf_edit_data_frete_scr USING uv_ucomm TYPE syucomm.

* Exibir os dados da pesquisa do frete de terceiro em tela.
  PERFORM zf_show_data_frete_scr USING uv_ucomm.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_status_0200_show
*&---------------------------------------------------------------------*
*& BPO da tela 0200 botão Exibir
*&---------------------------------------------------------------------*
*&      --> pt_exclud Tabela Interna de exclusão de botões
*&      <-- cv_title  Texto complemento do título da tela 0200
*&---------------------------------------------------------------------*
FORM zf_status_0200_show TABLES   pt_exclud TYPE STANDARD TABLE
                         CHANGING cv_title.

  cv_title = 'Exibir'.
  APPEND 'SAVE' TO pt_exclud.   "Salvar Frete

  LOOP AT SCREEN.
    IF screen-input EQ 1.
      screen-input = 0.
      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_status_0200_edit
*&---------------------------------------------------------------------*
*& BPO da tela 0200 botão Editar
*&---------------------------------------------------------------------*
*&      <-- cv_title  Texto complemento do título da tela 0200
*&---------------------------------------------------------------------*
FORM zf_status_0200_edit CHANGING cv_title.

  cv_title = 'Editar'.
  LOOP AT SCREEN.
* Verifivca se o Doc.vendas está vazio.
    IF *zlest0255-vbeln IS INITIAL.
      IF screen-group2 EQ 'ED2'.
        screen-input = 1.
        MODIFY SCREEN.

      ELSE.
        screen-input = 0.
        MODIFY SCREEN.

      ENDIF.

    ELSE.
      IF screen-group1 EQ 'EDT'.
        screen-input = 1.
        MODIFY SCREEN.

      ELSE.
        screen-input = 0.
        MODIFY SCREEN.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_cancel_final_data_frete
*&---------------------------------------------------------------------*
*& Cancelamento/Finalização do Frete de Terceiro
*&---------------------------------------------------------------------*
*&      --> UV_UCOMM Código de função. Valor do botão acionado pelo User
*&---------------------------------------------------------------------*
FORM zf_cancel_final_data_frete.
* Exibir os dados da pesquisa do frete de terceiro em tela.
  PERFORM zf_show_data_frete_scr USING sy-ucomm.
* Chama a Tela de opup de texto do Motivo de Cancelar/Finalizar Frete Teceiro.
  CALL SCREEN '0300' STARTING AT 7 7 ENDING AT 86 12.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_tela_edit_canc_final
*&---------------------------------------------------------------------*
*& Cria o editor do Cancela/Finaliza Frete de Terceiro
*&---------------------------------------------------------------------*
*&      --> UV_UCOMM Código de função. Valor do botão acionado pelo User
*&---------------------------------------------------------------------*
FORM zf_tela_edit_canc_final USING uv_ucomm TYPE syucomm.

  IF gcl_cc_can_fin IS INITIAL.
    CREATE OBJECT gcl_cc_can_fin
      EXPORTING
        container_name = gc_can_fin.

    IF gcl_cc_can_fin IS NOT INITIAL.
      CREATE OBJECT gcl_canc_final
        EXPORTING
          parent                     = gcl_cc_can_fin
          wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position          = 72
          max_number_chars           = 250
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
* Inibe a Barra de Tarefa do Editor
      CALL METHOD gcl_canc_final->set_toolbar_mode
        EXPORTING
          toolbar_mode = cl_gui_textedit=>false.
* Inibe a Barra de Status do Editor.
      CALL METHOD gcl_canc_final->set_statusbar_mode
        EXPORTING
          statusbar_mode = cl_gui_textedit=>true.

      IF uv_ucomm EQ 'CANCEL' OR "Cancelar Frete
         uv_ucomm EQ 'FINAL'.    "Finalizar Frete
* Exibe o Editor para edição.
        CALL METHOD gcl_canc_final->set_readonly_mode
          EXPORTING
            readonly_mode = cl_gui_textedit=>false.

      ELSE.
* Exibe o Editor somente leitura.
        CALL METHOD gcl_canc_final->set_readonly_mode
          EXPORTING
            readonly_mode = cl_gui_textedit=>true.

      ENDIF.

    ENDIF.

  ELSE.
    IF     gv_ucomm EQ 'SHOW' AND "Exibir Frete
           uv_ucomm EQ 'MTVO' OR  "Exibir Motivo de Cancelamento/Finalização
         ( gv_ucomm EQ 'MTVO' AND "Exibir Frete
           uv_ucomm EQ 'MTVO' ).  "Exibir Frete

* Exibe o Editor somente leitura.
      CALL METHOD gcl_canc_final->set_readonly_mode
        EXPORTING
          readonly_mode = cl_gui_textedit=>true.

    ELSEIF uv_ucomm EQ 'CANCEL' OR "Cancelar Frete
           uv_ucomm EQ 'FINAL'.    "Finalizar Frete
* Exibe o Editor para edição.
      CALL METHOD gcl_canc_final->set_readonly_mode
        EXPORTING
          readonly_mode = cl_gui_textedit=>false.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_salva_texto_canc_final
*&---------------------------------------------------------------------*
*& Salvar texto do motivo de Cancelamento/Finalização do Frete de Terceiro
*&---------------------------------------------------------------------*
*&      --> UV_ACAO      Ação acionada pelo Usuário
*&      --> UV_ZLEST0255 Prestação de Serviço de Frete - Negociação
*&---------------------------------------------------------------------*
FORM zf_salva_texto_canc_final USING uv_acao      TYPE syucomm
                                     uv_zlest0255 TYPE zlest0255.

  DATA: tl_lines  TYPE tlinet.

  DATA: el_header TYPE thead,
        el_lines  TYPE tline.

  CONSTANTS: cl_canc      TYPE tdid     VALUE 'CANC',
             cl_fina      TYPE tdid     VALUE 'FINA',
             cl_pt        TYPE spras    VALUE 'P',
             cl_zlesr0002 TYPE tdobject VALUE 'ZLESR0002'.

  CLEAR tg_editor2.
* Leitura do texto da Janel do Motivo do Cancelamento/Finalização do Frete.
  CALL METHOD gcl_canc_final->get_text_as_r3table
    IMPORTING
      table = tg_editor2.

  IF tg_editor2[] IS INITIAL.
    MESSAGE s278(00) WITH 'Descrição do motivo' DISPLAY LIKE 'E'.
    LEAVE SCREEN.

  ENDIF.
* Define o valor do Status do Frete de Terceiros.
  uv_zlest0255-status = COND #( WHEN uv_acao EQ 'CANCEL' THEN '002'
                                WHEN uv_acao EQ 'FINAL'  THEN '003' ).
* Define o valor do ID de texto.
  DATA(vl_tdid) = COND tdid( WHEN uv_acao EQ 'CANCEL' THEN cl_canc
                             WHEN uv_acao EQ 'FINAL'  THEN cl_fina ).

  CASE uv_acao.
    WHEN 'CANCEL'. "Frete Cancelado
      uv_zlest0255-user_CANCEL = sy-uname.
      uv_zlest0255-date_CANCEL = sy-datlo.
      uv_zlest0255-time_CANCEL = sy-timlo.

    WHEN 'FINAL'.  "Frete Finalizado
      uv_zlest0255-user_FINALIZADO = sy-uname.
      uv_zlest0255-date_FINALIZADO = sy-datlo.
      uv_zlest0255-time_FINALIZADO = sy-timlo.

    WHEN OTHERS.
*   Do nothing
  ENDCASE.

  MODIFY zlest0255 FROM uv_zlest0255.

  IF sy-subrc IS INITIAL.
* Preparação do texto para salvar.
    LOOP AT tg_editor2 INTO DATA(el_editor).
      el_lines-tdline = el_editor-line.
      APPEND el_lines TO tl_lines.
      CLEAR  el_lines.

    ENDLOOP.

    el_header-tdobject = cl_zlesr0002.
    el_header-tdid     = vl_tdid.
    el_header-tdspras  = cl_pt.
    el_header-tdname   = *zlest0255-id_neg.
* Salva o texto de observação da prestação de servoço de frete.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = el_header
        savemode_direct = abap_on
      TABLES
        lines           = tl_lines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.

    IF sy-subrc IS INITIAL.
* Confirma a gravação do texto de observação.
      CALL FUNCTION 'COMMIT_TEXT'
        EXPORTING
          object   = el_header-tdobject
          name     = el_header-tdname
          id       = el_header-tdid
          language = el_header-tdspras.

      READ TABLE tg_frete ASSIGNING FIELD-SYMBOL(<fs_frete>) WITH KEY bukrs  = *zlest0255-bukrs
                                                                      id_neg = *zlest0255-id_neg.

      IF sy-subrc IS INITIAL.
        <fs_frete>-status = *zlest0255-status.
        gcl_grid1->refresh_table_display( ).

      ENDIF.


    ELSE.
      ROLLBACK WORK.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      LEAVE SCREEN.

    ENDIF.

    COMMIT WORK.
* Registros salvos com sucesso!
    MESSAGE s155(zehs).

  ELSE.
    ROLLBACK WORK.
* Erro ao salvar os dados
    MESSAGE s155(zlfm) DISPLAY LIKE 'E'.
    LEAVE SCREEN.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_motivo_data_frete
*&---------------------------------------------------------------------*
*& Visualizar Motivo de Cancelamento/Finalização do Frete de Terceiro
*&---------------------------------------------------------------------*
FORM zf_motivo_data_frete.

* Exibir os dados da pesquisa do frete de terceiro em tela.
  PERFORM zf_show_data_frete_scr USING sy-ucomm.
  CHECK *zlest0255-status EQ '002' OR
        *zlest0255-status EQ '003'.
* Lê o texto do Motivo da Cancelamento/Finalização do Frete.
  PERFORM zf_ler_mtvo_cancel_final USING *zlest0255.
* Chama a Tela de opup de texto do Motivo de Cancelar/Finalizar Frete Teceiro.
  CALL SCREEN '0300' STARTING AT 7 7 ENDING AT 86 12.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_ler_mtvo_cancel_final
*&---------------------------------------------------------------------*
*& Lê o texto do Motivo da Cancelamento/Finalização do Frete
*&---------------------------------------------------------------------*
*&      --> UV_ZLEST0255 Prestação de Serviço de Frete - Negociação
*&---------------------------------------------------------------------*
FORM zf_ler_mtvo_cancel_final USING uv_zlest0255 TYPE zlest0255.

  DATA: tl_lines TYPE tlinet.

  DATA: vl_name  TYPE tdobname.

  CONSTANTS: cl_canc      TYPE tdid     VALUE 'CANC',
             cl_fina      TYPE tdid     VALUE 'FINA',
             cl_pt        TYPE spras    VALUE 'P',
             cl_zlesr0002 TYPE tdobject VALUE 'ZLESR0002'.

  vl_name = uv_ZLEST0255-id_neg.
* Define o valor do ID de texto.
  DATA(vl_tdid) = COND tdid( WHEN uv_zlest0255-status EQ '002' THEN cl_canc
                             WHEN uv_zlest0255-status EQ '003' THEN cl_fina ).

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = vl_tdid
      language                = cl_pt
      name                    = vl_name
      object                  = cl_zlesr0002
    TABLES
      lines                   = tl_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc IS INITIAL.
    CLEAR tg_editor2.
    LOOP AT tl_lines INTO DATA(el_lines).
      APPEND INITIAL LINE TO tg_editor2 ASSIGNING FIELD-SYMBOL(<fs_editor>).
      <fs_editor>-line = el_lines-tdline.

    ENDLOOP.

  ENDIF.

  CLEAR: tl_lines, el_lines, <fs_editor>.

  IF <fs_editor> IS ASSIGNED.
    UNASSIGN <fs_editor>.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_altera_qtd_frete_popup
*&---------------------------------------------------------------------*
*& topup de Alteração de Quantidade do Frete
*&---------------------------------------------------------------------*
FORM zf_altera_qtd_frete_popup.

  DATA: tl_fields TYPE TABLE OF sval.

  DATA: el_fields TYPE          sval.

  DATA: vl_resp TYPE c.

* Carrega a estrutura *ZLEST0255.
* Exibir os dados da pesquisa do frete de terceiro em tela.
  PERFORM zf_show_data_frete_scr USING sy-ucomm.

  CHECK *zlest0255-status EQ '001'.

  REFRESH tl_fields.
*** Quantidade Total
  el_fields-tabname    = 'ZLEST0255'.
  el_fields-fieldname  = 'QUANTIDADE'.
  el_fields-value      = *zlest0255-quantidade.
  el_fields-field_obl  = abap_on.
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.
*** Unidade da qtdade a ser transportada
  el_fields-tabname    = 'ZLEST0255'.
  el_fields-fieldname  = 'UNID_QUANT'.
  el_fields-value      = *zlest0255-unid_quant.
  el_fields-field_obl  = abap_off.
  el_fields-field_attr = '03'.
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.

  DATA(vl_ucomm) = sy-ucomm.
* Função de exibição de Popup.
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Alterar Quantidade Frete'
      start_column    = 20
      start_row       = 10
    IMPORTING
      returncode      = vl_resp
    TABLES
      fields          = tl_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  sy-ucomm = vl_ucomm.

  IF NOT sy-subrc IS INITIAL .
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ELSE.
    CHECK vl_resp IS INITIAL.
    READ TABLE tl_fields INTO el_fields INDEX 1.
    CONDENSE el_fields-value NO-GAPS.

    IF *zlest0255-quantidade NE el_fields-value.
      *zlest0255-quantidade = el_fields-value.

      UPDATE zlest0255
         SET quantidade = *zlest0255-quantidade
      WHERE bukrs  EQ *zlest0255-bukrs
        AND branch EQ *zlest0255-branch
        AND ano    EQ *zlest0255-ano
        AND id_neg EQ *zlest0255-id_neg.

      IF sy-subrc IS INITIAL.
        COMMIT WORK.
        READ TABLE tg_frete ASSIGNING FIELD-SYMBOL(<fs_frete>) WITH KEY bukrs = *zlest0255-bukrs
                                                                        id_neg = *zlest0255-id_neg.

        IF sy-subrc IS INITIAL.
          <fs_frete>-quantidade = *zlest0255-quantidade.
          gcl_grid1->refresh_table_display( ).

        ENDIF.

        MESSAGE |Quantidade atualizada com sucesso.| TYPE 'S'.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_srh_tp_filial_tarsp
*&---------------------------------------------------------------------*
*& Ajuda de pesquisa do Filial de Transporte
*&---------------------------------------------------------------------*
FORM zf_srh_tp_filial_tarsp.

  TYPES: BEGIN OF ty_filial_transp,
           branch TYPE j_1bbranc_,
           name   TYPE name1,
           bukrs  TYPE bukrs,
         END   OF ty_filial_transp.

  DATA: tl_rettab        TYPE TABLE OF ddshretval,
        tl_fields        TYPE TABLE OF dfies,
        tl_dyread        TYPE TABLE OF dynpread,
        tl_filial_transp TYPE TABLE OF ty_filial_transp.

  DATA: el_dyread TYPE          dynpread.

  DATA: vl_retfield  TYPE fieldname,
        vl_dynprofld TYPE dynfnam,
        vl_valor     TYPE char3.

  SELECT * FROM zlest0207 INTO TABLE @DATA(tl_t0207) WHERE bukrs EQ @*zlest0255-bukrs.

  SELECT branch name bukrs
    FROM j_1bbranch
    INTO TABLE tl_filial_transp
    FOR ALL ENTRIES IN tl_t0207
  WHERE bukrs  EQ tl_t0207-bukrs
    AND branch EQ tl_t0207-tdlnr+6(4).

  PERFORM get_fields_of_value_tab IN PROGRAM saplsdhi
                                      TABLES tl_filial_transp
                                             tl_fields
                                    CHANGING vl_retfield.

  READ TABLE tl_fields INTO DATA(el_fields) INDEX 1.
  CLEAR: tl_rettab, vl_dynprofld.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = el_fields-fieldname
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = vl_dynprofld
      window_title    = el_fields-fieldtext
      value_org       = sy-abcde+18(1) "S
    TABLES
      value_tab       = tl_filial_transp
      field_tab       = tl_fields
      return_tab      = tl_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc IS INITIAL.
* Verifica se a irformação seleciona da TI.
    CHECK NOT tl_rettab[] IS INITIAL.

    READ TABLE tl_rettab INTO DATA(el_rettab) INDEX 1.
    vl_retfield         = '*ZLEST0255-BRANCH'.
    *zlest0255-branch = vl_valor = el_rettab-fieldval.

    MOVE: vl_retfield TO el_dyread-fieldname,
          0           TO el_dyread-stepl,
          vl_valor    TO el_dyread-fieldvalue,
          abap_on     TO el_dyread-fieldinp.
    APPEND el_dyread  TO tl_dyread.
    CLEAR  el_dyread.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = sy-cprog
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = tl_dyread
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        OTHERS               = 8.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_gera_ordem_venda
*&---------------------------------------------------------------------*
*& Geração da ordem de venda do Frete de Terceiro
*&---------------------------------------------------------------------*
*&      --> UV_ZLEST0255 Prestação de Serviço de Frete - Negociação
*&---------------------------------------------------------------------*
FORM zf_gera_ordem_venda USING uv_zlest0255 TYPE zlest0255.

  DATA: BEGIN OF tl_partner OCCURS 0.
          INCLUDE STRUCTURE bapiparnr.
  DATA: END OF tl_partner.

  DATA: BEGIN OF tl_itemdata OCCURS 0.
          INCLUDE STRUCTURE bapisditm.
  DATA: END OF tl_itemdata.

  DATA: BEGIN OF tl_condition OCCURS 0.
          INCLUDE STRUCTURE bapicond .
  DATA: END OF tl_condition.

  DATA: BEGIN OF tl_text OCCURS 0.
          INCLUDE STRUCTURE bapisdtext.
  DATA: END OF tl_text.

  DATA: tl_return       TYPE          bapirettab,
        tl_schedules_in TYPE TABLE OF bapischdl  WITH HEADER LINE,
        tl_items_inx    TYPE TABLE OF bapisditmx WITH HEADER LINE.

  DATA: el_header_in    TYPE bapisdhd1,
        el_itemdata     TYPE bapisditm,
        el_header_inx2  TYPE bapisdh1x,
        el_header_inx   TYPE bapisdhd1x,
        el_items_inx    TYPE bapisditmx,
        el_schedules_in TYPE bapischdl,
        el_condition    TYPE bapicond,
        el_partner      TYPE bapiparnr,
        el_return       TYPE bapiret2.

  DATA: vl_vbeln_ov TYPE bapivbeln-vbeln,
        vl_tdlnr    TYPE tdlnr,
        vl_industry TYPE j_1bindus2.

  DATA(vl_dstcat) = COND j_1bdstcat( WHEN uv_zlest0255-pc_uf EQ uv_zlest0255-lr_uf THEN 0     "Mesmo Estado
                                     WHEN uv_zlest0255-pc_uf NE uv_zlest0255-lr_uf THEN 1 ).  "Estado Diferente

  SELECT SINGLE * FROM knvv INTO @DATA(el_knvv) WHERE kunnr EQ  @uv_zlest0255-id_cliente
                                                  AND vkorg EQ  @uv_zlest0255-bukrs
                                                  AND vtweg EQ '10'
                                                  AND spart EQ '08'.

  IF el_knvv-kdgrp IS INITIAL.
    MESSAGE |Não localizada classificação de Grupo de cliente para: { el_knvv-kunnr }, Org. Venda: { el_knvv-vkorg }, Canal: { el_knvv-vtweg } e Setor: { el_knvv-spart }.| TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.

  ELSE.
    vl_tdlnr    = |{ uv_zlest0255-branch ALPHA = IN WIDTH = 10 }|.
    vl_industry = |{ el_knvv-kdgrp       ALPHA = IN WIDTH = 2  }|.
* Busca o Código da natureza de operação
    SELECT SINGLE * FROM zlest0030 INTO @DATA(el_t0030) WHERE direct     EQ '2'                  "Saída
                                                          AND dstcat     EQ @vl_dstcat
                                                          AND industry   EQ @vl_industry
                                                          AND tpparceiro EQ '0'                  "Proprio
                                                          AND vkaus      EQ 'T'                  "Transportes
                                                          AND tdlnr      EQ @vl_tdlnr
                                                          AND bukrs      EQ @uv_zlest0255-bukrs.

    IF sy-subrc IS INITIAL.
      IF el_t0030-cfop IS INITIAL.
* Busca o Código da natureza de operação
        SELECT SINGLE * FROM zlest0030 INTO @el_t0030 WHERE direct     EQ '2'            "Saída
                                                        AND dstcat     EQ @vl_dstcat
                                                        AND industry   EQ @vl_industry
                                                        AND tpparceiro EQ '0'            "Proprio
                                                        AND vkaus      EQ 'T'            "Transportes
                                                        AND tdlnr      EQ @space
                                                        AND bukrs      EQ @space.

        IF sy-subrc IS INITIAL.
          IF el_t0030-cfop IS INITIAL.
            MESSAGE |Não localizado CFOP para Direção: '{ el_t0030-direct }', Ctg.Dest: '{ el_t0030-dstcat }', LNeg.Ctg: '{ el_t0030-industry }', Tp.Doc.Parc: '{ el_t0030-tpparceiro }', Utiliz: '{ el_t0030-vkaus }'.| TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.

          ELSE.
            IF     uv_zlest0255-pc_uf EQ uv_zlest0255-emit_uf.
              el_itemdata-cfop_long = el_t0030-cfop.

            ELSEIF uv_zlest0255-pc_uf NE uv_zlest0255-emit_uf.
              IF el_t0030-cfop_uf_emit_dif_prest IS INITIAL.
                MESSAGE |Não localizado CFOP fora do estado para Direção: '{ el_t0030-direct }', Ctg.Dest: '{ el_t0030-dstcat }', LNeg.Ctg: '{ el_t0030-industry }', Tp.Doc.Parc: '{ el_t0030-tpparceiro }', Utiliz: '{ el_t0030-vkaus }'.| TYPE 'S' DISPLAY
LIKE 'E'.
                EXIT.

              ELSE.
                el_itemdata-cfop_long = el_t0030-cfop_uf_emit_dif_prest.

              ENDIF.

            ENDIF.

          ENDIF.

        ELSE.
          MESSAGE |Não localizado CFOP para Direção: '2', Ctg.Dest: '{ vl_dstcat }', LNeg.Ctg: '{ vl_industry }', Tp.Doc.Parc: '0', Utiliz: 'T'.| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.

        ENDIF.

      ELSE.
        el_itemdata-cfop_long = el_t0030-cfop.

      ENDIF.

    ELSE.
* Busca o Código da natureza de operação
      SELECT SINGLE * FROM zlest0030 INTO @el_t0030 WHERE direct     EQ '2'            "Saída
                                                      AND dstcat     EQ @vl_dstcat
                                                      AND industry   EQ @vl_industry
                                                      AND tpparceiro EQ '0'            "Proprio
                                                      AND vkaus      EQ 'T'            "Transportes
                                                      AND tdlnr      EQ @space
                                                      AND bukrs      EQ @space.
      IF sy-subrc IS INITIAL.
        IF el_t0030-cfop IS INITIAL.
          MESSAGE |Não localizado CFOP para Direção: '{ el_t0030-direct }', Ctg.Dest: '{ el_t0030-dstcat }', LNeg.Ctg: '{ el_t0030-industry }', Tp.Doc.Parc: '{ el_t0030-tpparceiro }', Utiliz: '{ el_t0030-vkaus }'.| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.

        ELSE.
          IF     uv_zlest0255-pc_uf EQ uv_zlest0255-emit_uf.
            el_itemdata-cfop_long = el_t0030-cfop.

          ELSEIF uv_zlest0255-pc_uf NE uv_zlest0255-emit_uf.
            IF el_t0030-cfop_uf_emit_dif_prest IS INITIAL.
              MESSAGE |Não localizado CFOP fora do estado para Direção: '{ el_t0030-direct }', Ctg.Dest: '{ el_t0030-dstcat }', LNeg.Ctg: '{ el_t0030-industry }', Tp.Doc.Parc: '{ el_t0030-tpparceiro }', Utiliz: '{ el_t0030-vkaus }'.| TYPE 'S' DISPLAY
LIKE 'E'.
              EXIT.

            ELSE.
              el_itemdata-cfop_long = el_t0030-cfop_uf_emit_dif_prest.

            ENDIF.

          ENDIF.

        ENDIF.

      ELSE.
        MESSAGE |Não localizado CFOP para Direção: '2', Ctg.Dest: '{ vl_dstcat }', LNeg.Ctg: '{ vl_industry }', Tp.Doc.Parc: '0', Utiliz: 'T'.| TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.

      ENDIF.

    ENDIF.

  ENDIF.
*** Cabeçalho
  el_header_in-sales_org   = uv_zlest0255-bukrs.        "Org. de venda
  el_header_in-distr_chan  = '10'.                      "Canal distr.
  el_header_in-currency    = uv_zlest0255-waerk.        "Moeda
  el_header_in-pymt_meth   = 'P'.                       "Forma recebto
  el_header_in-division    = '08'.                      "Setor atividade
  el_header_in-doc_type    = 'ZTER'.                    "Tipo de ordem
  el_header_in-pmnttrms    =  uv_zlest0255-zterm.
  el_header_in-exrate_fi   = ''.                        "Taxa dolar
  el_header_in-bill_date   = uv_zlest0255-date_create.
  el_header_in-purch_no_c  = uv_zlest0255-id_neg.
  el_header_in-purch_no_s  = '.'.
  el_header_in-fix_val_dy  = uv_zlest0255-date_create.  "Data efetiva fixa
  el_header_in-pymt_meth   = ''.                        "Forma de pagamento
  el_header_in-dlvschduse  = 'T'.                       "Código de utilização
  el_header_in-incoterms1  = 'CIF'.
  el_header_in-incoterms2  = 'Serviço Frete Terceiros'.
*** Item
  el_itemdata-itm_number = '000010'.
  el_itemdata-material   = uv_zlest0255-matnr.          "Material da Ordem
  el_itemdata-plant      = uv_zlest0255-branch.
  el_itemdata-division   = '08'.                        "Setor atividade
  el_itemdata-target_qty = uv_zlest0255-quantidade.
  el_itemdata-target_qu  = uv_zlest0255-unid_quant.
  el_itemdata-sales_unit = uv_zlest0255-unid_quant.
  el_itemdata-gross_wght = uv_zlest0255-quantidade.
  el_itemdata-net_weight = uv_zlest0255-quantidade.
  el_itemdata-untof_wght = uv_zlest0255-unid_quant.
  el_itemdata-fix_val_dy = uv_zlest0255-date_create.
  el_itemdata-price_date = uv_zlest0255-date_create.
  el_itemdata-ex_rate_fi = ' '.
  el_itemdata-dlvschduse = 'T'.
  el_itemdata-incoterms1 = 'CIF'.
  el_itemdata-incoterms2 = 'Serviço'.
  el_itemdata-purch_no_c = uv_zlest0255-id_neg.
* Busca do Intinerário.
  SELECT SINGLE route FROM trolz INTO el_itemdata-route WHERE aland EQ 'BR'
                                                          AND azone EQ gv_zone_pc
                                                          AND lland EQ 'BR'
                                                          AND lzone EQ gv_zone_lr.

  APPEND el_itemdata TO tl_itemdata.

*** Item_inx  e schedules_in
  el_items_inx-itm_number = '000010'.
  el_items_inx-target_qty = abap_on.
  APPEND el_items_inx TO tl_items_inx.

  el_schedules_in-itm_number = '000010'.
  el_schedules_in-req_qty    = uv_zlest0255-quantidade.
  APPEND el_schedules_in TO tl_schedules_in.
*** Condição
  el_condition-itm_number = '000010'.
  el_condition-cond_type  = 'PR00'.
  el_condition-cond_value = uv_zlest0255-tarifa.
  el_condition-currency   = uv_zlest0255-waerk.
  el_condition-cond_unit  = uv_zlest0255-unid_tarifa.
  APPEND el_condition TO tl_condition.
*** Parceiros.
  el_partner-partn_role = 'AG'.
  el_partner-partn_numb = uv_zlest0255-id_cliente.
  APPEND el_partner TO tl_partner.

  el_partner-partn_role = 'RE'.
  el_partner-partn_numb = uv_zlest0255-id_cliente.
  APPEND el_partner TO tl_partner.

  el_partner-partn_role = 'RG'.
  el_partner-partn_numb = uv_zlest0255-id_cliente.
  APPEND el_partner TO tl_partner.

  el_partner-partn_role = 'WE'.
  el_partner-partn_numb = uv_zlest0255-id_cliente.
  APPEND el_partner TO tl_partner.

  el_partner-partn_role = 'PC'.
  el_partner-partn_numb = uv_zlest0255-pc_codigo.
  APPEND el_partner TO tl_partner.

  el_partner-partn_role = 'RM'.
  el_partner-partn_numb = uv_zlest0255-reme_codigo.
  APPEND el_partner TO tl_partner.

  el_partner-partn_role = 'LR'.
  el_partner-partn_numb = uv_zlest0255-lr_codigo.
  APPEND el_partner TO tl_partner.

  el_partner-partn_role = 'SP'.
  el_partner-partn_numb = |{ uv_zlest0255-branch  ALPHA = IN WIDTH = 10 }|.
  APPEND el_partner TO tl_partner.
*** Texto do cabeçalho.
* O Texto de observação da Integração salvo na tabela será inserido no texto
* do cabeçalho da OV para atender o campo "obsevações"  no STRADA.
  tl_text-text_id   = '0001'.
  tl_text-langu     = 'PT'.
  tl_text-text_line = uv_zlest0255-obs_integ.
  APPEND tl_text.
* Função de geração da Ordem de Venda.
  CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
    EXPORTING
      sales_header_in     = el_header_in
      sales_header_inx    = el_header_inx
    IMPORTING
      salesdocument_ex    = vl_vbeln_ov
    TABLES
      return              = tl_return
      sales_items_in      = tl_itemdata
      sales_items_inx     = tl_items_inx
      sales_partners      = tl_partner
      sales_schedules_in  = tl_schedules_in
      sales_conditions_in = tl_condition
      sales_text          = tl_text.

  READ TABLE tl_return INTO el_return WITH KEY type = 'E'.
  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    DATA(lcl_return) = NEW zcl_zmmr149_utilities( ).
* Exibe Mensagem De retorno da BAPI de criação da Ordem de Venda.
    lcl_return->exibe_msgs_valida_dados_ext( EXPORTING it_msg_valida = tl_return ).

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_on.

    uv_zlest0255-vbeln = vl_vbeln_ov.

    READ TABLE tg_frete ASSIGNING FIELD-SYMBOL(<fs_frete>) WITH KEY bukrs  = uv_zlest0255-bukrs
                                                                    id_neg = uv_zlest0255-id_neg.

    IF sy-subrc IS INITIAL.
      UPDATE zlest0255
         SET vbeln = vl_vbeln_ov
      WHERE bukrs  EQ uv_zlest0255-bukrs
        AND branch EQ uv_zlest0255-branch
        AND ano    EQ uv_zlest0255-ano
        AND id_neg EQ uv_zlest0255-id_neg.

      IF sy-subrc IS INITIAL.
        COMMIT WORK.
        MESSAGE |Ordem de vendas criada com sucesso. #{ vl_vbeln_ov ALPHA = OUT }| TYPE 'S'.

      ELSE.
        ROLLBACK WORK.
        MESSAGE |Erro ao salvar o número da Ordem de Venda. #{ vl_vbeln_ov ALPHA = OUT }| TYPE 'S' DISPLAY LIKE 'E'.

      ENDIF.

    ENDIF.

  ENDIF.

  sy-ucomm = gv_acao.

ENDFORM.
