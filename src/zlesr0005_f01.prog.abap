*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 10/10/2025                                              &*
*& Descrição: Serviço de Frete de Terceiros                           &*
*& Transação: ZLES0224 (Prest. Serv. Frete - Faturar)                 &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2T37 |10/10/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*
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
* Cliente
  el_opts-name       = cl_objectkey2.
  APPEND el_opts TO tl_screen-opt_list_tab.
  el_assoc-name      = 'S_CLIENT'.
  el_assoc-op_main   = cl_objectkey2.
  APPEND el_assoc TO tl_screen-ass_tab.
* ID Negociação
  el_opts-name       = cl_objectkey3.
  APPEND el_opts TO tl_screen-opt_list_tab.
  el_assoc-name      = 'S_ID_NEG'.
  el_assoc-op_main   = cl_objectkey3.
  APPEND el_assoc TO tl_screen-ass_tab.
* Período
  el_opts-name       = cl_objectkey4.
  APPEND el_opts TO tl_screen-opt_list_tab.
  el_assoc-name      = 'S_DTMOV'.
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

      WHEN '0200'.
        IF uv_namef EQ '*ZSDT0001OD-NR_ORDEM' OR
           uv_namef EQ '*ZSDT0001OD-NR_SAFRA'.
          lv_check = abap_on.

        ENDIF.

      WHEN OTHERS.
*     Do Nothing.
    ENDCASE.

    IF     uv_value IS INITIAL AND
       NOT lv_check IS INITIAL.

      IF uv_dynnr EQ '0200'.
* Preencher todos os campos obrigat rios
        MESSAGE s055(00) DISPLAY LIKE 'E'.
        gv_erro = abap_on.
        SET CURSOR FIELD uv_namef.

      ELSE.
* Preencher todos os campos obrigat rios
        MESSAGE e055(00).

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_show_data_frete_fat
*&---------------------------------------------------------------------*
*& Exibe os dados selecionado da pesquisa
*&---------------------------------------------------------------------*
FORM zf_show_data_frete_fat.

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
        it_outtab            = tg_frete_fat.

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

  CLEAR tg_fcat.

* Função de de catalogo de campos conforme estrutura definida
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZLESE_ALV_FRETE_FATURAR'
    CHANGING
      ct_fieldcat      = tg_fcat.

  LOOP AT tg_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN 'BRANCH'.
        vl_texto = 'Filial Transp'.

      WHEN 'TR_PLACA'.
        vl_texto = 'Placa'.

      WHEN 'DATE_CREATE'.
        vl_texto = 'Data'.

      WHEN 'ID_MOTORISTA'.
        vl_texto = |Cód. { <fs_fcat>-scrtext_s } |.

      WHEN 'NOME_MOTO'.
        vl_texto = 'Nome Motorista'.

      WHEN 'GR_MERCADORIA'.
        vl_texto = 'Gr mercadoria'.

      WHEN 'WGBEZ60'.
        vl_texto = 'Gr Descrição'.

      WHEN 'QUANTIDADE'.
        vl_texto = 'Qtdade'.

      WHEN 'TARIFA'.
        vl_texto = 'Vlr. Frete'.

      WHEN 'DOC_REM'.
        vl_texto = 'Remessa'.

      WHEN 'TKNUM'.
        vl_texto = 'Doc. Transp.'.

      WHEN 'FKNUM'.
        vl_texto = 'Doc. Custo'.

      WHEN 'OV_SERV'.
        vl_texto = 'OV Serviço'.

      WHEN 'FAT_SERV'.
        vl_texto = 'Fatura'.

      WHEN 'DOCNUM_SERV'.
        vl_texto = 'Dacte'.

      WHEN 'ST_DACTE'.
        vl_texto = 'St. Dacte'.

      WHEN 'DOCNUM_MDFE'.
        vl_texto = 'Mdfe'.

      WHEN 'ST_MDFE'.
        vl_texto = 'St. Mdfe'.

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
*&---------------------------------------------------------------------*
*& Form zf_select_data_frete_fat
*&---------------------------------------------------------------------*
*& Busca os dados da pesquisa do frete faturado
*&---------------------------------------------------------------------*
FORM zf_select_data_frete_fat.

  IF s_dtmov[]  IS INITIAL AND
     s_client[] IS INITIAL.
    MESSAGE |Campos Período e Clinete vazios. Informar um deles.| TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.

  ENDIF.

  LOOP AT s_branch.
    SELECT SINGLE * FROM j_1bbranch INTO @DATA(el_branch) WHERE bukrs EQ @p_bukrs AND branch EQ @s_branch-low.

    IF NOT sy-subrc IS INITIAL.
* Filial &2 não existe para empresa &1
      MESSAGE s782(8z) WITH p_bukrs s_branch-low DISPLAY LIKE 'E'.
      DATA(vl_exit) = abap_on.
      EXIT.

    ENDIF.

  ENDLOOP.

  IF NOT vl_exit IS INITIAL.
    RETURN.

  ENDIF.

  SELECT a~id_neg a~nr_ordem a~branch a~tr_placa a~date_create a~id_motorista f~name1 AS nome_moto
         a~gr_mercadoria e~wgbez60 a~ds_prod_pred b~quantidade b~peso_bruto b~peso_liq
         d~tarifa c~doc_rem c~tknum c~fknum c~ov_serv c~fat_serv c~docnum_serv
         c~docnum_mdfe
    FROM zlest0257 AS a
    INNER JOIN zlest0258 AS b
     ON a~id_ordem EQ b~id_ordem
    INNER JOIN zlest0259 AS c
     ON b~id_ordem EQ c~id_ordem
    LEFT JOIN zlest0255 AS d
     ON a~bukrs   EQ d~bukrs  AND
        a~branch  EQ d~branch AND
        a~ano     EQ d~ano    AND
        a~id_neg  EQ d~id_neg
    LEFT JOIN t023t AS e
     ON a~gr_mercadoria EQ e~matkl
    LEFT JOIN lfa1 AS f
     ON a~id_motorista EQ f~lifnr
    INTO CORRESPONDING FIELDS OF TABLE tg_frete_fat
  WHERE a~bukrs       EQ p_bukrs
    AND a~branch      IN s_branch
    AND a~id_cliente  IN s_client
    AND a~id_neg      IN s_id_neg
    AND a~date_create IN s_dtmov.

  IF sy-subrc IS INITIAL.
    SORT tg_frete_fat BY docnum_serv.
    SELECT docnum, cancel, code, model, candat FROM j_1bnfdoc
      INTO TABLE @DATA(tl_nfdoc_st)
      FOR ALL ENTRIES IN @tg_frete_fat
    WHERE docnum EQ @tg_frete_fat-docnum_serv.

    IF sy-subrc IS INITIAL.
      LOOP AT tl_nfdoc_st INTO DATA(el_nfdoc_st).
        READ TABLE tg_frete_fat ASSIGNING FIELD-SYMBOL(<fs_frete_fat>) WITH KEY docnum_serv = el_nfdoc_st-docnum.

        IF sy-subrc IS INITIAL.
          IF     el_nfdoc_st-cancel IS INITIAL AND
                 el_nfdoc_st-code   EQ '100'.
            <fs_frete_fat>-status = 'Faturado'.
* Verifica o Modelo da Nota.
            CASE el_nfdoc_st-model.
              WHEN '57'. "Conhecimento de Transporte Eletrônico
                <fs_frete_fat>-st_dacte = 'Autorizado'.

              WHEN '58'. "Manifesto Eletrônico de Documentos Fiscais
                <fs_frete_fat>-st_mdfe = 'Autorizado'.

              WHEN OTHERS.
*             Do nothing
            ENDCASE.

          ELSEIF el_nfdoc_st-cancel IS INITIAL AND
                 el_nfdoc_st-code   IS INITIAL.
            <fs_frete_fat>-status = 'Pendente'.
* Verifica o Modelo da Nota.
            CASE el_nfdoc_st-model.
              WHEN '57'. "Conhecimento de Transporte Eletrônico
                <fs_frete_fat>-st_dacte = 'A determinar'.

              WHEN '58'. "Manifesto Eletrônico de Documentos Fiscais
                <fs_frete_fat>-st_mdfe = 'A determinar'.

              WHEN OTHERS.
*             Do nothing
            ENDCASE.

          ELSEIF el_nfdoc_st-cancel IS NOT INITIAL.
            <fs_frete_fat>-status = 'Cancelado'.
* Verifica o Modelo da Nota.
            CASE el_nfdoc_st-model.
              WHEN '57'. "Conhecimento de Transporte Eletrônico
                <fs_frete_fat>-st_dacte = 'Cancelado'.

              WHEN '58'. "Manifesto Eletrônico de Documentos Fiscais
                <fs_frete_fat>-st_mdfe = 'Cancelado'.

              WHEN OTHERS.
*             Do nothing
            ENDCASE.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

    CLEAR <fs_frete_fat>-status.
    <fs_frete_fat>-status = 'Pendente'.
    MODIFY tg_frete_fat FROM <fs_frete_fat> TRANSPORTING status WHERE docnum_serv IS INITIAL.

  ELSE.
* Não encontrados dados para esta seleção
    MESSAGE s114(pt) DISPLAY LIKE sy-abcde+4(1). "E
    RETURN.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_show_data_conj_veic
*&---------------------------------------------------------------------*
*& Exibe os dados do Conjunto Veícular
*&---------------------------------------------------------------------*
FORM zf_show_data_conj_veic.

  DATA: tl_function TYPE ui_functions.

  DATA: el_layout  TYPE lvc_s_layo,
        el_variant TYPE disvariant.

  IF gcl_custom_container2 IS INITIAL.
    CREATE OBJECT gcl_custom_container2
      EXPORTING
        container_name = gc_container2.

    CREATE OBJECT gcl_grid2
      EXPORTING
        i_parent = gcl_custom_container2.
* Criar o Catalogo de de campos do ALV Conjunto Veícular.
    PERFORM zf_filcatalog_alv_cnj_vic.
* Exclui botões da Barra de tarefa do ALV Frete.
    PERFORM zf_exclud_btn_tbar_alv TABLES tl_function.
* Carrega a estrutura de Layout.
    el_layout-zebra      = abap_on.
    el_layout-cwidth_opt = abap_on.
    el_variant-report    = sy-repid.
* Carrega as configurações e dados para o ALV Frete
    CALL METHOD gcl_grid2->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = el_layout
        i_save               = 'A'
        is_variant           = el_variant
      CHANGING
        it_fieldcatalog      = tg_fcat
        it_outtab            = tg_conj_veic.

  ELSE.
    CALL METHOD gcl_grid2->refresh_table_display.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_filcatalog_alv_cnj_vic
*&---------------------------------------------------------------------*
*& Criar o Catalogo de de campos do ALV Conjunto Veícular
*&---------------------------------------------------------------------*
FORM zf_filcatalog_alv_cnj_vic.

  DATA: vl_texto TYPE scrtext_l.

  DATA: tl_fcat TYPE slis_t_fieldcat_alv.

  CLEAR tg_fcat.

* Declara  o de Classes.
  DATA: lcl_table   TYPE REF TO cl_salv_table,
        lcl_columns TYPE REF TO cl_salv_columns_table,
        lcl_aggreg  TYPE REF TO cl_salv_aggregations.

** Buisca o catalogo de campos conforme estrutura definida.
*... Create Instance
  CALL METHOD cl_salv_table=>factory
    IMPORTING
      r_salv_table = lcl_table
    CHANGING
      t_table      = tg_conj_veic.

  lcl_columns = lcl_table->get_columns( ).
  lcl_aggreg  = lcl_table->get_aggregations( ).

  tg_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = lcl_columns
            r_aggregations = lcl_aggreg ).
  CLEAR: lcl_table, lcl_columns, lcl_aggreg.

  LOOP AT tg_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN 'NOME_VEIC'.
        vl_texto = 'Nome Veículo'.

      WHEN 'PC_VEICULO'.
        vl_texto = 'Placa'.

      WHEN 'TP_VEICULO'.
        vl_texto = 'TP'.

      WHEN 'PROPRIETARIO'.
        vl_texto = 'Prop.'.

      WHEN 'STCD1'.
        vl_texto = 'CNPJ'.

      WHEN 'STCD2'.
        vl_texto = 'CPF'.

      WHEN 'STCD3'.
        vl_texto = 'Inscr.'.

      WHEN 'GRUPO'.
        vl_texto = 'Grupo'.

      WHEN 'FROTA'.
        vl_texto = 'Frota'.

      WHEN 'BAHNS'.
        vl_texto = 'RNTRC'.

      WHEN OTHERS.
        CONTINUE.

    ENDCASE.

    <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = <fs_fcat>-reptext = vl_texto.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_chek_fields_scr_0200
*&---------------------------------------------------------------------*
*& Verifica campos da tela de Manutenção do Frete
*&---------------------------------------------------------------------*
FORM zf_chek_fields_scr_0200.

* Verifica se não foi acionado a Exibir Frete.
  IF     sy-ucomm EQ 'SHOW'   OR
         sy-ucomm EQ 'EDIT'   OR
     ( ( gv_acao  EQ 'SHOW'   OR
         gv_acao  EQ 'NEW'   OR
         gv_acao  EQ 'EDIT' ) AND
         sy-ucomm EQ 'ENTE' ).
    IF *zlest0255-id_neg IS INITIAL AND
       *zlest0255-vbeln  IS INITIAL.
      MESSAGE |Campos Id negociação e Nr. OV vazios. Informar um deles.| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.

    ENDIF.
* Configuração e validação do respectivo campo acionado.
    PERFORM zf_setting_verify_field USING *zsdt0001od-nr_ordem
                                          '*ZSDT0001OD-NR_ORDEM'
                                          '0200'.
* Verifica se ocorreu erro na validação dos dados da tela 200.
    IF NOT gv_erro IS INITIAL.
      CLEAR gv_erro.
      EXIT.

    ENDIF.
* Configuração e validação do respectivo campo acionado.
    PERFORM zf_setting_verify_field USING *zsdt0001od-nr_safra
                                          '*ZSDT0001OD-NR_SAFRA'
                                          '0200'.
* Verifica se ocorreu erro na validação dos dados da tela 200.
    IF NOT gv_erro IS INITIAL.
      CLEAR gv_erro.
      EXIT.

    ELSE.
* Carrega dados da tela de Criação/Manutenção Frete Faturado.
      PERFORM zf_carrega_dados_tela_200.

    ENDIF.

  ENDIF.
* Verifica se ocorreu erro na validação dos dados da tela 200.
  IF NOT gv_erro IS INITIAL.
    CLEAR gv_erro.
    EXIT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_carrega_dados_tela_200
*&---------------------------------------------------------------------*
*& tCarrega dados da tela de Criação/Manutenção Frete Faturado
*&---------------------------------------------------------------------*
FORM zf_carrega_dados_tela_200.

  IF NOT *zlest0255-id_neg IS INITIAL AND
     NOT *zlest0255-vbeln  IS INITIAL.
    SELECT SINGLE * FROM *zlest0255 WHERE id_neg EQ *zlest0255-id_neg
                                      AND vbeln  EQ *zlest0255-vbeln.

  ELSEIF NOT *zlest0255-id_neg IS INITIAL AND
             *zlest0255-vbeln  IS INITIAL.
    SELECT SINGLE * FROM *zlest0255 WHERE id_neg EQ *zlest0255-id_neg.

  ELSEIF     *zlest0255-id_neg IS INITIAL AND
         NOT *zlest0255-vbeln  IS INITIAL.
    SELECT SINGLE * FROM *zlest0255 WHERE vbeln EQ *zlest0255-vbeln.

  ENDIF.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE |Serviço de Frete não encontado!| TYPE 'S' DISPLAY LIKE 'E'.
    gv_erro = abap_on.

  ENDIF.

  SELECT SINGLE * FROM *zsdt0001od WHERE nr_ordem EQ *zsdt0001od-nr_ordem
                              AND nr_safra EQ *zsdt0001od-nr_safra.

  IF sy-subrc IS INITIAL.
    SELECT SINGLE viagem_id FROM zlest0185 INTO vg_viagem_id WHERE id_ordem EQ *zsdt0001od-id_ordem.

    SELECT pc_veiculo tp_veiculo proprietario name1 stcd1
           stcd2 stcd3 grupo frota bahns
      FROM zlest0002 AS a
      INNER JOIN lfa1 AS b
       ON a~proprietario EQ b~lifnr AND
          b~land1 EQ 'BR'
      INTO CORRESPONDING FIELDS OF TABLE tg_conj_veic
    WHERE pc_veiculo EQ *zsdt0001od-ds_placa_trator
       OR pc_veiculo EQ *zsdt0001od-ds_placa_reboq_1
       OR pc_veiculo EQ *zsdt0001od-ds_placa_reboq_2
       OR pc_veiculo EQ *zsdt0001od-ds_placa_reboq_3.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_conj_veic ASSIGNING FIELD-SYMBOL(<fs_conj_veic>) WITH KEY pc_veiculo = *zsdt0001od-ds_placa_trator.
      IF sy-subrc IS INITIAL.
        <fs_conj_veic>-nome_veic = 'Cavalo'.

      ENDIF.

      READ TABLE tg_conj_veic ASSIGNING <fs_conj_veic> WITH KEY pc_veiculo = *zsdt0001od-ds_placa_reboq_1.
      IF sy-subrc IS INITIAL.
        <fs_conj_veic>-nome_veic = 'Carreta 1'.

      ENDIF.

      READ TABLE tg_conj_veic ASSIGNING <fs_conj_veic> WITH KEY pc_veiculo = *zsdt0001od-ds_placa_reboq_2.
      IF sy-subrc IS INITIAL.
        <fs_conj_veic>-nome_veic = 'Carreta 2'.

      ENDIF.

      READ TABLE tg_conj_veic ASSIGNING <fs_conj_veic> WITH KEY pc_veiculo = *zsdt0001od-ds_placa_reboq_3.
      IF sy-subrc IS INITIAL.
        <fs_conj_veic>-nome_veic = 'Carreta 3'.

      ENDIF.

    ENDIF.

    SELECT SINGLE * FROM *lfa1 WHERE lifnr EQ *zsdt0001od-id_motorista.

  ELSE.
    MESSAGE |Ordens de Carregamento não encontado!| TYPE 'S' DISPLAY LIKE 'E'.
    gv_erro = abap_on.

  ENDIF.

ENDFORM.
