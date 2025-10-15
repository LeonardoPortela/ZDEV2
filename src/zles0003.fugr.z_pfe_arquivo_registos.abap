FUNCTION z_pfe_arquivo_registos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_ARQUIVO STRUCTURE  ZPFE_ARQUIVO
*"      T_REG_CABECALHO STRUCTURE  ZPFE_LOTE
*"      T_REG_ITENS STRUCTURE  ZPFE_LOTE_ITEM
*"  CHANGING
*"     VALUE(VG_LOTE) TYPE  I
*"  EXCEPTIONS
*"      NAO_ADMINISTRADORA
*"      NAO_LOCAL_NEGOCIO
*"      NAO_CHAVE_HISTORICO
*"      NAO_CHAVE_HIST_INFO
*"      NAO_CIOT
*"      NAO_CIOT_INFO
*"----------------------------------------------------------------------

  TYPES BEGIN OF ty_split.
  TYPES: valor TYPE char50.
  TYPES END OF ty_split.

  DATA: wa_arquivo          TYPE zpfe_arquivo,
        wa_zpfe_lote        TYPE zpfe_lote,
        wa_zpfe_lote_i      TYPE zpfe_lote_item,
        wa_lfa1             TYPE lfa1,
        wa_j_1bbranch       TYPE j_1bbranch,
        vg_lote_i           TYPE i,
        st_lote             TYPE c LENGTH 10,
        st_lote_i           TYPE c LENGTH 10,
        wa_zcte_ciot        TYPE zcte_ciot,
        wa_zcte_identifica  TYPE zcte_identifica,
        wa_zpfe_lote_his_de TYPE zpfe_lote_his_de,
        t_split             TYPE TABLE OF ty_split WITH HEADER LINE,
        wl_0025             TYPE zlest0025,
        tipo_linha          TYPE i,
        it_lote             TYPE TABLE OF zpfe_lote,
        it_lote_itens       TYPE TABLE OF zpfe_lote_item.

  st_lote_i = 0.

  "  WRITE VG_LOTE TO ST_LOTE.

  LOOP AT t_arquivo INTO wa_arquivo.

    vg_lote = vg_lote + 1.

    SPLIT wa_arquivo-linha AT '|' INTO TABLE t_split.
    READ TABLE t_split INDEX 1.
    MOVE t_split-valor TO tipo_linha.

    CASE tipo_linha.
      WHEN 1.

        ADD 1 TO st_lote.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = st_lote
          IMPORTING
            output = wa_zpfe_lote-nm_lote.

        "Cabeçalho do Arquivo
        wa_zpfe_lote-dt_leitura = sy-datum.
        wa_zpfe_lote-hr_leitura = sy-uzeit.

        LOOP AT t_split.
          CASE sy-tabix.
            WHEN 1.
              "1)  Tipo do Registro: 01 para cabeçalho;
            WHEN 2.
              "2)	CNPJ Administradora de pagamentos de frete terceiro;
              SELECT SINGLE * INTO wa_lfa1
                FROM lfa1
               WHERE stcd1 EQ t_split-valor.

              IF sy-subrc IS INITIAL.
                wa_zpfe_lote-cd_adiministra = wa_lfa1-lifnr.
              ELSE.
                MESSAGE e002 WITH t_split-valor RAISING nao_administradora.
              ENDIF.
            WHEN 3.
              "3)	CNPJ Cliente: Transportadora do Grupo André Maggi;
              SELECT SINGLE * INTO wa_j_1bbranch
                FROM j_1bbranch
               WHERE stcd1 EQ t_split-valor.

              IF sy-subrc IS INITIAL.
                wa_zpfe_lote-bukrs  = wa_j_1bbranch-bukrs.
                wa_zpfe_lote-branch = wa_j_1bbranch-branch.
              ELSE.
                MESSAGE e003 WITH t_split-valor RAISING nao_local_negocio.
              ENDIF.
            WHEN 4.
              "4)	Data de Posição: Formato ddmmyyyy;
              CONCATENATE t_split-valor+4(4) t_split-valor+2(2) t_split-valor(2) INTO t_split-valor.
              MOVE t_split-valor TO wa_zpfe_lote-dt_posicao.
            WHEN 5.
              "4)	Data de Vencimento: Formato ddmmyyyy;
              CONCATENATE t_split-valor+4(4) t_split-valor+2(2) t_split-valor(2) INTO t_split-valor.
              MOVE t_split-valor TO wa_zpfe_lote-dt_vencimento.
            WHEN 6.
              "5)	Valor Total Arquivo: Sempre com duas casas decimais sem a vírgula;
              "moeda
              IF NOT t_split-valor IS INITIAL.
                PERFORM coloca_virgula USING t_split-valor.
              ELSE.
                t_split-valor  = '0,00'.
              ENDIF.

              CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
                EXPORTING
                  i_char         = t_split-valor
                IMPORTING
                  e_packed       = wa_zpfe_lote-vl_total_lote
                EXCEPTIONS
                  invalid_number = 1
                  OTHERS         = 2.

              wa_zpfe_lote-moeda = 'BRL'.
            WHEN 7.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = t_split-valor
                IMPORTING
                  output = wa_zpfe_lote-nr_lote_adm.

          ENDCASE.
        ENDLOOP.
        APPEND wa_zpfe_lote TO t_reg_cabecalho.
      WHEN 2.

        vg_lote_i = vg_lote_i + 1.

        WRITE vg_lote_i TO st_lote_i.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = st_lote_i
          IMPORTING
            output = wa_zpfe_lote_i-nm_lote_item.

        wa_zpfe_lote_i-nm_lote = wa_zpfe_lote-nm_lote.
        wa_zpfe_lote_i-nr_lote_adm = wa_zpfe_lote-nr_lote_adm.

        LOOP AT t_split.
          CASE sy-tabix.
            WHEN 1.
              "Tipo do Registro: 02 para detalhe;
            WHEN 2.
              "Código CIOT;
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = t_split-valor
                IMPORTING
                  output = wa_zpfe_lote_i-nucontrato.
            WHEN 3.
              "Histórico;

              CHECK t_split-valor IS NOT INITIAL.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = t_split-valor
                IMPORTING
                  output = wa_zpfe_lote_his_de-chvid_adm.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_zpfe_lote_his_de-chvid_adm
                IMPORTING
                  output = wa_zpfe_lote_his_de-chvid_adm.

              CLEAR: wa_zpfe_lote_i-chvid.
              SELECT SINGLE * INTO wa_zpfe_lote_his_de
                FROM zpfe_lote_his_de
               WHERE chvid_adm EQ wa_zpfe_lote_his_de-chvid_adm.

              IF sy-subrc IS INITIAL.
                wa_zpfe_lote_i-chvid = wa_zpfe_lote_his_de-chvid_emp.
              ENDIF.

            WHEN 4.
              "Data da Transação: Formato ddmmyyyy;
              CONCATENATE t_split-valor+4(4) t_split-valor+2(2) t_split-valor(2) INTO t_split-valor.
              MOVE t_split-valor TO wa_zpfe_lote_i-dt_transacao.
            WHEN 7.
              "Valor da Transação: Sempre com duas casas decimais sem a vírgula;
              IF NOT t_split-valor IS INITIAL.
                PERFORM coloca_virgula USING t_split-valor.
              ELSE.
                t_split-valor  = '0,00'.
              ENDIF.

              CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
                EXPORTING
                  i_char         = t_split-valor
                IMPORTING
                  e_packed       = wa_zpfe_lote_i-vl_transacao
                EXCEPTIONS
                  invalid_number = 1
                  OTHERS         = 2.
            WHEN 10.
              "Peso da Chegada: Sempre com duas casas decimais sem a vírgula;
              CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
                EXPORTING
                  i_char         = t_split-valor
                IMPORTING
                  e_packed       = wa_zpfe_lote_i-peso_importado
                EXCEPTIONS
                  invalid_number = 1
                  OTHERS         = 2.
            WHEN 11.
              "Data da Chegada;
              IF t_split-valor IS INITIAL.
                CONTINUE.
              ENDIF.
              CONCATENATE t_split-valor+4(4) t_split-valor+2(2) t_split-valor(2) INTO t_split-valor.
              MOVE t_split-valor TO wa_zpfe_lote_i-dt_chegada.
            WHEN 12.
              IF t_split-valor EQ 'A'.
                wa_zpfe_lote_i-vl_ajus_adm = wa_zpfe_lote_i-vl_transacao.
                SELECT SINGLE *
                  FROM zlest0025
                  INTO wl_0025
                   WHERE chvid EQ wa_zpfe_lote_i-chvid.
                IF wl_0025-naturezachvid EQ 'S'.
                  MULTIPLY wa_zpfe_lote_i-vl_ajus_adm BY -1.
                ENDIF.

              ENDIF.
            WHEN 13.
              IF t_split-valor IS INITIAL.
                CONTINUE.
              ENDIF.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = t_split-valor
                IMPORTING
                  output = wa_zpfe_lote_i-lote_origem.

          ENDCASE.
        ENDLOOP.

        APPEND wa_zpfe_lote_i TO t_reg_itens.
        CLEAR: wa_zpfe_lote_i-lote_origem,  wa_zpfe_lote_i-vl_ajus_adm.
    ENDCASE.

  ENDLOOP.

  IF t_reg_itens[] IS NOT INITIAL.
    SELECT *
      FROM zcte_ciot
      INTO TABLE @DATA(it_zcte_ciot)
       FOR ALL ENTRIES IN @t_reg_itens
     WHERE nucontrato EQ @t_reg_itens-nucontrato.
  ENDIF.

  SORT it_zcte_ciot BY nucontrato.

  """"""""""""" Dividir Lote
  LOOP AT t_reg_itens ASSIGNING FIELD-SYMBOL(<fs_item>).
    READ TABLE it_zcte_ciot INTO wa_zcte_ciot WITH KEY nucontrato = <fs_item>-nucontrato.
    IF sy-subrc IS INITIAL.
      CASE wa_zcte_ciot-tp_plano_administradora.
        WHEN zcl_ciot=>st_tp_plano_pre_pago.
          <fs_item>-tp_plano_administradora = zcl_ciot=>st_tp_plano_pre_pago.
        WHEN zcl_ciot=>st_tp_plano_pos_pago OR space.
          <fs_item>-tp_plano_administradora = zcl_ciot=>st_tp_plano_pos_pago.
      ENDCASE.
    ENDIF.
  ENDLOOP.

  CLEAR: it_lote, it_lote_itens, it_lote[], it_lote_itens[].

  LOOP AT t_reg_cabecalho ASSIGNING FIELD-SYMBOL(<fs_lote>).

    READ TABLE t_reg_itens WITH KEY nm_lote = <fs_lote>-nm_lote tp_plano_administradora = zcl_ciot=>st_tp_plano_pre_pago.
    DATA(vg_pre_pago) = sy-subrc.
    READ TABLE t_reg_itens WITH KEY nm_lote = <fs_lote>-nm_lote tp_plano_administradora = zcl_ciot=>st_tp_plano_pos_pago.
    DATA(vg_pos_pago) = sy-subrc.

    "Dividir o Lote
    IF vg_pre_pago IS INITIAL AND vg_pos_pago IS INITIAL.

      "Determina Primeiro Lote Como Pós Pago
      <fs_lote>-tp_plano_administradora = zcl_ciot=>st_tp_plano_pos_pago.
      "Copia
      MOVE-CORRESPONDING <fs_lote> TO wa_zpfe_lote.

      "Gera Novo Número Lote
      ADD 1 TO st_lote.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = st_lote
        IMPORTING
          output = wa_zpfe_lote-nm_lote.

      "Determina Segundo Lote como Pré Pago
      wa_zpfe_lote-tp_plano_administradora = zcl_ciot=>st_tp_plano_pre_pago.
      APPEND wa_zpfe_lote TO it_lote.

      "Copia Itens Pré Pagos, para lote Pré Pago
      LOOP AT t_reg_itens INTO wa_zpfe_lote_i
        WHERE nm_lote                 EQ <fs_lote>-nm_lote
          AND tp_plano_administradora EQ wa_zpfe_lote-tp_plano_administradora.
        wa_zpfe_lote_i-nm_lote = wa_zpfe_lote-nm_lote.
        APPEND wa_zpfe_lote_i TO it_lote_itens.
      ENDLOOP.

      "Apaga Registro Pré pagos do lote Pós Pago
      DELETE t_reg_itens WHERE nm_lote EQ <fs_lote>-nm_lote AND tp_plano_administradora EQ wa_zpfe_lote-tp_plano_administradora.

    ELSEIF vg_pre_pago IS INITIAL.
      <fs_lote>-tp_plano_administradora = zcl_ciot=>st_tp_plano_pre_pago.
    ELSEIF vg_pos_pago IS INITIAL.
      <fs_lote>-tp_plano_administradora = zcl_ciot=>st_tp_plano_pos_pago.
    ENDIF.

  ENDLOOP.

  LOOP AT it_lote INTO wa_zpfe_lote.
    APPEND wa_zpfe_lote TO t_reg_cabecalho.
  ENDLOOP.

  LOOP AT it_lote_itens INTO wa_zpfe_lote_i.
    APPEND wa_zpfe_lote_i TO t_reg_itens.
  ENDLOOP.

ENDFUNCTION.
