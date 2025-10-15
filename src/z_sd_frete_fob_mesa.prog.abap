*&---------------------------------------------------------------------*
*& Report  Z_SD_FRETE_FOB_MESA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


REPORT  z_sd_frete_fob_mesa.

TYPES: BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         vkorg TYPE t001w-vkorg,
       END OF ty_t001w.

DATA : t_zsdt0067    TYPE TABLE OF zsdt0067,
       t_zsdt0068    TYPE TABLE OF zsdt0068,

       t_zsdt0051    TYPE TABLE OF zsdt0051,
       t_zsdt0052    TYPE TABLE OF zsdt0052,
       t_zsdt0059    TYPE TABLE OF zsdt0059,
       t_zsdt0066    TYPE TABLE OF zsdt0066,
       t_zmmt0017    TYPE TABLE OF zmmt0017,

       tl_nro_sol_ov TYPE TABLE OF zsds007 WITH HEADER LINE,
       t_retorno     TYPE TABLE OF bapiret2 WITH HEADER LINE,
       t_saida_exec  TYPE TABLE OF zsds010  WITH HEADER LINE,

       t_outreturn   TYPE TABLE OF zfie_ret_document.

DATA : wa_zsdt0067         TYPE zsdt0067,
       wa_zsdt0068         TYPE zsdt0068,
       wa_zsdt0051         TYPE zsdt0051,
       wa_zsdt0052         TYPE zsdt0052,
       wa_zsdt0059         TYPE zsdt0059,
       wa_zsdt0066         TYPE zsdt0066,
       wa_zmmt0017         TYPE zmmt0017,
       wa_tbtco            TYPE tbtco,
       wa_zparam_cont_fret TYPE zparam_cont_fret,

       wa_outreturn        TYPE zfie_ret_document.

******23/03/2018 Validação da transação ZSDT0132

DATA: t_zsdt0158    TYPE TABLE OF zsdt0158,
      t_zsdt0158_qt TYPE TABLE OF zsdt0158_qt,
      t_t001w       TYPE TABLE OF ty_t001w,

      wa_zsdt0158   TYPE zsdt0158,
      wa_t001w      TYPE ty_t001w.

DATA: vtp_produto TYPE zmmt0017-tp_produto.
DATA: vlgort      TYPE zmmt0017-lgort.
DATA: wa_0017     TYPE zmmt0017.
DATA: vg_job     TYPE i,
      vg_job_158 TYPE i.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM: z_seleciona_dados,
           z_proc_dados.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM z_seleciona_dados .

  " ramon -->
  SELECT SINGLE low FROM tvarvc INTO @DATA(lv_stop) WHERE name = 'Z_SD_FRETE_FOB_MESA_STOP'.

  CHECK lv_stop IS INITIAL.
  " ramon --<

  SELECT SINGLE COUNT(*) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'CONTRATO_FRETE_SIGAM'
     AND status EQ 'R'.

  IF ( vg_job EQ 1 ).

    SELECT *
      INTO TABLE t_zsdt0067
      FROM zsdt0067
     WHERE rg_atualizado = '0'.

    SELECT *
      FROM zsdt0068
      INTO TABLE t_zsdt0068
       FOR ALL ENTRIES IN t_zsdt0067
     WHERE ch_referencia = t_zsdt0067-ch_referencia.

  ENDIF.
*

  SELECT SINGLE * FROM tbtco
    INTO wa_tbtco
    WHERE jobname EQ 'PROCESSA_SOL_OV_PT'
    AND   status  EQ  'R'.

  IF ( wa_tbtco IS NOT INITIAL ).

    SELECT * INTO TABLE t_zsdt0158
      FROM zsdt0158
      WHERE rg_atualizado   = '0'
      ORDER BY nro_sol_ov ASCENDING.

    IF ( t_zsdt0158[] IS NOT INITIAL ).
      SELECT werks vkorg
        FROM t001w
        INTO TABLE t_t001w
        FOR ALL ENTRIES IN t_zsdt0158
        WHERE werks EQ t_zsdt0158-filial.

      SELECT bukrs, branch FROM j_1bbranch
        INTO TABLE @DATA(t_j_1bbranch)
        FOR ALL ENTRIES IN @t_zsdt0158
        WHERE branch = @t_zsdt0158-filial.

      LOOP AT t_j_1bbranch[] INTO DATA(w_j_1bbranch).
        APPEND INITIAL LINE TO t_t001w[] ASSIGNING FIELD-SYMBOL(<w_t001w>).
        <w_t001w>-vkorg = w_j_1bbranch-bukrs.
        <w_t001w>-werks = w_j_1bbranch-branch.
        CLEAR: w_j_1bbranch.
      ENDLOOP.

      SORT t_t001w[] BY werks ASCENDING.
      "DELETE ADJACENT DUPLICATES FROM T_T001W[] COMPARING WERKS.
      DELETE t_t001w[] WHERE vkorg IS INITIAL.

      PERFORM proc_dados_0158.
    ENDIF.

    SELECT *
      FROM zsdt0158_qt
      INTO TABLE t_zsdt0158_qt
      WHERE rg_atualizado = '0'.

    IF ( t_zsdt0158_qt[] IS NOT INITIAL ) .

      PERFORM altera_quant_ov.

    ENDIF.

  ENDIF.

ENDFORM.                    " Z_SELECIONA_DADOS


*&---------------------------------------------------------------------*
*&      Form  Z_PROC_DADOS
*&---------------------------------------------------------------------*
FORM z_proc_dados .
  DATA: vl_cliente  TYPE kna1-kunnr,
        vl_ponto_c  TYPE kna1-kunnr,
        vl_terminal TYPE lfa1-lifnr,
        vl_lentrega TYPE kna1-kunnr.

  DATA vmatnr18 TYPE matnr18.

  SORT:t_zsdt0067 BY ch_referencia,
       t_zsdt0068 BY ch_referencia tp_funcao_parc.

  SELECT SINGLE * INTO wa_zparam_cont_fret FROM zparam_cont_fret.

  LOOP AT t_zsdt0067 INTO wa_zsdt0067.

    IF wa_zsdt0067-tp_doc_sap = 'O'."ORDEM DE VENDA

      IF wa_zsdt0067-status = 'L'.

        READ TABLE t_zsdt0068 INTO wa_zsdt0068 WITH KEY ch_referencia  = wa_zsdt0067-ch_referencia
                                                        tp_funcao_parc = 'Z1'
                                                        BINARY SEARCH.
        vl_terminal = wa_zsdt0068-id_parceiro.

        READ TABLE t_zsdt0068 INTO wa_zsdt0068 WITH KEY ch_referencia  = wa_zsdt0067-ch_referencia
                                                        tp_funcao_parc = 'PR'
                                                        BINARY SEARCH.

        vl_cliente  = wa_zsdt0068-id_parceiro.

        READ TABLE t_zsdt0068 INTO wa_zsdt0068 WITH KEY ch_referencia  = wa_zsdt0067-ch_referencia
                                                        tp_funcao_parc = 'PC'
                                                        BINARY SEARCH.

        vl_ponto_c  = wa_zsdt0068-id_parceiro.

        READ TABLE t_zsdt0068 INTO wa_zsdt0068 WITH KEY ch_referencia  = wa_zsdt0067-ch_referencia
                                                        tp_funcao_parc = 'LR'
                                                        BINARY SEARCH.
        vl_lentrega = wa_zsdt0068-id_parceiro.


        READ TABLE t_zsdt0158 INTO wa_zsdt0158 INDEX 1.

        READ TABLE t_zmmt0017 INTO wa_zmmt0017 WITH KEY centro_fixo =  wa_zsdt0158-filial
                                                         matnr      =  wa_zsdt0158-id_produto.



        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZNR_SOL_OV'
          IMPORTING
            number      = wa_zsdt0051-nro_sol_ov.

        PERFORM zprocessa_reg_lido USING wa_zsdt0067-ch_referencia
                                         wa_zsdt0051-nro_sol_ov
                                         wa_zsdt0067-status  .

        wa_zsdt0051-waerk        = wa_zsdt0067-moeda.
        wa_zsdt0051-vkorg        = wa_zsdt0067-cd_empresa.
        wa_zsdt0051-matnr        = wa_zsdt0067-cd_material.
        wa_zsdt0051-kunnr        = vl_cliente.
        wa_zsdt0051-usnam        = sy-uname.
        wa_zsdt0051-data_atual   = sy-datum.
        wa_zsdt0051-data_venda   = sy-datum.
        wa_zsdt0051-hora_atual   = sy-uzeit.
        wa_zsdt0051-tp_venda     = wa_zparam_cont_fret-tp_venda."'14'.
        wa_zsdt0051-dtde_logist  = ''.
        wa_zsdt0051-dtate_logist = ''.
        wa_zsdt0051-vbeln        = ''.
        wa_zsdt0051-status       = wa_zparam_cont_fret-status."'L'.
        wa_zsdt0051-auart        = wa_zparam_cont_fret-auart."'ZRFL'.
        wa_zsdt0051-vtweg        = wa_zparam_cont_fret-vtweg."'10'.
        wa_zsdt0051-spart        = wa_zparam_cont_fret-spart."'01'.
        wa_zsdt0051-vkgrp        = ''.
        wa_zsdt0051-vkbur        = ''.
        wa_zsdt0051-correto      = ''.
        wa_zsdt0051-bstkd        = wa_zsdt0067-ch_referencia.
        wa_zsdt0051-inco1        = wa_zsdt0067-tp_frete.
        wa_zsdt0051-inco2        = wa_zsdt0067-tp_frete.
        wa_zsdt0051-vkaus        = wa_zparam_cont_fret-vkaus."'I'.
        wa_zsdt0051-observacao   = ''.
        wa_zsdt0051-coment_logistica = 	''.
        wa_zsdt0051-param_espec  = wa_zparam_cont_fret-param_espec. "'F'.
        wa_zsdt0051-observacao   = wa_zsdt0067-observacao.

        INSERT INTO zsdt0051 VALUES wa_zsdt0051.

        "Tabela : zsdt0052
        wa_zsdt0052-nro_sol_ov  = wa_zsdt0051-nro_sol_ov.
        wa_zsdt0052-pgto_ant    = ''.
        wa_zsdt0052-zlsch       = wa_zparam_cont_fret-zlsch."'P'.
        wa_zsdt0052-zterm       = wa_zparam_cont_fret-zterm."'0001'.
        wa_zsdt0052-qte_venc    = wa_zparam_cont_fret-qte_venc."'0'.
        wa_zsdt0052-valdt       = ''.
        wa_zsdt0052-hbkid       = ''.
        wa_zsdt0052-usnam       = sy-uname.
        wa_zsdt0052-data_atual  = sy-datum.
        wa_zsdt0052-hora_atual  = sy-uzeit.

        INSERT INTO zsdt0052 VALUES wa_zsdt0052.

        "Tabela : zsdt0059
        wa_zsdt0059-nro_sol_ov  = wa_zsdt0051-nro_sol_ov.
        wa_zsdt0059-nivel       = wa_zparam_cont_fret-nivel."'01'.
        wa_zsdt0059-cod_fp      = wa_zparam_cont_fret-cod_fp."'04'.
        wa_zsdt0059-tipo_calc   = wa_zparam_cont_fret-tipo_calc."'V'.
        wa_zsdt0059-preco       = wa_zparam_cont_fret-preco."'1'.
        wa_zsdt0059-ocbot       = ''.
        wa_zsdt0059-cbot        = ''.
        wa_zsdt0059-c_decimais  = wa_zparam_cont_fret-c_decimais."'2'.
        wa_zsdt0059-bezei       = wa_zsdt0067-preco."preço venda bruto
        wa_zsdt0059-formula     = wa_zsdt0067-preco."preço
        wa_zsdt0059-formula2    = wa_zsdt0067-preco.
        wa_zsdt0059-waers       = wa_zsdt0067-moeda.
        wa_zsdt0059-usnam       = sy-uname.
        wa_zsdt0059-data_atual  = sy-datum.
        wa_zsdt0059-hora_atual  = sy-uzeit.

        INSERT INTO zsdt0059 VALUES wa_zsdt0059.

        "Tabela : zsdt0066
        wa_zsdt0066-nro_sol_ov  = wa_zsdt0051-nro_sol_ov.
        wa_zsdt0066-instrucao   = ''.
        wa_zsdt0066-posnr       = '10'.
        IF wa_zmmt0017-lgort IS NOT INITIAL.
          wa_zsdt0066-lgort       = wa_zmmt0017-lgort."'ARMZ'.
        ELSE.
          IF vg_job EQ '0'.
            MESSAGE 'Não Existe deposito configurado para o centro ' && wa_zsdt0158-filial && 'na transação ZMM0017. Procurar depto de estoque' TYPE 'I'.
            EXIT.
          ENDIF.
        ENDIF.
        wa_zsdt0066-zieme       = wa_zparam_cont_fret-zieme."'KG'.
        wa_zsdt0066-volum       = ''.
        wa_zsdt0066-pmein       = wa_zparam_cont_fret-pmein."'TO'.
        wa_zsdt0066-vbeln       = ''.
        wa_zsdt0066-inco1       = wa_zsdt0067-tp_frete.
        wa_zsdt0066-inco2       = wa_zsdt0067-tp_frete.
        wa_zsdt0066-ponto_c     = vl_ponto_c.
        wa_zsdt0066-status      = wa_zparam_cont_fret-status."'L'.
        wa_zsdt0066-matnr       = wa_zsdt0067-cd_material.
        wa_zsdt0066-werks       = wa_zsdt0067-cd_centro.
        wa_zsdt0066-charg       = wa_zsdt0067-cd_safra.
        wa_zsdt0066-zmeng       = wa_zsdt0067-qt_produto.
        wa_zsdt0066-dmbtr       = wa_zsdt0067-preco / wa_zparam_cont_fret-multiplicador ."preço
        wa_zsdt0066-vlrtot      = ( wa_zsdt0066-zmeng * wa_zsdt0066-dmbtr ) . "/ WA_ZPARAM_CONT_FRET-MULTIPLICADOR .
        wa_zsdt0066-lentrega    = vl_lentrega.
        wa_zsdt0066-kunnr       = wa_zsdt0067-cd_centro.
        wa_zsdt0066-waerk       = wa_zsdt0067-moeda.
        wa_zsdt0066-terminal    = vl_terminal.
        wa_zsdt0066-usnam       = sy-uname.
        wa_zsdt0066-data_atual  = sy-datum.
        wa_zsdt0066-hora_atual  = sy-uzeit.
        wa_zsdt0066-classificacao = wa_zsdt0067-classificacao.
        wa_zsdt0066-kunnr       = |{ wa_zsdt0066-kunnr ALPHA = IN }|.
*        wa_zsdt0066-matnr       = |{ wa_zsdt0066-matnr ALPHA = IN }|.

        vmatnr18 = |{ wa_zsdt0066-matnr ALPHA = IN }|.
        wa_zsdt0066-matnr  = vmatnr18.

        wa_zsdt0066-kvgr4         = wa_zsdt0158-kvgr4.
        wa_zsdt0066-kvgr5         = wa_zsdt0158-kvgr5.
        wa_zsdt0066-ck_troca_nota = wa_zsdt0158-ck_troca_nota.

        INSERT INTO zsdt0066 VALUES wa_zsdt0066.

        " ramon teste -->
        MESSAGE s016(ds) WITH 'Solicitação' wa_zsdt0158-nro_sol_ov 'criada via insert'.
        " ramon teste --<

      ELSEIF wa_zsdt0067-status = 'B'."Bloquear OV

        CLEAR: tl_nro_sol_ov.

        PERFORM zprocessa_reg_lido USING wa_zsdt0067-ch_referencia
                                         wa_zsdt0067-nro_sol_ov
                                         wa_zsdt0067-status  .

        MOVE: wa_zsdt0067-nro_sol_ov TO tl_nro_sol_ov-nro_sol_ov,
               '10'                  TO tl_nro_sol_ov-posnr.
        APPEND tl_nro_sol_ov.



        CALL FUNCTION 'ZSDMF005_MODIF_STATUS_ITEM_SOL'
          EXPORTING
            i_status               = 'B'
            i_tipo                 = '2'
            i_tipo_sol             = 'FL'
          TABLES
            ti_nro_sol_ov          = tl_nro_sol_ov
            te_return              = t_retorno
            te_saida_exec          = t_saida_exec
          EXCEPTIONS
            ov_ja_criada           = 1
            solicitacao_nao_existe = 2
            OTHERS                 = 3.
*
        UPDATE zsdt0051 SET status = 'D' WHERE nro_sol_ov = wa_zsdt0067-nro_sol_ov.
        LOOP AT t_retorno.

          PERFORM: mensagem USING wa_zsdt0067-ch_referencia
                                  '33'
                                  ''
                                  'S'"T_RETORNO-TYPE
                                  t_saida_exec-msg"T_RETORNO-MESSAGE
                                  ''
                                  ''
                                  ''
                                  ''
                                  'BL'.

        ENDLOOP.

        IF NOT t_outreturn[] IS INITIAL.

          SORT t_outreturn BY obj_key interface.

*--> 26.09.2023 22:50:06 - Migração S4 – ML - Início
*          CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*            DESTINATION 'XI_SIGAM_RETURN'
*            TABLES
*              outreturn = t_outreturn[].

          DATA: lv_rfc TYPE rfcdest.

          CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

          CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
            EXPORTING
              i_fm          = c_fm
            IMPORTING
              e_rfc         = lv_rfc
            EXCEPTIONS
              no_rfc        = 1
              no_rfc_config = 2
              OTHERS        = 3.

          IF sy-subrc EQ 0.
            CALL FUNCTION c_fm IN BACKGROUND TASK
              DESTINATION lv_rfc
              AS SEPARATE UNIT
              TABLES
                outreturn = t_outreturn[].
          ELSE.
            CALL FUNCTION c_fm IN BACKGROUND TASK
              TABLES
                outreturn = t_outreturn[].
          ENDIF.
*<-- 26.09.2023 22:50:06 - Migração S4 – ML – Fim

          COMMIT WORK.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    " Z_PROC_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZPROCESSA_REG_LIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zprocessa_reg_lido USING vl_ch_referencia
                              vl_nro_sol_ov
                              vl_status .
  DATA: vl_rg_atualizado TYPE zsdt0067-rg_atualizado.

  IF vl_status = 'L'.
    vl_rg_atualizado = '1'.
  ELSEIF vl_status = 'B'.
    vl_rg_atualizado = '2'.
  ENDIF.


  UPDATE zsdt0067 SET rg_atualizado = vl_rg_atualizado
                      nro_sol_ov = vl_nro_sol_ov
   WHERE ch_referencia = vl_ch_referencia.

*  LOOP AT T_ZSDT0067 INTO WA_ZSDT0067.
*
*    IF WA_ZSDT0067-STATUS = 'L'.
*      VL_RG_ATUALIZADO = '1'.
*    ELSEIF WA_ZSDT0067-STATUS = 'B'.
*      VL_RG_ATUALIZADO = '2'.
*    ENDIF.
*
*    UPDATE ZSDT0067 SET RG_ATUALIZADO = VL_RG_ATUALIZADO WHERE CH_REFERENCIA = WA_ZSDT0067-CH_REFERENCIA.
*
*  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    " ZPROCESSA_REG_LIDO

*&---------------------------------------------------------------------*
*&      Form  MENSAGEM
*&---------------------------------------------------------------------*
FORM mensagem  USING p_obj_key
                     p_interface
                     p_vbeln
                     p_type
                     p_message
                     p_msg_v1
                     p_msg_v2
                     p_msg_v3
                     p_msg_v4
                     p_id.

  CLEAR: wa_outreturn.

  wa_outreturn-obj_key        = p_obj_key.
  wa_outreturn-interface      = p_interface.
  wa_outreturn-dt_atualizacao = sy-datum.
  wa_outreturn-hr_atualizacao = sy-uzeit.
  wa_outreturn-type           = p_type.
  wa_outreturn-id             = p_id.
  wa_outreturn-num            = '899'.
  wa_outreturn-message        = p_message.
  wa_outreturn-message_v1     = p_msg_v1.

  APPEND wa_outreturn TO t_outreturn.

  CLEAR wa_outreturn.

ENDFORM.                    " MENSAGEM


*&---------------------------------------------------------------------*
*&      Form  PROC_DADOS_0158
*&---------------------------------------------------------------------*
FORM proc_dados_0158.

  LOOP AT t_zsdt0158 INTO wa_zsdt0158.

    IF ( wa_zsdt0158-status EQ 'L' ) AND ( wa_zsdt0158-tp_solicitacao EQ 'O' ).

      DATA(vl_cliente)  = wa_zsdt0158-filial.
      DATA(vl_forn)     = wa_zsdt0158-filial.

      SELECT SINGLE *
        FROM j_1bbranch INTO @DATA(wa_j_1bbranch)
        WHERE branch EQ @wa_zsdt0158-filial.

      "US 152850 11/10/2024 WPP ---->>>
      DATA(wa_zparam_cont_fret) = zcl_util_sd=>get_parametros_solicitacao_ov( i_zsdt0158 = wa_zsdt0158 ).

*      SELECT SINGLE *
*      FROM zparam_cont_fret
*      INTO wa_zparam_cont_fret
*      WHERE werks       EQ wa_zsdt0158-filial
*        AND bukrs       EQ wa_j_1bbranch-bukrs
*        AND tp_producao EQ wa_zsdt0158-tp_producao
*        AND dco         EQ wa_zsdt0158-dco
*        " 05.07.2022 - RAMON - 76636 ->
*        AND industrializacao EQ wa_zsdt0158-industrializacao.
*      " 05.07.2022 - RAMON - 76636 -<
*
*      IF ( wa_zparam_cont_fret IS INITIAL ).
*
*        SELECT SINGLE *
*        FROM zparam_cont_fret
*        INTO wa_zparam_cont_fret
*        WHERE werks       EQ ' '
*          AND bukrs       EQ wa_j_1bbranch-bukrs
*          AND tp_producao EQ wa_zsdt0158-tp_producao
*          AND dco         EQ wa_zsdt0158-dco
*          " 05.07.2022 - RAMON - 76636 ->
*          AND industrializacao EQ wa_zsdt0158-industrializacao.
*        " 05.07.2022 - RAMON - 76636 -<
*
*      ENDIF.
      "US 152850 11/10/2024 WPP <<<----

      CLEAR vtp_produto.

      IF wa_zsdt0158-tp_producao EQ 'C'.
        vtp_produto = 'CO'.
      ELSEIF wa_zsdt0158-tp_producao EQ 'R'.
        vtp_produto = 'RR'.
      ELSEIF  wa_zsdt0158-tp_producao EQ 'F'.
        vtp_produto = 'F'.
      ELSE.
        vtp_produto = ' '.
      ENDIF.
*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INiCIO


*      SELECT *
*        FROM zmmt0017 INTO TABLE t_zmmt0017
*       WHERE centro_fixo EQ wa_zsdt0158-filial
*        AND  matnr       EQ wa_zsdt0158-id_produto.

*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM

      SELECT SINGLE * FROM zsdt0056 INTO @DATA(wl_zsdt0056)
          WHERE cod_fp EQ @wa_zparam_cont_fret-cod_fp.

      SELECT SINGLE * FROM zsdt0070 INTO @DATA(wl_zsdt0070)
        WHERE cod_fp EQ @wa_zparam_cont_fret-cod_fp.

      IF ( wa_zparam_cont_fret IS NOT INITIAL ).

*** CS2020000143 inicio / Camila Brand 04.12.2020
        IF  wa_zsdt0158-nro_sol_ov IS INITIAL.
*** CS2020000143 fim
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'ZNR_SOL_OV'
            IMPORTING
              number                  = wa_zsdt0158-nro_sol_ov
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.
        ENDIF.

        "Atualiza a tabela de Solicitações com o número da Solicitação de OV
        UPDATE zsdt0158 SET nro_sol_ov = wa_zsdt0158-nro_sol_ov
           WHERE sequencial = wa_zsdt0158-sequencial.

        UPDATE zsdt0158_id SET nro_sol_ov = wa_zsdt0158-nro_sol_ov
          WHERE sequencial EQ wa_zsdt0158-sequencial.


        " 12.05.2022 - RAMON RECLIKE - 76054 -->
        IF wa_zsdt0158-nro_sol_ov IS NOT INITIAL.

          UPDATE zsdt0187 SET nro_sol_ov = wa_zsdt0158-nro_sol_ov
            WHERE sequencial EQ wa_zsdt0158-sequencial.

        ENDIF.
        " 12.05.2022 - RAMON RECLIKE - 76054 --<

        COMMIT WORK.

        READ TABLE t_t001w INTO wa_t001w WITH KEY werks = wa_zsdt0158-filial BINARY SEARCH.

        vl_cliente  = |{ vl_cliente ALPHA = IN }|.
        vl_forn     = |{ vl_forn ALPHA = IN }|.

        IF ( wa_tbtco IS NOT INITIAL ).

          SELECT SINGLE * FROM tbtcp
            INTO @DATA(wl_tbtcp)
            WHERE jobname   EQ @wa_tbtco-jobname AND
                  jobcount  EQ @wa_tbtco-jobcount.

        ENDIF.

*** CS2020000143 inicio / Camila Brand 04.12.2020
        SELECT SINGLE * FROM zsdt0051 INTO @DATA(wa_zsdt0051_aux)
             WHERE nro_sol_ov EQ @wa_zsdt0158-nro_sol_ov.

        IF wa_zsdt0051_aux-data_venda IS INITIAL.
          wa_zsdt0051-data_venda = sy-datum.
        ENDIF.

*** CS2020000143 fim / Camila Brand 04.12.2020

        wa_zsdt0051-nro_sol_ov            = wa_zsdt0158-nro_sol_ov.
        wa_zsdt0051-tp_venda              = wa_zparam_cont_fret-tp_venda.
        wa_zsdt0051-dtde_logist           = ''.
        wa_zsdt0051-dtate_logist          = ''.
        wa_zsdt0051-vbeln                 = ''.
        wa_zsdt0051-status                = wa_zparam_cont_fret-status.
        wa_zsdt0051-auart                 = wa_zparam_cont_fret-auart.
        wa_zsdt0051-vkorg                 = wa_t001w-vkorg.
        wa_zsdt0051-vtweg                 = wa_zparam_cont_fret-vtweg.
        wa_zsdt0051-spart                 = wa_zparam_cont_fret-spart.
        wa_zsdt0051-vkgrp                 = ''.
        wa_zsdt0051-vkbur                 = ''.

        " 05.07.2022 - RAMON - 76636 ->

        IF wa_zparam_cont_fret-kunnr IS NOT INITIAL.
          wa_zsdt0051-kunnr = wa_zparam_cont_fret-kunnr.
        ELSE.
          wa_zsdt0051-kunnr                 = vl_cliente.
        ENDIF.

        "wa_zsdt0051-kunnr                 = vl_cliente.
        " 05.07.2022 - RAMON - 76636 -<

        wa_zsdt0051-correto               = ''.
        wa_zsdt0051-bstkd                 = |Sol. OV { wa_zsdt0158-nro_sol_ov }|.
        wa_zsdt0051-inco1                 = wa_zsdt0158-tp_frete.
        wa_zsdt0051-inco2                 = wa_zsdt0158-tp_frete.
        wa_zsdt0051-vkaus                 = wa_zparam_cont_fret-vkaus.
        wa_zsdt0051-waerk                 = wa_zsdt0158-waers.
        wa_zsdt0051-observacao            = |Solicitação Criada por: { wa_zsdt0158-usnam }|.
        wa_zsdt0051-coment_logistica      = ''.
        wa_zsdt0051-matnr                 = wa_zsdt0158-id_produto.
        wa_zsdt0051-param_espec           = wa_zparam_cont_fret-param_espec.
        wa_zsdt0051-num_fixacao           = ''. "WA_ZPARAM_CONT_FRET-C_DECIMAIS.
        " wa_zsdt0051-data_venda            = ''. "CS2020000143
        wa_zsdt0051-taxa_curva            = ''.
        wa_zsdt0051-usnam                 = wl_tbtcp-authcknam. " SY-UNAME.
        wa_zsdt0051-data_atual            = sy-datum.
        wa_zsdt0051-hora_atual            = sy-uzeit.

*** CS2020000143 inicio / Camila Brand 04.12.2020
*        INSERT INTO zsdt0051 VALUES wa_zsdt0051.
        MODIFY zsdt0051 FROM wa_zsdt0051.
*** CS2020000143 Fim

        "TABELA : ZSDT0052
        wa_zsdt0052-nro_sol_ov  = wa_zsdt0158-nro_sol_ov.
        wa_zsdt0052-pgto_ant    = ''.
        wa_zsdt0052-zlsch       = wa_zparam_cont_fret-zlsch."'P'.
        wa_zsdt0052-zterm       = wa_zparam_cont_fret-zterm."'0001'.
        wa_zsdt0052-qte_venc    = wa_zparam_cont_fret-qte_venc."'0'.
        wa_zsdt0052-valdt       = ''.
        wa_zsdt0052-hbkid       = ''.
        wa_zsdt0052-usnam       = wl_tbtcp-authcknam.
        wa_zsdt0052-data_atual  = sy-datum.
        wa_zsdt0052-hora_atual  = sy-uzeit.

*** CS2020000143 inicio / Camila Brand 04.12.2020
*        INSERT INTO zsdt0052 VALUES wa_zsdt0052.
        MODIFY zsdt0052 FROM wa_zsdt0052.
*** CS2020000143 Fim

        "TABELA : ZSDT0059
        wa_zsdt0059-nro_sol_ov  = wa_zsdt0158-nro_sol_ov.
        wa_zsdt0059-nivel       = wa_zparam_cont_fret-nivel."'01'.
        wa_zsdt0059-posnr       = '10'.
        wa_zsdt0059-cod_fp      = wa_zparam_cont_fret-cod_fp."'04'.
        wa_zsdt0059-field       = wl_zsdt0070-field.
        wa_zsdt0059-bezei       = wl_zsdt0056-bezei.
        wa_zsdt0059-tipo_calc   = wa_zparam_cont_fret-tipo_calc."'V'.
        wa_zsdt0059-formula2    = wa_zsdt0158-vlr_pauta.

        DATA(vl_vlr_pauta) = wa_zsdt0158-vlr_pauta.

        wa_zsdt0059-formula     = vl_vlr_pauta.
        wa_zsdt0059-waers       = wa_zsdt0158-waers.
        wa_zsdt0059-preco       = wa_zparam_cont_fret-preco."'1'.
        wa_zsdt0059-ocbot       = ''.
        wa_zsdt0059-cbot        = ''.
        wa_zsdt0059-c_decimais  = wa_zparam_cont_fret-c_decimais."'2'.
        wa_zsdt0059-zmeng       = ''.
        wa_zsdt0059-usnam       = wl_tbtcp-authcknam.
        wa_zsdt0059-data_atual  = sy-datum.
        wa_zsdt0059-hora_atual  = sy-uzeit.

*** CS2020000143 inicio / Camila Brand 04.12.2020
*        INSERT INTO zsdt0059 VALUES wa_zsdt0059.
        MODIFY zsdt0059 FROM wa_zsdt0059.
*** CS2020000143 Fim

        "Tabela : zsdt0066
        "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO
*        SORT t_zmmt0017 BY tp_produto DESCENDING.
*
*        LOOP AT t_zmmt0017 INTO wa_zmmt0017.
*          IF wa_zmmt0017-tp_produto EQ 'CO' AND vtp_produto EQ 'CO'.
*            wa_0017-lgort = wa_zmmt0017-lgort.
*            EXIT.
*          ELSEIF wa_zmmt0017-tp_produto EQ 'RR' AND vtp_produto EQ 'RR'.
*            wa_0017-lgort = wa_zmmt0017-lgort.
*            EXIT.
*          ELSEIF wa_zmmt0017-tp_produto EQ ' '  AND vtp_produto EQ 'F'.
*            wa_0017-lgort = wa_zmmt0017-lgort.
*            EXIT.
*          ENDIF.
*          CLEAR wa_zmmt0017.
*        ENDLOOP.
*
*        IF wa_0017-lgort IS INITIAL.
*          READ TABLE t_zmmt0017 INTO wa_zmmt0017 INDEX 1.
*          wa_0017-lgort = wa_zmmt0017-lgort.
*        ENDIF.
        "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM

        wa_zsdt0066-nro_sol_ov    = wa_zsdt0158-nro_sol_ov.
        wa_zsdt0066-instrucao     = ''.
        wa_zsdt0066-posnr         = '10'.


        "US 152850 11/10/2024 WPP ---->>>
        IF  wa_zsdt0158-LGORT IS INITIAL AND wa_zsdt0158-eudr IS INITIAL.

          TRY .
            zcl_deposito=>zif_deposito~get_instance(
             )->get_deposito_material_filial(
             EXPORTING
               i_matnr          = wa_zsdt0158-id_produto    " Nº do material
               i_tp_produto     = vtp_produto    " Tipo de Produto
               i_bukrs          = wa_j_1bbranch-bukrs    " Empresa
               i_branch         = wa_zsdt0158-filial    " Local de negócios
             IMPORTING
               e_lgort          = DATA(e_lgort)  ).
          CATCH zcx_deposito. " Classe de Erro de Depósito
          ENDTRY.

          wa_zsdt0066-lgort         = e_lgort.
        ELSEIF wa_zsdt0158-LGORT IS NOT INITIAL.
          wa_zsdt0066-lgort         = wa_zsdt0158-LGORT.
        ENDIF.
        "US 152850 11/10/2024 WPP <<<----

        wa_zsdt0066-zieme         = wa_zsdt0158-unidade.
        wa_zsdt0066-volum         = ''.
        wa_zsdt0066-voleh         = ''.
        wa_zsdt0066-pmein         = wa_zparam_cont_fret-pmein."'TO'.
        wa_zsdt0066-vbeln         = ''.
        wa_zsdt0066-inco1         = wa_zsdt0158-tp_frete.
        wa_zsdt0066-inco2         = wa_zsdt0158-tp_frete.
        wa_zsdt0066-ponto_c       = |{ wa_zsdt0158-id_ponto_coleta ALPHA = IN }|.
        wa_zsdt0066-status        = wa_zparam_cont_fret-status."'L'.
        wa_zsdt0066-matnr         = wa_zsdt0158-id_produto.
        wa_zsdt0066-werks         = wa_zsdt0158-filial.
        wa_zsdt0066-charg         = wa_zsdt0158-safra.

        TRY .
            wa_zsdt0066-zmeng         = wa_zsdt0158-quantidade.
          CATCH cx_sy_conversion_overflow.
            wa_zsdt0066-zmeng         = 0.
        ENDTRY.

        wa_zsdt0066-dmbtr         = wa_zsdt0158-vlr_pauta.
        wa_zsdt0066-vlrtot        = ( wa_zsdt0158-quantidade * wa_zsdt0158-vlr_pauta ).
        wa_zsdt0066-lentrega      = |{ wa_zsdt0158-id_local_destino ALPHA = IN }|.
        wa_zsdt0066-kunnr         = |{ wa_zsdt0051-kunnr ALPHA = IN }|.
        wa_zsdt0066-waerk         = wa_zsdt0158-waers.
        wa_zsdt0066-terminal      = |{ wa_zsdt0158-id_terminal ALPHA = IN }|.
        wa_zsdt0066-usnam         = wl_tbtcp-authcknam.
        wa_zsdt0066-data_atual    = sy-datum.
        wa_zsdt0066-hora_atual    = sy-uzeit.
        wa_zsdt0066-classificacao = wa_zsdt0158-tp_producao.
        wa_zsdt0066-auart         = wa_zparam_cont_fret-auart.
        wa_zsdt0066-industrializacao = wa_zparam_cont_fret-industrializacao.
        wa_zsdt0066-dco           = wa_zsdt0158-nr_dco.
        wa_zsdt0066-kunnr         = |{ wa_zsdt0066-kunnr ALPHA = IN }|.
*        wa_zsdt0066-matnr         = |{ wa_zsdt0066-matnr ALPHA = IN }|.
        wa_zsdt0066-terminal      = |{ wa_zsdt0066-terminal ALPHA = IN }|.
        wa_zsdt0066-zona_pc       = |{ wa_zsdt0158-zona_pc ALPHA = IN }|.
        wa_zsdt0066-zona_lr       = |{ wa_zsdt0158-zona_lr ALPHA = IN }|.
        wa_zsdt0066-kvgr4         = wa_zsdt0158-kvgr4.
        wa_zsdt0066-kvgr5         = wa_zsdt0158-kvgr5.
        wa_zsdt0066-ck_troca_nota = wa_zsdt0158-ck_troca_nota.

*** CS2020000143 inicio / Camila Brand 04.12.2020
*        INSERT INTO zsdt0066 VALUES wa_zsdt0066.
        MODIFY zsdt0066 FROM wa_zsdt0066.

        " ramon teste -->
        MESSAGE s016(ds) WITH 'Solicitação' wa_zsdt0158-nro_sol_ov 'criada via modify'.
        " ramon teste --<

*** CS2020000143 Fim

        PERFORM atualiza_registro_ov USING wa_zsdt0158-status
                                           wa_zsdt0158-nro_sol_ov.

      ENDIF.

      " 26.09.2022 - RAMON - 19450 -->
    ELSEIF ( wa_zsdt0158-status EQ 'L' ) AND ( wa_zsdt0158-tp_solicitacao EQ 'P' ).

      CALL FUNCTION 'ZMM_ME21N_GERA_SOLICITACAO'
        EXPORTING
          iw_zsdt0158 = wa_zsdt0158.

      " 26.09.2022 - RAMON - 19450 --<
    ENDIF.

    CLEAR: wa_zparam_cont_fret.

  ENDLOOP.

**********************************************************************
*Atualiza a tabela ZSDT0187 com o número da OV gerada*
**********************************************************************

****  SELECT a~*, b~nro_sol_ov FROM zsdt0187 AS a
****      INNER JOIN zsdt0158_id AS b ON a~id_compra EQ b~id_compra
****      INTO TABLE @DATA(_zsdt0187)
****      WHERE "a~ov_ped EQ @abap_true AND
****        a~vbeln  EQ ''.
****
****  IF ( _zsdt0187[] IS NOT INITIAL ).
****
****    SELECT nro_sol_ov, vbeln FROM zsdt0066
****      INTO TABLE @DATA(_zsdt0066)
****      FOR ALL ENTRIES IN @_zsdt0187[]
****      WHERE nro_sol_ov EQ @_zsdt0187-nro_sol_ov.
****
****    IF ( _zsdt0066[] IS NOT INITIAL ).
****
****      LOOP AT _zsdt0187[] INTO DATA(wl_0187).
****        READ TABLE _zsdt0066[] INTO DATA(wl_0066) WITH KEY nro_sol_ov = wl_0187-nro_sol_ov.
****        IF ( sy-subrc = 0 ).
****          UPDATE zsdt0187 SET vbeln = wl_0066-vbeln
****            WHERE id_compra EQ wl_0187-a-id_compra.
****          "AND ov_ped    = abap_true.
****        ENDIF.
****        CLEAR: wl_0066.
****      ENDLOOP.
****
****    ENDIF.
****
****  ENDIF.

**********************************************************************
**********************************************************************

  COMMIT WORK.

ENDFORM.      "PROC_DADOS_0158


*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_REGISTRO_OV
*&---------------------------------------------------------------------*
FORM atualiza_registro_ov USING  p_wa_zsdt0158_status p_wa_zsdt0158_nro_sol_ov.

  DATA(vl_rg_atualizado) = COND string( WHEN ( p_wa_zsdt0158_status = 'L' ) THEN '1' ELSE '' ).

  UPDATE zsdt0158 SET rg_atualizado = vl_rg_atualizado WHERE nro_sol_ov = p_wa_zsdt0158_nro_sol_ov.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ALTERA_QUANT_OV
*&---------------------------------------------------------------------*
FORM altera_quant_ov .

  DATA: _erro TYPE sy-subrc.

  SORT t_zsdt0158_qt[] BY nro_sol_ov ASCENDING.

  SELECT nro_sol_ov, posnr, zmeng, zieme, dmbtr, vlrtot
    FROM zsdt0066
    INTO TABLE @DATA(t_0066)
    FOR ALL ENTRIES IN @t_zsdt0158_qt
    WHERE nro_sol_ov = @t_zsdt0158_qt-nro_sol_ov
      AND posnr      = '000010'.

  SORT t_0066[] BY nro_sol_ov ASCENDING.

  LOOP AT t_zsdt0158_qt[] INTO DATA(w_qt).

    READ TABLE t_0066[] INTO DATA(w_0066) WITH KEY nro_sol_ov = w_qt-nro_sol_ov BINARY SEARCH.

    IF ( sy-subrc = 0 ).

      w_0066-zmeng  = w_qt-qtdade_nova.
      w_0066-zieme  = w_qt-unidade.
      w_0066-dmbtr  = w_qt-vlr_pauta.
      w_0066-vlrtot = ( w_qt-unidade * w_qt-vlr_pauta ).

      UPDATE zsdt0066
        SET  zmeng  = w_0066-zmeng
             zieme  = w_0066-zieme
             dmbtr  = w_0066-dmbtr
             vlrtot = w_0066-vlrtot
        WHERE nro_sol_ov = w_qt-nro_sol_ov.

      COMMIT WORK.

      SELECT * FROM zsdt0066
        INTO TABLE @DATA(t_0066_update)
        WHERE nro_sol_ov = @w_qt-nro_sol_ov.

      IF ( sy-subrc = 0 ).

        CALL FUNCTION 'ZSDMF002_ATUALI_OV_SOLICITACAO'
          IMPORTING
            erro              = _erro
          TABLES
            ti_form_lote      = t_0066_update[]    " Tabela de Solicitação de Ordem de Venda – Formação de Lote
          EXCEPTIONS
            ov_nao_encontrada = 1
            OTHERS            = 2.

      ENDIF.

      UPDATE zsdt0158_qt
        SET rg_atualizado = '1'
        WHERE nro_sol_ov = w_qt-nro_sol_ov
          AND data_atual = w_qt-data_atual
          AND hora_atual = w_qt-hora_atual.

    ENDIF.

  ENDLOOP.

ENDFORM.
