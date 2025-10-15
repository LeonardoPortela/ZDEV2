*&---------------------------------------------------------------------*
*& Report  ZFIR059
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir059.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_bsak,
         bukrs TYPE bsak-bukrs,
         belnr TYPE bsak-belnr,
         lifnr TYPE bsak-lifnr,
       END OF ty_bsak.

TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         stblg TYPE bkpf-stblg,
       END OF ty_bkpf.

TYPES: BEGIN OF ty_zfit0125,
         dt_conv    TYPE bsik-budat,
         hkont_desc TYPE bsik-hkont,
         bschl_desc TYPE bsik-bschl,
         vlr_desc   TYPE bsik-dmbtr,
         vlr_resd   TYPE bsik-dmbtr,
         vlr_comp   TYPE bsik-dmbtr.
         INCLUDE STRUCTURE zfit0125.
TYPES: END OF ty_zfit0125.


*----------------------------------------------------------------------*
* Internal Tables e WorkAreas
*----------------------------------------------------------------------*

DATA: tg_0125      TYPE TABLE OF ty_zfit0125 WITH HEADER LINE,
      tg_0126      TYPE TABLE OF zfit0126 WITH HEADER LINE,
      tg_0125_agrp TYPE TABLE OF ty_zfit0125 WITH HEADER LINE,
      tg_bsak      TYPE TABLE OF ty_bsak WITH HEADER LINE,
      tg_bkpf      TYPE TABLE OF ty_bkpf WITH HEADER LINE,
      tg_bsik      TYPE TABLE OF bsik WITH HEADER LINE,
      it_outreturn TYPE TABLE OF zfie_ret_document,
      wa_outreturn TYPE zfie_ret_document.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS:
  c_e   TYPE c VALUE 'E',
  c_s   TYPE c VALUE 'S',
  c_x   TYPE c VALUE 'X',
  c_fi  TYPE zfie_ret_document-id         VALUE 'FI',
  c_899 TYPE zfie_ret_document-num        VALUE '899',
  c_44  TYPE zfie_ret_document-interface  VALUE '44'.

*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*

DATA: vg_msg  TYPE string,
      vg_msg1 TYPE string.

INITIALIZATION.

  DATA: vg_job TYPE i.

  SELECT SINGLE COUNT( * ) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'ZFIR059_JOB'
     AND status EQ 'R'.

  IF ( vg_job EQ 1 ).
    PERFORM: f_selecionar_dados,
             f_compensar_adto,
             f_envia_log_legado.
  ENDIF.

FORM f_selecionar_dados .

  PERFORM f_iniciar_variaveis.

  "Leitura Lançamentos
  SELECT *
    FROM zfit0125 INTO CORRESPONDING FIELDS OF TABLE tg_0125
   WHERE rg_atualizado EQ 'N'.

  CHECK tg_0125[] IS NOT INITIAL.

  SELECT *
    FROM zfit0126 INTO TABLE tg_0126
     FOR ALL ENTRIES IN tg_0125
   WHERE bukrs EQ tg_0125-bukrs
     AND belnr EQ tg_0125-belnr.

  IF tg_0126[] IS NOT INITIAL.
    SELECT *
      FROM bkpf INTO CORRESPONDING FIELDS OF TABLE tg_bkpf
       FOR ALL ENTRIES IN tg_0126
     WHERE bukrs EQ tg_0126-bukrs
       AND belnr EQ tg_0126-augbl
       AND gjahr EQ tg_0126-auggj.
  ENDIF.

  SELECT *
    FROM bsik INTO CORRESPONDING FIELDS OF TABLE tg_bsik
    FOR ALL ENTRIES IN tg_0125
   WHERE bukrs EQ tg_0125-bukrs
     AND belnr EQ tg_0125-belnr
     AND lifnr EQ tg_0125-lifnr.

  "Preparar tabela com agrupamento por Ch. Referencia.
  tg_0125_agrp[] = tg_0125[].
  SORT tg_0125_agrp[] BY ch_referencia.
  DELETE ADJACENT DUPLICATES FROM tg_0125_agrp COMPARING ch_referencia.

  LOOP AT tg_0125_agrp.
    UPDATE zfit0125 SET rg_atualizado = 'S'
                  WHERE ch_referencia = tg_0125_agrp-ch_referencia.
  ENDLOOP.

  COMMIT WORK.

  LOOP AT tg_0125.
    LOOP AT tg_bsik WHERE bukrs = tg_0125-bukrs
                      AND belnr = tg_0125-belnr
                      AND lifnr = tg_0125-lifnr.
      CASE tg_0125-tp_baixa.
        WHEN 'A'.
          IF tg_bsik-shkzg NE 'S'.
            DELETE tg_bsik.
          ENDIF.
        WHEN 'P'.
          IF tg_bsik-shkzg NE 'H'.
            DELETE tg_bsik.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

FORM f_compensar_adto .

  DATA: vl_error_compensacao TYPE c,
        vl_existe_adto       TYPE c,
        vl_valor_comp        TYPE bsik-dmbtr,
        vl_dt_conv           TYPE bsik-budat,
        vl_gerar_desconto    TYPE c,
        vl_dt_comp           TYPE c LENGTH 30.

  "Ordernar para mandar sempre a partida de Adiantamento(TP_BAIXA = A) primeiro para a função.
  SORT tg_0125 BY tp_baixa.

  LOOP AT tg_0125_agrp.

    CLEAR: vl_error_compensacao, vl_existe_adto, vl_valor_comp, vl_dt_conv, vl_gerar_desconto.

    vl_error_compensacao = ''.
    vl_existe_adto       = ''.

    "Valida documentos para compensação.
    LOOP AT tg_0125 WHERE ch_referencia = tg_0125_agrp-ch_referencia.

      READ TABLE tg_bsik WITH KEY bukrs = tg_0125-bukrs
                                  belnr = tg_0125-belnr
                                  lifnr = tg_0125-lifnr.
      IF sy-subrc NE 0 .
        vl_error_compensacao = 'X'.

        CLEAR: vg_msg.
        CONCATENATE 'Não foi encontrada uma partida em aberto para o Documento SAP(' tg_0125-belnr ')! Ch. Referência(' tg_0125-ch_referencia ').'
               INTO vg_msg.

        PERFORM f_prepara_mensagem USING tg_0125-ch_referencia
                                         c_e c_44 vg_msg
                                         '' '' ''.
        EXIT.
      ENDIF.


      READ TABLE tg_0126 WITH KEY bukrs = tg_0125-bukrs
                                  belnr = tg_0125-belnr
                                  lifnr = tg_0125-lifnr
                                  buzei = tg_bsik-buzei.
      IF sy-subrc = 0.
        READ TABLE tg_bkpf WITH KEY bukrs = tg_0126-bukrs
                                    belnr = tg_0126-augbl
                                    gjahr = tg_0126-auggj.

        IF ( sy-subrc = 0 ) AND ( tg_bkpf-stblg IS INITIAL ). "Verifica se documento de Compensação ainda esta ativo
          vl_error_compensacao = 'X'.

          CLEAR: vg_msg, vg_msg1.

          vg_msg1 = tg_0126-residual_comp.

          CONCATENATE 'Documento SAP(' tg_0125-belnr '), já foi compensado(Nro. Doc. Compensação:' tg_0126-augbl ')!  Ch. Referência(' tg_0125_agrp-ch_referencia ').'
                 INTO vg_msg.

          PERFORM f_formata_data_out USING tg_0126-augdt
                                  CHANGING vl_dt_comp.

          PERFORM f_prepara_mensagem USING tg_0125_agrp-ch_referencia
                                           'S'  c_44
                                           vg_msg tg_0126-augbl
                                           vl_dt_comp vg_msg1.
          EXIT.
        ENDIF.
      ENDIF.

















      IF ( tg_bsik-umskz IS NOT INITIAL ) AND ( tg_0125-tp_baixa = 'A' ).
        vl_existe_adto = 'X'.
        vl_dt_conv     = tg_bsik-budat. "Pega data de Liberação adiantamento para utilizar como data de conversão
      ENDIF.

      IF tg_bsik-shkzg = 'S'. "Débito
        vl_valor_comp = vl_valor_comp - abs( tg_bsik-dmbtr ).
      ELSEIF tg_bsik-shkzg = 'H'.  "Crédito
        vl_valor_comp = vl_valor_comp + abs( tg_bsik-dmbtr ).
      ENDIF.

    ENDLOOP.

    CHECK vl_error_compensacao IS INITIAL.

    IF vl_existe_adto IS INITIAL.
      CLEAR: vg_msg.
      CONCATENATE 'Adiantamento não encontrado! Ch. Referência(' tg_0125_agrp-ch_referencia ').'
             INTO vg_msg.

      PERFORM f_prepara_mensagem USING tg_0125_agrp-ch_referencia
                                       c_e c_44
                                       vg_msg '' '' ''.
      CONTINUE.
    ENDIF.

    "Gerar Residual Adiantamento ou Desconto caso diferença seja até 1,00 real.
    IF ( vl_valor_comp NE 0 ).
      LOOP AT tg_0125 WHERE ch_referencia = tg_0125_agrp-ch_referencia
                        AND tp_baixa      = 'A'.  "Adiantamento.

        IF abs( vl_valor_comp ) > 1. "Deixar Residual Adiantamento

          READ TABLE tg_bsik WITH KEY bukrs = tg_0125-bukrs
                                      belnr = tg_0125-belnr
                                      lifnr = tg_0125-lifnr.

          IF ( sy-subrc NE 0 ) OR
             ( ( vl_valor_comp > 0 ) OR ( abs( vl_valor_comp )  > abs( tg_bsik-dmbtr ) ) ) .

            CLEAR: vg_msg.
            CONCATENATE 'Total de contrapartidas é maior que o valor do adiantamento! Ch. Referência(' tg_0125_agrp-ch_referencia ').'
                   INTO vg_msg.

            PERFORM f_prepara_mensagem USING tg_0125_agrp-ch_referencia
                                             c_e c_44
                                             vg_msg '' '' ''.

            vl_error_compensacao = 'X'.
            EXIT.
          ENDIF.

          tg_0125-vlr_resd = abs( vl_valor_comp ).
        ELSE." Gerar desconto
          vl_gerar_desconto = 'X'.

          IF vl_valor_comp < 0.
            tg_0125-vlr_desc    = abs( vl_valor_comp ).
          ELSE.
            tg_0125-vlr_desc    = abs( vl_valor_comp ) * -1.
          ENDIF.
        ENDIF.

        MODIFY tg_0125.
        EXIT.
      ENDLOOP.
    ENDIF.

    CHECK vl_error_compensacao IS INITIAL.

    "Ajustes dados partidas com Data Conversão e/ou Conta Desconto
    LOOP AT tg_0125 WHERE ch_referencia = tg_0125_agrp-ch_referencia.
      IF vl_gerar_desconto IS NOT INITIAL.
        tg_0125-hkont_desc = '0000431101'. "Conta desconto
      ENDIF.
      tg_0125-dt_conv = vl_dt_conv.
      MODIFY tg_0125.
    ENDLOOP.

    CHECK vl_error_compensacao IS INITIAL.

    "    PERFORM F_GERAR_COMPENSACAO_FORNECEDOR USING TG_0125_AGRP-CH_REFERENCIA
    "                                        CHANGING VL_ERROR_COMPENSACAO.

    PERFORM f_bapi_gerar_compensacao USING  tg_0125_agrp-ch_referencia
                                     CHANGING vl_error_compensacao.

  ENDLOOP.

ENDFORM.



FORM f_gerar_compensacao_fornecedor USING p_ch_referencia TYPE zfit0125-ch_referencia
                                 CHANGING p_error.

  DATA: is_col_info  TYPE lvc_s_col,
        is_row_no    TYPE lvc_s_roid,
        ck_erro      TYPE c,
        it_compe     TYPE TABLE OF zde_doc_valor WITH HEADER LINE,
        it_retorno   TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
        it_bkpf_ret  TYPE TABLE OF zde_doc_valor WITH HEADER LINE,
        it_bkpf_ret2 TYPE TABLE OF bapiret2 WITH HEADER LINE,
        vl_desconto  TYPE dmbtr,
        vl_umskz     TYPE char10,
        wl_zfit0126  TYPE zfit0126,
        vl_doc_comp  TYPE bsik-belnr,
        wl_bsak      TYPE bsak,
        vl_dt_comp   TYPE c LENGTH 30,
        vl_zlsch     TYPE bsik-zlsch,
        vl_vlr_rsd   TYPE bsik-dmbtr.

  CLEAR: it_compe[], ck_erro, vl_desconto, vl_umskz, vl_vlr_rsd, vl_zlsch.

  LOOP AT tg_0125 WHERE ch_referencia = p_ch_referencia.

    CLEAR: it_compe.

    READ TABLE tg_bsik WITH KEY bukrs = tg_0125-bukrs
                                belnr = tg_0125-belnr.
    IF sy-subrc NE 0.
      p_error = 'X'.
      CLEAR: vg_msg.
      CONCATENATE 'Não foi encontrada uma partida em aberto para o Documento SAP(' tg_0125-belnr ')! Ch. Referência(' tg_0125-ch_referencia ').'
               INTO vg_msg.

      PERFORM f_prepara_mensagem USING tg_0125-ch_referencia
                                       c_e c_44 vg_msg
                                       '' '' ''.
      RETURN.
    ENDIF.

    it_compe-bukrs     = tg_bsik-bukrs.
    it_compe-belnr     = tg_bsik-belnr.
    it_compe-buzei     = tg_bsik-buzei.
    it_compe-gjahr     = tg_bsik-gjahr.
    it_compe-waers     = tg_bsik-waers.
    it_compe-dmbtr     = tg_bsik-dmbtr - tg_0125-vlr_resd.
    it_compe-konto     = tg_0125-hkont_desc.
    it_compe-budat     = sy-datum.
    it_compe-bldat     = sy-datum.
    it_compe-wwert     = tg_0125-dt_conv.
    it_compe-koart     = 'K'.
    it_compe-dmbtr_res = tg_0125-vlr_resd.
    it_compe-zfbdt_res = tg_bsik-zfbdt.
    it_compe-parid     = tg_bsik-lifnr.
    it_compe-shkzg     = tg_bsik-shkzg.
    it_compe-sgtxt     = tg_bsik-sgtxt.
    it_compe-umsks     = tg_bsik-umsks.

    APPEND it_compe.

    IF tg_0125-tp_baixa EQ 'A'.
      vl_vlr_rsd    = tg_0125-vlr_resd.
      vl_zlsch      = tg_bsik-zlsch.
      IF tg_0125-vlr_desc IS NOT INITIAL.
        vl_desconto = tg_0125-vlr_desc.
      ENDIF.
    ENDIF.

  ENDLOOP.

  CHECK it_compe[] IS NOT INITIAL.

  CALL FUNCTION 'Z_FI_GL_BAIXA_F_51'
    EXPORTING
      p_blart              = 'MA'
      p_tp_compensacao     = '04'
      p_umskz              = 'KZ'
      p_desconto           = vl_desconto
      p_forma_pag_residual = vl_zlsch
    TABLES
      it_compensar         = it_compe
      it_retorno           = it_retorno
      it_bkpf_ret          = it_bkpf_ret
      it_bkpf_ret2         = it_bkpf_ret2
    EXCEPTIONS
      erro_bloqueio        = 1
      OTHERS               = 2.

  IF sy-subrc NE 0.
    CLEAR: vg_msg.
    CONCATENATE sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
           INTO vg_msg.

    PERFORM f_prepara_mensagem USING p_ch_referencia
                                     c_e c_44
                                     vg_msg '' '' ''.
    EXIT.
  ELSE.

    READ TABLE it_bkpf_ret2 WITH KEY type = 'S' id = 'F5' number = 312.
    IF sy-subrc IS INITIAL.
      MOVE: it_bkpf_ret2-id         TO sy-msgid,
            it_bkpf_ret2-type       TO sy-msgty,
            it_bkpf_ret2-number     TO sy-msgno,
            it_bkpf_ret2-message_v1 TO sy-msgv1,
            it_bkpf_ret2-message_v2 TO sy-msgv2,
            it_bkpf_ret2-message_v3 TO sy-msgv3,
            it_bkpf_ret2-message_v4 TO sy-msgv4.

      vl_doc_comp = sy-msgv1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_doc_comp
        IMPORTING
          output = vl_doc_comp.

      CLEAR: vg_msg, it_compe.

      READ TABLE it_compe INDEX 1.

      SELECT SINGLE *
        FROM bsak INTO wl_bsak
       WHERE bukrs = it_compe-bukrs
         AND belnr = vl_doc_comp.

      CHECK sy-subrc = 0.

      "Gravar Documento Compensação
      LOOP AT it_compe.
        CLEAR: wl_zfit0126.
        wl_zfit0126-bukrs          = it_compe-bukrs.
        wl_zfit0126-belnr          = it_compe-belnr.
        wl_zfit0126-buzei          = it_compe-buzei.
        wl_zfit0126-ch_referencia  = p_ch_referencia.
        wl_zfit0126-augbl          = wl_bsak-belnr.
        wl_zfit0126-augdt          = wl_bsak-augdt.
        wl_zfit0126-auggj          = wl_bsak-auggj.
        wl_zfit0126-lifnr          = it_compe-parid.
        wl_zfit0126-residual_comp  = vl_vlr_rsd.
        MODIFY zfit0126 FROM wl_zfit0126.
      ENDLOOP.

      CONCATENATE 'O Documento' wl_bsak-belnr
                  'foi gerado para a empresa' wl_bsak-bukrs
                  'no exercício' wl_bsak-augdt(4) '!'
            INTO vg_msg SEPARATED BY space.

      PERFORM f_formata_data_out USING wl_bsak-augdt
                              CHANGING vl_dt_comp.

      vg_msg1 = vl_vlr_rsd.

      PERFORM f_prepara_mensagem USING p_ch_referencia
                                       'S' c_44
                                       vg_msg vl_doc_comp vl_dt_comp vg_msg1.

      EXIT.

    ELSE.
      LOOP AT it_bkpf_ret2 WHERE type = c_e.
        vg_msg = it_bkpf_ret2-message.
        PERFORM f_prepara_mensagem USING p_ch_referencia
                                         c_e c_44
                                         vg_msg '' '' ''.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " GERAR_COMPENSACAO_FORNECEDOR

FORM f_envia_log_legado .

* Chamar função assíncrona de retorno, confirmando a gravação
* de dados
  IF NOT it_outreturn[] IS INITIAL.
    SORT it_outreturn BY obj_key interface.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        outreturn = it_outreturn.

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
          outreturn = it_outreturn.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = it_outreturn.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

    COMMIT WORK.

  ENDIF.

ENDFORM.

FORM f_prepara_mensagem USING pobj_key
                              ptype
                              interface
                              pmessage
                              pmessage_v1
                              pmessage_v2
                              pmessage_v3.

  CLEAR wa_outreturn.

  wa_outreturn-obj_key        = pobj_key.
  wa_outreturn-interface      = interface.
  wa_outreturn-dt_atualizacao = sy-datum.
  wa_outreturn-hr_atualizacao = sy-uzeit.
  wa_outreturn-type           = ptype.
  wa_outreturn-id             = c_fi.
  wa_outreturn-num            = c_899.
  wa_outreturn-message        = pmessage.
  wa_outreturn-message_v1     = pmessage_v1.
  wa_outreturn-message_v2     = pmessage_v2.
  wa_outreturn-message_v3     = pmessage_v3.

  APPEND wa_outreturn TO it_outreturn.


ENDFORM.                    "z_prepara_mensagem

FORM f_iniciar_variaveis .

  CLEAR: tg_0125[], tg_0125_agrp[], it_outreturn[],
         tg_bsik[], tg_bsak[].

ENDFORM.

FORM f_formata_data_out USING p_data
                     CHANGING p_data_out.

  CHECK p_data IS NOT INITIAL.
  CLEAR: p_data_out.

  CONCATENATE p_data+6(2) '/' p_data+4(2) '/'  p_data(4)   INTO p_data_out.

ENDFORM.



FORM f_bapi_gerar_compensacao    USING p_ch_referencia TYPE zfit0125-ch_referencia
                                 CHANGING p_error.

  DATA: ck_erro     TYPE c,
        wl_zfit0126 TYPE zfit0126,
        vl_doc_comp TYPE bsik-belnr,
        wl_bsak     TYPE bsak,
        vl_dt_comp  TYPE c LENGTH 30,
        vl_vlr_rsd  TYPE bsik-dmbtr,
        dt_conv     TYPE bsik-budat,
        v_blart     TYPE bsik-blart,
        v_augdt     TYPE zfit0125-dt_lcto,
        vwaers      TYPE bsik-waers,
        vbukrs      TYPE bsik-bukrs.

  DATA: zcl_compensacao  TYPE REF TO zcl_fi_compensacao,
        it_partidas_comp TYPE z_partida_comp_fi_t,
        wl_partida       TYPE zde_partida_comp_fi.


  FREE zcl_compensacao.

  CREATE OBJECT zcl_compensacao.

  CLEAR: it_partidas_comp[], ck_erro,  vl_vlr_rsd, vl_doc_comp, dt_conv, vbukrs, vwaers, vl_vlr_rsd, v_blart, v_augdt .

  LOOP AT tg_0125 WHERE ch_referencia = p_ch_referencia.

    dt_conv  = tg_0125-dt_conv.
    vbukrs   = tg_0125-bukrs.
    vwaers   = tg_0125-waers.
    v_augdt  = tg_0125-dt_lcto.

    READ TABLE tg_bsik WITH KEY bukrs = tg_0125-bukrs
                                belnr = tg_0125-belnr.
    IF sy-subrc NE 0.
      p_error = 'X'.
      CLEAR: vg_msg.
      CONCATENATE 'Não foi encontrada uma partida em aberto para o Documento SAP(' tg_0125-belnr ')! Ch. Referência(' tg_0125-ch_referencia ').'
               INTO vg_msg.

      PERFORM f_prepara_mensagem USING tg_0125-ch_referencia
                                       c_e c_44 vg_msg
                                       '' '' ''.
      RETURN.
    ENDIF.


    IF tg_0125-tp_baixa EQ 'A'.
      IF tg_0125-vlr_desc IS NOT INITIAL.
        CLEAR wl_partida.
        wl_partida-bukrs  = tg_bsik-bukrs.

        IF tg_0125-vlr_desc < 0.
          wl_partida-bschl  = '50'.
          wl_partida-agkon  = '0000331102'.
        ELSE.
          wl_partida-bschl  = '40'.
          wl_partida-agkon  = '0000431101'.
        ENDIF.
        wl_partida-waers  = tg_bsik-waers.
        wl_partida-dmbtr  = tg_0125-vlr_desc.
        wl_partida-gsber  = tg_bsik-gsber.
        APPEND wl_partida TO it_partidas_comp.
      ENDIF.
    ENDIF.


    CLEAR wl_partida.
    wl_partida-bukrs          = tg_bsik-bukrs.
    wl_partida-belnr          = tg_bsik-belnr.
    wl_partida-gjahr          = tg_bsik-gjahr.
    wl_partida-buzei          = tg_bsik-buzei.
    wl_partida-lifnr          = tg_bsik-lifnr.

*---> 09/06/2023 - Migração S4 - JS
*            wl_partida-vlr_residual   = tg_0125-vlr_resd.
    wl_partida-vlr_residual = CONV #( tg_0125-vlr_resd ).
*<--- 09/06/2023 - Migração S4 - JS



*   CS2017000399 - Baixa Adto - Saldo Residual - Copiar informação do documento origem  US 74720 - BG - Inicio
    wl_partida-xref1          = tg_bsik-xref1.
    wl_partida-xref3          = tg_bsik-xref3.
*   CS2017000399 - Baixa Adto - Saldo Residual - Copiar informação do documento origem  US 74720 - BG - Inicio
    wl_partida-xnops          = abap_true.
    wl_partida-tx_auto        = abap_true.
    APPEND wl_partida TO it_partidas_comp.


    IF tg_0125-tp_baixa EQ 'P'.

      READ TABLE tg_bsik WITH KEY bukrs = tg_0125-bukrs
                                  belnr = tg_0125-belnr.
      IF sy-subrc EQ 0.
        v_blart = tg_bsik-blart.
      ENDIF.

    ENDIF.


  ENDLOOP.


  TRY.
      zcl_compensacao->set_bukrs(          i_bukrs = vbukrs ).
      "zcl_compensacao->set_dt_compensacao( i_augdt = sy-datum ).
      zcl_compensacao->set_dt_compensacao( i_augdt = v_augdt ).
      zcl_compensacao->set_moeda(          i_waers = vwaers ).
      zcl_compensacao->set_dt_conversao(   i_wwert = dt_conv  ).
      "zcl_compensacao->set_tp_documento(   i_blart = 'MA' ).
      zcl_compensacao->set_tp_documento(   i_blart = v_blart ).

      LOOP AT it_partidas_comp INTO wl_partida.
        zcl_compensacao->add_partida( EXPORTING i_partida_comp = wl_partida ).
      ENDLOOP.

      DATA(_compensado) = zcl_compensacao->compensar( IMPORTING e_belnr = vl_doc_comp ).
      MESSAGE |Dcumento gerado: { vl_doc_comp } | TYPE 'I'.
    CATCH zcx_fi_compensacao INTO DATA(ex_compensacao).
      vg_msg = ex_compensacao->get_text(  ).
  ENDTRY.


  IF vl_doc_comp IS INITIAL.
    CLEAR: vg_msg.
    PERFORM f_prepara_mensagem USING p_ch_referencia
                                     c_e c_44
                                     vg_msg '' '' ''.
    EXIT.
  ELSE.

    vl_doc_comp = |{ vl_doc_comp ALPHA = IN  }|.

    CLEAR: vg_msg.

    READ TABLE it_partidas_comp INTO wl_partida INDEX 1.

    SELECT SINGLE *
      FROM bsak INTO wl_bsak
     WHERE bukrs EQ wl_partida-bukrs
       AND belnr EQ vl_doc_comp.

    CHECK sy-subrc = 0.

    "Gravar Documento Compensação
    LOOP AT it_partidas_comp INTO wl_partida.
      CLEAR: wl_zfit0126.
      wl_zfit0126-bukrs          = wl_partida-bukrs.
      wl_zfit0126-belnr          = wl_partida-belnr.
      wl_zfit0126-buzei          = wl_partida-buzei.
      wl_zfit0126-ch_referencia  = p_ch_referencia.
      wl_zfit0126-augbl          = wl_bsak-belnr.
      wl_zfit0126-augdt          = wl_bsak-augdt.
      wl_zfit0126-auggj          = wl_bsak-auggj.
      wl_zfit0126-lifnr          = wl_partida-lifnr.
      wl_zfit0126-residual_comp  = wl_partida-vlr_residual.
      vl_vlr_rsd                 = ( vl_vlr_rsd + wl_partida-vlr_residual ).
      MODIFY zfit0126 FROM wl_zfit0126.
    ENDLOOP.

    CONCATENATE 'O Documento' wl_bsak-belnr
                'foi gerado para a empresa' wl_bsak-bukrs
                'no exercício' wl_bsak-augdt(4) '!'
          INTO vg_msg SEPARATED BY space.

    PERFORM f_formata_data_out USING wl_bsak-augdt
                            CHANGING vl_dt_comp.

    vg_msg1 = vl_vlr_rsd.

    PERFORM f_prepara_mensagem USING p_ch_referencia
                                     'S' c_44
                                     vg_msg vl_doc_comp vl_dt_comp vg_msg1.
    EXIT.

  ENDIF.

ENDFORM.                    " GERAR_COMPENSACAO_FORNECEDOR
