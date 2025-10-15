*&---------------------------------------------------------------------*
*&  Include           ZGL031_CLASS
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2JVI |12/05/2025 |Ajuste Campo Competencia para  &*
*&                                    |range para os tipos 17, 29, 21 &*
*&                                    |e '22'                         &*
*&                                    |Chamado: 164255.               &*
*&--------------------------------------------------------------------&*
*----------------------------------------------------------------------*
*       CLASS ZUTILS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zutils DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS z_number_get_next.

    METHODS checar_dia_util EXPORTING
                              e_intervalo TYPE phk_scrdd
                            CHANGING
                              c_data_venc TYPE sy-datum.

    METHODS z_seleciona_dados_0110.
    METHODS z_seleciona_dados_0150.
    METHODS z_seleciona_dados_0160.
    METHODS z_seleciona_dados_0170.

    METHODS z_check_baixa IMPORTING
                            i_wl_068     TYPE zglt068
                          EXPORTING
                            e_valida     TYPE c
                            e_last_month TYPE c
                            e_vlr_bx_usd TYPE zglt068-vlr_premio_usd
                            e_vlr_bx_brl TYPE zglt068-vlr_premio_brl.

    METHODS z_check_ctb_apolice IMPORTING
                                  i_wl_050 TYPE zglt050
                                EXPORTING
                                  e_valida TYPE c.

    METHODS z_ajusta_vlr_baixa EXPORTING e_error TYPE c.

    METHODS z_retorna_status_zib
      IMPORTING
        i_doc_lcto TYPE num10
        i_ano_lcto TYPE num4
      EXPORTING
        e_zibchv   TYPE zib_contabil_chv
        e_ziberr   TYPE zib_contabil_err.

    METHODS z_tratar_campos IMPORTING
                              name      TYPE screen-name
                              group1    TYPE char3
                              group2    TYPE char3
                              value     TYPE char1
                              invisible TYPE char1.

    METHODS z_calcula_intervalo_data IMPORTING
                                       i_date1                 TYPE sy-datum
                                       i_date2                 TYPE sy-datum
                                       i_date3                 TYPE sy-datum
                                       i_date4                 TYPE sy-datum
                                     EXPORTING
                                       e_intervalo_apropriacao TYPE i
                                       e_intervalo_vigencia    TYPE i
                                       e_intervalo_parcelas    TYPE i.

    METHODS z_ultimo_dia_mes IMPORTING
                               i_date1      TYPE sy-datum
                             EXPORTING
                               e_ultimo_dia TYPE sy-datum.

    DATA: at_months TYPE tfmatage.


    METHODS z_converter_moeda IMPORTING
                                moeda                 TYPE waers
                                taxa_cambio           TYPE wkurs
                                vlr_usd               TYPE dmbtr
                                vlr_brl               TYPE dmbtr
                              EXPORTING
                                vlr_premio_convertido TYPE dmbtr.

    METHODS z_show_splitter_error IMPORTING
                                    i_show TYPE c.

    METHODS z_validar_info_alv_0120.
    METHODS z_validar_info_alv_0130 IMPORTING i_valida_tot TYPE c.

    METHODS z_validar_cabecalho_0110.
    METHODS z_validar_cabecalho_0150.
    METHODS z_validar_cabecalho_0160.
    METHODS z_validar_cabecalho_0170.

    METHODS z_criar_mensagem_erro IMPORTING
                                    msg_type TYPE c
                                    text1    TYPE itex132
                                    text2    TYPE itex132
                                    field    TYPE char30
                                    index    TYPE sy-tabix
                                    aba      TYPE char4.

    METHODS z_style_disable_edit IMPORTING
                                   fieldname TYPE any
                                   style     TYPE any.

  PRIVATE SECTION.
    DATA: gt_holidays   TYPE TABLE OF iscal_day,
          wl_holidays   TYPE iscal_day,
          at_vlr_premio TYPE dmbtr,
          at_day        TYPE scal-indicator,
          at_cont       TYPE numc3 VALUE 1.
ENDCLASS.                    "ZUTEIS DEFINITION

*----------------------------------------------------------------------*
*       CLASS ZUTILS IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zutils IMPLEMENTATION.
  METHOD z_seleciona_dados_0110.
    DATA: wl_zglt065     TYPE zglt065,
          wl_lfa1        TYPE lfa1,
          cod_corretora  TYPE lfa1-lifnr,
          cod_seguradora TYPE lfa1-lifnr.

    CLEAR wl_cabecalho_0110_aux.

    SELECT SINGLE *
      FROM t001
      INTO wl_t001
     WHERE bukrs = wl_cabecalho_0110-bukrs.

    MOVE wl_t001-butxt TO wl_cabecalho_0110_aux-butxt.

    SELECT SINGLE *
      FROM zglt064
      INTO wl_zglt064
     WHERE seq_tipo = wl_cabecalho_0110-seq_tipo.

    MOVE wl_zglt064-descr TO wl_cabecalho_0110_aux-tipo.

*    SELECT SINGLE *
*      FROM ZGLT065
*      INTO WL_ZGLT065
*     WHERE SEQ_MOD = WL_CABECALHO_0110-SEQ_MOD.
*
*    MOVE WL_ZGLT065-DESCR TO WL_CABECALHO_0110_AUX-MODALIDADE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_cabecalho_0110-cod_corretora
      IMPORTING
        output = cod_corretora.

    SELECT SINGLE *
      FROM lfa1
      INTO wl_lfa1
     WHERE lifnr = cod_corretora.

    MOVE wl_lfa1-name1 TO wl_cabecalho_0110_aux-corretora.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_cabecalho_0110-cod_seguradora
      IMPORTING
        output = cod_seguradora.

    SELECT SINGLE *
      FROM lfa1
      INTO wl_lfa1
     WHERE lifnr = cod_seguradora.

    MOVE wl_lfa1-name1 TO wl_cabecalho_0110_aux-seguradora.
  ENDMETHOD.                    "Z_SELECIONA_DADOS

  METHOD z_seleciona_dados_0150.

    DATA: r_gerar_lote             TYPE REF TO zcl_gerar_lote,
          l_dt_vigen               LIKE LINE OF r_date,
          l_dt_aprop               LIKE LINE OF r_date,
          vlr_premio_usd           TYPE dmbtr,
          vlr_premio_brl           TYPE dmbtr,
          vl_tot_aprop_usd         TYPE dmbtr,
          vl_tot_aprop_brl         TYPE dmbtr,
          vl_dif_aprop_usd         TYPE dmbtr,
          vl_dif_aprop_brl         TYPE dmbtr,
          taxa_cambio              TYPE wkurs,
          at_index                 TYPE i,
          at_intervalo_apropriacao TYPE tfmatage,
          at_intervalo_vigencia    TYPE tfmatage,
          at_dt_apropriacao        TYPE sy-datum,
          at_dif_vigen             TYPE tfmatage,
          at_objkey                TYPE char20,
          vl_nr_item               TYPE zglt073-nr_item,
          vl_valida_baixa          TYPE c,
          vl_month_baixa           TYPE c,
          vl_vlr_bx_usd            TYPE zglt068-vlr_premio_usd,
          vl_vlr_bx_brl            TYPE zglt068-vlr_premio_brl,
          gt_zglt073_aux           TYPE TABLE OF zglt073,
          wl_zglt073_aux           TYPE zglt073,
          soma_parcela_brl         TYPE dmbtr,
          soma_parcela_usd         TYPE dmbtr.

*          GO_UTILS           TYPE REF TO ZUTILS.

*    CREATE OBJECT GO_UTILS.
    CLEAR: gt_saida_0150[], gt_zglt068[], gt_zglt050[], gt_zglt031[], gt_zglt032[],
           gt_zglt064[], gt_t001[].

    PERFORM f_monta_range USING '0150'.

    SELECT *
      FROM zglt050 INTO TABLE gt_zglt050
     WHERE finalizada EQ ''
       AND bukrs      IN r_bukrs
       AND seq_lcto   IN r_seq_lcto
       AND seq_tipo   IN r_seq_tipo
       AND loekz      EQ ''.

    CHECK gt_zglt050[] IS NOT INITIAL.

    SELECT *
      FROM zglt068
      INTO TABLE gt_zglt068
   FOR ALL ENTRIES IN gt_zglt050
     WHERE seq_lcto = gt_zglt050-seq_lcto
       AND dt_baixa = '00000000'.

    SELECT *
      FROM t001 INTO TABLE gt_t001
       FOR ALL ENTRIES IN gt_zglt050
     WHERE bukrs = gt_zglt050-bukrs.

    CLEAR: wl_cabecalho_0150-descr_bukrs.
    IF lines( gt_t001[] ) = 1.
      READ TABLE gt_t001 INTO wl_t001 INDEX 1.
      MOVE wl_t001-butxt TO wl_cabecalho_0150-descr_bukrs.
    ENDIF.
* -
    SELECT *
      FROM zglt064 INTO TABLE gt_zglt064
       FOR ALL ENTRIES IN gt_zglt050
     WHERE seq_tipo EQ gt_zglt050-seq_tipo.

    CLEAR: wl_cabecalho_0150-descr_tipo.
    IF lines( gt_zglt064[] ) = 1.
      READ TABLE gt_zglt064 INTO wl_zglt064 INDEX 1.
      MOVE wl_zglt064-descr TO wl_cabecalho_0150-descr_tipo.
    ENDIF.
* -
    IF gt_zglt064[] IS NOT INITIAL.
      SELECT *
        FROM zglt031 INTO TABLE gt_zglt031
         FOR ALL ENTRIES IN gt_zglt064
       WHERE tp_lcto EQ gt_zglt064-tp_lcto_aprop.

      SELECT *
        FROM zglt032 INTO TABLE gt_zglt032
         FOR ALL ENTRIES IN gt_zglt064
       WHERE tp_lcto EQ gt_zglt064-tp_lcto_aprop.
    ENDIF.

    SORT gt_zglt068 BY seq_lcto.
    LOOP AT gt_zglt068 INTO wl_zglt068.
      CLEAR: wl_zglt050, wl_t001, wl_zglt064.

      READ TABLE gt_zglt050 INTO wl_zglt050 WITH KEY seq_lcto = wl_zglt068-seq_lcto.

      CHECK sy-subrc = 0.

      DATA(_ok) = abap_false.
      PERFORM f_check_authority USING wl_zglt050-bukrs
                                      abap_false
                             CHANGING _ok.

      CHECK _ok IS NOT INITIAL.

      "Verifica se o Bem já foi baixado.
      me->z_check_baixa( EXPORTING i_wl_068     = wl_zglt068
                         IMPORTING e_valida     = vl_valida_baixa
                                   e_last_month = vl_month_baixa
                                   e_vlr_bx_usd = vl_vlr_bx_usd
                                   e_vlr_bx_brl = vl_vlr_bx_brl
                                  ).

      CHECK vl_valida_baixa IS NOT INITIAL.

      l_dt_aprop-sign   = 'I'.
      l_dt_aprop-option = 'BT'.
      l_dt_aprop-low    = wl_cabecalho_0150-dt_aprop_de.
      l_dt_aprop-high   = wl_cabecalho_0150-dt_aprop_ate.

      z_calcula_intervalo_data( EXPORTING i_date1                 = l_dt_aprop-low
                                          i_date2                 = l_dt_aprop-high
                                          i_date3                 = wl_zglt050-vig_de
                                          i_date4                 = wl_zglt050-vig_ate
                                IMPORTING e_intervalo_apropriacao = at_intervalo_apropriacao
                                          e_intervalo_vigencia    = at_intervalo_vigencia
                                          e_intervalo_parcelas    = at_index ).

      IF at_index = 0. "ALRS
        CONTINUE.
      ENDIF.
*      AT_INDEX = 1.
*      DO AT_INTERVALO_APROPRIACAO TIMES.

      DATA(_aprop_final) = ''.
      DATA(_ger_aprop_final) = ''.

      WHILE ( l_dt_aprop-low <= l_dt_aprop-high ).
*        CLEAR RETURN_STATUS.

*       Checa se a data inicial <= a data final informada, se ultrapassa

*        CHECK ( L_DT_APROP-LOW(6) LE L_DT_APROP-HIGH(6) ).

*       Verifica se a data informada é >= a data inicial da vigência e s
*       é <= a data final de vigência.

        IF ( _ger_aprop_final        IS INITIAL               ) AND " Não gerado ultima parcela
           ( l_dt_aprop-low(6)       EQ wl_zglt050-vig_ate(6) ) AND " Ultimo Mês
           ( wl_zglt050-vig_de+6(2)  > '15'                   ) AND " Inicio Vigencia Maior que dia 15 do Mês
           ( wl_zglt050-vig_ate+6(2) > wl_zglt050-vig_de+6(2) ) AND " Dia da vigencia final for superior ao dia da vigencia inicial
           ( ( at_index + 1 )        EQ at_intervalo_vigencia ).
          _aprop_final      = 'X'.                                  " Gerar linha para Aprop. da ultima parcela no ultimo mes da vigencia.
          _ger_aprop_final  = 'X'.
        ENDIF.

        IF ( l_dt_aprop-low(6) GE wl_zglt050-vig_de(6)  ) AND
           ( l_dt_aprop-low(6) LE wl_zglt050-vig_ate(6) ).

          CLEAR wl_saida_0150.

*       _____________________Verifica se vigência inicia até dia 15 do m

*       Regras da apropriação:

*       1. Para os seguros com início de vigência > 15 do mês,
*          a apropriação deverá ocorrer apenas no mês seguinte;

*       2. Para os seguros com início de vigência <= 15 do mês,
*          a apropriação não ocorrerá no último mês;

*       3. Para os seguros com início de vigência <= 15 do mês,
*          e dia da vigencia final for superior ao dia da vigencia inicial
*          a apropriação ocorrerá no último mês;

          IF ( l_dt_aprop-low(6) EQ wl_zglt050-vig_de(6) ) AND
             ( wl_zglt050-vig_de+6(2) GT '15'            ).

            PERFORM f_date_increment CHANGING l_dt_aprop-low.

          ELSEIF ( l_dt_aprop-low(6)      EQ wl_zglt050-vig_ate(6)  ) AND
                 ( wl_zglt050-vig_de+6(2) LE '15'                   ) AND
                 ( wl_zglt050-vig_ate+6(2) > wl_zglt050-vig_de+6(2) ).


          ELSEIF ( l_dt_aprop-low(6) EQ wl_zglt050-vig_ate(6) ) AND
                 ( wl_zglt050-vig_de+6(2) LE '15'             ).

*            PERFORM F_DATE_INCREMENT CHANGING L_DT_APROP-LOW.
*            CONTINUE.
          ENDIF.
*       ____________________Pega o último dia do mês_____________________
**<<<------"164255 - NMS - INI------>>>
          IF wl_cabecalho_0150-seq_tipo EQ 17 OR
             wl_cabecalho_0150-seq_tipo EQ 21 OR
             wl_cabecalho_0150-seq_tipo EQ 22 OR
             wl_cabecalho_0150-seq_tipo EQ 29.
**<<<------"164255 - NMS - FIM------>>>
            CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
              EXPORTING
                day_in            = l_dt_aprop-low
              IMPORTING
                last_day_of_month = at_dt_apropriacao.
**<<<------"164255 - NMS - INI------>>>
          ELSE.
            at_dt_apropriacao = wl_cabecalho_0150-dtapr_ult_de.

          ENDIF.
**<<<------"164255 - NMS - FIM------>>>
*       ________________________________________________________________

*          IF ( WL_ZGLT068-ANLN1 IS NOT INITIAL ) OR
*             ( WL_ZGLT068-ANLN2 IS NOT INITIAL ) OR
*             ( WL_ZGLT068-MATNR IS NOT INITIAL ).
*
*            SELECT SINGLE *
*              FROM ZGLT073
*              INTO WL_ZGLT073
*             WHERE SEQ_LCTO  EQ WL_ZGLT068-SEQ_LCTO
*               AND NRO_PARC  EQ AT_INDEX
*               AND ANLN1     EQ WL_ZGLT068-ANLN1
*               AND ANLN2     EQ WL_ZGLT068-ANLN2
*               AND MATNR     EQ WL_ZGLT068-MATNR
*               AND DT_APROPR EQ AT_DT_APROPRIACAO.
*
*          ELSE.
*
*            CLEAR: VL_NR_ITEM.
*            SELECT SINGLE *
*              FROM ZGLT073
*              INTO WL_ZGLT073
*             WHERE SEQ_LCTO   EQ WL_ZGLT068-SEQ_LCTO
*               AND NRO_PARC   EQ AT_INDEX
*               AND DESCR_BENS EQ WL_ZGLT068-DESCR_BENS
*               AND ANLN1      EQ ''
*               AND ANLN2      EQ ''
*               AND MATNR      EQ ''
*               AND DT_APROPR  EQ AT_DT_APROPRIACAO.
*
*          ENDIF.

          CLEAR: vl_nr_item.
          SELECT SINGLE *
            FROM zglt073
            INTO wl_zglt073
           WHERE seq_lcto   EQ wl_zglt068-seq_lcto
             AND nro_parc   EQ at_index
             AND nr_item    EQ wl_zglt068-nr_item.
          "AND DT_APROPR  EQ AT_DT_APROPRIACAO.

          IF ( sy-subrc IS INITIAL ).
**<<<------"164255 - NMS - INI------>>>
            IF wl_cabecalho_0150-seq_tipo NE 17 AND
               wl_cabecalho_0150-seq_tipo NE 21 AND
               wl_cabecalho_0150-seq_tipo NE 22 AND
               wl_cabecalho_0150-seq_tipo NE 29.
              wl_zglt073-dt_apropr = wl_cabecalho_0150-dtapr_ult_de.

            ENDIF.
**<<<------"164255 - NMS - FIM------>>>
            z_retorna_status_zib( EXPORTING i_doc_lcto = wl_zglt073-doc_lcto
                                            i_ano_lcto = wl_zglt073-dt_apropr(4)
                                  IMPORTING e_zibchv   = wl_zib_chave
                                            e_ziberr   = wl_zib_erro ).

            IF ( wl_zib_chave IS NOT INITIAL ).
              wl_saida_0150-status       = icon_green_light .
              wl_saida_0150-doc_contabil = wl_zib_chave-belnr.
              wl_saida_0150-xblnr = wl_zib_chave-belnr. " RJF - 83767
            ELSEIF ( wl_zib_erro IS NOT INITIAL ).
              wl_saida_0150-status       = icon_red_light.
            ELSE.
              wl_saida_0150-status       = icon_yellow_light.
            ENDIF.

            wl_saida_0150-bukrs          = wl_zglt050-bukrs.
            wl_saida_0150-dt_apropr      = wl_zglt073-dt_apropr.
            wl_saida_0150-seq_lcto       = wl_zglt073-seq_lcto.
            wl_saida_0150-nro_parc       = wl_zglt073-nro_parc.
            wl_saida_0150-nro_apolice    = wl_zglt050-nro_apolice.
            wl_saida_0150-nr_item        = wl_zglt073-nr_item.
            wl_saida_0150-anln1          = wl_zglt073-anln1.
            wl_saida_0150-anln2          = wl_zglt073-anln2.
            wl_saida_0150-invnr	         = wl_zglt073-invnr.
            wl_saida_0150-sernr	         = wl_zglt073-sernr.
            wl_saida_0150-matnr          = wl_zglt073-matnr.
            wl_saida_0150-descr_bens     = wl_zglt073-descr_bens.
            wl_saida_0150-werks          = wl_zglt073-werks.
            wl_saida_0150-kostl          = wl_zglt073-kostl.
            wl_saida_0150-vlr_premio_usd = wl_zglt073-vlr_premio_usd.
            wl_saida_0150-vlr_premio_brl = wl_zglt073-vlr_premio_brl.
            wl_saida_0150-wkurs          = wl_zglt073-wkurs.
            wl_saida_0150-dt_in_vig      = wl_zglt073-dt_in_vig.
            wl_saida_0150-dt_baixa       = wl_zglt073-dt_baixa.
            wl_saida_0150-lote           = wl_zglt073-lote.
            wl_saida_0150-doc_lcto       = wl_zglt073-doc_lcto.
            wl_saida_0150-month_baixa    = wl_zglt073-month_baixa.

          ELSE.

            vlr_premio_usd = ( wl_zglt068-vlr_premio_usd / at_intervalo_vigencia ). "WL_ZGLT050-VIG_ATE+4(2) ).
            vlr_premio_brl = ( wl_zglt068-vlr_premio_brl / at_intervalo_vigencia ). "WL_ZGLT050-VIG_ATE+4(2) ).
**<<<------"164255 - NMS - INI------>>>
            IF wl_cabecalho_0150-seq_tipo NE 17 AND
               wl_cabecalho_0150-seq_tipo NE 21 AND
               wl_cabecalho_0150-seq_tipo NE 22 AND
               wl_cabecalho_0150-seq_tipo NE 29.
**<<<------"164255 - NMS - FIM------>>>
              "(Situações onde ocorrem diferença no total apropriado)
              "Se for ultimo mês de apropriação(YYYYMM ),
              "calcular valor restante à apropriar.
              IF at_index = at_intervalo_vigencia .
                CLEAR: vl_tot_aprop_brl, vl_tot_aprop_usd, vlr_premio_brl, vlr_premio_usd,
                       vl_dif_aprop_brl, vl_dif_aprop_usd.

*             Pega o valor de todas as parcelas na tabela ZGLT073,            / 11.05.2018
*             somar a diferença do total e somar ao valor da última parcela.
                SELECT * FROM zglt073
                  INTO TABLE @DATA(tl_zglt073)
                  WHERE seq_lcto EQ @wl_zglt068-seq_lcto AND nr_item EQ @wl_zglt068-nr_item.

                IF tl_zglt073[] IS NOT INITIAL.
                  CLEAR: soma_parcela_brl, soma_parcela_usd.

                  LOOP AT tl_zglt073 INTO DATA(wl_parcela_zglt073).
                    soma_parcela_brl = wl_parcela_zglt073-vlr_premio_brl + soma_parcela_brl.
                    soma_parcela_usd = wl_parcela_zglt073-vlr_premio_usd + soma_parcela_usd.
                  ENDLOOP.

                ENDIF.

                IF ( soma_parcela_brl NE 0 ) AND ( soma_parcela_usd NE 0 ).

                  vl_dif_aprop_brl = wl_zglt068-vlr_premio_brl - soma_parcela_brl.
                  vl_dif_aprop_usd = wl_zglt068-vlr_premio_usd - soma_parcela_usd.

                  IF ( vl_dif_aprop_brl NE 0 ) OR ( vl_dif_aprop_usd NE 0 ).
                    vlr_premio_brl = vlr_premio_brl + vl_dif_aprop_brl.
                    vlr_premio_usd = vlr_premio_usd + vl_dif_aprop_usd.
                  ENDIF.

                ENDIF.

                " Quando houver apenas 1 parcela, receber o valor total do prêmio.
                IF ( at_index = 1 AND at_intervalo_vigencia = 1 ) AND ( tl_zglt073[] IS INITIAL ).
                  vlr_premio_brl = wl_zglt068-vlr_premio_brl.
                  vlr_premio_usd = wl_zglt068-vlr_premio_usd.
                ENDIF.

*******************************************************************

*              DO AT_INTERVALO_VIGENCIA TIMES.
*                ADD VLR_PREMIO_BRL TO VL_TOT_APROP_BRL.
*                ADD VLR_PREMIO_USD TO VL_TOT_APROP_USD.
*              ENDDO.

*              IF ( VL_TOT_APROP_BRL > 0 ) AND ( VL_TOT_APROP_USD > 0 ).
*
*                VL_DIF_APROP_BRL = WL_ZGLT068-VLR_PREMIO_BRL - VL_TOT_APROP_BRL.
*                VL_DIF_APROP_USD = WL_ZGLT068-VLR_PREMIO_USD - VL_TOT_APROP_USD.
*
*                IF ( VL_DIF_APROP_BRL NE 0 ) OR ( VL_DIF_APROP_USD NE 0 ).
*                  VLR_PREMIO_BRL = VLR_PREMIO_BRL + VL_DIF_APROP_BRL.
*                  VLR_PREMIO_USD = VLR_PREMIO_USD + VL_DIF_APROP_USD.
*                ENDIF.
*
*              ENDIF..
*******************************************************************


              ENDIF.

            ENDIF.   "<<<------"164255 - NMS------->>>
*            IF AT_INDEX = 0.
*              EXIT.
*            ENDIF.

            "Situação onde Bem já foi baixado, e mês é de Aproriação é igual mês da baixa.
            CLEAR: wl_saida_0150-month_baixa.
            IF vl_month_baixa IS NOT INITIAL.

              CLEAR: vl_tot_aprop_usd, vl_tot_aprop_brl.

              SELECT SINGLE SUM( vlr_premio_usd ) SUM( vlr_premio_brl )
                FROM zglt073 INTO ( vl_tot_aprop_usd , vl_tot_aprop_brl )
               WHERE seq_lcto   EQ wl_zglt068-seq_lcto
                 AND nr_item    EQ wl_zglt068-nr_item
               GROUP BY seq_lcto nr_item.

              vlr_premio_usd = wl_zglt068-vlr_premio_usd - ( vl_tot_aprop_usd + vl_vlr_bx_usd ).
              vlr_premio_brl = wl_zglt068-vlr_premio_brl - ( vl_tot_aprop_brl + vl_vlr_bx_brl ).

              IF ( vlr_premio_usd < 0 ) OR ( vlr_premio_brl < 0 ).
                vlr_premio_usd = 0.
                vlr_premio_brl = 0.
              ENDIF.

              wl_saida_0150-month_baixa = 'X'.
            ENDIF.

            taxa_cambio    = ( vlr_premio_brl / vlr_premio_usd ).

            wl_saida_0150-bukrs          = wl_zglt050-bukrs.
            wl_saida_0150-status         = icon_light_out.
            wl_saida_0150-dt_apropr      = at_dt_apropriacao.
            wl_saida_0150-seq_lcto       = wl_zglt068-seq_lcto.
            wl_saida_0150-nro_apolice    = wl_zglt050-nro_apolice.
            wl_saida_0150-nro_parc       = at_index.
            wl_saida_0150-anln1          = wl_zglt068-anln1.
            wl_saida_0150-anln2          = wl_zglt068-anln2.
            wl_saida_0150-invnr	         = wl_zglt068-invnr.
            wl_saida_0150-sernr	         = wl_zglt068-sernr.
            wl_saida_0150-nr_item        = wl_zglt068-nr_item.
            wl_saida_0150-matnr          = wl_zglt068-matnr.
            wl_saida_0150-descr_bens     = wl_zglt068-descr_bens.
            wl_saida_0150-werks          = wl_zglt068-werks.
            wl_saida_0150-kostl          = wl_zglt068-kostl.
*---> 10/06/2023 - Migração S4 - JS
*            wl_saida_0150-vlr_premio_usd = vlr_premio_usd.
*            wl_saida_0150-vlr_premio_brl = vlr_premio_brl.
**<<<------"164255 - NMS - INI------>>>
* Verifica se é a ultima parcela sendo executada.
            IF l_dt_aprop-low(6) EQ wl_zglt050-vig_ate(6).
* Ajuste CBRAND - Inicio
              IF wl_cabecalho_0150-seq_tipo EQ  17 OR
                 wl_cabecalho_0150-seq_tipo EQ  21 OR
                 wl_cabecalho_0150-seq_tipo EQ  22 OR
                 wl_cabecalho_0150-seq_tipo EQ  29.
* Ajuste CBRAND - Fim
* Verifica se há diferença de valor por conta de arrendonamentos ao calcular o valor da parcela de prémio.
*           | Vlr Recalculado| Vlr. anterior|DIF| Cálculo da diferença
                vlr_premio_usd = vlr_premio_usd - ( ( vlr_premio_usd * at_intervalo_vigencia ) - wl_zglt068-vlr_premio_usd ).
                vlr_premio_brl = vlr_premio_brl - ( ( vlr_premio_brl * at_intervalo_vigencia ) - wl_zglt068-vlr_premio_brl ).
              ENDIF.
            ENDIF.
**<<<------"164255 - NMS - FIM------>>>
            wl_saida_0150-vlr_premio_usd = CONV #( vlr_premio_usd ).
            wl_saida_0150-vlr_premio_brl = CONV #( vlr_premio_brl ).
*<--- 10/06/2023 - Migração S4 - JS
            wl_saida_0150-wkurs          = taxa_cambio.
            wl_saida_0150-dt_in_vig      = wl_zglt068-dt_in_vig.
            wl_saida_0150-dt_baixa       = wl_zglt068-dt_baixa.
          ENDIF.

          at_index = at_index + 1.
          APPEND wl_saida_0150 TO gt_saida_0150.
          CLEAR wl_saida_0150.
        ENDIF.

        IF _aprop_final IS NOT INITIAL.
          _aprop_final = ''.
        ELSE.
          PERFORM f_date_increment CHANGING l_dt_aprop-low.
        ENDIF.
      ENDWHILE.
*      ENDDO.

      CLEAR: wl_zglt050, wl_saida_0150, wl_zglt073, soma_parcela_brl, soma_parcela_usd, wl_parcela_zglt073.
    ENDLOOP.

    CALL METHOD obj_alv_0150->refresh_table_display
      EXPORTING
        is_stable = wl_stable.
  ENDMETHOD.                    "Z_SELECIONA_DADOS_0150

  METHOD z_seleciona_dados_0160.

    DATA: gt_067        TYPE TABLE OF zglt067.

    CLEAR: gt_saida_0160[], gt_067[], gt_zglt050[].

    PERFORM f_monta_range USING '0160'.

    SELECT *
      FROM zglt050 INTO TABLE gt_zglt050
     WHERE finalizada EQ ''
       AND bukrs      IN r_bukrs
       AND seq_lcto   IN r_seq_lcto
       AND seq_tipo   IN r_seq_tipo
       AND loekz      EQ ''.

    CHECK gt_zglt050[] IS NOT INITIAL.

    IF wl_cabecalho_0160-competencia IS NOT INITIAL.

      SELECT *
      FROM zglt067 INTO TABLE gt_067
      FOR ALL ENTRIES IN gt_zglt050
     WHERE seq_lcto EQ gt_zglt050-seq_lcto
       AND dt_venc  >= wl_cabecalho_0160-dt_aprop_de
       AND dt_venc  <= wl_cabecalho_0160-dt_aprop_ate.

    ELSE.

      SELECT *
      FROM zglt067 INTO TABLE gt_067
      FOR ALL ENTRIES IN gt_zglt050
     WHERE seq_lcto EQ gt_zglt050-seq_lcto.

    ENDIF.

    SORT gt_067 BY seq_lcto dt_venc.

    LOOP AT gt_067 INTO wl_zglt067.
      CLEAR: wl_saida_0160.

      READ TABLE gt_zglt050 INTO wl_zglt050 WITH KEY seq_lcto = wl_zglt067-seq_lcto.
      CHECK sy-subrc = 0.

      DATA(_ok) = abap_false.
      PERFORM f_check_authority USING wl_zglt050-bukrs
                                      abap_false
                             CHANGING _ok.

      CHECK _ok IS NOT INITIAL.

      IF ( wl_zglt067-lote IS INITIAL ).
        wl_saida_0160-status  = icon_light_out.
      ELSE.
        IF wl_zglt067-dt_lcto_ctb IS INITIAL.
          wl_zglt067-dt_lcto_ctb = wl_zglt067-erdat.
        ENDIF.

        me->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_zglt067-doc_lcto
                                            i_ano_lcto = wl_zglt067-dt_lcto_ctb(4)
                                  IMPORTING e_zibchv   = wl_zib_chave
                                            e_ziberr   = wl_zib_erro ).

        IF ( wl_zib_chave IS NOT INITIAL ).
          wl_saida_0160-status       = icon_green_light .
          wl_saida_0160-doc_contabil = wl_zib_chave-belnr.
          wl_saida_0160-dt_lcto_ctb  = wl_zglt067-dt_lcto_ctb.
        ELSEIF ( wl_zib_erro IS NOT INITIAL ).
          wl_saida_0160-status       = icon_red_light.
        ELSE.
          wl_saida_0160-status       = icon_yellow_light.
        ENDIF.
      ENDIF.

      wl_saida_0160-seq_lcto       = wl_zglt067-seq_lcto.
      wl_saida_0160-bukrs          = wl_zglt050-bukrs.
      wl_saida_0160-filial         = wl_zglt067-werks.
      wl_saida_0160-nro_parc       = wl_zglt067-nro_parc.
      wl_saida_0160-filial         = wl_zglt067-werks.
      wl_saida_0160-taxa_cambio    = wl_zglt067-wkurs.
      wl_saida_0160-vlr_premio_usd = wl_zglt067-vlr_premio_usd.
      wl_saida_0160-vlr_premio_brl = wl_zglt067-vlr_premio_brl.
      wl_saida_0160-dt_venc        = wl_zglt067-dt_venc.
      wl_saida_0160-pais_pgto      = wl_zglt067-banks.
      wl_saida_0160-forma_pgto     = wl_zglt067-zlsch.
      wl_saida_0160-bco_empresa    = wl_zglt067-hbkid.
      wl_saida_0160-bco_parceiro   = wl_zglt067-bvtyp.
      wl_saida_0160-bloq_pgto      = wl_zglt067-zlspr.
      wl_saida_0160-cod_barras     = wl_zglt067-cod_barras.
      wl_saida_0160-lote           = wl_zglt067-lote.
      wl_saida_0160-nro_documento  = wl_zglt067-doc_lcto.

      APPEND wl_saida_0160 TO gt_saida_0160.
    ENDLOOP.

    IF ( obj_alv_0160 IS NOT INITIAL ).
      CALL METHOD obj_alv_0160->refresh_table_display
        EXPORTING
          is_stable = wl_stable.
    ENDIF.

  ENDMETHOD. "Z_SELECIONA_DADOS_0160.

  METHOD z_seleciona_dados_0170.

    DATA: r_gerar_lote             TYPE REF TO zcl_gerar_lote,
          l_dt_vigen               LIKE LINE OF r_date,
          l_dt_aprop               LIKE LINE OF r_date,
          vlr_premio_usd           TYPE dmbtr,
          vlr_premio_brl           TYPE dmbtr,
          vl_tot_aprop_usd         TYPE dmbtr,
          vl_tot_aprop_brl         TYPE dmbtr,
          vl_dif_aprop_usd         TYPE dmbtr,
          vl_dif_aprop_brl         TYPE dmbtr,
          taxa_cambio              TYPE wkurs,
          at_index                 TYPE i,
          at_intervalo_apropriacao TYPE tfmatage,
          at_intervalo_vigencia    TYPE tfmatage,
          at_dt_apropriacao        TYPE sy-datum,
          at_dif_vigen             TYPE tfmatage,
          at_objkey                TYPE char20,
          vl_nr_item               TYPE zglt073-nr_item,
          vl_valida_baixa          TYPE c,
          vl_month_baixa           TYPE c,
          vl_vlr_bx_usd            TYPE zglt068-vlr_premio_usd,
          vl_vlr_bx_brl            TYPE zglt068-vlr_premio_brl,
          gt_zglt073_aux           TYPE TABLE OF zglt073,
          wl_zglt073_aux           TYPE zglt073,
          gt_050_bx                TYPE TABLE OF zglt050,
          gt_068_bx                TYPE TABLE OF zglt068.

    CLEAR: gt_saida_0170[], gt_zglt068[], gt_zglt050[], gt_zglt031[], gt_zglt032[],
           gt_zglt064[], gt_t001[], gt_zglt073_ctb[], gt_zib_chv[], gt_050_bx[], gt_068_bx[].

    PERFORM f_monta_range USING '0170'.

    SELECT *
      FROM zglt050 INTO TABLE gt_zglt050
     WHERE vig_de     >= wl_cabecalho_0170-dt_aprop_de
       AND vig_ate    <= wl_cabecalho_0170-dt_aprop_ate
       AND finalizada EQ ''
       AND bukrs      IN r_bukrs
       AND seq_lcto   IN r_seq_lcto
       AND seq_tipo   IN r_seq_tipo
       AND loekz      EQ ''.

    DELETE gt_zglt050 WHERE tp_opr EQ 'B'. "Baixa.

    CHECK gt_zglt050[] IS NOT INITIAL.


*----------------------------------------------------------*
*  Seleção Apolices de Baixa e seus bens
*----------------------------------------------------------*
    SELECT *
      FROM zglt050 INTO TABLE gt_050_bx
       FOR ALL ENTRIES IN gt_zglt050
     WHERE tp_opr       EQ 'B'
       AND ref_seq_lcto EQ gt_zglt050-seq_lcto
       AND loekz        EQ ''.

    IF gt_050_bx[] IS NOT INITIAL.
      SELECT *
        FROM zglt068 INTO TABLE gt_068_bx
         FOR ALL ENTRIES IN gt_050_bx
       WHERE seq_lcto EQ gt_050_bx-seq_lcto.
    ENDIF.

    SELECT *
      FROM zglt068
      INTO TABLE gt_zglt068
       FOR ALL ENTRIES IN gt_zglt050
     WHERE seq_lcto = gt_zglt050-seq_lcto
       AND dt_baixa = '00000000'.

    CHECK gt_zglt068[] IS NOT INITIAL.

    SELECT *
      FROM zglt073 INTO CORRESPONDING FIELDS OF TABLE gt_zglt073_ctb
       FOR ALL ENTRIES IN gt_zglt050
     WHERE seq_lcto EQ gt_zglt050-seq_lcto.

    CHECK gt_zglt073_ctb[] IS NOT INITIAL.

    LOOP AT gt_zglt073_ctb INTO DATA(_wl_zglt073_ctb).
      DATA(_tabix) = sy-tabix.
      CHECK _wl_zglt073_ctb-doc_lcto IS NOT INITIAL.
      CONCATENATE 'ZGL17' _wl_zglt073_ctb-doc_lcto _wl_zglt073_ctb-dt_apropr(4)
             INTO _wl_zglt073_ctb-obj_key.
      MODIFY gt_zglt073_ctb FROM _wl_zglt073_ctb INDEX _tabix.
    ENDLOOP.

    DELETE gt_zglt073_ctb WHERE obj_key IS INITIAL.
    CHECK gt_zglt073_ctb[] IS NOT INITIAL.

    SELECT *
      FROM zib_contabil_chv INTO TABLE gt_zib_chv
       FOR ALL ENTRIES IN gt_zglt073_ctb
     WHERE obj_key EQ gt_zglt073_ctb-obj_key.

    CHECK gt_zib_chv[] IS NOT INITIAL.

    SORT gt_zib_chv BY obj_key.

    LOOP AT gt_zglt073_ctb INTO _wl_zglt073_ctb.
      _tabix = sy-tabix.
      READ TABLE gt_zib_chv INTO DATA(_wl_zib_chv) WITH KEY obj_key = _wl_zglt073_ctb-obj_key BINARY SEARCH.

      IF ( sy-subrc NE 0 ) OR ( _wl_zib_chv-belnr IS INITIAL ).
        DELETE gt_zglt073_ctb INDEX _tabix.
      ENDIF.
    ENDLOOP.

    SELECT *
      FROM t001 INTO TABLE gt_t001
       FOR ALL ENTRIES IN gt_zglt050
     WHERE bukrs = gt_zglt050-bukrs.

    CLEAR: wl_cabecalho_0170-descr_bukrs.
    IF lines( gt_t001[] ) = 1.
      READ TABLE gt_t001 INTO wl_t001 INDEX 1.
      MOVE wl_t001-butxt TO wl_cabecalho_0170-descr_bukrs.
    ENDIF.

    SELECT *
      FROM zglt064 INTO TABLE gt_zglt064
       FOR ALL ENTRIES IN gt_zglt050
     WHERE seq_tipo EQ gt_zglt050-seq_tipo.

    CLEAR: wl_cabecalho_0170-descr_tipo.
    IF lines( gt_zglt064[] ) = 1.
      READ TABLE gt_zglt064 INTO wl_zglt064 INDEX 1.
      MOVE wl_zglt064-descr TO wl_cabecalho_0170-descr_tipo.
    ENDIF.

    IF gt_zglt064[] IS NOT INITIAL.
      SELECT *
        FROM zglt031 INTO TABLE gt_zglt031
         FOR ALL ENTRIES IN gt_zglt064
       WHERE tp_lcto EQ gt_zglt064-tp_lcto_aprop.

      SELECT *
        FROM zglt032 INTO TABLE gt_zglt032
         FOR ALL ENTRIES IN gt_zglt064
       WHERE tp_lcto EQ gt_zglt064-tp_lcto_aprop.
    ENDIF.

    SORT gt_zglt068 BY seq_lcto.
    LOOP AT gt_zglt068 INTO wl_zglt068.
      CLEAR: wl_zglt050, wl_t001, wl_zglt064, wl_saida_0170.

      READ TABLE gt_zglt050 INTO wl_zglt050 WITH KEY seq_lcto = wl_zglt068-seq_lcto.

      CHECK sy-subrc = 0.

      DATA(_ok) = abap_false.
      PERFORM f_check_authority USING wl_zglt050-bukrs
                                      abap_false
                             CHANGING _ok.

      CHECK _ok IS NOT INITIAL.

      LOOP AT gt_zglt073_ctb INTO _wl_zglt073_ctb WHERE seq_lcto = wl_zglt068-seq_lcto
                                                  AND nr_item  = wl_zglt068-nr_item.
        ADD _wl_zglt073_ctb-vlr_premio_usd TO wl_saida_0170-aprop_usd.
        ADD _wl_zglt073_ctb-vlr_premio_brl TO wl_saida_0170-aprop_brl.
        ADD 1 TO wl_saida_0170-nro_aprop.
      ENDLOOP.

      LOOP AT gt_050_bx INTO DATA(_wl_050_bx) WHERE ref_seq_lcto = wl_zglt068-seq_lcto.
        LOOP AT gt_068_bx INTO DATA(_wl_068_bx) WHERE seq_lcto = _wl_050_bx-seq_lcto
                                                  AND nr_item  = wl_zglt068-nr_item.


          ADD _wl_068_bx-vlr_premio_usd TO wl_saida_0170-vlr_bx_usd.
          ADD _wl_068_bx-vlr_premio_brl TO wl_saida_0170-vlr_bx_brl.
          wl_saida_0170-dt_baixa   = _wl_068_bx-dt_baixa.
        ENDLOOP.
      ENDLOOP.

      wl_saida_0170-bukrs             = wl_zglt050-bukrs.
      wl_saida_0170-status            = icon_light_out.
      wl_saida_0170-seq_lcto          = wl_zglt068-seq_lcto.
      wl_saida_0170-nro_apolice       = wl_zglt050-nro_apolice.
      wl_saida_0170-anln1             = wl_zglt068-anln1.
      wl_saida_0170-anln2             = wl_zglt068-anln2.
      wl_saida_0170-invnr	            = wl_zglt068-invnr.
      wl_saida_0170-sernr	            = wl_zglt068-sernr.
      wl_saida_0170-nr_item           = wl_zglt068-nr_item.
      wl_saida_0170-matnr             = wl_zglt068-matnr.
      wl_saida_0170-descr_bens        = wl_zglt068-descr_bens.
      wl_saida_0170-werks             = wl_zglt068-werks.
      wl_saida_0170-kostl             = wl_zglt068-kostl.
      wl_saida_0170-vlr_premio_usd    = wl_zglt068-vlr_premio_usd.
      wl_saida_0170-vlr_premio_brl    = wl_zglt068-vlr_premio_brl.
      wl_saida_0170-lote_ajus         = wl_zglt068-lote_ajus.
      wl_saida_0170-doc_lcto_ajus     = wl_zglt068-doc_lcto_ajus.

      IF ( wl_zglt068-dt_lcto_ctb_ajus IS NOT INITIAL ) AND ( wl_zglt068-dt_lcto_ctb_ajus NE '' ).
        wl_saida_0170-dt_lcto_ctb_ajus = wl_zglt068-dt_lcto_ctb_ajus.
      ELSE.
        wl_saida_0170-dt_lcto_ctb_ajus  = sy-datum.
      ENDIF.

      IF wl_zglt068-vlr_premio_usd > 0.
        wl_saida_0170-wkurs        = ( wl_zglt068-vlr_premio_brl / wl_zglt068-vlr_premio_usd ).
      ENDIF.

      wl_saida_0170-saldo_usd      = wl_saida_0170-vlr_premio_usd - ( wl_saida_0170-aprop_usd + wl_saida_0170-vlr_bx_usd ).
      wl_saida_0170-saldo_brl      = wl_saida_0170-vlr_premio_brl - ( wl_saida_0170-aprop_brl + wl_saida_0170-vlr_bx_brl ).

      IF ( ( wl_saida_0170-aprop_usd + wl_saida_0170-vlr_bx_usd ) > wl_saida_0170-vlr_premio_usd ) OR
         ( ( wl_saida_0170-aprop_brl + wl_saida_0170-vlr_bx_brl ) > wl_saida_0170-vlr_premio_brl ).
        wl_saida_0170-vlr_ajuste_usd = ( wl_saida_0170-aprop_usd + wl_saida_0170-vlr_bx_usd ) - wl_saida_0170-vlr_premio_usd.
        wl_saida_0170-vlr_ajuste_brl = ( wl_saida_0170-aprop_brl + wl_saida_0170-vlr_bx_brl ) - wl_saida_0170-vlr_premio_brl.
      ENDIF.

      "Check se valor calculado para ajuste é maior que o valor já apropriado.
      IF ( wl_saida_0170-vlr_ajuste_brl > wl_saida_0170-aprop_brl ) OR
         ( wl_saida_0170-vlr_ajuste_usd > wl_saida_0170-aprop_usd ).
* Início - RMNI - CS1023310 - Ajuste Seq. lançamento 1982 e 1970 - 08.12.2022
*        wl_saida_0170-vlr_ajuste_usd = wl_saida_0170-aprop_usd.
*        wl_saida_0170-vlr_ajuste_brl = wl_saida_0170-aprop_brl.

        wl_saida_0170-vlr_ajuste_usd = wl_saida_0170-saldo_usd * -1.
        wl_saida_0170-vlr_ajuste_brl = wl_saida_0170-saldo_brl * -1.
* Fim - RMNI - CS1023310 - Ajuste Seq. lançamento 1982 e 1970 - 08.12.2022
      ENDIF.

      IF wl_saida_0170-vlr_premio_brl > 0.
        wl_saida_0170-perc_aprop = ( ( wl_saida_0170-aprop_brl + wl_saida_0170-vlr_bx_brl ) / wl_saida_0170-vlr_premio_brl ) * 100.
      ENDIF.

*      CHECK WL_SAIDA_0170-PERC_APROP > 100.

      IF wl_saida_0170-perc_aprop > 100.

        CHECK wl_saida_0170-perc_aprop > 100.

      ELSE.

        CHECK wl_saida_0170-vlr_bx_brl <> '0.00'.

        IF ( ( wl_saida_0170-aprop_usd + wl_saida_0170-vlr_bx_usd ) < wl_saida_0170-vlr_premio_usd ) OR
           ( ( wl_saida_0170-aprop_brl + wl_saida_0170-vlr_bx_brl ) < wl_saida_0170-vlr_premio_brl ).
          wl_saida_0170-vlr_ajuste_usd = ( wl_saida_0170-aprop_usd + wl_saida_0170-vlr_bx_usd ) - wl_saida_0170-vlr_premio_usd.
          wl_saida_0170-vlr_ajuste_brl = ( wl_saida_0170-aprop_brl + wl_saida_0170-vlr_bx_brl ) - wl_saida_0170-vlr_premio_brl.
        ENDIF.

        "Check se valor calculado para ajuste é maior que o valor já apropriado.
        IF ( wl_saida_0170-vlr_ajuste_brl > wl_saida_0170-aprop_brl ) OR
           ( wl_saida_0170-vlr_ajuste_usd > wl_saida_0170-aprop_usd ).
* Início - RMNI - CS1023310 - Ajuste Seq. lançamento 1982 e 1970 - 08.12.2022
*          wl_saida_0170-vlr_ajuste_usd = wl_saida_0170-aprop_usd.
*          wl_saida_0170-vlr_ajuste_brl = wl_saida_0170-aprop_brl.

          wl_saida_0170-vlr_ajuste_usd = wl_saida_0170-saldo_usd * -1.
          wl_saida_0170-vlr_ajuste_brl = wl_saida_0170-saldo_brl * -1.
* Fim - RMNI - CS1023310 - Ajuste Seq. lançamento 1982 e 1970 - 08.12.2022
        ENDIF.

      ENDIF.

      IF ( wl_zglt068-lote_ajus IS INITIAL ).
        wl_saida_0170-status  = icon_light_out.
      ELSE.
        me->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_zglt068-doc_lcto_ajus
                                            i_ano_lcto = wl_zglt068-dt_lcto_ctb_ajus(4)
                                  IMPORTING e_zibchv   = wl_zib_chave
                                            e_ziberr   = wl_zib_erro ).

        IF ( wl_zib_chave IS NOT INITIAL ).
          wl_saida_0170-status           = icon_green_light .
          wl_saida_0170-doc_contabil     = wl_zib_chave-belnr.
        ELSEIF ( wl_zib_erro IS NOT INITIAL ).
          wl_saida_0170-status       = icon_red_light.
        ELSE.
          wl_saida_0170-status       = icon_yellow_light.
        ENDIF.
      ENDIF.

      APPEND wl_saida_0170 TO gt_saida_0170.

    ENDLOOP.

    CALL METHOD obj_alv_0170->refresh_table_display
      EXPORTING
        is_stable = wl_stable.
  ENDMETHOD.                    "Z_SELECIONA_DADOS_0170

  METHOD z_check_baixa.

    DATA: vl_dt_baixa    TYPE zglt068-dt_baixa.
    DATA: gt_zglt068_aux TYPE TABLE OF zglt068,
          wl_zglt068_aux TYPE zglt068,
          wl_zglt050_aux TYPE zglt050.

    CLEAR: e_last_month, e_vlr_bx_usd, e_vlr_bx_brl.
    e_valida = 'X'.

    SELECT a~seq_lcto a~nr_item a~anln1 a~anln2 a~matnr a~invnr a~sernr a~descr_bens
           a~werks a~kostl a~bland a~wkurs a~vlr_premio_usd a~vlr_premio_brl a~vlr_aj_prem_usd
           a~vlr_aj_prem_brl a~dt_in_vig a~dt_baixa a~clau_benef a~banco
           a~vlr_risco_usd a~vlr_risco_brl
      INTO CORRESPONDING FIELDS OF TABLE gt_zglt068_aux
      FROM zglt068 AS a INNER JOIN zglt050 AS b ON a~seq_lcto = b~seq_lcto
     WHERE b~tp_opr       = 'B' "Baixa
       AND b~loekz        = ''
       AND b~ref_seq_lcto = i_wl_068-seq_lcto
       AND a~nr_item      = i_wl_068-nr_item.

    SORT gt_zglt068_aux BY seq_lcto nr_item.
    DELETE ADJACENT DUPLICATES FROM gt_zglt068_aux COMPARING seq_lcto nr_item.

    LOOP AT gt_zglt068_aux INTO wl_zglt068_aux WHERE dt_baixa IS NOT INITIAL.

      CLEAR: vl_dt_baixa.

      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = wl_zglt068_aux-dt_baixa
        IMPORTING
          last_day_of_month = vl_dt_baixa.

      CHECK vl_dt_baixa IS NOT INITIAL.

      "Apropriar até o mes em que foi feito a baixa do Bem
      IF wl_cabecalho_0150-dt_aprop_ate > vl_dt_baixa.
        CLEAR: e_valida.
      ENDIF.

      IF wl_cabecalho_0150-dt_aprop_ate(6) EQ vl_dt_baixa(6).
        e_last_month = 'X'.
        e_vlr_bx_usd = wl_zglt068_aux-vlr_premio_usd.
        e_vlr_bx_brl = wl_zglt068_aux-vlr_premio_brl.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD z_check_ctb_apolice.

    e_valida = 'X'.

    CHECK i_wl_050-dt_criacao >= c_dt_corte_aprov.

    CHECK i_wl_050-tp_opr NE 'B'. "B = Baixa

    IF i_wl_050-belnr IS NOT INITIAL.
      SELECT SINGLE *
        FROM bkpf INTO @DATA(wl_bkpf)
       WHERE bukrs = @i_wl_050-bukrs
         AND belnr = @i_wl_050-belnr.
    ENDIF.

    IF ( wl_bkpf IS INITIAL ) OR ( wl_bkpf-stblg IS NOT INITIAL ). "Lançamento Estornado
      CLEAR: e_valida.
      MESSAGE s836(sd) WITH TEXT-e56 DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zglt034 INTO @DATA(wl_034)
     WHERE lote = @i_wl_050-lote.

    IF ( sy-subrc NE 0 ) OR ( wl_034-status_lote NE 'A' ).
      CLEAR: e_valida.
      MESSAGE s836(sd) WITH TEXT-e59 DISPLAY LIKE 'S'.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD z_ajusta_vlr_baixa.

    DATA: wl_068_aux    TYPE zglt068,
          wl_073_aux    TYPE zglt073,
          gt_zglt073_bx TYPE TABLE OF zglt073,
          vlr_aprop_brl TYPE zglt073-vlr_premio_brl,
          vlr_aprop_usd TYPE zglt073-vlr_premio_usd.


    FIELD-SYMBOLS: <saida_0130> TYPE ty_saida_0130.

    DATA: vl_vlr_tot_usd TYPE zglt068-vlr_premio_usd,
          vl_vlr_tot_brl TYPE zglt068-vlr_premio_brl,
          vl_vlr_dif_usd TYPE zglt068-vlr_premio_usd,
          vl_vlr_dif_brl TYPE zglt068-vlr_premio_brl.

    CLEAR: e_error,
           vl_vlr_tot_usd, vl_vlr_tot_brl,
           vl_vlr_dif_usd, vl_vlr_dif_brl.

    "Calcula totalizadores
    LOOP AT gt_saida_0130 ASSIGNING <saida_0130>.
      SELECT SINGLE a~seq_lcto a~nr_item a~anln1 a~anln2 a~matnr a~invnr a~sernr a~descr_bens
             a~werks a~kostl a~bland a~wkurs a~vlr_premio_usd a~vlr_premio_brl a~vlr_aj_prem_usd
             a~vlr_aj_prem_brl a~dt_in_vig a~dt_baixa a~clau_benef a~banco
             a~vlr_risco_usd a~vlr_risco_brl
        INTO CORRESPONDING FIELDS OF wl_068_aux
        FROM zglt068 AS a INNER JOIN zglt050 AS b ON a~seq_lcto = b~seq_lcto
       WHERE b~seq_lcto  = wl_cabecalho_0110-ref_seq_lcto
         AND b~loekz     = ''
         AND a~nr_item   = <saida_0130>-nr_item.

      IF sy-subrc NE 0.
        e_error = 'X'.
        MESSAGE s836(sd) WITH TEXT-e51 <saida_0130>-nr_item TEXT-e52 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      "Verifica se Item referenciado na apolice de Baixa, já teve apropriação no mês da baixa.
      "Caso tenha, não deixar alterar nenhum item da apolice, devido ao valor rateado do
      "Contas a receber em cima do mesmo.
      SELECT SINGLE *
        FROM zglt073 INTO wl_073_aux
       WHERE seq_lcto    = wl_cabecalho_0110-ref_seq_lcto
         AND nr_item     = <saida_0130>-nr_item
         AND month_baixa NE ''. "Apropriação mês baixa

      IF sy-subrc = 0.
        e_error = 'X'.
        MESSAGE s836(sd) WITH TEXT-e53 <saida_0130>-nr_item TEXT-e54 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      <saida_0130>-vlr_premio_usd = wl_068_aux-vlr_premio_usd.
      <saida_0130>-vlr_premio_brl = wl_068_aux-vlr_premio_brl.

      ADD <saida_0130>-vlr_premio_usd TO vl_vlr_tot_usd.
      ADD <saida_0130>-vlr_premio_brl TO vl_vlr_tot_brl.

      CLEAR: <saida_0130>-vlr_aj_prem_usd,<saida_0130>-vlr_aj_prem_brl.
    ENDLOOP.

    CHECK vl_vlr_tot_brl > 0.

    "Check Valor Rateado
    DATA(_erro_rateio) = ''.
    CLEAR: wl_068_aux.
    LOOP AT gt_saida_0130 ASSIGNING <saida_0130>.
      <saida_0130>-prop_baixa = <saida_0130>-vlr_premio_brl / vl_vlr_tot_brl. "#EC CI_FLDEXT_OK[2610650]
      " RMNI Ajustes casas decimais
      DATA(_vlr_rateio_usd)   = <saida_0130>-prop_baixa * wl_cabecalho_0110-vlr_premio_usd.
      DATA(_vlr_rateio_brl)   = <saida_0130>-prop_baixa * wl_cabecalho_0110-vlr_premio_brl.

      " RMNI Ajustes baixa
*      IF ( _vlr_rateio_usd >= <saida_0130>-vlr_premio_usd ) OR
*         ( _vlr_rateio_brl >= <saida_0130>-vlr_premio_brl ) .
*        _erro_rateio = 'X'.
*        MOVE-CORRESPONDING <saida_0130> TO wl_068_aux.
*        EXIT.
*      ENDIF.

      "Check se irá exceder o valor do bem com o valor da apolice da baixa.
      CLEAR: gt_zglt073_bx[], vlr_aprop_brl, vlr_aprop_usd.
      SELECT *
        FROM zglt073 INTO TABLE gt_zglt073_bx
       WHERE seq_lcto EQ wl_cabecalho_0110-ref_seq_lcto
         AND nr_item  EQ <saida_0130>-nr_item.

      LOOP AT gt_zglt073_bx INTO DATA(_wl_zglt073_bx) WHERE seq_lcto = wl_cabecalho_0110-ref_seq_lcto
                                                        AND nr_item  = <saida_0130>-nr_item.
        ADD _wl_zglt073_bx-vlr_premio_usd TO vlr_aprop_usd.
        ADD _wl_zglt073_bx-vlr_premio_brl TO vlr_aprop_brl.
      ENDLOOP.

      IF ( _vlr_rateio_brl + vlr_aprop_brl ) > <saida_0130>-vlr_premio_brl.
        MESSAGE |Valor do bem referente ao Item: { <saida_0130>-nr_item }, foi excedido. Deverá ser realizado um ajuste de Apropriação! | TYPE 'I'.
      ENDIF.

    ENDLOOP.

    IF _erro_rateio IS NOT INITIAL.
      e_error = 'X'.
      MESSAGE s836(sd) WITH TEXT-e67 wl_068_aux-nr_item DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT gt_saida_0130 ASSIGNING <saida_0130>.
      <saida_0130>-prop_baixa = <saida_0130>-vlr_premio_brl / vl_vlr_tot_brl. "#EC CI_FLDEXT_OK[2610650]
      <saida_0130>-vlr_premio_usd = <saida_0130>-prop_baixa * wl_cabecalho_0110-vlr_premio_usd.
      <saida_0130>-vlr_premio_brl = <saida_0130>-prop_baixa * wl_cabecalho_0110-vlr_premio_brl.
    ENDLOOP.

    CLEAR: vl_vlr_tot_usd, vl_vlr_tot_brl,
           vl_vlr_dif_usd, vl_vlr_dif_brl.

    "Valida se ocorrei diferença no rateio.
    LOOP AT gt_saida_0130 ASSIGNING <saida_0130>.
      ADD <saida_0130>-vlr_premio_usd TO vl_vlr_tot_usd.
      ADD <saida_0130>-vlr_premio_brl TO vl_vlr_tot_brl.
    ENDLOOP.

    vl_vlr_dif_usd =  wl_cabecalho_0110-vlr_premio_usd - vl_vlr_tot_usd.
    vl_vlr_dif_brl =  wl_cabecalho_0110-vlr_premio_brl - vl_vlr_tot_brl.

    IF ( vl_vlr_dif_usd NE 0 ) OR ( vl_vlr_dif_brl NE 0 ).
      READ TABLE gt_saida_0130 ASSIGNING <saida_0130> INDEX 1.
      <saida_0130>-vlr_premio_usd = <saida_0130>-vlr_premio_usd + vl_vlr_dif_usd.
      <saida_0130>-vlr_premio_brl = <saida_0130>-vlr_premio_brl + vl_vlr_dif_brl.
    ENDIF.

  ENDMETHOD.

  METHOD z_retorna_status_zib.
    DATA v_objkey    TYPE char20.

    CLEAR: e_zibchv, e_ziberr.
*    CONCATENATE 'ZGL17' I_DOC_LCTO I_ANO_LCTO INTO V_OBJKEY.

* "// US-164255 WBARBOSA 30/072025
    SELECT SINGLE gjahr
      FROM zglt035
      INTO @DATA(lv_gjahr)
     WHERE bukrs IN @r_bukrs
       AND doc_lcto EQ @i_doc_lcto.

    CONCATENATE 'ZGL17' i_doc_lcto lv_gjahr INTO v_objkey.
* "// US-164255 WBARBOSA 30/072025

    SELECT SINGLE *
      FROM zib_contabil_chv
      INTO e_zibchv
     WHERE obj_key = v_objkey.

    IF ( sy-subrc IS NOT INITIAL ).

      SELECT SINGLE *
        FROM zib_contabil_err
        INTO e_ziberr
       WHERE obj_key = v_objkey.
    ENDIF.

  ENDMETHOD.                    "z_retorna_status_zib

  METHOD z_number_get_next.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'Z_NUM_SEGU'
        quantity    = '1'
      IMPORTING
        number      = wl_cabecalho_0110-seq_lcto.

  ENDMETHOD.                    "Z_NUMBER_GET_NEXT

  METHOD z_ultimo_dia_mes.

  ENDMETHOD.                    "z_ultimo_dia_mes

  METHOD z_calcula_intervalo_data.
    DATA:
      v_data_inicio_apropriacao  TYPE sy-datum,
      v_data_fim_apropriacao     TYPE sy-datum,
      "
      v_data_inicio_apropriacao2 TYPE sy-datum,
      v_data_fim_apropriacao2    TYPE sy-datum,
      "
      v_data_inicio_vigencia     TYPE sy-datum,
      v_data_fim_vigencia        TYPE sy-datum,
      v_competencia              TYPE c LENGTH 6,
      v_comp_ini                 TYPE c LENGTH 6,
      v_comp_fim                 TYPE c LENGTH 6.

    v_data_inicio_apropriacao = i_date1.
    v_data_fim_apropriacao    = i_date2.
    v_data_inicio_vigencia    = i_date3.
    v_data_fim_vigencia       = i_date4.

*  Calcular diferença entre a data inicial/final digitada.

    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from    = v_data_inicio_apropriacao
        i_date_to      = v_data_fim_apropriacao
        i_flg_separate = ' '
      IMPORTING
        e_months       = at_months.

    IF ( at_months IS INITIAL ).
      e_intervalo_apropriacao = 1.
    ELSE.
      e_intervalo_apropriacao = at_months.
    ENDIF.

*  Calcular diferença entre a data inicial/final de vigência do cadastro.
    "ALRS
**<<<------"164255 - NMS - INI------>>>
*    CONCATENATE v_data_fim_vigencia+4(2)  v_data_fim_vigencia+0(4)  INTO v_competencia.
    CONCATENATE v_data_fim_vigencia(4)  v_data_fim_vigencia+4(2)  INTO v_competencia.
**<<<------"164255 - NMS - FIM------>>>
    PERFORM valida_competencia USING v_competencia
                                  CHANGING return_status
                                           v_data_inicio_apropriacao2
                                           v_data_fim_apropriacao2.

    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from    = v_data_inicio_vigencia
        i_date_to      = v_data_inicio_apropriacao2 "V_DATA_FIM_VIGENCIA
        i_flg_separate = ' '
      IMPORTING
        e_months       = me->at_months.

    e_intervalo_vigencia = me->at_months.

    IF ( at_months IS INITIAL ).
      IF ( v_data_inicio_vigencia+6(2) <= 15 ).
        e_intervalo_vigencia = 1.
      ELSE.
*        e_intervalo_vigencia = 0. "ALRS
        CLEAR: v_comp_ini, v_comp_fim.
        v_comp_ini = v_data_inicio_vigencia+0(6).
        v_comp_fim = v_data_fim_vigencia+0(6).
*&------------------------------------------Ajuste referente chamado CS2023000220 / AOENNING. EQUALIZAÇÃO ECC X HANA - SMC
        IF v_comp_ini NE v_comp_fim.
          e_intervalo_vigencia = 0. "ALRS
        ELSE.
          e_intervalo_vigencia = 1.
        ENDIF.
*&------------------------------------------Ajuste referente chamado CS2023000220 / AOENNING. EQUALIZAÇÃO ECC X HANA - SMC
      ENDIF.
    ELSE.
      e_intervalo_vigencia = me->at_months.

      IF ( v_data_inicio_vigencia+6(2) <= 15 ).
        e_intervalo_vigencia = at_months + 1.
      ENDIF.

    ENDIF.

*  Calcular intervalo entre a data inicial digitada, e a data inicial da vigência do cadastro,
*  para assim saber qual sera a parcela da sequência, exemplo:
*  Data inicial de vigência = 01.01.2015 |
*  Data inicial digitada    = 01.02.2015 -- > Diferença entre as datas é de 1 mês
*  Logo, começamos na parcela 2.
*  -
*  Abaixo, fazemos exatamente esta tratativa, caso não houver diferença, começamos
*  na parcela 1.

*    IF V_DATA_INICIO_VIGENCIA+4(2) EQ V_DATA_INICIO_APROPRIACAO+4(2) AND V_DATA_INICIO_VIGENCIA+6(2) > 15.
*
*      E_INTERVALO_PARCELAS = 0.
*
*    ELSE.

    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from    = v_data_inicio_vigencia
        i_date_to      = v_data_inicio_apropriacao
        i_flg_separate = ' '
      IMPORTING
        e_months       = me->at_months.

    IF ( at_months IS INITIAL ).
      IF ( v_data_inicio_vigencia+6(2) <= 15 ).
        e_intervalo_parcelas = 1.
      ELSE.
*        e_intervalo_parcelas = 0. "ALRS
*      &------------------------------------------Ajuste referente chamado CS2023000220 / AOENNING. EQUALIZAÇÃO ECC X HANA - SMC
        IF v_comp_ini IS NOT INITIAL AND v_comp_fim IS NOT INITIAL.

          IF v_comp_ini NE v_comp_fim.
            e_intervalo_parcelas = 0. "ALRS
          ELSE.
            e_intervalo_parcelas = 1. "ALRS
          ENDIF.
        ELSE.
**<<<------"164255 - NMS - INI------>>>
          IF ( wl_cabecalho_0150-seq_tipo EQ 17   OR
               wl_cabecalho_0150-seq_tipo EQ 29   OR
               wl_cabecalho_0150-seq_tipo EQ 21   OR
               wl_cabecalho_0150-seq_tipo EQ 22 ) AND
               wl_cabecalho_0150-competencia LT wl_cabecalho_0150-competenci2.
            e_intervalo_parcelas = 1.

          ELSE.
**<<<------"164255 - NMS - FIM------>>>
            e_intervalo_parcelas = 0. "ALRS
**<<<------"164255 - NMS - INI------>>>
          ENDIF.
**<<<------"164255 - NMS - FIM------>>>
        ENDIF.
*&------------------------------------------Ajuste referente chamado CS2023000220 / AOENNING. EQUALIZAÇÃO ECC X HANA - SMC
      ENDIF.
    ELSE.
*      IF ( V_DATA_INICIO_VIGENCIA+6(2) > 15 ).
      e_intervalo_parcelas = me->at_months.
*      ELSE.
*        E_INTERVALO_PARCELAS = AT_MONTHS + 1.
*      ENDIF.

      IF ( v_data_inicio_vigencia+6(2) <= 15 ).
        e_intervalo_parcelas = at_months + 1.
      ENDIF.

    ENDIF.

*    ENDIF.

  ENDMETHOD.                    "z_calcula_intervalo_data

  METHOD checar_dia_util.

*   Verifica se é feriado e adiciona um dia na data até que
*   seja um dia útil

    "USER STORY 158527 - MMSILVA - 17.01.2025 - Inicio
    DATA: lv_hol_cal       TYPE scal-hcalid,
          lv_fac_cal       TYPE scal-fcalid,
          lv_nome_processo TYPE ze_nomep.

    lv_nome_processo = sy-tcode.

    zcl_calendario=>get_calendario(
      EXPORTING
        i_bukrs            = wl_cabecalho_0110-bukrs
*       I_TIPO_PROCESSO    = 'T'
*       I_NOME_PROCESSO    = lv_nome_processo
      IMPORTING
        e_holiday_calendar = lv_hol_cal
        e_factory_calendar = lv_fac_cal ).

    IF lv_hol_cal IS INITIAL.
      lv_hol_cal = 'BR'.
    ENDIF.

    IF lv_fac_cal IS INITIAL.
      lv_fac_cal = 'ZF'.
    ENDIF.
    "USER STORY 158527 - MMSILVA - 17.01.2025 - Fim

    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        holiday_calendar = lv_hol_cal "USER STORY 158527 - MMSILVA - 17.01.2025
        date_from        = sy-datum
        date_to          = c_data_venc
      TABLES
        holidays         = gt_holidays.


    READ TABLE gt_holidays INTO wl_holidays WITH KEY date = c_data_venc.

    IF ( sy-subrc IS INITIAL ).
      ADD 1 TO c_data_venc.
    ENDIF.

*   Verifica se é um dia útil da semana
    CALL FUNCTION 'DATE_COMPUTE_DAY'
      EXPORTING
        date = c_data_venc
      IMPORTING
        day  = me->at_day.

*   Caso não for um dia útil, adicionar 2 para sábado e 1 para domingo
    IF me->at_day = 6.
      ADD 2 TO c_data_venc.
    ELSEIF me->at_day = 7.
      ADD 1 TO c_data_venc.
    ENDIF.

*   Diferença de dias/horas entre duas datas
    CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
      EXPORTING
        date1         = c_data_venc
        date2         = sy-datum
        output_format = '02'
      IMPORTING
        days          = e_intervalo.
  ENDMETHOD.                    "Z_CHECAR_DIA_UTIL
  METHOD z_converter_moeda.

    IF ( moeda = 'USD' ).
      me->at_vlr_premio = ( vlr_usd * taxa_cambio ).
    ELSE.
      me->at_vlr_premio = ( vlr_brl / taxa_cambio ).
    ENDIF.

    vlr_premio_convertido = me->at_vlr_premio.
  ENDMETHOD.                    "Z_CONVERTER_MOEDA

  METHOD z_criar_mensagem_erro.
    DATA: linha_registro TYPE numc3.

    linha_registro      = index.

    wl_msg_return-tabix = linha_registro.
    wl_msg_return-field = field.
    wl_msg_return-aba   = aba.

    CASE msg_type.
      WHEN 'I'.
        CONCATENATE icon_message_information_small text1 text2 'Linha:' linha_registro
               INTO wl_msg_return-msg SEPARATED BY space.
      WHEN 'W'.
        CONCATENATE icon_message_warning_small text1 text2 'Linha:' linha_registro
               INTO wl_msg_return-msg SEPARATED BY space.
      WHEN OTHERS.
        CONCATENATE icon_message_error_small text1 text2 'Linha:' linha_registro
               INTO wl_msg_return-msg SEPARATED BY space.
    ENDCASE.



    APPEND wl_msg_return TO gt_msg_return.
    CLEAR wl_msg_return.

*    delete adjacent duplicates from gt_msg_return.

  ENDMETHOD.                    "Z_CRIAR_MENSAGEM_ERRO

  METHOD z_validar_info_alv_0120.
    DATA: vlr_total_premio TYPE waers,
          v_msg            TYPE itex132,
          v_vlr_premio     TYPE dmbtr,
          wl_t012          TYPE t012.

    CLEAR: vlr_total_premio.

    LOOP AT gt_saida_0120 INTO wl_saida_0120.
      at_index = sy-tabix.

      IF wl_cabecalho_0110-waers = 'BRL'.
        IF wl_saida_0120-vlr_premio_brl IS INITIAL.

          z_criar_mensagem_erro( msg_type = ''
                                 text1    = TEXT-e01
                                 text2    = 'Valor premio R$'
                                 field    = 'VLR_PREMIO_BRL'
                                 index    = at_index
                                 aba      = '0120' ).
        ENDIF.
      ELSE.
        IF wl_saida_0120-vlr_premio_usd IS INITIAL.
          z_criar_mensagem_erro( msg_type = ''
                                 text1    = TEXT-e01
                                 text2    = 'Valor premio US$'
                                 field    = 'VLR_PREMIO_USD'
                                 index    = at_index
                                 aba      = '0120' ).
        ENDIF.
      ENDIF.

      IF wl_saida_0120-filial IS INITIAL.
        z_criar_mensagem_erro( msg_type = ''
                               text1    = TEXT-e01
                               text2    = 'Filial'
                               field    = 'FILIAL'
                               index    = at_index
                               aba      = '0120' ).
      ENDIF.

      IF wl_saida_0120-taxa_cambio IS INITIAL.
        z_criar_mensagem_erro( msg_type = ''
                               text1    = TEXT-e01
                               text2    = 'Taxa Câmbio'
                               field    = 'TAXA_CAMBIO'
                               index    = at_index
                               aba      = '0120' ).
      ENDIF.

      IF wl_saida_0120-dt_venc IS INITIAL.
        z_criar_mensagem_erro( msg_type = ''
                               text1    = TEXT-e01
                               text2    = 'Dt Vencimento'
                               field    = 'DT_VENC'
                               index    = at_index
                               aba      = '0120' ).
      ENDIF.

*      IF WL_SAIDA_0120-PAIS_PGTO IS INITIAL.
*        Z_CRIAR_MENSAGEM_ERRO( MSG_TYPE = ''
*                               TEXT1 = TEXT-E01
*                               TEXT2 = 'País pgto'
*                               FIELD = 'PAIS_PGTO'
*                               INDEX = AT_INDEX
*                               ABA   = '0120' ).
*      ENDIF.

      IF wl_saida_0120-forma_pgto IS INITIAL.
        CLEAR: wl_t001.
        SELECT SINGLE * FROM t001 INTO wl_t001 WHERE bukrs EQ  wl_cabecalho_0110-bukrs.
        IF wl_t001-land1 EQ 'BR'.
          z_criar_mensagem_erro( msg_type = ''
                                 text1    = TEXT-e01
                                 text2    = 'Forma pgto'
                                 field    = 'FORMA_PGTO'
                                 index    = at_index
                                 aba      = '0120' ).
        ENDIF.
      ENDIF.

      IF wl_saida_0120-bco_empresa IS INITIAL.
        SELECT SINGLE * FROM t001 INTO wl_t001 WHERE bukrs EQ  wl_cabecalho_0110-bukrs.
        IF wl_t001-land1 EQ 'BR'.
          z_criar_mensagem_erro( msg_type = ''
                                 text1    = TEXT-e01
                                 text2    = 'Bco.Empresa'
                                 field    = 'BCO_EMPRESA'
                                 index    = at_index
                                 aba      = '0120' ).
        ENDIF.
      ENDIF.

      IF wl_saida_0120-bco_empresa IS NOT INITIAL.
        CLEAR: wl_t012.
        SELECT SINGLE *
          FROM t012 INTO wl_t012
         WHERE bukrs = wl_cabecalho_0110-bukrs
           AND hbkid = wl_saida_0120-bco_empresa.

        IF sy-subrc NE 0.
          z_criar_mensagem_erro( msg_type = ''
                                 text1    = TEXT-e01
                                 text2    = 'Bco.Empresa'
                                 field    = 'BCO_EMPRESA'
                                 index    = at_index
                                 aba      = '0120' ).
        ENDIF.
      ENDIF.

      IF ( wl_saida_0120-cod_barras IS NOT INITIAL ) AND
         ( strlen( wl_saida_0120-cod_barras ) <> 47 ).
        z_criar_mensagem_erro( msg_type = ''
                               text1    = TEXT-e60
                               text2    = 'Cod.Barras'
                               field    = 'COD_BARRAS'
                               index    = at_index
                               aba      = '0120' ).
      ENDIF.

      IF ( wl_saida_0120-cod_barras IS NOT INITIAL ) AND  ( strlen( wl_saida_0120-cod_barras ) = 47 ).

        DATA l_cb_valor TYPE c LENGTH 17.

        IF wl_cabecalho_0110-waers = 'BRL'.
          l_cb_valor = wl_saida_0120-vlr_premio_brl.
        ELSE.
          l_cb_valor = wl_saida_0120-vlr_premio_usd.
        ENDIF.

        REPLACE ALL OCCURRENCES OF '.' IN l_cb_valor WITH space.
        REPLACE ALL OCCURRENCES OF ',' IN l_cb_valor WITH space.
        CONDENSE l_cb_valor.

        "Verifica número de casas do Valor
        DATA(l_str_cb_valor) = strlen( l_cb_valor ).

        "Extrair do tamanho do Cód, barra o número de casa do valor
        DATA(l_cb_initial) = strlen( wl_saida_0120-cod_barras ) - l_str_cb_valor.

        "Monta código de barras experados
        DATA(l_cb_correto) = wl_saida_0120-cod_barras.
        MOVE l_cb_valor TO l_cb_correto+l_cb_initial(l_str_cb_valor).

        "Verifica se ocódigo de barra esperado é igual ao informado
        IF wl_saida_0120-cod_barras <> l_cb_correto.

          z_criar_mensagem_erro( msg_type = ''
                                 text1    = 'Valor do código de barras incorreto!'
                                 text2    = 'Cod.Barras'
                                 field    = 'COD_BARRAS'
                                 index    = at_index
                                 aba      = '0120' ).
        ENDIF.

      ENDIF.

*      IF WL_SAIDA_0120-BCO_PARCEIRO IS INITIAL.
*        Z_CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E01
*                               TEXT2 = 'Banco parceiro'
*                               FIELD = 'BCO_PARCEIRO'
*                               INDEX = AT_INDEX
*                               ABA   = '0120' ).
*      ENDIF.

*      IF WL_SAIDA_0120-BLOQ_PGTO IS INITIAL.
*        Z_CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E01
*                               TEXT2 = 'Bloq pgto'
*                               FIELD = 'BLOQ_PGTO'
*                               INDEX = AT_INDEX
*                               ABA   = '0120' ).
*      ENDIF.
*

    ENDLOOP.

    CLEAR: v_vlr_premio.

    IF ( gt_msg_return[] IS INITIAL ).
      LOOP AT gt_saida_0120 INTO DATA(wl_saida_0120_aux).
        CASE wl_cabecalho_0110-waers.
          WHEN 'BRL'.
            ADD wl_saida_0120_aux-vlr_premio_brl TO v_vlr_premio.
          WHEN 'USD'.
            ADD wl_saida_0120_aux-vlr_premio_usd TO v_vlr_premio.
        ENDCASE.
      ENDLOOP.

      CASE wl_cabecalho_0110-waers.
        WHEN 'BRL'.
          IF ( v_vlr_premio <> wl_cabecalho_0110-vlr_premio_brl ).
            z_criar_mensagem_erro( msg_type = ''
                                   text1    = TEXT-e62
                                   text2    = 'Valor premio R$'
                                   field    = 'VLR_PREMIO_BRL'
                                   index    = at_index
                                   aba      = '0120' ).

          ENDIF.
        WHEN 'USD'.
          IF ( v_vlr_premio <> wl_cabecalho_0110-vlr_premio_usd ).
            z_criar_mensagem_erro( msg_type = ''
                                   text1    = TEXT-e63
                                   text2    = 'Valor premio US$'
                                   field    = 'VLR_PREMIO_USD'
                                   index    = at_index
                                   aba      = '0120' ).
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDMETHOD.                    "Z_VALIDAR_INFO_ALV_0120

  METHOD z_validar_info_alv_0130.

    DATA: gt_saida_0130_aux TYPE TABLE OF ty_saida_0130,
          wl_saida_0130_aux TYPE ty_saida_0130,
          v_vlr_premio      TYPE dmbtr,
          vl_msg            TYPE itex132.

    LOOP AT gt_saida_0130 INTO wl_saida_0130.
      at_index = sy-tabix.

*   Verifica se é mercadoria ou imobilizado, para obrigar
*   o preenchimento do campo;
*   Faço a seleção da ZGLT064 no method z_pesquisar_registros linha 836.

      IF wl_zglt064-mercadoria EQ 'X'.
        IF wl_saida_0130-mercadoria IS INITIAL.
          z_criar_mensagem_erro( msg_type = ''
                                 text1    = TEXT-e01
                                 text2    = 'Mercadoria'
                                 field    = 'MERCADORIA'
                                 index    = at_index
                                 aba      = '0130' ).
        ENDIF.

      ELSEIF wl_zglt064-imobilizado = 'X'.
        IF wl_saida_0130-imobilizado IS INITIAL.
          z_criar_mensagem_erro( msg_type = ''
                                 text1    = TEXT-e01
                                 text2    = 'Imobilizado'
                                 field    = 'IMOBILIZADO'
                                 index    = at_index
                                 aba      = '0130' ).
        ENDIF.
      ENDIF.

*   Verifica o tipo da moeda para obrigado o preenchimento
*   do campo;

      IF wl_cabecalho_0110-waers = 'BRL'.
        IF wl_saida_0130-vlr_premio_brl IS INITIAL.
          z_criar_mensagem_erro( msg_type = ''
                                 text1    = TEXT-e01
                                 text2    = 'Valor prêmio R$'
                                 field    = 'VLR_PREMIO_BRL'
                                 index    = at_index
                                 aba      = '0130' ).
        ENDIF.

*        IF WL_SAIDA_0130-VLR_AJ_PREM_BRL IS INITIAL.
*          Z_CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E01
*                                 TEXT2 = 'Valor ajuste premio R$'
*                                 ABA   = C_TAB2 ).
*        ENDIF.

      ELSE.
        IF wl_saida_0130-vlr_premio_usd IS INITIAL.
          z_criar_mensagem_erro( msg_type = ''
                                 text1    = TEXT-e01
                                 text2    = 'Valor prêmio US$'
                                 field    = 'VLR_PREMIO_USD'
                                 index    = at_index
                                 aba      = '0130' ).
        ENDIF.

*        IF WL_SAIDA_0130-VLR_AJ_PREM_USD IS INITIAL.
*          Z_CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E01
*                                 TEXT2 = 'Valor ajuste premio US$'
*                                 ABA   = C_TAB2 ).
*        ENDIF.

      ENDIF.

*     Verifica se demais campos estão preenchidos;

      IF wl_saida_0130-filial IS INITIAL.
        z_criar_mensagem_erro( msg_type = ''
                               text1    = TEXT-e01
                               text2    = 'Filial'
                               field    = 'FILIAL'
                               index    = at_index
                               aba      = '0130' ).
      ELSE.
        CLEAR wl_j_1bbranch.

        PERFORM f_check_filial USING wl_cabecalho_0110-bukrs
                                     wl_saida_0130-filial.

        IF sy-subrc NE 0.
          z_criar_mensagem_erro( msg_type = ''
                                 text1    = TEXT-e14
                                 text2    = 'Filial'
                                 field    = 'FILIAL'
                                 index    = at_index
                                 aba      = '0130' ).
        ENDIF.
      ENDIF.

*     Verifica Registros em duplicidade CHASSI NR_SERIE IMOBILIZADO MERCADORIA.
      IF ( wl_zglt064-mercadoria  EQ 'X' ) OR
         ( wl_zglt064-imobilizado EQ 'X' ).

        READ TABLE gt_saida_0130_aux INTO wl_saida_0130_aux
          WITH KEY chassi      = wl_saida_0130-chassi
                   nr_serie    = wl_saida_0130-nr_serie
                   imobilizado = wl_saida_0130-imobilizado
                   mercadoria  = wl_saida_0130-mercadoria.

        IF sy-subrc = 0.
          IF ( wl_zglt064-imobilizado  EQ 'X' ).
            z_criar_mensagem_erro( msg_type = ''
                                   text1    = TEXT-e27
                                   text2    = 'Imobilizado'
                                   field    = 'IMOBILIZADO'
                                   index    = at_index
                                   aba      = '0130' ).
          ELSE.
            z_criar_mensagem_erro( msg_type = ''
                                   text1    = TEXT-e28
                                   text2    = 'Mercadoria'
                                   field    = 'MERCADORIA'
                                   index    = at_index
                                   aba      = '0130' ).
          ENDIF.
        ELSE.
          MOVE-CORRESPONDING wl_saida_0130 TO wl_saida_0130_aux.
          APPEND wl_saida_0130_aux TO gt_saida_0130_aux.
        ENDIF.

      ENDIF.

      IF wl_saida_0130-centro_custo IS NOT INITIAL.
        DATA: wl_csks TYPE csks.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_saida_0130-centro_custo
          IMPORTING
            output = wl_saida_0130-centro_custo.

        SELECT SINGLE *
          FROM csks INTO wl_csks
         WHERE bukrs   = wl_cabecalho_0110-bukrs
           AND gsber   = wl_saida_0130-filial
           AND kostl   = wl_saida_0130-centro_custo
           AND datab LE sy-datum
           AND datbi GE sy-datum.

        IF sy-subrc NE 0.
          CLEAR: vl_msg.
          CONCATENATE TEXT-e35 TEXT-e36 INTO vl_msg SEPARATED BY space.

          z_criar_mensagem_erro( msg_type = ''
                                 text1    = vl_msg
                                 text2    = 'Filial'
                                 field    = 'FILIAL'
                                 index    = at_index
                                 aba      = '0130' ).
        ELSE.
          "Verificar se o centro de custo esta bloqueado para lançamento.
          IF wl_csks-bkzkp IS NOT INITIAL.
            CLEAR: vl_msg.
            vl_msg = |{ TEXT-e74 } { wl_csks-kostl }|.
            z_criar_mensagem_erro( msg_type = 'E'
                                   text1    = vl_msg
                                   text2    = TEXT-e75
                                   field    = 'KOSTL'
                                   index    = at_index
                                   aba      = '0130' ).
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR: vl_msg.
        vl_msg = |Informe o centro de custo|.
        z_criar_mensagem_erro( msg_type = ''
                               text1    = vl_msg
                               text2    = ''
                               field    = 'KOSTL'
                               index    = at_index
                               aba      = '0130' ).

      ENDIF.

      IF ( wl_saida_0130-aufnr IS NOT INITIAL ).

        wl_saida_0130-aufnr = |{ wl_saida_0130-aufnr ALPHA = IN }|.
        SELECT SINGLE * FROM aufk INTO @DATA(w_aufk)
          WHERE aufnr = @wl_saida_0130-aufnr
            AND bukrs = @wl_cabecalho_0110-bukrs.
        IF ( sy-subrc = 0 ).
          IF ( w_aufk-kostv <> wl_saida_0130-centro_custo ).
            CLEAR: vl_msg.
            vl_msg = |Centro de Custo da Ordem diferente do informado para o item.|.
            z_criar_mensagem_erro( msg_type = ''
                                   text1    = vl_msg
                                   text2    = 'Ordem'
                                   field    = 'AUFNR'
                                   index    = at_index
                                   aba      = '0130' ).
          ENDIF.
          IF ( w_aufk-werks <> wl_saida_0130-filial ).
            CLEAR: vl_msg.
            vl_msg = |Filial da Ordem diferente do informado para o item.|.
            z_criar_mensagem_erro( msg_type = ''
                                   text1    = vl_msg
                                   text2    = 'Ordem'
                                   field    = 'AUFNR'
                                   index    = at_index
                                   aba      = '0130' ).
          ENDIF.
        ELSE.
          CLEAR: vl_msg.
          vl_msg = |Ordem { wl_saida_0130-aufnr } não encontrada!|.
          z_criar_mensagem_erro( msg_type = ''
                                 text1    = vl_msg
                                 text2    = 'Ordem'
                                 field    = 'AUFNR'
                                 index    = at_index
                                 aba      = '0130' ).
        ENDIF.

      ENDIF.

      IF ( wl_saida_0130-vornr IS NOT INITIAL ).

        SELECT SINGLE *  FROM caufv
          INTO @DATA(wa_caufv)
          WHERE aufnr EQ @wl_saida_0130-aufnr.

        SELECT SINGLE *
          FROM afvc INTO @DATA(wa_afvc)
          WHERE aufpl = @wa_caufv-aufpl
          AND   vornr = @wl_saida_0130-vornr.

        IF sy-subrc NE 0.
          CLEAR: vl_msg.
          vl_msg = |Operação invalida para Ordem PM.|.
          z_criar_mensagem_erro( msg_type = ''
                                 text1    = vl_msg
                                 text2    = 'Operação'
                                 field    = 'VORNR'
                                 index    = at_index
                                 aba      = '0130' ).
        ENDIF.

      ELSE.

        SELECT SINGLE *
         FROM aufk   INTO @DATA(wl_aufk)
               WHERE bukrs  = @wl_saida_0120-bukrs
               AND   aufnr  = @wl_saida_0130-aufnr.
        IF sy-subrc = 0.
          IF wl_aufk-autyp = '30'.
            CLEAR: vl_msg.
            vl_msg = |Informe a operação para Ordem PM.|.
            z_criar_mensagem_erro( msg_type = ''
                                   text1    = vl_msg
                                   text2    = 'Operação'
                                   field    = 'VORNR'
                                   index    = at_index
                                   aba      = '0130' ).
          ENDIF.
        ENDIF.
      ENDIF.


      IF wl_saida_0130-descr_bens IS INITIAL.
        z_criar_mensagem_erro( msg_type = ''
                               text1    = TEXT-e01
                               text2    = 'Desc. Bens'
                               field    = 'DESCR_BENS'
                               index    = at_index
                               aba      = '0130' ).
      ENDIF.

*      IF WL_SAIDA_0130-UF IS INITIAL.
*        Z_CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E01
*                               TEXT2 = 'Uf'
*                               FIELD = 'UF'
*                               INDEX = AT_INDEX
*                               ABA   = '0130' ).
*      ENDIF.


    ENDLOOP.

    CLEAR: v_vlr_premio.

    CHECK ( gt_msg_return[] IS INITIAL ).

    IF ( i_valida_tot IS NOT INITIAL ) AND ( gt_saida_0130[] IS NOT INITIAL ).
      LOOP AT gt_saida_0130 INTO wl_saida_0130_aux.
        CASE wl_cabecalho_0110-waers.
          WHEN 'BRL'.
            ADD wl_saida_0130_aux-vlr_premio_brl TO v_vlr_premio.
          WHEN 'USD'.
            ADD wl_saida_0130_aux-vlr_premio_usd TO v_vlr_premio.
        ENDCASE.
      ENDLOOP.

      CASE wl_cabecalho_0110-waers.
        WHEN 'BRL'.
          IF ( v_vlr_premio <> wl_cabecalho_0110-vlr_premio_brl ).
            z_criar_mensagem_erro( msg_type = ''
                                   text1    = TEXT-e62
                                   text2    = 'Valor premio R$'
                                   field    = 'VLR_PREMIO_BRL'
                                   index    = at_index
                                   aba      = '0130' ).

          ENDIF.
        WHEN 'USD'.
          IF ( v_vlr_premio <> wl_cabecalho_0110-vlr_premio_usd ).
            z_criar_mensagem_erro( msg_type = ''
                                   text1    = TEXT-e63
                                   text2    = 'Valor premio US$'
                                   field    = 'VLR_PREMIO_USD'
                                   index    = at_index
                                   aba      = '0130' ).
          ENDIF.
      ENDCASE.
    ENDIF.


  ENDMETHOD.                    "Z_VALIDAR_INFO_ALV_0130

  METHOD z_show_splitter_error.

*    MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E06.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = i_show
        i_repid    = sy-repid
        i_set_cell = 'WL_CELL'
        i_set_obj  = 'WL_OBJ'
      IMPORTING
        e_messagem = wg_mensagem
      TABLES
        it_msgs    = gt_msg_return.

  ENDMETHOD.                    "Z_SHOW_SPLITTER

  METHOD z_validar_cabecalho_0110.
    CLEAR: return_status.
    DATA: zcheck_modificacao TYPE char01.

* RJF - Ini - DEVK9A1CR3 - GL - CS2022000470 Incl campo Seq endosso apolice #77353 RJF
    IF wl_cabecalho_0110-tp_opr EQ 'E'. " Endosso
      IF wl_cabecalho_0110-seq_endosso IS INITIAL.
        MESSAGE s836(sd) WITH 'Obrigatório informar SEQ_ENDOSSO!'(e71) DISPLAY LIKE 'E'. "'text-e17'
        return_status = 'X'.
      ELSE.
        IF sy-ucomm EQ c_novo.
          SELECT SINGLE *
           FROM zglt050
           INTO @DATA(wl_zglt050x)
          WHERE nro_apolice EQ @wl_cabecalho_0110-nro_apolice
            AND seq_endosso EQ @wl_cabecalho_0110-seq_endosso
            AND tp_opr      EQ @wl_cabecalho_0110-tp_opr.

          IF sy-subrc IS INITIAL AND wl_zglt050x-seq_endosso IS NOT INITIAL.
            MESSAGE s836(sd) WITH 'Seq. Endosso informada já existe para essa apólice!'(e72) DISPLAY LIKE 'E'.
            return_status = 'X'.
          ENDIF.

          LOOP AT SCREEN.
            IF screen-name = 'WL_CABECALHO_0110-SEQ_ENDOSSO'.
              screen-input        = '0'.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.

        ELSEIF sy-ucomm EQ c_change.
          LOOP AT SCREEN.
            IF screen-name = 'WL_CABECALHO_0110-SEQ_ENDOSSO'.
              screen-input        = '1'.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.

        ENDIF.
      ENDIF.
    ENDIF.


    "Verifica duplicidade da apolice / US 77353 / AOENNING
*    CLEAR: zcheck_modificacao.
*    if wl_cabecalho_0110-tp_opr = 'E'.
*
*      select single * from zglt050 into @data(ws_zglt050)
*      where nro_apolice eq @wl_cabecalho_0110-nro_apolice
*        and tp_opr eq @wl_cabecalho_0110-tp_opr
*        and seq_endosso eq @wl_cabecalho_0110-seq_endosso.
*      if sy-subrc eq 0. "*Bug Impeditivo 153361 / AOENNING
*        if gt_saida_0130 is initial.
*          message s024(sd) with text-e73 display like 'E'.
*          return_status = 'X'.
*        else.
**Bug Impeditivo 153361 / AOENNING
*          select * from zglt068 into table @data(it_ZGLT068)
*            where seq_lcto eq @wl_cabecalho_0110-seq_lcto.
*          if sy-subrc ne 0.
*            message s024(sd) with text-e73 display like 'E'.
*            return_status = 'X'.
*            exit.
*          endif.
*
*          loop at gt_saida_0130[] assigning field-symbol(<saida_0130>).
*            read table it_ZGLT068 into data(wa_ZGLT068) with key nr_item = <saida_0130>-nr_item.
*            if sy-subrc ne 0.
*              message s024(sd) with text-e73 display like 'E'.
*              return_status = 'X'.
*              exit.
*            endif.
*
*            if <saida_0130>-descr_bens ne wa_ZGLT068-descr_bens.
*              zcheck_modificacao = 'X'.
*              exit.
*            endif.
*            clear: wa_ZGLT068.
*          endloop.
*        endif.
*      endif.
*    endif.
*
*    if zcheck_modificacao ne abap_true.
*      return_status = 'X'.
*       message s024(sd) with text-e73 display like 'E'.
*       exit.
*    endif.
*
*    free: it_ZGLT068.
*    clear: wa_ZGLT068.
*Bug Impeditivo 153361 / AOENNING

* RJF - Fim - DEVK9A1CR3 - GL - CS2022000470 Incl campo Seq dendosso apolice #77353 RJF

    IF wl_cabecalho_0110-bukrs IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.

    ELSEIF wl_cabecalho_0110-nro_apolice IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.
*
    ELSEIF wl_cabecalho_0110-vig_de IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.

    ELSEIF wl_cabecalho_0110-vig_de IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.
*
    ELSEIF wl_cabecalho_0110-cod_seguradora IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.

    ELSEIF wl_cabecalho_0110-wkurs IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.

    ELSEIF wl_cabecalho_0110-seq_tipo IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.
*
*    ELSEIF WL_CABECALHO_0110-SEQ_MOD IS INITIAL.
*      MESSAGE S836(SD) WITH TEXT-E17 DISPLAY LIKE 'E'.
*      RETURN_STATUS = 'X'.
*
    ELSEIF wl_cabecalho_0110-seq_parc IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.

    ELSEIF wl_cabecalho_0110-waers IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.
*
    ELSEIF wl_cabecalho_0110-waers EQ 'USD'.

      IF wl_cabecalho_0110-vlr_premio_usd IS INITIAL.
        MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
        return_status = 'X'.
      ENDIF.

      IF wl_cabecalho_0110-tp_opr NE 'B'.

        IF wl_cabecalho_0110-vlr_asseg_usd IS INITIAL.
          MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
          return_status = 'X'.

        ELSEIF wl_cabecalho_0110-vlr_lmi_usd IS INITIAL.
          MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
          return_status = 'X'.
        ENDIF.

      ENDIF.

    ELSEIF wl_cabecalho_0110-waers EQ 'BRL'.
*
      IF wl_cabecalho_0110-vlr_premio_brl IS INITIAL.
        MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
        return_status = 'X'.
      ENDIF.

      IF wl_cabecalho_0110-tp_opr NE 'B'.

        IF wl_cabecalho_0110-vlr_asseg_brl IS INITIAL.
          MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
          return_status = 'X'.

        ELSEIF wl_cabecalho_0110-vlr_lmi_brl IS INITIAL.
          MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
          return_status = 'X'.
        ENDIF.

      ENDIF.

    ENDIF.

*    PERFORM zf_valida_campos CHANGING return_status.


*    DATA AT_MONTHS TYPE TFMATAGE.
*    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
*      EXPORTING
*        I_DATE_FROM    = WL_CABECALHO_0110-VIG_DE
*        I_DATE_TO      = WL_CABECALHO_0110-VIG_ATE
*        I_FLG_SEPARATE = ' '
*      IMPORTING
*        E_MONTHS       = AT_MONTHS.
*    IF ( AT_MONTHS > 12 ).
*      MESSAGE S836(SD) WITH 'Período de vigência não pode ultrapassar de 1 ano.' DISPLAY LIKE 'E'.
*      RETURN_STATUS = 'X'.
*    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_cabecalho_0110-cod_seguradora
      IMPORTING
        output = cod_fornecedor.

    CHECK ( return_status IS INITIAL ).

    CLEAR: wl_zglt064, wl_zglt032.
    header_status = abap_true. "Indica que o cabeçalho foi preenchido.

    SELECT SINGLE *
      FROM zglt064
      INTO wl_zglt064
     WHERE seq_tipo EQ wl_cabecalho_0110-seq_tipo.

    vg_tp_lcto = wl_zglt064-tp_lcto.
    IF wl_cabecalho_0110-tp_opr = 'B'. "Baixa.
      vg_tp_lcto = wl_zglt064-tp_lcto_ar. "Contas a Receber
    ENDIF.

    SELECT SINGLE *
      FROM zglt032
      INTO wl_zglt032
     WHERE tp_lcto EQ vg_tp_lcto
       AND hkont   EQ cod_fornecedor.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s836(sd) WITH TEXT-e19 '(ZGL014).' DISPLAY LIKE 'E'.
      return_status = 'X'.
      EXIT.
    ENDIF.

*    DATA: VLR_PREMIO_USD TYPE DEC_16_14_S,
*          VLR_PREMIO_BRL TYPE DEC_16_14_S.

*    VLR_PREMIO_USD = WL_CABECALHO_0110-VLR_PREMIO_USD.
*    VLR_PREMIO_BRL = WL_CABECALHO_0110-VLR_PREMIO_BRL.

*   Converte Vlr Prêmio
    z_converter_moeda( EXPORTING moeda                 = wl_cabecalho_0110-waers
                                 taxa_cambio           = wl_cabecalho_0110-wkurs
                                 vlr_usd               = wl_cabecalho_0110-vlr_premio_usd
                                 vlr_brl               = wl_cabecalho_0110-vlr_premio_brl
                       IMPORTING vlr_premio_convertido = me->at_vlr_premio ).

    IF wl_cabecalho_0110-waers = 'USD'.
      wl_cabecalho_0110-vlr_premio_brl = me->at_vlr_premio.
    ELSE.
      wl_cabecalho_0110-vlr_premio_usd = me->at_vlr_premio.
    ENDIF.

*   Converte Vlr Assegurado
    z_converter_moeda( EXPORTING moeda                 = wl_cabecalho_0110-waers
                                 taxa_cambio           = wl_cabecalho_0110-wkurs
                                 vlr_usd               = wl_cabecalho_0110-vlr_asseg_usd
                                 vlr_brl               = wl_cabecalho_0110-vlr_asseg_brl
                       IMPORTING vlr_premio_convertido = me->at_vlr_premio ).

    IF wl_cabecalho_0110-waers = 'USD'.
      wl_cabecalho_0110-vlr_asseg_brl = me->at_vlr_premio.
    ELSE.
      wl_cabecalho_0110-vlr_asseg_usd = me->at_vlr_premio.
    ENDIF.

*   Converte Vlr Limite Máximo Indenização (LMI)
    z_converter_moeda( EXPORTING moeda                 = wl_cabecalho_0110-waers
                                 taxa_cambio           = wl_cabecalho_0110-wkurs
                                 vlr_usd               = wl_cabecalho_0110-vlr_lmi_usd
                                 vlr_brl               = wl_cabecalho_0110-vlr_lmi_brl
                       IMPORTING vlr_premio_convertido = me->at_vlr_premio ).

    IF wl_cabecalho_0110-waers = 'USD'.
      wl_cabecalho_0110-vlr_lmi_brl = me->at_vlr_premio.
    ELSE.
      wl_cabecalho_0110-vlr_lmi_usd = me->at_vlr_premio.
    ENDIF.

    PERFORM zf_valida_campos CHANGING return_status.

  ENDMETHOD.                    "Z_VALIDAR_INFO_CABECALHO

  METHOD z_validar_cabecalho_0150.
    CLEAR: return_status.

*    IF WL_CABECALHO_0150-BUKRS IS INITIAL.
*      MESSAGE S836(SD) WITH TEXT-E17 DISPLAY LIKE 'E'.
*      RETURN_STATUS = 'X'.

    "ELSEIF WL_CABECALHO_0150-DT_APROP_DE IS INITIAL.
    "  MESSAGE S836(SD) WITH TEXT-E17 DISPLAY LIKE 'E'.
    "  RETURN_STATUS = 'X'.

*    ELSEIF WL_CABECALHO_0150-SEQ_TIPO IS INITIAL.
*      MESSAGE S836(SD) WITH TEXT-E17 DISPLAY LIKE 'E'.
*      RETURN_STATUS = 'X'.

    IF wl_cabecalho_0150-competencia IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.
    ELSE.
**<<<------"164255 - NMS - INI------>>>
      IF wl_cabecalho_0150-seq_tipo EQ 17 OR
         wl_cabecalho_0150-seq_tipo EQ 21 OR
         wl_cabecalho_0150-seq_tipo EQ 22 OR
         wl_cabecalho_0150-seq_tipo EQ 29.
**<<<------"164255 - NMS - FIM------>>>
        PERFORM valida_competencia USING wl_cabecalho_0150-competencia
                                CHANGING return_status
                                         wl_cabecalho_0150-dt_aprop_de
                                         wl_cabecalho_0150-dtapr_ult_de.

        IF wl_cabecalho_0150-competenci2 IS INITIAL.
          wl_cabecalho_0150-competenci2  = wl_cabecalho_0150-competencia.
          wl_cabecalho_0150-dt_aprop_ate = wl_cabecalho_0150-dtapr_ult_de.

        ELSE.
          PERFORM valida_competencia USING wl_cabecalho_0150-competenci2
                                  CHANGING return_status
                                           wl_cabecalho_0150-dt_aprop_ate
                                           wl_cabecalho_0150-dt_aprop_ate.

        ENDIF.
**<<<------"164255 - NMS - INI------>>>
      ELSE.
        PERFORM valida_competencia USING wl_cabecalho_0150-competencia
                                CHANGING return_status
                                         wl_cabecalho_0150-dt_aprop_de
                                         wl_cabecalho_0150-dt_aprop_ate.

        wl_cabecalho_0150-dtapr_ult_de = wl_cabecalho_0150-dt_aprop_ate.

      ENDIF.
**<<<------"164255 - NMS - FIM------>>>
    ENDIF.

    PERFORM zf_valida_campos CHANGING return_status.

  ENDMETHOD.                    "z_validar_cabecalho_0150

  METHOD z_validar_cabecalho_0160.
    CLEAR: return_status.

*    IF WL_CABECALHO_0160-BUKRS IS INITIAL.
*      MESSAGE S836(SD) WITH TEXT-E17 DISPLAY LIKE 'E'.
*      RETURN_STATUS = 'X'.
*    ELSEIF WL_CABECALHO_0160-SEQ_TIPO IS INITIAL.
*      MESSAGE S836(SD) WITH TEXT-E17 DISPLAY LIKE 'E'.
*      RETURN_STATUS = 'X'.

*    IF WL_CABECALHO_0160-COMPETENCIA IS INITIAL.
*      MESSAGE S836(SD) WITH TEXT-E17 DISPLAY LIKE 'E'.
*      RETURN_STATUS = 'X'.
*    ELSE.
    IF wl_cabecalho_0160 IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.
    ELSE.
      IF wl_cabecalho_0160-competencia IS NOT INITIAL.


        PERFORM valida_competencia USING wl_cabecalho_0160-competencia
                                CHANGING return_status
                                         wl_cabecalho_0160-dt_aprop_de
                                         wl_cabecalho_0160-dt_aprop_ate.

      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD z_validar_cabecalho_0170.
    CLEAR: return_status.

*    IF WL_CABECALHO_0150-BUKRS IS INITIAL.
*      MESSAGE S836(SD) WITH TEXT-E17 DISPLAY LIKE 'E'.
*      RETURN_STATUS = 'X'.

    "ELSEIF WL_CABECALHO_0150-DT_APROP_DE IS INITIAL.
    "  MESSAGE S836(SD) WITH TEXT-E17 DISPLAY LIKE 'E'.
    "  RETURN_STATUS = 'X'.

*    ELSEIF WL_CABECALHO_0150-SEQ_TIPO IS INITIAL.
*      MESSAGE S836(SD) WITH TEXT-E17 DISPLAY LIKE 'E'.
*      RETURN_STATUS = 'X'.

    IF wl_cabecalho_0170-dt_aprop_de IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.
    ELSEIF wl_cabecalho_0170-dt_aprop_ate IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
      return_status = 'X'.
    ELSEIF wl_cabecalho_0170-dt_aprop_de > wl_cabecalho_0170-dt_aprop_ate.
      MESSAGE s836(sd) WITH TEXT-e68 DISPLAY LIKE 'E'.
      return_status = 'X'.
    ELSE.
      "PERFORM VALIDA_COMPETENCIA USING WL_CABECALHO_0170-COMPETENCIA
      "                        CHANGING RETURN_STATUS
      "                                 WL_CABECALHO_0170-DT_APROP_DE
      "                                 WL_CABECALHO_0170-DT_APROP_ATE.
    ENDIF.

  ENDMETHOD.                    "z_validar_cabecalho_0150

  METHOD z_style_disable_edit.

    wl_estilo-fieldname = fieldname.
    wl_estilo-style     = style.

*    APPEND WL_ESTILO TO GT_ESTILO.
    INSERT wl_estilo INTO TABLE gt_estilo[].
  ENDMETHOD.                    "Z_STYLE_DISABLE_EDIT

  METHOD z_tratar_campos.

    wl_fields-campo     = name.
    wl_fields-group1    = group1.
    wl_fields-group2    = group2.
    wl_fields-value     = value.
    wl_fields-invisible = invisible.

    APPEND wl_fields TO gt_fields.
  ENDMETHOD.                    "Z_TRATAR_CAMPOS
ENDCLASS.                    "ZUTILS IMPLEMENTATION

DATA: go_utils TYPE REF TO zutils.

*----------------------------------------------------------------------*
*       CLASS Z_INSERT_ROW_ALV DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS z_insert_row_alv DEFINITION.
  PUBLIC SECTION.

    METHODS insert_row_tela_0120. "Contas a Pagar e Receber
    METHODS insert_row_tela_0130 IMPORTING append_table TYPE c. "Bens Assegurados
    METHODS insert_row_tela_0130_prorrog IMPORTING append_table TYPE c. "Bens Assegurados/Prorrogados

ENDCLASS.                    "Z_INSERT_ROW_ALV DEFINITION

*----------------------------------------------------------------------*
*       CLASS Z_INSERT_ROW_ALV IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS z_insert_row_alv IMPLEMENTATION.
  METHOD insert_row_tela_0120.
    DATA:
      v_index               TYPE sy-tabix,
      v_filial              TYPE j_1bbranch-branch,
      vlr_premio_parcela    TYPE dmbtr,
      vlr_diferenca_parcela TYPE dmbtr.

    CREATE OBJECT go_utils.
    CLEAR: wl_lfbk, gt_lfbk, vlr_diferenca_parcela.

*    IF ( WL_CABECALHO_0110-WAERS EQ 'BRL' ).
*      VLR_PREMIO_PARCELA    = ( WL_CABECALHO_0110-VLR_PREMIO_BRL / WL_CABECALHO_0110-SEQ_PARC ).
*
*      IF ( VLR_DIFERENCA_PARCELA IS INITIAL ).
*        VLR_DIFERENCA_PARCELA = ( VLR_PREMIO_PARCELA * WL_CABECALHO_0110-SEQ_LCTO ).
*        VLR_DIFERENCA_PARCELA = ( WL_CABECALHO_0110-VLR_PREMIO_BRL - VLR_DIFERENCA_PARCELA ).
*        ADD VLR_DIFERENCA_PARCELA TO VLR_PREMIO_PARCELA.
*      ENDIF.
*
*    ELSE.
*      VLR_PREMIO_PARCELA    = ( WL_CABECALHO_0110-VLR_PREMIO_USD / WL_CABECALHO_0110-SEQ_PARC ).
*
*      IF ( VLR_DIFERENCA_PARCELA IS INITIAL ).
*        VLR_DIFERENCA_PARCELA = ( VLR_PREMIO_PARCELA * WL_CABECALHO_0110-SEQ_LCTO ).
*        VLR_DIFERENCA_PARCELA = ( WL_CABECALHO_0110-VLR_PREMIO_USD - VLR_DIFERENCA_PARCELA ).
*        ADD VLR_DIFERENCA_PARCELA TO VLR_PREMIO_PARCELA.
*      ENDIF.
*
*    ENDIF.

    REFRESH gt_saida_0120.
    go_utils->z_seleciona_dados_0110( ).

    SELECT *
      FROM lfbk
      INTO TABLE gt_lfbk
     WHERE lifnr = cod_fornecedor.

    DESCRIBE TABLE gt_lfbk LINES lines.
    IF ( lines = 1 ).
      READ TABLE gt_lfbk INTO wl_lfbk INDEX 1.
    ENDIF.

    v_index = 1.

    WHILE ( v_index <= wl_cabecalho_0110-seq_parc ).
      CLEAR: wl_saida_0120, gt_estilo[], lines, gt_color.

      CONCATENATE wl_cabecalho_0110-bukrs+2(2) '01' INTO
      wl_saida_0120-filial.

      wl_saida_0120-status        = icon_light_out.
      wl_saida_0120-nro_parc      = v_index.
      "WL_SAIDA_0120-PAIS_PGTO     = 'BR'.
      "WL_SAIDA_0120-BCO_EMPRESA   = 'BBRA'.
      wl_saida_0120-bco_parceiro  = wl_lfbk-bankn.
      wl_saida_0120-taxa_cambio   = wl_cabecalho_0110-wkurs.

      IF ( wl_cabecalho_0110-waers = 'USD' ).

        vlr_premio_parcela = ( wl_cabecalho_0110-vlr_premio_usd / wl_cabecalho_0110-seq_parc ).

        IF ( vlr_diferenca_parcela IS INITIAL ).
          vlr_diferenca_parcela = ( vlr_premio_parcela * wl_cabecalho_0110-seq_parc ).
          vlr_diferenca_parcela = ( wl_cabecalho_0110-vlr_premio_usd - vlr_diferenca_parcela ).
          ADD vlr_diferenca_parcela TO vlr_premio_parcela.
        ENDIF.


*---> 10/06/2023 - Migração S4 - JS
*              wl_saida_0120-vlr_premio_usd = vlr_premio_parcela.
        wl_saida_0120-vlr_premio_usd = CONV #( vlr_premio_parcela ).
*<--- 10/06/2023 - Migração S4 - JS

*---> 29/05/2023 - Migração S4 - JS
        DATA: lv_vlr_premio_usd        TYPE dmbtr,
              lv_vlr_premio_convertido TYPE dmbtr.

        lv_vlr_premio_usd = CONV #( wl_saida_0120-vlr_premio_usd ).
        lv_vlr_premio_convertido = CONV #( wl_saida_0120-vlr_premio_brl ).
*<--- 29/05/2023 - Migração S4 - JS










        go_utils->z_converter_moeda( EXPORTING moeda                 = wl_cabecalho_0110-waers
                                               taxa_cambio           = wl_saida_0120-taxa_cambio
*---> 29/05/2023 - Migração S4 - JS
                                              "vlr_usd     = wl_saida_0120-vlr_premio_usd
                                              "vlr_brl     = space
                                              "IMPORTING
                                              "vlr_premio_convertido = wl_saida_0120-vlr_premio_brl ).
                                               vlr_usd               = lv_vlr_premio_usd
                                               vlr_brl               = ' '
                                     IMPORTING vlr_premio_convertido = lv_vlr_premio_convertido ).
*<--- 29/05/2023 - Migração S4 - JS

        "IR163960 - Ajuste valor premio BRL moeda USD - BG
        wl_saida_0120-vlr_premio_brl =     CONV #( lv_vlr_premio_convertido ).





        go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_BRL'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).

      ELSEIF ( wl_cabecalho_0110-waers = 'BRL' ).

        vlr_premio_parcela = ( wl_cabecalho_0110-vlr_premio_brl / wl_cabecalho_0110-seq_parc ).

        IF ( vlr_diferenca_parcela IS INITIAL ).
          vlr_diferenca_parcela = ( vlr_premio_parcela * wl_cabecalho_0110-seq_parc ).
          vlr_diferenca_parcela = ( wl_cabecalho_0110-vlr_premio_brl - vlr_diferenca_parcela ).
          ADD vlr_diferenca_parcela TO vlr_premio_parcela.
        ENDIF.



*---> 10/06/2023 - Migração S4 - JS
*        wl_saida_0120-vlr_premio_brl = vlr_premio_parcela.
        wl_saida_0120-vlr_premio_brl = CONV #( vlr_premio_parcela ).
*<--- 10/06/2023 - Migração S4 - JS




*---> 29/05/2023 - Migração S4 - JS
        DATA: lv_vlr_premio_brl TYPE dmbtr.

        lv_vlr_premio_brl = CONV #( wl_saida_0120-vlr_premio_brl ).
        lv_vlr_premio_convertido = CONV #( wl_saida_0120-vlr_premio_brl ).

*<--- 29/05/2023 - Migração S4 - JS











        go_utils->z_converter_moeda( EXPORTING moeda                 = wl_cabecalho_0110-waers
                                               taxa_cambio           = wl_saida_0120-taxa_cambio
                                               vlr_usd               = space
*---> 29/05/2023 - Migração S4 - JS
                                              "vlr_brl     = wl_saida_0120-vlr_premio_brl
                                               vlr_brl               = lv_vlr_premio_brl
*<--- 29/05/2023 - Migração S4 - JS
                                              "IMPORTING
                                              "vlr_premio_convertido = wl_saida_0120-vlr_premio_usd ).
                                     IMPORTING vlr_premio_convertido = lv_vlr_premio_convertido ).
        wl_saida_0120-vlr_premio_usd = CONV #( lv_vlr_premio_convertido ).
*<--- 29/05/2023 - Migração S4 - JS


        go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_USD'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
      ENDIF.

*      DESCRIBE TABLE GT_SAIDA_0120 LINES LINES.

      INSERT LINES OF gt_estilo INTO TABLE wl_saida_0120-estilo.
      APPEND wl_saida_0120 TO gt_saida_0120.
*      INSERT LINES OF GT_COLOR  INTO TABLE WL_SAIDA_0120-COLOR.

*      MODIFY GT_SAIDA_0120 FROM WL_SAIDA_0120
*       INDEX LINES.

      ADD 1 TO v_index.
    ENDWHILE.

    CALL METHOD obj_alv_0120->refresh_table_display
      EXPORTING
        is_stable = wl_stable.
  ENDMETHOD.                    "INSERIR_LINHA_TELA_0110

  METHOD insert_row_tela_0130.
    CLEAR: wl_saida_0130, lines, gt_estilo[].
    CREATE OBJECT go_utils.

    wl_saida_0130-dt_inic_vigenc = wl_cabecalho_0110-vig_de.
    wl_saida_0130-taxa_cambio    = wl_cabecalho_0110-wkurs.
    wl_saida_0130-clau_benef     = 'NÃO'.

    IF ( NOT wl_zglt064-imobilizado IS INITIAL ).
      go_utils->z_style_disable_edit( fieldname = 'CENTRO_CUSTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CHASSI'
                                      style     = cl_gui_alv_grid=>mc_style_enabled ).
      go_utils->z_style_disable_edit( fieldname = 'DESCR_BENS'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'MERCADORIA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'NR_SERIE'
                                      style     = cl_gui_alv_grid=>mc_style_enabled ).
      go_utils->z_style_disable_edit( fieldname = 'SUBNUMERO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
    ELSEIF ( NOT wl_zglt064-mercadoria IS INITIAL ).
      go_utils->z_style_disable_edit( fieldname = 'DESCR_BENS'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'IMOBILIZADO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SUBNUMERO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
    ELSE.
      go_utils->z_style_disable_edit( fieldname = 'IMOBILIZADO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'MERCADORIA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SUBNUMERO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
    ENDIF.

    CASE wl_cabecalho_0110-waers.
      WHEN 'USD'.
        go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_BRL'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_BRL'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_BRL'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
      WHEN 'BRL'.
        go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_USD'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_USD'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_USD'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
    ENDCASE.

*    DESCRIBE TABLE GT_SAIDA_0130 LINES LINES.

    INSERT LINES OF gt_estilo INTO TABLE wl_saida_0130-estilo.

    IF append_table IS NOT INITIAL.

      INSERT wl_saida_0130 INTO gt_saida_0130 INDEX 1.
      "APPEND WL_SAIDA_0130 TO GT_SAIDA_0130.

*    MODIFY GT_SAIDA_0130 FROM WL_SAIDA_0130 INDEX LINES.

      CALL METHOD obj_alv_0130->refresh_table_display
        EXPORTING
          is_stable = wl_stable.
    ENDIF.

  ENDMETHOD.                    "Z_INSERT_ROW_TELA_0120

  METHOD insert_row_tela_0130_prorrog.
    CLEAR: wl_saida_0130, lines, gt_estilo[].
    CREATE OBJECT go_utils.

    wl_saida_0130-dt_inic_vigenc = wl_cabecalho_0110-vig_de.
    wl_saida_0130-taxa_cambio    = wl_cabecalho_0110-wkurs.
    wl_saida_0130-clau_benef     = 'NÃO'.

    IF ( NOT wl_zglt064-imobilizado IS INITIAL ).
      go_utils->z_style_disable_edit( fieldname = 'CENTRO_CUSTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CHASSI'
                                      style     = cl_gui_alv_grid=>mc_style_enabled ).
      go_utils->z_style_disable_edit( fieldname = 'DESCR_BENS'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'MERCADORIA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'NR_SERIE'
                                      style     = cl_gui_alv_grid=>mc_style_enabled ).
      go_utils->z_style_disable_edit( fieldname = 'SUBNUMERO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
    ELSEIF ( NOT wl_zglt064-mercadoria IS INITIAL ).
      go_utils->z_style_disable_edit( fieldname = 'DESCR_BENS'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'IMOBILIZADO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SUBNUMERO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
    ELSE.
      go_utils->z_style_disable_edit( fieldname = 'IMOBILIZADO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'MERCADORIA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SUBNUMERO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
    ENDIF.

    CASE wl_cabecalho_0110-waers.
      WHEN 'USD'.
        go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_BRL'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_BRL'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_BRL'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
      WHEN 'BRL'.
        go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_USD'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_USD'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_USD'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
    ENDCASE.

*    DESCRIBE TABLE GT_SAIDA_0130 LINES LINES.

*    INSERT LINES OF GT_ESTILO INTO TABLE WL_SAIDA_0130-ESTILO.

    IF append_table IS NOT INITIAL.

      LOOP AT gt_saida_0130 ASSIGNING FIELD-SYMBOL(<fs_saida_0130>).
        <fs_saida_0130>-estilo = gt_estilo[].
      ENDLOOP.
      " WL_SAIDA_0130 INTO GT_SAIDA_0130 INDEX 1.
      "APPEND WL_SAIDA_0130 TO GT_SAIDA_0130.

*    MODIFY GT_SAIDA_0130 FROM WL_SAIDA_0130 INDEX LINES.

*      CALL METHOD OBJ_ALV_0130->REFRESH_TABLE_DISPLAY
*        EXPORTING
*          IS_STABLE = WL_STABLE.
    ENDIF.

  ENDMETHOD.                    "Z_INSERT_ROW_TELA_0130_PRORROG.
ENDCLASS.                    "Z_INSERT_ROW_ALV IMPLEMENTATION

*&---------------------------------------------------------------------*
*&  Include           Z_CLASS_TIPO_OPERACAO
*&---------------------------------------------------------------------*

CLASS z_tipo_operacao DEFINITION.
  PUBLIC SECTION.
    METHODS z_criar_nova_operacao.
    METHODS z_pesquisar_registros.
    METHODS z_pesquisar_registros_prorrog.
    METHODS z_salvar_registros.
    METHODS z_check_operacao.
    METHODS z_check_operacao_prorrog.
    METHODS z_clear_referencia.
    METHODS z_modo_edicao.
    METHODS z_del_apolice.
ENDCLASS.                    "Z_TIPO_OPERACAO DEFINITION

*----------------------------------------------------------------------*
*       CLASS Z_TIPO_OPERACAO IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS z_tipo_operacao IMPLEMENTATION.
  METHOD z_criar_nova_operacao.

    CLEAR: wl_cabecalho_0110, gt_saida_0120, gt_saida_0130, wl_editor,
           gt_editor, gt_fcat_0120, gt_fcat_0130, wg_mensagem,
           wl_cabecalho_0110_aux, gt_msg_return.

    CALL METHOD obj_custom_editor->set_text_as_stream
      EXPORTING
        text = gt_editor.

    IF ( obj_alv_0120 IS NOT INITIAL ).
      CALL METHOD obj_alv_0120->refresh_table_display
        EXPORTING
          is_stable = wl_stable.
    ENDIF.

    IF ( obj_alv_0130 IS NOT INITIAL ).
      CALL METHOD obj_alv_0130->refresh_table_display
        EXPORTING
          is_stable = wl_stable.
    ENDIF.
  ENDMETHOD.                    "Z_CRIAR_NOVA_OPERACAO

  METHOD z_pesquisar_registros.

    DATA: lv_seq_lcto   TYPE zseq_lcto,
          vl_dt_baixa   TYPE zglt068-dt_baixa,
          vl_doc_delete TYPE c,
          wl_zglt035    TYPE zglt035.

    op_modo = c_search.

    CREATE OBJECT go_utils.
    CLEAR: gt_saida_0120, gt_saida_0130, gt_editor, wl_editor.

    lv_seq_lcto = wl_cabecalho_0110-seq_lcto.
    CLEAR wl_cabecalho_0110.

    SELECT SINGLE *
      FROM zglt050 INTO CORRESPONDING FIELDS OF wl_cabecalho_0110
     WHERE seq_lcto EQ lv_seq_lcto
       AND loekz    EQ ''.

    IF ( sy-subrc IS NOT INITIAL ).
      MESSAGE s836(sd) WITH TEXT-e26 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(_ok) = abap_false.
    PERFORM f_check_authority USING wl_cabecalho_0110-bukrs
                                    abap_true
                           CHANGING _ok.
    IF _ok IS INITIAL.
      CLEAR wl_cabecalho_0110.

      IF ( obj_alv_0120 IS NOT INITIAL ).
        CALL METHOD obj_alv_0120->refresh_table_display
          EXPORTING
            is_stable = wl_stable.
      ENDIF.

      IF ( obj_alv_0130 IS NOT INITIAL ).
        CALL METHOD obj_alv_0130->refresh_table_display
          EXPORTING
            is_stable = wl_stable.
      ENDIF.
    ENDIF.

    "Contabilização Apolice.

    IF ( wl_cabecalho_0110-lote IS INITIAL ).
      wl_cabecalho_0110-status = icon_light_out.
    ELSE.

      "Verifica se Doc. Lcto foi estornado.
      IF ( wl_cabecalho_0110-bukrs  IS NOT INITIAL ) AND
         ( wl_cabecalho_0110-lote   IS NOT INITIAL ) AND
         ( wl_cabecalho_0110-belnr  IS INITIAL     ).

        CLEAR: wl_zglt035, vl_doc_delete.
        SELECT SINGLE *
          FROM zglt035
          INTO wl_zglt035
         WHERE bukrs    = wl_cabecalho_0110-bukrs
           AND doc_lcto = wl_cabecalho_0110-doc_lcto.

        IF ( ( sy-subrc EQ 0 ) AND ( wl_zglt035-loekz EQ 'X' ) ) OR (  sy-subrc NE 0 ).
          vl_doc_delete = 'X'.
        ENDIF.

        IF vl_doc_delete IS NOT INITIAL.
          UPDATE zglt050 SET lote        = space
                             doc_lcto    = space
                             dt_lcto_ctb = space
                       WHERE seq_lcto = wl_cabecalho_0110-seq_lcto.

          IF sy-subrc = 0.
            wl_cabecalho_0110-doc_lcto    = space.
            wl_cabecalho_0110-lote        = space.
            wl_cabecalho_0110-belnr       = space.
            wl_cabecalho_0110-status      = icon_red_light.
            wl_cabecalho_0110-dt_lcto_ctb = space.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ( wl_cabecalho_0110-doc_lcto IS NOT INITIAL ) AND
         ( wl_cabecalho_0110-belnr    IS INITIAL     ).

        SELECT SINGLE *
          FROM zglt034 INTO wl_zglt034
         WHERE bukrs = wl_cabecalho_0110-bukrs
           AND lote  = wl_cabecalho_0110-lote.

        go_utils->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_cabecalho_0110-doc_lcto
                                                  i_ano_lcto = wl_zglt034-data_atual(4)
                                        IMPORTING e_zibchv   = wl_zib_chave
                                                  e_ziberr   = wl_zib_erro ).

        IF ( wl_zib_chave IS NOT INITIAL ).
          wl_cabecalho_0110-status = icon_green_light .
          wl_cabecalho_0110-belnr  = wl_zib_chave-belnr.
          UPDATE zglt050 SET belnr = wl_zib_chave-belnr
                       WHERE seq_lcto = wl_cabecalho_0110-seq_lcto.
        ELSEIF ( wl_zib_erro IS NOT INITIAL ).
          wl_cabecalho_0110-status  = icon_red_light.
        ELSE.
          wl_cabecalho_0110-status  = icon_yellow_light.
        ENDIF.
      ELSEIF wl_cabecalho_0110-belnr IS NOT INITIAL.
        wl_cabecalho_0110-status = icon_green_light .
      ENDIF.

    ENDIF.

    wl_editor = wl_cabecalho_0110-observacao.
    APPEND wl_editor TO gt_editor.

    CALL METHOD obj_custom_editor->set_text_as_stream
      EXPORTING
        text = gt_editor.

    SELECT SINGLE *
      FROM zglt064
      INTO wl_zglt064
     WHERE seq_tipo EQ wl_cabecalho_0110-seq_tipo.

    SELECT *
      FROM zglt067
      INTO TABLE gt_zglt067
     WHERE seq_lcto EQ wl_cabecalho_0110-seq_lcto.

* Seleciona os nomes dos match-codes, para exibir no cabeçalho.
    go_utils->z_seleciona_dados_0110( ).

* Seleciona os dados do Contas a Pagar que estão vinculados no cadastro
* do cabeçalho, e exibe essas informações na tela 0120.

    LOOP AT gt_zglt067 INTO wl_zglt067.
      CLEAR: gt_estilo[], wl_saida_0120, return_status.

      return_status = COND #( WHEN autorizado IS INITIAL THEN abap_true ELSE abap_false ).

      IF ( wl_zglt067-lote IS INITIAL ).
        wl_saida_0120-status         = icon_light_out.
      ELSE.
        IF wl_zglt067-dt_lcto_ctb IS INITIAL.
          wl_zglt067-dt_lcto_ctb = wl_zglt067-erdat.
        ENDIF.
        go_utils->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_zglt067-doc_lcto
                                                  i_ano_lcto = wl_zglt067-dt_lcto_ctb(4)
                                        IMPORTING e_zibchv   = wl_zib_chave
                                                  e_ziberr   = wl_zib_erro ).

        IF ( wl_zib_chave IS NOT INITIAL ).
          wl_saida_0120-status       = icon_green_light .
          wl_saida_0120-doc_contabil = wl_zib_chave-belnr.
          wl_saida_0120-dt_lcto_ctb  = wl_zglt067-dt_lcto_ctb.
        ELSEIF ( wl_zib_erro IS NOT INITIAL ).
          wl_saida_0120-status       = icon_red_light.
        ELSE.
          wl_saida_0120-status       = icon_yellow_light.
        ENDIF.

        return_status = 'X'.
      ENDIF.

      wl_saida_0120-seq_lcto       = wl_cabecalho_0110-seq_lcto.
      wl_saida_0120-bukrs          = wl_cabecalho_0110-bukrs.
      wl_saida_0120-filial         = wl_zglt067-werks.
      wl_saida_0120-nro_parc       = wl_zglt067-nro_parc.
      wl_saida_0120-filial         = wl_zglt067-werks.
      wl_saida_0120-taxa_cambio    = wl_zglt067-wkurs.
      wl_saida_0120-vlr_premio_usd = wl_zglt067-vlr_premio_usd.
      wl_saida_0120-vlr_premio_brl = wl_zglt067-vlr_premio_brl.
      wl_saida_0120-dt_venc        = wl_zglt067-dt_venc.
      wl_saida_0120-pais_pgto      = wl_zglt067-banks.
      wl_saida_0120-forma_pgto     = wl_zglt067-zlsch.
      wl_saida_0120-bco_empresa    = wl_zglt067-hbkid.
      wl_saida_0120-bco_parceiro   = wl_zglt067-bvtyp.
      wl_saida_0120-bloq_pgto      = wl_zglt067-zlspr.
      wl_saida_0120-cod_barras     = wl_zglt067-cod_barras.
      wl_saida_0120-lote           = wl_zglt067-lote.
      wl_saida_0120-nro_documento  = wl_zglt067-doc_lcto.

      IF ( return_status IS NOT INITIAL ).
        go_utils->z_style_disable_edit( fieldname = 'BCO_EMPRESA'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'BCO_PARCEIRO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'BLOQ_PGTO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'COD_BARRAS'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'DT_VENC'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'FILIAL'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'FORMA_PGTO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'LOTE'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'NRO_DOCUMENTO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'NRO_PARC'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'PAIS_PGTO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'TAXA_CAMBIO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_BRL'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_USD'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
      ENDIF.

      INSERT LINES OF gt_estilo INTO TABLE wl_saida_0120-estilo.
      APPEND wl_saida_0120 TO gt_saida_0120.
    ENDLOOP.

    IF ( obj_alv_0120 IS NOT INITIAL ).
      CALL METHOD obj_alv_0120->refresh_table_display
        EXPORTING
          is_stable = wl_stable.
    ENDIF.

* Seleciona os dados dos Bens Assegurados que estão vinculados no cadastro
* do cabeçalho, e exibe essas informações na tela 0130.

    CLEAR: wl_saida_0130, gt_estilo[], gt_saida_0130.

    SELECT *
      FROM zglt068
      INTO TABLE gt_zglt068
     WHERE seq_lcto EQ wl_cabecalho_0110-seq_lcto.

    SORT gt_zglt068 BY nr_item.

    LOOP AT gt_zglt068 INTO wl_zglt068.
      CLEAR: gt_estilo[], wl_saida_0130, vl_dt_baixa.

      IF wl_cabecalho_0110-tp_opr NE 'B'.
        SELECT SINGLE a~dt_baixa INTO vl_dt_baixa
          FROM zglt068 AS a INNER JOIN zglt050 AS b ON a~seq_lcto = b~seq_lcto
         WHERE b~tp_opr       = 'B' "Baixa.
           AND b~loekz        = ''
           AND b~ref_seq_lcto = wl_zglt068-seq_lcto
           AND a~nr_item      = wl_zglt068-nr_item
           AND a~dt_baixa     NE '00000000'.

        IF ( sy-subrc = 0 ) AND ( vl_dt_baixa IS NOT INITIAL ).
          wl_saida_0130-st_baixa = icon_checked.
          wl_saida_0130-dt_baixa = vl_dt_baixa.
        ENDIF.
      ELSE.
        wl_saida_0130-dt_baixa      = wl_zglt068-dt_baixa.
      ENDIF.

      wl_cabecalho_0110-seq_lcto    = wl_zglt068-seq_lcto.
      wl_saida_0130-nr_item         = wl_zglt068-nr_item.
      wl_saida_0130-imobilizado     = wl_zglt068-anln1.
      wl_saida_0130-subnumero       = wl_zglt068-anln2.
      wl_saida_0130-mercadoria      = wl_zglt068-matnr.
      wl_saida_0130-descr_bens      = wl_zglt068-descr_bens.
      wl_saida_0130-filial          = wl_zglt068-werks.
      wl_saida_0130-chassi          = wl_zglt068-invnr.
      wl_saida_0130-nr_serie        = wl_zglt068-sernr.
      wl_saida_0130-centro_custo    = wl_zglt068-kostl.
      wl_saida_0130-aufnr           = wl_zglt068-aufnr.
      wl_saida_0130-taxa_cambio     = wl_zglt068-wkurs.
      wl_saida_0130-vlr_premio_usd  = wl_zglt068-vlr_premio_usd.
      wl_saida_0130-vlr_premio_brl  = wl_zglt068-vlr_premio_brl.
      wl_saida_0130-vlr_aj_prem_usd = wl_zglt068-vlr_aj_prem_usd.
      wl_saida_0130-vlr_aj_prem_brl = wl_zglt068-vlr_aj_prem_brl.
      wl_saida_0130-vlr_risco_usd   = wl_zglt068-vlr_risco_usd.
      wl_saida_0130-vlr_risco_brl   = wl_zglt068-vlr_risco_brl.
      wl_saida_0130-dt_inic_vigenc  = wl_zglt068-dt_in_vig.
      wl_saida_0130-banco           = wl_zglt068-banco.

      IF ( wl_zglt068-clau_benef = abap_true ).
        wl_saida_0130-clau_benef = 'SIM'.
      ELSE.
        wl_saida_0130-clau_benef = 'NÃO'.
      ENDIF.


      go_utils->z_style_disable_edit( fieldname = 'BANCO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CENTRO_CUSTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CHASSI'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CLAU_BENEF'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
*      go_utils->z_style_disable_edit( fieldname = 'DESCR_BENS'
*                                     style     = cl_gui_alv_grid=>mc_style_disabled ). "149362 CS2024000693 ZGL047 - 149362- Habilitar edição no campo Desc. Bens - PSA
      go_utils->z_style_disable_edit( fieldname = 'DESCR_BENS'
                                      style     = COND #( WHEN autorizado IS INITIAL THEN cl_gui_alv_grid=>mc_style_disabled ELSE cl_gui_alv_grid=>mc_style_enabled ) ).
      go_utils->z_style_disable_edit( fieldname = 'DT_BAIXA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DT_INIC_VIGENC'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'FILIAL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'IMOBILIZADO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'MERCADORIA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SEQ_LCTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SUBNUMERO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'TAXA_CAMBIO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'UF'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'AUFNR'
                                      style     = COND #( WHEN autorizado IS INITIAL THEN cl_gui_alv_grid=>mc_style_disabled ELSE cl_gui_alv_grid=>mc_style_enabled ) ).
      go_utils->z_style_disable_edit( fieldname = 'VORNR'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).


      INSERT LINES OF gt_estilo INTO TABLE wl_saida_0130-estilo.
      APPEND wl_saida_0130 TO gt_saida_0130.
    ENDLOOP.

*   Desabilita todos os campos do cabeçalho, após os registro serem carregados.
    CLEAR gt_fields.
    go_utils->z_tratar_campos( name      = space
                               group1    = 'GR1'
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    go_utils->z_tratar_campos( name      = space
                               group1    = 'NU1'
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    IF ( obj_alv_0130 IS NOT INITIAL ).
      CALL METHOD obj_alv_0130->refresh_table_display
        EXPORTING
          is_stable = wl_stable.
    ENDIF.

  ENDMETHOD.                    "Z_PESQUISAR_REGISTRO.

  METHOD z_pesquisar_registros_prorrog.

    DATA: lv_seq_lcto   TYPE zseq_lcto,
          vl_dt_baixa   TYPE zglt068-dt_baixa,
          vl_doc_delete TYPE c,
          wl_zglt035    TYPE zglt035.

    op_modo = c_search.

    CREATE OBJECT go_utils.
    CLEAR: gt_saida_0120, gt_saida_0130, gt_editor, wl_editor.

    lv_seq_lcto = wl_cabecalho_0110-ref_seq_lcto. "wl_cabecalho_0110-seq_lcto.
    CLEAR wl_cabecalho_0110.

    SELECT SINGLE *
      FROM zglt050 INTO CORRESPONDING FIELDS OF wl_cabecalho_0110
     WHERE seq_lcto EQ lv_seq_lcto
       AND loekz    EQ ''.

    IF ( sy-subrc IS NOT INITIAL ).
      MESSAGE s836(sd) WITH TEXT-e26 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CLEAR wl_cabecalho_0110-seq_lcto.

    DATA(_ok) = abap_false.
    PERFORM f_check_authority USING wl_cabecalho_0110-bukrs
                                    abap_true
                           CHANGING _ok.
    IF _ok IS INITIAL.
      CLEAR wl_cabecalho_0110.

      IF ( obj_alv_0120 IS NOT INITIAL ).
        CALL METHOD obj_alv_0120->refresh_table_display
          EXPORTING
            is_stable = wl_stable.
      ENDIF.

      IF ( obj_alv_0130 IS NOT INITIAL ).
        CALL METHOD obj_alv_0130->refresh_table_display
          EXPORTING
            is_stable = wl_stable.
      ENDIF.
    ENDIF.

    "Contabilização Apolice.

    IF ( wl_cabecalho_0110-lote IS INITIAL ).
      wl_cabecalho_0110-status = icon_light_out.
    ELSE.

      "Verifica se Doc. Lcto foi estornado.
      IF ( wl_cabecalho_0110-bukrs  IS NOT INITIAL ) AND
         ( wl_cabecalho_0110-lote   IS NOT INITIAL ) AND
         ( wl_cabecalho_0110-belnr  IS INITIAL     ).

        CLEAR: wl_zglt035, vl_doc_delete.
        SELECT SINGLE *
          FROM zglt035
          INTO wl_zglt035
         WHERE bukrs    = wl_cabecalho_0110-bukrs
           AND doc_lcto = wl_cabecalho_0110-doc_lcto.

        IF ( ( sy-subrc EQ 0 ) AND ( wl_zglt035-loekz EQ 'X' ) ) OR (  sy-subrc NE 0 ).
          vl_doc_delete = 'X'.
        ENDIF.

        IF vl_doc_delete IS NOT INITIAL.
          UPDATE zglt050 SET lote        = space
                             doc_lcto    = space
                             dt_lcto_ctb = space
                       WHERE seq_lcto = wl_cabecalho_0110-seq_lcto.

          IF sy-subrc = 0.
            wl_cabecalho_0110-doc_lcto    = space.
            wl_cabecalho_0110-lote        = space.
            wl_cabecalho_0110-belnr       = space.
            wl_cabecalho_0110-status      = icon_red_light.
            wl_cabecalho_0110-dt_lcto_ctb = space.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ( wl_cabecalho_0110-doc_lcto IS NOT INITIAL ) AND
         ( wl_cabecalho_0110-belnr    IS INITIAL     ).

        SELECT SINGLE *
          FROM zglt034 INTO wl_zglt034
         WHERE bukrs = wl_cabecalho_0110-bukrs
           AND lote  = wl_cabecalho_0110-lote.

        go_utils->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_cabecalho_0110-doc_lcto
                                                  i_ano_lcto = wl_zglt034-data_atual(4)
                                        IMPORTING e_zibchv   = wl_zib_chave
                                                  e_ziberr   = wl_zib_erro ).

        IF ( wl_zib_chave IS NOT INITIAL ).
          wl_cabecalho_0110-status = icon_green_light .
          wl_cabecalho_0110-belnr  = wl_zib_chave-belnr.
          UPDATE zglt050 SET belnr = wl_zib_chave-belnr
                       WHERE seq_lcto = wl_cabecalho_0110-seq_lcto.
        ELSEIF ( wl_zib_erro IS NOT INITIAL ).
          wl_cabecalho_0110-status  = icon_red_light.
        ELSE.
          wl_cabecalho_0110-status  = icon_yellow_light.
        ENDIF.
      ELSEIF wl_cabecalho_0110-belnr IS NOT INITIAL.
        wl_cabecalho_0110-status = icon_green_light .
      ENDIF.

    ENDIF.

    wl_editor = wl_cabecalho_0110-observacao.
    APPEND wl_editor TO gt_editor.

    CALL METHOD obj_custom_editor->set_text_as_stream
      EXPORTING
        text = gt_editor.

    SELECT SINGLE *
      FROM zglt064
      INTO wl_zglt064
     WHERE seq_tipo EQ wl_cabecalho_0110-seq_tipo.

    SELECT *
      FROM zglt067
      INTO TABLE gt_zglt067
     WHERE seq_lcto EQ wl_cabecalho_0110-seq_lcto.

* Seleciona os nomes dos match-codes, para exibir no cabeçalho.
    go_utils->z_seleciona_dados_0110( ).

* Seleciona os dados do Contas a Pagar que estão vinculados no cadastro
* do cabeçalho, e exibe essas informações na tela 0120.

    LOOP AT gt_zglt067 INTO wl_zglt067.
      CLEAR: gt_estilo[], wl_saida_0120, return_status.

      return_status = COND #( WHEN autorizado IS INITIAL THEN abap_true ELSE abap_false ).

      IF ( wl_zglt067-lote IS INITIAL ).
        wl_saida_0120-status         = icon_light_out.
      ELSE.
        IF wl_zglt067-dt_lcto_ctb IS INITIAL.
          wl_zglt067-dt_lcto_ctb = wl_zglt067-erdat.
        ENDIF.
        go_utils->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_zglt067-doc_lcto
                                                  i_ano_lcto = wl_zglt067-dt_lcto_ctb(4)
                                        IMPORTING e_zibchv   = wl_zib_chave
                                                  e_ziberr   = wl_zib_erro ).

        IF ( wl_zib_chave IS NOT INITIAL ).
          wl_saida_0120-status       = icon_green_light .
          wl_saida_0120-doc_contabil = wl_zib_chave-belnr.
          wl_saida_0120-dt_lcto_ctb  = wl_zglt067-dt_lcto_ctb.
        ELSEIF ( wl_zib_erro IS NOT INITIAL ).
          wl_saida_0120-status       = icon_red_light.
        ELSE.
          wl_saida_0120-status       = icon_yellow_light.
        ENDIF.

        return_status = 'X'.
      ENDIF.

      wl_saida_0120-seq_lcto       = wl_cabecalho_0110-seq_lcto.
      wl_saida_0120-bukrs          = wl_cabecalho_0110-bukrs.
      wl_saida_0120-filial         = wl_zglt067-werks.
      wl_saida_0120-nro_parc       = wl_zglt067-nro_parc.
      wl_saida_0120-filial         = wl_zglt067-werks.
      wl_saida_0120-taxa_cambio    = wl_zglt067-wkurs.
      wl_saida_0120-vlr_premio_usd = wl_zglt067-vlr_premio_usd.
      wl_saida_0120-vlr_premio_brl = wl_zglt067-vlr_premio_brl.
      wl_saida_0120-dt_venc        = wl_zglt067-dt_venc.
      wl_saida_0120-pais_pgto      = wl_zglt067-banks.
      wl_saida_0120-forma_pgto     = wl_zglt067-zlsch.
      wl_saida_0120-bco_empresa    = wl_zglt067-hbkid.
      wl_saida_0120-bco_parceiro   = wl_zglt067-bvtyp.
      wl_saida_0120-bloq_pgto      = wl_zglt067-zlspr.
      wl_saida_0120-cod_barras     = wl_zglt067-cod_barras.
      wl_saida_0120-lote           = wl_zglt067-lote.
      wl_saida_0120-nro_documento  = wl_zglt067-doc_lcto.

      IF ( return_status IS NOT INITIAL ).
        go_utils->z_style_disable_edit( fieldname = 'BCO_EMPRESA'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'BCO_PARCEIRO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'BLOQ_PGTO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'COD_BARRAS'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'DT_VENC'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'FILIAL'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'FORMA_PGTO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'LOTE'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'NRO_DOCUMENTO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'NRO_PARC'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'PAIS_PGTO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'TAXA_CAMBIO'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_BRL'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
        go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_USD'
                                        style     = cl_gui_alv_grid=>mc_style_disabled ).
      ENDIF.

      INSERT LINES OF gt_estilo INTO TABLE wl_saida_0120-estilo.
      APPEND wl_saida_0120 TO gt_saida_0120.
    ENDLOOP.

    IF ( obj_alv_0120 IS NOT INITIAL ).
      CALL METHOD obj_alv_0120->refresh_table_display
        EXPORTING
          is_stable = wl_stable.
    ENDIF.

* Seleciona os dados dos Bens Assegurados que estão vinculados no cadastro
* do cabeçalho, e exibe essas informações na tela 0130.

    CLEAR: wl_saida_0130, gt_estilo[], gt_saida_0130.

    SELECT *
      FROM zglt068
      INTO TABLE gt_zglt068
     WHERE seq_lcto EQ wl_cabecalho_0110-seq_lcto.

    SORT gt_zglt068 BY nr_item.

    LOOP AT gt_zglt068 INTO wl_zglt068.
      CLEAR: gt_estilo[], wl_saida_0130, vl_dt_baixa.

      IF wl_cabecalho_0110-tp_opr NE 'B'.
        SELECT SINGLE a~dt_baixa INTO vl_dt_baixa
          FROM zglt068 AS a INNER JOIN zglt050 AS b ON a~seq_lcto = b~seq_lcto
         WHERE b~tp_opr       = 'B' "Baixa.
           AND b~loekz        = ''
           AND b~ref_seq_lcto = wl_zglt068-seq_lcto
           AND a~nr_item      = wl_zglt068-nr_item
           AND a~dt_baixa     NE '00000000'.

        IF ( sy-subrc = 0 ) AND ( vl_dt_baixa IS NOT INITIAL ).
          wl_saida_0130-st_baixa = icon_checked.
          wl_saida_0130-dt_baixa = vl_dt_baixa.
        ENDIF.
      ELSE.
        wl_saida_0130-dt_baixa      = wl_zglt068-dt_baixa.
      ENDIF.

      wl_cabecalho_0110-seq_lcto    = wl_zglt068-seq_lcto.
      wl_saida_0130-nr_item         = wl_zglt068-nr_item.
      wl_saida_0130-imobilizado     = wl_zglt068-anln1.
      wl_saida_0130-subnumero       = wl_zglt068-anln2.
      wl_saida_0130-mercadoria      = wl_zglt068-matnr.
      wl_saida_0130-descr_bens      = wl_zglt068-descr_bens.
      wl_saida_0130-filial          = wl_zglt068-werks.
      wl_saida_0130-chassi          = wl_zglt068-invnr.
      wl_saida_0130-nr_serie        = wl_zglt068-sernr.
      wl_saida_0130-centro_custo    = wl_zglt068-kostl.
      wl_saida_0130-aufnr           = wl_zglt068-aufnr.
      wl_saida_0130-taxa_cambio     = wl_zglt068-wkurs.
      wl_saida_0130-vlr_premio_usd  = wl_zglt068-vlr_premio_usd.
      wl_saida_0130-vlr_premio_brl  = wl_zglt068-vlr_premio_brl.
      wl_saida_0130-vlr_aj_prem_usd = wl_zglt068-vlr_aj_prem_usd.
      wl_saida_0130-vlr_aj_prem_brl = wl_zglt068-vlr_aj_prem_brl.
      wl_saida_0130-vlr_risco_usd   = wl_zglt068-vlr_risco_usd.
      wl_saida_0130-vlr_risco_brl   = wl_zglt068-vlr_risco_brl.
      wl_saida_0130-dt_inic_vigenc  = wl_zglt068-dt_in_vig.
      wl_saida_0130-banco           = wl_zglt068-banco.

      IF ( wl_zglt068-clau_benef = abap_true ).
        wl_saida_0130-clau_benef = 'SIM'.
      ELSE.
        wl_saida_0130-clau_benef = 'NÃO'.
      ENDIF.


      go_utils->z_style_disable_edit( fieldname = 'BANCO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CENTRO_CUSTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CHASSI'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CLAU_BENEF'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DESCR_BENS'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DT_BAIXA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DT_INIC_VIGENC'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'FILIAL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'IMOBILIZADO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'MERCADORIA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SEQ_LCTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SUBNUMERO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'TAXA_CAMBIO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'UF'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'AUFNR'
                                      style     = COND #( WHEN autorizado IS INITIAL THEN cl_gui_alv_grid=>mc_style_disabled ELSE cl_gui_alv_grid=>mc_style_enabled ) ).
      go_utils->z_style_disable_edit( fieldname = 'VORNR'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).


      INSERT LINES OF gt_estilo INTO TABLE wl_saida_0130-estilo.
      APPEND wl_saida_0130 TO gt_saida_0130.
    ENDLOOP.

*   Desabilita todos os campos do cabeçalho, após os registro serem carregados.
    CLEAR gt_fields.
    go_utils->z_tratar_campos( name      = space
                               group1    = 'GR1'
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    go_utils->z_tratar_campos( name      = space
                               group1    = 'NU1'
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    IF ( obj_alv_0130 IS NOT INITIAL ).
      CALL METHOD obj_alv_0130->refresh_table_display
        EXPORTING
          is_stable = wl_stable.
    ENDIF.

  ENDMETHOD.                    "Z_PESQUISAR_REGISTRO.

  METHOD z_salvar_registros.

    DATA: gt_068     TYPE TABLE OF zglt068,
          wl_068     TYPE zglt068,
          vl_nr_item TYPE zglt068-nr_item,
          vl_error   TYPE c.

    DATA: go_utils  TYPE REF TO zutils.

    REFRESH: gt_zglt067, gt_zglt068.

    op_modo = c_save.

    CREATE OBJECT: go_utils.

    "Check de permissão de Empresa
    DATA(_ok) = abap_false.
    PERFORM f_check_authority USING wl_cabecalho_0110-bukrs
                                    abap_true
                           CHANGING _ok.
    CHECK _ok IS NOT INITIAL.

    IF ( wl_cabecalho_0110-seq_lcto IS INITIAL ).
      go_utils->z_number_get_next( ).
    ENDIF.

    CLEAR wl_cabecalho_0110-observacao.
    LOOP AT gt_editor INTO wl_editor.
      CONCATENATE wl_cabecalho_0110-observacao wl_editor INTO wl_cabecalho_0110-observacao.
    ENDLOOP.

    custom_mode = 1.
*   -

    SELECT SINGLE *
      FROM zglt064
      INTO wl_zglt064
     WHERE seq_tipo = wl_cabecalho_0110-seq_tipo.

    wl_cabecalho_0110-dt_criacao   = sy-datum.
    wl_cabecalho_0110-hr_criacao   = sy-uzeit.
    wl_cabecalho_0110-tp_lcto      = wl_zglt064-tp_lcto.
    wl_cabecalho_0110-dep_resp     = wl_zglt064-dep_resp.
*    WL_CABECALHO_0110-DT_ALTERACAO = SY-DATUM.
*    WL_CABECALHO_0110-HR_ULT_ALT   = SY-UZEIT.
    wl_cabecalho_0110-usnam        = sy-uname.

    CLEAR: wl_zglt050.
    MOVE-CORRESPONDING wl_cabecalho_0110 TO wl_zglt050.

    MODIFY zglt050 FROM wl_zglt050.
    COMMIT WORK.

    "USER STORY 163032 - MMSILVA - 09.01.2025 - Inicio
    IF gt_zglt050 IS NOT INITIAL.
      MODIFY gt_zglt050 FROM wl_zglt050 INDEX sy-tabix.
      COMMIT WORK.
    ENDIF.
    "USER STORY 163032 - MMSILVA - 09.01.2025 - Fim

*   Pega os dados do Contas a Pagar que estão na tabela de saída
*   e grava na tabela ZGLT067.
    DELETE FROM zglt067 WHERE seq_lcto = wl_cabecalho_0110-seq_lcto.

    LOOP AT gt_saida_0120 INTO wl_saida_0120.
      CLEAR: wl_zglt067, gt_estilo[], wl_saida_0120-estilo.

      wl_zglt067-seq_lcto       = wl_cabecalho_0110-seq_lcto.
      wl_zglt067-nro_parc       = wl_saida_0120-nro_parc.
      wl_zglt067-werks          = wl_saida_0120-filial.
      wl_zglt067-wkurs          = wl_saida_0120-taxa_cambio.
      wl_zglt067-vlr_premio_usd = wl_saida_0120-vlr_premio_usd.
      wl_zglt067-vlr_premio_brl = wl_saida_0120-vlr_premio_brl.
      wl_zglt067-dt_venc        = wl_saida_0120-dt_venc.
      wl_zglt067-hbkid          = wl_saida_0120-bco_empresa.
      wl_zglt067-bvtyp          = wl_saida_0120-bco_parceiro.
      wl_zglt067-zlsch          = wl_saida_0120-forma_pgto.
      wl_zglt067-zlspr          = wl_saida_0120-bloq_pgto.
      wl_zglt067-banks          = wl_saida_0120-pais_pgto.
      wl_zglt067-cod_barras     = wl_saida_0120-cod_barras.
      wl_zglt067-lote           = wl_saida_0120-lote.
      wl_zglt067-doc_lcto       = wl_saida_0120-nro_documento.
      wl_zglt067-ernam          = sy-uname.
      wl_zglt067-erdat          = sy-datum.
      APPEND wl_zglt067 TO gt_zglt067.

      go_utils->z_style_disable_edit( fieldname = 'BCO_EMPRESA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'BCO_PARCEIRO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'BLOQ_PGTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'COD_BARRAS'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DT_VENC'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'FILIAL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'FORMA_PGTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'LOTE'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'NRO_DOCUMENTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'NRO_PARC'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'PAIS_PGTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'TAXA_CAMBIO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).

      INSERT LINES OF gt_estilo INTO TABLE wl_saida_0120-estilo.
      MODIFY gt_saida_0120 FROM wl_saida_0120.
    ENDLOOP.

    MODIFY zglt067 FROM TABLE gt_zglt067.
    COMMIT WORK.

*   Pega os dados do Bens Assegurados que foram excluidos e deleta da tabela.
    SELECT *
      FROM zglt068 INTO TABLE gt_068
     WHERE seq_lcto = wl_cabecalho_0110-seq_lcto.

    LOOP AT gt_068 INTO wl_068.

      READ TABLE gt_saida_0130 INTO wl_saida_0130
        WITH KEY nr_item     = wl_068-nr_item
                 imobilizado = wl_068-anln1
                 subnumero   = wl_068-anln2
                 mercadoria  = wl_068-matnr.

      IF sy-subrc NE 0.
        DELETE FROM zglt068 WHERE seq_lcto = wl_068-seq_lcto
                              AND nr_item  = wl_068-nr_item
                              AND anln1    = wl_068-anln1
                              AND anln2    = wl_068-anln2
                              AND matnr    = wl_068-matnr.
      ENDIF.

    ENDLOOP.

*   Pega os dados do Bens Assegurados que estão na tabela de saída
*   e grava na tabela ZGLT068.

    CLEAR: vl_nr_item.

    LOOP AT gt_saida_0130 INTO wl_saida_0130.
      CLEAR: wl_zglt068, gt_estilo[], wl_saida_0130-estilo.

      wl_zglt068-seq_lcto        = wl_cabecalho_0110-seq_lcto.
      wl_zglt068-nr_item         = wl_saida_0130-nr_item.

      IF ( wl_zglt068-nr_item IS INITIAL ).

        IF vl_nr_item IS INITIAL.

          SELECT SINGLE MAX( nr_item )
            INTO vl_nr_item
            FROM zglt068
           WHERE seq_lcto = wl_cabecalho_0110-seq_lcto.

          IF vl_nr_item IS INITIAL .
            vl_nr_item  = 0.
          ENDIF.

        ENDIF.

        ADD 1 TO vl_nr_item.

        wl_zglt068-nr_item       = vl_nr_item.
        wl_saida_0130-nr_item    = wl_zglt068-nr_item.

      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_saida_0130-centro_custo
        IMPORTING
          output = wl_saida_0130-centro_custo.

      wl_zglt068-anln1           = wl_saida_0130-imobilizado.
      wl_zglt068-anln2           = wl_saida_0130-subnumero.
      wl_zglt068-matnr           = wl_saida_0130-mercadoria.
      wl_zglt068-descr_bens      = wl_saida_0130-descr_bens.
      wl_zglt068-werks           = wl_saida_0130-filial.
      wl_zglt068-invnr           = wl_saida_0130-chassi.
      wl_zglt068-sernr           = wl_saida_0130-nr_serie.
      wl_zglt068-kostl           = wl_saida_0130-centro_custo.
      wl_zglt068-aufnr           = wl_saida_0130-aufnr.
      wl_zglt068-vornr           = wl_saida_0130-vornr.
      wl_zglt068-wkurs           = wl_saida_0130-taxa_cambio.
      wl_zglt068-vlr_premio_usd  = wl_saida_0130-vlr_premio_usd.
      wl_zglt068-vlr_premio_brl  = wl_saida_0130-vlr_premio_brl.
      wl_zglt068-vlr_aj_prem_usd = wl_saida_0130-vlr_aj_prem_usd.
      wl_zglt068-vlr_aj_prem_brl = wl_saida_0130-vlr_aj_prem_brl.
      wl_zglt068-vlr_risco_usd   = wl_saida_0130-vlr_risco_usd.
      wl_zglt068-vlr_risco_brl   = wl_saida_0130-vlr_risco_brl.
      wl_zglt068-dt_in_vig       = wl_saida_0130-dt_inic_vigenc.
      wl_zglt068-dt_baixa        = wl_saida_0130-dt_baixa.

      IF wl_cabecalho_0110-tp_opr NE 'B'.
        CLEAR: wl_zglt068-dt_baixa.
      ENDIF.

      wl_zglt068-banco           = wl_saida_0130-banco.

      IF ( wl_saida_0130-clau_benef(3) = 'SIM' ).
        wl_zglt068-clau_benef = abap_true.
      ELSE.
        wl_zglt068-clau_benef = abap_false.
      ENDIF.

      APPEND wl_zglt068 TO gt_zglt068.

      go_utils->z_style_disable_edit( fieldname = 'AUFNR'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'BANCO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CENTRO_CUSTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CHASSI'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CLAU_BENEF'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DESCR_BENS'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DT_BAIXA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DT_INIC_VIGENC'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'FILIAL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'IMOBILIZADO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'MERCADORIA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SEQ_LCTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SUBNUMERO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'TAXA_CAMBIO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'UF'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VORNR'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).


      INSERT LINES OF gt_estilo INTO TABLE wl_saida_0130-estilo.
      MODIFY gt_saida_0130 FROM wl_saida_0130.
    ENDLOOP.

    MODIFY zglt068 FROM TABLE gt_zglt068.
    COMMIT WORK.

*   Desabilita todos os campos do cabeçalho, após os registro serem carregados.
    CLEAR gt_fields.
    go_utils->z_tratar_campos( name      = space
                               group1    = 'GR1'
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    go_utils->z_tratar_campos( name      = space
                               group1    = 'NU1'
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

*    CLEAR: GT_SAIDA_0120, GT_SAIDA_0130.
  ENDMETHOD.                    "Z_SALVAR_REGISTROS

  METHOD z_check_operacao.

    TYPES: BEGIN OF ty_050,
             seq_lcto    TYPE zglt050-seq_lcto,
             bukrs       TYPE zglt050-bukrs,
             nro_apolice TYPE zglt050-nro_apolice,
             dt_criacao  TYPE zglt050-dt_criacao,
             usnam       TYPE zglt050-usnam,
           END OF ty_050.

    DATA: gt_return_tab TYPE TABLE OF ddshretval,
          wl_return_tab TYPE ddshretval,
          gt_dselc      TYPE TABLE OF dselc,
          wl_dselc      TYPE dselc,
          gt_050        TYPE TABLE OF ty_050.

    DATA: wl_0050 TYPE zglt050.

    CREATE OBJECT go_utils.

    REFRESH: gt_050, gt_return_tab, gt_dselc.
    CLEAR: gt_050, wl_return_tab, wl_dselc.

    CASE wl_cabecalho_0110-tp_opr.

      WHEN 'E' OR 'B'. " Endosso ou Baixa.

        IF wl_cabecalho_0110-bukrs IS INITIAL.
          me->z_clear_referencia( ).
          MESSAGE TEXT-e39 TYPE 'S'.
          RETURN.
        ENDIF.

        SELECT *
          FROM zglt050 INTO CORRESPONDING FIELDS OF TABLE gt_050
         WHERE bukrs = wl_cabecalho_0110-bukrs
           AND tp_opr NE 'B'
           AND loekz  EQ ''.

        SORT gt_050 BY seq_lcto DESCENDING.

        CLEAR wl_dselc.
        wl_dselc-fldname = 'F0001'.
        wl_dselc-dyfldname = 'SEQ_LCTO'.
        APPEND wl_dselc TO gt_dselc.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'SEQ_LCTO'
            value_org       = 'S'
          TABLES
            value_tab       = gt_050
            return_tab      = gt_return_tab
            dynpfld_mapping = gt_dselc.

        CLEAR: wl_0050, wl_return_tab.
        READ TABLE gt_return_tab INTO wl_return_tab WITH KEY fieldname = 'F0001'.
        IF wl_return_tab IS INITIAL.
          me->z_clear_referencia( ).
          RETURN.
        ENDIF.

        wl_cabecalho_0110-ref_seq_lcto = wl_return_tab-fieldval.

        SELECT SINGLE *
          FROM zglt050 INTO wl_0050
         WHERE seq_lcto = wl_cabecalho_0110-ref_seq_lcto
           AND loekz    EQ ''.

        IF ( sy-subrc NE 0                             ) OR
           ( wl_cabecalho_0110-ref_seq_lcto IS INITIAL ) OR
           ( wl_0050-nro_apolice            IS INITIAL ) OR
           ( wl_0050-vig_de                 IS INITIAL ) OR
           ( wl_0050-vig_ate                IS INITIAL ) OR
           ( wl_0050-cod_seguradora         IS INITIAL ) OR
           ( wl_0050-waers                  IS INITIAL ).
          me->z_clear_referencia( ).
          RETURN.
        ENDIF.

        go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-BUKRS'
                                   group1    = space
                                   group2    = space
                                   value     = '0'
                                   invisible = '0' ).

*        go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-SEQ_ENDOSSO'
*                                   group1    = space
*                                   group2    = space
*                                   value     = '0'
*                                   invisible = '0' ).

        go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-TP_OPR'
                                   group1    = space
                                   group2    = space
                                   value     = '0'
                                   invisible = '0' ).

        go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-NRO_APOLICE'
                                   group1    = space
                                   group2    = space
                                   value     = '0'
                                   invisible = '0' ).

        go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-COD_SEGURADORA'
                                   group1    = space
                                   group2    = space
                                   value     = '0'
                                   invisible = '0' ).

        go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-WAERS'
                                   group1    = space
                                   group2    = space
                                   value     = '0'
                                   invisible = '0' ).

        CASE wl_cabecalho_0110-tp_opr.
          WHEN 'E'. "Endosso
            wl_cabecalho_0110-vig_de   = sy-datum.
            wl_cabecalho_0110-vig_ate  = wl_0050-vig_ate.
            wl_cabecalho_0110-wkurs    = wl_0050-wkurs.
            wl_cabecalho_0110-seq_tipo = wl_0050-seq_tipo.
            wl_cabecalho_0110-seq_parc = 1.

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VIG_ATE'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).

*            go_utils->z_tratar_campos(  name      = 'WL_CABECALHO_0110-SEQ_ENDOSSO'
*                                        group1    = space
*                                        group2    = space
*                                        value     = '0'
*                                        invisible = '0' ).

*            LOOP AT SCREEN.
** RJF
*              IF sy-ucomm EQ 'NOVO' AND screen-name = 'WL_CABECALHO_0110-SEQ_ENDOSSO'.
*                BREAK-POINT.
*                screen-input        = '0'.
*                MODIFY SCREEN.
*              ENDIF.
*
**CHANGE
*              IF sy-ucomm EQ 'CHANGE' AND screen-name = 'WL_CABECALHO_0110-SEQ_ENDOSSO'.
*                BREAK-POINT.
*                screen-input        = '1'.
*                MODIFY SCREEN.
*              ENDIF.
*
*            ENDLOOP.


          WHEN 'B'. "Baixa
            wl_cabecalho_0110-vig_de   = wl_0050-vig_de.
            wl_cabecalho_0110-vig_ate  = sy-datum.
            wl_cabecalho_0110-wkurs    = wl_0050-wkurs.
            wl_cabecalho_0110-seq_tipo = wl_0050-seq_tipo.
            wl_cabecalho_0110-seq_parc = 1.

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VIG_DE'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).

        ENDCASE.

        wl_cabecalho_0110-nro_apolice    = wl_0050-nro_apolice .
        wl_cabecalho_0110-cod_seguradora = wl_0050-cod_seguradora.
        wl_cabecalho_0110-waers          = wl_0050-waers.

        IF ( wl_cabecalho_0110-waers = 'BRL' ).

          CLEAR: wl_cabecalho_0110-vlr_premio_usd,
                 wl_cabecalho_0110-vlr_asseg_usd.

          go_utils->z_tratar_campos( name      = space
                                     group1    = space
                                     group2    = 'GR3'
                                     value     = '0'
                                     invisible = '0' ).

        ELSEIF ( wl_cabecalho_0110-waers = 'USD').

          CLEAR: wl_cabecalho_0110-vlr_premio_brl,
                 wl_cabecalho_0110-vlr_asseg_brl.

          go_utils->z_tratar_campos( name      = space
                                     group1    = space
                                     group2    = 'GR2'
                                     value     = '0'
                                     invisible = '0' ).
        ENDIF.

        wl_cabecalho_0110-inf_tp_opr   = 'X'.

      WHEN 'N'.
        go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-TP_OPR'
                                   group1    = space
                                   group2    = space
                                   value     = '0'
                                   invisible = '0' ).

      WHEN 'P'.

*        IF wl_cabecalho_0110-bukrs IS INITIAL.
*          me->z_clear_referencia( ).
*          MESSAGE text-e39 TYPE 'S'.
*          RETURN.
*        ENDIF.

    ENDCASE.


  ENDMETHOD.

  METHOD z_check_operacao_prorrog.

    TYPES: BEGIN OF ty_050,
             seq_lcto    TYPE zglt050-seq_lcto,
             bukrs       TYPE zglt050-bukrs,
             nro_apolice TYPE zglt050-nro_apolice,
             dt_criacao  TYPE zglt050-dt_criacao,
             usnam       TYPE zglt050-usnam,
           END OF ty_050.

    DATA: gt_return_tab TYPE TABLE OF ddshretval,
          wl_return_tab TYPE ddshretval,
          gt_dselc      TYPE TABLE OF dselc,
          wl_dselc      TYPE dselc,
          gt_050        TYPE TABLE OF ty_050.

    DATA: wl_0050 TYPE zglt050.

    CREATE OBJECT go_utils.

    REFRESH: gt_050, gt_return_tab, gt_dselc.
    CLEAR: gt_050, wl_return_tab, wl_dselc.

*    CASE wl_cabecalho_0110-tp_opr.
*
*      WHEN 'E' OR 'B'. " Endosso ou Baixa.

    IF wl_cabecalho_0110-bukrs IS INITIAL.
      me->z_clear_referencia( ).
      MESSAGE TEXT-e39 TYPE 'S'.
      RETURN.
    ENDIF.

    SELECT *
      FROM zglt050 INTO CORRESPONDING FIELDS OF TABLE gt_050
     WHERE bukrs = wl_cabecalho_0110-bukrs
       AND tp_opr             NE 'B'
       AND loekz              EQ ''
       AND bukrs              NE space
       AND cod_seguradora     NE space
       AND cod_corretora      NE space
       AND waers              NE space
       AND vlr_asseg_usd      NE space
       AND vlr_asseg_brl      NE space
       AND vlr_lmi_usd        NE space
       AND vlr_lmi_brl        NE space
       AND seq_tipo           NE space
       AND perc_comissao      NE space.

    SORT gt_050 BY seq_lcto DESCENDING.

    CLEAR wl_dselc.
    wl_dselc-fldname = 'F0001'.
    wl_dselc-dyfldname = 'SEQ_LCTO'.
    APPEND wl_dselc TO gt_dselc.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'SEQ_LCTO'
        value_org       = 'S'
      TABLES
        value_tab       = gt_050
        return_tab      = gt_return_tab
        dynpfld_mapping = gt_dselc.

    CLEAR: wl_0050, wl_return_tab.
    READ TABLE gt_return_tab INTO wl_return_tab WITH KEY fieldname = 'F0001'.
    IF wl_return_tab IS INITIAL.
      me->z_clear_referencia( ).
      RETURN.
    ENDIF.

    wl_cabecalho_0110-ref_seq_lcto = wl_return_tab-fieldval.

    SELECT SINGLE *
      FROM zglt050 INTO wl_0050
     WHERE seq_lcto = wl_cabecalho_0110-ref_seq_lcto.

    IF ( sy-subrc NE 0                             ) OR
       ( wl_cabecalho_0110-ref_seq_lcto IS INITIAL ) OR
       ( wl_0050-bukrs                  IS INITIAL ) OR
       ( wl_0050-cod_seguradora         IS INITIAL ) OR
       ( wl_0050-cod_corretora          IS INITIAL ) OR
       ( wl_0050-waers                  IS INITIAL ) OR
       ( wl_0050-vlr_asseg_usd          IS INITIAL ) OR
       ( wl_0050-vlr_asseg_brl          IS INITIAL ) OR
       ( wl_0050-vlr_lmi_usd            IS INITIAL ) OR
       ( wl_0050-vlr_lmi_brl            IS INITIAL ) OR
       ( wl_0050-seq_tipo               IS INITIAL ) OR
       ( wl_0050-perc_comissao          IS INITIAL ).

      me->z_clear_referencia( ).
      RETURN.
    ENDIF.

    go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-BUKRS'
                               group1    = space
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-COD_SEGURADORA'
                               group1    = space
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-COD_CORRETORA'
                               group1    = space
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

*    go_utils->z_tratar_campos(  name      = 'WL_CABECALHO_0110-SEQ_ENDOSSO'
*                                group1    = space
*                                group2    = space
*                                value     = '0'
*                                invisible = '0' ).

    go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-WAERS'
                               group1    = space
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_ASSEG_USD'
                               group1    = space
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_ASSEG_BRL'
                               group1    = space
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_LMI_USD'
                               group1    = space
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_LMI_BRL'
                               group1    = space
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-SEQ_TIPO'
                               group1    = space
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-PERC_COMISSAO'
                               group1    = space
                               group2    = space
                               value     = '0'
                               invisible = '0' ).

    wl_cabecalho_0110-vig_de   = wl_0050-vig_de.
    wl_cabecalho_0110-vig_ate  = sy-datum.
    wl_cabecalho_0110-wkurs    = wl_0050-wkurs.
    wl_cabecalho_0110-seq_tipo = wl_0050-seq_tipo.
    wl_cabecalho_0110-seq_parc = 1.

    "Empresa
    wl_cabecalho_0110-bukrs = wl_0050-bukrs.

    "seguradora
    wl_cabecalho_0110-cod_seguradora = wl_0050-cod_seguradora.

    "corretora
    wl_cabecalho_0110-cod_corretora = wl_0050-cod_corretora.

    "moeda
    wl_cabecalho_0110-waers  = wl_0050-waers.

    "Valor Assegurado US$
    wl_cabecalho_0110-vlr_asseg_usd = wl_0050-vlr_asseg_usd.

    "Valor Assegurado R$
    wl_cabecalho_0110-vlr_asseg_brl = wl_0050-vlr_asseg_brl.

    "Limite Máximo Indeniz US$
    wl_cabecalho_0110-vlr_lmi_usd   = wl_0050-vlr_lmi_usd.

    "Limite Máximo Indeniz R$
    wl_cabecalho_0110-vlr_lmi_brl   = wl_0050-vlr_lmi_brl.

    "Tipo
    wl_cabecalho_0110-seq_tipo      = wl_0050-seq_tipo.

    "Comissão
    wl_cabecalho_0110-perc_comissao = wl_0050-perc_comissao.


*    WL_CABECALHO_0110-SEQ_PARC  = WL_0050-SEQ_PARC.


*        CASE wl_cabecalho_0110-tp_opr.
*          WHEN 'E'. "Endosso
*            wl_cabecalho_0110-vig_de   = sy-datum.
*            wl_cabecalho_0110-vig_ate  = wl_0050-vig_ate.
*            wl_cabecalho_0110-wkurs    = wl_0050-wkurs.
*            wl_cabecalho_0110-seq_tipo = wl_0050-seq_tipo.
*            wl_cabecalho_0110-seq_parc = 1.
*
*            go_utils->z_tratar_campos(  name      = 'WL_CABECALHO_0110-VIG_ATE'
*                                        group1    = space
*                                        group2    = space
*                                        value     = '0'
*                                        invisible = '0' ).
*
*          WHEN 'B'. "Baixa
*            wl_cabecalho_0110-vig_de   = wl_0050-vig_de.
*            wl_cabecalho_0110-vig_ate  = sy-datum.
*            wl_cabecalho_0110-wkurs    = wl_0050-wkurs.
*            wl_cabecalho_0110-seq_tipo = wl_0050-seq_tipo.
*            wl_cabecalho_0110-seq_parc = 1.
*
*            go_utils->z_tratar_campos(  name      = 'WL_CABECALHO_0110-VIG_DE'
*                                        group1    = space
*                                        group2    = space
*                                        value     = '0'
*                                        invisible = '0' ).
*
*        ENDCASE.
*
*    wl_cabecalho_0110-nro_apolice    = wl_0050-nro_apolice .
*    wl_cabecalho_0110-cod_seguradora = wl_0050-cod_seguradora.
*    wl_cabecalho_0110-waers          = wl_0050-waers.

    IF ( wl_cabecalho_0110-waers = 'BRL' ).

      CLEAR: wl_cabecalho_0110-vlr_premio_usd,
             wl_cabecalho_0110-vlr_asseg_usd.

      go_utils->z_tratar_campos( name      = space
                                 group1    = space
                                 group2    = 'GR3'
                                 value     = '0'
                                 invisible = '0' ).

    ELSEIF ( wl_cabecalho_0110-waers = 'USD').

      CLEAR: wl_cabecalho_0110-vlr_premio_brl,
             wl_cabecalho_0110-vlr_asseg_brl.

      go_utils->z_tratar_campos( name      = space
                                 group1    = space
                                 group2    = 'GR2'
                                 value     = '0'
                                 invisible = '0' ).
    ENDIF.


    wl_cabecalho_0110-inf_tp_opr   = 'X'.
*
*      WHEN 'N'.
*        go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-TP_OPR'
*                                   group1    = space
*                                   group2    = space
*                                   value     = '0'
*                                   invisible = '0' ).
*
*    ENDCASE.


  ENDMETHOD.

  METHOD z_clear_referencia.
    CLEAR: wl_cabecalho_0110-inf_tp_opr,
           wl_cabecalho_0110-tp_opr,
           wl_cabecalho_0110-ref_seq_lcto,
           wl_cabecalho_0110-nro_apolice,
           wl_cabecalho_0110-cod_seguradora,
           wl_cabecalho_0110-waers.
  ENDMETHOD.

  METHOD z_modo_edicao.

    DATA: vl_seq_lcto_ref TYPE zglt050-seq_lcto,
          vl_ctb_pag_rec  TYPE zglt067-lote,
          vl_ctb_aprop    TYPE zglt073-lote,
          vl_lote_null    TYPE zglt067-lote.

    CREATE OBJECT: go_utils.

    CLEAR: vl_seq_lcto_ref, vl_ctb_pag_rec, vl_ctb_aprop, vl_lote_null.

    edit = abap_false.

    "Caso tenha uma outra apólice referenciando-a. (Endosso/Baixa)
    SELECT SINGLE seq_lcto
      FROM zglt050 INTO vl_seq_lcto_ref
     WHERE ref_seq_lcto = wl_cabecalho_0110-seq_lcto
       AND loekz EQ ''.

    "Caso já tenha gerado doc. Lcto Ctb. p/ C.Pagar/C.Receber.
    SELECT SINGLE lote
      FROM zglt067 INTO vl_ctb_pag_rec
     WHERE seq_lcto = wl_cabecalho_0110-seq_lcto
       AND lote     NE vl_lote_null.

    "Caso já tenha gerado doc. Lcto Ctb. p/ Apropriações
    SELECT SINGLE lote
      FROM zglt073 INTO vl_ctb_aprop
     WHERE seq_lcto = wl_cabecalho_0110-seq_lcto
       AND lote     NE vl_lote_null.

    SELECT SINGLE *
      FROM  zglt067 INTO @DATA(wa_zglt067)
     WHERE seq_lcto EQ @wl_cabecalho_0110-seq_lcto.



    CASE wl_cabecalho_0110-tp_opr.
      WHEN 'N' OR 'E' OR 'P' OR space . "Novo/Endosso

        CLEAR: gt_fields[].

        go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-BUKRS'
                                   group1    = space
                                   group2    = space
                                   value     = '0'
                                   invisible = '0' ).

*            go_utils->z_tratar_campos(  name      = 'WL_CABECALHO_0110-SEQ_ENDOSSO'
*                                        group1    = space
*                                        group2    = space
*                                        value     = '0'
*                                        invisible = '0' ).


        go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-TP_OPR'
                                   group1    = space
                                   group2    = space
                                   value     = '0'
                                   invisible = '0' ).


        IF vl_seq_lcto_ref  IS NOT INITIAL.
          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-NRO_APOLICE'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).
        ENDIF.

        IF ( vl_seq_lcto_ref  IS NOT INITIAL ) OR
           ( vl_ctb_pag_rec   IS NOT INITIAL ).

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-COD_SEGURADORA'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).
        ENDIF.

        IF ( vl_seq_lcto_ref        IS NOT INITIAL ) OR
           ( vl_ctb_pag_rec         IS NOT INITIAL ) OR
           ( vl_ctb_aprop           IS NOT INITIAL ) OR
           ( wl_cabecalho_0110-lote IS NOT INITIAL ).

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-WAERS'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).

          IF vl_ctb_aprop IS NOT INITIAL.
            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VIG_DE'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VIG_ATE'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).
          ENDIF.

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-WKURS'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-SEQ_ENDOSSO'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_PREMIO_BRL'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_PREMIO_USD'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_ASSEG_USD'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_ASSEG_BRL'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_LMI_USD'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_LMI_BRL'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).

        ELSE.

          IF ( wl_cabecalho_0110-waers = 'BRL' ).
            CLEAR: wl_cabecalho_0110-vlr_premio_usd,
                   wl_cabecalho_0110-vlr_asseg_usd.

            go_utils->z_tratar_campos( name      = space
                                       group1    = space
                                       group2    = 'GR3'
                                       value     = '0'
                                       invisible = '0' ).

          ELSEIF ( wl_cabecalho_0110-waers = 'USD').
            CLEAR: wl_cabecalho_0110-vlr_premio_brl,
                   wl_cabecalho_0110-vlr_asseg_brl.

            go_utils->z_tratar_campos( name      = space
                                       group1    = space
                                       group2    = 'GR2'
                                       value     = '0'
                                       invisible = '0' ).
          ENDIF.

        ENDIF.


        IF  wl_cabecalho_0110-lote IS NOT INITIAL AND wl_cabecalho_0110-doc_lcto IS NOT INITIAL AND wl_cabecalho_0110-belnr IS NOT INITIAL.

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-SEQ_PARC'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).


          "USER STORY 163032 - MMSILVA - 09.01.2025 - Inicio
          FREE: r_parid, _param.

          CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
            EXPORTING
              user_name           = sy-uname
            TABLES
              user_parameters     = _param
            EXCEPTIONS
              user_name_not_exist = 1
              OTHERS              = 2.


          IF _param IS NOT INITIAL.
            LOOP AT _param ASSIGNING FIELD-SYMBOL(<ws_param_nep>).
              IF <ws_param_nep>-parid = 'ZGL047_EDIT_TIPO'.
                edit_apolice = abap_true.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF edit_apolice = abap_false.
            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-SEQ_TIPO'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).
          ENDIF.
          "USER STORY 163032 - MMSILVA - 09.01.2025 - Fim
        ENDIF.

      WHEN 'B'. "Baixa

        IF wa_zglt067 IS INITIAL.

          CLEAR: gt_fields[].

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-BUKRS'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-TP_OPR'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).


          IF vl_seq_lcto_ref  IS NOT INITIAL.
            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-NRO_APOLICE'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).
          ENDIF.

          IF ( vl_seq_lcto_ref  IS NOT INITIAL ) OR
             ( vl_ctb_pag_rec   IS NOT INITIAL ).

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-COD_SEGURADORA'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).
          ENDIF.

          IF ( vl_seq_lcto_ref        IS NOT INITIAL ) OR
             ( vl_ctb_pag_rec         IS NOT INITIAL ) OR
             ( vl_ctb_aprop           IS NOT INITIAL ) OR
             ( wl_cabecalho_0110-lote IS NOT INITIAL ).

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-WAERS'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).

            IF vl_ctb_aprop IS NOT INITIAL.
              go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VIG_DE'
                                         group1    = space
                                         group2    = space
                                         value     = '0'
                                         invisible = '0' ).

              go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VIG_ATE'
                                         group1    = space
                                         group2    = space
                                         value     = '0'
                                         invisible = '0' ).
            ENDIF.

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-WKURS'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_PREMIO_BRL'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_PREMIO_USD'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_ASSEG_USD'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_ASSEG_BRL'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_LMI_USD'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).

            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-VLR_LMI_BRL'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).

          ELSE.

            IF ( wl_cabecalho_0110-waers = 'BRL' ).
              CLEAR: wl_cabecalho_0110-vlr_premio_usd,
                     wl_cabecalho_0110-vlr_asseg_usd.

              go_utils->z_tratar_campos( name      = space
                                         group1    = space
                                         group2    = 'GR3'
                                         value     = '0'
                                         invisible = '0' ).

            ELSEIF ( wl_cabecalho_0110-waers = 'USD').
              CLEAR: wl_cabecalho_0110-vlr_premio_brl,
                     wl_cabecalho_0110-vlr_asseg_brl.

              go_utils->z_tratar_campos( name      = space
                                         group1    = space
                                         group2    = 'GR2'
                                         value     = '0'
                                         invisible = '0' ).
            ENDIF.

          ENDIF.

          go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-SEQ_PARC'
                                     group1    = space
                                     group2    = space
                                     value     = '0'
                                     invisible = '0' ).

          "USER STORY 163032 - MMSILVA - 09.01.2025 - Inicio
          FREE: r_parid, _param.

          CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
            EXPORTING
              user_name           = sy-uname
            TABLES
              user_parameters     = _param
            EXCEPTIONS
              user_name_not_exist = 1
              OTHERS              = 2.


          IF _param IS NOT INITIAL.
            LOOP AT _param ASSIGNING FIELD-SYMBOL(<ws_param_b>).
              IF <ws_param_b>-parid = 'ZGL047_EDIT_TIPO'.
                edit_apolice = abap_true.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF edit_apolice = abap_false.
            go_utils->z_tratar_campos( name      = 'WL_CABECALHO_0110-SEQ_TIPO'
                                       group1    = space
                                       group2    = space
                                       value     = '0'
                                       invisible = '0' ).
          ENDIF.
          "USER STORY 163032 - MMSILVA - 09.01.2025 - Fim
        ELSE.
          edit = abap_true.
        ENDIF.
    ENDCASE.

    LOOP AT SCREEN.
      IF sy-ucomm EQ c_novo.
        IF screen-name = 'WL_CABECALHO_0110-SEQ_ENDOSSO'.
          screen-input        = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF sy-ucomm EQ c_change.
        IF screen-name = 'WL_CABECALHO_0110-SEQ_ENDOSSO'.
          screen-input        = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD z_del_apolice.

    DATA: vl_seq_lcto_ref TYPE zglt050-seq_lcto,
          vl_ctb_pag_rec  TYPE zglt067-lote,
          vl_ctb_aprop    TYPE zglt073-lote,
          vl_lote_null    TYPE zglt067-lote,
          var_answer      TYPE c.

    CREATE OBJECT: go_utils.

    CLEAR: vl_seq_lcto_ref, vl_ctb_pag_rec, vl_ctb_aprop, vl_lote_null.

    CHECK wl_cabecalho_0110-seq_lcto IS NOT INITIAL.

    "Caso tenha uma outra apólice referenciando-a. (Endosso/Baixa)
    SELECT SINGLE seq_lcto
      FROM zglt050 INTO vl_seq_lcto_ref
     WHERE ref_seq_lcto = wl_cabecalho_0110-seq_lcto
       AND loekz = ''.

    "Caso já tenha gerado doc. Lcto Ctb. p/ C.Pagar/C.Receber.
    SELECT SINGLE lote
      FROM zglt067 INTO vl_ctb_pag_rec
     WHERE seq_lcto EQ wl_cabecalho_0110-seq_lcto
       AND lote     NE vl_lote_null.

    "Caso já tenha gerado doc. Lcto Ctb. p/ Apropriações
    SELECT SINGLE lote
      FROM zglt073 INTO vl_ctb_aprop
     WHERE seq_lcto = wl_cabecalho_0110-seq_lcto
       AND lote     NE vl_lote_null.


    IF ( vl_seq_lcto_ref IS NOT INITIAL ).
      MESSAGE |Apólice já referenciada pela apólice de Seq. Lcto: { vl_seq_lcto_ref } ! | TYPE 'S'.
      EXIT.
    ENDIF.

    IF ( vl_ctb_pag_rec IS NOT INITIAL ) .
      MESSAGE |Apólice já possui lançamentos de Pgto/Recbto!| TYPE 'S'.
      EXIT.
    ENDIF.

    IF ( vl_ctb_aprop IS NOT INITIAL )  .
      MESSAGE |Apólice já possui lançamentos de apropriação!| TYPE 'S'.
      EXIT.
    ENDIF.

    IF ( wl_cabecalho_0110-lote IS NOT INITIAL )  .
      MESSAGE |Já existe lançamento contábil para a Apólice!| TYPE 'S'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja realmente excluir essa apólice?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK var_answer EQ '1'.

    UPDATE zglt050 SET loekz = 'X'
     WHERE seq_lcto = wl_cabecalho_0110-seq_lcto.

    MESSAGE |Apólice exclúida com sucesso!| TYPE 'S'.


  ENDMETHOD.

ENDCLASS.                    "Z_TIPO_OPERACAO IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS Z_GERAR_PAGAR_RECEBER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS z_seguro_geracao DEFINITION.
  PUBLIC SECTION.
    METHODS z_gerar_ctb_apolice.
    METHODS z_estorno_ctb_apolice.
    METHODS z_gerar_pagar_receber.
    METHODS z_estorno_pagar_receber.
    METHODS z_gerar_apropriacoes.
    METHODS z_gerar_ajuste_aprop.
    METHODS z_estorno_apropriacoes IMPORTING vl_budat TYPE budat.
    METHODS z_estorno_ajuste_aprop.
    METHODS z_baixar_bem_seg.
    METHODS z_inc_bens_baixa.
    METHODS z_modify_centro.
    METHODS z_converte_moeda IMPORTING
                               vlr_doc        TYPE dmbtr
                               currency       TYPE waers
                             EXPORTING
                               vlr_convertido TYPE dmbtr.

  PRIVATE SECTION.
    DATA: go_utils     TYPE REF TO zutils,
          r_gerar_lote TYPE REF TO zcl_gerar_lote,
          wl_zglt035   TYPE zglt035,
          wl_zglt031   TYPE zglt031,
          wl_zglt036   TYPE zglt036,
          at_index     TYPE sy-tabix,
          gt_zglt036   TYPE TABLE OF zglt036.

ENDCLASS.                    "Z_GERAR_CONTAS_PAGAR IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS Z_GERAR_PAGAR_RECEBER IMPLEMENTATION
*----------------------------------------------------------------------*

CLASS z_seguro_geracao IMPLEMENTATION.

  METHOD z_gerar_ctb_apolice.


    CREATE OBJECT go_utils.
    CREATE OBJECT r_gerar_lote.

    DATA: descr_lote        TYPE char100,
          doc_num           TYPE num10,
          vlr_moeda_interna TYPE dmbtr,
          vlr_moeda_grupo   TYPE dmbtr,
          wl_x001           TYPE x001,
          wl_xblnr          TYPE zglt035-xblnr,
          vl_gsber          TYPE zglt036-gsber.

    DATA: vl_branch TYPE j_1bbranch-branch.


    "CS2017002479 - 21.11.2017 - Ini
    CLEAR: gt_msg_return[].

    go_utils->z_validar_info_alv_0120( ).

    IF ( gt_msg_return[] IS NOT INITIAL ).
      l_active_node = 2.
      APPEND l_active_node TO lt_nodes_select.
    ELSE.
      go_utils->z_validar_info_alv_0130( i_valida_tot = 'X' ).

      IF ( gt_msg_return[] IS NOT INITIAL ).
        l_active_node = 3.
        APPEND l_active_node TO lt_nodes_select.
      ENDIF.
    ENDIF.

    IF ( gt_msg_return[] IS NOT INITIAL ).
      go_utils->z_show_splitter_error( i_show = 'X' ).
      EXIT.
    ENDIF.
    "CS2017002479 - 21.11.2017 - Fim

    CLEAR: wl_zglt050, gt_zglt036[].

    SELECT SINGLE *
      FROM zglt050 INTO wl_zglt050
     WHERE seq_lcto = wl_cabecalho_0110-seq_lcto
       AND loekz    EQ ''.

    CHECK ( sy-subrc = 0 ) AND ( wl_zglt050-lote IS INITIAL ).

    DATA(_ok) = abap_false.
    PERFORM f_check_authority USING wl_zglt050-bukrs
                                    abap_true
                           CHANGING _ok.

    CHECK _ok IS NOT INITIAL.


*=== Seleciona os tipos de lançamentos.
    CLEAR: wl_zglt064, vg_tp_lcto, wl_zglt035.
    SELECT SINGLE *
      FROM zglt064 INTO wl_zglt064
     WHERE seq_tipo EQ wl_zglt050-seq_tipo.

    IF ( sy-subrc NE 0 ) OR ( wl_zglt064-tp_lcto_apolice IS INITIAL ).
      MESSAGE s836(sd) WITH TEXT-e55 DISPLAY LIKE 'S'.
      EXIT.
    ENDIF.

    vg_tp_lcto = wl_zglt064-tp_lcto_apolice.

    SELECT SINGLE *
      FROM zglt031
      INTO wl_zglt031
     WHERE tp_lcto EQ vg_tp_lcto.

    IF sy-subrc NE 0.
      MESSAGE s836(sd) WITH TEXT-e55 DISPLAY LIKE 'S'.
      EXIT.
    ENDIF.

    "Busca Filial Matriz.
    CLEAR: vl_gsber.
    SELECT SINGLE branch
      FROM j_1bbranch INTO vl_gsber
     WHERE bukrs      EQ  wl_cabecalho_0110-bukrs
       AND branch     NE '0001'
       AND cgc_branch EQ '1'.

    IF ( sy-subrc NE 0 ) OR ( vl_gsber IS INITIAL ).

      READ TABLE gt_saida_0120 INTO DATA(ws_saida_0120) INDEX 1 .
      IF sy-subrc EQ 0.
        CONCATENATE wl_cabecalho_0110-bukrs '-' ws_saida_0120-filial INTO DATA(_emp_filial).
      ENDIF.

      "Verificar set
      SELECT SINGLE *
        FROM setleaf INTO @DATA(_wl_setleaf)
       WHERE setname = 'MAGGI_ZGL0016_DIV'
         AND valfrom = @_emp_filial.

      IF sy-subrc NE 0.
        MESSAGE s836(sd) WITH TEXT-e55 DISPLAY LIKE 'S'.
        EXIT.
      ENDIF.

      vl_gsber = ws_saida_0120-filial.
    ENDIF.


    SELECT *
      FROM zglt032 INTO TABLE gt_zglt032
     WHERE tp_lcto EQ vg_tp_lcto.

    IF gt_zglt032[] IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e55 DISPLAY LIKE 'S'.
      EXIT.
    ENDIF.

    CLEAR: e_status, e_messa.
    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        i_bukrs  = wl_zglt050-bukrs
        i_data   = sy-datum
      IMPORTING
        e_status = e_status
        e_messa  = e_messa
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF e_status = 'E'.
      MESSAGE e_messa TYPE 'S'.
      EXIT.
    ENDIF.

    "Inicializa dados Lote.
    CALL FUNCTION 'FI_CURRENCY_INFORMATION'
      EXPORTING
        i_bukrs = wl_zglt050-bukrs
      IMPORTING
        e_x001  = wl_x001.

*=== Etapa2: Gera número do documento.
    r_gerar_lote->create_lote( EXPORTING i_bukrs      = wl_zglt050-bukrs
                                         i_descr_lote = 'Contratação Seguros'
                                         i_dep_resp   = wl_zglt064-dep_resp
                                         i_user_resp  = sy-uname
                               IMPORTING e_num_lote   = wl_zglt035-lote ).

    CLEAR: wl_xblnr.
    CONCATENATE wl_zglt050-vig_de+06(2) '/'
                wl_zglt050-vig_de+4(02) '/'
                wl_zglt050-vig_de(04) '-'
                wl_zglt050-vig_ate(04) INTO wl_xblnr.

    at_index = sy-tabix.
    wl_zglt035-bukrs          = wl_zglt050-bukrs.
    wl_zglt035-tp_lcto        = wl_zglt064-tp_lcto_apolice.
    wl_zglt035-dpto_resp      = wl_zglt064-dep_resp.
    wl_zglt035-taxa           = wl_zglt050-wkurs.
    wl_zglt035-moeda_doc      = wl_zglt050-waers.
    wl_zglt035-st_lc_moeda    = wl_zglt031-st_lc_moeda.
    wl_zglt035-moeda_interna  = wl_t001-waers.
    wl_zglt035-moeda_int_hist = wl_zglt031-moeda_int_hist.
    wl_zglt035-moeda_forte    = wl_zglt031-moeda_forte.
    wl_zglt035-moeda_ft_hist  = wl_zglt031-moeda_ft_hist.
    wl_zglt035-moeda_grupo    = wl_zglt031-moeda_grupo.
    wl_zglt035-moeda_gp_hist  = wl_zglt031-moeda_gp_hist.
    wl_zglt035-blart          = wl_zglt031-blart.
    wl_zglt035-xblnr          = wl_xblnr.
    wl_zglt035-bktxt          = wl_zglt031-bktxt.
    wl_zglt035-budat          = sy-datum.
    wl_zglt035-bldat          = sy-datum.
    wl_zglt035-dt_lcto        = sy-datum.
    wl_zglt035-prov_est       = wl_zglt031-prov_est.
    wl_zglt035-st_ap_fiscal   = wl_zglt031-st_ap_fiscal.
    wl_zglt035-monat          = sy-datum+4(2).
    wl_zglt035-gjahr          = sy-datum(4).
    wl_zglt035-usnam          = sy-uname.
    wl_zglt035-dt_entrada     = sy-datum.
    wl_zglt035-hr_entrada     = sy-uzeit.

    CLEAR: gt_zglt036.
    LOOP AT gt_zglt032 INTO wl_zglt032.
      wl_zglt036-seqitem = sy-tabix.
      wl_zglt036-tp_lcto = wl_zglt032-tp_lcto.
      wl_zglt036-bschl   = wl_zglt032-bschl.
      wl_zglt036-zuonr   = wl_zglt050-nro_apolice.
      wl_zglt036-hkont   = wl_zglt032-hkont.
      wl_zglt036-gsber   = vl_gsber.
      CONCATENATE 'Apólice nº' wl_zglt050-nro_apolice
            INTO wl_zglt036-sgtxt  SEPARATED BY space.

      CLEAR: wl_zglt036-kostl, wl_zglt036-seqsub.

      IF ( wl_zglt050-waers EQ 'BRL' ).
        wl_zglt036-vlr_moeda_doc   = wl_zglt050-vlr_premio_brl.

*          Z_CONVERTE_MOEDA( EXPORTING
*                            VLR_DOC  = WL_ZGLT050-VLR_PREMIO_BRL
*                            CURRENCY = WL_T001-WAERS
*                            IMPORTING
*                            VLR_CONVERTIDO = VLR_MOEDA_INTERNA ).
*
*          Z_CONVERTE_MOEDA( EXPORTING
*                            VLR_DOC  = WL_ZGLT050-VLR_PREMIO_BRL
*                            CURRENCY = WL_X001-HWAE3
*                            IMPORTING
*                            VLR_CONVERTIDO = VLR_MOEDA_GRUPO ).
      ELSE.
        wl_zglt036-vlr_moeda_doc   = wl_zglt050-vlr_premio_usd.

*          Z_CONVERTE_MOEDA( EXPORTING
*                            VLR_DOC  = WL_ZGLT050-VLR_PREMIO_USD
*                            CURRENCY = WL_T001-WAERS
*                            IMPORTING
*                            VLR_CONVERTIDO = VLR_MOEDA_INTERNA ).
*
*          Z_CONVERTE_MOEDA( EXPORTING
*                            VLR_DOC  = WL_ZGLT050-VLR_PREMIO_USD
*                            CURRENCY = WL_X001-HWAE3
*                            IMPORTING
*                            VLR_CONVERTIDO = VLR_MOEDA_GRUPO ).
      ENDIF.

      wl_zglt036-vlr_moeda_int     = wl_zglt050-vlr_premio_brl.
      wl_zglt036-vlr_moeda_forte   = wl_zglt050-vlr_premio_usd.
      "WL_ZGLT036-VLR_MOEDA_GRUPO   = VLR_MOEDA_GRUPO.

      wl_zglt036-estrategia_forn   = 'X'. "Força assumir estrategia de Pgto de Fornecedor.

      APPEND wl_zglt036 TO gt_zglt036.
      CLEAR wl_zglt036.
    ENDLOOP.

    r_gerar_lote->contabilizar_lote( CHANGING i_zglt036 = gt_zglt036
                                              i_zglt035 = wl_zglt035 ).

    wl_cabecalho_0110-lote       = wl_zglt035-lote.
    wl_cabecalho_0110-doc_lcto   = wl_zglt035-doc_lcto.

    UPDATE zglt050 SET lote        = wl_zglt035-lote
                       doc_lcto    = wl_zglt035-doc_lcto
                       dt_lcto_ctb = sy-datum
                 WHERE seq_lcto = wl_zglt050-seq_lcto.

    CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
      EXPORTING
        p_num_lote = wl_zglt035-lote.

    CLEAR: _emp_filial, ws_saida_0120.

  ENDMETHOD.

  METHOD z_estorno_ctb_apolice.

    DATA: it_dta   TYPE STANDARD TABLE OF bdcdata,
          wa_dta   TYPE bdcdata,
          wg_bdc   TYPE bdcdata,
          tg_bdc   TYPE TABLE OF bdcdata,
          tg_msg   TYPE TABLE OF bdcmsgcoll,
          wg_msg   TYPE bdcmsgcoll,
          opt      TYPE ctu_params,
          vl_stblg TYPE bkpf-stblg.


    IF wl_cabecalho_0110-belnr IS INITIAL.
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e48 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(_ok) = abap_false.
    PERFORM f_check_authority USING wl_cabecalho_0110-bukrs
                                    abap_true
                           CHANGING _ok.

    CHECK _ok IS NOT INITIAL.

    CLEAR wl_zglt050.
    SELECT SINGLE *
      FROM zglt050 INTO wl_zglt050
     WHERE seq_lcto  = wl_cabecalho_0110-seq_lcto
       AND loekz     EQ ''.

    IF ( wl_zglt050-doc_lcto IS INITIAL ).
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e47 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    "Verifica se já possui apropriação lançada para a apolice
    SELECT SINGLE *
      FROM zglt073 INTO @DATA(wl_073)
     WHERE seq_lcto EQ @wl_cabecalho_0110-seq_lcto.

    IF sy-subrc = 0.
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e57 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    "Verifica se já possui C.Pagar/Receber com contabilização
    SELECT SINGLE *
      FROM zglt067 INTO @DATA(wl_067)
     WHERE seq_lcto = @wl_cabecalho_0110-seq_lcto
       AND doc_lcto NE '0000000000'.

    IF sy-subrc = 0.
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e58 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.


    FREE: it_dta.
    DEFINE shdb.
      CLEAR wa_dta.
      wa_dta-program   = &1.
      wa_dta-dynpro    = &2.
      wa_dta-dynbegin  = &3.
      wa_dta-fnam      = &4.
      wa_dta-fval      = &5.
      APPEND wa_dta TO it_dta.
    END-OF-DEFINITION.

    shdb:
    'SAPMF05A' '0105' 'X'  ' '           ' ',
    ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
    ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
    ' '        ' '    ' '  'RF05A-BELNS' wl_cabecalho_0110-belnr,
    ' '        ' '    ' '  'BKPF-BUKRS'  wl_cabecalho_0110-bukrs,
    ' '        ' '    ' '  'RF05A-GJAHS' wl_cabecalho_0110-dt_lcto_ctb(4),
    ' '        ' '    ' '  'UF05A-STGRD' '01'.

    opt-dismode = 'E'.
    CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE stblg
      FROM bkpf INTO vl_stblg
     WHERE bukrs = wl_cabecalho_0110-bukrs
       AND belnr = wl_cabecalho_0110-belnr
       AND gjahr = wl_cabecalho_0110-dt_lcto_ctb(4).

    CHECK ( sy-subrc = 0 ) AND ( vl_stblg IS NOT INITIAL ).

    UPDATE zglt050 SET lote        = space
                       doc_lcto    = space
                       belnr       = space
                       dt_lcto_ctb = space
                 WHERE seq_lcto    = wl_cabecalho_0110-seq_lcto.

    MOVE abap_false      TO wl_cabecalho_0110-lote.
    MOVE abap_false      TO wl_cabecalho_0110-doc_lcto.
    MOVE abap_false      TO wl_cabecalho_0110-belnr.
    MOVE icon_light_out  TO wl_cabecalho_0110-status.

    COMMIT WORK.
    MESSAGE s836(sd) WITH TEXT-s04 DISPLAY LIKE 'S'.

  ENDMETHOD.

  METHOD z_gerar_pagar_receber.

    TYPES: BEGIN OF ty_lote_agrp,
             filial TYPE ty_saida_0160-filial,
             lote   TYPE zglt035-lote,
             tipo   TYPE c LENGTH 1, " L = Libera /  A = Aprova
           END OF ty_lote_agrp.

    CREATE OBJECT go_utils.
    CREATE OBJECT r_gerar_lote.

    DATA: descr_lote        TYPE char100,
          doc_num           TYPE num10,
          vlr_moeda_interna TYPE dmbtr,
          vlr_moeda_grupo   TYPE dmbtr,
          vl_valida         TYPE c,
          wl_x001           TYPE x001,
          wl_xblnr          TYPE zglt035-xblnr,
          vl_parc_atual     TYPE string,
          vl_parc_tot       TYPE string,
          vl_parcela        TYPE string,
          wl_bkpf_ap        TYPE bkpf,
          vl_tipo_lote      TYPE c,
          wl_zglt035        TYPE zglt035,
          it_lote_agrp      TYPE TABLE OF ty_lote_agrp,
          wl_lote_agrp      TYPE ty_lote_agrp.

    REFRESH: it_lote_agrp.

    LOOP AT gt_selected_rows INTO wl_selected_rows.

      CLEAR: vl_tipo_lote, wl_zglt031, gt_zglt032[], wl_saida_0160, wl_zglt050.

      READ TABLE gt_saida_0160 INTO wl_saida_0160 INDEX wl_selected_rows-index.
      CHECK ( sy-subrc = 0 ) AND ( wl_saida_0160-lote IS INITIAL ).

      READ TABLE gt_zglt050 INTO wl_zglt050 WITH KEY seq_lcto = wl_saida_0160-seq_lcto.
      CHECK ( sy-subrc = 0 ) AND ( wl_zglt050-seq_lcto IS NOT INITIAL ).

      DATA(_ok) = abap_false.
      PERFORM f_check_authority USING wl_zglt050-bukrs
                                      abap_true
                             CHANGING _ok.

      CHECK _ok IS NOT INITIAL.

      "Ini CS2017000785
      go_utils->z_check_ctb_apolice( EXPORTING i_wl_050 = wl_zglt050
                                     IMPORTING e_valida = vl_valida ).
      IF vl_valida IS INITIAL.
        CONTINUE.
      ENDIF.
      "Fim CS2017000785

      SELECT SINGLE *
        FROM zglt067 INTO @DATA(_wl_zglt067)
       WHERE seq_lcto = @wl_saida_0160-seq_lcto
         AND nro_parc = @wl_saida_0160-nro_parc.

      CHECK ( sy-subrc = 0 ) AND ( _wl_zglt067-lote IS INITIAL ).

*=== Seleciona os tipos de lançamentos.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_zglt050-cod_seguradora
        IMPORTING
          output = wl_zglt050-cod_seguradora.

      CLEAR: wl_zglt064, vg_tp_lcto.
      SELECT SINGLE *
        FROM zglt064 INTO wl_zglt064
       WHERE seq_tipo EQ wl_zglt050-seq_tipo.

      CHECK ( sy-subrc = 0 ) AND ( wl_zglt064-tp_lcto IS NOT INITIAL ).

      vg_tp_lcto = wl_zglt064-tp_lcto.
      IF wl_zglt050-tp_opr = 'B'. "Baixa.
        vg_tp_lcto = wl_zglt064-tp_lcto_ar. "Contas a Receber
      ENDIF.

      SELECT SINGLE *
        FROM zglt031
        INTO wl_zglt031
       WHERE tp_lcto EQ vg_tp_lcto.

      IF sy-subrc NE 0.
        MESSAGE s836(sd) WITH TEXT-e55 DISPLAY LIKE 'S'.
        CONTINUE.
      ENDIF.

      SELECT *
        FROM zglt032 INTO TABLE gt_zglt032
       WHERE tp_lcto EQ vg_tp_lcto
         AND (   ( bschl   IN ('40','50') ) OR
                 ( hkont   IN (wl_zglt050-cod_seguradora) )  ).

      SORT gt_zglt032 BY bschl ASCENDING.

      IF gt_zglt032[] IS INITIAL.
        MESSAGE s836(sd) WITH TEXT-e55 DISPLAY LIKE 'S'.
        CONTINUE.
      ENDIF.

      "Inicializa dados Lote.
      "DESCR_LOTE = WL_ZGLT064-DESCR.

      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          i_bukrs = wl_zglt050-bukrs
        IMPORTING
          e_x001  = wl_x001.

      CLEAR: e_status, e_messa.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          i_bukrs  = wl_zglt050-bukrs
          i_data   = sy-datum
        IMPORTING
          e_status = e_status
          e_messa  = e_messa
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF e_status = 'E'.
        MESSAGE e_messa TYPE 'S'.
        CONTINUE.
      ENDIF.

*=== Etapa2: Gera número do documento.

      IF wl_zglt050-dt_criacao >= c_dt_corte_aprov.
        vl_tipo_lote = 'A'. "Aprovação
      ELSE.
        vl_tipo_lote = 'L'. "Liberação
      ENDIF.

      READ TABLE it_lote_agrp INTO wl_lote_agrp WITH KEY filial = wl_saida_0160-filial
                                                         tipo   = vl_tipo_lote.

      IF ( sy-subrc = 0 ) AND ( wl_lote_agrp-lote IS NOT INITIAL ).
        wl_zglt035-lote = wl_lote_agrp-lote.
      ELSE.
        r_gerar_lote->create_lote( EXPORTING i_bukrs      = wl_zglt050-bukrs
                                             i_descr_lote = 'Pagto/Recebto Seguros'
                                             i_dep_resp   = wl_zglt064-dep_resp
                                             i_user_resp  = sy-uname
                                   IMPORTING e_num_lote   = wl_zglt035-lote ).

        CLEAR: wl_lote_agrp.
        wl_lote_agrp-filial   = wl_saida_0160-filial.
        wl_lote_agrp-lote     = wl_zglt035-lote.
        wl_lote_agrp-tipo     = vl_tipo_lote.

        APPEND wl_lote_agrp TO it_lote_agrp.
      ENDIF.

      CLEAR: wl_xblnr.
      CONCATENATE wl_zglt050-vig_de+06(2) '/'
                  wl_zglt050-vig_de+4(02) '/'
                  wl_zglt050-vig_de(04) '-'
                  wl_zglt050-vig_ate(04) INTO wl_xblnr.

      at_index = sy-tabix.
      wl_zglt035-bukrs          = wl_zglt050-bukrs.
      wl_zglt035-tp_lcto        = vg_tp_lcto.
      wl_zglt035-dpto_resp      = wl_zglt064-dep_resp.
      wl_zglt035-taxa           = wl_zglt050-wkurs.
      wl_zglt035-moeda_doc      = wl_zglt050-waers.
      wl_zglt035-st_lc_moeda    = wl_zglt031-st_lc_moeda.
      wl_zglt035-moeda_interna  = wl_t001-waers.
      wl_zglt035-moeda_int_hist = wl_zglt031-moeda_int_hist.
      wl_zglt035-moeda_forte    = wl_zglt031-moeda_forte.
      wl_zglt035-moeda_ft_hist  = wl_zglt031-moeda_ft_hist.
      wl_zglt035-moeda_grupo    = wl_zglt031-moeda_grupo.
      wl_zglt035-moeda_gp_hist  = wl_zglt031-moeda_gp_hist.
      wl_zglt035-blart          = wl_zglt031-blart.
      wl_zglt035-xblnr          = wl_xblnr.
      wl_zglt035-bktxt          = wl_zglt031-bktxt.
      wl_zglt035-budat          = sy-datum.
      wl_zglt035-bldat          = sy-datum.
      wl_zglt035-dt_lcto        = sy-datum.
      wl_zglt035-prov_est       = wl_zglt031-prov_est.
      wl_zglt035-st_ap_fiscal   = wl_zglt031-st_ap_fiscal.
      wl_zglt035-monat          = sy-datum+4(2).
      wl_zglt035-gjahr          = sy-datum(4).
      wl_zglt035-usnam          = sy-uname.
      wl_zglt035-dt_entrada     = sy-datum.
      wl_zglt035-hr_entrada     = sy-uzeit.
      wl_zglt035-lote_prec      = wl_zglt050-lote.
      wl_zglt035-doc_lcto_prec  = wl_zglt050-doc_lcto.
      wl_zglt035-belnr_prec     = wl_zglt050-belnr.

      CLEAR: vl_parcela, vl_parc_atual, vl_parc_tot.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wl_saida_0160-nro_parc
        IMPORTING
          output = vl_parc_atual.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wl_zglt050-seq_parc
        IMPORTING
          output = vl_parc_tot.

      CONDENSE vl_parc_atual NO-GAPS.
      CONDENSE vl_parc_tot   NO-GAPS.

      CONCATENATE vl_parc_atual '/' vl_parc_tot
             INTO vl_parcela.

      LOOP AT gt_zglt032 INTO wl_zglt032.

        CLEAR: wl_zglt036.

        wl_zglt036-seqitem = sy-tabix.
        wl_zglt036-tp_lcto = wl_zglt032-tp_lcto.
        wl_zglt036-bschl   = wl_zglt032-bschl.
        wl_zglt036-gsber   = wl_saida_0160-filial.
        wl_zglt036-hbkid   = wl_saida_0160-bco_empresa.
        wl_zglt036-bvtyp   = wl_saida_0160-bco_parceiro.
        wl_zglt036-dt_vct	 = wl_saida_0160-dt_venc.
        wl_zglt036-zlsch   = wl_saida_0160-forma_pgto.
        wl_zglt036-zlspr   = wl_saida_0160-bloq_pgto.

        IF strlen( wl_saida_0160-cod_barras ) = 47.
          wl_zglt036-esrnr = wl_saida_0160-cod_barras(10).
          wl_zglt036-esrre = wl_saida_0160-cod_barras+10(27).
        ENDIF.

        wl_zglt036-zuonr   = wl_zglt050-nro_apolice.

        IF ( wl_zglt036-seqitem EQ 1 ).
          wl_zglt036-hkont = wl_zglt050-cod_seguradora.
        ELSE.
          wl_zglt036-hkont = wl_zglt032-hkont.
        ENDIF.

        IF wl_zglt050-tp_opr = 'B'. "Baixa.
          "Contas a Receber
*          IF WL_ZGLT036-HKONT = WL_ZGLT050-COD_SEGURADORA.
*            CONCATENATE WL_ZGLT064-SEQ_TIPO '-' WL_ZGLT064-DESCR
*                   INTO WL_ZGLT036-ZUONR.
*
*          ENDIF.

          CONCATENATE 'Restituição Prêmio Nº' wl_zglt050-nro_apolice
                 INTO wl_zglt036-sgtxt SEPARATED BY space.
        ELSE.
          "Contas a Pagar
          IF wl_zglt036-hkont = wl_zglt050-cod_seguradora.
            CONCATENATE 'Parcela'  vl_parcela
                   INTO wl_zglt036-zuonr SEPARATED BY space.

          ENDIF.

          CONCATENATE 'Pagamento Seguro Parc.' vl_parcela
               INTO wl_zglt036-sgtxt SEPARATED BY space.

        ENDIF.




        CLEAR: wl_zglt036-kostl, wl_zglt036-seqsub.

        IF ( wl_zglt050-waers EQ 'BRL' ).
          wl_zglt036-vlr_moeda_doc   = wl_saida_0160-vlr_premio_brl.

*          Z_CONVERTE_MOEDA( EXPORTING
*                            VLR_DOC  = WL_SAIDA_0160-VLR_PREMIO_BRL
*                            CURRENCY = WL_T001-WAERS
*                            IMPORTING
*                            VLR_CONVERTIDO = VLR_MOEDA_INTERNA ).
*
*          Z_CONVERTE_MOEDA( EXPORTING
*                            VLR_DOC  = WL_SAIDA_0160-VLR_PREMIO_BRL
*                            CURRENCY = WL_X001-HWAE3
*                            IMPORTING
*                            VLR_CONVERTIDO = VLR_MOEDA_GRUPO ).
        ELSE.
          wl_zglt036-vlr_moeda_doc   = wl_saida_0160-vlr_premio_usd.

*          Z_CONVERTE_MOEDA( EXPORTING
*                            VLR_DOC  = WL_SAIDA_0160-VLR_PREMIO_USD
*                            CURRENCY = WL_T001-WAERS
*                            IMPORTING
*                            VLR_CONVERTIDO = VLR_MOEDA_INTERNA ).
*
*          Z_CONVERTE_MOEDA( EXPORTING
*                            VLR_DOC  = WL_SAIDA_0160-VLR_PREMIO_USD
*                            CURRENCY = WL_X001-HWAE3
*                            IMPORTING
*                            VLR_CONVERTIDO = VLR_MOEDA_GRUPO ).
        ENDIF.

        wl_zglt036-vlr_moeda_int     = wl_saida_0160-vlr_premio_brl.
        wl_zglt036-vlr_moeda_forte   = wl_saida_0160-vlr_premio_usd.
        "WL_ZGLT036-VLR_MOEDA_GRUPO   = VLR_MOEDA_GRUPO.

        "Somente  passar dados bancários  para  as chaves
        "de lançamento onde é informado o código do fornecedor
        IF wl_zglt036-hkont NE wl_zglt050-cod_seguradora.
          CLEAR: wl_zglt036-hbkid, wl_zglt036-bvtyp, wl_zglt036-dt_vct,
                 wl_zglt036-zlsch, wl_zglt036-zlspr.
        ENDIF.

        APPEND wl_zglt036 TO gt_zglt036.
        CLEAR wl_zglt036.
      ENDLOOP.

      r_gerar_lote->contabilizar_lote( CHANGING i_zglt036 = gt_zglt036
                                                i_zglt035 = wl_zglt035 ).

      wl_saida_0160-status          = icon_yellow_light.
      wl_saida_0160-lote            = wl_zglt035-lote.
      wl_saida_0160-nro_documento   = wl_zglt035-doc_lcto.

      UPDATE zglt067 SET lote        = wl_zglt035-lote
                         doc_lcto    = wl_zglt035-doc_lcto
                         dt_lcto_ctb = sy-datum
                   WHERE seq_lcto = wl_saida_0160-seq_lcto
                     AND nro_parc = wl_saida_0160-nro_parc.

      MODIFY gt_saida_0160 FROM wl_saida_0160 INDEX wl_selected_rows-index.

      CLEAR gt_zglt036[].
    ENDLOOP.

    LOOP AT it_lote_agrp INTO wl_lote_agrp.

      IF wl_lote_agrp-tipo = 'A'. "Aprovar Lote Direto

        CLEAR: wl_zglt035.
        SELECT SINGLE *
          FROM zglt035 INTO wl_zglt035
         WHERE lote EQ wl_lote_agrp-lote.

        IF ( sy-subrc = 0 ).

          IF wl_zglt035-prov_est IS NOT INITIAL.
            PERFORM grava_zib IN PROGRAM zgl017 USING wl_zglt035-lote IF FOUND.
          ELSE.
            SUBMIT z_grava_zib_zgl WITH p_lote = wl_zglt035-lote AND RETURN.
          ENDIF.

          UPDATE zglt034
             SET status_lote = 'A'
           WHERE lote EQ wl_zglt035-lote.
        ENDIF.

      ELSEIF wl_lote_agrp-tipo = 'L'. "Liberar Lote.

        CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
          EXPORTING
            p_num_lote = wl_lote_agrp-lote.

      ENDIF.

    ENDLOOP.

    CALL METHOD obj_alv_0160->refresh_table_display
      EXPORTING
        is_stable = wl_stable.
  ENDMETHOD.                    "Z_GERAR_CONTAS_PAGAR

  METHOD z_gerar_apropriacoes.

    TYPES: BEGIN OF ty_lote_agrp,
             bukrs TYPE ty_saida_0150-bukrs,
             werks TYPE ty_saida_0150-werks,
             lote  TYPE zglt035-lote,
           END OF ty_lote_agrp.

    DATA: wl_tka02     TYPE tka02,

* ---> S4 Migration - 17/07/2023 - CA
*          wl_cskb      TYPE cskb,
* <--- S4 Migration - 17/07/2023 - CA

          wl_skb1      TYPE skb1,
          wl_tbsl      TYPE tbsl,
          vl_mes_ano   TYPE string,
          vl_sgtxt     TYPE string,
          wl_zglt035   TYPE zglt035,
          vl_valida    TYPE c,
          it_lote_agrp TYPE TABLE OF ty_lote_agrp,
          wl_lote_agrp TYPE ty_lote_agrp,
          wl_xblnr     TYPE zglt035-xblnr,
          it_seq_erro  TYPE TABLE OF zseq_lcto.

* ---> S4 Migration - 17/07/2023 - CA
    DATA: lt_returns         TYPE TABLE OF bapiret2,
          ls_coeldes         TYPE bapi1030_ceoutputlist,
          lv_controllingarea TYPE bapi1030_gen-co_area,
          lv_costelement     TYPE bapi1030_gen-cost_elem,
          lv_keydate         TYPE bapi1030_gen-some_date.
* <--- S4 Migration - 17/07/2023 - CA
**<<<------"164255 - NMS - INI------>>>
    DATA: vl_dt_aprop TYPE dats,
          vl_dt_parc  TYPE dats, "CSB - BUG - 189757
          vl_month    TYPE dlymo.
**<<<------"164255 - NMS - FIM------>>>
    CREATE OBJECT go_utils.
    CREATE OBJECT r_gerar_lote.

    CLEAR: gt_zglt073, gt_zglt036, it_lote_agrp[].
**<<<------"164255 - NMS - INI------>>>
    IF wl_cabecalho_0150-seq_tipo EQ 17 OR
       wl_cabecalho_0150-seq_tipo EQ 21 OR
       wl_cabecalho_0150-seq_tipo EQ 22 OR
       wl_cabecalho_0150-seq_tipo EQ 29.

      SELECT seq_lcto, nro_parc FROM zglt073
        INTO TABLE @DATA(tl_zglt073)
        FOR ALL ENTRIES IN @gt_saida_0150
      WHERE seq_lcto EQ @gt_saida_0150-seq_lcto.

      IF sy-subrc IS INITIAL.
        SORT tl_zglt073 BY seq_lcto nro_parc.

      ENDIF.

    ELSE.
**<<<------"164255 - NMS - FIM------>>>
      LOOP AT gt_selected_rows INTO wl_selected_rows.
        READ TABLE gt_saida_0150 INTO wl_saida_0150 INDEX wl_selected_rows-index.

        DATA(nr_parcelas) = 001.

        WHILE ( nr_parcelas < wl_saida_0150-nro_parc ).

          SELECT SINGLE
              z73~seq_lcto
          FROM zglt073 AS z73
          INTO @DATA(wa_seq_lcto)
          WHERE
              seq_lcto EQ @wl_saida_0150-seq_lcto AND
              nro_parc EQ @nr_parcelas.

          IF sy-subrc <> 0 .
*          MESSAGE 'Existem parcelas anteriores pendentes de lançamento!' TYPE 'E'.
*          EXIT.

            APPEND wl_saida_0150-seq_lcto TO it_seq_erro[].
            DELETE TABLE gt_selected_rows[] FROM wl_selected_rows.
            nr_parcelas = wl_saida_0150-nro_parc.

          ELSE.
            nr_parcelas =  nr_parcelas + 001.
          ENDIF.

        ENDWHILE.
      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM it_seq_erro[] COMPARING ALL FIELDS.
**<<<------"164255 - NMS - INI------>>>
    ENDIF.
**<<<------"164255 - NMS - FIM------>>>
    LOOP AT gt_selected_rows INTO wl_selected_rows.
      CLEAR: wl_saida_0150, wl_zglt050, wl_zglt064, wl_zglt031, wl_zglt032, wl_t001, gt_zglt032[].

      READ TABLE:
      gt_saida_0150 INTO wl_saida_0150 INDEX wl_selected_rows-index,
      gt_zglt050    INTO wl_zglt050 WITH KEY seq_lcto = wl_saida_0150-seq_lcto.

      CHECK sy-subrc EQ 0.
      DATA(_ok) = abap_false.
      PERFORM f_check_authority USING wl_zglt050-bukrs
                                      abap_true
                             CHANGING _ok.

      CHECK _ok IS NOT INITIAL.


      SELECT SINGLE *
        FROM zglt073 INTO @DATA(_wl_0073)
       WHERE seq_lcto = @wl_saida_0150-seq_lcto
         AND nr_item  = @wl_saida_0150-nr_item
         AND nro_parc = @wl_saida_0150-nro_parc.

      IF ( sy-subrc = 0 ) AND ( wl_saida_0150-status <> '@0A@' OR wl_saida_0150-doc_contabil IS NOT INITIAL ).
        MESSAGE s836(sd) WITH TEXT-e65 wl_saida_0150-nr_item  DISPLAY LIKE 'E'.
        CONTINUE.
      ELSE.
        MOVE abap_false     TO wl_saida_0150-lote.
        MOVE abap_false     TO wl_saida_0150-doc_lcto.
        MOVE abap_false     TO wl_saida_0150-doc_contabil.
        MOVE icon_light_out TO wl_saida_0150-status.
      ENDIF.

      SELECT SINGLE *
        FROM zglt068 INTO @DATA(_wl_068)
       WHERE seq_lcto = @wl_saida_0150-seq_lcto
         AND nr_item  = @wl_saida_0150-nr_item.

      IF sy-subrc NE 0 .
        MESSAGE s836(sd) WITH TEXT-e69 DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      IF _wl_068-lote_ajus IS NOT INITIAL.
        MESSAGE s836(sd) WITH TEXT-e70 DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      "Ini CS2017000785
      go_utils->z_check_ctb_apolice( EXPORTING i_wl_050 = wl_zglt050
                                     IMPORTING e_valida = vl_valida ).
      IF vl_valida IS INITIAL.
        CONTINUE.
      ENDIF.

      SELECT SINGLE *
        FROM t001 INTO wl_t001
       WHERE bukrs = wl_zglt050-bukrs.

      SELECT SINGLE *
        FROM zglt064 INTO wl_zglt064
       WHERE seq_tipo = wl_zglt050-seq_tipo.

      IF sy-subrc NE 0.
        MESSAGE s836(sd) WITH TEXT-e33 DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      SELECT SINGLE *
        FROM zglt031 INTO wl_zglt031
       WHERE tp_lcto EQ wl_zglt064-tp_lcto_aprop.

      IF sy-subrc NE 0.
        MESSAGE s836(sd) WITH TEXT-e34 DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      SELECT *
        FROM zglt032
        INTO TABLE gt_zglt032
       WHERE tp_lcto EQ wl_zglt064-tp_lcto_aprop.

      IF gt_zglt032[] IS INITIAL.
        MESSAGE s836(sd) WITH TEXT-e32 DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.
      "Fim CS2017000785

      CHECK ( ( wl_saida_0150-vlr_premio_brl > 0 ) OR
              ( wl_saida_0150-vlr_premio_usd > 0 ) ).

      CHECK wl_saida_0150-lote IS INITIAL.

      CLEAR: e_status, e_messa.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          i_bukrs  = wl_zglt050-bukrs
          i_data   = wl_saida_0150-dt_apropr
        IMPORTING
          e_status = e_status
          e_messa  = e_messa
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF e_status = 'E'.
        MESSAGE e_messa TYPE 'S'.
        CONTINUE.
      ENDIF.

      CLEAR: wl_xblnr.
      CONCATENATE wl_zglt050-vig_de+06(2) '/'
                  wl_zglt050-vig_de+4(02) '/'
                  wl_zglt050-vig_de(04) '-'
                  wl_zglt050-vig_ate(04) INTO wl_xblnr.

      READ TABLE it_lote_agrp INTO wl_lote_agrp WITH KEY bukrs = wl_saida_0150-bukrs
                                                         werks = wl_saida_0150-werks.

      IF ( sy-subrc = 0 ) AND ( wl_lote_agrp-lote IS NOT INITIAL ).
        wl_zglt035-lote = wl_lote_agrp-lote.
      ELSE.
        r_gerar_lote->create_lote( EXPORTING i_bukrs      = wl_zglt050-bukrs
                                             i_descr_lote = 'Apropriação Seguros'
                                             i_dep_resp   = wl_zglt050-dep_resp
                                             i_user_resp  = sy-uname
                                   IMPORTING e_num_lote   = wl_zglt035-lote ).

        CLEAR: wl_lote_agrp.
        wl_lote_agrp-bukrs = wl_saida_0150-bukrs.
        wl_lote_agrp-werks = wl_saida_0150-werks.
        wl_lote_agrp-lote  = wl_zglt035-lote.
        APPEND wl_lote_agrp TO it_lote_agrp.
      ENDIF.

      wl_zglt035-bukrs          = wl_zglt050-bukrs.
      wl_zglt035-tp_lcto        = wl_zglt064-tp_lcto_aprop.
      wl_zglt035-dpto_resp      = wl_zglt064-dep_resp.
      wl_zglt035-taxa           = wl_saida_0150-wkurs.
      wl_zglt035-moeda_doc      = wl_zglt050-waers.
      wl_zglt035-st_lc_moeda    = wl_zglt031-st_lc_moeda.
      wl_zglt035-moeda_interna  = wl_t001-waers.
      wl_zglt035-moeda_int_hist = wl_zglt031-moeda_int_hist.
      wl_zglt035-moeda_forte    = wl_zglt031-moeda_forte.
      wl_zglt035-moeda_ft_hist  = wl_zglt031-moeda_ft_hist.
      wl_zglt035-moeda_grupo    = wl_zglt031-moeda_grupo.
      wl_zglt035-moeda_gp_hist  = wl_zglt031-moeda_gp_hist.
      wl_zglt035-blart          = wl_zglt031-blart.
      wl_zglt035-xblnr          = wl_xblnr.
      wl_zglt035-bktxt          = wl_saida_0150-nro_apolice.
      wl_zglt035-budat          = wl_saida_0150-dt_apropr.
      wl_zglt035-bldat          = wl_saida_0150-dt_apropr.
      wl_zglt035-dt_lcto        = wl_saida_0150-dt_apropr.
      wl_zglt035-prov_est       = wl_zglt031-prov_est.
      wl_zglt035-st_ap_fiscal   = wl_zglt031-st_ap_fiscal.
      wl_zglt035-monat          = wl_saida_0150-dt_apropr+4(2).
      wl_zglt035-gjahr          = wl_saida_0150-dt_apropr(4).
      wl_zglt035-usnam          = sy-uname.
      wl_zglt035-dt_entrada     = sy-datum.
      wl_zglt035-hr_entrada     = sy-uzeit.
      wl_zglt035-lote_prec      = wl_zglt050-lote.
      wl_zglt035-doc_lcto_prec  = wl_zglt050-doc_lcto.
      wl_zglt035-belnr_prec     = wl_zglt050-belnr.

      CLEAR: gt_zglt036, vl_mes_ano, vl_sgtxt, vl_dt_parc.
**<<<------"164255 - NMS - INI------>>>
*      CONCATENATE wl_zglt035-monat '/' wl_zglt035-gjahr
*             INTO vl_mes_ano.
* "// US-164255 WBARBOSA 30/072025
*      VL_DT_APROP = WL_CABECALHO_0150-DT_APROP_DE.
*      VL_DT_APROP = WL_SAIDA_0150-DT_IN_VIG.
      vl_dt_aprop = wl_zglt050-vig_de.
* "// US-164255 WBARBOSA 30/072025

      vl_month    = wl_saida_0150-nro_parc - 1.

* "// US-164255 WBARBOSA 31/07/2025
      IF vl_dt_aprop+6(2) > 15.
        ADD 1 TO vl_month.
      ENDIF.
* "// US-164255 WBARBOSA 31/07/2025

* Faz o cálculo da data conforme a parcela de acordo com a data de apropriação inicial.
*** BUG - 189757 - CBRAND - Inicio
      CONCATENATE vl_dt_aprop+0(4) vl_dt_aprop+4(2) '01' INTO  vl_dt_parc.
*** Se passar na função 31.08.2025 - Vai gerar o texto como mês 10.
*** BUG - 189757 - CBRAND - Fim
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = vl_dt_parc "vl_dt_aprop "BUG - 189757 CSB
          days      = 0
          months    = vl_month
          signum    = '+'
          years     = 0
        IMPORTING
          calc_date = vl_dt_aprop.

      vl_mes_ano = |{ vl_dt_aprop+4(2) }/{ vl_dt_aprop(4) }|.
**<<<------"164255 - NMS - FIM------>>>
      CONCATENATE wl_zglt064-seq_tipo '-' wl_zglt064-descr
             INTO vl_sgtxt.

      CONCATENATE 'Aprop. Seguro' vl_sgtxt vl_mes_ano
             INTO vl_sgtxt SEPARATED BY space.

      LOOP AT gt_zglt032 INTO wl_zglt032.
        CLEAR: wl_zglt036.

        wl_zglt036-seqitem  = sy-tabix.
        wl_zglt036-tp_lcto  = wl_zglt032-tp_lcto.
        wl_zglt036-bschl    = wl_zglt032-bschl.
        wl_zglt036-kostl    = wl_saida_0150-kostl.
        "WL_ZGLT036-AUFNR    = WL_SAIDA_0130-AUFNR.
        wl_zglt036-aufnr    = _wl_068-aufnr.
        wl_zglt036-vornr    = _wl_068-vornr.
        wl_zglt036-gsber    = wl_saida_0150-werks.
        wl_zglt036-sgtxt    = vl_sgtxt.
        wl_zglt036-zuonr    = wl_zglt050-nro_apolice.

        IF ( wl_zglt050-waers = 'USD' ).
          wl_zglt036-vlr_moeda_doc = wl_saida_0150-vlr_premio_usd.
        ELSE.
          wl_zglt036-vlr_moeda_doc = wl_saida_0150-vlr_premio_brl.
        ENDIF.

        wl_zglt036-vlr_moeda_int   = wl_saida_0150-vlr_premio_brl.
        wl_zglt036-vlr_moeda_forte = wl_saida_0150-vlr_premio_usd.
        wl_zglt036-hkont           = wl_zglt032-hkont.

        "Se  a conta for  classe de custo  preencher ZGLT036-SEQSUB = 000001
* ---> S4 Migration - 17/07/2023 - CA
        CLEAR: wl_tka02,wl_skb1,wl_tbsl.
*        CLEAR: wl_tka02,wl_cskb,wl_skb1,wl_tbsl.
* <--- S4 Migration - 17/07/2023 - CA


        SELECT SINGLE *
          FROM tbsl INTO wl_tbsl
         WHERE bschl EQ wl_zglt036-bschl.

        SELECT SINGLE *
          FROM tka02 INTO wl_tka02
         WHERE bukrs  = wl_zglt035-bukrs.


* ---> S4 Migration - 17/07/2023 - CA
*        SELECT SINGLE *
*          FROM cskb INTO wl_cskb
*         WHERE kokrs  = wl_tka02-kokrs
*           AND kstar  = wl_zglt036-hkont+0(10)
*           AND datab  LE sy-datum
*           AND datbi  GE sy-datum.
* <--- S4 Migration - 17/07/2023 - CA


        IF '0200_0201' CS wl_zglt035-bukrs. "EUROPA
          SELECT SINGLE *              "#EC CI_DB_OPERATION_OK[2431747]
            FROM skb1 INTO wl_skb1
           WHERE bukrs = wl_zglt035-bukrs
             AND saknr = wl_zglt036-hkont+0(10)
             AND fstag   = 'YB09'.
        ENDIF.

* ---> S4 Migration - 17/07/2023 - CA
        lv_controllingarea  = wl_tka02-kokrs.
        lv_costelement      = wl_zglt036-hkont+0(10).
        lv_keydate          = sy-datum.

        CLEAR: lt_returns[], ls_coeldes.

        CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
          EXPORTING
            controllingarea   = lv_controllingarea
            costelement       = lv_costelement
            keydate           = lv_keydate
          IMPORTING
            costelementdetail = ls_coeldes
          TABLES
            return            = lt_returns.

        IF ( ls_coeldes-cost_elem IS NOT INITIAL OR wl_skb1-fstag   = 'YB09' ) AND wl_tbsl-koart  = 'S'.
*        IF ( wl_cskb-kstar IS NOT INITIAL OR wl_skb1-fstag   = 'YB09' ) AND wl_tbsl-koart  = 'S'.
* <--- S4 Migration - 17/07/2023 - CA
          wl_zglt036-seqsub   = 1.
        ELSE.
          CLEAR: wl_zglt036-kostl, wl_zglt036-seqsub.
        ENDIF.

        APPEND wl_zglt036 TO gt_zglt036.
        CLEAR wl_zglt036.
      ENDLOOP.

      r_gerar_lote->contabilizar_lote( CHANGING i_zglt036 = gt_zglt036
                                                i_zglt035 = wl_zglt035 ).

      wl_saida_0150-status      = icon_yellow_light.
      wl_saida_0150-lote        = wl_zglt035-lote.
      wl_saida_0150-doc_lcto    = wl_zglt035-doc_lcto.

      MODIFY gt_saida_0150 FROM wl_saida_0150 INDEX wl_selected_rows-index.

      wl_zglt073-seq_lcto       = wl_saida_0150-seq_lcto.
      wl_zglt073-dt_apropr      = wl_saida_0150-dt_apropr.
      wl_zglt073-nro_parc       = wl_saida_0150-nro_parc.
      wl_zglt073-nr_item        = wl_saida_0150-nr_item.
      wl_zglt073-anln1          = wl_saida_0150-anln1.
      wl_zglt073-anln2          = wl_saida_0150-anln2.
      wl_zglt073-invnr          = wl_saida_0150-invnr.
      wl_zglt073-sernr          = wl_saida_0150-sernr.
      wl_zglt073-matnr          = wl_saida_0150-matnr.
      wl_zglt073-descr_bens     = wl_saida_0150-descr_bens.
      wl_zglt073-werks          = wl_saida_0150-werks.
      wl_zglt073-kostl          = wl_saida_0150-kostl.
      wl_zglt073-wkurs          = wl_saida_0150-wkurs.
      wl_zglt073-vlr_premio_usd = wl_saida_0150-vlr_premio_usd.
      wl_zglt073-vlr_premio_brl = wl_saida_0150-vlr_premio_brl.
      wl_zglt073-dt_in_vig      = wl_saida_0150-dt_in_vig.
      wl_zglt073-dt_baixa       = wl_saida_0150-dt_baixa.
      wl_zglt073-lote           = wl_zglt035-lote.
      wl_zglt073-doc_lcto       = wl_zglt035-doc_lcto.
      wl_zglt073-month_baixa    = wl_saida_0150-month_baixa.

      APPEND wl_zglt073 TO gt_zglt073.
      CLEAR wl_zglt073.

    ENDLOOP.

    INSERT zglt073 FROM TABLE gt_zglt073.
    COMMIT WORK.

    "Aprovar Lote.
    LOOP AT it_lote_agrp INTO wl_lote_agrp.

      CLEAR: wl_zglt035.
      SELECT SINGLE *
        FROM zglt035 INTO wl_zglt035
       WHERE lote EQ wl_lote_agrp-lote.

      IF ( sy-subrc = 0 ).

        IF wl_zglt035-prov_est IS NOT INITIAL.
          PERFORM grava_zib IN PROGRAM zgl017 USING wl_zglt035-lote IF FOUND.
        ELSE.
          SUBMIT z_grava_zib_zgl WITH p_lote = wl_zglt035-lote AND RETURN.
        ENDIF.

        UPDATE zglt034
           SET status_lote = 'A'
         WHERE lote EQ wl_zglt035-lote.
      ENDIF.

    ENDLOOP.

    CALL METHOD obj_alv_0150->refresh_table_display
      EXPORTING
        is_stable = wl_stable.

    IF ( it_seq_erro[] IS NOT INITIAL ).
      MESSAGE 'Algumas apropriações possuem parcelas anteriores pendentes de lançamento!' TYPE 'I'.
    ENDIF.

  ENDMETHOD.                    "Z_GERAR_APROPRIACOES


  METHOD z_gerar_ajuste_aprop.

    TYPES: BEGIN OF ty_lote_agrp,
             bukrs TYPE ty_saida_0150-bukrs,
             werks TYPE ty_saida_0150-werks,
             lote  TYPE zglt035-lote,
           END OF ty_lote_agrp.

    DATA: wl_tka02     TYPE tka02,

* ---> S4 Migration - 17/07/2023 - CA
*          wl_cskb      TYPE cskb,
* <--- S4 Migration - 17/07/2023 - CA

          wl_skb1      TYPE skb1,
          wl_tbsl      TYPE tbsl,
          vl_mes_ano   TYPE string,
          vl_sgtxt     TYPE string,
          wl_zglt035   TYPE zglt035,
          vl_valida    TYPE c,
          it_lote_agrp TYPE TABLE OF ty_lote_agrp,
          wl_lote_agrp TYPE ty_lote_agrp,
          wl_xblnr     TYPE zglt035-xblnr.

* ---> S4 Migration - 17/07/2023 - CA
    DATA: lt_returns         TYPE TABLE OF bapiret2,
          ls_coeldes         TYPE bapi1030_ceoutputlist,
          lv_controllingarea TYPE bapi1030_gen-co_area,
          lv_costelement     TYPE bapi1030_gen-cost_elem,
          lv_keydate         TYPE bapi1030_gen-some_date.
* <--- S4 Migration - 17/07/2023 - CA


    DATA: lva_vlr_ajuste_usd TYPE zglt068-vlr_premio_usd, "Novo
          lva_vlr_ajuste_brl TYPE zglt068-vlr_premio_brl. "Novo

    CREATE OBJECT go_utils.
    CREATE OBJECT r_gerar_lote.

    CLEAR: gt_zglt073, gt_zglt036, it_lote_agrp[].

    LOOP AT gt_selected_rows INTO wl_selected_rows.
      CLEAR: wl_saida_0170, wl_zglt050, wl_zglt064, wl_zglt031, wl_zglt032, wl_t001, gt_zglt032[].

      READ TABLE:
      gt_saida_0170 INTO wl_saida_0170 INDEX wl_selected_rows-index,
      gt_zglt050    INTO wl_zglt050 WITH KEY seq_lcto = wl_saida_0170-seq_lcto.

      CHECK sy-subrc EQ 0.


      lva_vlr_ajuste_brl = wl_saida_0170-vlr_ajuste_brl.
      lva_vlr_ajuste_usd = wl_saida_0170-vlr_ajuste_usd.

      DATA(_ok) = abap_false.
      PERFORM f_check_authority USING wl_zglt050-bukrs
                                      abap_true
                             CHANGING _ok.

      CHECK _ok IS NOT INITIAL.

      SELECT SINGLE *
        FROM zglt068 INTO @DATA(_wl_0068)
       WHERE seq_lcto = @wl_saida_0170-seq_lcto
         AND nr_item  = @wl_saida_0170-nr_item.

      CHECK ( sy-subrc = 0 ).

      IF ( _wl_0068-lote_ajus IS NOT INITIAL ).
        MESSAGE s836(sd) WITH TEXT-e66 wl_saida_0170-nr_item  DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      go_utils->z_check_ctb_apolice( EXPORTING i_wl_050 = wl_zglt050
                                     IMPORTING e_valida = vl_valida ).
      IF vl_valida IS INITIAL.
        CONTINUE.
      ENDIF.

      SELECT SINGLE *
        FROM t001 INTO wl_t001
       WHERE bukrs = wl_zglt050-bukrs.

      SELECT SINGLE *
        FROM zglt064 INTO wl_zglt064
       WHERE seq_tipo = wl_zglt050-seq_tipo.

      IF sy-subrc NE 0.
        MESSAGE s836(sd) WITH TEXT-e33 DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      SELECT SINGLE *
        FROM zglt031 INTO wl_zglt031
       WHERE tp_lcto EQ wl_zglt064-tp_lcto_aprop.

      IF sy-subrc NE 0.
        MESSAGE s836(sd) WITH TEXT-e34 DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      SELECT *
        FROM zglt032
        INTO TABLE gt_zglt032
       WHERE tp_lcto EQ wl_zglt064-tp_lcto_aprop.

      IF gt_zglt032[] IS INITIAL.
        MESSAGE s836(sd) WITH TEXT-e32 DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      IF ( ( lva_vlr_ajuste_brl > 0 ) OR ( lva_vlr_ajuste_usd > 0 ) ).

        " inversão de chave
        LOOP AT gt_zglt032 ASSIGNING FIELD-SYMBOL(<fs_zglt032>).
          CASE <fs_zglt032>-bschl.
            WHEN '40'.
              <fs_zglt032>-bschl = 50.
            WHEN '50'.
              <fs_zglt032>-bschl = 40.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

      ENDIF.

      lva_vlr_ajuste_brl = abs( lva_vlr_ajuste_brl ).
      lva_vlr_ajuste_usd = abs( lva_vlr_ajuste_usd ).

*        READ TABLE gt_zglt032 INTO DATA(wa_bschl_1) INDEX 1. "WITH KEY bschl = '40'.
*
*        READ TABLE gt_zglt032 INTO DATA(wa_bschl_2) INDEX 2. "WITH KEY bschl = '50'.
*
*        READ TABLE gt_zglt032 ASSIGNING FIELD-SYMBOL(<fs_zglt032>) WITH KEY bschl = '40'.
*        IF sy-subrc IS INITIAL.
*          <fs_zglt032>-bschl = wa_bschl_2-bschl.
*        ENDIF.
*
*        READ TABLE gt_zglt032 ASSIGNING <fs_zglt032> WITH KEY bschl = '50'.
*        IF sy-subrc IS INITIAL.
*          <fs_zglt032>-bschl = wa_bschl_1-bschl.
*        ENDIF.

* Início - RMNI - CS1023310 - Ajuste Seq. lançamento 1982 e 1970 - 14.12.2022
      "        IF  ( wl_saida_0170-vlr_ajuste_brl < 0 ).
*          wl_saida_0170-vlr_ajuste_brl =   wl_saida_0170-vlr_ajuste_brl * ( -1 ).
      "        ENDIF.

      "        IF  ( wl_saida_0170-vlr_ajuste_usd < 0 ) .
*          wl_saida_0170-vlr_ajuste_usd =   wl_saida_0170-vlr_ajuste_usd * ( -1 ).
      "        ENDIF.

*      ENDIF.
* Fim - RMNI - CS1023310 - Ajuste Seq. lançamento 1982 e 1970 - 14.12.2022
      CHECK ( ( lva_vlr_ajuste_brl > 0 ) OR
              ( lva_vlr_ajuste_usd > 0 ) ).

      CHECK wl_saida_0170-lote_ajus IS INITIAL.

      CLEAR: e_status, e_messa.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          i_bukrs  = wl_zglt050-bukrs
          i_data   = wl_saida_0170-dt_lcto_ctb_ajus
        IMPORTING
          e_status = e_status
          e_messa  = e_messa
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF e_status = 'E'.
        MESSAGE e_messa TYPE 'S'.
        CONTINUE.
      ENDIF.

      CLEAR: wl_xblnr.
      CONCATENATE wl_zglt050-vig_de+06(2) '/'
                  wl_zglt050-vig_de+4(02) '/'
                  wl_zglt050-vig_de(04) '-'
                  wl_zglt050-vig_ate(04) INTO wl_xblnr.

      READ TABLE it_lote_agrp INTO wl_lote_agrp WITH KEY bukrs = wl_saida_0170-bukrs
                                                         werks = wl_saida_0170-werks.

      IF ( sy-subrc = 0 ) AND ( wl_lote_agrp-lote IS NOT INITIAL ).
        wl_zglt035-lote = wl_lote_agrp-lote.
      ELSE.
        r_gerar_lote->create_lote( EXPORTING i_bukrs      = wl_zglt050-bukrs
                                             i_descr_lote = 'Ajuste Apropriação Seguros'
                                             i_dep_resp   = wl_zglt050-dep_resp
                                             i_user_resp  = sy-uname
                                   IMPORTING e_num_lote   = wl_zglt035-lote ).

        CLEAR: wl_lote_agrp.
        wl_lote_agrp-bukrs = wl_saida_0170-bukrs.
        wl_lote_agrp-werks = wl_saida_0170-werks.
        wl_lote_agrp-lote  = wl_zglt035-lote.
        APPEND wl_lote_agrp TO it_lote_agrp.
      ENDIF.

      wl_zglt035-bukrs          = wl_zglt050-bukrs.
      wl_zglt035-tp_lcto        = wl_zglt064-tp_lcto_aprop.
      wl_zglt035-dpto_resp      = wl_zglt064-dep_resp.
      wl_zglt035-taxa           = wl_saida_0170-wkurs.
      wl_zglt035-moeda_doc      = wl_zglt050-waers.
      wl_zglt035-st_lc_moeda    = wl_zglt031-st_lc_moeda.
      wl_zglt035-moeda_interna  = wl_t001-waers.
      wl_zglt035-moeda_int_hist = wl_zglt031-moeda_int_hist.
      wl_zglt035-moeda_forte    = wl_zglt031-moeda_forte.
      wl_zglt035-moeda_ft_hist  = wl_zglt031-moeda_ft_hist.
      wl_zglt035-moeda_grupo    = wl_zglt031-moeda_grupo.
      wl_zglt035-moeda_gp_hist  = wl_zglt031-moeda_gp_hist.
      wl_zglt035-blart          = wl_zglt031-blart.
      wl_zglt035-xblnr          = wl_xblnr.
      wl_zglt035-bktxt          = wl_saida_0170-nro_apolice.
      wl_zglt035-budat          = wl_saida_0170-dt_lcto_ctb_ajus.
      wl_zglt035-bldat          = wl_saida_0170-dt_lcto_ctb_ajus.
      wl_zglt035-dt_lcto        = wl_saida_0170-dt_lcto_ctb_ajus.
      wl_zglt035-prov_est       = wl_zglt031-prov_est.
      wl_zglt035-st_ap_fiscal   = wl_zglt031-st_ap_fiscal.
      wl_zglt035-monat          = wl_saida_0170-dt_lcto_ctb_ajus+4(2).
      wl_zglt035-gjahr          = wl_saida_0170-dt_lcto_ctb_ajus(4).
      wl_zglt035-usnam          = sy-uname.
      wl_zglt035-dt_entrada     = sy-datum.
      wl_zglt035-hr_entrada     = sy-uzeit.
      wl_zglt035-lote_prec      = wl_zglt050-lote.
      wl_zglt035-doc_lcto_prec  = wl_zglt050-doc_lcto.
      wl_zglt035-belnr_prec     = wl_zglt050-belnr.

      CLEAR: gt_zglt036, vl_mes_ano, vl_sgtxt.

      CONCATENATE wl_zglt035-monat '/' wl_zglt035-gjahr
             INTO vl_mes_ano.

      CONCATENATE wl_zglt064-seq_tipo '-' wl_zglt064-descr
             INTO vl_sgtxt.

      CONCATENATE 'Ajuste Aprop. Seguro' vl_sgtxt vl_mes_ano
             INTO vl_sgtxt SEPARATED BY space.

      LOOP AT gt_zglt032 INTO wl_zglt032.
        CLEAR: wl_zglt036.

        wl_zglt036-seqitem  = sy-tabix.
        wl_zglt036-tp_lcto  = wl_zglt032-tp_lcto.
        wl_zglt036-bschl    = wl_zglt032-bschl.

*        CASE wl_zglt032-bschl.
*          WHEN '50'.
*            wl_zglt036-bschl = '40'.
*          WHEN '40'.
*            wl_zglt036-bschl = '50'.
*        ENDCASE.

        IF wl_zglt036-bschl IS INITIAL.
          MESSAGE 'Chave 40/50 não encontrada!' TYPE 'S'.
          RETURN.
        ENDIF.

        wl_zglt036-kostl    = wl_saida_0170-kostl.
        wl_zglt036-gsber    = wl_saida_0170-werks.
        wl_zglt036-sgtxt    = vl_sgtxt.
        wl_zglt036-zuonr    = wl_zglt050-nro_apolice.

        IF ( wl_zglt050-waers = 'USD' ).
          wl_zglt036-vlr_moeda_doc = lva_vlr_ajuste_usd.
        ELSE.
          wl_zglt036-vlr_moeda_doc = lva_vlr_ajuste_brl.
        ENDIF.

        wl_zglt036-vlr_moeda_int   = lva_vlr_ajuste_brl.
        wl_zglt036-vlr_moeda_forte = lva_vlr_ajuste_usd.
        wl_zglt036-hkont           = wl_zglt032-hkont.

        "Se  a conta for  classe de custo  preencher ZGLT036-SEQSUB = 000001
* ---> S4 Migration - 17/07/2023 - CA

        CLEAR: wl_tka02,wl_skb1,wl_tbsl.
*        CLEAR: wl_tka02,wl_cskb,wl_skb1,wl_tbsl.
* <--- S4 Migration - 17/07/2023 - CA

        SELECT SINGLE *
          FROM tbsl INTO wl_tbsl
         WHERE bschl EQ wl_zglt036-bschl.

        SELECT SINGLE *
          FROM tka02 INTO wl_tka02
         WHERE bukrs  = wl_zglt035-bukrs.

* ---> S4 Migration - 17/07/2023 - CA
*        SELECT SINGLE *
*          FROM cskb INTO wl_cskb
*         WHERE kokrs  = wl_tka02-kokrs
*           AND kstar  = wl_zglt036-hkont+0(10)
*           AND datab  LE sy-datum
*           AND datbi  GE sy-datum.
* <--- S4 Migration - 17/07/2023 - CA


        IF '0200_0201' CS wl_zglt035-bukrs. "EUROPA
          SELECT SINGLE *              "#EC CI_DB_OPERATION_OK[2431747]
            FROM skb1 INTO wl_skb1
           WHERE bukrs = wl_zglt035-bukrs
             AND saknr = wl_zglt036-hkont+0(10)
             AND fstag   = 'YB09'.
        ENDIF.

* ---> S4 Migration - 17/07/2023 - CA
        lv_controllingarea  = wl_tka02-kokrs.
        lv_costelement      = wl_zglt036-hkont+0(10).
        lv_keydate          = sy-datum.

        CLEAR: lt_returns[], ls_coeldes.

        CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
          EXPORTING
            controllingarea   = lv_controllingarea
            costelement       = lv_costelement
            keydate           = lv_keydate
          IMPORTING
            costelementdetail = ls_coeldes
          TABLES
            return            = lt_returns.

        IF ( ls_coeldes-cost_elem IS NOT INITIAL OR wl_skb1-fstag   = 'YB09' ) AND wl_tbsl-koart  = 'S'.
*        IF ( wl_cskb-kstar IS NOT INITIAL OR wl_skb1-fstag   = 'YB09' ) AND wl_tbsl-koart  = 'S'.
* <--- S4 Migration - 17/07/2023 - CA
          wl_zglt036-seqsub   = 1.
        ELSE.
          CLEAR: wl_zglt036-kostl, wl_zglt036-seqsub.
        ENDIF.

        APPEND wl_zglt036 TO gt_zglt036.
        CLEAR wl_zglt036.
      ENDLOOP.

      r_gerar_lote->contabilizar_lote( CHANGING i_zglt036 = gt_zglt036
                                                i_zglt035 = wl_zglt035 ).

      wl_saida_0170-status         = icon_yellow_light.
      wl_saida_0170-lote_ajus      = wl_zglt035-lote.
      wl_saida_0170-doc_lcto_ajus  = wl_zglt035-doc_lcto.

      UPDATE zglt068 SET lote_ajus        = wl_zglt035-lote
                         doc_lcto_ajus    = wl_zglt035-doc_lcto
                         dt_lcto_ctb_ajus = wl_saida_0170-dt_lcto_ctb_ajus
* Início - RMNI - CS1023310 - Ajuste Seq. lançamento 1982 e 1970 - 16.12.2022
*                         vlr_aprop_aj_usd = lva_vlr_ajuste_usd
*                         vlr_aprop_aj_brl = lva_vlr_ajuste_brl
                         vlr_aprop_aj_usd = wl_saida_0170-vlr_ajuste_usd
                         vlr_aprop_aj_brl = wl_saida_0170-vlr_ajuste_brl
* Fim - RMNI - CS1023310 - Ajuste Seq. lançamento 1982 e 1970 - 16.12.2022
                   WHERE seq_lcto = wl_saida_0170-seq_lcto
                     AND nr_item  = wl_saida_0170-nr_item.

      COMMIT WORK.

      MODIFY gt_saida_0170 FROM wl_saida_0170 INDEX wl_selected_rows-index.

    ENDLOOP.

    "Aprovar Lote.
    LOOP AT it_lote_agrp INTO wl_lote_agrp.

      CLEAR: wl_zglt035.
      SELECT SINGLE *
        FROM zglt035 INTO wl_zglt035
       WHERE lote EQ wl_lote_agrp-lote.

      IF ( sy-subrc = 0 ).

        IF wl_zglt035-prov_est IS NOT INITIAL.
          PERFORM grava_zib IN PROGRAM zgl017 USING wl_zglt035-lote IF FOUND.
        ELSE.
          SUBMIT z_grava_zib_zgl WITH p_lote = wl_zglt035-lote AND RETURN.
        ENDIF.

        UPDATE zglt034
           SET status_lote = 'A'
         WHERE lote EQ wl_zglt035-lote.
      ENDIF.

    ENDLOOP.

    CALL METHOD obj_alv_0170->refresh_table_display
      EXPORTING
        is_stable = wl_stable.
  ENDMETHOD.                    "Z_GERAR_AJUSTE_APROP

  METHOD z_estorno_apropriacoes.


    TYPES: BEGIN OF ty_zib_err,
             bukrs   TYPE bkpf-bukrs,
             belnr   TYPE bkpf-belnr,
             gjahr   TYPE bkpf-gjahr,
             message TYPE bapiret2-message,
           END OF ty_zib_err.

    DATA: it_dta   TYPE STANDARD TABLE OF bdcdata,
          wa_dta   TYPE bdcdata,
          wg_bdc   TYPE bdcdata,
          tg_bdc   TYPE TABLE OF bdcdata,
          tg_msg   TYPE TABLE OF bdcmsgcoll,
          wg_msg   TYPE bdcmsgcoll,
          opt      TYPE ctu_params,
          vl_stblg TYPE bkpf-stblg.

    DATA: it_msg     TYPE TABLE OF bdcmsgcoll,
          it_zib_err TYPE TABLE OF ty_zib_err,
          it_bapiret TYPE TABLE OF bapiret2.

    FIELD-SYMBOLS: <saida_0150> TYPE ty_saida_0150.

*    READ TABLE gt_selected_rows INTO wl_selected_rows INDEX 1.

*-US 83767-20-02-2025-#83767-RJF-Inicio
    SELECT * FROM zib_contabil
    INTO TABLE @DATA(it_zib_contabil)
    FOR ALL ENTRIES IN @gt_saida_0150
    WHERE xblnr EQ @gt_saida_0150-xblnr.
*-US 83767-20-02-2025-#83767-RJF-Fim

    LOOP AT gt_selected_rows  INTO wl_selected_rows.

      READ TABLE gt_saida_0150 ASSIGNING <saida_0150> INDEX wl_selected_rows-index.

      DATA(_ok) = abap_false.
      PERFORM f_check_authority USING <saida_0150>-bukrs
                                      abap_true
                             CHANGING _ok.

      CHECK _ok IS NOT INITIAL.

      IF <saida_0150>-doc_contabil IS INITIAL.
        ROLLBACK WORK.
        MESSAGE s836(sd) WITH TEXT-e48 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

*-US 83767-20-02-2025-#83767-RJF-Inicio
      IF <saida_0150>-doc_contabil IS NOT INITIAL.

        READ TABLE it_zib_contabil INTO DATA(wa_contabil) WITH KEY xblnr = <saida_0150>-xblnr.
        IF sy-subrc IS INITIAL.
          SELECT * FROM zib_contabil_chv
            UP TO 1 ROWS
            INTO @DATA(wa_contabil_chv)
            WHERE obj_key EQ @wa_contabil-obj_key
              AND bukrs   EQ @wa_contabil-bukrs
              AND gjahr   EQ @wa_contabil-gjahr.
          ENDSELECT.

          IF sy-subrc IS INITIAL.
            SELECT * FROM bkpf
              UP TO 1 ROWS
              INTO @DATA(wa_bkpf)
              WHERE belnr EQ @wa_contabil-xblnr(10)
                AND bukrs EQ @wa_contabil-bukrs
                AND gjahr EQ @wa_contabil-gjahr.
            ENDSELECT.
            IF sy-subrc IS INITIAL AND wa_bkpf-stblg IS INITIAL.
              ROLLBACK WORK.
              MESSAGE s836(sd) WITH TEXT-e76 DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*-US 83767-20-02-2025-#83767-RJF-Fim

      SELECT SINGLE *
        FROM zglt068 INTO @DATA(_wl_068)
       WHERE seq_lcto = @<saida_0150>-seq_lcto
         AND nr_item  = @<saida_0150>-nr_item.

      IF sy-subrc NE 0 .
        ROLLBACK WORK.
        MESSAGE s836(sd) WITH TEXT-e69 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF _wl_068-lote_ajus IS NOT INITIAL.
        ROLLBACK WORK.
        MESSAGE s836(sd) WITH TEXT-e70 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CLEAR wl_zglt073.
      SELECT SINGLE *
        FROM zglt073 INTO wl_zglt073
       WHERE seq_lcto  = <saida_0150>-seq_lcto
         AND nro_parc  = <saida_0150>-nro_parc
         AND doc_lcto  = <saida_0150>-doc_lcto.

      IF ( wl_zglt073-doc_lcto IS INITIAL ).
        ROLLBACK WORK.
        MESSAGE s836(sd) WITH TEXT-e47 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.


    ENDLOOP.

    DATA(vl_budat_str) = |{ vl_budat+6(2) }.{ vl_budat+4(2) }.{ vl_budat+0(4) }|.

    FREE: it_dta.
    DEFINE shdb.
      CLEAR wa_dta.
      wa_dta-program   = &1.
      wa_dta-dynpro    = &2.
      wa_dta-dynbegin  = &3.
      wa_dta-fnam      = &4.
      wa_dta-fval      = &5.
      APPEND wa_dta TO it_dta.
    END-OF-DEFINITION.

    LOOP AT gt_selected_rows  INTO wl_selected_rows.

      READ TABLE gt_saida_0150 ASSIGNING <saida_0150> INDEX wl_selected_rows-index.

      shdb:
      'SAPMF05A' '0105' 'X'  ' '           ' ',
      ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
      ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
      ' '        ' '    ' '  'RF05A-BELNS' <saida_0150>-doc_contabil,
      ' '        ' '    ' '  'BKPF-BUKRS'  <saida_0150>-bukrs,
      ' '        ' '    ' '  'RF05A-GJAHS' <saida_0150>-dt_apropr(4),
      ' '        ' '    ' '  'UF05A-STGRD' 'Z1',
      ' '        ' '    ' '  'BSIS-BUDAT'  vl_budat_str.

      opt-dismode = 'N'."'E'.

      CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt
               MESSAGES INTO it_msg.

      READ TABLE it_msg ASSIGNING FIELD-SYMBOL(<fs_msg>)
                                        WITH KEY msgtyp = 'E'.

      IF sy-subrc IS INITIAL
        AND ( <fs_msg>-msgid <> 'F5' OR <fs_msg>-msgnr <> 361 ).

        LOOP AT it_msg ASSIGNING <fs_msg>.

          APPEND INITIAL LINE TO it_zib_err ASSIGNING FIELD-SYMBOL(<fs_zib_err>).

          <fs_zib_err>-bukrs  = <saida_0150>-bukrs.
          <fs_zib_err>-belnr = <saida_0150>-doc_contabil.
          <fs_zib_err>-gjahr = <saida_0150>-dt_apropr(4).

          MESSAGE ID <fs_msg>-msgid TYPE <fs_msg>-msgtyp NUMBER <fs_msg>-msgnr
            INTO DATA(l_msg)
            WITH <fs_msg>-msgv1 <fs_msg>-msgv2 <fs_msg>-msgv3 <fs_msg>-msgv4.

          <fs_zib_err>-message        = l_msg.

        ENDLOOP.

      ELSE.
*      CHECK SY-SUBRC IS INITIAL.

        SELECT SINGLE stblg
          FROM bkpf INTO vl_stblg
         WHERE bukrs = <saida_0150>-bukrs
           AND belnr = <saida_0150>-doc_contabil
           AND gjahr = <saida_0150>-dt_apropr(4).

        CHECK ( sy-subrc = 0 ) AND ( vl_stblg IS NOT INITIAL ).

        DELETE FROM zglt073 WHERE seq_lcto = <saida_0150>-seq_lcto
                              AND nro_parc = <saida_0150>-nro_parc
                              AND doc_lcto = <saida_0150>-doc_lcto.

        MOVE abap_false     TO <saida_0150>-lote.
        MOVE abap_false     TO <saida_0150>-doc_lcto.
        MOVE abap_false     TO <saida_0150>-doc_contabil.
        MOVE icon_light_out TO <saida_0150>-status.
        COMMIT WORK.

        APPEND INITIAL LINE TO it_zib_err ASSIGNING <fs_zib_err>.
        <fs_zib_err>-bukrs  = <saida_0150>-bukrs.
        <fs_zib_err>-belnr = <saida_0150>-doc_contabil.
        <fs_zib_err>-gjahr = <saida_0150>-dt_apropr(4).
        MESSAGE s836(sd) INTO l_msg WITH TEXT-s04.
        <fs_zib_err>-message        = l_msg.

      ENDIF.

      REFRESH: it_msg, it_dta. CLEAR opt.

    ENDLOOP.

    IF it_zib_err[] IS NOT INITIAL.

      cl_demo_output=>new(
        )->begin_section( `FB08:`
        )->write_text( |Log de Erros de processamento estorno contábeis: \n|
        ")->WRITE_DATA( SY-DATUM
        )->write_data( it_zib_err[]
        )->end_section(
        )->display( ).

    ENDIF.

    CALL METHOD obj_alv_0150->refresh_table_display.

  ENDMETHOD.

  METHOD z_estorno_ajuste_aprop.

    DATA: it_dta   TYPE STANDARD TABLE OF bdcdata,
          wa_dta   TYPE bdcdata,
          wg_bdc   TYPE bdcdata,
          tg_bdc   TYPE TABLE OF bdcdata,
          tg_msg   TYPE TABLE OF bdcmsgcoll,
          wg_msg   TYPE bdcmsgcoll,
          opt      TYPE ctu_params,
          vl_stblg TYPE bkpf-stblg.


    FIELD-SYMBOLS: <saida_0170> TYPE ty_saida_0170.

    READ TABLE gt_selected_rows INTO wl_selected_rows INDEX 1.
    READ TABLE gt_saida_0170 ASSIGNING <saida_0170> INDEX wl_selected_rows-index.

    DATA(_ok) = abap_false.
    PERFORM f_check_authority USING <saida_0170>-bukrs
                                    abap_true
                           CHANGING _ok.

    CHECK _ok IS NOT INITIAL.

    IF <saida_0170>-doc_contabil IS INITIAL.
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e48 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zglt068 INTO @DATA(_wl_068)
     WHERE seq_lcto  = @<saida_0170>-seq_lcto
       AND nr_item   = @<saida_0170>-nr_item.

    IF ( _wl_068-doc_lcto_ajus IS INITIAL ).
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e47 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    FREE: it_dta.
    DEFINE shdb.
      CLEAR wa_dta.
      wa_dta-program   = &1.
      wa_dta-dynpro    = &2.
      wa_dta-dynbegin  = &3.
      wa_dta-fnam      = &4.
      wa_dta-fval      = &5.
      APPEND wa_dta TO it_dta.
    END-OF-DEFINITION.

    shdb:
    'SAPMF05A' '0105' 'X'  ' '           ' ',
    ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
    ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
    ' '        ' '    ' '  'RF05A-BELNS' <saida_0170>-doc_contabil,
    ' '        ' '    ' '  'BKPF-BUKRS'  <saida_0170>-bukrs,
    ' '        ' '    ' '  'RF05A-GJAHS' <saida_0170>-dt_lcto_ctb_ajus(4),
    ' '        ' '    ' '  'UF05A-STGRD' '01'.

    opt-dismode = 'E'.
    CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE stblg
      FROM bkpf INTO vl_stblg
     WHERE bukrs = <saida_0170>-bukrs
       AND belnr = <saida_0170>-doc_contabil
       AND gjahr = <saida_0170>-dt_lcto_ctb_ajus(4).

    CHECK ( sy-subrc = 0 ) AND ( vl_stblg IS NOT INITIAL ).

    CLEAR: _wl_068-lote_ajus,
           _wl_068-doc_lcto_ajus,
           _wl_068-dt_lcto_ctb_ajus,
           _wl_068-vlr_aprop_aj_usd,
           _wl_068-vlr_aprop_aj_brl.

    MODIFY zglt068 FROM _wl_068.
    COMMIT WORK.

    MOVE abap_false     TO <saida_0170>-lote_ajus.
    MOVE abap_false     TO <saida_0170>-doc_lcto_ajus.
    MOVE abap_false     TO <saida_0170>-doc_contabil.
    MOVE icon_light_out TO <saida_0170>-status.

    MESSAGE s836(sd) WITH TEXT-s04 DISPLAY LIKE 'S'.

    CALL METHOD obj_alv_0170->refresh_table_display.

  ENDMETHOD. " Z_ESTORNO_AJUSTE_APROP


  METHOD z_estorno_pagar_receber.

    DATA: it_dta   TYPE STANDARD TABLE OF bdcdata,
          wa_dta   TYPE bdcdata,
          wg_bdc   TYPE bdcdata,
          tg_bdc   TYPE TABLE OF bdcdata,
          tg_msg   TYPE TABLE OF bdcmsgcoll,
          wg_msg   TYPE bdcmsgcoll,
          opt      TYPE ctu_params,
          vl_stblg TYPE bkpf-stblg.

    FIELD-SYMBOLS: <saida_0160> TYPE ty_saida_0160.

    READ TABLE gt_selected_rows INTO wl_selected_rows INDEX 1.
    READ TABLE gt_saida_0160 ASSIGNING <saida_0160> INDEX wl_selected_rows-index.

    IF <saida_0160>-doc_contabil IS INITIAL.
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e48 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(_ok) = abap_false.
    PERFORM f_check_authority USING <saida_0160>-bukrs
                                    abap_true
                           CHANGING _ok.

    CHECK _ok IS NOT INITIAL.

    CLEAR wl_zglt067.
    SELECT SINGLE *
      FROM zglt067 INTO wl_zglt067
     WHERE seq_lcto  = <saida_0160>-seq_lcto
       AND nro_parc  = <saida_0160>-nro_parc
       AND doc_lcto  = <saida_0160>-nro_documento.

    IF ( wl_zglt067-doc_lcto IS INITIAL ).
      ROLLBACK WORK.
      MESSAGE s836(sd) WITH TEXT-e47 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF wl_zglt067-dt_lcto_ctb IS INITIAL.
      wl_zglt067-dt_lcto_ctb = wl_zglt067-erdat.
    ENDIF.

    FREE: it_dta.
    DEFINE shdb.
      CLEAR wa_dta.
      wa_dta-program   = &1.
      wa_dta-dynpro    = &2.
      wa_dta-dynbegin  = &3.
      wa_dta-fnam      = &4.
      wa_dta-fval      = &5.
      APPEND wa_dta TO it_dta.
    END-OF-DEFINITION.

    shdb:
    'SAPMF05A' '0105' 'X'  ' '           ' ',
    ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
    ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
    ' '        ' '    ' '  'RF05A-BELNS' <saida_0160>-doc_contabil,
    ' '        ' '    ' '  'BKPF-BUKRS'  <saida_0160>-bukrs,
    ' '        ' '    ' '  'RF05A-GJAHS' wl_zglt067-dt_lcto_ctb(4),
    ' '        ' '    ' '  'UF05A-STGRD' '01'.

    opt-dismode = 'E'.
    CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE stblg
      FROM bkpf INTO vl_stblg
     WHERE bukrs = <saida_0160>-bukrs
       AND belnr = <saida_0160>-doc_contabil
       AND gjahr = wl_zglt067-dt_lcto_ctb(4).

    CHECK ( sy-subrc = 0 ) AND ( vl_stblg IS NOT INITIAL ).

    MOVE abap_false      TO <saida_0160>-lote.
    MOVE abap_false      TO <saida_0160>-nro_documento.
    MOVE abap_false      TO <saida_0160>-doc_contabil.
    MOVE icon_light_out  TO <saida_0160>-status.

    MOVE abap_false TO wl_zglt067-lote.
    MOVE abap_false TO wl_zglt067-doc_lcto.
    MOVE abap_false TO wl_zglt067-dt_lcto_ctb.

    MODIFY zglt067 FROM wl_zglt067.

    COMMIT WORK.
    MESSAGE s836(sd) WITH TEXT-s04 DISPLAY LIKE 'S'.


    CALL METHOD obj_alv_0160->refresh_table_display.

  ENDMETHOD.

  METHOD z_baixar_bem_seg.

    DATA: gt_zglt068_aux TYPE TABLE OF zglt068,
          vl_dt_baixa    TYPE zglt068-dt_baixa.

    CREATE OBJECT: go_utils.

    "Seleção de Bens da Apolice Original.
    CLEAR: gt_saida_0132[].

    SELECT *
      FROM zglt068 INTO TABLE gt_zglt068_aux
     WHERE seq_lcto EQ wl_cabecalho_0110-ref_seq_lcto.

    SORT gt_zglt068_aux BY nr_item.

    LOOP AT gt_zglt068_aux INTO wl_zglt068.
      CLEAR: gt_estilo[], wl_saida_0132.

      SELECT SINGLE a~dt_baixa INTO vl_dt_baixa
        FROM zglt068 AS a INNER JOIN zglt050 AS b ON a~seq_lcto = b~seq_lcto
       WHERE b~tp_opr       = 'B' "Baixa.
         AND b~loekz        = ''
         AND b~ref_seq_lcto = wl_zglt068-seq_lcto
         AND a~nr_item      = wl_zglt068-nr_item
         AND a~dt_baixa     NE '00000000'.

      CHECK sy-subrc NE 0.

      READ TABLE gt_saida_0130 INTO wl_saida_0130 WITH KEY nr_item = wl_zglt068-nr_item.

      CHECK sy-subrc NE 0.

      wl_saida_0132-nr_item         = wl_zglt068-nr_item.
      wl_saida_0132-imobilizado     = wl_zglt068-anln1.
      wl_saida_0132-subnumero       = wl_zglt068-anln2.
      wl_saida_0132-mercadoria      = wl_zglt068-matnr.
      wl_saida_0132-descr_bens      = wl_zglt068-descr_bens.
      wl_saida_0132-filial          = wl_zglt068-werks.
      wl_saida_0132-chassi          = wl_zglt068-invnr.
      wl_saida_0132-nr_serie        = wl_zglt068-sernr.
      wl_saida_0132-centro_custo    = wl_zglt068-kostl.
      wl_saida_0132-taxa_cambio     = wl_zglt068-wkurs.
      wl_saida_0132-vlr_premio_usd  = wl_zglt068-vlr_premio_usd.
      wl_saida_0132-vlr_premio_brl  = wl_zglt068-vlr_premio_brl.
      wl_saida_0132-vlr_aj_prem_usd = wl_zglt068-vlr_aj_prem_usd.
      wl_saida_0132-vlr_aj_prem_brl = wl_zglt068-vlr_aj_prem_brl.
      wl_saida_0132-vlr_risco_usd   = wl_zglt068-vlr_risco_usd.
      wl_saida_0132-vlr_risco_brl   = wl_zglt068-vlr_risco_brl.
      wl_saida_0132-dt_inic_vigenc  = wl_zglt068-dt_in_vig.
      "WL_SAIDA_0132-DT_BAIXA        = WL_ZGLT068-DT_BAIXA.
      wl_saida_0132-banco           = wl_zglt068-banco.

      IF ( wl_zglt068-clau_benef = abap_true ).
        wl_saida_0132-clau_benef = 'SIM'.
      ELSE.
        wl_saida_0132-clau_benef = 'NÃO'.
      ENDIF.

      go_utils->z_style_disable_edit( fieldname = 'BANCO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CENTRO_CUSTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CHASSI'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CLAU_BENEF'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DESCR_BENS'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      "GO_UTILS->Z_STYLE_DISABLE_EDIT( FIELDNAME = 'DT_BAIXA'
      "                               STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED ).
      go_utils->z_style_disable_edit( fieldname = 'DT_INIC_VIGENC'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'FILIAL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'IMOBILIZADO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'MERCADORIA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SEQ_LCTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SUBNUMERO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'TAXA_CAMBIO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'UF'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).

      INSERT LINES OF gt_estilo INTO TABLE wl_saida_0132-estilo.
      APPEND wl_saida_0132 TO gt_saida_0132.
    ENDLOOP.

    screen_item      = c_screen_0132.
    CALL SCREEN 0132 STARTING AT 04 04 ENDING AT 150 20.
  ENDMETHOD.

  METHOD z_inc_bens_baixa.

    CREATE OBJECT: go_utils.

    IF ( wl_cabecalho_0110-vlr_premio_brl <= 0 ) OR
       ( wl_cabecalho_0110-vlr_premio_usd <= 0 ).
      MESSAGE s836(sd) WITH TEXT-e50 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT gt_saida_0132 INTO wl_saida_0132 WHERE check IS NOT INITIAL.
      CLEAR: wl_saida_0130, gt_estilo[].

      MOVE-CORRESPONDING wl_saida_0132 TO wl_saida_0130.

      LOOP AT gt_saida_0120 INTO wl_saida_0120 WHERE dt_venc IS NOT INITIAL.
        wl_saida_0130-dt_baixa = wl_saida_0120-dt_venc.
        EXIT.
      ENDLOOP.
****      WL_SAIDA_0130-DT_BAIXA = WL_CABECALHO_0110-VIG_ATE.

      CLEAR: wl_saida_0130-vlr_aj_prem_usd, wl_saida_0130-vlr_aj_prem_brl,
             wl_saida_0130-vlr_risco_usd  , wl_saida_0130-vlr_risco_brl.

      CLEAR: wl_saida_0130-estilo.

      go_utils->z_style_disable_edit( fieldname = 'BANCO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CENTRO_CUSTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CHASSI'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'CLAU_BENEF'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DESCR_BENS'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DT_BAIXA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'DT_INIC_VIGENC'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'FILIAL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'IMOBILIZADO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'MERCADORIA'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SEQ_LCTO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'SUBNUMERO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'TAXA_CAMBIO'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'UF'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_AJ_PREM_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_PREMIO_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_BRL'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VLR_RISCO_USD'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'AUFNR'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).
      go_utils->z_style_disable_edit( fieldname = 'VORNR'
                                      style     = cl_gui_alv_grid=>mc_style_disabled ).

      INSERT LINES OF gt_estilo INTO TABLE wl_saida_0130-estilo.

      APPEND wl_saida_0130 TO gt_saida_0130.
    ENDLOOP.

  ENDMETHOD.

  METHOD z_modify_centro.

    DATA: var_answer   TYPE c,
          vl_kostl_sel TYPE csks-kostl,
          wl_079       TYPE zglt079.

    FIELD-SYMBOLS <saida_0130> TYPE ty_saida_0130.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_change_werks-kostl
      IMPORTING
        output = wl_change_werks-kostl.

    IF wl_change_werks-werks IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e42 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF wl_change_werks-kostl IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-e43 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT SINGLE kostl
      FROM csks INTO vl_kostl_sel
     WHERE bukrs  = wl_cabecalho_0110-bukrs
       AND gsber  = wl_change_werks-werks
       AND kostl  = wl_change_werks-kostl
       AND datab  LE sy-datum
       AND datbi  GE sy-datum.

    IF sy-subrc NE 0.
      MESSAGE s836(sd) WITH TEXT-e35 TEXT-e36 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja realmente alterar o Centro de Custo do(s) registro(s) selecionado(s)?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK var_answer EQ '1'.

    LOOP AT gt_selected_rows INTO wl_selected_rows.
      CLEAR: wl_079.
      READ TABLE: gt_saida_0130 ASSIGNING <saida_0130> INDEX wl_selected_rows-index.
      CHECK sy-subrc = 0.

      IF ( <saida_0130>-nr_item IS INITIAL ).
        ROLLBACK WORK.
        MESSAGE s836(sd) WITH TEXT-e44 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      "Grava Log Alteração.
      wl_079-seq_lcto   = wl_cabecalho_0110-seq_lcto.
      wl_079-nr_item    = <saida_0130>-nr_item.
      wl_079-dt_atual   = sy-datum.
      wl_079-hr_atual   = sy-uzeit.
      wl_079-usname     = sy-uname.
      wl_079-old_werks  = <saida_0130>-filial.
      wl_079-old_kostl  = <saida_0130>-centro_custo.
      wl_079-new_werks  = wl_change_werks-werks.
      wl_079-new_kostl  = wl_change_werks-kostl.

      MODIFY zglt079 FROM wl_079.

      IF ( sy-subrc NE 0 ).
        ROLLBACK WORK.
        MESSAGE s836(sd) WITH TEXT-e45 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      UPDATE zglt068 SET werks = wl_change_werks-werks
                         kostl = wl_change_werks-kostl
        WHERE seq_lcto = wl_cabecalho_0110-seq_lcto
          AND nr_item  = <saida_0130>-nr_item.

      IF ( sy-subrc NE 0 ).
        ROLLBACK WORK.
        MESSAGE s836(sd) WITH TEXT-e45 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      <saida_0130>-filial       =  wl_change_werks-werks.
      <saida_0130>-centro_custo =  wl_change_werks-kostl.

    ENDLOOP.

    CALL METHOD obj_alv_0130->refresh_table_display
      EXPORTING
        is_stable = wl_stable.

    COMMIT WORK.
    MESSAGE s836(sd) WITH TEXT-s03 DISPLAY LIKE 'S'.

    LEAVE TO SCREEN 0.

  ENDMETHOD.

  METHOD z_converte_moeda.

    CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
      EXPORTING
        client           = sy-mandt
        date             = sy-datum
        foreign_currency = wl_cabecalho_0110-waers "DE EX: BRL
        local_amount     = vlr_doc
        local_currency   = currency "PARA EX: USD
        rate             = 0
        type_of_rate     = 'M'
        read_tcurr       = 'X'
      IMPORTING
        foreign_amount   = vlr_convertido.
  ENDMETHOD.                    "z_converte_moeda
ENDCLASS.                    "Z_GERAR_CONTAS_PAGAR IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_TOOLBAR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_toolbar DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      set_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_TOOLBAR IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_toolbar IMPLEMENTATION.

  METHOD constructor.
    CLEAR obj_toolbar_manager.

    CREATE OBJECT obj_toolbar_manager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD set_toolbar.

    DATA(block) = COND #( WHEN autorizado IS INITIAL THEN abap_true ELSE abap_false ).

    IF v_log_erro IS INITIAL.
      v_log_erro = icon_led_green.
*      v_log_erro = COND #( WHEN gt_msg_return[] IS INITIAL THEN icon_led_green ELSE icon_led_red ).
    ENDIF.

    CLEAR: wl_toolbar.

    CASE screen_item.
      WHEN c_screen_0120.

      WHEN c_screen_0130.
        wl_toolbar-butn_type = 3.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_DELETE_ROW'.
        wl_toolbar-icon         =  icon_delete_row.
        wl_toolbar-quickinfo    = 'Deletar linha'.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-disabled     = block.

*        IF OP_MODO       = C_SEARCH OR
*           OP_MODO       = C_SAVE.
*          WL_TOOLBAR-DISABLED = 1.
*        ENDIF.

        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_INSERT_ROW'.
        wl_toolbar-icon         =  icon_insert_row.
        wl_toolbar-quickinfo    = 'Inserir linha'.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-disabled     = block.

        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_IMPORT_XLS'.
        wl_toolbar-icon         = icon_import.
        wl_toolbar-quickinfo    = 'Importar Excel'.
        wl_toolbar-text         = 'Importar Excel'.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-disabled     = block.

        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

*        append wl_toolbar to e_object->mt_toolbar.
*        clear wl_toolbar.
*
*        wl_toolbar-function     = 'BTN_INSERT_ROW'.
*        wl_toolbar-icon         =  icon_insert_row.
*        wl_toolbar-quickinfo    = 'Inserir linha'.
*        wl_toolbar-butn_type    = 0.
*        wl_toolbar-disabled     = block.
*
*        append wl_toolbar to e_object->mt_toolbar.
*        clear wl_toolbar.

*        wl_toolbar-function     = 'BTN_LOG_ERRO'.
*        wl_toolbar-icon         = icon_import.
*        wl_toolbar-icon         = v_log_erro.
*        wl_toolbar-quickinfo    = 'Log de erros'.
*        wl_toolbar-text         = 'Check Log'.
*        wl_toolbar-butn_type    = 0.
*        wl_toolbar-disabled     = block.
*
*        APPEND wl_toolbar TO e_object->mt_toolbar.
*        CLEAR wl_toolbar.



        wl_toolbar-function     = 'BTN_MODIFY_CENTRO'.
        wl_toolbar-icon         = icon_other_object.
        wl_toolbar-quickinfo    = 'Alterar C.Custo'.
        wl_toolbar-text         = 'Alterar C.Custo'.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-disabled     = block.

        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_LOG_CCUSTO'.
        wl_toolbar-icon         = icon_history.
        wl_toolbar-quickinfo    = 'Histórico C.Custo'.
        wl_toolbar-text         = 'Histórico C.Custo'.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-disabled     = block.

        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

*        IF OP_MODO  = C_SEARCH
*        OR OP_MODO  = C_SAVE.
*          WL_TOOLBAR-DISABLED = '1'.
*        ENDIF.

*        CALL METHOD OBJ_TOOLBAR_MANAGER->REORGANIZE
*          EXPORTING
*            IO_ALV_TOOLBAR = E_OBJECT.
      WHEN c_screen_0132.

        wl_toolbar-function     = 'SEL_ALL'.
        wl_toolbar-icon         = icon_select_all.
        wl_toolbar-quickinfo    = 'Marcar todos'.
        wl_toolbar-text         = 'Marcar todos'.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-disabled     = block.

        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'DESSEL_ALL'.
        wl_toolbar-icon         = icon_select_all.
        wl_toolbar-quickinfo    = 'Desmarcar todos'.
        wl_toolbar-text         = 'Desmarcar todos'.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-disabled     = block.

        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.
    ENDCASE.

    CASE screen_principal.
      WHEN c_screen_0150.
        wl_toolbar-butn_type = 3.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

*       Deleta os botões que foram criados para as outras telas.
*        DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION(3) = 'BTN'.

        wl_toolbar-function     = 'BTN_GERAR_APROPRIACAO'.
        wl_toolbar-icon         = icon_generate.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Gerar Apropriações →'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_ESTORNO_APROPRIACAO'.
        wl_toolbar-icon         = icon_storno.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Estornar Contábil →'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-butn_type = 3.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_REFRESH_0150'.
        wl_toolbar-icon         = icon_refresh.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Atualizar'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.
**<<<------"164255 - NMS - INI------>>>
* Separador de botões.
        wl_toolbar-butn_type = 3.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.
* Botão de Alteração de Data de Apropriação
        wl_toolbar-function     = 'BTN_DTAPROP_MAS_0150'.
        wl_toolbar-icon         = icon_mass_change.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-quickinfo    = 'Altera Dt Apropriação em massa'.
        wl_toolbar-text         = 'Dt. Apropr. Massa'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.
**<<<------"164255 - NMS - FIM------>>>
*        CALL METHOD OBJ_TOOLBAR_MANAGER->REORGANIZE
*          EXPORTING
*            IO_ALV_TOOLBAR = E_OBJECT.


      WHEN c_screen_0160.

*        CLEAR E_OBJECT->MT_TOOLBAR.
        wl_toolbar-butn_type = 3.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_GERAR_CONTAS_PAGAR'.
        wl_toolbar-icon         = icon_generate.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Gerar C.Pagar/C.Receber →'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_ESTORNO_CONTAS_PAGAR'.
        wl_toolbar-icon         = icon_storno.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Estornar C.Pagar/C.Receber →'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-butn_type = 3.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_REFRESH_0160'.
        wl_toolbar-icon         = icon_refresh.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Atualizar'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

      WHEN c_screen_0170.

        wl_toolbar-butn_type = 3.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

*       Deleta os botões que foram criados para as outras telas.
*        DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION(3) = 'BTN'.

        wl_toolbar-function     = 'BTN_GERAR_APROPRIACAO_AJU'.
        wl_toolbar-icon         = icon_generate.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Gerar Ajuste Apropriações →'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_ESTORNO_APROPRIACAO_AJU'.
        wl_toolbar-icon         = icon_storno.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Estornar Contábil →'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-butn_type = 3.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_REFRESH_0170'.
        wl_toolbar-icon         = icon_refresh.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Atualizar'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

      WHEN c_screen_0110.

        wl_toolbar-butn_type = 3.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_COPIA_BANCO'.
        wl_toolbar-icon         = icon_copy_object.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Banco'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

        wl_toolbar-function     = 'BTN_COPIA_FORMA_PGTO'.
        wl_toolbar-icon         = icon_copy_object.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Forma Pgto'.
        wl_toolbar-disabled     = block.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.



    ENDCASE.
  ENDMETHOD.                    "SET_TOOLBAR
  METHOD on_data_changed_finished.

    IF et_good_cells[] IS NOT INITIAL.

    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD get_ucomm.
    DATA: go_utils         TYPE REF TO zutils,
          r_operacao       TYPE REF TO z_tipo_operacao,
          r_seguro_geracao TYPE REF TO z_seguro_geracao,
          v_ans            TYPE n,
          wl_0073          TYPE zglt073,
          vl_nr_item       TYPE zglt073-nr_item,
          vl_doc_delete    TYPE c,
          wl_zglt035       TYPE zglt035,
          it_del_row       TYPE TABLE OF lvc_s_row-index,
          wl_del_row       TYPE lvc_s_row-index,
          vl_budat         TYPE bsis-budat,
          it_val_est       TYPE TABLE OF sval.

    CREATE OBJECT: go_utils, r_operacao, r_seguro_geracao.

    CASE e_ucomm.
      WHEN 'BTN_INSERT_ROW'.

        DATA r_insert_row_alv  TYPE REF TO z_insert_row_alv.
        CREATE OBJECT r_insert_row_alv.

        REFRESH gt_msg_return.

        IF ( header_status = abap_true                 ) "Verifica se o cabeçalho foi preenchido.
        OR ( wl_cabecalho_0110-seq_lcto IS NOT INITIAL ).

*          go_utils->z_validar_info_alv_0130( i_valida_tot = '' ). "Em seguida valida a tela do Bens Assegurados.

          IF ( gt_msg_return IS INITIAL ).

            IF wl_cabecalho_0110-tp_opr = 'B'. "Baixa

              r_seguro_geracao->z_baixar_bem_seg( ).

              "----------------------------------------------------------------------
              "  Ativa necessidade de verificar LOG
              "----------------------------------------------------------------------
              CLEAR v_log_erro.
              IF gt_saida_0130[] IS NOT INITIAL.
                go_utils->z_validar_info_alv_0130( i_valida_tot = '' ). "Em seguida valida a tela do Bens Assegurados.
                IF ( gt_msg_return IS NOT INITIAL ).
                  v_log_erro = icon_led_yellow.
                  REFRESH gt_msg_return.
                ENDIF.
              ENDIF.

              CALL METHOD obj_alv_0130->refresh_table_display
                EXPORTING
                  is_stable = wl_stable.

              RETURN.
            ENDIF.

            IF wl_cabecalho_0110-tp_opr = 'P'. "Prorrogacao

              r_seguro_geracao->z_baixar_bem_seg( ).

              "----------------------------------------------------------------------
              "  Ativa necessidade de verificar LOG
              "----------------------------------------------------------------------
              CLEAR v_log_erro.
              IF gt_saida_0130[] IS NOT INITIAL.
                go_utils->z_validar_info_alv_0130( i_valida_tot = '' ). "Em seguida valida a tela do Bens Assegurados.
                IF ( gt_msg_return IS NOT INITIAL ).
                  v_log_erro = icon_led_yellow.
                  REFRESH gt_msg_return.
                ENDIF.
              ENDIF.

              r_insert_row_alv->insert_row_tela_0130_prorrog( EXPORTING append_table = 'X' ).

              CALL METHOD obj_alv_0130->refresh_table_display
                EXPORTING
                  is_stable = wl_stable.

              RETURN.
            ENDIF.

            r_insert_row_alv->insert_row_tela_0130( EXPORTING append_table = 'X' ).

            "----------------------------------------------------------------------
            "  Ativa necessidade de verificar LOG
            "----------------------------------------------------------------------
            CLEAR v_log_erro.
            IF gt_saida_0130[] IS NOT INITIAL.
              go_utils->z_validar_info_alv_0130( i_valida_tot = '' ). "Em seguida valida a tela do Bens Assegurados.
              IF ( gt_msg_return IS NOT INITIAL ).
                v_log_erro = icon_led_yellow.
                REFRESH gt_msg_return.
              ENDIF.
            ENDIF.

            CALL METHOD obj_alv_0130->refresh_table_display
              EXPORTING
                is_stable = wl_stable.

          ELSE.
            go_utils->z_show_splitter_error( i_show = 'X' ).
          ENDIF.

        ELSE.
          MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
        ENDIF.

      WHEN 'BTN_DELETE_ROW'.

        IF ( header_status = abap_true                 ) "Verifica se o cabeçalho foi preenchido.
        OR ( wl_cabecalho_0110-seq_lcto IS NOT INITIAL ).

          CLEAR: gt_selected_rows, gt_selected_rows[],
                 it_del_row[], wl_del_row.

          CALL METHOD obj_alv_0130->get_selected_rows
            IMPORTING
              et_index_rows = gt_selected_rows.

          LOOP AT gt_selected_rows INTO wl_selected_rows.

            READ TABLE gt_saida_0130 INTO wl_saida_0130 INDEX wl_selected_rows-index.

            IF sy-subrc = 0.

              "Verifica se houve Apropriação para o Bem
              CLEAR: vl_nr_item, wl_0073.

              SELECT SINGLE *
                FROM zglt073 INTO wl_0073
               WHERE seq_lcto  EQ wl_cabecalho_0110-seq_lcto
                 AND nr_item   EQ wl_saida_0130-nr_item.

              IF sy-subrc = 0.
                MESSAGE s836(sd) WITH TEXT-e37 TEXT-e38 DISPLAY LIKE 'E'.
                EXIT.
              ENDIF.

              IF wl_cabecalho_0110-tp_opr EQ 'B'. "Baixa.

                SELECT SINGLE a~* INTO @DATA(_wl_068)
                  FROM zglt068 AS a INNER JOIN zglt050 AS b ON a~seq_lcto = b~seq_lcto
                 WHERE b~seq_lcto     = @wl_cabecalho_0110-ref_seq_lcto
                   AND b~loekz        = ''
                   AND a~nr_item      = @wl_saida_0130-nr_item.

                IF ( sy-subrc = 0 ) AND ( _wl_068-doc_lcto_ajus IS NOT INITIAL ).
                  MESSAGE s836(sd) WITH TEXT-e70 DISPLAY LIKE 'E'.
                  EXIT.
                ENDIF.

                "Verifica se Item referenciado na apolice de Baixa, já teve apropriação no mês da baixa.
                "Caso tenha, não deixar alterar nenhum item da apolice, devido ao valor rateado do
                "Contas a receber em cima do mesmo.
                SELECT SINGLE *
                  FROM zglt073 INTO wl_0073
                 WHERE seq_lcto    = wl_cabecalho_0110-ref_seq_lcto
                   AND nr_item     = wl_saida_0130-nr_item
                   AND month_baixa NE ''. "Apropriação mês baixa

                IF sy-subrc = 0.
                  MESSAGE s836(sd) WITH TEXT-e53  wl_saida_0130-nr_item TEXT-e54 DISPLAY LIKE 'E'.
                  EXIT.
                ENDIF.
              ENDIF.

*              IF ( WL_SAIDA_0130-IMOBILIZADO IS NOT INITIAL ) OR
*                 ( WL_SAIDA_0130-SUBNUMERO   IS NOT INITIAL ) OR
*                 ( WL_SAIDA_0130-MERCADORIA  IS NOT INITIAL ).
*
*                SELECT SINGLE *
*                  FROM ZGLT073 INTO WL_0073
*                 WHERE SEQ_LCTO  EQ WL_CABECALHO_0110-SEQ_LCTO
*                   AND ANLN1     EQ WL_SAIDA_0130-IMOBILIZADO
*                   AND ANLN2     EQ WL_SAIDA_0130-SUBNUMERO
*                   AND MATNR     EQ WL_SAIDA_0130-MERCADORIA.
*
*                IF SY-SUBRC = 0.
*                  MESSAGE S836(SD) WITH TEXT-E37 TEXT-E38 DISPLAY LIKE 'E'.
*                  EXIT.
*                ENDIF.
*
*              ELSEIF WL_SAIDA_0130-DESCR_BENS IS NOT INITIAL.
*
*                SELECT SINGLE *
*                  FROM ZGLT073 INTO WL_0073
*                 WHERE SEQ_LCTO   = WL_CABECALHO_0110-SEQ_LCTO
*                   AND DESCR_BENS = WL_SAIDA_0130-DESCR_BENS
*                   AND ANLN1      = ''
*                   AND ANLN2      = ''
*                   AND MATNR      = ''.
*
*                IF SY-SUBRC = 0.
*                  MESSAGE S836(SD) WITH TEXT-E37 TEXT-E38 DISPLAY LIKE 'E'.
*                  EXIT.
*                ENDIF.
*
*              ENDIF.

            ENDIF.

            wl_del_row = wl_selected_rows-index.
            APPEND wl_del_row TO it_del_row.
          ENDLOOP.

          "Deleta Linha selecionadas
          IF it_del_row[] IS NOT INITIAL.
            SORT it_del_row DESCENDING.
            LOOP AT it_del_row INTO wl_del_row.

              DELETE gt_saida_0130  INDEX wl_del_row.
*          	  Deleta os registros que estiverem com erros na linha excluida.
              DELETE gt_msg_return WHERE tabix = wl_del_row.
            ENDLOOP.
          ENDIF.

          IF ( sy-subrc IS INITIAL ).
            go_utils->z_show_splitter_error( i_show = '' ).

            "----------------------------------------------------------------------
            "  Ativa necessidade de verificar LOG
            "----------------------------------------------------------------------
            CLEAR v_log_erro.
            IF gt_saida_0130[] IS NOT INITIAL.
              go_utils->z_validar_info_alv_0130( i_valida_tot = '' ). "Em seguida valida a tela do Bens Assegurados.
              IF ( gt_msg_return IS NOT INITIAL ).
                v_log_erro = icon_led_yellow.
                REFRESH gt_msg_return.
              ENDIF.
            ENDIF.

            CALL METHOD obj_alv_0130->refresh_table_display
              EXPORTING
                is_stable = wl_stable.
          ENDIF.

        ELSE.
          MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
        ENDIF.

      WHEN 'BTN_GERAR_CONTAS_PAGAR'.
*        CLEAR: GT_MSG_RETURN, WG_MENSAGEM.
*
*        CHECK WL_CABECALHO_0110-SEQ_LCTO IS NOT INITIAL.
*
*        IF ( GT_SAIDA_0120 IS INITIAL ).
*          MESSAGE TEXT-E16 TYPE 'I' DISPLAY LIKE 'W'.
*        ELSE.
*          GO_UTILS->Z_VALIDAR_INFO_ALV_0120( ).
*
*          IF ( GT_MSG_RETURN IS INITIAL ).
*            R_SEGURO_GERACAO->Z_GERAR_PAGAR_RECEBER( ).
*            R_OPERACAO->Z_PESQUISAR_REGISTROS( ).
*            LEAVE TO SCREEN 0100.
*
**            IF ( WL_CABECALHO_0110-SEQ_LCTO IS INITIAL ).
**              R_OPERACAO->Z_SALVAR_REGISTROS( ).
**            ENDIF.
*
*          ELSE.
*            GO_UTILS->Z_SHOW_SPLITTER_ERROR( I_SHOW = 'X' ).
*          ENDIF.
*        ENDIF.

        REFRESH gt_selected_rows.

        CALL METHOD obj_alv_0160->get_selected_rows
          IMPORTING
            et_index_rows = gt_selected_rows.

        DESCRIBE TABLE gt_selected_rows LINES lines.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e20 TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Gerar C.Pagar/Receber'
              text_question         = TEXT-i46
              text_button_1         = 'Sim'
              icon_button_1         = 'ICON_OKAY'
              text_button_2         = 'Não'
              icon_button_2         = 'ICON_CANCEL'
              display_cancel_button = ''
            IMPORTING
              answer                = v_ans.

          CHECK ( v_ans = 1 ).
          CREATE OBJECT r_seguro_geracao.
          r_seguro_geracao->z_gerar_pagar_receber( ).
        ENDIF.

      WHEN 'BTN_ESTORNO_CONTAS_PAGAR'.

        REFRESH gt_selected_rows.

        CALL METHOD obj_alv_0160->get_selected_rows
          IMPORTING
            et_index_rows = gt_selected_rows.

        CHECK gt_selected_rows[] IS NOT INITIAL.

        IF ( lines( gt_selected_rows[] ) > 1 ).
          MESSAGE TEXT-e46 TYPE 'I' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Estorno C.Pagar/C.Receber'
            text_question         = 'Deseja realmente gerar o estorno do(s) registro(s) selecionado(s)?'
            text_button_1         = 'Sim'
            icon_button_1         = 'ICON_OKAY'
            text_button_2         = 'Não'
            icon_button_2         = 'ICON_CANCEL'
            display_cancel_button = ''
          IMPORTING
            answer                = v_ans.

        CHECK v_ans = 1 .

        CREATE OBJECT r_seguro_geracao.
        r_seguro_geracao->z_estorno_pagar_receber( ).

      WHEN 'BTN_GERAR_APROPRIACAO'.
        REFRESH gt_selectedcell.


        CALL METHOD obj_alv_0150->get_selected_rows
          IMPORTING
            et_index_rows = gt_selected_rows.

        DESCRIBE TABLE gt_selected_rows LINES lines.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e20 TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Gerar Apropriação'
              text_question         = TEXT-i04
              text_button_1         = 'Sim'
              icon_button_1         = 'ICON_OKAY'
              text_button_2         = 'Não'
              icon_button_2         = 'ICON_CANCEL'
              display_cancel_button = ''
            IMPORTING
              answer                = v_ans.

          CHECK ( v_ans = 1 ).

          CREATE OBJECT r_seguro_geracao.
          r_seguro_geracao->z_gerar_apropriacoes( ).
        ENDIF.

      WHEN 'BTN_GERAR_APROPRIACAO_AJU'.
        REFRESH gt_selectedcell.

        CALL METHOD obj_alv_0170->get_selected_rows
          IMPORTING
            et_index_rows = gt_selected_rows.

        DESCRIBE TABLE gt_selected_rows LINES lines.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e20 TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Gerar Ajuste Apropriação'
              text_question         = TEXT-i04
              text_button_1         = 'Sim'
              icon_button_1         = 'ICON_OKAY'
              text_button_2         = 'Não'
              icon_button_2         = 'ICON_CANCEL'
              display_cancel_button = ''
            IMPORTING
              answer                = v_ans.

          CHECK ( v_ans = 1 ).
          CREATE OBJECT r_seguro_geracao.
          r_seguro_geracao->z_gerar_ajuste_aprop( ).
        ENDIF.

      WHEN 'BTN_ESTORNO_APROPRIACAO'.
        REFRESH gt_selected_rows.

        CALL METHOD obj_alv_0150->get_selected_rows
          IMPORTING
            et_index_rows = gt_selected_rows.

        CHECK gt_selected_rows[] IS NOT INITIAL.

*        IF ( LINES( GT_SELECTED_ROWS[] ) > 1 ).
*          MESSAGE TEXT-E46 TYPE 'I' DISPLAY LIKE 'E'.
*          RETURN.
*        ENDIF.

        IF ( lines( gt_selected_rows[] ) = 0 ).
          MESSAGE 'Selecione uma linha!' TYPE 'I' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Estorno Apropriação'
            text_question         = 'Deseja realmente gerar o estorno do(s) registro(s) selecionado(s)?'
            text_button_1         = 'Sim'
            icon_button_1         = 'ICON_OKAY'
            text_button_2         = 'Não'
            icon_button_2         = 'ICON_CANCEL'
            display_cancel_button = ''
          IMPORTING
            answer                = v_ans.

        CHECK v_ans = 1 .

        APPEND INITIAL LINE TO it_val_est ASSIGNING FIELD-SYMBOL(<fs_val_est>).
        <fs_val_est>-tabname = 'BSIS'.
        <fs_val_est>-fieldname = 'BUDAT'.

        CALL FUNCTION 'POPUP_GET_VALUES'
          EXPORTING
            popup_title     = 'Data estorno:'
            "start_column    = '5'
            "start_row       = '5'
          TABLES
            fields          = it_val_est
          EXCEPTIONS
            error_in_fields = 1
            OTHERS          = 2.
        IF sy-subrc = 0.
          TRY.
              vl_budat = CONV #( it_val_est[ 1 ]-value ).
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ENDIF.

        CREATE OBJECT r_seguro_geracao.
        r_seguro_geracao->z_estorno_apropriacoes( vl_budat = vl_budat ).

      WHEN 'BTN_ESTORNO_APROPRIACAO_AJU'.
        REFRESH gt_selected_rows.

        CALL METHOD obj_alv_0170->get_selected_rows
          IMPORTING
            et_index_rows = gt_selected_rows.

        CHECK gt_selected_rows[] IS NOT INITIAL.

        IF ( lines( gt_selected_rows[] ) > 1 ).
          MESSAGE TEXT-e46 TYPE 'I' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Estorno Apropriação'
            text_question         = 'Deseja realmente gerar o estorno do(s) registro(s) selecionado(s)?'
            text_button_1         = 'Sim'
            icon_button_1         = 'ICON_OKAY'
            text_button_2         = 'Não'
            icon_button_2         = 'ICON_CANCEL'
            display_cancel_button = ''
          IMPORTING
            answer                = v_ans.

        CHECK v_ans = 1 .

        CREATE OBJECT r_seguro_geracao.
        r_seguro_geracao->z_estorno_ajuste_aprop( ).

      WHEN 'BTN_REFRESH_0160'.
        LOOP AT gt_saida_0160 INTO wl_saida_0160.
          at_index = sy-tabix.

          "Verifica se Doc. Lcto foi estornado.
          IF ( wl_saida_0160-bukrs         IS NOT INITIAL ) AND
             ( wl_saida_0160-lote          IS NOT INITIAL ) AND
             ( wl_saida_0160-doc_contabil  IS INITIAL     ).

            CLEAR: wl_zglt035, vl_doc_delete.
            SELECT SINGLE *
              FROM zglt035
              INTO wl_zglt035
             WHERE bukrs    = wl_saida_0160-bukrs
               AND doc_lcto = wl_saida_0160-nro_documento.

            IF ( ( sy-subrc EQ 0 ) AND ( wl_zglt035-loekz EQ 'X' ) ) OR (  sy-subrc NE 0 ).
              vl_doc_delete = 'X'.
            ENDIF.

            IF wl_saida_0160-nro_documento IS INITIAL.
              vl_doc_delete = 'X'.
            ENDIF.

            IF vl_doc_delete IS NOT INITIAL.
              UPDATE zglt067 SET lote        = space
                                 doc_lcto    = space
                                 dt_lcto_ctb = space
                           WHERE seq_lcto = wl_saida_0160-seq_lcto
                             AND nro_parc = wl_saida_0160-nro_parc.

              IF sy-subrc = 0.
                wl_saida_0160-status        = icon_red_light.
                wl_saida_0160-nro_documento = space.
                wl_saida_0160-lote          = space.
                wl_saida_0160-doc_contabil  = space.

                MODIFY gt_saida_0160 FROM wl_saida_0160 INDEX at_index
                TRANSPORTING status nro_documento lote doc_contabil.

                CONTINUE.
              ENDIF.
            ENDIF.

          ENDIF.

          IF ( wl_saida_0160-lote IS NOT INITIAL ) AND
             ( wl_saida_0160-doc_contabil IS INITIAL ).

            SELECT SINGLE *
              FROM zglt034
              INTO wl_zglt034
             WHERE bukrs = wl_saida_0160-bukrs
               AND lote  = wl_saida_0160-lote.

*           Verifica se o lote foi aprovado para buscar o doc contábil (belnr).
            " CHECK ( WL_ZGLT034-STATUS_LOTE = 'A' ).
            go_utils->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_saida_0160-nro_documento
                                                      i_ano_lcto = wl_zglt034-data_atual(4)
                                            IMPORTING e_zibchv   = wl_zib_chave
                                                      e_ziberr   = wl_zib_erro ).

            IF ( wl_zib_chave IS NOT INITIAL ).
              wl_saida_0160-status       = icon_green_light .
              wl_saida_0160-doc_contabil = wl_zib_chave-belnr.
            ELSEIF ( wl_zib_erro IS NOT INITIAL ).
              wl_saida_0160-status       = icon_red_light.
            ENDIF.

            MODIFY gt_saida_0160 FROM wl_saida_0160 INDEX at_index
            TRANSPORTING status doc_contabil.

          ENDIF.

        ENDLOOP.

        CALL METHOD obj_alv_0160->refresh_table_display
          EXPORTING
            is_stable = wl_stable.

*        IF ( SY-SUBRC IS INITIAL ).
*          MESSAGE S836(SD) WITH TEXT-I05 DISPLAY LIKE 'S'.
*
*          CALL METHOD OBJ_ALV_0120->REFRESH_TABLE_DISPLAY
*            EXPORTING
*              IS_STABLE = WL_STABLE.
*        ELSE.
*          MESSAGE S836(SD) WITH TEXT-E21 DISPLAY LIKE 'E'.
*        ENDIF.

      WHEN 'BTN_REFRESH_0150'.
        LOOP AT gt_saida_0150 INTO wl_saida_0150.
          at_index = sy-tabix.

          "Verifica se Doc. Lcto foi estornado.
          IF ( wl_saida_0150-doc_lcto IS NOT INITIAL ) AND
             ( wl_saida_0150-doc_contabil IS INITIAL ).

            CLEAR: wl_zglt035, vl_doc_delete.
            SELECT SINGLE *
              FROM zglt035
              INTO wl_zglt035
             WHERE bukrs    = wl_saida_0150-bukrs
               AND doc_lcto = wl_saida_0150-doc_lcto.

            IF ( ( sy-subrc EQ 0 ) AND ( wl_zglt035-loekz EQ 'X' ) ) OR (  sy-subrc NE 0 ).
              vl_doc_delete = 'X'.
            ENDIF.

            IF vl_doc_delete IS NOT INITIAL.
              DELETE FROM zglt073 WHERE seq_lcto = wl_saida_0150-seq_lcto
                                    AND nro_parc = wl_saida_0150-nro_parc
                                    AND doc_lcto = wl_saida_0150-doc_lcto.
              IF sy-subrc = 0.
                wl_saida_0150-status        = icon_red_light.
                wl_saida_0150-doc_lcto      = space.
                wl_saida_0150-lote          = space.
                wl_saida_0150-doc_contabil  = space.

                MODIFY gt_saida_0150 FROM wl_saida_0150 INDEX at_index
                TRANSPORTING status doc_lcto lote doc_contabil.

                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.

          IF ( wl_saida_0150-lote IS NOT INITIAL ) AND
             ( wl_saida_0150-doc_contabil IS INITIAL ).

            SELECT SINGLE *
              FROM zglt034
              INTO wl_zglt034
             WHERE bukrs = wl_saida_0150-bukrs
               AND lote  = wl_saida_0150-lote.

*           Verifica se o lote foi aprovado para buscar o doc contábil (belnr).
            "CHECK ( WL_ZGLT034-STATUS_LOTE = 'A' ).
            IF wl_saida_0150-dt_apropr IS NOT INITIAL.
              go_utils->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_saida_0150-doc_lcto
                                                        i_ano_lcto = wl_saida_0150-dt_apropr(4) "WL_ZGLT034-DATA_ATUAL(4)
                                              IMPORTING e_zibchv   = wl_zib_chave
                                                        e_ziberr   = wl_zib_erro ).
            ELSE.
              go_utils->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_saida_0150-doc_lcto
                                                        i_ano_lcto = wl_zglt034-data_atual(4)
                                              IMPORTING e_zibchv   = wl_zib_chave
                                                        e_ziberr   = wl_zib_erro ).

            ENDIF.

            IF ( wl_zib_chave IS NOT INITIAL ).
              wl_saida_0150-status       = icon_green_light .
              wl_saida_0150-doc_contabil = wl_zib_chave-belnr.
            ELSEIF ( wl_zib_erro IS NOT INITIAL ).
              wl_saida_0150-status       = icon_red_light.
            ENDIF.

            MODIFY gt_saida_0150 FROM wl_saida_0150 INDEX at_index
            TRANSPORTING status doc_lcto doc_contabil.
          ENDIF.
        ENDLOOP.

        CALL METHOD obj_alv_0150->refresh_table_display
          EXPORTING
            is_stable = wl_stable.

*        IF ( SY-SUBRC IS INITIAL ).
*          MESSAGE S836(SD) WITH TEXT-I05 DISPLAY LIKE 'S'.
*
*          CALL METHOD OBJ_ALV_0150->REFRESH_TABLE_DISPLAY
*            EXPORTING
*              IS_STABLE = WL_STABLE.
*        ELSE.
*          MESSAGE S836(SD) WITH TEXT-E21 DISPLAY LIKE 'E'.
*        ENDIF.

      WHEN 'BTN_REFRESH_0170'.
        LOOP AT gt_saida_0170 INTO wl_saida_0170.
          at_index = sy-tabix.

          "Verifica se Doc. Lcto foi estornado.
          IF ( wl_saida_0170-doc_lcto_ajus IS NOT INITIAL ) AND
             ( wl_saida_0170-doc_contabil  IS INITIAL     ).

            CLEAR: wl_zglt035, vl_doc_delete.
            SELECT SINGLE *
              FROM zglt035
              INTO wl_zglt035
             WHERE bukrs    = wl_saida_0170-bukrs
               AND doc_lcto = wl_saida_0170-doc_lcto_ajus.

            IF ( ( sy-subrc EQ 0 ) AND ( wl_zglt035-loekz EQ 'X' ) ) OR (  sy-subrc NE 0 ).
              vl_doc_delete = 'X'.
            ENDIF.

            IF vl_doc_delete IS NOT INITIAL.
              UPDATE zglt068 SET lote_ajus        = space
                                 doc_lcto_ajus    = space
                                 dt_lcto_ctb_ajus = space
                           WHERE seq_lcto = wl_saida_0170-seq_lcto
                             AND nr_item  = wl_saida_0170-nr_item.
              IF sy-subrc = 0.
                wl_saida_0170-status        = icon_red_light.
                wl_saida_0170-doc_lcto_ajus = space.
                wl_saida_0170-lote_ajus     = space.
                wl_saida_0170-doc_contabil  = space.

                MODIFY gt_saida_0170 FROM wl_saida_0170 INDEX at_index
                TRANSPORTING status doc_lcto_ajus lote_ajus doc_contabil.

                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.

          IF ( wl_saida_0170-lote_ajus    IS NOT INITIAL ) AND
             ( wl_saida_0170-doc_contabil IS INITIAL     ).

            SELECT SINGLE *
              FROM zglt034
              INTO wl_zglt034
             WHERE bukrs = wl_saida_0170-bukrs
               AND lote  = wl_saida_0170-lote_ajus.

            go_utils->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_saida_0170-doc_lcto_ajus
                                                      i_ano_lcto = wl_zglt034-data_atual(4)
                                            IMPORTING e_zibchv   = wl_zib_chave
                                                      e_ziberr   = wl_zib_erro ).

            IF ( wl_zib_chave IS NOT INITIAL ).
              wl_saida_0170-status       = icon_green_light.
              wl_saida_0170-doc_contabil = wl_zib_chave-belnr.
            ELSEIF ( wl_zib_erro IS NOT INITIAL ).
              wl_saida_0170-status       = icon_red_light.
            ENDIF.

            MODIFY gt_saida_0170 FROM wl_saida_0170 INDEX at_index
            TRANSPORTING status doc_lcto_ajus doc_contabil.
          ENDIF.
        ENDLOOP.

        CALL METHOD obj_alv_0170->refresh_table_display
          EXPORTING
            is_stable = wl_stable.


      WHEN 'BTN_IMPORT_XLS'.

        CHECK ( wl_cabecalho_0110-tp_opr IS NOT INITIAL ) AND ( wl_cabecalho_0110-bukrs IS NOT INITIAL ).

        IF wl_cabecalho_0110-tp_opr EQ 'B'. "Baixa
          MESSAGE s836(sd) WITH TEXT-e41 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        CALL SCREEN 0131 STARTING AT 02 02 ENDING AT 83 04.
        IF gt_msg_return[] IS NOT INITIAL.
          go_utils->z_show_splitter_error( i_show = 'X' ).
        ENDIF.
      WHEN 'BTN_MODIFY_CENTRO'.

        IF ( wl_cabecalho_0110-seq_lcto IS INITIAL ).
          ROLLBACK WORK.
          MESSAGE s836(sd) WITH TEXT-e44 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF wl_cabecalho_0110-tp_opr EQ 'B'. "Baixa
          MESSAGE s836(sd) WITH TEXT-e41 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        CLEAR: wl_change_werks,
               gt_selected_rows,
               gt_selected_rows[],
               gt_msg_return,
               gt_msg_return[],
               lt_nodes_select.

        CALL METHOD obj_alv_0130->get_selected_rows
          IMPORTING
            et_index_rows = gt_selected_rows.

        CHECK gt_selected_rows[] IS NOT INITIAL.

        LOOP AT gt_selected_rows INTO wl_selected_rows.
          at_index = sy-tabix.

          CLEAR: wl_saida_0130.
          READ TABLE: gt_saida_0130 INTO wl_saida_0130 INDEX wl_selected_rows-index.

          IF ( wl_saida_0130-nr_item IS INITIAL ).
            ROLLBACK WORK.
            MESSAGE s836(sd) WITH TEXT-e44 DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          IF ( wl_saida_0130-st_baixa IS NOT INITIAL ).
            go_utils->z_criar_mensagem_erro( msg_type = ''
                                             text1    = TEXT-e49
                                             text2    = ''
                                             field    = ''
                                             index    = at_index
                                             aba      = '0130' ).
          ENDIF.

          IF wl_saida_0130-filial IS INITIAL.
            go_utils->z_criar_mensagem_erro( msg_type = ''
                                             text1    = TEXT-e01
                                             text2    = 'Filial'
                                             field    = 'FILIAL'
                                             index    = at_index
                                             aba      = '0130' ).
          ENDIF.

          IF wl_saida_0130-centro_custo IS INITIAL.
            go_utils->z_criar_mensagem_erro( msg_type = ''
                                             text1    = TEXT-e01
                                             text2    = 'Centro Custo'
                                             field    = 'CENTRO_CUSTO'
                                             index    = at_index
                                             aba      = '0130' ).
          ENDIF.

        ENDLOOP.

        IF ( gt_msg_return[] IS NOT INITIAL ).
          l_active_node = 3.
          APPEND l_active_node TO lt_nodes_select.
          go_utils->z_show_splitter_error( i_show = 'X' ).
          RETURN.
        ENDIF.

        CALL SCREEN 0133 STARTING AT 25 10 ENDING AT 60 03.

      WHEN 'BTN_LOG_CCUSTO'.

        CLEAR: gt_selected_rows, gt_selected_rows[].

        CALL METHOD obj_alv_0130->get_selected_rows
          IMPORTING
            et_index_rows = gt_selected_rows.

        CHECK gt_selected_rows[] IS NOT INITIAL.

        IF lines( gt_selected_rows[] ) > 1.
          MESSAGE s836(sd) WITH TEXT-e46 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        READ TABLE gt_selected_rows INTO wl_selected_rows INDEX 1.
        READ TABLE: gt_saida_0130 INTO wl_saida_0130 INDEX wl_selected_rows-index.

        CHECK sy-subrc = 0.

        SELECT *
          FROM zglt079
          INTO TABLE gt_zglt079
         WHERE seq_lcto = wl_cabecalho_0110-seq_lcto
           AND nr_item  = wl_saida_0130-nr_item.

        IF gt_zglt079[] IS NOT INITIAL.

          SORT gt_zglt079 BY dt_atual hr_atual.

          PERFORM montar_layout_log_ccusto.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              it_fieldcat           = it_estrutura[]
              i_save                = 'A'
              i_screen_start_column = 3
              i_screen_start_line   = 3
              i_screen_end_column   = 100
              i_screen_end_line     = 13
            TABLES
              t_outtab              = gt_zglt079.
        ENDIF.

      WHEN 'BTN_COPIA_BANCO'.
        DATA: wl_copia_120 LIKE LINE OF gt_saida_0120.

        CHECK ( gt_saida_0120[] IS NOT INITIAL ).

        READ TABLE gt_saida_0120 INTO wl_saida_0120 INDEX 1.
        wl_copia_120-bco_empresa = wl_saida_0120-bco_empresa.
        CLEAR: wl_saida_0120.

        LOOP AT gt_saida_0120 INTO wl_saida_0120.
          wl_saida_0120-bco_empresa = wl_copia_120-bco_empresa.
          MODIFY gt_saida_0120 FROM wl_saida_0120.
          CLEAR: wl_saida_0120.
        ENDLOOP.

        CALL METHOD obj_alv_0120->refresh_table_display
          EXPORTING
            is_stable = wl_stable.

      WHEN 'BTN_COPIA_FORMA_PGTO'.
*        DATA: WL_COPIA_120 LIKE LINE OF GT_SAIDA_0120.

        CHECK ( gt_saida_0120[] IS NOT INITIAL ).

        READ TABLE gt_saida_0120 INTO wl_saida_0120 INDEX 1.
        wl_copia_120-forma_pgto = wl_saida_0120-forma_pgto.
        CLEAR: wl_saida_0120.

        LOOP AT gt_saida_0120 INTO wl_saida_0120.
          wl_saida_0120-forma_pgto = wl_copia_120-forma_pgto.
          MODIFY gt_saida_0120 FROM wl_saida_0120.
          CLEAR: wl_saida_0120.
        ENDLOOP.

        CALL METHOD obj_alv_0120->refresh_table_display
          EXPORTING
            is_stable = wl_stable.

      WHEN 'BTN_LOG_ERRO'.

        REFRESH: gt_msg_return. CLEAR v_log_erro.
        go_utils->z_validar_info_alv_0130( i_valida_tot = '' ). "Em seguida valida a tela do Bens Assegurados.

        IF ( gt_msg_return IS NOT INITIAL ).
          v_log_erro = icon_led_red.
          go_utils->z_show_splitter_error( i_show = 'X' ).

          CALL METHOD obj_alv_0130->refresh_table_display
            EXPORTING
              is_stable = wl_stable.

        ENDIF.

      WHEN 'SEL_ALL'.

        CALL METHOD obj_alv_0132->get_selected_rows
          IMPORTING
            et_index_rows = gt_selected_rows.


        LOOP AT gt_selected_rows ASSIGNING FIELD-SYMBOL(<wl_selected_rows>).
          READ TABLE gt_saida_0132 ASSIGNING FIELD-SYMBOL(<wa_saida_0132>) INDEX <wl_selected_rows>-index.
          <wa_saida_0132>-check = abap_true.
        ENDLOOP.

        CALL METHOD obj_alv_0132->refresh_table_display
          EXPORTING
            is_stable = wl_stable.
      WHEN 'DESSEL_ALL'.
        FREE: gt_selected_rows.
        CALL METHOD obj_alv_0132->get_selected_rows
          IMPORTING
            et_index_rows = gt_selected_rows.
        LOOP AT gt_selected_rows ASSIGNING <wl_selected_rows>.
          READ TABLE gt_saida_0132 ASSIGNING <wa_saida_0132> INDEX <wl_selected_rows>-index.
          <wa_saida_0132>-check  = abap_false.
        ENDLOOP.
        CALL METHOD obj_alv_0132->refresh_table_display
          EXPORTING
            is_stable = wl_stable.
**<<<------"164255 - NMS - INI------>>>
      WHEN 'BTN_DTAPROP_MAS_0150'. "Altera Data de Apropriação em massa
* Altera Data de Apropriação em massa.
        PERFORM zf_muda_data_aprop_massa.
**<<<------"164255 - NMS - FIM------>>>
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "GET_UCOMM
ENDCLASS.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION


*&---------------------------------------------------------------------*
*&  Include           Z_LCL_EVENT_HANDLER_DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_click FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      handle_double_click FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING node_key.

*    METHODS HANDLE_NODE_CTMENU_SELECTED
*      FOR EVENT NODE_CONTEXT_MENU_SELECTED OF CL_GUI_ALV_TREE
*        IMPORTING NODE_KEY
*                  FCODE.

*    METHODS HANDLE_ITEM_CTMENU_REQUEST
*      FOR EVENT ITEM_CONTEXT_MENU_REQUEST OF CL_GUI_ALV_TREE
*        IMPORTING NODE_KEY
*                  FIELDNAME
*                  MENU.

*    METHODS HANDLE_ITEM_CTMENU_SELECTED
*      FOR EVENT ITEM_CONTEXT_MENU_SELECTED OF CL_GUI_ALV_TREE
*        IMPORTING NODE_KEY
*                  FIELDNAME
*                  FCODE.

*    METHODS HANDLE_ITEM_DOUBLE_CLICK
*      FOR EVENT ITEM_DOUBLE_CLICK OF CL_GUI_ALV_TREE
*      IMPORTING NODE_KEY
*                FIELDNAME.

    CLASS-METHODS:
      top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING e_dyndoc_id.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CONSTANTS:
      tree_pai_01   TYPE n VALUE 1,
      tree_filho_01 TYPE n VALUE 2,
      tree_filho_02 TYPE n VALUE 3,
      tree_filho_03 TYPE n VALUE 5,
      tree_filho_04 TYPE n VALUE 6,
      tree_filho_05 TYPE n VALUE 7.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_click.

    DATA: vl_nr_lote  TYPE zglt034-lote,
          lt_zib_erro TYPE TABLE OF zib_contabil_err,
          lw_zib_erro TYPE zib_contabil_err.

    CASE screen_item.
      WHEN c_screen_0120.

        READ TABLE gt_saida_0120 INTO wl_saida_0120 INDEX e_row_id-index.

        CASE e_column_id.
          WHEN 'LOTE'.
            CHECK ( wl_saida_0120-lote IS NOT INITIAL ).

            SET PARAMETER ID 'LOT' FIELD wl_saida_0120-lote.
            CALL TRANSACTION 'ZGL017' AND SKIP FIRST SCREEN.

          WHEN 'NRO_DOCUMENTO'.
            CHECK ( wl_saida_0120-nro_documento IS NOT INITIAL ).

            SET PARAMETER ID 'BLN' FIELD wl_saida_0120-nro_documento.
            SET PARAMETER ID 'LOT' FIELD vl_nr_lote.
            CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.

          WHEN 'DOC_CONTABIL'.
            CHECK ( wl_saida_0120-doc_contabil IS NOT INITIAL ).

            SET PARAMETER ID 'BLN' FIELD wl_saida_0120-doc_contabil.
            SET PARAMETER ID 'BUK' FIELD wl_saida_0120-bukrs.
            SET PARAMETER ID 'GJR' FIELD wl_saida_0120-dt_lcto_ctb(4).

            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

          WHEN 'STATUS'.

            CHECK ( wl_saida_0120-status = icon_red_light ).
            CLEAR gt_saida_0200.

            go_utils->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_saida_0120-nro_documento
                                                      i_ano_lcto = wl_zglt034-data_atual(4)
                                            IMPORTING e_zibchv   = wl_zib_chave
                                                      e_ziberr   = wl_zib_erro ).

            SELECT *
              FROM zib_contabil_err
              INTO TABLE lt_zib_erro
             WHERE obj_key = wl_zib_erro-obj_key.

            LOOP AT lt_zib_erro INTO lw_zib_erro.
              wl_saida_0200-status   = icon_led_red.
              wl_saida_0200-msg_erro = lw_zib_erro-message.
              APPEND wl_saida_0200 TO gt_saida_0200.
            ENDLOOP.

            CALL SCREEN 0200 STARTING AT 5  10
                               ENDING AT 78 14.
          WHEN OTHERS.
        ENDCASE.

    ENDCASE.

    CASE screen_principal.

      WHEN c_screen_0150.

        DATA: vl_lote TYPE zglt034-lote.

        READ TABLE gt_saida_0150 INTO wl_saida_0150 INDEX e_row_id-index.

        CASE e_column_id.
          WHEN 'LOTE'.
            CHECK ( wl_saida_0150-lote IS NOT INITIAL ).

            SET PARAMETER ID 'LOT' FIELD wl_saida_0150-lote.
            CALL TRANSACTION 'ZGL017' AND SKIP FIRST SCREEN.
          WHEN 'DOC_LCTO'.
            CHECK ( wl_saida_0150-doc_lcto IS NOT INITIAL ).

            SET PARAMETER ID 'BLN' FIELD wl_saida_0150-doc_lcto.
            SET PARAMETER ID 'LOT' FIELD vl_lote.
            CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.

          WHEN 'STATUS'.

            CHECK ( wl_saida_0150-status = icon_red_light ).

            IF ( wl_saida_0150-lote IS NOT INITIAL ) AND
                   ( wl_saida_0150-doc_contabil IS INITIAL ).

              SELECT SINGLE *
                FROM zglt034
                INTO wl_zglt034
               WHERE bukrs = wl_saida_0150-bukrs
                 AND lote  = wl_saida_0150-lote.

              IF sy-subrc IS INITIAL.

                DATA v_objkey    TYPE char20.
                CONCATENATE 'ZGL17' wl_saida_0150-doc_lcto wl_saida_0150-dt_apropr(4) INTO v_objkey.
                PERFORM zf_mostrar_log_erro USING v_objkey.

              ENDIF.

            ENDIF.

          WHEN 'DOC_CONTABIL'.

            CHECK ( wl_saida_0150-doc_contabil IS NOT INITIAL ).

            SET PARAMETER ID 'BLN' FIELD wl_saida_0150-doc_contabil.         "w_transaction-belnr.
            SET PARAMETER ID 'BUK' FIELD wl_saida_0150-bukrs.                "w_transaction-bukrs.
            SET PARAMETER ID 'GJR' FIELD wl_cabecalho_0150-competencia+2(4). "w_transaction-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

        ENDCASE.

      WHEN c_screen_0160.

        READ TABLE gt_saida_0160 INTO wl_saida_0160 INDEX e_row_id-index.

        CASE e_column_id.
          WHEN 'LOTE'.
            CHECK ( wl_saida_0160-lote IS NOT INITIAL ).

            SET PARAMETER ID 'LOT' FIELD wl_saida_0160-lote.
            CALL TRANSACTION 'ZGL017' AND SKIP FIRST SCREEN.

          WHEN 'NRO_DOCUMENTO'.
            CHECK ( wl_saida_0160-nro_documento IS NOT INITIAL ).

            SET PARAMETER ID 'BLN' FIELD wl_saida_0160-nro_documento.
            SET PARAMETER ID 'LOT' FIELD vl_nr_lote.
            CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.

          WHEN 'DOC_CONTABIL'.
            CHECK ( wl_saida_0160-doc_contabil IS NOT INITIAL ).

            SET PARAMETER ID 'BLN' FIELD wl_saida_0160-doc_contabil.
            SET PARAMETER ID 'BUK' FIELD wl_saida_0160-bukrs.
            SET PARAMETER ID 'GJR' FIELD wl_saida_0160-dt_lcto_ctb(4).

            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

          WHEN 'STATUS'.

            CHECK ( wl_saida_0160-status = icon_red_light ).
            CLEAR gt_saida_0200.

            go_utils->z_retorna_status_zib( EXPORTING i_doc_lcto = wl_saida_0160-nro_documento
                                                      i_ano_lcto = wl_zglt034-data_atual(4)
                                            IMPORTING e_zibchv   = wl_zib_chave
                                                      e_ziberr   = wl_zib_erro ).

            SELECT *
              FROM zib_contabil_err
              INTO TABLE lt_zib_erro
             WHERE obj_key = wl_zib_erro-obj_key.

            LOOP AT lt_zib_erro INTO lw_zib_erro.
              wl_saida_0200-status   = icon_led_red.
              wl_saida_0200-msg_erro = lw_zib_erro-message.
              APPEND wl_saida_0200 TO gt_saida_0200.
            ENDLOOP.

            CALL SCREEN 0200 STARTING AT 5  10
                               ENDING AT 78 14.
          WHEN OTHERS.
        ENDCASE.

      WHEN c_screen_0170.

        READ TABLE gt_saida_0170 INTO wl_saida_0170 INDEX e_row_id-index.

        CASE e_column_id.
          WHEN 'LOTE_AJUS'.
            CHECK ( wl_saida_0170-lote_ajus IS NOT INITIAL ).

            SET PARAMETER ID 'LOT' FIELD wl_saida_0170-lote_ajus.
            CALL TRANSACTION 'ZGL017' AND SKIP FIRST SCREEN.
          WHEN 'DOC_LCTO_AJUS'.
            CHECK ( wl_saida_0170-doc_lcto_ajus IS NOT INITIAL ).

            SET PARAMETER ID 'BLN' FIELD wl_saida_0170-doc_lcto_ajus.
            SET PARAMETER ID 'LOT' FIELD vl_lote.
            CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.

          WHEN 'DOC_CONTABIL'.

            CHECK ( wl_saida_0170-doc_contabil IS NOT INITIAL ).

            SET PARAMETER ID 'BLN' FIELD wl_saida_0170-doc_contabil.
            SET PARAMETER ID 'BUK' FIELD wl_saida_0170-bukrs.
            SET PARAMETER ID 'GJR' FIELD wl_saida_0170-dt_lcto_ctb_ajus(4).

            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDCASE.

    ENDCASE.

  ENDMETHOD.                    "ON_CLICK

  METHOD  handle_double_click.

*    IF ( GT_MSG_RETURN IS INITIAL ).

    IF v_log_erro = icon_led_red OR v_log_erro = icon_led_yellow.
*      node_key = tree_filho_02.
      MESSAGE s000(z_les) WITH 'Verif. itens assegurados!'.
      RETURN.
    ELSE.

      CASE node_key.
        WHEN tree_pai_01.
          screen_principal = c_screen_0140.
        WHEN tree_filho_01.
          screen_principal = c_screen_0110.
          screen_item      = c_screen_0120.
        WHEN tree_filho_02.
          screen_principal = c_screen_0110.
          screen_item      = c_screen_0130.
        WHEN tree_filho_03.
          screen_principal = c_screen_0150.
          screen_item      = space.
        WHEN tree_filho_04.
          screen_principal = c_screen_0160.
          screen_item      = space.
        WHEN tree_filho_05.
          screen_principal = c_screen_0170.
          screen_item      = space.
        WHEN OTHERS.
      ENDCASE.

*    ELSE.
*      MESSAGE S836(SD) WITH TEXT-E06 DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

*  METHOD HANDLE_ITEM_CTMENU_SELECTED.
*    BREAK-POINT.
*  ENDMETHOD.                    "HANDLE_ITEM_CTMENU_SELECTED

*  METHOD HANDLE_ITEM_DOUBLE_CLICK.
*    BREAK-POINT.
*  ENDMETHOD.                    "HANDLE_ITEM_DOUBLE_CLICK

*  METHOD HANDLE_NODE_CTMENU_SELECTED.
*    BREAK-POINT.
*  ENDMETHOD.                    "HANDLE_NODE_CTMENU_SELECTED


  METHOD on_data_changed.

    DATA: v_leave_screen    LIKE abap_true,
          v_vlr_premio      TYPE dmbtr,
          wl_saida_0120_aux TYPE ty_saida_0120,
          wl_saida_0130_aux TYPE ty_saida_0130,
          lv_value          TYPE lvc_value.

    CREATE OBJECT go_utils.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      DELETE gt_msg_return WHERE field = ls_good-fieldname
                             AND aba   = screen_item
                             AND tabix = ls_good-row_id.
      CASE screen_item.
        WHEN 0120.
          READ TABLE gt_saida_0120 INTO wl_saida_0120 INDEX ls_good-row_id.
          at_index = ls_good-row_id.

          CASE ls_good-fieldname.
*Inicio Alteração - Leandro Valentim Ferreira - 17.04.23 - 108325
            WHEN 'COD_BARRAS'.
              DATA: v_data1      TYPE sy-datum,
                    v_data_base  TYPE sydatum VALUE '19971007',
                    v_data_venc  TYPE sydatum,
                    v_intervalo1 TYPE phk_scrdd.

              CHECK   ls_good-value IS NOT INITIAL.

              v_intervalo1 = ls_good-value+33(4).
* FGM - Após estourar o intervalo 9999 em 21/02/2025 o intervalo reinicia em 1000
              IF v_intervalo1 < 8000.
                v_intervalo1 = v_intervalo1 + 9000.
              ENDIF.
              v_data_venc = v_data_base + v_intervalo1.

              zcl_miro=>get_proximo_dia_util(
                EXPORTING
                  i_data_base        = v_data_venc
                  i_signum           = '+'
                  i_ck_data_zles0145 = abap_true
                RECEIVING
                  r_data             = DATA(v_data_d_util)
                EXCEPTIONS
                  erro               = 1 ).

              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'DT_VENC'
                  i_value     = v_data_d_util.

              wl_saida_0120-dt_venc = v_data_d_util.
              wl_saida_0120-cod_barras = ls_good-value.

              MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX ls_good-row_id
              TRANSPORTING dt_venc cod_barras.
*Fim Alteração - Leandro Valentim Ferreira - 17.04.23 - 108325
            WHEN 'FILIAL'.
              CLEAR wl_j_1bbranch.

              PERFORM f_check_filial USING wl_cabecalho_0110-bukrs
                                           ls_good-value.

              IF ( sy-subrc IS NOT INITIAL ).
                MESSAGE s836(sd) WITH TEXT-e22 DISPLAY LIKE 'E'.
                CLEAR: ls_good-value.
              ENDIF.

              wl_saida_0120-filial = ls_good-value.

              MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX ls_good-row_id
              TRANSPORTING filial.

            WHEN 'DT_VENC'.
              DATA: v_data      TYPE sy-datum,
                    v_intervalo TYPE phk_scrdd.

              v_data = ls_good-value.

              CHECK ls_good-value IS NOT INITIAL.

              IF ls_good-value < sy-datum.
                CLEAR v_data.
                MESSAGE s836(sd) WITH TEXT-e61 DISPLAY LIKE 'E'.
              ENDIF.

              IF v_data IS NOT INITIAL.
                go_utils->checar_dia_util( IMPORTING e_intervalo = v_intervalo
                                           CHANGING  c_data_venc = v_data ).

                IF ( v_intervalo <= 3 ) AND  ( wl_cabecalho_0110-tp_opr NE 'B' ).
                  CLEAR v_data.
                  MESSAGE s836(sd) WITH TEXT-e02 TEXT-e25 DISPLAY LIKE 'E'.
                ENDIF.
              ENDIF.

              wl_saida_0120-dt_venc = v_data.

* Ajusta data de baixa para data de vencimento
              IF wl_cabecalho_0110-tp_opr = 'B'.
                LOOP AT gt_saida_0130 ASSIGNING FIELD-SYMBOL(<fs_saida_0130>).
                  <fs_saida_0130>-dt_baixa = wl_saida_0120-dt_venc.
                ENDLOOP.
              ENDIF.

              MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX ls_good-row_id
        TRANSPORTING dt_venc.

            WHEN 'VLR_PREMIO_USD'.


              CLEAR v_vlr_premio.

              wl_saida_0120-vlr_premio_usd = ls_good-value.

              " IR163960 - Ajuste valor premio BRL moeda USD - BG
              IF ( wl_saida_0120-vlr_premio_usd IS NOT INITIAL ).
                wl_saida_0120-vlr_premio_brl = ( wl_saida_0120-vlr_premio_usd * wl_saida_0120-taxa_cambio ).
              ENDIF.

              MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX ls_good-row_id
              TRANSPORTING vlr_premio_brl vlr_premio_usd.

              LOOP AT gt_saida_0120 INTO wl_saida_0120_aux.
                v_vlr_premio = v_vlr_premio + wl_saida_0120_aux-vlr_premio_usd.
              ENDLOOP.

              IF ( v_vlr_premio > wl_cabecalho_0110-vlr_premio_usd ).
                CLEAR wl_saida_0120-vlr_premio_usd.
                MESSAGE s836(sd) WITH TEXT-e13 TEXT-e23 DISPLAY LIKE 'E'.

                MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX ls_good-row_id
                        TRANSPORTING vlr_premio_brl vlr_premio_usd .
              ENDIF.
              " IR163960 - Ajuste valor premio BRL moeda USD - BG - Fim

            WHEN 'VLR_PREMIO_BRL'.
              CLEAR v_vlr_premio.

              wl_saida_0120-vlr_premio_brl = ls_good-value.

              IF ( wl_saida_0120-vlr_premio_brl IS NOT INITIAL ).
                wl_saida_0120-vlr_premio_usd = ( wl_saida_0120-vlr_premio_brl / wl_saida_0120-taxa_cambio ).
              ENDIF.

              MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX ls_good-row_id
        TRANSPORTING vlr_premio_brl vlr_premio_usd." IR163960 - Ajuste valor premio BRL moeda USD - BG

              LOOP AT gt_saida_0120 INTO wl_saida_0120_aux.
                v_vlr_premio = v_vlr_premio + wl_saida_0120_aux-vlr_premio_brl.
              ENDLOOP.

              IF ( v_vlr_premio > wl_cabecalho_0110-vlr_premio_brl ).
                CLEAR wl_saida_0120-vlr_premio_brl.
                MESSAGE s836(sd) WITH TEXT-e12 TEXT-e24 DISPLAY LIKE 'E'.

                MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX ls_good-row_id
          TRANSPORTING vlr_premio_brl vlr_premio_usd." IR163960 - Ajuste valor premio BRL moeda USD - BG
              ENDIF.

            WHEN OTHERS.
              CONTINUE.
          ENDCASE.

          IF ( gt_msg_return IS NOT INITIAL ).
            go_utils->z_show_splitter_error( i_show = '' ).
          ENDIF.

          CALL METHOD obj_alv_0120->get_current_cell
            IMPORTING
              es_row_id = gv_row
              es_col_id = gv_col.

          CALL METHOD obj_alv_0120->refresh_table_display
            EXPORTING
              is_stable = wl_stable.

*          IF OBJ_ALV_0130 IS NOT INITIAL.
*
*            CALL METHOD OBJ_ALV_0130->REFRESH_TABLE_DISPLAY
*              EXPORTING
*                IS_STABLE = WL_STABLE.
*
*          ENDIF.

          LEAVE TO SCREEN 0100.

        WHEN 0130.

          READ TABLE gt_saida_0130 INTO wl_saida_0130 INDEX ls_good-row_id.
          at_index = ls_good-row_id.

          CASE ls_good-fieldname.
            WHEN 'FILIAL'.
              CLEAR wl_j_1bbranch.

              PERFORM f_check_filial USING wl_cabecalho_0110-bukrs
                                           ls_good-value.

              IF ( sy-subrc IS NOT INITIAL ).
                MESSAGE s836(sd) WITH TEXT-e22 DISPLAY LIKE 'E'.
                CLEAR: ls_good-value.
              ENDIF.

              wl_saida_0130-filial = ls_good-value.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
              TRANSPORTING filial.

            WHEN 'NR_SERIE'.

              PERFORM f_seleciona_dados_imobilizado USING ls_good-value
                                                 CHANGING ls_good-fieldname
                                                          wl_saida_0130-imobilizado
                                                          wl_saida_0130-subnumero
                                                          wl_saida_0130-chassi
                                                          wl_saida_0130-nr_serie
                                                          wl_saida_0130-descr_bens
                                                          wl_saida_0130-centro_custo
                                                          wl_saida_0130-uf
                                                          wl_saida_0130-filial.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
        TRANSPORTING imobilizado subnumero chassi descr_bens centro_custo nr_serie.

            WHEN 'CHASSI'.

              PERFORM f_seleciona_dados_imobilizado USING ls_good-value
                                                 CHANGING ls_good-fieldname
                                                          wl_saida_0130-imobilizado
                                                          wl_saida_0130-subnumero
                                                          wl_saida_0130-chassi
                                                          wl_saida_0130-nr_serie
                                                          wl_saida_0130-descr_bens
                                                          wl_saida_0130-centro_custo
                                                          wl_saida_0130-uf
                                                          wl_saida_0130-filial.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
        TRANSPORTING imobilizado subnumero chassi descr_bens centro_custo nr_serie uf.

            WHEN 'IMOBILIZADO'.

              PERFORM f_seleciona_dados_imobilizado USING ls_good-value
                                                 CHANGING ls_good-fieldname
                                                          wl_saida_0130-imobilizado
                                                          wl_saida_0130-subnumero
                                                          wl_saida_0130-chassi
                                                          wl_saida_0130-nr_serie
                                                          wl_saida_0130-descr_bens
                                                          wl_saida_0130-centro_custo
                                                          wl_saida_0130-uf
                                                          wl_saida_0130-filial.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
        TRANSPORTING imobilizado subnumero chassi descr_bens centro_custo nr_serie uf.

            WHEN 'MERCADORIA'.
              CHECK ( ls_good-value NE space ).

              SELECT SINGLE *
                FROM mara
                INTO wl_mara
               WHERE matnr = ls_good-value.

              IF ( sy-subrc IS NOT INITIAL ).
                MESSAGE s836(sd) WITH TEXT-e10 DISPLAY LIKE 'E'.
              ELSE.
                SELECT SINGLE *
                  FROM makt
                  INTO wl_makt
                 WHERE matnr = ls_good-value.

                wl_saida_0130-mercadoria = wl_mara-matnr.
                wl_saida_0130-descr_bens = wl_makt-maktx.

                MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
          TRANSPORTING mercadoria descr_bens.
              ENDIF.

            WHEN 'AUFNR'.

              SELECT SINGLE * FROM aufk
                INTO @DATA(wl_aufk)
                WHERE bukrs EQ @wl_cabecalho_0110-bukrs
                AND   aufnr EQ @ls_good-value
                AND   gsber EQ @wl_saida_0130-filial.

              IF sy-subrc EQ 0.
                wl_saida_0130-centro_custo =  wl_aufk-kostv.
              ENDIF.

              wl_saida_0130-aufnr = ls_good-value.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
        TRANSPORTING  aufnr centro_custo.

            WHEN 'CENTRO_CUSTO'.
              DATA: wl_csks TYPE csks.

              SELECT SINGLE *
                FROM csks
                INTO wl_csks
               WHERE bukrs = wl_cabecalho_0110-bukrs
                 AND gsber = wl_saida_0130-filial
                 AND kostl = ls_good-value.

              IF ( sy-subrc IS NOT INITIAL ).
                CLEAR: ls_good-value, wl_saida_0130-centro_custo.
                MESSAGE s836(sd) WITH TEXT-e35 TEXT-e36 DISPLAY LIKE 'E'.
              ENDIF.

              "Verifica centro de custo bloqueado para lançamento.
              IF wl_csks-bkzkp IS NOT INITIAL.
                CLEAR: ls_good-value, wl_saida_0130-centro_custo.
                MESSAGE s836(sd) WITH TEXT-e74 ls_good-value TEXT-e75 DISPLAY LIKE 'E' .
              ENDIF.

              wl_saida_0130-centro_custo = ls_good-value.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
               TRANSPORTING centro_custo.

            WHEN 'VLR_PREMIO_USD'.
              CLEAR v_vlr_premio.
              wl_saida_0130-vlr_premio_usd = ls_good-value.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
        TRANSPORTING vlr_premio_usd.

              LOOP AT gt_saida_0130 INTO wl_saida_0130_aux.
                v_vlr_premio = v_vlr_premio + wl_saida_0130_aux-vlr_premio_usd.
              ENDLOOP.

              IF ( v_vlr_premio > wl_cabecalho_0110-vlr_premio_usd ).
                CLEAR wl_saida_0130-vlr_premio_usd.
                MESSAGE s836(sd) WITH TEXT-e13 TEXT-e23 DISPLAY LIKE 'E'.
              ELSE.
                go_utils->z_converter_moeda( EXPORTING moeda                 = wl_cabecalho_0110-waers
                                                       taxa_cambio           = wl_saida_0130-taxa_cambio
                                                       vlr_usd               = wl_saida_0130-vlr_premio_usd
                                                       vlr_brl               = space
                                             IMPORTING vlr_premio_convertido = wl_saida_0130-vlr_premio_brl ).
              ENDIF.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
              TRANSPORTING vlr_premio_brl vlr_premio_usd.

            WHEN 'VLR_PREMIO_BRL'.
              CLEAR v_vlr_premio.
              wl_saida_0130-vlr_premio_brl = ls_good-value.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
        TRANSPORTING vlr_premio_brl.

              LOOP AT gt_saida_0130 INTO wl_saida_0130_aux.
                v_vlr_premio = v_vlr_premio + wl_saida_0130_aux-vlr_premio_brl.
              ENDLOOP.

              IF ( v_vlr_premio > wl_cabecalho_0110-vlr_premio_brl ).
                CLEAR wl_saida_0130-vlr_premio_brl.
                MESSAGE s836(sd) WITH TEXT-e12 TEXT-e24 DISPLAY LIKE 'E'.
              ELSE.
                go_utils->z_converter_moeda( EXPORTING moeda                 = wl_cabecalho_0110-waers
                                                       taxa_cambio           = wl_saida_0130-taxa_cambio
                                                       vlr_usd               = space
                                                       vlr_brl               = wl_saida_0130-vlr_premio_brl
                                             IMPORTING vlr_premio_convertido = wl_saida_0130-vlr_premio_usd ).
              ENDIF.
              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
        TRANSPORTING vlr_premio_usd vlr_premio_brl.

            WHEN 'VLR_AJ_PREM_USD'.
              wl_saida_0130-vlr_aj_prem_usd = ls_good-value.

              go_utils->z_converter_moeda( EXPORTING moeda                 = wl_cabecalho_0110-waers
                                                     taxa_cambio           = wl_saida_0130-taxa_cambio
                                                     vlr_usd               = wl_saida_0130-vlr_aj_prem_usd
                                                     vlr_brl               = space
                                           IMPORTING vlr_premio_convertido = wl_saida_0130-vlr_aj_prem_brl ).

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
        TRANSPORTING vlr_aj_prem_brl vlr_aj_prem_usd.

            WHEN 'VLR_AJ_PREM_BRL'.
              wl_saida_0130-vlr_aj_prem_brl = ls_good-value.

              go_utils->z_converter_moeda( EXPORTING moeda                 = wl_cabecalho_0110-waers
                                                     taxa_cambio           = wl_saida_0130-taxa_cambio
                                                     vlr_usd               = space
                                                     vlr_brl               = wl_saida_0130-vlr_aj_prem_brl
                                           IMPORTING vlr_premio_convertido = wl_saida_0130-vlr_aj_prem_usd ).

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
        TRANSPORTING vlr_aj_prem_usd vlr_aj_prem_brl.

            WHEN 'VLR_RISCO_USD'.
              wl_saida_0130-vlr_risco_usd = ls_good-value.

              go_utils->z_converter_moeda( EXPORTING moeda                 = wl_cabecalho_0110-waers
                                                     taxa_cambio           = wl_saida_0130-taxa_cambio
                                                     vlr_usd               = wl_saida_0130-vlr_risco_usd
                                                     vlr_brl               = space
                                           IMPORTING vlr_premio_convertido = wl_saida_0130-vlr_risco_brl ).

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
        TRANSPORTING vlr_risco_brl vlr_risco_usd.

            WHEN 'VLR_RISCO_BRL'.
              wl_saida_0130-vlr_risco_brl = ls_good-value.

              go_utils->z_converter_moeda( EXPORTING moeda                 = wl_cabecalho_0110-waers
                                                     taxa_cambio           = wl_saida_0130-taxa_cambio
                                                     vlr_usd               = space
                                                     vlr_brl               = wl_saida_0130-vlr_risco_brl
                                           IMPORTING vlr_premio_convertido = wl_saida_0130-vlr_risco_usd ).

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX ls_good-row_id
        TRANSPORTING vlr_risco_usd vlr_risco_brl.

            WHEN OTHERS.
              CONTINUE.
          ENDCASE.

          IF ( gt_msg_return IS NOT INITIAL ).
            go_utils->z_show_splitter_error( i_show = '' ).
          ENDIF.

          CALL METHOD obj_alv_0130->get_current_cell
            IMPORTING
              es_row_id = gv_row
              es_col_id = gv_col.

          CALL METHOD obj_alv_0130->refresh_table_display
            EXPORTING
              is_stable = wl_stable.

          LEAVE TO SCREEN 0100.



      ENDCASE.
    ENDLOOP.
  ENDMETHOD.                    "FIELD_DATA_CHANGED

  METHOD on_data_changed_finished.

    IF et_good_cells[] IS NOT INITIAL.

    ENDIF.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = space
        i_repid    = sy-repid
      IMPORTING
        e_messagem = wg_mensagem
      TABLES
        it_msgs    = gt_msg_return.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD top_of_page.

*    BREAK-POINT.

  ENDMETHOD.                    "TOP_OF_PAGE

  METHOD on_onf4.

    TYPES: BEGIN OF ty_csks,
             kokrs TYPE csks-kokrs,
             kostl TYPE csks-kostl,
             datbi TYPE csks-datbi,
             datab TYPE csks-datab,
             ltext TYPE cskt-ltext,
           END OF ty_csks.

    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE dd03l-fieldname,
             s(1)      TYPE c,
           END OF ty_field,

           BEGIN OF ty_value,
             tabname    TYPE dd03l-tabname,
             fieldname  TYPE dd03l-fieldname,
             char79(79) TYPE c,
           END OF ty_value,

           BEGIN OF ty_material,
             matnr TYPE mara-matnr,
             maktx TYPE makt-maktx,
           END OF ty_material.



    DATA: BEGIN OF wl_valuetab,
            field(50),
          END OF wl_valuetab.

    " Tabelas internas
    DATA: go_utils            TYPE REF TO zutils,
          gt_j_1bbranch       TYPE TABLE OF j_1bbranch,
          gt_tgsbt            TYPE TABLE OF tgsbt,
          gt_csks             TYPE TABLE OF ty_csks,
          gt_bnka             TYPE TABLE OF bnka,
          gt_t042z            TYPE TABLE OF t042z,
          gt_t012t            TYPE TABLE OF t012t,
          gt_t008t            TYPE TABLE OF t008t,
          gt_zib_contabil_chv TYPE TABLE OF zib_contabil_chv,
          gt_valuetab         LIKE TABLE OF wl_valuetab,
          gt_field            TYPE TABLE OF ty_field,
          gt_value            TYPE TABLE OF ty_value,
          gt_material         TYPE TABLE OF ty_material,
          gt_t005u            TYPE TABLE OF t005u.

    " Tabelas work-áreas
    DATA: wl_j_1bbranch TYPE j_1bbranch,
          wl_csks       TYPE ty_csks,
          wl_field      TYPE ty_field,
          wl_bnka       TYPE bnka,
          wl_t042z      TYPE t042z,
          wl_t012t      TYPE t012t,
          wl_t008t      TYPE t008t,
          wl_value      TYPE ty_value,
          wl_material   TYPE ty_material,
          wl_t005u      TYPE t005u.

    " Variáveis
    DATA: wl_char(20),
          wl_fieldname(30),
          wl_tabname(30),
          vl_deakt         TYPE anla-deakt,
          wl_index         TYPE sy-tabix,
          v_anln1          TYPE anln1,
          lv_estado        TYPE zaa001-cod_regi.

    DEFINE d_preenche_value.
      wl_valuetab = &1.
      APPEND wl_valuetab TO gt_valuetab.
    END-OF-DEFINITION.

    CREATE OBJECT go_utils.

*-----------------------------------------------*
* SET ONF4                                      *
* SUBTELA 0120 - CONTAS A PAGAR E RECEBER       *
* MATCH CODE IN ALV                             *
*-----------------------------------------------*

    CASE screen_item.
      WHEN '0120'.
*        OP_MODO = C_ONF4_TAB1.
        READ TABLE gt_saida_0120 INTO wl_saida_0120 INDEX es_row_no-row_id.

        CASE e_fieldname.
          WHEN 'FILIAL'.

            SELECT DISTINCT branch name
              FROM j_1bbranch
              INTO CORRESPONDING FIELDS OF TABLE gt_j_1bbranch
             WHERE bukrs EQ wl_cabecalho_0110-bukrs.

            IF gt_j_1bbranch IS INITIAL.
              "CHECK empresa.
              SELECT SINGLE * FROM t001 INTO @DATA(ls_t001) WHERE bukrs EQ @wl_cabecalho_0110-bukrs.
              IF ls_t001-land1 NE 'BR'.
                SELECT DISTINCT *
              FROM tgsbt
              INTO CORRESPONDING FIELDS OF TABLE gt_tgsbt
                  WHERE spras EQ sy-langu.
              ENDIF.

              wl_fieldname = 'GSBER'.
              wl_tabname   = 'TGSBT'.

              LOOP AT gt_tgsbt INTO DATA(wl_tgsbt).
                d_preenche_value: wl_tgsbt-gsber,
                                  wl_tgsbt-gtext.
              ENDLOOP.

              wl_field-tabname   = wl_tabname.
              wl_field-fieldname = 'GSBER'.
              wl_field-s         = 'X'.
              APPEND wl_field TO gt_field.

              wl_field-tabname   = wl_tabname.
              wl_field-fieldname = 'GTEXT'.
              wl_field-s         = 'X'.
              APPEND wl_field TO gt_field.


            ELSE.


              wl_fieldname = 'BRANCH'.
              wl_tabname   = 'J_1BBRANCH'.

              LOOP AT gt_j_1bbranch INTO wl_j_1bbranch.
                d_preenche_value: wl_j_1bbranch-branch,
                                  wl_j_1bbranch-name.
              ENDLOOP.

              wl_field-tabname   = wl_tabname.
              wl_field-fieldname = 'BRANCH'.
              wl_field-s         = 'X'.
              APPEND wl_field TO gt_field.

              wl_field-tabname   = wl_tabname.
              wl_field-fieldname = 'NAME'.
              wl_field-s         = 'X'.
              APPEND wl_field TO gt_field.
            ENDIF.
          WHEN 'PAIS_PGTO'.
            SELECT DISTINCT banks banka
              FROM bnka
              INTO CORRESPONDING FIELDS OF TABLE gt_bnka.

            wl_fieldname = 'BANKS'.
            wl_tabname   = 'BNKA'.

            LOOP AT gt_bnka INTO wl_bnka.
              d_preenche_value: wl_bnka-banks,
                                wl_bnka-banka.
            ENDLOOP.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'BANKS'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'BANKA'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

          WHEN 'FORMA_PGTO'.

            SELECT DISTINCT zlsch text1
              FROM t042z
              INTO CORRESPONDING FIELDS OF TABLE gt_t042z
             WHERE land1 = 'BR'.

            wl_fieldname = 'ZLSCH'.
            wl_tabname   = 'T042Z'.

            LOOP AT gt_t042z INTO wl_t042z.
              d_preenche_value: wl_t042z-zlsch,
                                wl_t042z-text1.
            ENDLOOP.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'ZLSCH'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'TEXT1'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

          WHEN 'BCO_EMPRESA'.

            SELECT DISTINCT hbkid text1 spras
              FROM t012t
              INTO CORRESPONDING FIELDS OF TABLE gt_t012t
             WHERE spras = 'PT'.

            wl_fieldname = 'HBKID'.
            wl_tabname   = 'T012T'.

            LOOP AT gt_t012t INTO wl_t012t.
              d_preenche_value: wl_t012t-hbkid,
                                wl_t012t-text1.
            ENDLOOP.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'HBKID'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'TEXT1'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

          WHEN 'BCO_PARCEIRO'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wl_cabecalho_0110-cod_seguradora
              IMPORTING
                output = wl_cabecalho_0110-cod_seguradora.

            SELECT *
              FROM lfbk
              INTO TABLE gt_lfbk
             WHERE lifnr = wl_cabecalho_0110-cod_seguradora.

            wl_fieldname = 'BVTYP'.
            wl_tabname   = 'LFBK'.

            LOOP AT gt_lfbk INTO wl_lfbk.
              d_preenche_value: wl_lfbk-lifnr,
                                wl_lfbk-bankn,
                                wl_lfbk-bvtyp.
            ENDLOOP.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'LIFNR'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'BANKN'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'BVTYP'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

          WHEN 'BLOQ_PGTO'.

            SELECT zahls textl spras
              FROM t008t
              INTO CORRESPONDING FIELDS OF TABLE gt_t008t
             WHERE spras = 'PT'.

            wl_fieldname = 'ZAHLS'.
            wl_tabname   = 'T008T'.

            LOOP AT gt_t008t INTO wl_t008t.
              d_preenche_value: wl_t008t-zahls,
                                wl_t008t-textl.
            ENDLOOP.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'ZAHLS'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'TEXTL'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.
        ENDCASE.
*-----------------------------------------------*
* SET ONF4                                      *
* SUBTELA 0130 - BENS ASSEGURADOS               *
* MATCH CODE IN ALV                             *
*-----------------------------------------------*
      WHEN '0130'.
*        OP_MODO = C_ONF4_TAB2.
        READ TABLE gt_saida_0130 INTO wl_saida_0130 INDEX es_row_no-row_id.

        CASE e_fieldname.
          WHEN 'FILIAL'.

            SELECT DISTINCT branch name
              FROM j_1bbranch
              INTO CORRESPONDING FIELDS OF TABLE gt_j_1bbranch
             WHERE bukrs EQ wl_cabecalho_0110-bukrs.

            wl_fieldname = 'BRANCH'.
            wl_tabname   = 'J_1BBRANCH'.

            LOOP AT gt_j_1bbranch INTO wl_j_1bbranch.
              d_preenche_value: wl_j_1bbranch-branch,
                                wl_j_1bbranch-name.
            ENDLOOP.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'BRANCH'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'NAME'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

          WHEN 'CENTRO_CUSTO'.

            CHECK wl_saida_0130-filial IS NOT INITIAL.

            SELECT a~kokrs a~kostl a~datbi a~datab b~ltext
              INTO CORRESPONDING FIELDS OF TABLE gt_csks
             FROM csks AS a INNER JOIN cskt AS b ON a~kokrs = b~kokrs
                                                AND a~kostl = b~kostl
                                                AND a~datbi = b~datbi
            WHERE a~bukrs  = wl_cabecalho_0110-bukrs
              AND a~gsber  = wl_saida_0130-filial
              AND a~datab  LE sy-datum
              AND a~datbi  GE sy-datum
              AND b~spras  EQ sy-langu.

            LOOP AT gt_csks INTO wl_csks.
              d_preenche_value: wl_csks-kostl,
                                wl_csks-ltext.
            ENDLOOP.

            wl_field-tabname   = 'CSKS'.
            wl_field-fieldname = 'KOSTL'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = 'CSKT'.
            wl_field-fieldname = 'LTEXT'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

          WHEN 'CHASSI'
            OR 'IMOBILIZADO'
            OR 'NR_SERIE'.

            CLEAR: gt_anla, wl_anla.

            SELECT bukrs anln1 anln2 txt50 invnr sernr
              FROM anla
              INTO CORRESPONDING FIELDS OF TABLE gt_anla
             WHERE bukrs EQ wl_cabecalho_0110-bukrs
               AND deakt EQ vl_deakt.

            wl_fieldname = 'INVNR'.
            wl_tabname   = 'ANLA'.

            LOOP AT gt_anla INTO wl_anla.
              CLEAR wl_anlz.

              SELECT SINGLE *
                FROM anlz
                INTO wl_anlz
               WHERE bukrs = wl_anla-bukrs
                 AND anln1 = wl_anla-anln1
                 AND anln2 = wl_anla-anln2
                 AND adatu <= sy-datum
                 AND bdatu >= sy-datum.

              SELECT SINGLE cod_regi
                FROM zaa001
                INTO lv_estado
               WHERE bukrs = wl_anlz-bukrs
                 AND anln1 = wl_anlz-anln1.

              d_preenche_value: wl_anla-anln1,
                                wl_anla-anln2,
                                wl_anla-invnr,
                                wl_anlz-kostl,
                                wl_anlz-kfzkz,
                                wl_anla-txt50,
                                wl_anla-sernr,
                                wl_anlz-werks,
                                lv_estado.
            ENDLOOP.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'ANLN1'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'ANLN2'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'INVNR'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = 'ANLZ'.
            wl_field-fieldname = 'KOSTL'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = 'ANLZ'.
            wl_field-fieldname = 'KFZKZ'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'TXT50'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'SERNR'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = 'ANLZ'.
            wl_field-fieldname = 'WERKS'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = 'ZAA001'.
            wl_field-fieldname = 'COD_REGI'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

          WHEN 'MERCADORIA'.

            SELECT matnr
              FROM mara
              INTO CORRESPONDING FIELDS OF TABLE gt_mara.

            SELECT DISTINCT maktx matnr
              FROM makt
              INTO CORRESPONDING FIELDS OF TABLE gt_makt
           FOR ALL ENTRIES IN gt_mara
             WHERE matnr = gt_mara-matnr.

            wl_fieldname = 'MATNR'.
            wl_tabname   = 'MARA'.

            SORT gt_makt BY matnr.
            LOOP AT gt_mara INTO wl_mara.
              READ TABLE gt_makt INTO wl_makt WITH KEY matnr = wl_mara-matnr
                  BINARY SEARCH.

              MOVE wl_mara-matnr TO wl_valuetab-field.
              APPEND wl_valuetab TO gt_valuetab.

              MOVE wl_makt-maktx TO wl_valuetab-field.
              APPEND wl_valuetab TO gt_valuetab.
            ENDLOOP.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'MATNR'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname   = 'MAKT'.
            wl_field-fieldname = 'MAKTX'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

          WHEN 'UF'.
            SELECT *
              FROM t005u
              INTO TABLE gt_t005u
             WHERE spras EQ 'PT'
               AND land1 EQ 'BR'.

            wl_fieldname = 'BLAND'.
            wl_tabname   = 'T005U'.

            LOOP AT gt_t005u INTO wl_t005u.
              MOVE wl_t005u-bland TO wl_valuetab-field.
              APPEND wl_valuetab TO gt_valuetab.
            ENDLOOP.

            wl_field-tabname   = wl_tabname.
            wl_field-fieldname = 'BLAND'.
            wl_field-s         = 'X'.
            APPEND wl_field TO gt_field.

          WHEN 'VORNR'.

            SELECT SINGLE *
              FROM caufv
              INTO @DATA(wa_caufv)
              WHERE aufnr  =  @wl_saida_0130-aufnr.

            SELECT *
              FROM afvc
              INTO TABLE @DATA(tl_afvc)
              WHERE aufpl = @wa_caufv-aufpl.

            CHECK tl_afvc IS NOT INITIAL.

            wl_fieldname  = 'VORNR'.
            wl_tabname    = 'AFVC'.

            LOOP AT tl_afvc INTO DATA(wl_afvc).
              wl_index = sy-tabix.
              MOVE: wl_afvc-vornr  TO wl_valuetab-field.
              APPEND wl_valuetab   TO gt_valuetab.
              CLEAR:  wl_valuetab.

              MOVE: wl_afvc-ltxa1  TO wl_valuetab-field.
              APPEND wl_valuetab   TO gt_valuetab.
              CLEAR:  wl_valuetab.

            ENDLOOP.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'VORNR'.
            wl_field-s = 'X'.
            APPEND wl_field TO gt_field.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'LTXA1'.
            wl_field-s = ' '.
            APPEND wl_field TO gt_field.
          WHEN OTHERS.

        ENDCASE.
    ENDCASE.

*------------------------------------------------------------------------------------*
*    A função constroi o matchcode, e insere os valores na tabela GT_VALUETAB        *
*    e retorna na GT_VALUE.                                                          *
*------------------------------------------------------------------------------------*
    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
      EXPORTING
        cucol                     = '10'
        curow                     = '5'
        fieldname                 = wl_fieldname
        tabname                   = wl_tabname
      IMPORTING
        index                     = wl_index
        select_value              = wl_char
      TABLES
        fields                    = gt_field
        select_values             = gt_value
        valuetab                  = gt_valuetab
      EXCEPTIONS
        field_not_in_ddic         = 001
        more_then_one_selectfield = 002
        no_selectfield            = 003.

*-----------------------------------------------*
* GET ONF4                                      *
* SUBTELA 0110 - CONTAS A PAGAR E RECEBER       *
* MATCH CODE IN ALV                             *
*-----------------------------------------------*
    IF ( sy-subrc IS INITIAL ).
      DELETE gt_msg_return WHERE field = e_fieldname
                             AND aba   = screen_item
                             AND tabix = es_row_no-row_id.

      CASE screen_item.
        WHEN 0120.
          READ TABLE gt_saida_0120 INTO wl_saida_0120 INDEX es_row_no-row_id.
          CASE e_fieldname.
            WHEN 'FILIAL'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'BRANCH'.
              IF sy-subrc EQ 0.
                MOVE: wl_value-char79 TO wl_saida_0120-filial.
                MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX es_row_no-row_id
                       TRANSPORTING filial.
              ELSE.
                READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'GSBER'.
                MOVE: wl_value-char79 TO wl_saida_0120-filial.
                MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX es_row_no-row_id
                       TRANSPORTING filial.
              ENDIF.

            WHEN 'PAIS_PGTO'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'BANKS'.
              MOVE wl_value-char79 TO wl_saida_0120-pais_pgto.

              MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX es_row_no-row_id
                    TRANSPORTING pais_pgto.

            WHEN 'FORMA_PGTO'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'ZLSCH'.
              MOVE wl_value-char79 TO wl_saida_0120-forma_pgto.

              MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX es_row_no-row_id
                    TRANSPORTING forma_pgto.

            WHEN 'BCO_EMPRESA'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'HBKID'.
              MOVE wl_value-char79 TO wl_saida_0120-bco_empresa.

              MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX es_row_no-row_id
                    TRANSPORTING bco_empresa.

            WHEN 'BCO_PARCEIRO'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'BVTYP'.
              MOVE wl_value-char79 TO wl_saida_0120-bco_parceiro.

              MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX es_row_no-row_id
                    TRANSPORTING bco_parceiro.

            WHEN 'BLOQ_PGTO'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'ZAHLS'.
              MOVE wl_value-char79 TO wl_saida_0120-bloq_pgto.

              MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX es_row_no-row_id
                    TRANSPORTING bloq_pgto.

            WHEN 'NRO_DOCUMENTO'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'BELNR'.
              MOVE wl_value-char79 TO wl_saida_0120-nro_documento.

              MODIFY gt_saida_0120 FROM wl_saida_0120 INDEX es_row_no-row_id
                    TRANSPORTING nro_documento.
          ENDCASE.

          go_utils->z_show_splitter_error( i_show = '' ).

          CALL METHOD obj_alv_0120->refresh_table_display
            EXPORTING
              is_stable = wl_stable.

*-----------------------------------------------*
* GET ONF4                                      *
* SUBTELA 0120 - BENS ASSEGURADOS               *
* MATCH CODE IN ALV                             *
*-----------------------------------------------*

        WHEN 0130.
          READ TABLE gt_saida_0130 INTO wl_saida_0130 INDEX es_row_no-row_id.

          CASE e_fieldname.
            WHEN 'FILIAL'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'BRANCH'.
              MOVE: wl_value-char79 TO wl_saida_0130-filial.
              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX es_row_no-row_id
                     TRANSPORTING filial.
            WHEN 'CENTRO_CUSTO'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'KOSTL'.
              MOVE: wl_value-char79 TO wl_saida_0130-centro_custo.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wl_saida_0130-centro_custo
                IMPORTING
                  output = wl_saida_0130-centro_custo.


              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX es_row_no-row_id
                     TRANSPORTING centro_custo.

            WHEN 'CHASSI'
              OR 'IMOBILIZADO'
              OR 'NR_SERIE'.

              LOOP AT gt_value INTO wl_value.
                CASE wl_value-fieldname.
                  WHEN 'INVNR'.
                    MOVE wl_value-char79 TO wl_saida_0130-chassi.
                  WHEN 'ANLN1'.
                    MOVE wl_value-char79 TO wl_saida_0130-imobilizado.
                  WHEN 'ANLN2'.
                    MOVE wl_value-char79 TO wl_saida_0130-subnumero.
                  WHEN 'KOSTL'.
                    MOVE wl_value-char79 TO wl_saida_0130-centro_custo.
                  WHEN 'TXT50'.
                    MOVE wl_value-char79 TO wl_saida_0130-descr_bens.
                  WHEN 'SERNR'.
                    MOVE wl_value-char79 TO wl_saida_0130-nr_serie.
                  WHEN 'WERKS'.
                    MOVE wl_value-char79 TO wl_saida_0130-filial.
                  WHEN 'COD_REGI'.
                    MOVE wl_value-char79 TO wl_saida_0130-uf.
                  WHEN OTHERS.
                ENDCASE.
              ENDLOOP.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX es_row_no-row_id
                     TRANSPORTING imobilizado  subnumero descr_bens
                                  centro_custo chassi nr_serie uf filial.

            WHEN 'MERCADORIA'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'MATNR'.
              MOVE wl_value-char79 TO wl_saida_0130-mercadoria.

              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'MAKTX'.
              MOVE wl_value-char79 TO wl_saida_0130-descr_bens.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX es_row_no-row_id
                     TRANSPORTING mercadoria descr_bens.

            WHEN 'UF'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'BLAND'.
              MOVE wl_value-char79 TO wl_saida_0130-uf.

              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX es_row_no-row_id
                     TRANSPORTING uf.

            WHEN 'VORNR'.
              READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'VORNR'.
              MOVE: wl_value-char79 TO wl_saida_0130-vornr.
              MODIFY gt_saida_0130 FROM wl_saida_0130 INDEX es_row_no-row_id
                     TRANSPORTING vornr.

            WHEN OTHERS.
          ENDCASE.

          go_utils->z_show_splitter_error( i_show = '' ).

          CALL METHOD obj_alv_0130->refresh_table_display
            EXPORTING
              is_stable = wl_stable.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "ON_ONF4

  METHOD on_double_click.
    DATA: vl_bloq_pgto  TYPE ty_saida_0120-bloq_pgto,
          vl_forma_pgto TYPE ty_saida_0120-forma_pgto.

    CLEAR: gt_selectedcell,
           vl_bloq_pgto,
           vl_forma_pgto.

    FIELD-SYMBOLS <fs_saida_0120> TYPE ty_saida_0120.

    CALL METHOD obj_alv_0120->get_selected_cells
      IMPORTING
        et_cell = gt_selectedcell.

    LOOP AT gt_saida_0120 ASSIGNING <fs_saida_0120>.
      READ TABLE gt_selectedcell INTO wl_selectedcell INDEX 1.

      CASE wl_selectedcell-col_id.
        WHEN 'BLOQ_PGTO'.

          IF ( vl_bloq_pgto IS INITIAL ).
            vl_bloq_pgto = <fs_saida_0120>-bloq_pgto.
          ELSE.
            <fs_saida_0120>-bloq_pgto = vl_bloq_pgto.
          ENDIF.

        WHEN 'FORMA_PGTO'.

          IF ( vl_forma_pgto IS INITIAL ).
            vl_forma_pgto = <fs_saida_0120>-forma_pgto.
          ELSE.
            <fs_saida_0120>-forma_pgto = vl_forma_pgto.
          ENDIF.

        WHEN OTHERS.
          EXIT.
      ENDCASE.
    ENDLOOP.

    IF sy-subrc IS INITIAL.
      CALL METHOD obj_alv_0120->refresh_table_display
        EXPORTING
          is_stable = wl_stable.
    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
