FUNCTION zsdmf001_gera_ov_solicitacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TIPO) TYPE  CHAR1 DEFAULT '1'
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"  TABLES
*"      TI_NRO_SOL_OV STRUCTURE  ZSDS007
*"      TE_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      TE_SAIDA_EXEC STRUCTURE  ZSDS010 OPTIONAL
*"      TE_VBAK STRUCTURE  VBAK OPTIONAL
*"      TE_VBAP STRUCTURE  VBAP OPTIONAL
*"  EXCEPTIONS
*"      OV_JA_CRIADA
*"      SOLICITACAO_NAO_EXISTE
*"----------------------------------------------------------------------
  DEFINE add_html.

    ls_obj_cont-line = &1.
    APPEND ls_obj_cont TO lt_obj_cont .
  END-OF-DEFINITION.
  TYPE-POOLS: rmdi.

  DATA: tl_header         TYPE TABLE OF zsdt0051 WITH HEADER LINE,
        wl_header         TYPE zsdt0051,
        tl_cond_pgt       TYPE  TABLE OF zsdt0052 WITH HEADER LINE,
        tl_cond_esp       TYPE  TABLE OF zsdt0073 WITH HEADER LINE,
        wl_header_in      TYPE bapisdhd1,
        wl_header_inx     TYPE bapisdhd1x,
        wl_header_inx2    TYPE  bapisdh1x,
        wl_vbeln          TYPE vbak-vbeln,
        wl_netwr          TYPE vbak-netwr,
        wl_vlrtot         TYPE vbak-netwr,
        wl_cont           TYPE sy-tabix,
        wl_cont_aux       TYPE sy-tabix,
        wl_fieldname      TYPE rmdi_name,
        wl_text           TYPE rmdi_ddtxt,
        tl_ovs            TYPE TABLE OF ty_ovs WITH HEADER LINE,
        wl_itens          TYPE zsdt0053,
        tl_vbuv           TYPE  TABLE OF vbuv WITH HEADER LINE,
        tl_itens          TYPE TABLE OF zsdt0053 WITH HEADER LINE,
        tl_itens_aux      TYPE TABLE OF zsdt0053 WITH HEADER LINE,
        tl_pgt_ant        TYPE TABLE OF zsdt0054 WITH HEADER LINE,
        tl_logistica      TYPE TABLE OF zsdt0055 WITH HEADER LINE,
        tl_preco          TYPE TABLE OF zsdt0059 WITH HEADER LINE,
*        tl_0048           TYPE TABLE OF zsdt0048 WITH HEADER LINE,
        tl_items_in       TYPE TABLE OF bapisditm  WITH HEADER LINE,
*        TL_ITEMS_IN_AUX       TYPE TABLE OF BAPISDITM  WITH HEADER LINE,
        tl_items_inx      TYPE TABLE OF bapisditmx WITH HEADER LINE,
        tl_partners       TYPE TABLE OF bapiparnr  WITH HEADER LINE,
*        TL_SCHEDULES      TYPE TABLE OF BAPISCHDL  WITH HEADER LINE,
        tl_schedules_in   TYPE TABLE OF bapischdl  WITH HEADER LINE,
*        TL_SCHEDULES_IN_AUX   TYPE TABLE OF BAPISCHDL  WITH HEADER LINE,
        tl_schedules_inx  TYPE TABLE OF bapischdlx WITH HEADER LINE,
        tl_conditions_in  TYPE TABLE OF bapicond   WITH HEADER LINE,
*        TL_CONDITIONS_IN_AUX  TYPE TABLE OF BAPICOND   WITH HEADER LINE,
        tl_conditions_inx TYPE TABLE OF bapicondx  WITH HEADER LINE,
        tl_return         TYPE TABLE OF bapiret2   WITH HEADER LINE,
        tl_return_aux     TYPE TABLE OF bapiret2   WITH HEADER LINE,
        tl_text_in        TYPE TABLE OF bapisdtext WITH HEADER LINE,
        tl_saida_exec     TYPE TABLE OF ty_saida_exec WITH HEADER LINE,
        tl_saida_exec_aux TYPE TABLE OF ty_saida_exec WITH HEADER LINE,
        tl_mara           TYPE TABLE OF mara WITH HEADER LINE,
        tl_makt           TYPE TABLE OF makt WITH HEADER LINE,
        tl_t001w          TYPE TABLE OF t001w WITH HEADER LINE,
        tl_kna1           TYPE TABLE OF kna1 WITH HEADER LINE,
        tl_tvkot          TYPE TABLE OF tvkot WITH HEADER LINE,
        tl_tvgrt          TYPE TABLE OF tvgrt WITH HEADER LINE,
        tl_zmail          TYPE TABLE OF zmail WITH HEADER LINE,
        tl_nro_sol_ov     TYPE TABLE OF zsds007 WITH HEADER LINE,
        wl_posnr          TYPE sy-tabix,
        wl_vlr_covert     TYPE dzmeng, "plfh-mgvgw,
        tl_texto          TYPE catsxt_longtext_itab,
        wl_texto          TYPE LINE OF catsxt_longtext_itab,
        wl_dmbtr          TYPE zsdt0053-dmbtr,
*        wl_bapiparex  TYPE bapiparex         ,
        tl_bapiparex      TYPE TABLE OF bapiparex WITH HEADER LINE,
        tl_log_erro       TYPE TABLE OF bapiincomp  WITH HEADER LINE, "ZSDT0066 Trazer o erro correto na geração OV  - BG #122413
        wl_bape_vbak      TYPE bape_vbak,
        wl_bape_vbakx     TYPE bape_vbakx,
        lt_obj_cont       TYPE TABLE OF solisti1,
        ls_obj_cont       TYPE solisti1,
        wl_field(300),
        wl_qtd(20),
        wl_data(10),
        wl_t001w          TYPE t001w,
        wl_matnr(18),
        wl_kna1           TYPE kna1,
        wl_assunto        TYPE string,
        lv_email          TYPE zmail-email,
        lv_vkorg          TYPE zsdt0051-vkorg,
        r_auart_ser       TYPE RANGE OF bapisdhd1-doc_type.

  REFRESH: tl_items_in, tl_items_inx,tl_partners, tl_schedules_in, tl_schedules_inx,
           tl_conditions_in, tl_conditions_inx, tl_return,
           tl_text_in, tl_saida_exec, estrutura, events, tl_mara, tl_bapiparex, tl_pgt_ant, tl_logistica, tl_preco,
           tl_header, tl_cond_pgt, tl_saida_exec, tl_texto, tl_vbuv,
           tl_zmail, tl_nro_sol_ov, tl_zmail, tl_t001w, tl_kna1, tl_makt, tl_tvgrt, tl_tvkot,
           tl_cond_esp.
  CLEAR: wl_header, wl_header_in, wl_header_inx, tl_itens, tl_items_in, tl_items_inx,tl_partners, tl_ovs,
          tl_schedules_in, tl_schedules_inx, tl_conditions_in, tl_conditions_inx, tl_return,
         tl_ovs, tl_itens_aux, tl_text_in, tl_saida_exec, wa_estrutura, xs_events, tl_mara,
         wl_bape_vbak, wl_bape_vbakx, tl_bapiparex,tl_pgt_ant, tl_logistica, tl_preco, tl_cond_pgt, tl_ovs, wl_cont,
         wl_cont_aux, wl_texto, wl_dmbtr, tl_vbuv, tl_cond_esp.

  IF ti_nro_sol_ov[] IS NOT INITIAL.
    SELECT  *
      FROM zsdt0051
      INTO TABLE tl_header
       FOR ALL ENTRIES IN ti_nro_sol_ov
       WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov
         AND status        EQ 'L'.

    IF sy-subrc IS INITIAL.

      SELECT *
        FROM tvkot
        INTO TABLE tl_tvkot
         FOR ALL ENTRIES IN tl_header
         WHERE spras EQ sy-langu
           AND vkorg EQ tl_header-vkorg.

      SELECT *
        FROM tvgrt
        INTO TABLE tl_tvgrt
         FOR ALL ENTRIES IN tl_header
         WHERE spras EQ sy-langu
           AND vkgrp EQ tl_header-vkgrp.

      SELECT *
        FROM kna1
        INTO TABLE tl_kna1
         FOR ALL ENTRIES IN tl_header
         WHERE kunnr EQ tl_header-kunnr.

      SELECT *
        FROM zsdt0052
        INTO TABLE tl_cond_pgt
        FOR ALL ENTRIES IN ti_nro_sol_ov
         WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.

      SELECT *
        FROM zsdt0073
        INTO TABLE tl_cond_esp
        FOR ALL ENTRIES IN ti_nro_sol_ov
         WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.

      SELECT *
        FROM zsdt0054
        INTO TABLE tl_pgt_ant
        FOR ALL ENTRIES IN ti_nro_sol_ov
         WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.

      SELECT *
        FROM zsdt0055
        INTO TABLE tl_logistica
         FOR ALL ENTRIES IN ti_nro_sol_ov
         WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.

      SELECT *
       FROM zsdt0059
       INTO TABLE tl_preco
        FOR ALL ENTRIES IN ti_nro_sol_ov
        WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.

      SELECT *
        FROM zsdt0053
        INTO TABLE tl_itens
        FOR ALL ENTRIES IN ti_nro_sol_ov
         WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov
           AND posnr      EQ ti_nro_sol_ov-posnr
           AND vbeln      EQ space.

      IF sy-subrc IS INITIAL.
*      SELECT *
*        FROM ZSDT0048
*        INTO TABLE TL_0048
*         FOR ALL ENTRIES IN TL_ITENS
*          WHERE BUKRS   EQ WL_HEADER-VKORG
*            AND VTWEG   EQ WL_HEADER-VTWEG
*            AND SPART   EQ TL_ITENS-SPART.
**            AND werks_d EQ tl_itens-werks
**            AND vkbur   EQ wl_header-vkbur.

        SELECT *
          FROM mara
          INTO TABLE tl_mara
           FOR ALL ENTRIES IN tl_itens
            WHERE matnr EQ tl_itens-matnr.

        SELECT *
          FROM makt
          INTO TABLE tl_makt
          FOR ALL ENTRIES IN tl_itens
           WHERE matnr EQ tl_itens-matnr
             AND spras EQ sy-langu.

        SELECT *
          FROM t001w
           INTO TABLE tl_t001w
           FOR ALL ENTRIES IN tl_itens
           WHERE werks EQ tl_itens-werks.

*      ELSE.
*        RAISE OV_JA_CRIADA.
*      ENDIF.

        tl_itens_aux[] = tl_itens[].

*    SORT: TL_ITENS BY AUART SPART INCO1.
        CLEAR: tl_ovs.

        LOOP AT tl_header INTO wl_header.
          CLEAR: wl_header_in.

          REFRESH: tl_texto, tl_text_in.
          IF i_tipo EQ '1'.
            CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
              EXPORTING
                im_title = 'Texto para Ordem de Venda'
              CHANGING
                ch_text  = tl_texto.

            LOOP AT tl_texto INTO wl_texto.
              CLEAR: tl_text_in.
              tl_text_in-text_line(72) = wl_texto.
              tl_text_in-text_id    = '0002'.
              tl_text_in-langu      = sy-langu.
              tl_text_in-format_col = '/'.
              APPEND tl_text_in.
            ENDLOOP.
          ENDIF.

*        REFRESH: TL_TEXT_IN.
*        WL_CONT = STRLEN( WL_HEADER-OBSERVACAO ).
*        IF WL_CONT GT 132.
*          DO.
*            TL_TEXT_IN-TEXT_LINE  = WL_HEADER-OBSERVACAO+WL_CONT_AUX(132).
*            TL_TEXT_IN-TEXT_ID    = '0002'.
*            TL_TEXT_IN-LANGU      = SY-LANGU.
*            TL_TEXT_IN-FORMAT_COL = '/'.
*            APPEND TL_TEXT_IN.
*            ADD 132 TO WL_CONT_AUX.
*            IF WL_CONT_AUX GE WL_CONT.
*              EXIT.
*            ENDIF.
*          ENDDO.
*        ELSE.
*          TL_TEXT_IN-TEXT_LINE  = WL_HEADER-OBSERVACAO.
*          TL_TEXT_IN-TEXT_ID    = '0002'.
*          TL_TEXT_IN-LANGU      = SY-LANGU.
*          TL_TEXT_IN-FORMAT_COL = '/'.
*          APPEND TL_TEXT_IN.
*        ENDIF.
***          READ TABLE TL_COND_PGT
***            WITH KEY NRO_SOL_OV = WL_HEADER-NRO_SOL_OV.
***
***          IF TL_COND_PGT-PGTO_ANT EQ 'X'
***          AND WL_HEADER-AUART NE 'ZPER'.
***            WL_HEADER_IN-DLV_BLOCK = '11'.
***          ENDIF.
***
**** Extension - Campo ZPESAGEM
***          CLEAR TL_BAPIPAREX.
***          TL_BAPIPAREX-STRUCTURE     = 'BAPE_VBAK'.
***          IF WL_HEADER-AUART EQ 'ZPER'
***          OR WL_HEADER-AUART EQ 'ZEXP'
***          OR WL_HEADER-AUART EQ 'ZEXI'
***          OR WL_HEADER-AUART EQ 'YV02'.
***            WL_BAPE_VBAK-ZPESAGEM = '02'.
***          ELSE.
***            WL_BAPE_VBAK-ZPESAGEM = '01'.
***          ENDIF.
***          TL_BAPIPAREX-VALUEPART1 = WL_BAPE_VBAK.
***          APPEND TL_BAPIPAREX.
***
***          CLEAR TL_BAPIPAREX.
***          TL_BAPIPAREX-STRUCTURE     = 'BAPE_VBAKX'.
***          WL_BAPE_VBAKX-ZPESAGEM = 'X'.
***          TL_BAPIPAREX-VALUEPART1 = WL_BAPE_VBAKX.
***          APPEND  TL_BAPIPAREX.
***
***
***          WL_HEADER_IN-SALES_ORG  = WL_HEADER-VKORG.
***          WL_HEADER_IN-DISTR_CHAN = WL_HEADER-VTWEG.
***          WL_HEADER_IN-SALES_OFF  = WL_HEADER-VKBUR.
***          WL_HEADER_IN-SALES_GRP  = WL_HEADER-VKGRP.
****    WL_HEADER_IN-PURCH_DATE   = WL_HEADER-ERDAT.
****    CONCATENATE WL_HEADER-CULTURA WL_HEADER-SAFRA I_DOC_SIMULACAO INTO WL_HEADER_IN-PURCH_NO_C SEPARATED BY '-'.
****    wl_header_in-purch_no_c = i_doc_simulacao.
***          WL_HEADER_IN-CURRENCY   = WL_HEADER-WAERK.
****    WL_HEADER_IN-CURRENCY   = WL_HEADER-WAERK.
***          WL_HEADER_IN-PYMT_METH   = 'P'.
***          WL_HEADER_IN-INCOTERMS1 = WL_HEADER-INCO1.
***          WL_HEADER_IN-INCOTERMS2 = WL_HEADER-INCO2.
***          WL_HEADER_IN-DIVISION   = WL_HEADER-SPART.
***          WL_HEADER_IN-DOC_TYPE   = WL_HEADER-AUART.
***          WL_HEADER_IN-PMNTTRMS   = TL_COND_PGT-ZTERM.
***          WL_HEADER_IN-PURCH_NO_C = |SOL.OV.{ WL_HEADER-NRO_SOL_OV }|.
***          WL_HEADER_IN-FIX_VAL_DY =  TL_COND_PGT-VALDT.
***          WL_HEADER_IN-PYMT_METH =  TL_COND_PGT-ZLSCH.
***          WL_HEADER_IN-DLVSCHDUSE =  WL_HEADER-VKAUS.
***
***          IF WL_HEADER-AUART EQ 'ZREM'.
***            WL_HEADER_IN-REF_DOC_L  = WL_HEADER-NRO_SOL_OV.
***          ENDIF.
***
***          IF I_VBELN IS NOT INITIAL.
***            WL_HEADER_IN-REFOBJKEY  = I_VBELN.
***            WL_HEADER_IN-PURCH_DATE = SY-DATUM.
***            WL_HEADER_IN-REF_DOC_L  = I_VBELN.
***            WL_HEADER_IN-REF_DOC    = I_VBELN.
***            WL_HEADER_IN-REFOBJTYPE = 'VBRK'.
***            WL_HEADER_IN-REFDOCTYPE = 'M'.
***            WL_HEADER_IN-REFDOC_CAT = 'M'.
***          ENDIF.

*    IF WL_HEADER-TPSIM EQ 'TS'.
*      WL_HEADER_IN-PMNTTRMS = 'I001'.
*      WL_HEADER_IN-FIX_VAL_DY = WL_HEADER-DTPGTCULT.
*
*    ELSEIF WL_HEADER-TPSIM EQ 'AD'.
*      WL_HEADER_IN-PMNTTRMS = 'I002'.
*    ELSEIF WL_HEADER-TPSIM EQ 'VV'.
*      WL_HEADER_IN-PMNTTRMS = 'I003'.
*    ELSEIF WL_HEADER-TPSIM EQ 'TV'.
*      WL_HEADER_IN-PMNTTRMS = 'I004'.
*      WL_HEADER_IN-FIX_VAL_DY = WL_HEADER-DTPGTCULT.
*    ELSEIF WL_HEADER-TPSIM EQ 'VP'.
*      WL_HEADER_IN-PMNTTRMS = 'I005'.
*    ENDIF.

*---Monta dados de Parceiro
*          REFRESH: TL_PARTNERS.
*          CLEAR: TL_PARTNERS.
*          READ TABLE TL_ITENS INTO WL_ITENS
*            WITH KEY NRO_SOL_OV = WL_HEADER-NRO_SOL_OV.
*
*          IF WL_HEADER_IN-DOC_TYPE NE 'ZEXI'
*          AND WL_HEADER_IN-DOC_TYPE NE 'ZEXP'.
*            IF WL_ITENS-PONTO_C IS INITIAL.
*              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                EXPORTING
*                  INPUT  = WL_ITENS-WERKS
*                IMPORTING
*                  OUTPUT = TL_PARTNERS-PARTN_NUMB.
*            ELSE.
*              TL_PARTNERS-PARTN_NUMB = WL_ITENS-PONTO_C.
*            ENDIF.
*            TL_PARTNERS-PARTN_ROLE = 'PC'.
*            APPEND TL_PARTNERS.
*            CLEAR TL_PARTNERS.
*
*            TL_PARTNERS-PARTN_ROLE = 'LR'.
*            TL_PARTNERS-PARTN_NUMB = WL_HEADER-KUNNR.
*            APPEND TL_PARTNERS.
*            CLEAR TL_PARTNERS.
*
*          ENDIF.
*
*          TL_PARTNERS-PARTN_ROLE = 'AG'.
*          TL_PARTNERS-PARTN_NUMB = WL_HEADER-KUNNR.
*          APPEND TL_PARTNERS.
*          CLEAR TL_PARTNERS.
*
*          TL_PARTNERS-PARTN_ROLE = 'RE'.
*          TL_PARTNERS-PARTN_NUMB = WL_HEADER-KUNNR.
*          APPEND TL_PARTNERS.
*          CLEAR TL_PARTNERS.
*
*          TL_PARTNERS-PARTN_ROLE = 'RG'.
*          TL_PARTNERS-PARTN_NUMB = WL_HEADER-KUNNR.
*          APPEND TL_PARTNERS.
*          CLEAR TL_PARTNERS.
*
*          TL_PARTNERS-PARTN_ROLE = 'WE'.
*          TL_PARTNERS-PARTN_NUMB = WL_HEADER-KUNNR.
*          APPEND TL_PARTNERS.
*          CLEAR TL_PARTNERS.
*---Monta dados de Parceiro

*--> 06/07/2023 - Migração S4 - LA - Início
          SORT tl_itens BY nro_sol_ov.
*<--- S4 Migration - 06/07/2023 - LA - Fim

*** Inicio - Rubenilson Pereira - 09.10.2025 #192341
          SELECT SINGLE b~param_espec
            FROM zsdt0051 AS a
            INNER JOIN zsdt0057 AS b
            ON b~tp_venda = a~tp_venda
            INTO @DATA(lv_param_espec)
            WHERE a~nro_sol_ov = @wl_header-nro_sol_ov.
*** Fim - Rubenilson Pereira - 09.10.2025 #192341

          LOOP AT tl_itens INTO wl_itens
            WHERE nro_sol_ov EQ wl_header-nro_sol_ov.

*           Verifica se é algodão para checar se existe docnum de retorno
            IF wl_header-auart EQ 'ZEXI' AND wl_header-auart EQ 'ZEXP'.
              READ TABLE tl_mara INTO DATA(wa_mara) WITH KEY matnr = wl_itens-matnr.
              IF sy-subrc IS INITIAL.
                IF wa_mara-matkl EQ '700140'.
                  CHECK wl_itens-docnum_rt IS NOT INITIAL.
                ENDIF.
              ENDIF.
            ENDIF.

            READ TABLE tl_cond_pgt WITH KEY nro_sol_ov = wl_header-nro_sol_ov.

*           Processo Novo para o Algodão caso existir Auart cadastrado na 53 utilizar no Header
            wl_header-auart =
            COND #( WHEN wl_itens-auart IS NOT INITIAL
                         THEN wl_itens-auart
                         ELSE wl_header-auart
                  ).

*           Extension - Campo ZPESAGEM
            FREE: tl_bapiparex.

            wl_bape_vbak =
            VALUE #(
                      zpesagem = SWITCH #( wl_header-auart
                                           WHEN 'ZPER' OR 'ZEXP' OR 'ZEXI' OR 'YV02'
                                           THEN '02'
                                           ELSE '01'
                                         )
                    ).

            wl_bape_vbakx = VALUE #( zpesagem = abap_true ).

            tl_bapiparex[] =
            VALUE #(
                     ( structure = 'BAPE_VBAK'  valuepart1 = wl_bape_vbak )
                     ( structure = 'BAPE_VBAKX' valuepart1 = wl_bape_vbakx )
                   ).

            wl_header_in =
            VALUE #(
                     dlv_block  = COND #( WHEN tl_cond_pgt-pgto_ant EQ abap_true AND wl_header-auart NE 'ZPER' THEN '11' ELSE '' )
                     sales_org  = wl_header-vkorg
                     distr_chan = wl_header-vtweg
                     sales_off  = wl_header-vkbur
                     sales_grp  = wl_header-vkgrp
                     currency   = wl_header-waerk
                     pymt_meth  = COND #( WHEN tl_cond_pgt-zlsch IS NOT INITIAL THEN tl_cond_pgt-zlsch ELSE 'P' )
                     incoterms1 = wl_header-inco1
                     incoterms2 = wl_header-inco2
                     division   = wl_header-spart
                     doc_type   = wl_header-auart
                     pmnttrms   = tl_cond_pgt-zterm
                     purch_no_c = |SOL.OV.{ wl_header-nro_sol_ov }| && '-' && wl_itens-posnr "US 150347-13-09-2024-#150347-RJF-fim
                     fix_val_dy =  tl_cond_pgt-valdt
                     dlvschduse =  wl_header-vkaus
                     ref_doc_l  = COND #( WHEN wl_header-auart EQ 'ZREM' THEN wl_header-nro_sol_ov )
            ).

            IF i_vbeln IS NOT INITIAL.
              wl_header_in-refobjkey  = i_vbeln.
              wl_header_in-purch_date = sy-datum.
              wl_header_in-ref_doc_l  = i_vbeln.
              wl_header_in-ref_doc    = i_vbeln.
              wl_header_in-refobjtype = 'VBRK'.
              wl_header_in-refdoctype = 'M'.
              wl_header_in-refdoc_cat = 'M'.
            ENDIF.

*---Monta dados de Parceiro
            REFRESH: tl_partners.
            CLEAR: tl_partners.

            IF wl_header_in-doc_type NE 'ZEXI' AND
               wl_header_in-doc_type NE 'ZEXP'.
              IF wl_itens-ponto_c IS INITIAL.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = wl_itens-werks
                  IMPORTING
                    output = tl_partners-partn_numb.
              ELSE.
                tl_partners-partn_numb = wl_itens-ponto_c.
              ENDIF.
              tl_partners-partn_role = 'PC'.
              APPEND tl_partners.
              CLEAR tl_partners.

              tl_partners-partn_role = 'LR'.
              tl_partners-partn_numb = wl_header-kunnr.
              APPEND tl_partners.
              CLEAR tl_partners.
            ENDIF.

            tl_partners-partn_role = 'AG'.
            tl_partners-partn_numb = wl_header-kunnr.
            APPEND tl_partners.
            CLEAR tl_partners.

            tl_partners-partn_role = 'RE'.
            tl_partners-partn_numb = wl_header-kunnr.
            APPEND tl_partners.
            CLEAR tl_partners.

            tl_partners-partn_role = 'RG'.
            tl_partners-partn_numb = wl_header-kunnr.
            APPEND tl_partners.
            CLEAR tl_partners.

            tl_partners-partn_role = 'WE'.
            tl_partners-partn_numb = wl_header-kunnr.
            APPEND tl_partners.
            CLEAR tl_partners.

            IF wl_itens-kunnr IS NOT INITIAL.
              LOOP AT tl_partners
                 WHERE partn_numb IS NOT INITIAL
                   AND partn_role NE 'PC'.

                tl_partners-partn_numb = wl_itens-kunnr.
                MODIFY tl_partners.
              ENDLOOP.
            ENDIF.
*---Monta dados de Parceiro

            IF wl_header_in-doc_type EQ 'ZEXI'
            OR wl_header_in-doc_type EQ 'ZEXP'.
              tl_partners-partn_role = 'Z1'.
              tl_partners-partn_numb = wl_itens-terminal.
              APPEND tl_partners.
              CLEAR tl_partners.
            ENDIF.

            IF wl_header-param_espec EQ 'M'.
              READ TABLE tl_cond_esp
                WITH KEY fixacao = wl_itens-fixacao.

              wl_header_in-pmnttrms   = tl_cond_esp-zterm.
            ENDIF.

            REFRESH: tl_items_in, tl_conditions_in, tl_schedules_in.


            CLEAR: wl_posnr.
            ADD 1 TO wl_posnr.
            tl_items_in-itm_number    =  wl_posnr * 10.
            tl_items_in-material      = wl_itens-matnr.
            tl_items_in-target_qty    = wl_vlr_covert = wl_itens-zmeng.
            tl_items_in-target_qu     = wl_itens-zieme.
            tl_items_in-sales_unit    = wl_itens-zieme.
            tl_items_in-incoterms1    = wl_header_in-incoterms1.

            IF wl_header_in-doc_type EQ 'ZELV'.
              tl_items_in-cfop_long    = '7933AA'.
            ELSEIF wl_header_in-doc_type EQ 'ZSER' OR wl_header_in-doc_type EQ 'ZSAN'.
              tl_items_in-cfop_long    = '5933AA'.
            ENDIF.

            IF wl_header_in-doc_type EQ 'ZEXI'
            OR wl_header_in-doc_type EQ 'ZEXP'
            OR wl_header-param_espec EQ 'X'  .        "atende as vendas ZFEX algodão
              tl_items_in-volume     = wl_itens-volum.
              tl_items_in-volunit    = wl_itens-voleh.
              tl_items_in-gross_wght = wl_itens-brgew.
              tl_items_in-net_weight = wl_itens-zmeng.
              tl_items_in-untof_wght = wl_itens-zieme.
            ELSE.
              CLEAR: tl_items_in-gross_wght,
                     tl_items_in-net_weight,
                     tl_items_in-volume,
                     tl_items_in-volunit,
                     tl_items_in-untof_wght.

            ENDIF.

*            TL_ITEMS_IN-USAGE_IND    = 'I'.
            tl_items_in-plant         = wl_itens-werks.
            tl_items_in-batch         = wl_itens-charg.
*            TL_ITEMS_IN-SHIP_POINT   = WL_ITENS-WERKS.
            tl_items_in-store_loc     = wl_itens-lgort.
            tl_items_in-fix_val_dy    = wl_itens-valdt.

            IF tl_items_in-fix_val_dy IS NOT INITIAL.
              wl_header_in-fix_val_dy = tl_items_in-fix_val_dy.
            ENDIF.
            tl_items_in-dlvschduse    =  wl_header-vkaus.
*              IF wl_header-inco1 EQ 'FOB'." Rubenilson Pereira - 09.10.2025 #192341
            IF wl_header-inco1 EQ 'FOB' OR ( lv_param_espec EQ 'A' OR lv_param_espec EQ 'Z' )." Rubenilson Pereira - 09.10.2025 #192341
              tl_items_in-route         =  'FOB'.
            ELSE.
              CLEAR: tl_items_in-route.
            ENDIF.

            IF i_vbeln IS NOT INITIAL.
*                  TL_ITEMS_IN-USAGE_IND    = 'S'.
              tl_items_in-ship_point   = wl_itens-werks.
              tl_items_in-matfrgtgrp   = '00000001'.
              tl_items_in-ref_doc_ca   = 'M'.
              tl_items_in-refobjtype   = 'VBRK'.
              tl_items_in-refobjkey    = i_vbeln.
              tl_items_in-ref_doc      = i_vbeln.
              tl_items_in-ref_doc_it   = tl_items_in-itm_number.
            ENDIF.


            APPEND tl_items_in.

            IF wl_header-param_espec EQ 'M'.
              READ TABLE tl_preco
                        WITH KEY nro_sol_ov = wl_header-nro_sol_ov
                                 preco      = 2
*                                    PRECO      = 1
                                 posnr      = wl_itens-fixacao.
*               ##Inicio##   Editado por Welgem inserindo P para realizar o leitura da tabela com Bezei
            ELSEIF wl_header-param_espec EQ 'P'.
              READ TABLE tl_preco
                        WITH KEY nro_sol_ov = wl_header-nro_sol_ov
                                 bezei      = 'TAXA CAMBIO'.
*               ##Fim##   Editado por Welgem inserindo P para realizar o leitura da tabela com Bezei
            ELSE.

              READ TABLE tl_preco
                 WITH KEY nro_sol_ov = wl_header-nro_sol_ov
                          preco      = 2.
            ENDIF.

            CLEAR: tl_conditions_in.
            tl_conditions_in-itm_number  = tl_items_in-itm_number .
            tl_conditions_in-cond_type   = 'PR00'.
            tl_conditions_in-currency    = wl_header-waerk.
*                TL_CONDITIONS_IN-COND_VALUE  = WL_ITENS-DMBTR .
            IF wl_header_in-doc_type EQ 'ZPER'.
              wl_dmbtr = wl_itens-dmbtr.

*               ##Inicio##   Editado por Welgem Comentado a linha que Recebe WL_ITENS-KURSF para inserir TL_PRECO-FORMULA2
*                 WL_HEADER_IN-EXRATE_FI = WL_HEADER_IN-EXCHG_RATE = TL_CONDITIONS_IN-CONEXCHRAT = WL_ITENS-KURSF.
              wl_header_in-exrate_fi = wl_header_in-exchg_rate = tl_conditions_in-conexchrat = tl_preco-formula2.
*               ##Fim##   Editado por Welgem Comentado a linha que Recebe WL_ITENS-KURSF para inserir TL_PRECO-FORMULA2

              wl_header_in-price_date = wl_header_in-bill_date =  sy-datum.

            ELSEIF wl_header_in-doc_type EQ 'ZEXP'
                OR wl_header_in-doc_type EQ 'ZEXI'.

              wl_dmbtr = wl_itens-dmbtr.
              wl_header_in-exrate_fi = wl_header_in-exchg_rate = tl_conditions_in-conexchrat = wl_itens-kursf.
              wl_header_in-price_date = wl_header_in-bill_date =  sy-datum.

            ELSE.
              wl_dmbtr = tl_preco-formula2.
              CLEAR: tl_conditions_in-conexchrat.

            ENDIF.

            IF wl_header-param_espec EQ 'X'.  "atende as vendas ZFEX algodão
              wl_dmbtr = wl_itens-dmbtr.
            ENDIF.

            tl_conditions_in-cond_value  = wl_dmbtr.

            tl_conditions_in-cond_unit   = 'TO'.

            "USXXXXXXX

            IF wl_itens-pmein EQ 'STM' OR wl_itens-pmein EQ 'ST'.
              tl_conditions_in-cond_unit = wl_itens-pmein.
            ENDIF.

            SELECT 'I'     AS sign,
                   'EQ'    AS option,
                   valfrom AS low
              FROM setleaf
              INTO CORRESPONDING FIELDS OF TABLE @r_auart_ser
              WHERE setname = 'ZSDT0066_AUART_SERV'.

            IF r_auart_ser IS NOT INITIAL.  "// Auart do tipo de Serviço
              IF wl_header_in-doc_type IN r_auart_ser.
                tl_conditions_in-cond_unit = wl_itens-pmein.
              ENDIF.
            ENDIF.

*            IF wl_header_in-doc_type EQ 'ZSAN' OR
*               wl_header_in-doc_type EQ 'ZSER' OR
*               wl_header_in-doc_type EQ 'ZELV' OR
*               wl_header_in-doc_type EQ 'ZSBE'.
*              tl_conditions_in-cond_unit   = wl_itens-pmein.
*            ELSE.
*              tl_conditions_in-cond_unit   = 'TO'.
*            ENDIF.

            CASE wl_header-spart.
              WHEN '06'.
                tl_conditions_in-cond_unit   = wl_itens-pmein.
            ENDCASE.

            APPEND tl_conditions_in.

            IF wl_itens-desc_absoluto IS NOT INITIAL.
              tl_conditions_in-cond_type   = 'RB00'.
              tl_conditions_in-cond_value  = wl_itens-desc_absoluto.
              APPEND tl_conditions_in.
            ENDIF.

            CLEAR: tl_schedules_in.
            tl_schedules_in-itm_number = tl_items_in-itm_number.
            tl_schedules_in-req_qty    = wl_vlr_covert.
*          TL_SCHEDULES_IN-REQ_DLV_BL    = '10'.
*    *        tl_schedules_in-req_qty    = tl_itens-zmeng.
            APPEND tl_schedules_in.


*          DELETE TL_ITENS.
*        ENDLOOP.
            ON CHANGE OF wl_header-nro_sol_ov.
              REFRESH: tl_texto.
              DELETE tl_text_in WHERE itm_number IS NOT INITIAL.
              IF i_tipo EQ '1'.
                CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
                  EXPORTING
                    im_title = 'Texto para item da Ordem de Venda'
                  CHANGING
                    ch_text  = tl_texto.

                LOOP AT tl_texto INTO wl_texto.
                  tl_text_in-text_line(72) = wl_texto.
                  tl_text_in-text_id    = '0001'.
                  tl_text_in-itm_number    = tl_items_in-itm_number.
                  tl_text_in-langu      = sy-langu.
                  tl_text_in-format_col = '/'.
                  APPEND tl_text_in.
                ENDLOOP.
              ENDIF.
            ENDON.

            "Adicionar informações da Instrução do Item.
            IF NOT ( wl_itens-instrucao IS INITIAL ).
              CLEAR: tl_text_in-text_line(72).
              DELETE tl_text_in WHERE text_line(5) EQ 'Instr'.
              CONCATENATE 'Instrução:' wl_itens-instrucao INTO tl_text_in-text_line(72) SEPARATED BY space.
              tl_text_in-itm_number = '000000'.
              tl_text_in-text_id    = '0002'.
              tl_text_in-langu      = sy-langu.
              tl_text_in-format_col = '/'.
              APPEND tl_text_in.
            ENDIF.

*CONTAINER
            "Adicionar informações da Container do Item.
            IF NOT ( wl_itens-container IS INITIAL ).
              CLEAR: tl_text_in-text_line(72).
              DELETE tl_text_in WHERE text_line(5) EQ 'Conta'.
              CONCATENATE 'Container:' wl_itens-container INTO tl_text_in-text_line(72) SEPARATED BY space.
              tl_text_in-itm_number = '000000'.
              tl_text_in-text_id    = '0002'.
              tl_text_in-langu      = sy-langu.
              tl_text_in-format_col = '/'.
              APPEND tl_text_in.
            ENDIF.

            DATA: v_saldo TYPE vbak-netwr,
                  t_tipo  TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE.

            IF wl_itens-kunnr IS NOT INITIAL.
              wl_header-kunnr = wl_itens-kunnr.
            ENDIF.

******CS201900165
*            CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*              EXPORTING
*                CLASS         = '0000'
*                SETNR         = 'MAGGI_LIMITE_FIN'
*              TABLES
*                SET_VALUES    = T_TIPO
*              EXCEPTIONS
*                SET_NOT_FOUND = 1
*                OTHERS        = 2.
*
*            IF LINE_EXISTS( T_TIPO[ FROM = WL_HEADER-AUART ] ).
*
*              CALL FUNCTION 'Z_SALDO_CLIENTE'
*                EXPORTING
*                  V_KUNNR = WL_HEADER-KUNNR
*                  V_VKORG = WL_HEADER-VKORG
*                  V_GJAHR = SY-DATUM(4)
*                IMPORTING
*                  V_SALDO = V_SALDO.
*
*              IF V_SALDO LT 0.
*                WL_HEADER_IN-DLV_BLOCK = '01'.
*              ENDIF.
*
*              IF V_SALDO LT WL_ITENS-VLRTOT.
*                WL_HEADER_IN-DLV_BLOCK = '01'.
*              ENDIF.
*
*            ENDIF.
******CS201900165

            IF wl_header_in-doc_type EQ 'ZMIT'.

              wl_header_in-cust_grp3  = wl_itens-kvgr3.
              wl_header_in-cust_grp4  = '001'.

              wl_header_in-ztrocanota = wl_itens-ck_troca_nota.

              wl_header_inx-cust_grp3 = abap_true.
              wl_header_inx-cust_grp4 = abap_true.


            ENDIF.
            IF  wl_header_in-doc_type EQ 'ZFEX'.  "<<RIM-SKM-IR128418-23.02.23
              wl_header_in-ztrocanota = wl_itens-ck_troca_nota.  "<<RIM-SKM-IR128418-23.02.23
            ENDIF.                                      "<<RIM-SKM-IR128418-23.02.23

            wl_header_in-cust_grp5  = COND #( WHEN wl_itens-kvgr5 IS NOT INITIAL THEN wl_itens-kvgr5 ELSE '001' ).
            wl_header_inx-cust_grp5 = abap_true.

*-US 150347-13-09-2024-#150347-RJF-inicio
            DATA: lv_found TYPE c.
            IF wl_header_in-doc_type IS NOT INITIAL AND wl_header_in-purch_no_c IS NOT INITIAL.
              SELECT SINGLE vk~vbeln, vd~posnr
                FROM vbak AS vk
                INNER JOIN vbkd AS vd ON
                vd~vbeln = vk~vbeln
              INTO @DATA(wa_vbak)
             WHERE vk~auart EQ @wl_header_in-doc_type
               AND vd~bstkd EQ @wl_header_in-purch_no_c.
            ENDIF.
            FREE lv_found.
            IF wa_vbak-vbeln IS NOT INITIAL AND sy-subrc IS INITIAL.
              wl_vbeln = wa_vbak-vbeln. "&& '-' && wa_vbak-posnr.
              lv_found = abap_on.
            ELSE.
              CLEAR: wl_vbeln, wa_vbak.
*-US 150347-13-09-2024-#150347-RJF-fim

              CLEAR: tl_items_in, tl_conditions_in, tl_schedules_in.
*     Criar Ordem
              CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
                EXPORTING
                  sales_header_in      = wl_header_in
                  sales_header_inx     = wl_header_inx
                IMPORTING
                  salesdocument_ex     = wl_vbeln
                TABLES
                  return               = tl_return
                  sales_items_in       = tl_items_in
                  sales_items_inx      = tl_items_inx
                  sales_partners       = tl_partners
                  sales_schedules_in   = tl_schedules_in
                  sales_schedules_inx  = tl_schedules_inx
                  sales_conditions_in  = tl_conditions_in
                  sales_conditions_inx = tl_conditions_inx
                  sales_text           = tl_text_in
                  extensionin          = tl_bapiparex
                  incomplete_log       = tl_log_erro. "ZSDT0066 Trazer o erro correto na geração OV  - BG #122413

*-US 150347-13-09-2024-#150347-RJF-inicio
            ENDIF.
*-US 150347-13-09-2024-#150347-RJF-fim

*     Verirfica se a ordem foi criada.
*-US 150347-13-09-2024-#150347-RJF-inicio
            IF lv_found IS INITIAL.
*-US 150347-13-09-2024-#150347-RJF-fim
              IF NOT wl_vbeln IS INITIAL.
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.

                MOVE wl_vbeln TO tl_ovs-vbeln.

                APPEND tl_ovs.
                CLEAR: tl_ovs.
*    **
                REFRESH: tl_vbuv.
                SELECT *
                  FROM vbuv
                  INTO TABLE tl_vbuv
                   WHERE vbeln EQ wl_vbeln.

                IF sy-subrc IS INITIAL.
                  LOOP AT tl_vbuv.
                    CLEAR: tl_return, wl_fieldname, wl_text.
                    wl_fieldname = tl_vbuv-fdnam.

                    CALL FUNCTION 'RM_DDIC_TEXTS_GET'
                      EXPORTING
                        i_name                = wl_fieldname
                        i_type                = 'DTEL'
                        i_langu               = sy-langu
                      IMPORTING
                        e_ddtxt               = wl_text
                      EXCEPTIONS
                        objtype_not_supported = 1
                        illegal_input         = 2
                        OTHERS                = 3.
                    IF sy-subrc <> 0.
                      CONCATENATE 'Existem campos incompletos na OV:' tl_vbuv-fdnam INTO tl_return-message SEPARATED BY space.

                    ELSE.

                      CONCATENATE 'Existem campos incompletos na OV:' wl_text INTO tl_return-message SEPARATED BY space.
                    ENDIF.
                    tl_return-type = 'E'.
                    APPEND tl_return.
                  ENDLOOP.
                  REFRESH: tl_return_aux.
                  wl_header_inx2-updateflag = 'D'.
                  CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
                    EXPORTING
                      salesdocument    = wl_vbeln
*                     ORDER_HEADER_IN  = WL_ORDERHEADERIN
                      order_header_inx = wl_header_inx2
                    TABLES
                      return           = tl_return_aux.
                  READ TABLE tl_return_aux  WITH KEY type = 'E'.

                  IF sy-subrc NE 0.
                    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                      EXPORTING
                        wait = 'X'.
                  ENDIF.
                  CLEAR: wl_vbeln.

                ELSE.
*                    CLEAR: WL_NETWR.
*                    SELECT SINGLE NETWR
*                      FROM VBAK
*                      INTO WL_NETWR
*                       WHERE VBELN EQ WL_VBELN.
*
*                    IF WL_NETWR NE WL_ITENS-VLRTOT.
*                      WL_HEADER_INX2-UPDATEFLAG = 'U'.
*
*                      WL_VLRTOT = WL_ITENS-VLRTOT - WL_NETWR.
*                      READ TABLE TL_CONDITIONS_IN INDEX 1.
*                      TL_CONDITIONS_IN-COND_TYPE   = 'RB00'.
*                      TL_CONDITIONS_INX-COND_TYPE = 'X'.
*                      TL_CONDITIONS_IN-COND_VALUE  = WL_VLRTOT.
*                      TL_CONDITIONS_INX-COND_VALUE = 'X'.
*                      TL_CONDITIONS_INX-CURRENCY = 'X'.
*                      TL_CONDITIONS_INX-ITM_NUMBER = 'X'.
*                      TL_CONDITIONS_INX-COND_UNIT = 'X'.
*
*                      APPEND TL_CONDITIONS_IN.
*                      APPEND TL_CONDITIONS_INX.
*
*                      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
*                        EXPORTING
*                          SALESDOCUMENT        = WL_VBELN
*                          ORDER_HEADER_INX     = WL_HEADER_INX2
*                        TABLES
*                          CONDITIONS_IN        = TL_CONDITIONS_IN
*                          CONDITIONS_INX       = TL_CONDITIONS_INX
*                          RETURN               = TL_RETURN.
*
*                    ENDIF.

                  MOVE wl_vbeln TO wl_itens-vbeln.
                  IF wl_header_in-dlv_block IS NOT INITIAL.
                    wl_itens-status = 'B'.
                  ENDIF.
                  MODIFY tl_itens FROM wl_itens.
                  MODIFY zsdt0053 FROM wl_itens.
*** INICIO - STEFANINI - FT - 2000015830 - IR192047
                  COMMIT WORK AND WAIT.
*** FIM - STEFANINI - FT - 2000015830 - IR192047
                ENDIF.
              ELSE.
                APPEND LINES OF tl_return TO te_return.

              ENDIF.
*-US 150347-13-09-2024-#150347-RJF-inicio
            ELSE.
              MOVE wl_vbeln TO wl_itens-vbeln.
              MODIFY tl_itens FROM wl_itens.
              MODIFY zsdt0053 FROM wl_itens.
              COMMIT WORK AND WAIT.
            ENDIF.
*-US 150347-13-09-2024-#150347-RJF-fim
            CLEAR: tl_saida_exec.
            MOVE: wl_itens-nro_sol_ov  TO tl_saida_exec-nro_sol_ov,
                  wl_itens-posnr       TO tl_saida_exec-posnr,
                  wl_itens-zmeng       TO tl_saida_exec-zmeng,
                  wl_itens-valdt       TO tl_saida_exec-valdt,
                  wl_itens-vlrtot      TO tl_saida_exec-vlrtot,
                  wl_vbeln             TO tl_saida_exec-vbeln.

            IF wl_vbeln IS NOT INITIAL.
              APPEND tl_saida_exec.
            ELSE.
              LOOP AT tl_return WHERE type EQ 'E'.
                MOVE: tl_return-message TO tl_saida_exec-msg.
                APPEND tl_saida_exec.
              ENDLOOP.
              "ZSDT0066 Trazer o erro correto na geração OV  - BG #122413 - INICIO
              LOOP AT tl_log_erro .
                MOVE: tl_log_erro-table_name TO tl_saida_exec-msg.
                APPEND tl_saida_exec.
              ENDLOOP.
              "ZSDT0066 Trazer o erro correto na geração OV  - BG #122413 - FIM
            ENDIF.

          ENDLOOP.
        ENDLOOP.

        IF tl_ovs[] IS NOT INITIAL.
          SELECT *
            FROM vbak
            INTO TABLE te_vbak
             FOR ALL ENTRIES IN tl_ovs
              WHERE vbeln EQ tl_ovs-vbeln.

          IF sy-subrc IS INITIAL.
            SELECT *
              FROM vbap
              INTO TABLE te_vbap
               FOR ALL ENTRIES IN tl_ovs
                WHERE vbeln EQ tl_ovs-vbeln.
          ENDIF.
        ENDIF.


      ENDIF.
    ELSE.
      RAISE solicitacao_nao_existe.
    ENDIF.
  ENDIF.

  IF ti_nro_sol_ov[] IS NOT INITIAL.
    CALL FUNCTION 'ZSDMF004_GERA_ADIANTAMENTO'
      TABLES
        ti_nro_sol_ov = ti_nro_sol_ov
        te_return     = tl_saida_exec.

  ENDIF.

*-CS2023000189-26.04.2023-#108710-JT-inicio
*----------------------------------
* envia OVS ao trace cotton
*----------------------------------
  IF tl_saida_exec[] IS NOT INITIAL.
    tl_saida_exec_aux[] = tl_saida_exec[].

    SORT tl_saida_exec_aux BY nro_sol_ov posnr.
    DELETE ADJACENT DUPLICATES FROM tl_saida_exec_aux
                          COMPARING nro_sol_ov posnr.

    LOOP AT tl_saida_exec_aux INTO DATA(wl_saida) WHERE vbeln IS NOT INITIAL.
      DATA(l_task) = 'TRACE_ORDEM_VENDA' && wl_saida-nro_sol_ov && wl_saida-posnr.

      CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE' STARTING NEW TASK l_task
        EXPORTING
          i_nro_sol_ov = wl_saida-nro_sol_ov
          i_posnr      = wl_saida-posnr
          i_acao       = 'C'
        EXCEPTIONS
          OTHERS       = 1.
    ENDLOOP.
  ENDIF.
*-CS2023000189-26.04.2023-#108710-JT-fim

  IF tl_saida_exec[] IS NOT INITIAL.
    SELECT *
      FROM zmail
      INTO TABLE tl_zmail
       FOR ALL ENTRIES IN tl_itens
       WHERE werks EQ tl_itens-werks
         AND tcode EQ sy-tcode.

**  Enviar e-mail para usuário que criou a solicitação
    SELECT DISTINCT zsdt0051~vkorg zsdt0060~email
      FROM zsdt0051
     INNER JOIN zsdt0060 ON zsdt0051~vkgrp = zsdt0060~vkgrp AND zsdt0051~vkbur EQ zsdt0060~vkbur
      INTO (lv_vkorg, lv_email)
       FOR ALL ENTRIES IN ti_nro_sol_ov
     WHERE nro_sol_ov =  ti_nro_sol_ov-nro_sol_ov
       AND email      <> ''.

      CLEAR tl_zmail.
      tl_zmail-bukrs   = lv_vkorg.
      tl_zmail-email   = lv_email.
      tl_zmail-tcode   = sy-tcode.
      tl_zmail-usuario = sy-uname.
      APPEND tl_zmail.

    ENDSELECT.

    LOOP AT tl_saida_exec
       WHERE vbeln IS NOT INITIAL.
      tl_nro_sol_ov-nro_sol_ov = tl_saida_exec-nro_sol_ov.

      COLLECT tl_nro_sol_ov.
    ENDLOOP.

    LOOP AT tl_itens
       WHERE vbeln IS NOT INITIAL.

*Refresh Tables
      REFRESH : lt_obj_cont.

      READ TABLE tl_header
       WITH KEY nro_sol_ov = tl_itens-nro_sol_ov.

      READ TABLE tl_tvkot
         WITH KEY vkorg = tl_header-vkorg.

      READ TABLE tl_tvgrt
         WITH KEY vkgrp = tl_header-vkgrp.

      READ TABLE tl_kna1
        WITH KEY kunnr = tl_header-kunnr.

      READ TABLE tl_t001w
        WITH KEY werks = tl_itens-werks.

      add_html:
      '<!DOCTYPE html>',
      '<html>',
      '<head>',
      '<title>Liberacao de documento</title>',
      '<style type="text/css">',
      'body,td,th {',
      '  font-family: "Arial", Times, serif; }',
      '</style>',
      '</head>',
    '<p><img src= "cid:img1.gif"></p>',
******Header do HTML
      '<table width="700" height="361" border="1">',
      '  <tr>',
      '    <td width="801" valign="top"><table width="800" border="0">',
      '      <tr>',
      '       <td width="173">Ordem de venda</td>',
      '        <td width="166">&nbsp;</td>',
      '        <td width="135">Data Venda</td>',
      '        <td width="160">Tipo de Negócio</td>',
      '      </tr>',
      '      <tr>'.
      CONCATENATE '          <td>' tl_itens-vbeln '</td>' INTO wl_field.
      CONDENSE wl_field NO-GAPS.
      add_html: wl_field,
  '       <td>&nbsp;</td>'.
*      CONCATENATE '          <td>' '</td>' INTO WL_FIELD.
*      CONDENSE WL_FIELD NO-GAPS.
*      ADD_HTML: WL_FIELD.
*  '        <td>#AUART#</td>',
      WRITE wl_header-data_atual TO wl_data.
      CONCATENATE '          <td>' wl_data '</td>' INTO wl_field.
      CONDENSE wl_field NO-GAPS.
      add_html: wl_field.
*  '        <td>#DATA_ATUAL#</td>',
      CONCATENATE '          <td>' wl_header-inco1 ' - ' wl_header-inco2 '</td>' INTO wl_field.
      CONDENSE wl_field NO-GAPS.
      add_html: wl_field,
*  '        <td>#INCO1#</td>',
      '     </tr>',
      '    </table>',
*      '      <br />',
*      '      <table width="800" border="0">',
*      '        <tr>',
*      '          <td width="260">Organização Vendas</td>',
*      '          <td width="262">Filial</td>',
*      '          <td width="264">Vendedor</td>',
*      '        </tr>',
*      '        <tr>'.
*      CONCATENATE '          <td width="260">' WL_HEADER-VKORG '-' TL_TVKOT-VTEXT '</td>' INTO WL_FIELD.
**  CONDENSE WL_FIELD NO-GAPS.
*      ADD_HTML: WL_FIELD.
**  '          <td>#VKORG#</td>',
*      READ TABLE TL_ITENS INDEX 1.
*      READ TABLE TL_T001W
*        WITH KEY WERKS = TL_ITENS-WERKS.
*
*      CONCATENATE '          <td width="262">' TL_ITENS-WERKS '-' TL_T001W-NAME1 '</td>' INTO WL_FIELD.
**  CONDENSE WL_FIELD NO-GAPS.
*      ADD_HTML: WL_FIELD.
**  '          <td>#WERKS#</td>'.
*      CONCATENATE '          <td width="264">' WL_HEADER-VKGRP '-' TL_TVGRT-BEZEI '</td>' INTO WL_FIELD.
**  CONDENSE WL_FIELD NO-GAPS.
*      ADD_HTML: WL_FIELD,
**  '          <td>#VKGRP#</td>',
*      '        </tr>',
*      '      </table>',
      '      <br />',
      '      <table width="800" border="0">',
      '        <tr>',
      '         <td width="426">Cliente</td>',
      '          <td width="286">Local</td>',
      '          <td width="88">UF</td>',
      '        </tr>',
      '        <tr>'.
      CONCATENATE '          <td width="426">' wl_header-kunnr '-' tl_kna1-name1 '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
      add_html: wl_field.
*'          <td>#KUNNR#</td>',
      CONCATENATE '          <td width="286">' tl_kna1-ort01 '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
      add_html: wl_field.
*'          <td width="286">#LOCAL#</td>',
      CONCATENATE '          <td width="286">' tl_kna1-regio '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
      add_html: wl_field,
*'          <td width="88">#UF#</td>',
    '        </tr>',
    '      </table>',
    '      <br />',
    '      <table width="800" border="0">',
      '        <tr>',
      '         <td width="426">Organização de Vendas</td>',
      '          <td width="286">Local</td>',
      '          <td width="88">UF</td>',
      '        </tr>',
      '        <tr>'.
      CONCATENATE '          <td width="426">' tl_itens-werks '-' tl_t001w-name1 '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
      add_html: wl_field.
*'          <td>#KUNNR#</td>',
      CONCATENATE '          <td width="286">' tl_t001w-ort01 '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
      add_html: wl_field.
*'          <td width="286">#LOCAL#</td>',
      CONCATENATE '          <td width="286">' tl_t001w-regio '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
      add_html: wl_field,
*'          <td width="88">#UF#</td>',
    '        </tr>',
    '      </table>',
    '      <br />', """"
    '      <table width="800" border="0">',
    '        <tr>',
    '          <td width="486">Produto</td>',
    '          <td width="214" align="center">Quatidade</td>',
    '          <td width="100"> </td>',
    '        </tr>',
    '        <tr>'.

*      LOOP AT TL_ITENS.
      READ TABLE tl_makt
        WITH KEY matnr = tl_itens-matnr.
*---> 09/06/2023 - Migração S4 - JS
*       wl_matnr = tl_itens-matnr.
      wl_matnr = CONV #( tl_itens-matnr ).
*<--- 09/06/2023 - Migração S4 - JS

      SHIFT wl_matnr LEFT DELETING LEADING '0'.
      CONCATENATE '          <td width="486">' wl_matnr ' - ' tl_makt-maktx '</td>' INTO wl_field.
*    CONDENSE WL_FIELD NO-GAPS.
      add_html: wl_field.
      WRITE tl_itens-zmeng TO wl_qtd.
      CONDENSE wl_qtd NO-GAPS.
      CONCATENATE '          <td width="214" align="right">' wl_qtd '</td>' INTO wl_field.
      add_html: wl_field.
*    '          <td align="right">&nbsp;</td>',
      CONCATENATE '          <td width="100">' tl_itens-zieme '</td>' INTO wl_field.
*    CONDENSE WL_FIELD NO-GAPS.
      add_html: wl_field,
*    '          <td>&nbsp;</td>',
      '        </tr>  '.
*      ENDLOOP.
      add_html:
      '      </table>',
      '      <br />',
      '      <table width="800" border="0">',
      '        <tr>',
      '          <td width="787">Observações:</td>',
      '        </tr>',
      '        <tr>'.
      CONCATENATE '          <td>' wl_header-observacao '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
      add_html: wl_field,
*    '          <td>&nbsp;</td>',
      '        </tr>',
      '      </table>',
      '      <br />',
      '      <table width="800" border="0">',
      '        <tr>',
      '          <td colspan="2">Periodo Embarque</td>',
      '          <td width="164">&nbsp;</td>',
      '          <td width="371">&nbsp;</td>',
      '        </tr>'.

      IF tl_header-inco1 EQ 'FOB'.
        add_html:
        '        <tr>',
        '          <td width="70">Inicial:</td>'.
        WRITE wl_header-dtde_logist TO wl_data..
        CONCATENATE '          <td width="192">' wl_data '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
        add_html: wl_field,
*    '          <td width="127">&nbsp;</td>',
        '          <td width="60">Final:</td>'.
        WRITE wl_header-dtate_logist TO wl_data.
        CONCATENATE '          <td width="460">' wl_data '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
        add_html: wl_field,
**    '          <td wid th="545">&nbsp;</td>',
        '        </tr>'.
      ELSEIF tl_header-inco1 EQ 'CIF'.
        add_html:'<tr>',
                      '<td width="70">Data</td>',
                     ' <td width="192" align="center">Quantidade</td>',
                      '<td width="60">UM</td>',
                      '<td>&nbsp;</td>',
                    '</tr>'.

        LOOP AT tl_logistica.
          WRITE tl_logistica-data_progr TO wl_data.
          add_html:'<tr>'.
          CONCATENATE '<td width="70">' wl_data '</td>' INTO wl_field.
          add_html: wl_field.
*                   '<td width="70">Data</td>',
          WRITE tl_logistica-cadencia_qte TO wl_qtd.
          CONCATENATE '<td width="192" align="right">' wl_qtd '</td>' INTO wl_field.
          add_html: wl_field.
*                     '<td width="192" align="right">Quantidade</td>',
          CONCATENATE '<td width="60">' tl_logistica-zieme '</td>' INTO wl_field.
          add_html: wl_field,
*                     '<td width="60">UM</td>',
                     '<td>&nbsp;</td>',
                     '</tr>'.
        ENDLOOP.
      ENDIF.
      add_html:
      '    </table>',
      '    <br /></td>',
      '  </tr>',
      '</table>',
      '<p>&nbsp;</p>',
      '</body>',
      '</html>'.

      CONCATENATE 'Ordem de Venda Nº' tl_itens-vbeln
      INTO wl_assunto SEPARATED BY space.
      LOOP AT tl_zmail WHERE bukrs EQ tl_header-vkorg
                         AND ( werks EQ tl_itens-werks OR
                               werks IS INITIAL ).
        PERFORM envia_email_html IN PROGRAM zsdr0022 TABLES lt_obj_cont
                                            USING tl_zmail-email
                                                  wl_assunto.
      ENDLOOP.
    ENDLOOP.



    IF i_tipo EQ '1'.
      PERFORM montar_layout USING 'TL_SAIDA_EXEC'.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          it_fieldcat           = estrutura[]
          i_save                = 'A'
          i_screen_start_column = 3
          i_screen_start_line   = 3
          i_screen_end_column   = 100
          i_screen_end_line     = 13
        TABLES
          t_outtab              = tl_saida_exec.
    ELSE.
      te_saida_exec[] = tl_saida_exec[].
    ENDIF.
  ENDIF.
ENDFUNCTION.
