FUNCTION zsdmf005_modif_status_item_sol.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_STATUS) TYPE  ZSDT0053-STATUS
*"     REFERENCE(I_TIPO) TYPE  CHAR1 DEFAULT '1'
*"     REFERENCE(I_TIPO_SOL) TYPE  CHAR2 DEFAULT 'VN'
*"     REFERENCE(I_SYUCOMM) TYPE  SYUCOMM OPTIONAL
*"  TABLES
*"      TI_NRO_SOL_OV STRUCTURE  ZSDS007
*"      TE_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      TE_SAIDA_EXEC STRUCTURE  ZSDS010 OPTIONAL
*"      TE_VBAK STRUCTURE  VBAK OPTIONAL
*"      TE_VBAP STRUCTURE  VBAP OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_bsid,
           belnr TYPE bsid-belnr,
           vbel2 TYPE bsid-vbel2,
         END OF ty_bsid ,

         BEGIN OF ty_bkpf,
           belnr     TYPE bkpf-belnr,
           xreversal TYPE bkpf-xreversal,
         END OF ty_bkpf,

         BEGIN OF ty_ovs,
           bukrs TYPE bsid-bukrs,
           vbeln TYPE bsid-vbeln,
         END OF ty_ovs.


  DATA: tl_0053         TYPE TABLE OF zsdt0053 WITH HEADER LINE.
  DATA: lw_0053         LIKE LINE OF tl_0053.

  DATA: tl_0066         TYPE TABLE OF zsdt0066 WITH HEADER LINE.
  DATA: tl_0051 TYPE TABLE OF zsdt0051 WITH HEADER LINE,
        lw_0051 LIKE LINE OF tl_0051.


  DATA: tl_0052         TYPE TABLE OF zsdt0052 WITH HEADER LINE.
  DATA: tl_0054         TYPE TABLE OF zsdt0054 WITH HEADER LINE.
  DATA: tl_0059         TYPE TABLE OF zsdt0059.
  DATA: wl_0059         TYPE zsdt0059.
  DATA: wl_0059_aux1    TYPE zsdt0059.
  DATA: wl_0059_aux     TYPE zsdt0059.
  DATA: lw_0059_read    TYPE zsdt0059.
  DATA: tl_bsid         TYPE TABLE OF ty_bsid WITH HEADER LINE.
  DATA: tl_bkpf         TYPE TABLE OF ty_bkpf WITH HEADER LINE.
  DATA: tl_ovs          TYPE TABLE OF ty_ovs WITH HEADER LINE.
  DATA: tl_vbak            TYPE TABLE OF vbak WITH HEADER LINE,
        tl_vbap            TYPE TABLE OF vbap WITH HEADER LINE,
        tl_vbep            TYPE TABLE OF vbep WITH HEADER LINE,
        wl_orderheaderin   TYPE bapisdh1,
        wl_orderheaderinx  TYPE bapisdh1x,
        tl_bapisditm       TYPE TABLE OF bapisditm WITH HEADER LINE,
        tl_bapisditmx      TYPE TABLE OF bapisditmx WITH HEADER LINE,
        tl_return          TYPE TABLE OF bapiret2 WITH HEADER LINE,
        wl_return          TYPE bapiret2,
        tl_schedule_lines  TYPE TABLE OF bapischdl WITH HEADER LINE,
        tl_schedule_linesx TYPE TABLE OF bapischdlx WITH HEADER LINE,
        tl_saida_exec      TYPE TABLE OF ty_saida_exec WITH HEADER LINE,
        tl_saida_exec_aux  TYPE TABLE OF ty_saida_exec WITH HEADER LINE,
        wl_tabix           TYPE sy-tabix,
        wl_tabix_aux       TYPE sy-tabix,
        wl_matnr           TYPE mara-matnr,
        wl_zieme           TYPE mara-meins,
        wl_pmein           TYPE mara-meins,
        wl_menge           TYPE ekpo-menge.

  DATA: lt_zsdt0056 TYPE TABLE OF zsdt0056,
        lw_zsdt0056 TYPE zsdt0056.

  DATA: lt_zsdt0051 TYPE TABLE OF zsdt0051,
        lw_zsdt0051 TYPE zsdt0051.

  DATA: r_bezei      TYPE RANGE OF zsdt0056-bezei,
        r_bezei_line LIKE LINE OF r_bezei.
  DATA: w_bezei      TYPE RANGE OF zsdt0056-bezei,
        w_bezei_line LIKE LINE OF w_bezei.
  DATA: p_bezei      TYPE RANGE OF zsdt0056-bezei,
        p_bezei_line LIKE LINE OF p_bezei.
  DATA: c_bezei      TYPE RANGE OF zsdt0056-bezei,
        c_bezei_line LIKE LINE OF c_bezei.
  " 29.10.2024 - 147331 - RAMON -->
  DATA: b_bezei      TYPE RANGE OF zsdt0056-bezei,
        b_bezei_line LIKE LINE OF c_bezei.
  " 29.10.2024 - 147331 - RAMON --<
  DATA: var_len           TYPE i.
  DATA: wl_premio         TYPE dmbtr.
  DATA: formula_c         TYPE dmbtr.
  DATA: formula_p         TYPE dmbtr.

  DATA: formula_b         TYPE dmbtr." 29.10.2024 - 147331 - RAMON --
  DATA: formula_r         TYPE p LENGTH 13 DECIMALS 5.

  DATA: var_saldo_negativo TYPE zsdt0053-zmeng.
  DATA: true_taxa TYPE c,
        true_qtd  TYPE c.
  DATA: wl_setleaf         TYPE setleaf.


  DATA: var_msg                       TYPE string.
  DATA: cx_exception                  TYPE REF TO zcx_webservice.
  DATA: obj_zcl_webservice_taxa_curva TYPE REF TO zcl_webservice_tx_curva.

  DATA: lv_resp TYPE c.  "*-IR 189210-14.01.2025-#163685-JT

  REFRESH: tl_vbak, tl_vbap, tl_vbep, tl_saida_exec.
  REFRESH: tl_0053, tg_saldo, tl_0054, tl_0052, tl_bsid, tl_bkpf, tl_ovs, tl_0051.
  IF ti_nro_sol_ov[] IS NOT INITIAL.



    IF i_tipo_sol EQ 'VN'.
      SELECT *
        FROM zsdt0053
        INTO TABLE tl_0053
         FOR ALL ENTRIES IN ti_nro_sol_ov
          WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov
            AND posnr      EQ ti_nro_sol_ov-posnr
            AND vbeln      NE space.
    ELSEIF i_tipo_sol EQ 'FL'.
      SELECT *
        FROM zsdt0066
        INTO CORRESPONDING FIELDS OF TABLE tl_0053
         FOR ALL ENTRIES IN ti_nro_sol_ov
          WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov
            AND posnr      EQ ti_nro_sol_ov-posnr
            AND vbeln      NE space.
    ENDIF.

    SELECT *
      FROM zsdt0052
      INTO TABLE tl_0052
       FOR ALL ENTRIES IN ti_nro_sol_ov
        WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.



    SELECT * FROM zsdt0056 INTO TABLE lt_zsdt0056.

    IF ( sy-subrc EQ 0 ).

      r_bezei_line-sign   = 'I'.
      r_bezei_line-option = 'EQ'.
      r_bezei_line-low    = 'TAXA CAMBIO FRAME'.
      r_bezei_line-high   = 'TAXA CAMBIO FRAME'.
      APPEND r_bezei_line TO r_bezei.
      APPEND r_bezei_line TO w_bezei.

      p_bezei_line-sign   = 'I'.
      p_bezei_line-option = 'EQ'.
      p_bezei_line-low    = 'PREMIO FRAME'.
      p_bezei_line-high   = 'PREMIO FRAME'.
      APPEND p_bezei_line TO p_bezei.
      APPEND p_bezei_line TO w_bezei.

      c_bezei_line-sign   = 'I'.
      c_bezei_line-option = 'EQ'.
      c_bezei_line-low    = 'CHICAGO FRAME'.
      c_bezei_line-high   = 'CHICAGO FRAME'.
      APPEND c_bezei_line TO c_bezei.
      APPEND c_bezei_line TO w_bezei.

      " 29.10.2024 - 147331 - RAMON -->
      b_bezei_line-sign   = 'I'.
      b_bezei_line-option = 'EQ'.
      b_bezei_line-low    = 'BM&F FRAME'.
      b_bezei_line-high   = 'BM&F FRAME'.
      APPEND b_bezei_line TO b_bezei.
      APPEND b_bezei_line TO w_bezei.
      " 29.10.2024 - 147331 - RAMON --<

      LOOP AT lt_zsdt0056 INTO lw_zsdt0056.

        var_len  = strlen( lw_zsdt0056-bezei ).

        CASE var_len.
          WHEN: '2' OR '3'.
            IF ( lw_zsdt0056-bezei(1) EQ 'T' ).
              CLEAR: r_bezei_line.

              r_bezei_line-sign   = 'I'.
              r_bezei_line-option = 'EQ'.
              r_bezei_line-low    = lw_zsdt0056-bezei.
              r_bezei_line-high   = lw_zsdt0056-bezei.
              APPEND r_bezei_line TO r_bezei.
              APPEND r_bezei_line TO w_bezei.

            ENDIF.

            IF ( lw_zsdt0056-bezei(1) EQ 'C' ).
              CLEAR: c_bezei_line.

              c_bezei_line-sign   = 'I'.
              c_bezei_line-option = 'EQ'.
              c_bezei_line-low    = lw_zsdt0056-bezei.
              c_bezei_line-high   = lw_zsdt0056-bezei.
              APPEND c_bezei_line TO c_bezei.
              APPEND c_bezei_line TO w_bezei.

            ENDIF.

            " 29.10.2024 - 147331 - RAMON -->
            IF ( lw_zsdt0056-bezei(1) EQ 'B' ).
              CLEAR: b_bezei_line.

              b_bezei_line-sign   = 'I'.
              b_bezei_line-option = 'EQ'.
              b_bezei_line-low    = lw_zsdt0056-bezei.
              b_bezei_line-high   = lw_zsdt0056-bezei.
              APPEND b_bezei_line TO b_bezei.
              APPEND b_bezei_line TO w_bezei.

            ENDIF.
            " 29.10.2024 - 147331 - RAMON --<

            IF ( lw_zsdt0056-bezei(1) EQ 'P' ).
              CLEAR: p_bezei_line.

              p_bezei_line-sign   = 'I'.
              p_bezei_line-option = 'EQ'.
              p_bezei_line-low    = lw_zsdt0056-bezei.
              p_bezei_line-high   = lw_zsdt0056-bezei.
              APPEND p_bezei_line TO p_bezei.
              APPEND p_bezei_line TO w_bezei.

            ENDIF.
          WHEN OTHERS.
            CLEAR: lw_zsdt0056, var_len.
            CONTINUE.
        ENDCASE.
        CLEAR: lw_zsdt0056, var_len.
      ENDLOOP.
    ENDIF.

    SELECT *
      FROM zsdt0059
      INTO TABLE tl_0059
      FOR ALL ENTRIES IN tl_0053
    WHERE nro_sol_ov EQ tl_0053-nro_sol_ov
      AND posnr      EQ tl_0053-fixacao
      AND bezei      IN w_bezei.

    IF i_status EQ 'X'.
      IF tl_0053[] IS NOT INITIAL.
        SELECT *
        FROM zsdt0051
        INTO TABLE tl_0051
         FOR ALL ENTRIES IN tl_0053
          WHERE nro_sol_ov EQ tl_0053-nro_sol_ov.

        LOOP AT tl_0053.
          READ TABLE tl_0051
            WITH KEY nro_sol_ov = tl_0053-nro_sol_ov.

          IF sy-subrc IS INITIAL.
            MOVE: tl_0053-vbeln TO tl_ovs-vbeln,
                  tl_0051-vkorg TO tl_ovs-bukrs.
            APPEND tl_ovs.

          ENDIF.
          CLEAR: tl_ovs, tl_0051.
        ENDLOOP.

        IF tl_ovs[] IS NOT INITIAL.
          SELECT belnr vbel2
            FROM bsid
            INTO TABLE tl_bsid
             FOR ALL ENTRIES IN tl_ovs
             WHERE bukrs EQ tl_ovs-bukrs
               AND vbel2 EQ tl_ovs-vbeln.

          IF sy-subrc IS INITIAL.
            SELECT belnr xreversal
              FROM bkpf
              INTO TABLE tl_bkpf
               FOR ALL ENTRIES IN tl_bsid
               WHERE belnr EQ tl_bsid-belnr.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    SELECT *
      FROM zsdt0054
      INTO TABLE tl_0054
       FOR ALL ENTRIES IN ti_nro_sol_ov
       WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.
  ENDIF.

  CASE i_status.
    WHEN 'E'. "Encerramento da ordem de venda

      PERFORM busca_saldo TABLES tl_0053
                          USING i_tipo_sol.
      DELETE tg_saldo WHERE vbeln IS INITIAL.
      IF tg_saldo[] IS NOT INITIAL.
        LOOP AT tg_saldo INTO wg_saldo.
          READ TABLE tl_0053
            WITH KEY vbeln      = wg_saldo-vbeln
                     nro_sol_ov = wg_saldo-nro_sol_ov.
          IF sy-subrc IS INITIAL.
            tl_0053-zmeng = wg_saldo-total.
            MODIFY tl_0053 INDEX sy-tabix.
          ENDIF.
          CLEAR: tl_0053.
        ENDLOOP.
        CALL FUNCTION 'ZSDMF002_ATUALI_OV_SOLICITACAO'
          EXPORTING
            i_tipo        = i_tipo
          TABLES
            ti_itens_ov   = tl_0053
            te_saida_exec = tl_saida_exec.

        LOOP AT tl_0053.
          tl_0053-status = 'E'.
*          TL_0053-VLRTOT = TL_0053-ZMENG * TL_0053-DMBTR.
          wl_matnr = tl_0053-matnr.
          wl_zieme = tl_0053-zieme.
          wl_pmein = tl_0053-pmein.
          wl_menge = tl_0053-zmeng.

          CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
            EXPORTING
              i_matnr              = wl_matnr
              i_in_me              = wl_zieme
              i_out_me             = wl_pmein
              i_menge              = wl_menge
            IMPORTING
              e_menge              = wl_menge
            EXCEPTIONS
              error_in_application = 1
              error                = 2
              OTHERS               = 3.


          IF sy-subrc EQ 0.
*          IF WG_HEADER-PARAM_ESPEC NE C_P.
            tl_0053-vlrtot = wl_menge * tl_0053-dmbtr.
*          ELSE.
*            TRY.
*                WL_ITENS-DMBTR = WL_ITENS-VLRTOT / WL_MENGE.
*              CATCH CX_SY_ZERODIVIDE.
*            ENDTRY.
*          ENDIF.
          ENDIF.

          IF i_tipo_sol EQ 'VN'.

            UPDATE zsdt0051 SET job = abap_false
                                usnam = sy-uname
                                data_atual = sy-datum
                                hora_atual = sy-uzeit
              WHERE nro_sol_ov EQ tl_0053-nro_sol_ov.

            UPDATE zsdt0053 SET status = 'E'
                                job    = 'E'
                                zmeng  = tl_0053-zmeng
                                vlrtot = tl_0053-vlrtot
              WHERE nro_sol_ov EQ tl_0053-nro_sol_ov
                AND posnr      EQ tl_0053-posnr
                AND vbeln      EQ tl_0053-vbeln.
          ELSEIF i_tipo_sol EQ 'FL'.
            UPDATE zsdt0066 SET status_form = 'E'
                                zmeng = tl_0053-zmeng
                                vlrtot = tl_0053-vlrtot
              WHERE nro_sol_ov EQ tl_0053-nro_sol_ov
                AND posnr      EQ tl_0053-posnr
                AND vbeln      EQ tl_0053-vbeln.
          ENDIF.
        ENDLOOP.
      ENDIF.

    WHEN 'B'. "Bloqueio
      SELECT *
        FROM vbak
        INTO TABLE tl_vbak
         FOR ALL ENTRIES IN tl_0053
         WHERE vbeln EQ tl_0053-vbeln.
      IF sy-subrc IS INITIAL.

        LOOP AT tl_0053.
          IF tl_0053-status NE 'E'.

            READ TABLE tl_0054
             WITH KEY nro_sol_ov = tl_0053-nro_sol_ov
                      posnr      = tl_0053-posnr.

            IF sy-subrc IS INITIAL.
              wl_orderheaderin-dlv_block   = '11'.
            ELSE.
              wl_orderheaderin-dlv_block   = '10'.
            ENDIF.
            wl_orderheaderinx-updateflag = 'U'.
            wl_orderheaderinx-dlv_block   = 'X'.

            "*---> 28/06/2023 - Migração S4 - LO --> Material não foi utilizado
            CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
              EXPORTING
                salesdocument    = tl_0053-vbeln
                order_header_in  = wl_orderheaderin
                order_header_inx = wl_orderheaderinx
              TABLES
*               ORDER_ITEM_IN    = TL_BAPISDITM
*               ORDER_ITEM_INX   = TL_BAPISDITMX
*               SCHEDULE_LINES   = TL_SCHEDULE_LINES
*               SCHEDULE_LINESX  = TL_SCHEDULE_LINESX
                return           = tl_return.

            CLEAR:wl_return.
            READ TABLE tl_return INTO wl_return WITH KEY type = 'E'.

            IF sy-subrc NE 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              IF i_tipo_sol EQ 'VN'.
                UPDATE zsdt0053 SET status = 'B'
                  WHERE nro_sol_ov EQ tl_0053-nro_sol_ov
                    AND posnr      EQ tl_0053-posnr
                    AND vbeln      EQ tl_0053-vbeln.
              ELSEIF i_tipo_sol EQ 'FL'.
                UPDATE zsdt0066 SET status_form = 'B'
                   WHERE nro_sol_ov EQ tl_0053-nro_sol_ov
                     AND posnr      EQ tl_0053-posnr
                     AND vbeln      EQ tl_0053-vbeln.
              ENDIF.
            ELSE.
*            DELETE TL_0053 FROM WL_TABIX.
            ENDIF.
            CLEAR: tl_saida_exec.

          ENDIF.

          MOVE: tl_0053-nro_sol_ov  TO tl_saida_exec-nro_sol_ov,
                tl_0053-posnr       TO tl_saida_exec-posnr,
                tl_0053-zmeng       TO tl_saida_exec-zmeng,
                tl_0053-valdt       TO tl_saida_exec-valdt,
                tl_0053-vlrtot      TO tl_saida_exec-vlrtot,
                tl_0053-vbeln       TO tl_saida_exec-vbeln.

          IF tl_0053-status NE 'E'.
            IF wl_return IS INITIAL.
              APPEND tl_saida_exec.
            ELSE.
              LOOP AT tl_return WHERE type EQ 'E'.
                MOVE: tl_return-message TO tl_saida_exec-msg.
                APPEND tl_saida_exec.
              ENDLOOP.
            ENDIF.
          ELSE.
            tl_saida_exec-msg = 'A O.V. não pode ser bloqueada, pois já está encerrada!'.
            APPEND tl_saida_exec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    WHEN 'D'. "Desbloqueio
      SELECT *
         FROM vbak
         INTO TABLE tl_vbak
          FOR ALL ENTRIES IN tl_0053
          WHERE vbeln EQ tl_0053-vbeln.
      IF sy-subrc IS INITIAL.
        wl_orderheaderinx-updateflag = 'U'.
        wl_orderheaderin-dlv_block   = ' '.
        wl_orderheaderinx-dlv_block   = 'X'.
        LOOP AT tl_0053.
          IF tl_0053-status NE 'E'.
            "*---> 28/06/2023 - Migração S4 - LO --> Material não foi utilizado
            CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
              EXPORTING
                salesdocument    = tl_0053-vbeln
                order_header_in  = wl_orderheaderin
                order_header_inx = wl_orderheaderinx
              TABLES
*               ORDER_ITEM_IN    = TL_BAPISDITM
*               ORDER_ITEM_INX   = TL_BAPISDITMX
*               SCHEDULE_LINES   = TL_SCHEDULE_LINES
*               SCHEDULE_LINESX  = TL_SCHEDULE_LINESX
                return           = tl_return.

            CLEAR:wl_return.
            READ TABLE tl_return INTO wl_return WITH KEY type = 'E'.

            IF sy-subrc NE 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
              IF i_tipo_sol EQ 'VN'.
                UPDATE zsdt0053 SET status = 'D'
                  WHERE nro_sol_ov EQ tl_0053-nro_sol_ov
                    AND posnr      EQ tl_0053-posnr
                    AND vbeln      EQ tl_0053-vbeln.
              ELSEIF i_tipo_sol EQ 'FL'.
                UPDATE zsdt0066 SET status_form = 'D'
                WHERE nro_sol_ov EQ tl_0053-nro_sol_ov
                  AND posnr      EQ tl_0053-posnr
                  AND vbeln      EQ tl_0053-vbeln.
              ENDIF.
            ELSE.
*            DELETE TL_0053 FROM WL_TABIX.
            ENDIF.
            CLEAR: tl_saida_exec.

          ENDIF.

          MOVE: tl_0053-nro_sol_ov  TO tl_saida_exec-nro_sol_ov,
                tl_0053-posnr       TO tl_saida_exec-posnr,
                tl_0053-zmeng       TO tl_saida_exec-zmeng,
                tl_0053-valdt       TO tl_saida_exec-valdt,
                tl_0053-vlrtot      TO tl_saida_exec-vlrtot,
                tl_0053-vbeln       TO tl_saida_exec-vbeln.

          IF tl_0053-status NE 'E'.
            IF wl_return IS INITIAL.
              APPEND tl_saida_exec.
            ELSE.
              LOOP AT tl_return WHERE type EQ 'E'.
                MOVE: tl_return-message TO tl_saida_exec-msg.
                APPEND tl_saida_exec.
              ENDLOOP.
            ENDIF.
          ELSE.
            tl_saida_exec-msg = 'A O.V. não pode ser desbloqueada, pois já está encerrada!'.
            APPEND tl_saida_exec.
          ENDIF.

        ENDLOOP.
      ENDIF.
    WHEN 'X'. "Excluir
      wl_orderheaderinx-updateflag = 'D'.
      SORT: tl_bsid BY vbel2,
            tl_bkpf BY belnr xreversal.

      LOOP AT tl_0053.
        CLEAR: tl_bkpf, tl_bsid.
        READ TABLE tl_0052
         WITH KEY nro_sol_ov = tl_0053-nro_sol_ov.

        IF tl_0052-pgto_ant IS NOT INITIAL.
          READ TABLE tl_bsid
            WITH KEY vbel2 = tl_0053-vbeln
                     BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            READ TABLE tl_bkpf
              WITH KEY belnr = tl_bsid-belnr
                   xreversal = space
                       BINARY SEARCH.

            IF sy-subrc IS NOT INITIAL.
              READ TABLE tl_bkpf
                WITH KEY belnr = tl_bsid-belnr
                     xreversal = 2
                         BINARY SEARCH.
            ENDIF.
            IF sy-subrc IS INITIAL.
              wl_return-type = 'E'.
              wl_return-message = TEXT-001.
              APPEND wl_return TO tl_return.
              CLEAR: wl_return.
            ENDIF.

          ENDIF.
        ENDIF.
        IF tl_bkpf IS INITIAL.

*-IR 189210-14.01.2025-#163685-JT-inicio
*----------------------------------
*-------- envia OVS ao trace cotton
*----------------------------------
*-#165704-14.01.2025-#165704-JT-inicio
*-------- verifica se integrou POST / PUT
          SELECT SINGLE *
            INTO @DATA(_zsdt0213_post)
            FROM zsdt0213_integra
           WHERE nro_sol_ov = @tl_0053-nro_sol_ov
             AND posnr      = @tl_0053-posnr
             AND vbeln      = @tl_0053-vbeln
             AND metodo    IN ('POST','PUT').

          IF sy-subrc = 0 AND _zsdt0213_post-integrado = abap_true.
            CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE'
              EXPORTING
                i_nro_sol_ov = tl_0053-nro_sol_ov
                i_posnr      = tl_0053-posnr
                i_vbeln      = tl_0053-vbeln
                i_acao       = 'E'
              EXCEPTIONS
                OTHERS       = 1.

*----------------------------------
*-------- verifica se integrou DELETE
*----------------------------------
            SELECT SINGLE *
              INTO @DATA(_zsdt0213)
              FROM zsdt0213_integra
             WHERE nro_sol_ov = @tl_0053-nro_sol_ov
               AND posnr      = @tl_0053-posnr
               AND vbeln      = @tl_0053-vbeln
               AND metodo     = 'DELETE'.

            IF NOT ( sy-subrc = 0  AND _zsdt0213-integrado = abap_true ).
              CALL FUNCTION 'S_AUT_POPUP_TO_DISPLAY_TEXT_LO'
                EXPORTING
                  iv_titel        = 'ATENÇÃO! OV não Eliminada no TRACE COTTON! Verifique! Erro:'
                  iv_textline1    = _zsdt0213-mesg_retorno(65)
                  iv_textline2    = _zsdt0213-mesg_retorno+65(65)
                  iv_textline3    = _zsdt0213-mesg_retorno+130(65)
                  iv_start_column = 20
                  iv_start_row    = 10.

              UPDATE zsdt0213_integra SET integrado  = abap_true
                                    WHERE nro_sol_ov = tl_0053-nro_sol_ov
                                      AND posnr      = tl_0053-posnr
                                      AND vbeln      = tl_0053-vbeln
                                      AND metodo     = 'DELETE'.
              RETURN.
            ENDIF.
          ENDIF.
*-#165704-14.01.2025-#165704-JT-fim
*-IR 189210-14.01.2025-#163685-JT-inicio

          "*---> 28/06/2023 - Migração S4 - LO --> Material não foi utilizado
          CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              salesdocument    = tl_0053-vbeln
*             ORDER_HEADER_IN  = WL_ORDERHEADERIN
              order_header_inx = wl_orderheaderinx
            TABLES
              return           = tl_return.
        ENDIF.
        CLEAR:wl_return.
        READ TABLE tl_return INTO wl_return WITH KEY type = 'E'.

        IF sy-subrc NE 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          IF i_tipo_sol EQ 'VN'.
            UPDATE zsdt0053 SET vbeln = space
                                status = space
              WHERE nro_sol_ov EQ tl_0053-nro_sol_ov
                AND posnr      EQ tl_0053-posnr
                AND vbeln      EQ tl_0053-vbeln.
          ELSE.
            UPDATE zsdt0066 SET vbeln = space
                                status = space
             WHERE nro_sol_ov EQ tl_0053-nro_sol_ov
               AND posnr      EQ tl_0053-posnr
               AND vbeln      EQ tl_0053-vbeln.
          ENDIF.
        ELSE.
*          DELETE TL_0053 FROM WL_TABIX.
        ENDIF.
        CLEAR: tl_saida_exec.
        MOVE: tl_0053-nro_sol_ov  TO tl_saida_exec-nro_sol_ov,
              tl_0053-posnr       TO tl_saida_exec-posnr,
              tl_0053-zmeng       TO tl_saida_exec-zmeng,
              tl_0053-valdt       TO tl_saida_exec-valdt,
              tl_0053-vlrtot      TO tl_saida_exec-vlrtot,
              tl_0053-vbeln       TO tl_saida_exec-vbeln.

        "Altera Status na ZSDT0158 - Tabela Solicitação Formação Lote e Pedido Transferência
        SELECT SINGLE * FROM zsdt0158 INTO @DATA(w_zsdt0158)
          WHERE nro_sol_ov = @tl_saida_exec-nro_sol_ov.
        IF ( sy-subrc = 0 ).
          UPDATE zsdt0158 SET status = 'G' WHERE sequencial = w_zsdt0158-sequencial.
        ENDIF.


        IF wl_return IS INITIAL.
          APPEND tl_saida_exec.
        ELSE.
          LOOP AT tl_return WHERE type EQ 'E'.
            MOVE: tl_return-message TO tl_saida_exec-msg.
            APPEND tl_saida_exec.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
  ENDCASE.



*  ################################  UPDATE ZSDT0059  @@@ INICIO @@@ ###########################################
  IF sy-subrc IS INITIAL.
    IF ( i_status EQ 'E' ).
      CLEAR: true_taxa, true_qtd.

      LOOP AT tl_0059 INTO wl_0059 WHERE field EQ 'PRECO'
                                     AND ( bezei EQ 'TAXA CAMBIO FRAME'
                                        OR bezei EQ 'PREMIO FRAME'
                                        OR bezei EQ 'CHICAGO FRAME'
                                        OR bezei EQ 'BM&F FRAME' ).  " 29.10.2024 - 147331 - RAMON

        IF wl_0059-bezei EQ 'TAXA CAMBIO FRAME'.
          formula_r = wl_0059-formula2.
        ELSEIF wl_0059-bezei EQ 'PREMIO FRAME'.
          formula_p = wl_0059-formula2.
        ELSEIF wl_0059-bezei EQ 'CHICAGO FRAME'.
          formula_c = wl_0059-formula2.

          " 29.10.2024 - 147331 - RAMON -->
        ELSEIF wl_0059-bezei EQ 'BM&F FRAME'.
          formula_b = wl_0059-formula2.
          " 29.10.2024 - 147331 - RAMON <--

        ENDIF.

      ENDLOOP.

      DATA: c_tabix TYPE sy-tabix,
            p_tabix TYPE sy-tabix,
            b_tabix TYPE sy-tabix, " 29.10.2024 - 147331 - RAMON
            r_tabix TYPE sy-tabix,
            c_t     TYPE sy-tabix,
            p_t     TYPE sy-tabix,
            r_t     TYPE sy-tabix,
            b_t     TYPE sy-tabix, " 29.10.2024 - 147331 - RAMON
            c_q     TYPE sy-tabix,
            p_q     TYPE sy-tabix,
            r_q     TYPE sy-tabix,
            b_q     TYPE sy-tabix.

      LOOP AT tl_0059 INTO wl_0059.

        c_tabix = sy-tabix.
        p_tabix = sy-tabix.
        r_tabix = sy-tabix.
        b_tabix = sy-tabix. " 29.10.2024 - 147331 - RAMON

        wl_premio = wl_0059-formula2.


        CLEAR: var_saldo_negativo.

        IF wl_premio IS INITIAL AND wl_0059-posnr1 IS INITIAL.

          IF wl_0059-field EQ 'QTDFIXADA'.

            READ TABLE tl_0053  INTO lw_0053  WITH KEY nro_sol_ov = wl_0059-nro_sol_ov
                                                       fixacao    = wl_0059-posnr.
            READ TABLE tg_saldo INTO wg_saldo WITH KEY nro_sol_ov = lw_0053-nro_sol_ov
                                                       vbeln      = lw_0053-vbeln.
            var_saldo_negativo = ( ( wg_saldo-zmeng - wg_saldo-total ) * -1 ).

            IF wl_0059-bezei IN c_bezei AND c_q IS INITIAL.
              c_q = c_tabix.
              UPDATE zsdt0059 SET formula  = var_saldo_negativo
                                  formula2 = var_saldo_negativo
                                  posnr1   = lw_0053-posnr
                                  monat    = wl_0059-monat
                                  cbot     = wl_0059-cbot
                                  valdt    = sy-datum
                              WHERE nro_sol_ov EQ  wl_0059-nro_sol_ov
                                AND nivel      EQ  wl_0059-nivel
                                AND posnr      EQ  wl_0059-posnr
                                AND cod_fp     EQ  wl_0059-cod_fp
                                AND field      EQ  wl_0059-field.

              LOOP AT tl_0059 INTO wl_0059 WHERE field EQ 'PRECO' AND bezei EQ wl_0059-bezei.
                READ TABLE tl_0059 INTO wl_0059_aux1 WITH KEY field = 'PRECO' bezei = 'C1'.

                UPDATE zsdt0059 SET formula  = formula_c
                                    formula2 = formula_c
                                    posnr1   = lw_0053-posnr
                                    monat    = wl_0059_aux1-monat
                                    cbot     = wl_0059_aux1-cbot
                                    valdt    = sy-datum
                                WHERE nro_sol_ov EQ  wl_0059-nro_sol_ov
                                  AND nivel      EQ  wl_0059-nivel
                                  AND posnr      EQ  wl_0059-posnr
                                  AND cod_fp     EQ  wl_0059-cod_fp
                                  AND field      EQ  wl_0059-field.
              ENDLOOP.

            ELSEIF wl_0059-bezei IN p_bezei AND p_q IS INITIAL.
              p_q = p_tabix.
              UPDATE zsdt0059 SET formula  = var_saldo_negativo
                                  formula2 = var_saldo_negativo
                                  posnr1   = lw_0053-posnr
                                  monat    = wl_0059-monat
                                  cbot     = wl_0059-cbot
                                  valdt    = sy-datum
                              WHERE nro_sol_ov EQ  wl_0059-nro_sol_ov
                                AND nivel      EQ  wl_0059-nivel
                                AND posnr      EQ  wl_0059-posnr
                                AND cod_fp     EQ  wl_0059-cod_fp
                                AND field      EQ  wl_0059-field.

              LOOP AT tl_0059 INTO wl_0059 WHERE field EQ 'PRECO' AND bezei EQ wl_0059-bezei.
                READ TABLE tl_0059 INTO wl_0059_aux1 WITH KEY field = 'PRECO' bezei = 'P1'.

                UPDATE zsdt0059 SET formula  = formula_p
                                    formula2 = formula_p
                                    posnr1   = lw_0053-posnr
                                    monat    = wl_0059_aux1-monat
                                    cbot     = wl_0059_aux1-cbot
                                    valdt    = sy-datum
                                WHERE nro_sol_ov EQ  wl_0059-nro_sol_ov
                                  AND nivel      EQ  wl_0059-nivel
                                  AND posnr      EQ  wl_0059-posnr
                                  AND cod_fp     EQ  wl_0059-cod_fp
                                  AND field      EQ  wl_0059-field.
              ENDLOOP.

            ELSEIF wl_0059-bezei IN r_bezei AND r_q IS INITIAL.
              r_q = r_tabix.
              UPDATE zsdt0059 SET formula  = var_saldo_negativo
                                  formula2 = var_saldo_negativo
                                  posnr1   = lw_0053-posnr
                                  monat    = wl_0059-monat
                                  cbot     = wl_0059-cbot
                                  valdt    = sy-datum
                              WHERE nro_sol_ov EQ  wl_0059-nro_sol_ov
                                AND nivel      EQ  wl_0059-nivel
                                AND posnr      EQ  wl_0059-posnr
                                AND cod_fp     EQ  wl_0059-cod_fp
                                AND field      EQ  wl_0059-field.

              LOOP AT tl_0059 INTO wl_0059 WHERE field EQ 'PRECO' AND bezei EQ wl_0059-bezei.
                READ TABLE tl_0059 INTO wl_0059_aux1 WITH KEY field = 'PRECO' bezei = 'T1'.

                UPDATE zsdt0059 SET formula  = formula_r
                                    formula2 = formula_r
                                    posnr1   = lw_0053-posnr
                                    monat    = wl_0059_aux1-monat
                                    cbot     = wl_0059_aux1-cbot
                                    valdt    = sy-datum
                                WHERE nro_sol_ov EQ  wl_0059-nro_sol_ov
                                  AND nivel      EQ  wl_0059-nivel
                                  AND posnr      EQ  wl_0059-posnr
                                  AND cod_fp     EQ  wl_0059-cod_fp
                                  AND field      EQ  wl_0059-field.
              ENDLOOP.

              " 29.10.2024 - 147331 - RAMON -->
            ELSEIF wl_0059-bezei IN b_bezei AND b_q IS INITIAL.
              b_q = b_tabix.
              UPDATE zsdt0059 SET formula  = var_saldo_negativo
                                  formula2 = var_saldo_negativo
                                  posnr1   = lw_0053-posnr
                                  monat    = wl_0059-monat
                                  cbot     = wl_0059-cbot
                                  valdt    = sy-datum
                              WHERE nro_sol_ov EQ  wl_0059-nro_sol_ov
                                AND nivel      EQ  wl_0059-nivel
                                AND posnr      EQ  wl_0059-posnr
                                AND cod_fp     EQ  wl_0059-cod_fp
                                AND field      EQ  wl_0059-field.

              LOOP AT tl_0059 INTO wl_0059 WHERE field EQ 'PRECO' AND bezei EQ wl_0059-bezei.
                READ TABLE tl_0059 INTO wl_0059_aux1 WITH KEY field = 'PRECO' bezei = 'B1'.

                UPDATE zsdt0059 SET formula  = formula_b
                                    formula2 = formula_b
                                    posnr1   = lw_0053-posnr
                                    monat    = wl_0059_aux1-monat
                                    cbot     = wl_0059_aux1-cbot
                                    valdt    = sy-datum
                                WHERE nro_sol_ov EQ  wl_0059-nro_sol_ov
                                  AND nivel      EQ  wl_0059-nivel
                                  AND posnr      EQ  wl_0059-posnr
                                  AND cod_fp     EQ  wl_0059-cod_fp
                                  AND field      EQ  wl_0059-field.
              ENDLOOP.
              " 29.10.2024 - 147331 - RAMON <--


            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.
      CLEAR: c_tabix, p_tabix, r_tabix, b_tabix, " 29.10.2024 - 147331 - RAMON --
             c_t, p_t, r_t, b_t, " 29.10.2024 - 147331 - RAMON --
             c_q, p_q, r_q, b_t. " 29.10.2024 - 147331 - RAMON --

    ENDIF.

*  ################################  UPDATE ZSDT0059  @@@ FIM @@@ ###########################################

    CLEAR: wl_setleaf.

    IF ti_nro_sol_ov[] IS NOT INITIAL.

      SELECT * FROM zsdt0051
        INTO TABLE lt_zsdt0051
        FOR ALL ENTRIES IN ti_nro_sol_ov
      WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.

      IF i_syucomm EQ 'ENCERRA'.

*  IF ( I_SYUCOMM NE 'BLOQUEAR' ) AND ( I_SYUCOMM NE 'DESBLOQ' ) AND ( I_SYUCOMM NE 'ESTOR_ADI' ) AND ( I_SYUCOMM NE 'DEL' ).
        READ TABLE lt_zsdt0051 INTO lw_zsdt0051 INDEX 1.

        IF lw_zsdt0051-waerk EQ 'BRL'.

          SELECT SINGLE * FROM setleaf INTO wl_setleaf WHERE setname EQ 'MAGGI_ZSDT0062_HEDGE'
                                                         AND valfrom EQ lw_zsdt0051-tp_venda.

          IF ( sy-subrc EQ 0 ) AND ( lw_zsdt0051-param_espec = 'M' ) AND sy-uname NE 'WBARBOSA'.

*     A SEQUENCIA ABAIXO EDI -> FRA -> FRE.  ATENDE AO ENCERRAMENTO DO FRAME
            "WebService para Capturar o Valor da Taxa e gravar no campo TAXA_CURVA
            FREE: obj_zcl_webservice_taxa_curva.
            CREATE OBJECT obj_zcl_webservice_taxa_curva.
            TRY.

                obj_zcl_webservice_taxa_curva->executar( i_numero  = lw_zsdt0051-nro_sol_ov
                                                         i_fixacao = tl_0053-fixacao
                                                         i_tcode   = 'ZSDT0062'
                                                         i_tipo    = 'EDI'
                                                        ).

                obj_zcl_webservice_taxa_curva->executar( i_numero  = lw_zsdt0051-nro_sol_ov
                                                         i_fixacao = tl_0053-fixacao
                                                         i_tipo    = 'FRA'
                                                         i_tcode   = 'ZSDT0066'
                                                         ).

                obj_zcl_webservice_taxa_curva->executar( i_numero  =  lw_zsdt0051-nro_sol_ov
                                                          i_tipo    = 'FRE'
                                                          i_fixacao = tl_0053-fixacao
                                                          i_tcode   = 'ZSDT0062'
                                                          i_status  = 'X'
                                                          ).

              CATCH zcx_webservice INTO cx_exception.
                var_msg = cx_exception->get_text( ).
                MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH var_msg.
            ENDTRY.
            CLEAR: lw_0051.

          ELSEIF ( sy-subrc EQ 0 ) AND sy-uname NE 'WBARBOSA'.

            FREE: obj_zcl_webservice_taxa_curva.
            CREATE OBJECT obj_zcl_webservice_taxa_curva.
            TRY.
                obj_zcl_webservice_taxa_curva->executar( i_numero =  lw_zsdt0051-nro_sol_ov
                                                         i_tipo   = 'EDI'
                                                         i_tcode  = 'ZSDT0062'
                                                         ).

                obj_zcl_webservice_taxa_curva->executar( i_numero =  lw_zsdt0051-nro_sol_ov
                                                         i_tipo   = 'ENC'
                                                         i_tcode  = 'ZSDT0062'
                                                         ).

              CATCH zcx_webservice INTO cx_exception.
                var_msg = cx_exception->get_text( ).
                MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH var_msg.
            ENDTRY.

            CLEAR: lw_0051.
          ENDIF.
        ELSEIF lw_zsdt0051-waerk EQ 'USD' AND sy-uname NE 'WBARBOSA'.
*Vai executar o passo abaixo, apenas para fazer o a atualização da Aba Logistica, pois esse processo ficou dentro do método
          FREE: obj_zcl_webservice_taxa_curva.
          CREATE OBJECT obj_zcl_webservice_taxa_curva.
          TRY.

              obj_zcl_webservice_taxa_curva->executar(  i_numero  = lw_zsdt0051-nro_sol_ov
                                                        i_fixacao = tl_0053-fixacao
                                                        i_tcode   = 'ZSDT0062'
                                                        i_tipo    = 'LOG'
                                                       ).

            CATCH zcx_webservice INTO cx_exception.
              var_msg = cx_exception->get_text( ).
              MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH var_msg.
          ENDTRY.
          CLEAR: lw_0051.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*-IR 189210-14.01.2025-#163685-JT-inicio - comentado
*-CS2023000189-26.04.2023-#108710-JT-inicio
*----------------------------------
* envia OVS ao trace cotton
*----------------------------------
*  IF tl_saida_exec[] IS NOT INITIAL.
*    READ TABLE tl_return INTO wl_return WITH KEY type = 'E'.  "CS2023000189-18.12.2023-#129705-JT
*
*    IF sy-subrc <> 0.
*      tl_saida_exec_aux[] = tl_saida_exec[].
*
*      SORT tl_saida_exec_aux BY nro_sol_ov posnr.
*      DELETE ADJACENT DUPLICATES FROM tl_saida_exec_aux
*                            COMPARING nro_sol_ov posnr.
*
*      LOOP AT tl_saida_exec_aux INTO DATA(wl_saida) WHERE vbeln IS NOT INITIAL.
*        DATA(l_task) = 'TRACE_ORDEM_VENDA' && wl_saida-nro_sol_ov && wl_saida-posnr.
*
*        CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE' STARTING NEW TASK l_task
*          EXPORTING
*            i_nro_sol_ov = wl_saida-nro_sol_ov
*            i_posnr      = wl_saida-posnr
*            i_vbeln      = wl_saida-vbeln
*            i_acao       = 'E'
*          EXCEPTIONS
*            OTHERS       = 1.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
*-CS2023000189-26.04.2023-#108710-JT-fim
*-IR 189210-14.01.2025-#163685-JT-fim - comentado

  IF i_tipo EQ '1'.
    IF tl_saida_exec[] IS NOT INITIAL.
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
    ENDIF.
  ELSE.
    te_saida_exec[] = tl_saida_exec[].
  ENDIF.

ENDFUNCTION.
