*----------------------------------------------------------------------*
***INCLUDE ZFIR0025_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_6500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_6500 INPUT.
  CLEAR wl_erro.
  DATA: vlinhacpa(3),
*        VQTDECP      TYPE I,
*        VTOTAL_CP    TYPE ZFIT0036-VLR_PGTO,
* ---> S4 Migration - 10/06/2023 - DG
*        vtotal_rs    TYPE zfit0036-vlr_pgto,
*        vvlr_pgto    TYPE zfit0036-vlr_pgto.
        vtotal_rs    TYPE BSID-DMBE2,
        vvlr_pgto    TYPE BSID-DMBE2.
* <--- S4 Migration - 10/06/2023 - DG

*       VTOTAL_AD    TYPE ZFIT0036-VLR_PGTO.
  CASE ok-code.
    WHEN 'GERAR'.
      vtotal_cp = 0.
      vtotal_rs = 0.
      vvlr_pgto = 0.
      vqtdecp = 0.
      IF tg_compe[] IS INITIAL.
        MESSAGE 'Vincular Invoice X Adiantamento'(349) TYPE 'I'.
        wl_erro = 'X'.
        EXIT.
      ENDIF.
      IF wg_cadcom-dt_pgto IS INITIAL.
        MESSAGE 'Informe a data de pagamento'(263) TYPE 'I'.
        wl_erro = 'X'.
        EXIT.
      ENDIF.
      LOOP AT tg_pagar INTO wg_pagar.
        IF wg_pagar-vlr_comp GT 0.
          ADD 1 TO vqtdecp.
          ADD wg_pagar-vlr_comp TO vtotal_cp .
          ADD wg_pagar-vlr_pgto TO vvlr_pgto .
          ADD wg_pagar-vlr_sld  TO vtotal_rs.
        ENDIF.
        IF wg_pagar-vlr_sld LT 0.
          vlinhacpa = sy-tabix.
          CONCATENATE 'Saldo negativo PAGAR, linha'(283) vlinhacpa INTO vmsgcp SEPARATED BY space.
          MESSAGE vmsgcp TYPE 'I'.
          wl_erro = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF wl_erro = 'X'.
        EXIT.
      ENDIF.
      IF vtotal_cp EQ 0.
        MESSAGE 'Informe valor compensação CONTAS A PAGAR '(284) TYPE 'I'.
        wl_erro = 'X'.
        EXIT.
      ENDIF.

      IF vqtdecp NE 1.
        MESSAGE 'Informe valor compensação CONTAS A PAGAR, um de cada vez '(285) TYPE 'I'.
        wl_erro = 'X'.
        EXIT.
      ENDIF.

      vtotal_ad = 0.
      LOOP AT tg_adiant INTO wg_adiant.
        ADD wg_adiant-vlr_comp TO  vtotal_ad.
      ENDLOOP.

      vtotal_ad = vvlr_pgto - vtotal_ad.
      IF vtotal_rs NE vtotal_ad.
        MESSAGE 'Total compensação  não zera '(350) TYPE 'I'.
        wl_erro = 'X'.
        EXIT.
      ENDIF.

      IF vtotal_ad EQ 0.
        MESSAGE 'ADIANTAMENTO sem valores a compensar'(351) TYPE 'I'.
        wl_erro = 'X'.
        EXIT.
      ENDIF.

      CLEAR wl_erro.
      PERFORM f_shdb_ivad CHANGING wg_pagar wl_erro.
      IF wl_erro EQ 'X' .
        MESSAGE 'Erro na execução do processo (SHDB)'(288) TYPE 'I'.
        SET SCREEN 0.
        EXIT.
      ENDIF.
      wg_atualiza_shdb = 'X'.
      " Marca status compensado 'C' em todas a INVOICE PAGAR e RECEBER que tiveram Valores compensados.
      SORT: tg_adiant  BY belnr,
            tg_pagar   BY belnr.
      tg_compe_aux[] = tg_compe[].
      "
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZID_INV'
        IMPORTING
          number      = vseq.
      vnum = vseq .

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vnum
        IMPORTING
          output = vnum.
      "
      SORT tg_compe_aux BY belnr_cp belnr_cr.
      DELETE ADJACENT DUPLICATES FROM tg_compe_aux COMPARING belnr_cp belnr_cr.
      LOOP AT tg_compe_aux INTO wg_compe.
        READ TABLE tg_pagar INTO wg_pagar WITH KEY belnr = wg_compe-belnr_cp BINARY SEARCH.
        IF sy-subrc = 0.
          wg_pagar-fg_ok = 'X'.
          MODIFY tg_pagar FROM wg_pagar INDEX sy-tabix  TRANSPORTING fg_ok.
          UPDATE zfit0036 SET status = 'C'
                              lote_cp = vnum
          WHERE obj_key = wg_pagar-obj_key
          AND belnr   = wg_pagar-belnr36
          AND buzei   = wg_pagar-buzei36.
        ENDIF.

      ENDLOOP.
      COMMIT WORK.

      IF wl_erro NE 'Z'. "
        REFRESH it_bsik_shdb.
        vtentativas = 0.
        WHILE vtentativas LT 10.
          WAIT UP TO 1 SECONDS.
          SELECT * "bukrs lifnr belnr dmbtr dmbe2 budat buzei
           FROM bsik
           INTO CORRESPONDING FIELDS OF TABLE it_bsik_shdb
           WHERE bukrs EQ p_bukrs
           AND   belnr EQ wg_documento
           AND   umsks EQ ' '.
          ADD 1 TO vtentativas.
          IF NOT it_bsik_shdb[] IS INITIAL.
            EXIT.
          ENDIF.
        ENDWHILE.
        IF  it_bsik_shdb[] IS INITIAL.
          wl_erro = 'X' .
          EXIT.
        ENDIF.
        LOOP AT it_bsik_shdb INTO wa_bsik.
          SELECT SINGLE *
            FROM zfit0036
            INTO @DATA(_zfit0036)
             WHERE obj_key = @wg_pagar-obj_key
              AND belnr    = @wg_pagar-belnr36
              AND buzei    = @wg_pagar-buzei36.

          CLEAR wa_zfit0036_ins.
          wa_zfit0036_ins-obj_key  = wg_pagar-obj_key .
          wa_zfit0036_ins-bukrs    = p_bukrs.
          wa_zfit0036_ins-belnr    = wa_bsik-belnr.
          wa_zfit0036_ins-buzei    = wa_bsik-buzei.
          wa_zfit0036_ins-invoice  = wg_pagar-invoice.
          wa_zfit0036_ins-navio    = wg_pagar-navio.
          wa_zfit0036_ins-lote_cp  = vnum.
          wa_zfit0036_ins-STATUS_ARQ_INV  = 'A'.
          "
          wa_zfit0036_ins-IN_PERFORMANCE      = _zfit0036-IN_PERFORMANCE.
          wa_zfit0036_ins-id_tipo_invoice     = _zfit0036-id_tipo_invoice.
          wa_zfit0036_ins-invoice_terc        = _zfit0036-invoice_terc.
          wa_zfit0036_ins-matnr               = _zfit0036-matnr.

          INSERT INTO  zfit0036 VALUES wa_zfit0036_ins.
          IF sy-subrc NE 0.
            ROLLBACK WORK.
          ELSE.
            COMMIT WORK.
          ENDIF.

        ENDLOOP.

      ENDIF.
      REFRESH tg_compe.
      MESSAGE 'Processo realizado com sucesso'(290) TYPE 'I'.
      SET SCREEN 0.

    WHEN 'BTN_COMP'.
      vtotal_cp = 0.
      vtotal_rs = 0.
      vvlr_pgto = 0.
      LOOP AT tg_pagar INTO wg_pagar.
        ADD wg_pagar-vlr_comp TO vtotal_cp .
        ADD wg_pagar-vlr_pgto TO vvlr_pgto .
        ADD wg_pagar-vlr_sld  TO vtotal_rs .
        IF wg_pagar-vlr_sld LT 0.
          vlinhacpa = sy-tabix.
          CONCATENATE 'Saldo negativo PAGAR, linha'(283) vlinhacpa INTO vmsgcp SEPARATED BY space.
          MESSAGE vmsgcp TYPE 'I'.
          wl_erro = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF wl_erro = 'X'.
        EXIT.
      ENDIF.
      IF vtotal_cp EQ 0.
        MESSAGE 'Informe valor compensação CONTAS A PAGAR '(284) TYPE 'I'.
        wl_erro = 'X'.
        EXIT.
      ENDIF.
      vtotal_ad = 0.
      LOOP AT tg_adiant INTO wg_adiant.
        ADD wg_adiant-vlr_comp TO  vtotal_ad.
      ENDLOOP.

      vtotal_ad = vvlr_pgto - vtotal_ad.

      IF vtotal_rs NE vtotal_ad.
        MESSAGE 'Total compensação não zera '(350) TYPE 'I'.
        wl_erro = 'X'.
        EXIT.
      ENDIF.
      REFRESH tg_compe.
      LOOP AT tg_pagar INTO wg_pagar.
        IF wg_pagar-vlr_comp EQ 0.
          CONTINUE.
        ENDIF.
        LOOP AT tg_adiant INTO wg_adiant.
          wg_compe-belnr_cp = wg_pagar-belnr.
          wg_compe-belnr_cr = wg_adiant-belnr.
          wg_compe-vlr_comp = wg_adiant-vlr_comp.
          APPEND wg_compe TO tg_compe.

        ENDLOOP.
        DELETE tg_compe WHERE vlr_comp = 0.
      ENDLOOP.

      CALL METHOD grid15->refresh_table_display
        EXPORTING
          is_stable = wa_stable.


    WHEN 'SAIR'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.
