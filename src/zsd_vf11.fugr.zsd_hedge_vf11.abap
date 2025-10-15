FUNCTION zsd_hedge_vf11.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(XVBRP) TYPE  VBRPVB
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"----------------------------------------------------------------------

  CREATE OBJECT obj_auart.

  sair = 0.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 1
      text       = 'Aguardando Gravação'.

  DO.

    ADD 1 TO sair.

    SELECT SINGLE *
      FROM vbrk
      INTO @DATA(wa_vbrk)
      WHERE vbeln EQ @i_vbeln.

    IF sy-subrc IS NOT INITIAL.

      IF sair EQ 5 .
        EXIT.
      ENDIF.

      WAIT UP TO 2 SECONDS.

    ELSE.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sair * 20
        text       = 'Aguardando Gravação'.

  ENDDO.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 100
      text       = 'Aguardando Gravação'.

  CHECK wa_vbrk IS NOT INITIAL.

* //" Get SET de AUART de Devolução/Recusa
  CALL METHOD obj_auart->get_auart
    EXPORTING
      set     = 'TODOS'
    RECEIVING
      r_range = r_auart.

* //" Get SET de AUART de Complemento
  CALL METHOD obj_auart->get_auart
    EXPORTING
      set     = 'ZHEDGECOMP'
    RECEIVING
      r_range = r_comp.

*   //" Get SET de AUART de Devolução/Recusa
  CALL METHOD obj_auart->get_auart
    EXPORTING
      set     = 'ZHEDGEDEVO/RECU'
    RECEIVING
      r_range = r_devo_recu.

  SELECT SINGLE *
      FROM vbak
      INTO @DATA(w_vbak)
      WHERE vbeln = @xvbrp-aubel.


  IF w_vbak-auart IN r_devo_recu.
    var_dir = 'Y'.
  ELSEIF w_vbak-auart IN r_comp.
    var_dir = 'W'.
  ENDIF.

  IF w_vbak-auart IN r_auart.

    SELECT COUNT(*)
      FROM zsdt0053
     WHERE vbeln EQ xvbrp-aubel
       AND status IN ('W' , 'Y' ).

    IF sy-subrc IS INITIAL.
      dir_mi_in = 'MI'.
    ELSE.

      SELECT COUNT(*) FROM zsdt0100
        WHERE vbeln EQ xvbrp-aubel
        AND status NE 'C'.

      IF sy-subrc IS INITIAL.
        dir_mi_in = 'MI'.
      ENDIF.
    ENDIF.

    SELECT COUNT(*)
      FROM zsdt0090
      WHERE vbeln EQ xvbrp-aubel
        AND posnn EQ xvbrp-aupos
        AND categoria EQ var_dir
        AND estorno EQ abap_false.

    IF sy-subrc IS INITIAL.
      dir_mi_in = 'IN'.
    ENDIF.

    CASE dir_mi_in.
      WHEN 'MI'.

        SELECT * FROM zsdt0053
           INTO TABLE lt_zsdt0053
         WHERE vbeln EQ xvbrp-aubel
           AND status IN ('W' , 'Y' ).

        IF lt_zsdt0053 IS NOT INITIAL.
          SELECT * FROM zsdt0051
             INTO TABLE @DATA(it_zsdt0051)
             FOR  ALL ENTRIES IN @lt_zsdt0053
           WHERE nro_sol_ov EQ @lt_zsdt0053-nro_sol_ov.
        ENDIF.

        SELECT * FROM zsdt0100
          INTO TABLE lt_zsdt0100
          WHERE vbeln EQ xvbrp-aubel.

        DELETE lt_zsdt0100 WHERE status EQ 'C'.

        IF it_zsdt0051[] IS INITIAL.
          SELECT * FROM zsdt0051
             APPENDING TABLE it_zsdt0051
             FOR  ALL ENTRIES IN lt_zsdt0100
           WHERE nro_sol_ov EQ lt_zsdt0100-nro_sol_ov.
        ENDIF.

        READ TABLE it_zsdt0051 WITH KEY waerk = 'BRL' TRANSPORTING NO FIELDS.
        CHECK sy-subrc IS INITIAL.

        TRY.
            DATA(tp_venda) = it_zsdt0051[ nro_sol_ov = lt_zsdt0053[ 1 ]-nro_sol_ov ]-tp_venda.
          CATCH cx_sy_itab_line_not_found.
            TRY.
                tp_venda = it_zsdt0051[ nro_sol_ov = lt_zsdt0100[ 1 ]-nro_sol_ov ]-tp_venda.
              CATCH cx_sy_itab_line_not_found.
                CLEAR tp_venda.
            ENDTRY.
        ENDTRY.

        SELECT COUNT(*)
          FROM setleaf
          WHERE setname EQ 'MAGGI_ZSDT0062_HEDGE'
            AND valfrom EQ tp_venda.

        CHECK sy-subrc IS INITIAL.

        IF lines( lt_zsdt0100 ) EQ 0.
          READ TABLE lt_zsdt0053 INTO lw_zsdt0053 INDEX 1.
          IF NOT sy-subrc IS INITIAL.
            EXIT.
          ENDIF.
        ENDIF.

        READ TABLE lt_zsdt0053 INTO lw_zsdt0053 INDEX 1.
        IF sy-subrc EQ 0 AND lw_zsdt0053-fixacao IS INITIAL.
          UPDATE zsdt0053 SET status = 'C'
            WHERE vbeln EQ xvbrp-aubel.
        ELSE.
          IF lt_zsdt0100 IS NOT INITIAL.
            UPDATE zsdt0100 SET status = 'C'
              WHERE vbeln EQ xvbrp-aubel.
          ELSE.
            UPDATE zsdt0053 SET status = 'C'
              WHERE vbeln EQ xvbrp-aubel.
          ENDIF.
        ENDIF.

        UPDATE zsdt0055 SET status = 'C'
          WHERE vbeln EQ xvbrp-aubel.

        READ TABLE lt_zsdt0100 INTO lw_zsdt0100 INDEX 1.

        FREE: obj_zcl_webservice_taxa_curva.
        CREATE OBJECT obj_zcl_webservice_taxa_curva.

        TRY.
            IF ( lw_zsdt0053-fixacao IS INITIAL ) AND ( lw_zsdt0100-fixacao IS INITIAL ).

              CALL METHOD obj_zcl_webservice_taxa_curva->executar
                EXPORTING
                  i_numero = lw_zsdt0053-nro_sol_ov   " Numero de Solicitação de Ordem de Venda
                  i_tipo   = 'EDI'                    " Nº item do Tipo de Solicitação
                  i_tcode  = 'VF11'                   " Código de transação atual
                  i_vbeln  = xvbrp-aubel.             " Nº documento de vendas e distribuição

            ELSE.
              IF lw_zsdt0053 IS NOT INITIAL.
                CALL METHOD obj_zcl_webservice_taxa_curva->executar
                  EXPORTING
                    i_numero  = lw_zsdt0053-nro_sol_ov   " Numero de Solicitação de Ordem de Venda
                    i_tipo    = 'EDF'                    " Nº item do Tipo de Solicitação
                    i_fixacao = lw_zsdt0053-fixacao      " Nº item do documento de vendas e distribuição
                    i_status  = lw_zsdt0053-status
                    i_tcode   = 'VF11'                   " Código de transação atual
                    i_vbeln   = xvbrp-aubel              " Nº documento de vendas e distribuição
                    i_auart   = w_vbak-auart.             " Tipo de documento de vendas

              ELSE.
                CALL METHOD obj_zcl_webservice_taxa_curva->executar
                  EXPORTING
                    i_numero  = lw_zsdt0100-nro_sol_ov   " Numero de Solicitação de Ordem de Venda
                    i_tipo    = 'EDF'                    " Nº item do Tipo de Solicitação
                    i_fixacao = lw_zsdt0100-fixacao      " Nº item do documento de vendas e distribuição
                    i_status  = lw_zsdt0100-status
                    i_tcode   = 'VF11'                   " Código de transação atual
                    i_vbeln   = xvbrp-aubel              " Nº documento de vendas e distribuição
                    i_auart   = w_vbak-auart.            " Tipo de documento de vendas
              ENDIF.
            ENDIF.

          CATCH zcx_webservice INTO cx_exception.
            var_msg = cx_exception->get_text( ).
            MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH var_msg.
        ENDTRY.

      WHEN 'IN'.

        UPDATE zsdt0090  SET estorno       = abap_true
                             usnam_e       = sy-uname
                             data_atual_e  = sy-datum
                             hora_atual_e  = sy-uzeit
                             origem_est    = sy-cprog
               WHERE vbeln EQ xvbrp-aubel
                 AND categoria EQ var_dir
                 AND estorno EQ abap_false.

        CHECK sy-subrc IS INITIAL.
        COMMIT WORK.

        CALL METHOD zcl_webservice_tx_curva=>hedge_insumos
          EXPORTING
            i_tipo  = 'EST'
            i_vbeln = xvbrp-aubel
            i_dir   = var_dir.

    ENDCASE.
  ENDIF.

ENDFUNCTION.
