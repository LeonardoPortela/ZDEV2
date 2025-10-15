FUNCTION z_proc_ent_remessa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_MANUAL) TYPE  CHAR1 DEFAULT 'X'
*"  TABLES
*"      IT_NOTAS_SAIDA STRUCTURE  ZSDT_ENTRADA_REM OPTIONAL
*"      IT_RETORNO STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      SEM_SAIDAS
*"      COM_SAIDAS
*"----------------------------------------------------------------------

  DATA: l_subrc    TYPE sy-subrc,
        p_vbeln    TYPE vbeln_va,
        p_vbeln_vl TYPE vbeln_vl,
        vg_tabix   TYPE sy-tabix,
        wa_remessa TYPE ty_remessas,
        deliverys  TYPE TABLE OF ty_remessas INITIAL SIZE 0 WITH HEADER LINE,
        wa_likp    TYPE likp.

  IF ( it_notas_saida[] IS INITIAL ) AND ( p_manual IS NOT INITIAL ).
    MESSAGE e000 RAISING sem_saidas.
  ELSEIF ( it_notas_saida[] IS NOT INITIAL ) AND ( p_manual IS INITIAL ).
    MESSAGE e001 RAISING com_saidas.
  ENDIF.

  IF ( it_notas_saida[] IS INITIAL ).
    "Processo automático
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_notas_saida
      FROM zsdt_entrada_rem
     WHERE tp_status EQ vg_status.

  ENDIF.

  CLEAR: t_messtab[], it_retorno[].

  SORT  it_notas_saida BY vbeln.

  LOOP AT it_notas_saida INTO wa_entrada_rem WHERE tp_status EQ vg_status.

    vg_tabix = sy-tabix.

    SELECT SINGLE vbeln INTO wa_remessa-ref_doc
      FROM vbak
     WHERE vgbel EQ wa_entrada_rem-vbeln.

    IF sy-subrc EQ 0.

      SELECT SINGLE vbeln INTO wa_remessa-numero
        FROM vbfa
       WHERE vbelv   EQ wa_remessa-ref_doc
         AND vbtyp_n EQ 'T'.

      SELECT SINGLE * INTO wa_likp
        FROM likp
       WHERE vbeln EQ wa_remessa-numero.

      it_notas_saida-dt_chegada  = wa_likp-erdat.
      it_notas_saida-qt_chegada  = wa_likp-ntgew.
      it_notas_saida-vbeln_ord_e = wa_remessa-numero.
      it_notas_saida-vbeln_e     = wa_remessa-ref_doc.
      it_notas_saida-tp_status   = 'X'.
      MODIFY it_notas_saida INDEX vg_tabix TRANSPORTING vbeln_ord_e vbeln_e.

      wa_entrada_rem-dt_chegada  = wa_likp-erdat.
      wa_entrada_rem-qt_chegada  = wa_likp-ntgew.
      wa_entrada_rem-vbeln_ord_e = wa_remessa-numero.
      wa_entrada_rem-vbeln_e     = wa_remessa-ref_doc.
      wa_entrada_rem-tp_status   = 'X'.
      MODIFY zsdt_entrada_rem FROM wa_entrada_rem.

      COMMIT WORK.

      CLEAR: w_messtab.
      w_messtab-tcode  = sy-tcode.
      w_messtab-msgtyp = 'S'.
      w_messtab-msgnr  = '003'.
      w_messtab-msgv1  = wa_entrada_rem-vbeln.
      w_messtab-msgv2  = 'Entrada referente a fatura somente atualizado!'.
      APPEND w_messtab TO t_messtab.

    ELSE.

      SELECT SINGLE MAX( sq_proc ) INTO wa_log-sq_proc
        FROM zsdt_entrada_log
       WHERE vbeln  EQ wa_entrada_rem-vbeln
         AND docnum EQ wa_entrada_rem-docnum.

      IF wa_log-sq_proc IS INITIAL.
        wa_log-sq_proc = 1.
      ENDIF.

      wa_log-vbeln  = wa_entrada_rem-vbeln.
      wa_log-docnum = wa_entrada_rem-docnum.

      CLEAR: wa_romaneio.

      SELECT SINGLE * INTO wa_romaneio
        FROM zsdt0001
       WHERE doc_rem EQ wa_entrada_rem-vbeln_s
         AND tp_movimento EQ 'E'.

      "Criar Ordem de Compra
      PERFORM cria_ordem_compra USING wa_entrada_rem l_subrc p_vbeln.

      IF l_subrc EQ 0.

        "Criar Remessa
        PERFORM criar_delivery TABLES deliverys USING wa_entrada_rem p_vbeln wa_romaneio l_subrc.

        IF l_subrc EQ 0.

          "Gerar entrada
          PERFORM picking_delivery TABLES deliverys USING wa_romaneio wa_entrada_rem .

          LOOP AT deliverys INTO wa_remessa.

            IF wa_remessa-l_subrc EQ 0.

              it_notas_saida-vbeln_ord_e = wa_remessa-numero.
              it_notas_saida-vbeln_e     = wa_remessa-ref_doc.
              it_notas_saida-tp_status   = 'X'.
              MODIFY it_notas_saida INDEX vg_tabix TRANSPORTING vbeln_ord_e vbeln_e.

              wa_entrada_rem-vbeln_ord_e = wa_remessa-numero.
              wa_entrada_rem-vbeln_e     = wa_remessa-ref_doc.
              wa_entrada_rem-tp_status   = 'X'.
              MODIFY zsdt_entrada_rem FROM wa_entrada_rem.

              IF wa_romaneio IS NOT INITIAL.
                wa_romaneio-status = 'X'.
                MODIFY zsdt0001 FROM wa_romaneio.
              ENDIF.

              COMMIT WORK.

            ELSE.
              "Estornar Delivery e Ordem de Compra
              PERFORM estornar_delivery USING wa_remessa-numero l_subrc.
              " Estornar Ordem de Compra
              IF l_subrc EQ 0.
                PERFORM estornar_ordem_compra USING p_vbeln.
              ENDIF.
            ENDIF.

          ENDLOOP.

        ELSE.
          " Estornar Ordem de Compra
          PERFORM estornar_ordem_compra USING p_vbeln.
        ENDIF.

      ENDIF.

    ENDIF.

    LOOP AT t_messtab INTO w_messtab.
      MOVE-CORRESPONDING w_messtab TO wa_log.
      wa_log-sq_item = sy-tabix.
      MODIFY zsdt_entrada_log FROM wa_log.
      COMMIT WORK.
    ENDLOOP.

    MOVE t_messtab[] TO it_retorno[].

  ENDLOOP.

ENDFUNCTION.
