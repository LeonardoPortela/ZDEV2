************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 29.06.2009                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Geração de Documento originados do SIGAM            *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 29.06.2009    Marcus Barbara       Criação                 *
************************************************************************
REPORT  zfi_xi_document.

START-OF-SELECTION.

  DATA: it_document          TYPE TABLE OF zfie_document INITIAL SIZE 0 WITH HEADER LINE,
        it_documen2          TYPE TABLE OF zfie_document INITIAL SIZE 0 WITH HEADER LINE,
        it_documen3          TYPE TABLE OF zfie_document INITIAL SIZE 0 WITH HEADER LINE,
        it_performs          TYPE TABLE OF zib_contabil_ext INITIAL SIZE 0 WITH HEADER LINE,
        it_header            LIKE TABLE OF  zfie_documentheader,
        it_item              LIKE TABLE OF  zfie_documentitem,
        it_return            LIKE TABLE OF  zfie_return,
        it_zib_contabil      TYPE TABLE OF zib_contabil,
        it_zib_contabil_aux  TYPE TABLE OF zib_contabil,
        it_zib_contabil_cont TYPE TABLE OF zib_contabil,
        wa_document          TYPE zfie_document,
        it_lote_item         TYPE TABLE OF zpfe_lote_item WITH HEADER LINE,
        wa_ciot              TYPE zcte_ciot,
        wa_documen3          TYPE zfie_document,
        wa_return	           LIKE zfie_return,
        chave_referencia     TYPE RANGE OF zib_contabil-obj_key,
        rg_atualizado        TYPE c LENGTH 1 VALUE 'N'.

  DATA: vg_job      TYPE i.
  DATA: xv_jobnm TYPE btcjob.
  DATA: xv_stepc TYPE btcstepcnt.
  DATA: vnr_registros TYPE zib_contabil-nr_registros.

  DATA: BEGIN OF raw_enq OCCURS 0.
          INCLUDE STRUCTURE seqg7.
        DATA: END OF raw_enq.

  "Documentos de Cockpit de Administradora de Pagamento de Frete Terceiro*************
  DATA: wa_zpfe_lote      TYPE zpfe_lote,
        wa_zpfe_lote_item TYPE zpfe_lote_item,
        wa_msg_lote       TYPE zpfe_lote_msg,
        seqimsg           TYPE ze_seqlan.
************************************************************************************

  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
*     EVENTID         =
*     EVENTPARM       =
*     EXTERNAL_PROGRAM_ACTIVE       =
*     JOBCOUNT        = XV_JOBCN
      jobname         = xv_jobnm
      stepcount       = xv_stepc
    EXCEPTIONS
      no_runtime_info = 1
      OTHERS          = 2.

  IF xv_jobnm = 'ENVIO_CONTAS_RECEBER/PAGAR'.
    SELECT SINGLE COUNT(*) INTO vg_job
     FROM tbtco
    WHERE jobname EQ 'ENVIO_CONTAS_RECEBER/PAGAR'
      AND status EQ 'R'.
  ELSEIF xv_jobnm = 'ENVIO_CONTAS_HOLANDA'.
    SELECT SINGLE COUNT(*) INTO vg_job
     FROM tbtco
    WHERE jobname EQ 'ENVIO_CONTAS_HOLANDA'
      AND status EQ 'R'.
  ELSEIF xv_jobnm = 'IMOBILIZADO_AJUSTE_FISCAL'.
    SELECT SINGLE COUNT(*) INTO vg_job
       FROM tbtco
      WHERE jobname EQ 'IMOBILIZADO_AJUSTE_FISCAL'
        AND status EQ 'R'.
  ELSE.
    SELECT SINGLE COUNT(*) INTO vg_job
     FROM tbtco
    WHERE jobname EQ 'ENVIO_AP_FRETE'
      AND status EQ 'R'.
  ENDIF.

  IF ( vg_job EQ 1 ).
    "ALRS
    IF xv_jobnm = 'ENVIO_CONTAS_RECEBER/PAGAR'.

      SELECT *
        FROM zib_contabil
        INTO TABLE it_zib_contabil
        WHERE rg_atualizado EQ rg_atualizado
          AND obj_key       IN chave_referencia
          AND interface     NE 98
          AND interface     NE 53
          AND interface     NE 54
          AND bukrs         NE '0201'
          AND bukrs         NE '0202'.

      " Iva argentina
      SELECT *
       FROM zib_contabil
       APPENDING TABLE it_zib_contabil
       WHERE rg_atualizado EQ rg_atualizado
         AND obj_key       IN chave_referencia
         AND interface     EQ 54
         AND bukrs         NE '0201'
         AND bukrs         NE '0202'
         AND EXISTS ( SELECT * FROM zmmt_eeiva_zgr WHERE obj_key = zib_contabil~obj_key ).

*        SELECT *
*          FROM zib_contabil
*          INTO TABLE it_zib_contabil
*          WHERE rg_atualizado EQ 'N'
*            AND interface     NE 98
*            AND interface     NE 53
*            AND interface     NE 54
*            AND bukrs         NE '0201'
*            AND bukrs         NE '0202'.
*        " Iva argentina
*        SELECT *
*         FROM zib_contabil
*         APPENDING TABLE it_zib_contabil
*         WHERE rg_atualizado EQ 'N'
*           AND interface     EQ 54
*           AND bukrs         NE '0201'
*           AND bukrs         NE '0202'
*           AND EXISTS ( SELECT * FROM zmmt_eeiva_zgr WHERE obj_key = zib_contabil~obj_key ).

    ELSEIF xv_jobnm = 'ENVIO_CONTAS_HOLANDA'.
      SELECT *
        FROM zib_contabil
        INTO TABLE it_zib_contabil
        WHERE rg_atualizado EQ 'N'
          AND interface     NE 98
          AND interface     NE 53
          AND bukrs         IN ( '0201', '0202' ).
    ELSEIF xv_jobnm = 'IMOBILIZADO_AJUSTE_FISCAL'.
      SELECT *
        FROM zib_contabil
        INTO TABLE it_zib_contabil
        WHERE rg_atualizado EQ 'N'
          AND interface     EQ 53.
    ELSE.
      SELECT * UP TO 1 ROWS
        FROM zib_contabil
        APPENDING TABLE it_zib_contabil
        WHERE rg_atualizado EQ 'N'
          AND interface     EQ 98.
    ENDIF.

    SORT it_zib_contabil BY obj_key.

*    IT_ZIB_CONTABIL_AUX[] = IT_ZIB_CONTABIL[]. "copia todas as linhas para contar depois

    DELETE ADJACENT DUPLICATES FROM it_zib_contabil COMPARING obj_key.

    CHECK it_zib_contabil[] IS NOT INITIAL.

*    IT_ZIB_CONTABIL_CONT[] = IT_ZIB_CONTABIL[]. " Contador de registros
*
*    DELETE IT_ZIB_CONTABIL_CONT WHERE NR_REGISTROS = 0. " Elimina os zerados
*
*    LOOP AT IT_ZIB_CONTABIL_CONT INTO DATA(WA_ZIB_CONTABIL).
*      VNR_REGISTROS = 0.
*      LOOP AT IT_ZIB_CONTABIL_AUX INTO DATA(WA_ZIB_CONTABIL_AUX)  WHERE OBJ_KEY = WA_ZIB_CONTABIL-OBJ_KEY.
*        ADD 1 TO VNR_REGISTROS.
*      ENDLOOP.
*      IF VNR_REGISTROS NE WA_ZIB_CONTABIL-NR_REGISTROS.
*        UPDATE ZIB_CONTABIL SET QUANTITY = 999999 "Marca erros com registros com 999999
*        WHERE OBJ_KEY = WA_ZIB_CONTABIL-OBJ_KEY.
*      ENDIF.
*    ENDLOOP.
*    COMMIT WORK.

    CALL FUNCTION 'Z_ENQUEUE_ZIB_CONTABIL'
      EXPORTING
        mode = 'S'.

    SELECT obj_key seqitem bschl gsber bukrs interface bktxt bldat
           budat gjahr monat blart xblnr hkont wrbtr waers zfbdt
           zlspr zlsch kidno sgtxt xref1 xref2 xref3 bupla zuonr
           umskz kostl aufnr prctr waers_i dmbtr waers_f dmbe2
           bvtyp hbkid bankl bankn bewar waers_g dmbe3 tax_code matnr vbund land1
           quantity base_uom  controle_vat matnr_fi hkont2 valut esrnr esrre werks
           wf_saldo_finan_a zterm vl_bc_iva_wrbtr vl_bc_iva_dmbtr vl_bc_iva_dmbe2 vornr vbeln rldnr kurrf
      INTO CORRESPONDING FIELDS OF TABLE it_document
      FROM zib_contabil
      FOR ALL ENTRIES IN it_zib_contabil
     WHERE obj_key       EQ it_zib_contabil-obj_key
       AND rg_atualizado EQ 'N'
       AND interface     NE 99
       AND interface     NE 98
       AND interface     NE 97.

    "99 - São dados de pagamento de estadia de frete de terceiro (Outros)
    "98 - Pagamento a Administradora de Pagamento de Frete
    "97 - Pagamento a Administradora de Pagamento de Frete - Partidas Contábeis (Itens)

    CALL FUNCTION 'Z_FI_MANYDOCUMENT_PROCESSA'
      TABLES
        it_document = it_document.

    SELECT obj_key seqitem bschl gsber bukrs interface bktxt bldat
           budat gjahr monat blart xblnr hkont wrbtr waers zfbdt
           zlspr zlsch kidno sgtxt xref1 xref2 xref3 bupla zuonr
           umskz kostl aufnr prctr waers_i dmbtr waers_f dmbe2
           bvtyp hbkid bankl bankn bewar waers_g dmbe3 tax_code matnr vbund land1
           quantity base_uom controle_vat matnr_fi hkont2 valut esrnr esrre werks
           wf_saldo_finan_a zterm vl_bc_iva_wrbtr vl_bc_iva_dmbtr vl_bc_iva_dmbe2 vornr vbeln rldnr kurrf
      INTO CORRESPONDING FIELDS OF TABLE it_document
      FROM zib_contabil
      FOR ALL ENTRIES IN it_zib_contabil
     WHERE obj_key       EQ it_zib_contabil-obj_key
       AND rg_atualizado EQ 'N'
       AND interface     EQ 98.
*** inclusao igor vilela - performs dinamicos apos execucao do job. - inicio
    SELECT *
      FROM zib_contabil_ext
      INTO TABLE it_performs
       FOR ALL ENTRIES IN it_document
       WHERE obj_key EQ it_document-obj_key.
*** inclusao igor vilela - performs dinamicos apos execucao do job. - fim


    MOVE it_document[] TO it_documen2[].
    SORT it_documen2 BY obj_key.
    DELETE ADJACENT DUPLICATES FROM it_documen2 COMPARING obj_key.

    LOOP AT it_documen2 INTO wa_document.

      CLEAR: it_documen3[], it_header[], it_item[], it_return[], wa_zpfe_lote.

      SELECT SINGLE * INTO wa_zpfe_lote
        FROM zpfe_lote
       WHERE nm_lote EQ wa_document-obj_key(10).

      IF NOT sy-subrc IS INITIAL .
        DELETE FROM zib_contabil WHERE obj_key EQ wa_document-obj_key.
        COMMIT WORK.
        CONTINUE.
      ENDIF.

      LOOP AT it_document INTO wa_documen3 WHERE obj_key EQ wa_document-obj_key.
        APPEND wa_documen3 TO it_documen3.
      ENDLOOP.

      CALL FUNCTION 'Z_FI_MANYDOCUMENT_PROCESSA'
        TABLES
          it_document = it_documen3
          it_header   = it_header
          it_item     = it_item
          it_return   = it_return.

      seqimsg = 0.

      LOOP AT it_return INTO wa_return.

        seqimsg = seqimsg + 1.

        IF ( wa_return-type EQ 'S' AND wa_return-id = 'Z01' AND wa_return-number EQ '003' ).
          wa_zpfe_lote-status   =  'F'.
          wa_zpfe_lote-belnr    = wa_return-message_v1(10).
          wa_zpfe_lote-gjahr    = wa_return-message_v3(04).
          wa_zpfe_lote-dt_belnr = sy-datum.
          wa_zpfe_lote-hr_belnr = sy-uzeit.
          MODIFY zpfe_lote FROM wa_zpfe_lote.
          COMMIT WORK.
        ENDIF.

        wa_msg_lote-obj_key    = wa_document-obj_key.
        wa_msg_lote-seqmsg     = seqimsg.
        wa_msg_lote-type       = wa_return-type.
        wa_msg_lote-id         = wa_return-id.
        wa_msg_lote-numero     = wa_return-number.
        wa_msg_lote-message    = wa_return-message.
        wa_msg_lote-log_no     = wa_return-log_no.
        wa_msg_lote-log_msg_no = wa_return-log_msg_no.
        wa_msg_lote-message_v1 = wa_return-message_v1.
        wa_msg_lote-message_v2 = wa_return-message_v2.
        wa_msg_lote-message_v3 = wa_return-message_v3.
        wa_msg_lote-message_v4 = wa_return-message_v4.
        MODIFY zpfe_lote_msg FROM wa_msg_lote.
      ENDLOOP.

      IF wa_zpfe_lote-belnr IS INITIAL.
        wa_zpfe_lote-status = 'E'.
        MODIFY zpfe_lote FROM wa_zpfe_lote.
        COMMIT WORK.
      ENDIF.

      READ TABLE it_performs
        WITH KEY obj_key = wa_document-obj_key.

      IF sy-subrc IS INITIAL.
        PERFORM (it_performs-formname) IN PROGRAM (it_performs-repid) IF FOUND
                                       USING wa_document-obj_key.
      ENDIF.

    ENDLOOP.

    PERFORM leitura_interface_pf_e.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  LEITURA_INTERFACE_PF_E
*&---------------------------------------------------------------------*
*       Pagamento de Frete Via Admnistradora (PF-e)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM leitura_interface_pf_e .

  DATA: ck_fazer TYPE sy-subrc.

  ck_fazer = 1.

  "Corrigir Falhas Anteriores
  "Verificar se Existe algum lote não concluído com itens concluídos
  SELECT * INTO TABLE @DATA(it_zpfe_lote)
    FROM zpfe_lote AS ll
   WHERE status NE 'C'
     AND NOT EXISTS ( SELECT * FROM zpfe_lote_item AS ii WHERE ii~nm_lote EQ ll~nm_lote AND ii~status NE 'C' )
     AND EXISTS ( SELECT * FROM zpfe_lote_item AS ii WHERE ii~nm_lote EQ ll~nm_lote ).

  "Finaliza Lotes
  LOOP AT it_zpfe_lote INTO DATA(lc_zpfe_lote).
    lc_zpfe_lote-status = 'C'.
    MODIFY zpfe_lote FROM lc_zpfe_lote.
  ENDLOOP.

  COMMIT WORK.

  TRY .

      CLEAR: it_document[], it_documen2[], it_documen3[].

      SELECT obj_key seqitem bschl gsber bukrs interface bktxt bldat
             budat gjahr monat blart xblnr hkont wrbtr waers zfbdt
             zlspr zlsch kidno sgtxt xref1 xref2 xref3 bupla zuonr
             umskz kostl aufnr prctr waers_i dmbtr waers_f dmbe2
             bvtyp hbkid bankl bankn bewar waers_g dmbe3  tax_code matnr vbund land1
             quantity base_uom  controle_vat matnr_fi hkont2 valut kurrf
        INTO CORRESPONDING FIELDS OF TABLE it_document
              FROM zib_contabil
        FOR ALL ENTRIES IN it_zib_contabil
       WHERE obj_key       EQ it_zib_contabil-obj_key
         AND rg_atualizado EQ 'N'
         AND interface     EQ 97.

      IF sy-subrc IS INITIAL.
        ck_fazer = 0.
      ENDIF.

      MOVE it_document[] TO it_documen2[].
      SORT it_documen2 BY obj_key.
      DELETE ADJACENT DUPLICATES FROM it_documen2 COMPARING obj_key.

      LOOP AT it_documen2 INTO wa_document.

        CLEAR: it_documen3[], it_header[], it_item[], it_return[], wa_zpfe_lote.

        SELECT SINGLE * INTO wa_zpfe_lote_item
          FROM zpfe_lote_item
         WHERE obj_key EQ wa_document-obj_key.

        IF NOT sy-subrc IS INITIAL .
          DELETE FROM zib_contabil WHERE obj_key EQ wa_document-obj_key.
          COMMIT WORK.
          CONTINUE.
        ENDIF.

        LOOP AT it_document INTO wa_documen3 WHERE obj_key EQ wa_document-obj_key.
          APPEND wa_documen3 TO it_documen3.
        ENDLOOP.

        CALL FUNCTION 'Z_FI_MANYDOCUMENT_PROCESSA'
          TABLES
            it_document = it_documen3
            it_header   = it_header
            it_item     = it_item
            it_return   = it_return.

        seqimsg = 0.

        LOOP AT it_return INTO wa_return.
          seqimsg = seqimsg + 1.
          IF ( wa_return-type EQ 'S' AND wa_return-id = 'Z01' AND wa_return-number EQ '003' ).
            wa_zpfe_lote_item-belnr = wa_return-message_v1(10).

            SELECT * INTO TABLE it_lote_item
              FROM zpfe_lote_item
             WHERE obj_key EQ wa_document-obj_key.

            IF sy-subrc IS INITIAL.
              LOOP AT it_lote_item.
                it_lote_item-status   =  'C'.
                it_lote_item-belnr    = wa_return-message_v1(10).
                it_lote_item-gjahr    = wa_return-message_v3(04).
                it_lote_item-dt_belnr = sy-datum.
                it_lote_item-hr_belnr = sy-uzeit.
                MODIFY zpfe_lote_item FROM it_lote_item.

                IF it_lote_item-chvid EQ '2'.
                  SELECT SINGLE * INTO wa_ciot
                    FROM zcte_ciot
                   WHERE docnum EQ it_lote_item-docnum.

                  IF sy-subrc IS INITIAL.
                    wa_ciot-st_ciot = '6'.
                    MODIFY zcte_ciot FROM wa_ciot.
                  ENDIF.
                ENDIF.
                COMMIT WORK.
              ENDLOOP.
            ENDIF.
          ENDIF.

          wa_msg_lote-obj_key    = wa_document-obj_key.
          wa_msg_lote-seqmsg     = seqimsg.
          wa_msg_lote-type       = wa_return-type.
          wa_msg_lote-id         = wa_return-id.
          wa_msg_lote-numero     = wa_return-number.
          wa_msg_lote-message    = wa_return-message.
          wa_msg_lote-log_no     = wa_return-log_no.
          wa_msg_lote-log_msg_no = wa_return-log_msg_no.
          wa_msg_lote-message_v1 = wa_return-message_v1.
          wa_msg_lote-message_v2 = wa_return-message_v2.
          wa_msg_lote-message_v3 = wa_return-message_v3.
          wa_msg_lote-message_v4 = wa_return-message_v4.
          MODIFY zpfe_lote_msg FROM wa_msg_lote.
        ENDLOOP.

        IF wa_zpfe_lote_item-belnr IS INITIAL.
          UPDATE zpfe_lote_item
             SET status   =  'E'
           WHERE obj_key EQ wa_document-obj_key.
          COMMIT WORK.
        ENDIF.

      ENDLOOP.

      IF it_document[] IS NOT INITIAL.

        DATA: tl_lote      TYPE TABLE OF zpfe_lote WITH HEADER LINE,
              tl_lote_item TYPE TABLE OF zpfe_lote_item WITH HEADER LINE.

        REFRESH: tl_lote, tl_lote_item.

        "Lotes Não Concluídos
        SELECT * FROM zpfe_lote INTO TABLE tl_lote FOR ALL ENTRIES IN it_document
         WHERE nm_lote EQ it_document-obj_key(10)
           AND status  NE 'C'.

        "Itens Não Concluídos dos Lotes
        IF sy-subrc IS INITIAL.
          SELECT *
            FROM zpfe_lote_item INTO TABLE tl_lote_item
             FOR ALL ENTRIES IN tl_lote
           WHERE nm_lote EQ tl_lote-nm_lote
             AND status  NE 'C'.
        ENDIF.

        "Percorre os Lotes
        LOOP AT tl_lote.
          "Se não existir Itens no Lote não Concluido, marca lote como concluído
          READ TABLE tl_lote_item WITH KEY nm_lote = tl_lote-nm_lote.
          IF sy-subrc IS NOT INITIAL.
            UPDATE zpfe_lote SET status = 'C' WHERE nm_lote EQ tl_lote-nm_lote AND nr_lote_adm EQ tl_lote-nr_lote_adm.
          ENDIF.
        ENDLOOP.

        COMMIT WORK.

      ENDIF.

    CATCH cx_root.
  ENDTRY.

  IF ck_fazer IS INITIAL.
    "Corrigir Falhas Anteriores
    "Verificar se Existe algum lote não concluído com itens concluídos
    SELECT * INTO TABLE @it_zpfe_lote
      FROM zpfe_lote AS ll
     WHERE status NE 'C'
       AND NOT EXISTS ( SELECT * FROM zpfe_lote_item AS ii WHERE ii~nm_lote EQ ll~nm_lote AND ii~status NE 'C' )
       AND EXISTS ( SELECT * FROM zpfe_lote_item AS ii WHERE ii~nm_lote EQ ll~nm_lote ).

    "Finaliza Lotes
    LOOP AT it_zpfe_lote INTO lc_zpfe_lote.
      lc_zpfe_lote-status = 'C'.
      MODIFY zpfe_lote FROM lc_zpfe_lote.
    ENDLOOP.

    COMMIT WORK.
  ENDIF.

ENDFORM.
