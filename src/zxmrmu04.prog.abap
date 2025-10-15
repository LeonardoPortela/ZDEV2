*&---------------------------------------------------------------------*
*&  Include           ZXMRMU04
*&---------------------------------------------------------------------*

 DATA: e_fardo     TYPE zpme0059.
 DATA: t_fardo     TYPE zpmt0059.


 DATA: obj_trace TYPE REF TO zcl_webservice_trace.
 FREE: obj_trace.

 CREATE OBJECT: obj_trace.

 DATA: w_doc       LIKE LINE OF i_doc-mdoc,
       wa_zppt0006 TYPE zppt0006,
       vid         TYPE zppt0006-id.

 LOOP AT i_doc-mdoc INTO w_doc.
   IF w_doc-mblnr IS NOT INITIAL. "Documento estorno gerado
     EXIT.
   ENDIF.
 ENDLOOP.

*-CS2021000713 - 15.10.2021 - JT - inicio
*------------------------------------
*- checa permissao de usuario para estornar ALGODAO
*------------------------------------
 SELECT *
   FROM zppt0027
   INTO TABLE @DATA(t_zppt0027)
  WHERE usuario = @sy-uname.

 IF t_zppt0027[] IS NOT INITIAL.
   SELECT werks
     INTO TABLE @DATA(t_t001w)
     FROM t001w
      FOR ALL ENTRIES IN @t_zppt0027
    WHERE werks >= @t_zppt0027-werks_from
      AND werks <= @t_zppt0027-werks_to.

   SORT t_t001w BY werks.
   DELETE ADJACENT DUPLICATES FROM t_t001w
                         COMPARING werks.
 ENDIF.
*-CS2021000713 - 15.10.2021 - JT - fim

 vg_interface = '39'.
 REFRESH it_outreturn.
 SELECT  * " Verifica se é estorno do fardinho
   FROM zppt0002
   INTO TABLE it_zppt0002
   FOR ALL ENTRIES IN i_doc_c-mdoc
   WHERE mblnr = i_doc_c-mdoc-mblnr
   AND   status_registro NE '05'.

 IF sy-subrc = 0.

*-CS2021000713 - 15.10.2021 - JT - inicio
   LOOP AT it_zppt0002 INTO wa_zppt0002.
     READ TABLE t_t001w INTO DATA(w_t001w) WITH KEY werks = wa_zppt0002-werks
                                           BINARY SEARCH.
     IF sy-subrc <> 0.
       DATA(l_erro) = abap_true.
       EXIT.
     ENDIF.
   ENDLOOP.

   IF l_erro = abap_true.
     MESSAGE e024(sd) WITH 'Usuário sem permissão para efetuar '
                           'Estorno deste Processo!'
                      RAISING exception.
   ENDIF.
*-CS2021000713 - 15.10.2021 - JT - fim

   READ TABLE it_zppt0002 INTO wa_zppt0002 INDEX 1.
   "
   UPDATE zppt0002 SET status_registro = '05' "Estorno Fardinho
   WHERE acharg = wa_zppt0002-acharg
   AND   werks  = wa_zppt0002-werks
   AND   status_registro NE '05'.


   CONCATENATE wa_zppt0002-acharg '|' wa_zppt0002-werks INTO vg_obj_key.
   wa_outreturn-obj_key        = vg_obj_key.
   wa_outreturn-interface      = vg_interface.
   wa_outreturn-id             = vg_interface.
   wa_outreturn-num            = vg_interface.
   CONCATENATE 'Estorno material fardinho:' w_doc-mblnr INTO wa_outreturn-message.
   wa_outreturn-message_v1     = '05'.
   wa_outreturn-message_v2     = ''.
   wa_outreturn-type           = 'S'.
   wa_outreturn-dt_atualizacao = sy-datum.
   wa_outreturn-hr_atualizacao = sy-uzeit.
   APPEND wa_outreturn TO it_outreturn.

   SELECT MAX( id ) INTO vid
     FROM zppt0006
   WHERE charg  = wa_zppt0002-charg
   AND   acharg = wa_zppt0002-acharg
   AND   werks  = wa_zppt0002-werks.
   "
   ADD 1 TO vid.
   wa_zppt0006-werks             = wa_zppt0002-werks.
   wa_zppt0006-charg             = wa_zppt0002-charg.
   wa_zppt0006-acharg            = wa_zppt0002-acharg.
   wa_zppt0006-id_fardinho       = wa_zppt0002-id_fardinho.
   wa_zppt0006-id_fardao         = wa_zppt0002-id_fardao.
   wa_zppt0006-id                = vid.
   wa_zppt0006-budat             = wa_zppt0002-budat.
   wa_zppt0006-matnr             = wa_zppt0002-matnr.
   wa_zppt0006-verid             = wa_zppt0002-verid.
   wa_zppt0006-lgort             = wa_zppt0002-lgort.
   wa_zppt0006-cd_classificacao  = wa_zppt0002-cd_classificacao.
   wa_zppt0006-msgnr             = '000'.
   wa_zppt0006-status_msg        = 'S'.
   wa_zppt0006-cd_mensagem       = wa_outreturn-message.
   wa_zppt0006-data              = sy-datum.
   wa_zppt0006-hora              = sy-uzeit.
   wa_zppt0006-id_cotton         = wa_zppt0002-id_cotton.
   wa_zppt0006-flag_envio        = 'W'.
   wa_zppt0006-status_registro   = wa_outreturn-message_v1+0(3).
   MODIFY zppt0006 FROM wa_zppt0006.
   "
   SORT it_outreturn BY obj_key interface.
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*   CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*     DESTINATION 'XI_SIGAM_RETURN'
*     TABLES
*       outreturn = it_outreturn.

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

 ELSE.
   SELECT  * " Verifica se é estorno do fardão
   FROM zppt0002
   INTO TABLE it_zppt0002
   FOR ALL ENTRIES IN i_doc_c-mdoc
   WHERE mblnr02 = i_doc_c-mdoc-mblnr
   AND   status_registro NE '06'.

   IF sy-subrc = 0.

*-CS2021000713 - 15.10.2021 - JT - inicio
     LOOP AT it_zppt0002 INTO wa_zppt0002.
       READ TABLE t_t001w INTO w_t001w WITH KEY werks = wa_zppt0002-werks
                                       BINARY SEARCH.
       IF sy-subrc <> 0.
         l_erro = abap_true.
         EXIT.
       ENDIF.
     ENDLOOP.

     IF l_erro = abap_true.
       MESSAGE e024(sd) WITH 'Usuário sem permissão para efetuar '
                             'Estorno deste Processo!'
                        RAISING exception.
     ENDIF.
*-CS2021000713 - 15.10.2021 - JT - fim

     READ TABLE it_zppt0002 INTO wa_zppt0002 INDEX 1.
     "
     UPDATE zppt0002 SET status_registro = '06' "Estorno fardão
     WHERE charg = wa_zppt0002-charg
     AND   werks  = wa_zppt0002-werks
     AND   status_registro NE '06'.
     "
     IF wa_zppt0002-id_cotton IS INITIAL.
       CONCATENATE wa_zppt0002-charg '|' wa_zppt0002-werks INTO vg_obj_key.
     ELSE.
       CONCATENATE wa_zppt0002-id_cotton '|' wa_zppt0002-werks INTO vg_obj_key.
     ENDIF.
     wa_outreturn-obj_key        = vg_obj_key.
     wa_outreturn-interface      = vg_interface.
     wa_outreturn-id             = vg_interface.
     wa_outreturn-num            = vg_interface.
     CONCATENATE 'Estorno material fardão:' w_doc-mblnr INTO wa_outreturn-message.
     wa_outreturn-message_v1     = '06'.
     wa_outreturn-message_v2     = ''.
     wa_outreturn-type           = 'S'.
     wa_outreturn-dt_atualizacao = sy-datum.
     wa_outreturn-hr_atualizacao = sy-uzeit.
     APPEND wa_outreturn TO it_outreturn.
     "
     SELECT MAX( id ) INTO vid
     FROM zppt0006
     WHERE charg  = wa_zppt0002-charg
     AND   acharg = wa_zppt0002-charg
     AND   werks  = wa_zppt0002-werks.
     "
     ADD 1 TO vid.
     wa_zppt0006-charg        = wa_zppt0002-charg.
     wa_zppt0006-acharg       = wa_zppt0002-charg.
     wa_zppt0006-id_fardao    = wa_zppt0002-id_fardao.
     wa_zppt0006-id_fardinho  = 0.
     wa_zppt0006-id           = vid.
     wa_zppt0006-budat        = wa_zppt0002-budat.
     wa_zppt0006-werks        = wa_zppt0002-werks.
     wa_zppt0006-matnr        = wa_zppt0002-matnr.
     wa_zppt0006-verid        = wa_zppt0002-verid.
     wa_zppt0006-msgnr        = '000'.
     wa_zppt0006-status_msg   = 'S'.
     wa_zppt0006-cd_mensagem  = wa_outreturn-message.
     wa_zppt0006-data  = sy-datum.
     wa_zppt0006-hora = sy-uzeit.
     wa_zppt0006-id_cotton    = wa_zppt0002-id_cotton.
     wa_zppt0006-id_cotton    = wa_zppt0002-id_cotton.
     wa_zppt0006-flag_envio   = 'W'.
     wa_zppt0006-status_registro   = wa_outreturn-message_v1+0(3).
     MODIFY zppt0006 FROM wa_zppt0006.

     SORT it_outreturn BY obj_key interface.
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
     CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
       DESTINATION 'XI_SIGAM_RETURN'
       TABLES
         outreturn = it_outreturn.

    CONSTANTS: cc_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = cc_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION cc_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = it_outreturn.
    ELSE.
      CALL FUNCTION cc_fm IN BACKGROUND TASK
        TABLES
          outreturn = it_outreturn.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

     COMMIT WORK.

   ENDIF.

 ENDIF.
*
* IF WA_ZPPT0002 IS NOT INITIAL.
*
*   SELECT SINGLE *
*     FROM ZPPT0002
*     INTO @DATA(W_ZPPT0002)
*     WHERE CHARG = @WA_ZPPT0002-CHARG
*     AND   WERKS = @WA_ZPPT0002-WERKS.
*
*   E_FARDO-NR_FARDO              = W_ZPPT0002-ACHARG.
*   E_FARDO-ID_FILIAL_SAP         = W_ZPPT0002-WERKS.
*   E_FARDO-NR_MAQUINA            = W_ZPPT0002-VERID.
*   E_FARDO-ID_MATERIAL_SAP       = W_ZPPT0002-MATNR.
*   E_FARDO-QUANTIDADE            = W_ZPPT0002-MENGE.
*   IF W_ZPPT0002-ID_COTTON IS INITIAL.
*     E_FARDO-NR_FARDO_ORIGEM       = W_ZPPT0002-CHARG.
*   ELSE.
*     E_FARDO-NR_FARDO_ORIGEM       = W_ZPPT0002-ID_COTTON.
*   ENDIF.
*   E_FARDO-DT_LANCAMENTO         = W_ZPPT0002-BUDAT.
*   CONCATENATE W_ZPPT0002-BUDAT+0(4) '-' W_ZPPT0002-BUDAT+4(2) '-' W_ZPPT0002-BUDAT+6(2) INTO E_FARDO-DT_LANCAMENTO.
*   E_FARDO-DT_DOCUMENTO          = W_ZPPT0002-BLDAT.
*   CONCATENATE W_ZPPT0002-BLDAT+0(4) '-' W_ZPPT0002-BLDAT+4(2) '-' W_ZPPT0002-BLDAT+6(2) INTO E_FARDO-DT_DOCUMENTO.
*   E_FARDO-DT_FABRICACAO         = W_ZPPT0002-DT_FABRICACAO.
*   CONCATENATE W_ZPPT0002-DT_FABRICACAO+0(4) '-' W_ZPPT0002-DT_FABRICACAO+4(2) '-' W_ZPPT0002-DT_FABRICACAO+6(2) INTO E_FARDO-DT_FABRICACAO.
*   E_FARDO-HR_FABRICACAO         = W_ZPPT0002-HR_FABRICACAO.
*   E_FARDO-STATUS                = W_ZPPT0002-STATUS.
*   E_FARDO-USUARIO               = W_ZPPT0002-USNAM.
*   E_FARDO-SAFRA                 = W_ZPPT0002-CD_SAFRA.
*   E_FARDO-ID_FARDINHO           = W_ZPPT0002-ID_FARDINHO.
*   E_FARDO-CD_SAI                = W_ZPPT0002-CD_SAI.
*   E_FARDO-PESO_BRUTO            = W_ZPPT0002-PESO_BRUTO.
*   E_FARDO-PESO_LIQUIDO          = W_ZPPT0002-PESO_LIQUIDO.
*   E_FARDO-ID_FARDAO             = W_ZPPT0002-ID_FARDAO.
*   E_FARDO-CD_CLASSIFICACAO      = W_ZPPT0002-CD_CLASSIFICACAO.
*   E_FARDO-DEPOSITO              = W_ZPPT0002-LGORT.
*   E_FARDO-DOC_MATERIAL_FARDINHO = W_ZPPT0002-MBLNR.
*   E_FARDO-DOC_MATERIAL_FARDAO   = W_ZPPT0002-MBLNR02.
*   E_FARDO-NOME_RESPONSAVEL      = ' '.
*   E_FARDO-DT_ATUALIZACAO        = SY-DATUM.
*   CONCATENATE SY-DATUM+0(4) '-' SY-DATUM+4(2) '-' SY-DATUM+6(2) INTO E_FARDO-DT_ATUALIZACAO.
*   E_FARDO-STATUS_REGISTRO       = W_ZPPT0002-STATUS_REGISTRO.
*   E_FARDO-CD_MENSAGEM           = WA_ZPPT0006-CD_MENSAGEM.
*   E_FARDO-RG_ATUALIZADO         = 1.
*   E_FARDO-QTD_FARDINHOS         = W_ZPPT0002-QTD_FARDINHOS.
*   E_FARDO-PESO_CAROCO           = W_ZPPT0002-PESO_CAROCO.
*   E_FARDO-PESO_FIBRILHA         = W_ZPPT0002-PESO_FIBRILHA.
*   E_FARDO-ROWVERSION            = 0.
*   E_FARDO-SEQ_NUM               = 0.
*   APPEND E_FARDO TO T_FARDO.
*   "
*   DATA(_RET_CODE) = OBJ_TRACE->ATUALIZA_TRACE( T_FARDO = T_FARDO ).
*   IF _RET_CODE = 200.
*     UPDATE ZPPT0006 SET FLAG_ENVIO = 'P'
*     WHERE WERKS  = WA_ZPPT0006-WERKS
*     AND   CHARG  = WA_ZPPT0006-CHARG
*     AND   ACHARG = WA_ZPPT0006-ACHARG
*     AND   ID     = WA_ZPPT0006-ID
*     AND   FLAG_ENVIO = 'R'.
*     COMMIT WORK.
*   ENDIF.
* ENDIF.
