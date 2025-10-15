FUNCTION Z_FI_MANYDOCUMENT_POST .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_DOCUMENT STRUCTURE  ZFIE_DOCUMENT
*"----------------------------------------------------------------------

  DATA: WA_OBJ_KEY       LIKE ZIB_CONTABIL-OBJ_KEY,
        WA_ZIB_CONTABIL2 LIKE ZIB_CONTABIL,
        WA_ZIB_CONTABIL  LIKE ZIB_CONTABIL,
        WA_LOG_CONTABIL  LIKE ZIB_CONTABIL_LOG.

  DEFINE INSERT_IB_CONTABIL.
    IF WA_DOCUMENT-bukrs = '0100' .
        clear WA_DOCUMENT-BUPLA.
    ENDIF.
    MOVE-CORRESPONDING WA_DOCUMENT TO WA_ZIB_CONTABIL.
    WA_ZIB_CONTABIL-RG_ATUALIZADO = 'N'.
    MODIFY ZIB_CONTABIL FROM WA_ZIB_CONTABIL.
    "INSERT INTO ZIB_CONTABIL VALUES WA_ZIB_CONTABIL.

    MOVE-CORRESPONDING WA_ZIB_CONTABIL TO WA_LOG_CONTABIL.
    WA_LOG_CONTABIL-DTREG = SY-DATUM.
    WA_LOG_CONTABIL-HRREG = SY-UZEIT.

    MODIFY ZIB_CONTABIL_LOG FROM WA_LOG_CONTABIL.
    "INSERT INTO ZIB_CONTABIL_LOG VALUES WA_LOG_CONTABIL.
  END-OF-DEFINITION.


* Percorrendo informações contábeis enviados pelo XI
  LOOP AT IT_DOCUMENT INTO WA_DOCUMENT.

* Verificando se existe chave de referencia e sequencia de envio igual
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF WA_ZIB_CONTABIL2
      FROM ZIB_CONTABIL
     WHERE OBJ_KEY EQ WA_DOCUMENT-OBJ_KEY
       AND SEQITEM EQ WA_DOCUMENT-SEQITEM.

* Se não existe insere no banco de dados
    IF SY-SUBRC NE 0.
      INSERT_IB_CONTABIL.
    ELSE.
*Se existe apaga o antigo e insere novamente o atual
*Corrige erros de envios anteriores somente com o reenvio
*sem a necessidade de alterar a chave (ID_ENVIADO_SAP)
      IF WA_ZIB_CONTABIL2-RG_ATUALIZADO EQ 'S'.
        DELETE FROM ZIB_CONTABIL
         WHERE OBJ_KEY EQ WA_DOCUMENT-OBJ_KEY
           AND SEQITEM EQ WA_DOCUMENT-SEQITEM.
        INSERT_IB_CONTABIL.
      ENDIF.
    ENDIF.

  ENDLOOP.

  COMMIT WORK.

ENDFUNCTION.
