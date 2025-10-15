*&---------------------------------------------------------------------*
*&  Include           ZBAPI_NOTIF_CREATE
*&---------------------------------------------------------------------*

*    CONCATENATE 'EMPRESTIMO PARA UNIDADE' TBX_CENTRO_DESTINO
*           INTO  SHORT_TEXT SEPARATED BY SPACE.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        INPUT         = WA_SAIDA_INFO_EMPRESTIMO-EQUNR
*      IMPORTING
*        OUTPUT        = WA_SAIDA_INFO_EMPRESTIMO-EQUNR.
*
*    IT_NOTIFHEADER-EQUIPMENT  = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR.
*    IT_NOTIFHEADER-SHORT_TEXT = SHORT_TEXT.
*    IT_NOTIFHEADER-PRIORITY   = '3'.
*    IT_NOTIFHEADER-CODE_GROUP = 'F0000010'.
*    IT_NOTIFHEADER-CODING     = '0070'.
*    APPEND IT_NOTIFHEADER.
*  ENDLOOP.
*
*  CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE'
*    EXPORTING
**     EXTERNAL_NUMBER          =
*      NOTIF_TYPE               = 'Z4'
*      NOTIFHEADER              = IT_NOTIFHEADER
**     TASK_DETERMINATION       = ' '
**     SENDER                   =
**     ORDERID                  =
*    IMPORTING
*      NOTIFHEADER_EXPORT       = IT_NOTIFHEADER_EXPORT
*   TABLES
**     NOTITEM                  =
**     NOTIFCAUS                =
**     NOTIFACTV                =
**     NOTIFTASK                =
**     NOTIFPARTNR              =
**     LONGTEXTS                =
**     KEY_RELATIONSHIPS        =
*      RETURN                   = IT_RETURN.
*
*  CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
*    EXPORTING
*      NUMBER      = IT_NOTIFHEADER_EXPORT-NOTIF_NO
*    IMPORTING
*      NOTIFHEADER = IT_NOTIFHEADER_EXPORT
*    TABLES
*      RETURN      = IT_RETURN.
*
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    IMPORTING
*      RETURN = IT_RETURN.
