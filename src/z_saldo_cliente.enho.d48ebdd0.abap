"Name: \PR:SAPMV50A\FO:USEREXIT_MOVE_FIELD_TO_LIPS\SE:BEGIN\EI
ENHANCEMENT 0 Z_SALDO_CLIENTE.
*
*   DATA: V_SALDO     TYPE VBAK-NETWR,
*         V_AUART     TYPE VBAK-AUART,
*         V_LFIMG     type LIPS-LFIMG,
*         WA_VBAK     type VBAK,
*         WA_KONV     type KONV,
*         VREFER      TYPE ZSDT0151-CH_REFERENCIA,
*         WA_ZSDT0151 TYPE ZSDT0151,
*         WA_ZSDT0001 TYPE ZSDT0001,
*         T_TIPO      TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
*         W_ANSWER(1).
*
*   SELECT SINGLE AUART
*     INTO V_AUART
*     FROM VBAK
*     WHERE VBELN = LIPS-VGBEL.
*
*   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*     EXPORTING
*       CLASS         = '0000'
*       SETNR         = 'MAGGI_LIMITE_FIN'
*     TABLES
*       SET_VALUES    = T_TIPO
*     EXCEPTIONS
*       SET_NOT_FOUND = 1
*       OTHERS        = 2.
*   IF SY-SUBRC <> 0.
**        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*   ENDIF.
*
*   SORT T_TIPO BY FROM.
*   READ TABLE T_TIPO WITH KEY FROM = V_AUART.
*   IF SY-SUBRC = 0.
*     IMPORT  VREFER  FROM MEMORY ID 'MREFER'.
*     SELECT SINGLE *
*       FROM ZSDT0151
*       INTO WA_ZSDT0151
*       WHERE VKORG          = LIKP-VKORG
*       AND   WERKS          = LIPS-WERKS
*       AND   VBELN          = LIPS-VGBEL
*       AND   CH_REFERENCIA  = VREFER.
*
*     IF SY-SUBRC NE 0.
*       V_LFIMG = LIPS-LFIMG.
*       IF  VREFER is not INITIAL.
*          SELECT SINGLE *
*            from zsdt0001
*            into wa_zsdt0001
*            where ch_referencia = VREFER.
*            V_LFIMG = wa_zsdt0001-peso_liq.
*       ENDIF.
*       CALL FUNCTION 'Z_SALDO_CLIENTE'
*         EXPORTING
*           V_KUNNR = LIKP-KUNNR
*           V_VKORG = LIKP-VKORG
*           V_GJAHR = LIKP-ERDAT+0(4)
*           V_VGBEL = LIPS-VGBEL
*           V_LFIMG = V_LFIMG
*         IMPORTING
*           V_SALDO = V_SALDO.
*
*       IF V_SALDO LT 0.
*         CALL FUNCTION 'POPUP_TO_CONFIRM'
*           EXPORTING
*             TEXT_QUESTION         = 'Cliente sem Limite de Crédito, Enviar Workflow para Aprovação ?'
*             TEXT_BUTTON_1         = 'Sim'
*             ICON_BUTTON_1         = 'ICON_OKAY '
*             TEXT_BUTTON_2         = 'Não'
*             ICON_BUTTON_2         = 'ICON_CANCEL'
*             DEFAULT_BUTTON        = '1'
*             DISPLAY_CANCEL_BUTTON = ' '
*             START_COLUMN          = 25
*             START_ROW             = 6
*           IMPORTING
*             ANSWER                = W_ANSWER
*           EXCEPTIONS
*             TEXT_NOT_FOUND        = 1
*             OTHERS                = 2.
*
*         IF W_ANSWER = '1'. "sim
*           WA_ZSDT0151-CH_REFERENCIA  = VREFER.
*
*           WA_ZSDT0151-VKORG          = LIKP-VKORG.
*           WA_ZSDT0151-WERKS          = LIPS-WERKS.
*           WA_ZSDT0151-VBELN          = LIPS-VGBEL.
*           WA_ZSDT0151-ERDAT          = LIKP-ERDAT.
*           WA_ZSDT0151-LFIMG          = v_LFIMG.
*           WA_ZSDT0151-SALDO          = V_SALDO.
*           CALL FUNCTION 'Z_SALDO_CLIENTE'
*              EXPORTING
*                V_KUNNR = LIKP-KUNNR
*                V_VKORG = LIKP-VKORG
*                V_GJAHR = LIKP-ERDAT+0(4)
*         IMPORTING
*           V_SALDO = V_SALDO.
*
*           WA_ZSDT0151-LIMITE         = V_SALDO.
*           "total faturamento
*           clear WA_KONV.
*           SELECT SINGLE *
*               FROM VBAK
*               INTO WA_VBAK
*               WHERE VBELN = LIPS-VGBEL.
*
*           IF SY-SUBRC = 0.
*             SELECT SINGLE *
*               FROM KONV
*               INTO WA_KONV
*             WHERE KNUMV  = WA_VBAK-KNUMV
*             AND   KSCHL  = 'PR00'.
*           ENDIF.
*
*           WA_ZSDT0151-TOTAL          = V_LFIMG  * WA_KONV-KBETR..
*           WA_ZSDT0151-DATA_ATUAL     = SY-DATUM.
*           WA_ZSDT0151-HORA_ATUAL     = SY-UZEIT.
*           WA_ZSDT0151-USUARIO        = SY-UNAME.
*           WA_ZSDT0151-STATUS         = ''.
*           MODIFY ZSDT0151 FROM WA_ZSDT0151.
*           COMMIT WORK.
*           MESSAGE E000(Z01) WITH 'Remessa não gravada, enviado fatura para aprovação.'.
*         else.
*             MESSAGE E000(Z01) WITH 'Remessa não gravada, cliente sem limite de credito'.
*         ENDIF.
*         "Interrompe
*
*       ENDIF.
*     ELSEIF WA_ZSDT0151-STATUS NE 'A'.
*       "Interrompe
*       MESSAGE E000(Z01) WITH 'Remessa para Ordem/Romaneio ainda não aprovado o limite de crédito'.
*     ENDIF.
*   ENDIF.
ENDENHANCEMENT.
