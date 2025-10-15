"Name: \PR:SAPMV50A\FO:USEREXIT_SAVE_DOCUMENT_PREPARE\SE:BEGIN\EI
ENHANCEMENT 0 Z_SALDO_CLIENTE.
*
  DATA: V_SALDO      TYPE VBAK-NETWR,
        V_SALDO_RESIDUAL TYPE VBAK-NETWR,
        V_TOTAL      TYPE NETWR_AK,
        V_LIMITE     TYPE KLIMG,
        V_AUART      TYPE VBAK-AUART,
        V_LFIMG      TYPE LIPS-LFIMG,
        WA_VBAK      TYPE VBAK,
        WA_KONV      TYPE KONV,
        VREFER       TYPE ZSDT0151-CH_REFERENCIA,
        WA_ZSDT0151  TYPE ZSDT0151,
        IT_ZSDT0151  TYPE TABLE OF ZSDT0151,
        WA_ZSDT0001  TYPE ZSDT0001,
        FLAG_EXEC(3),
        T_TIPO      TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
        V_NUMBER   TYPE I,
        W_MSG(100),
        W_ANSWER(1).

*   IMPORT  FLAG_EXEC  FROM MEMORY ID 'VEXECVL'.
*   CHECK flag_exec ne 'SIM'.
*
*   FLAG_EXEC = 'SIM'.
*   EXPORT FLAG_EXEC TO MEMORY ID 'VEXECVL'.

   if sy-tcode+0(2) = 'VL'.
      if sy-ucomm ne 'WABU_T'   and
         sy-ucomm ne 'SICH_T'   .
        exit.
      endif.
   else.
      CHECK sy-ucomm is INITIAL.
   endif.

   CHECK LIPS-LFIMG ne 0.

   SELECT SINGLE AUART
     INTO V_AUART
     FROM VBAK
     WHERE VBELN = LIPS-VGBEL.

   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       CLASS         = '0000'
       SETNR         = 'MAGGI_LIMITE_FIN'
     TABLES
       SET_VALUES    = T_TIPO
     EXCEPTIONS
       SET_NOT_FOUND = 1
       OTHERS        = 2.
   IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.


   SORT T_TIPO BY FROM.
   READ TABLE T_TIPO WITH KEY FROM = V_AUART.
   IF SY-SUBRC = 0.
     clear WA_ZSDT0151.
     if sy-tcode+0(2) = 'VL'.
        clear VREFER.
       SELECT *
          FROM ZSDT0151
          INTO TABLE IT_ZSDT0151
          WHERE VKORG          = LIKP-VKORG
          AND   WERKS          = LIPS-WERKS
          AND   VBELN          = LIPS-VGBEL
          AND   CH_REFERENCIA  = ''
          ORDER BY ERDAT ASCENDING.
       if sy-subrc eq 0.
          READ TABLE IT_ZSDT0151 INTO WA_ZSDT0151 INDEX 1.
       endif.
     else.
        IMPORT  VREFER  FROM MEMORY ID 'MREFER'.
        SELECT SINGLE *
          FROM ZSDT0151
          INTO WA_ZSDT0151
          WHERE VKORG          = LIKP-VKORG
          AND   WERKS          = LIPS-WERKS
          AND   VBELN          = LIPS-VGBEL
          AND   CH_REFERENCIA  = VREFER.
        IF VREFER is INITIAL.
          SY-SUBRC = 0.
          WA_ZSDT0151-STATUS = 'A'.
        ENDIF.
     endif.
     IF SY-SUBRC NE 0 or ( WA_ZSDT0151-STATUS EQ 'R' AND sy-tcode+0(2) = 'VL' ).
       V_LFIMG = LIPS-LFIMG.

       CALL FUNCTION 'Z_SALDO_CLIENTE'
         EXPORTING
           V_KUNNR          = LIKP-KUNNR
           V_VKORG          = LIKP-VKORG
           V_GJAHR          = LIKP-ERDAT+0(4)
           V_VGBEL          = LIPS-VGBEL
           V_LFIMG          = V_LFIMG
         IMPORTING
           V_SALDO          = V_SALDO
           V_TOTAL          = V_TOTAL
           V_SALDO_RESIDUAL = V_SALDO_RESIDUAL
           V_LIMITE         = V_LIMITE.

       W_MSG = |Cliente sem Limite de Crédito, ( { V_LIMITE } ). Enviar Workflow para Aprovação ?|.
       IF WA_ZSDT0151-STATUS EQ 'R'.
          W_MSG = 'Limite de Crédito Rejeitado, Enviar Workflow para Aprovação novamente?'.
       ENDIF.


       IF V_SALDO_RESIDUAL LT 0.

         CALL FUNCTION 'POPUP_TO_CONFIRM'
           EXPORTING
             TEXT_QUESTION         = W_MSG
             TEXT_BUTTON_1         = 'Sim'
             ICON_BUTTON_1         = 'ICON_OKAY '
             TEXT_BUTTON_2         = 'Não'
             ICON_BUTTON_2         = 'ICON_CANCEL'
             DEFAULT_BUTTON        = '1'
             DISPLAY_CANCEL_BUTTON = ' '
             START_COLUMN          = 25
             START_ROW             = 6
           IMPORTING
             ANSWER                = W_ANSWER
           EXCEPTIONS
             TEXT_NOT_FOUND        = 1
             OTHERS                = 2.

         IF W_ANSWER = '1'. "sim

           CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                NR_RANGE_NR = '01'
                OBJECT      = 'ZGL_LOTE'
              IMPORTING
                NUMBER      = V_NUMBER.

           WA_ZSDT0151-LOTE           = V_NUMBER.
           WA_ZSDT0151-CH_REFERENCIA  = VREFER.
           WA_ZSDT0151-VKORG          = LIKP-VKORG.
           WA_ZSDT0151-WERKS          = LIPS-WERKS.
           WA_ZSDT0151-VBELN          = LIPS-VGBEL.
           WA_ZSDT0151-ERDAT          = LIKP-ERDAT.
           WA_ZSDT0151-MATNR          = LIPS-MATNR.
           WA_ZSDT0151-KUNNR          = LIKP-KUNNR.
           WA_ZSDT0151-LFIMG          = v_LFIMG.
           WA_ZSDT0151-SALDO          = V_SALDO_RESIDUAL.
           WA_ZSDT0151-LIMITE         = V_LIMITE.
           WA_ZSDT0151-TOTAL          = V_TOTAL.
           WA_ZSDT0151-DATA_ATUAL     = SY-DATUM.
           WA_ZSDT0151-HORA_ATUAL     = SY-UZEIT.
           WA_ZSDT0151-USUARIO        = SY-UNAME.
           WA_ZSDT0151-STATUS         = ''.
           MODIFY ZSDT0151 FROM WA_ZSDT0151.
           COMMIT WORK.
           MESSAGE E000(Z01) WITH 'Remessa não gravada,' 'enviado fatura para aprovação.'.
         ELSE.
           MESSAGE E000(Z01) WITH 'Remessa não gravada,' 'cliente sem limite de credito'.
         ENDIF.
         "Interrompe

       ENDIF.
     ELSEIF WA_ZSDT0151-STATUS EQ 'R'.
       "Interrompe
       MESSAGE E000(Z01) WITH 'Remessa para Ordem/Romaneio'
                              ' Rejeitada!'.
     ELSEIF WA_ZSDT0151-STATUS NE 'A'.
       "Interrompe
       MESSAGE E000(Z01) WITH 'Remessa para Ordem/Romaneio'
                              ' ainda não aprovado o limite de crédito'.
     ENDIF.
   ENDIF.
ENDENHANCEMENT.
