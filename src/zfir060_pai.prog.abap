*&---------------------------------------------------------------------*
*&  Include           ZFIR060_PAI
*&---------------------------------------------------------------------*

MODULE USER_COMMAND_0102 INPUT.

   DATA: IT_RSPARAMS   TYPE TABLE OF RSPARAMS,
        WA_RSPARAMS   TYPE RSPARAMS.


  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      CLEAR: IT_RSPARAMS[].

      IF VG_DTINI_PROC IS INITIAL.
        MESSAGE 'Informe uma data de processamento!' TYPE 'S'.
        EXIT.
      ENDIF.

      AUTHORITY-CHECK OBJECT 'M_MATE_BUK'
        ID 'BUKRS' FIELD  P_BUKRS
        ID 'ACTVT' FIELD '03'.    "Visualização

      CASE SY-SUBRC.
        WHEN 0.
          "  tem autorização!
        WHEN 4.
          MESSAGE 'Sem autorização para esta empresa' TYPE 'I'.
          EXIT.
        WHEN 12.
          MESSAGE 'Sem autorização neste objeto ' TYPE 'I'.
          EXIT.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

      CLEAR: WA_RSPARAMS.
      WA_RSPARAMS-KIND    = 'S'.
      WA_RSPARAMS-SIGN    = 'I'.
      WA_RSPARAMS-OPTION  = 'EQ'.

      WA_RSPARAMS-SELNAME = 'P_FKDAT'.
      WA_RSPARAMS-LOW     = VG_DTINI_PROC.
      APPEND WA_RSPARAMS TO IT_RSPARAMS.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = 'Confirma o processamento nessa data informada?'
          TEXT_BUTTON_1         = 'Sim'
          TEXT_BUTTON_2         = 'Não'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ''
        IMPORTING
          ANSWER                = VAR_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.

      CHECK VAR_ANSWER EQ '1'.

      SUBMIT ZSDR0081 WITH SELECTION-TABLE IT_RSPARAMS
                        AND RETURN.

      MESSAGE 'Processamento concluído!' TYPE 'S'.
      LEAVE TO SCREEN 0.


    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.



ENDMODULE.
