*----------------------------------------------------------------------*
***INCLUDE ZMMR114_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE OK-CODE.
    WHEN C_BTN_DISPLAY_WERKS.
      DATA LV_ENABLED TYPE C.
      DATA LV_ERROR   TYPE ABAP_BOOL VALUE ABAP_TRUE.

      IF ( WG_BUKRS IS NOT INITIAL ).
        IF ( VG_OP_MODE = C_SEARCH )
        OR ( VG_OP_MODE = C_SAVE   ).
          LV_ENABLED  = ABAP_ON.
        ELSE.
          LV_ENABLED = ABAP_OFF.
        ENDIF.

        DATA(OLD_WERKS) = R_WERKS[].
        PERFORM F_SELECTIONS_DIALOG TABLES R_WERKS
                                     USING 'Informar centro'
                                           LV_ENABLED
                                           'PC_WERKS'.

        IF SY-UCOMM = 'ACPT'.
          DATA CONDITIONS TYPE TABLE OF RSIS_S_CONDITION_ID.

          DO.
            TRY.
                LOOP AT R_WERKS[] INTO R_WERKS.
                  SELECT SINGLE BRANCH
                    FROM J_1BBRANCH
                    INTO @DATA(LV_WERKS)
                   WHERE BUKRS  = @WG_BUKRS
                     AND BRANCH = @R_WERKS-LOW.

                  IF ( NOT SY-SUBRC IS INITIAL ).
                    DATA(_TXT_ERROR) = |Centro { R_WERKS-LOW } não pertence a empresa { WG_BUKRS }.|.
                    MESSAGE _TXT_ERROR TYPE 'S' DISPLAY LIKE 'E'.

                    PERFORM F_SELECTIONS_DIALOG
                     TABLES R_WERKS
                      USING _TXT_ERROR LV_ENABLED 'PC_WERKS'.

                    RAISE EXCEPTION TYPE CX_ABAP_INVALID_PARAM_VALUE.
                  ENDIF.
                ENDLOOP.

                LV_ERROR = ABAP_FALSE.
              CATCH CX_ABAP_INVALID_PARAM_VALUE.
                LV_ERROR = ABAP_TRUE.
            ENDTRY.

            CHECK ( LV_ERROR = ABAP_FALSE ).
            EXIT.
          ENDDO.

        ENDIF.

        IF R_WERKS[] IS NOT INITIAL.
          SORT R_WERKS BY LOW.
          READ TABLE R_WERKS INDEX 1.
          BTN_DISPLAY_WERKS = ICON_DISPLAY_MORE.
        ELSE.
          BTN_DISPLAY_WERKS = ICON_ENTER_MORE.
          CLEAR R_WERKS.
        ENDIF.
      ELSE.
        MESSAGE S836(SD) WITH TEXT-E02 'Empresa.' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN C_SEARCH.
      PERFORM F_BUSCA_DADOS.
    WHEN C_SAVE.
      CLEAR GT_FIELDS.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          PERCENTAGE = 1
          TEXT       = TEXT-W02.
      PERFORM F_VERIFICA_ERROS.
      PERFORM F_GRAVA.

    WHEN C_CANCEL.
      SET SCREEN 0.
    WHEN C_BACK.
      SET SCREEN 0.
    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
