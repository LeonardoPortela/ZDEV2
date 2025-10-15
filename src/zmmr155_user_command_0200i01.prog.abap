*----------------------------------------------------------------------*
***INCLUDE ZMMR155_USER_COMMAND_0200I01.
*----------------------------------------------------------------------*
DATA: ED_ID_CARGA TYPE ZDE_ID_CARGA.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE OK_CODE.
    WHEN 'INCLUIR'.

      TRY .

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = ED_ID_CARGA
            IMPORTING
              OUTPUT = ED_ID_CARGA.

          DATA(CARGA_ADD) = ZCL_FACTORY_CARGA=>ZIF_FACTORY_CARGA~GET_INSTANCE(
            )->SET_FACTORY_OBJETO_ID( I_ID_CARGA = ED_ID_CARGA
            )->GET_FACTORY_OBJETO(
            )->SET_REGISTRO( I_ID_CARGA = ED_ID_CARGA I_NO_ENQUEUE = ABAP_TRUE
            ).

          READ TABLE IT_CARGAS ASSIGNING FIELD-SYMBOL(<FS_CARGA>) WITH KEY CARGA-ID_CARGA = ED_ID_CARGA.
          IF SY-SUBRC IS INITIAL.
            CARGA_ADD->GET_INFO_ALV_APRESENTACAO( IMPORTING E_APRESENTACAO = <FS_CARGA> ).
          ELSE.
            CARGA_ADD->GET_INFO_ALV_APRESENTACAO( IMPORTING E_APRESENTACAO = E_APRESENTACAO ).
            APPEND E_APRESENTACAO TO IT_CARGAS.
          ENDIF.

          CLEAR: CARGA_ADD.

          LEAVE TO SCREEN 0.

        CATCH ZCX_CARGA INTO EX_CARGA.  "
          EX_CARGA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
        CATCH CX_ROOT INTO EX_ROOT.
          MESSAGE EX_ROOT->GET_LONGTEXT( ) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TL0200'.
ENDMODULE.
