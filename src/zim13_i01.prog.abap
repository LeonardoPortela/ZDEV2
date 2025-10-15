*&---------------------------------------------------------------------*
*&  Include           ZIM12_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok-code.
    WHEN 'EXPORTAR'.

      MESSAGE 'Não implementado' TYPE 'I'.

    WHEN 'IMPORTAR'.
*. 121322 CS2023000651 Customizar ZIM15 com versão a importar KP06 - SPA

      CLEAR: retorno_filtro,wa_field,li_field,p_versio.
      FREE: retorno_filtro,wa_field,li_field,p_versio.

      MOVE 'RKCSP' TO wa_field-tabname.
      MOVE 'VERSN' TO wa_field-fieldname.
      APPEND wa_field TO li_field.

      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
*         NO_VALUE_CHECK = ' '
          popup_title = 'Filtro Versão'
*         START_COLUMN = '5'
*         START_ROW   = '5'
      "IMPORTING
          "RETURNCODE  = retorno_filtro
        tables
          fields      = li_field
* EXCEPTIONS
*         ERROR_IN_FIELDS = 1
*         OTHERS      = 2
        .


      IF sy-subrc <> 0.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno

        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      ELSE.

        FIELD-SYMBOLS <li_field> LIKE sval.

        LOOP AT li_field ASSIGNING <li_field> WHERE tabname = 'RKCSP' AND fieldname = 'VERSN'.
          p_versio = <li_field>-value.
        ENDLOOP.

        IF p_versio IS NOT INITIAL.
**********************************************************************
          IF r_zim02 = 'X'.
            PERFORM f_zim02_add TABLES t_inv.
          ENDIF.

          IF r_kp06 = 'X' .
            PERFORM f_kp06_add.
          ENDIF.
**********************************************************************

        ENDIF.


      ENDIF.

    WHEN c_add.

    WHEN c_cancel.
      CLEAR wg_acao.
      SET SCREEN 0.

    WHEN c_exit OR c_back.
      CLEAR wg_acao.
      SET SCREEN 0.
  ENDCASE.

ENDMODULE.
