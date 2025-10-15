
FUNCTION zpm_check_authorize.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MATRICULA) TYPE  PERSNO
*"  EXPORTING
*"     REFERENCE(E_RESULT) TYPE  CHAR1
*"     REFERENCE(E_MESSAGE) TYPE  STRING
*"----------------------------------------------------------------------

  DATA(load_class) = NEW lcl_fx( ).
  DATA: _tipo TYPE char6.
  DATA: qtd TYPE i.
  load_class->call_options(
    IMPORTING
      opt_return = _tipo
  ).
  CASE _tipo.
    WHEN 'BIOMET'.
      SELECT SINGLE * FROM zmmt0088 INTO @DATA(ls_zmmt0088) WHERE matricula  EQ @i_matricula.
      IF sy-subrc = 0.
        IF ls_zmmt0088-polegar_direito IS NOT INITIAL OR ls_zmmt0088-polegar_esquerdo IS NOT INITIAL.
          DATA: biometry TYPE REF TO zcl_biometry,
                valid(1) TYPE c.
          CREATE OBJECT biometry.

          CLEAR: qtd,valid.
          qtd = 1.
          DO.
            CALL METHOD load_class->call_biometry
              EXPORTING
                qtd = qtd.

            IF result = 'W'.
              CLEAR: qtd.
              exit.
            ENDIF.


            TRY.
                CALL METHOD biometry->validate_digital
                  EXPORTING
                    hash_validation_l = ls_zmmt0088-polegar_esquerdo
                    hash_validation_r = ls_zmmt0088-polegar_direito
                  RECEIVING
                    result            = DATA(get_biometria).
              CATCH cx_root.
            ENDTRY.
            IF get_biometria-lado = 'L' OR get_biometria-lado = 'R'.
              msg = 'Identificação Validada!'.
              result = 'S'.
              CLEAR: qtd.
              qtd = 0.
              EXIT.
            ELSE.
              qtd = qtd + 1.
            ENDIF.
          ENDDO.
        ENDIF.
      ELSE.
        msg = 'Não existe digital cadastrada para o Usuário!'.
        result = 'W'.
        EXIT.
      ENDIF.
    WHEN 'PASSWD'.
      SELECT SINGLE * FROM zmmt0120 INTO @DATA(_zmmt0120) WHERE matricula  EQ @i_matricula.
      IF sy-subrc = 0.
        DATA: tab_pop TYPE TABLE OF sval,
              wa_pop  TYPE sval,
              input   TYPE string.

        wa_pop-tabname   = 'ZMMT0120'.
        wa_pop-fieldname = 'SENHA'.
        APPEND wa_pop TO tab_pop.


        DO.

          CALL FUNCTION 'POPUP_GET_VALUES'
            EXPORTING
              popup_title     = 'Senha:'
*             START_COLUMN    = '1'
*             START_ROW       = '1'
            TABLES
              fields          = tab_pop
            EXCEPTIONS
              error_in_fields = 1
              OTHERS          = 2.

          READ TABLE tab_pop INTO wa_pop INDEX 1.
          IF _zmmt0120-senha = wa_pop-value.
            msg = 'Identificação Validada!'.
            result = 'S'.
            CLEAR: qtd.
            qtd = 0.
            EXIT.
          ELSE.
            qtd = qtd + 1.
            msg = 'Senha não confere com a Cadastrada!'.
            result = 'W'.
          ENDIF.
        ENDDO.

      ELSE.
        msg = 'Não existe senha cadastrada para o Usuário!'.
        result = 'W'.
        EXIT.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  e_message = msg.
  e_result = result.

ENDFUNCTION.
