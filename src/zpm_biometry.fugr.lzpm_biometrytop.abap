
FUNCTION-POOL zpm_biometry.
DATA: msg TYPE string.
DATA: result TYPE string.
CLASS lcl_fx DEFINITION.
  PUBLIC SECTION.
    DATA: biometry_service TYPE REF TO zcl_biometry.
    METHODS:
      call_options EXPORTING opt_return TYPE char6,
      call_biometry IMPORTING qtd TYPE i,
      get_biometria IMPORTING p_esquerdo TYPE zmmt0088-polegar_esquerdo p_direito TYPE zmmt0088-polegar_esquerdo EXPORTING valid TYPE char1.
ENDCLASS.
CLASS lcl_fx IMPLEMENTATION.
  METHOD call_options.
    DATA: ls_answer TYPE answer,
          ls_return TYPE cHAR6.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar      = 'Autenticação Biometria/Senha'
        text_question = 'Como deseja validar sua Autenticação ?'
        text_button_1 = 'Biometria'(001)
*       ICON_BUTTON_1 = ' '
        text_button_2 = 'Senha'(002)
*       ICON_BUTTON_2 = ' '
      IMPORTING
        answer        = ls_answer.

    IF sy-subrc = 0.
      CASE ls_answer.
        WHEN 'A'.
          msg = 'Cancelado'.
          result = 'W'.
          RETURN.
        WHEN '1'.
          ls_return = 'BIOMET'.
        WHEN '2'.
          ls_return = 'PASSWD'.
      ENDCASE.

      IF ls_return IS NOT INITIAL.
        opt_return = ls_return.
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD get_biometria.
    TRY.
        CALL METHOD biometry_service->validate_digital
          EXPORTING
            hash_validation_l = p_esquerdo
            hash_validation_r = p_direito
          RECEIVING
            result            = DATA(_zmmt0081).
      CATCH cx_root.
    ENDTRY.

    IF _zmmt0081-lado IS NOT INITIAL AND _zmmt0081-polegar IS NOT INITIAL AND _zmmt0081-im_polegar IS NOT INITIAL.
      valid = 'S'.
    ELSE.
      valid = 'N'.
    ENDIF.


  ENDMETHOD.

  METHOD call_biometry.
    DATA: ls_answer               TYPE answer,
          ls_answer_call_biometry TYPE cHAR6,
          ls_titlebar             TYPE char50.

    ls_titlebar = |Coleta da Digital - Tentativa { qtd }|.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = ls_titlebar
        text_question  = 'Posicionar Polegar Direito ou Esquerdo!'
        text_button_1  = 'Continuar'(001)
        icon_button_1  = space
        text_button_2  = 'Desistir'
        icon_button_2  = space
        default_button = '1'
      IMPORTING
        answer         = ls_answer_call_biometry.

    IF sy-subrc = 0.

      CASE ls_answer_call_biometry.
        WHEN 'A'.
          msg = 'Cancelado'.
          result = 'W'.
          "MESSAGE msg TYPE 'I' DISPLAY LIKE result.
          "exit.
        WHEN '1'.

        WHEN '2'.
          msg = 'Desistir'.
          result = 'W'.
          "MESSAGE msg TYPE 'I' DISPLAY LIKE result.
          "exit.
      ENDCASE.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
