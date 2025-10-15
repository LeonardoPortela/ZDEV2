function ZMESSAGE_PREPARE.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LANGUAGE) LIKE  T100-SPRSL DEFAULT SPACE
*"     REFERENCE(MSG_ID) LIKE  T100-ARBGB OPTIONAL
*"     REFERENCE(MSG_NO) LIKE  T100-MSGNR OPTIONAL
*"     REFERENCE(MSG_COMPLETA) TYPE  CHAR1 DEFAULT 'X'
*"  CHANGING
*"     VALUE(MSG_TEXT) TYPE  STRING
*"     VALUE(MSG_VAR1) LIKE  BALM-MSGV1 DEFAULT SPACE
*"     VALUE(MSG_VAR2) LIKE  BALM-MSGV2 DEFAULT SPACE
*"     VALUE(MSG_VAR3) LIKE  BALM-MSGV3 DEFAULT SPACE
*"     VALUE(MSG_VAR4) LIKE  BALM-MSGV4 DEFAULT SPACE
*"  EXCEPTIONS
*"      FUNCTION_NOT_COMPLETED
*"      MESSAGE_NOT_FOUND
*"--------------------------------------------------------------------

  DATA: texto        type c length 200,
        resto_texto  type c length 150,
        texto_result type c length 50,
        qtd_linha    type i.

  if msg_completa eq 'X'.

    call function 'MESSAGE_PREPARE'
      exporting
        language               = language
        msg_id                 = msg_id
        msg_no                 = msg_no
        msg_var1               = msg_var1
        msg_var2               = msg_var2
        msg_var3               = msg_var3
        msg_var4               = msg_var4
      importing
        msg_text               = msg_text
      exceptions
        function_not_completed = 1
        message_not_found      = 2
        others                 = 3.

    case sy-subrc.
      when 1.
        message e100(bl) raising function_not_completed.
      when 2.
        message e101(bl) with sy-msgv1 sy-msgv2 sy-msgv3 raising message_not_found.
      when others.
        message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endcase.

  elseif ( msg_completa ne 'X' ) and ( msg_text is not initial ).

    move msg_text to texto.
    qtd_linha = 0.

    while ( texto ne space ) AND ( qtd_linha le 4 ).

      qtd_linha = qtd_linha + 1.

      call function 'TEXT_SPLIT'
        exporting
          length = 50
          text   = texto
        importing
          line   = texto_result
          rest   = resto_texto.

      move resto_texto to texto.

      case qtd_linha.
        when 1.
          msg_var1 = texto_result.
        when 2.
          msg_var2 = texto_result.
        when 3.
          msg_var3 = texto_result.
        when 4.
          msg_var4 = texto_result.
      endcase.

    endwhile.

  endif.

endfunction.
