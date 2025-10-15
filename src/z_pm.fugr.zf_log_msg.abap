FUNCTION zf_log_msg.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      MT_MESS TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  CHECK mt_mess IS NOT INITIAL.

   CALL FUNCTION 'MESSAGES_INITIALIZE'.

      LOOP AT mt_mess ASSIGNING FIELD-SYMBOL(<fs_ret2>).

        IF <fs_ret2>-id IS INITIAL.

          <fs_ret2>-id = 'DS'. "<-classe padrao abap
          <fs_ret2>-number = '016'.
          <fs_ret2>-message_v1 = <fs_ret2>-message.

        ENDIF.

        CALL FUNCTION 'MESSAGE_STORE'
          EXPORTING
            arbgb                  = <fs_ret2>-id
            "EXCEPTION_IF_NOT_ACTIVE  = 'X'
            msgty                  = <fs_ret2>-type
            msgv1                  = <fs_ret2>-message_v1
            msgv2                  = <fs_ret2>-message_v2
            msgv3                  = <fs_ret2>-message_v3
            msgv4                  = <fs_ret2>-message_v4
            txtnr                  = <fs_ret2>-number
          EXCEPTIONS
            message_type_not_valid = 1
            not_active             = 2
            OTHERS                 = 3.     "#EC CI_SUBRC

      ENDLOOP.

      CALL FUNCTION 'MESSAGES_STOP'
        EXCEPTIONS
          a_message = 1
          e_message = 2
          i_message = 3
          w_message = 4
          OTHERS    = 5.     "#EC CI_SUBRC

      CALL FUNCTION 'MESSAGES_SHOW'
        EXPORTING
          batch_list_type     = 'B'
          show_linno          = ' '
          show_linno_text     = 'X'
          show_linno_text_len = '3'
          i_use_grid          = ' '
          i_amodal_window     = ' '
        EXCEPTIONS
          inconsistent_range  = 1
          no_messages         = 2
          OTHERS              = 3.     "#EC CI_SUBRC

ENDFUNCTION.
